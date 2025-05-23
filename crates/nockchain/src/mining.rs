use std::env; // Added for environment variable access
use std::str::FromStr;

use kernels::miner::KERNEL;
use nockapp::kernel::checkpoint::JamPaths;
use nockapp::kernel::form::Kernel;
use nockapp::nockapp::driver::{IODriverFn, NockAppHandle, PokeResult};
use nockapp::nockapp::wire::Wire;
use nockapp::nockapp::NockAppError;
use nockapp::noun::slab::NounSlab;
use nockapp::noun::{AtomExt, NounExt};
use nockvm::noun::{Atom, Cell, D, Noun, T};
use nockvm_macros::tas;
use tempfile::tempdir;
use tracing::{info, instrument, warn};

// Define NONCES_PER_ATTEMPT as a static variable, configurable via environment variable
static NONCES_PER_ATTEMPT: u64 = {
    match std::env::var("NOCKCHAIN_NONCES_PER_ATTEMPT") {
        Ok(val_str) => match val_str.parse::<u64>() {
            Ok(val) => val,
            Err(_) => {
                // Using println! here as tracing might not be initialized yet for static variables.
                // Consider a more robust logging strategy if this becomes an issue.
                println!("WARN: Invalid NOCKCHAIN_NONCES_PER_ATTEMPT value '{}', using default 1,000,000.", val_str);
                1_000_000
            }
        },
        Err(_) => {
            // println!("INFO: NOCKCHAIN_NONCES_PER_ATTEMPT not set, using default 1,000,000.");
            1_000_000 // Default if env var is not set
        }
    }
};

pub enum MiningWire {
    Mined,
    Candidate,
    SetPubKey,
    Enable,
}

impl MiningWire {
    pub fn verb(&self) -> &'static str {
        match self {
            MiningWire::Mined => "mined",
            MiningWire::SetPubKey => "setpubkey",
            MiningWire::Candidate => "candidate",
            MiningWire::Enable => "enable",
        }
    }
}

impl Wire for MiningWire {
    const VERSION: u64 = 1;
    const SOURCE: &'static str = "miner";

    fn to_wire(&self) -> nockapp::wire::WireRepr {
        let tags = vec![self.verb().into()];
        nockapp::wire::WireRepr::new(MiningWire::SOURCE, MiningWire::VERSION, tags)
    }
}

#[derive(Debug, Clone)]
pub struct MiningKeyConfig {
    pub share: u64,
    pub m: u64,
    pub keys: Vec<String>,
}

impl FromStr for MiningKeyConfig {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Expected format: "share,m:key1,key2,key3"
        let parts: Vec<&str> = s.split(':').collect();
        if parts.len() != 2 {
            return Err("Invalid format. Expected 'share,m:key1,key2,key3'".to_string());
        }

        let share_m: Vec<&str> = parts[0].split(',').collect();
        if share_m.len() != 2 {
            return Err("Invalid share,m format".to_string());
        }

        let share = share_m[0].parse::<u64>().map_err(|e| e.to_string())?;
        let m = share_m[1].parse::<u64>().map_err(|e| e.to_string())?;
        let keys: Vec<String> = parts[1].split(',').map(String::from).collect();

        Ok(MiningKeyConfig { share, m, keys })
    }
}

pub fn create_mining_driver(
    mining_config: Option<Vec<MiningKeyConfig>>,
    mine: bool,
    init_complete_tx: Option<tokio::sync::oneshot::Sender<()>>,
) -> IODriverFn {
    Box::new(move |mut handle| {
        Box::pin(async move {
            let Some(configs) = mining_config else {
                enable_mining(&handle, false).await?;

                if let Some(tx) = init_complete_tx {
                    tx.send(()).map_err(|_| {
                        warn!("Could not send driver initialization for mining driver.");
                        NockAppError::OtherError
                    })?;
                }

                return Ok(());
            };
            if configs.len() == 1
                && configs[0].share == 1
                && configs[0].m == 1
                && configs[0].keys.len() == 1
            {
                set_mining_key(&handle, configs[0].keys[0].clone()).await?;
            } else {
                set_mining_key_advanced(&handle, configs).await?;
            }
            enable_mining(&handle, mine).await?;

            if let Some(tx) = init_complete_tx {
                tx.send(()).map_err(|_| {
                    warn!("Could not send driver initialization for mining driver.");
                    NockAppError::OtherError
                })?;
            }

            if !mine {
                return Ok(());
            }
            let mut next_attempt: Option<NounSlab> = None;
            let mut current_attempt: tokio::task::JoinSet<()> = tokio::task::JoinSet::new();

            loop {
                tokio::select! {
                    effect_res = handle.next_effect() => {
                        let Ok(effect) = effect_res else {
                          warn!("Error receiving effect in mining driver: {effect_res:?}");
                        continue;
                        };
                        let Ok(effect_cell) = (unsafe { effect.root().as_cell() }) else {
                            drop(effect);
                            continue;
                        };

                        if effect_cell.head().eq_bytes("mine") {
                            let candidate_slab = {
                                let mut slab = NounSlab::new();
                                slab.copy_into(effect_cell.tail());
                                slab
                            };
                            if !current_attempt.is_empty() {
                                next_attempt = Some(candidate_slab);
                            } else {
                                let (cur_handle, attempt_handle) = handle.dup();
                                handle = cur_handle;
                                current_attempt.spawn(mining_attempt(candidate_slab, attempt_handle));
                            }
                        }
                    },
                    mining_attempt_res = current_attempt.join_next(), if !current_attempt.is_empty()  => {
                        if let Some(Err(e)) = mining_attempt_res {
                            warn!("Error during mining attempt: {e:?}");
                        }
                        let Some(candidate_slab) = next_attempt else {
                            continue;
                        };
                        next_attempt = None;
                        let (cur_handle, attempt_handle) = handle.dup();
                        handle = cur_handle;
                        current_attempt.spawn(mining_attempt(candidate_slab, attempt_handle));

                    }
                }
            }
        })
    })
}

pub async fn mining_attempt(candidate: NounSlab, handle: NockAppHandle) -> () {
    // NONCES_PER_ATTEMPT is now a static variable defined at the module level

    let candidate_root = candidate.root();
    let candidate_cell = match candidate_root.as_cell() {
        Ok(cell) => cell,
        Err(_) => {
            warn!("Mining candidate is not a cell");
            return;
        }
    };

    let length_noun = candidate_cell.head();

    let tail_cell = match candidate_cell.tail().as_cell() {
        Ok(cell) => cell,
        Err(_) => {
            warn!("Mining candidate tail is not a cell");
            return;
        }
    };

    let commitment_noun = tail_cell.head();

    let initial_nonce_atom = match tail_cell.tail().as_atom() {
        Ok(atom) => atom,
        Err(_) => {
            warn!("Mining candidate initial_nonce is not an atom");
            return;
        }
    };

    let initial_nonce_val = match AtomExt::to_u64(&initial_nonce_atom) {
        Ok(val) => val,
        Err(_) => {
            warn!("Failed to convert initial_nonce to u64");
            return;
        }
    };

    info!(
        "Starting nonce iteration for candidate. Initial nonce: {}",
        initial_nonce_val
    );

    let snapshot_dir =
        tokio::task::spawn_blocking(|| tempdir().expect("Failed to create temporary directory"))
            .await
            .expect("Failed to create temporary directory");
    let hot_state = zkvm_jetpack::hot::produce_prover_hot_state();
    let snapshot_path_buf = snapshot_dir.path().to_path_buf();
    let jam_paths = JamPaths::new(snapshot_dir.path());
    // Spawns a new std::thread for this mining attempt
    let kernel =
        Kernel::load_with_hot_state_huge(snapshot_path_buf, jam_paths, KERNEL, &hot_state, false)
            .await
            .expect("Could not load mining kernel");

    for nonce_offset in 0..NONCES_PER_ATTEMPT {
        let current_nonce_val = initial_nonce_val + nonce_offset;
        let mut attempt_slab = NounSlab::new();

        let length_noun_copy = attempt_slab
            .copy_foreign_noun(length_noun)
            .expect("Failed to copy length_noun");
        let commitment_noun_copy = attempt_slab
            .copy_foreign_noun(commitment_noun)
            .expect("Failed to copy commitment_noun");
        let current_nonce_atom =
            Atom::from_u64(&mut attempt_slab, current_nonce_val).expect("Failed to create nonce atom");

        let attempt_noun = T(
            &mut attempt_slab,
            &[
                length_noun_copy,
                T(
                    &mut attempt_slab,
                    &[commitment_noun_copy, current_nonce_atom.as_noun()],
                ),
            ],
        );
        attempt_slab.set_root(attempt_noun);

        match kernel
            .poke(MiningWire::Candidate.to_wire(), attempt_slab)
            .await
        {
            Ok(effects_slab) => {
                for effect in effects_slab.to_vec() {
                    let Ok(effect_cell) = (unsafe { effect.root().as_cell() }) else {
                        drop(effect);
                        continue;
                    };
                    if effect_cell.head().eq_bytes("command") {
                        info!(
                            "Mined block with nonce: {}! Submitting to nockchain.",
                            current_nonce_val
                        );
                        match handle.poke(MiningWire::Mined.to_wire(), effect.clone()).await {
                            Ok(_) => {
                                info!("Successfully submitted mined block for nonce {}", current_nonce_val);
                                return;
                            }
                            Err(e) => {
                                warn!(
                                    "Failed to poke nockchain with mined PoW for nonce {}: {:?}",
                                    current_nonce_val, e
                                );
                            }
                        }
                    }
                }
            }
            Err(e) => {
                warn!(
                    "Kernel poke failed during nonce iteration for nonce {}: {:?}",
                    current_nonce_val, e
                );
                // Depending on the error, we might want to break or return.
                // For now, we continue to the next nonce.
                continue;
            }
        }
    }

    info!(
        "Finished iterating {} nonces starting from {} (up to {} attempts), no block mined.",
        NONCES_PER_ATTEMPT, initial_nonce_val, NONCES_PER_ATTEMPT // Clarified log
    );
}

#[instrument(skip(handle, pubkey))]
async fn set_mining_key(
    handle: &NockAppHandle,
    pubkey: String,
) -> Result<PokeResult, NockAppError> {
    let mut set_mining_key_slab = NounSlab::new();
    let set_mining_key = Atom::from_value(&mut set_mining_key_slab, "set-mining-key")
        .expect("Failed to create set-mining-key atom");
    let pubkey_cord =
        Atom::from_value(&mut set_mining_key_slab, pubkey).expect("Failed to create pubkey atom");
    let set_mining_key_poke = T(
        &mut set_mining_key_slab,
        &[D(tas!(b"command")), set_mining_key.as_noun(), pubkey_cord.as_noun()],
    );
    set_mining_key_slab.set_root(set_mining_key_poke);

    handle
        .poke(MiningWire::SetPubKey.to_wire(), set_mining_key_slab)
        .await
}

async fn set_mining_key_advanced(
    handle: &NockAppHandle,
    configs: Vec<MiningKeyConfig>,
) -> Result<PokeResult, NockAppError> {
    let mut set_mining_key_slab = NounSlab::new();
    let set_mining_key_adv = Atom::from_value(&mut set_mining_key_slab, "set-mining-key-advanced")
        .expect("Failed to create set-mining-key-advanced atom");

    // Create the list of configs
    let mut configs_list = D(0);
    for config in configs {
        // Create the list of keys
        let mut keys_noun = D(0);
        for key in config.keys {
            let key_atom =
                Atom::from_value(&mut set_mining_key_slab, key).expect("Failed to create key atom");
            keys_noun = T(&mut set_mining_key_slab, &[key_atom.as_noun(), keys_noun]);
        }

        // Create the config tuple [share m keys]
        let config_tuple = T(
            &mut set_mining_key_slab,
            &[D(config.share), D(config.m), keys_noun],
        );

        configs_list = T(&mut set_mining_key_slab, &[config_tuple, configs_list]);
    }

    let set_mining_key_poke = T(
        &mut set_mining_key_slab,
        &[D(tas!(b"command")), set_mining_key_adv.as_noun(), configs_list],
    );
    set_mining_key_slab.set_root(set_mining_key_poke);

    handle
        .poke(MiningWire::SetPubKey.to_wire(), set_mining_key_slab)
        .await
}

//TODO add %set-mining-key-multisig poke
#[instrument(skip(handle))]
async fn enable_mining(handle: &NockAppHandle, enable: bool) -> Result<PokeResult, NockAppError> {
    let mut enable_mining_slab = NounSlab::new();
    let enable_mining = Atom::from_value(&mut enable_mining_slab, "enable-mining")
        .expect("Failed to create enable-mining atom");
    let enable_mining_poke = T(
        &mut enable_mining_slab,
        &[D(tas!(b"command")), enable_mining.as_noun(), D(if enable { 0 } else { 1 })],
    );
    enable_mining_slab.set_root(enable_mining_poke);
    handle
        .poke(MiningWire::Enable.to_wire(), enable_mining_slab)
        .await
}
