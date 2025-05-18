# Nockchain

**Nockchain is a lightweight blockchain for heavyweight verifiable applications.**


We believe the future of blockchains is lightweight trustless settlement of heavyweight verifiable computation. The only way to get there is by replacing verifiability-via-public-replication with verifiability-via-private-proving. Proving happens off-chain; verification is on-chain.

*Nockchain is entirely experimental and many parts are unaudited. We make no representations or guarantees as to the behavior of this software.*


## Setup

Install `rustup` by following their instructions at: [https://rustup.rs/](https://rustup.rs/)

Install `hoonc`, the Hoon compiler:

```
make install-hoonc
```

To build the Nockchain and the wallet binaries and their required assets:

```
make build
```

## Install Wallet

After you've run the setup and build commands, install the wallet:

```
make install-nockchain-wallet
```

To run the wallet, see the nockchain-wallet [README](./crates/nockchain-wallet/README.md).


## Install Nockchain

After you've run the setup and build commands, install Nockchain:

```
make install-nockchain
```

## FakeNet Quick Start

Use the helper script to bootstrap a local fakenet node:

```bash
MINING_PUBKEY=<your_pubkey> scripts/setup-fakenet.sh
```

The script installs prerequisites, builds and installs the project, then
starts `nockchain --fakenet` with limits tuned to your hardware. Provide the
mining public key via the `MINING_PUBKEY` environment variable.

 <<<<<<< codex/update-setup-fakenet-sh-and-readme
To spin up both a leader and a follower in detached tmux sessions run:

```bash
MINING_PUBKEY=<your_pubkey> scripts/setup-fakenet.sh --tmux
```

This will execute:

```bash
tmux new-session -d -s nock_leader "make run-nockchain-leader"
tmux new-session -d -s nock_follower "make run-nockchain-follower"
```
You can then attach with `tmux attach -t nock_leader` or `tmux attach -t nock_follower`.
 =======
Add `export MINING_PUBKEY=<your_pubkey>` to your `~/.bashrc` (or a system-wide
profile) so the variable persists across reboots. The helper script relies on
this variable and will fail if it is unset.
 >>>>>>> master



## Testing Nodes

To run a test Nockchain node that publishes the genesis block:

```
make run-nockchain-leader
```


To run a test Nockchain node that waits for the genesis block:

```
make run-nockchain-follower
```


## Mining Setup

See [docs/mining-setup.md](docs/mining-setup.md) for instructions on running a miner.
