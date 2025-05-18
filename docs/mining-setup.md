# Mining Setup

This guide shows how to start a Nockchain node with mining enabled.

## Required Flags

- `--mining-pubkey <pubkey>` – Base58 public key to receive mining rewards.
- `--genesis-leader` or `--genesis-watcher` – Choose leader to publish the genesis block or watcher to wait for it.
- `--npc-socket <path>` – Path for the Nockchain NPC socket.
- `--bind <multiaddr>` – Address to listen for peers (can be specified multiple times).
- `--peer <multiaddr>` – Initial peer addresses (can be specified multiple times).
- `--new-peer-id` – Generate a new libp2p peer identity at startup.
- `--no-default-peers` – Do not connect to built‑in peers automatically.

Advanced miners can use `--mining-key-adv share,m:key1,key2,...` to specify weighted mining keys. The flag is mutually exclusive with `--mining-pubkey`.

## Example Configuration

Run a leader node on a test network:

```bash
nockchain --fakenet \
  --genesis-leader \
  --npc-socket nockchain.sock \
  --mining-pubkey <your_pubkey> \
  --bind /ip4/0.0.0.0/udp/3005/quic-v1 \
  --peer /ip4/127.0.0.1/udp/3006/quic-v1 \
  --new-peer-id --no-default-peers
```

Run a watcher node:

```bash
nockchain --fakenet \
  --genesis-watcher \
  --npc-socket nockchain.sock \
  --mining-pubkey <your_pubkey> \
  --bind /ip4/0.0.0.0/udp/3006/quic-v1 \
  --peer /ip4/127.0.0.1/udp/3005/quic-v1 \
  --new-peer-id --no-default-peers
```

## Resource Considerations

Mining involves continuous EquiX proof‑of‑work calculations. Expect high CPU usage and increased memory pressure. Ensure adequate cooling and power, and consider running on dedicated hardware if mining for extended periods. Connection limits and memory usage can be tuned with `--max-system-memory-bytes` or `--max-system-memory-fraction` if needed.
