#!/usr/bin/env bash
set -euo pipefail

# Install prerequisites
if ! command -v rustup >/dev/null 2>&1; then
    curl https://sh.rustup.rs -sSf | sh -s -- -y
    source "$HOME/.cargo/env"
else
    rustup update
fi

if command -v apt-get >/dev/null 2>&1; then
    sudo apt-get update
    sudo apt-get install -y build-essential pkg-config libssl-dev clang cmake
fi

# Build and install
make build
make install-nockchain
make install-nockchain-wallet

CPU_COUNT=$(nproc)
MEM_KB=$(grep MemTotal /proc/meminfo | awk '{print $2}')
MEM_BYTES=$((MEM_KB * 1024))
MEM_LIMIT_BYTES=$((MEM_BYTES / 2))

MAX_ESTABLISHED=$((CPU_COUNT * 8))
MAX_INCOMING=$((MAX_ESTABLISHED / 2))
MAX_OUTGOING=$((MAX_ESTABLISHED / 2))

exec nockchain --fakenet \
  --mining-pubkey "${MINING_PUBKEY:?MINING_PUBKEY not set}" \
  --max-established "${MAX_ESTABLISHED}" \
  --max-established-incoming "${MAX_INCOMING}" \
  --max-established-outgoing "${MAX_OUTGOING}" \
  --max-pending-incoming "${MAX_INCOMING}" \
  --max-pending-outgoing "${MAX_OUTGOING}" \
  --max-established-per-peer 2 \
  --max-system-memory-bytes "${MEM_LIMIT_BYTES}"
