#!/usr/bin/env bash
set -euo pipefail

# Push built nix store paths to haskell.cachix.org
# Uses age to decrypt the cachix auth token with your SSH key

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEMO_ROOT="$(dirname "$SCRIPT_DIR")"
ENCRYPTED_TOKEN="$DEMO_ROOT/secrets/cachix-token.age"

if [[ ! -f "$ENCRYPTED_TOKEN" ]]; then
    echo "Error: Encrypted token not found at $ENCRYPTED_TOKEN"
    exit 1
fi

# Find age binary
AGE=$(command -v age || nix-shell -p age --run 'command -v age')

# Decrypt the token using SSH key
echo "Decrypting cachix token..."
export CACHIX_AUTH_TOKEN=$($AGE -d -i ~/.ssh/id_ed25519 "$ENCRYPTED_TOKEN")

# Find cachix binary
CACHIX=$(command -v cachix || nix-shell -p cachix --run 'command -v cachix')

if [[ $# -eq 0 ]]; then
    echo "Usage: $0 <store-path> [store-path...]"
    echo "   or: $0 --watch-exec <command>"
    exit 1
fi

if [[ "$1" == "--watch-exec" ]]; then
    shift
    echo "Watching and pushing to haskell cache..."
    exec $CACHIX watch-exec haskell -- "$@"
else
    echo "Pushing to haskell cache..."
    exec $CACHIX push haskell "$@"
fi
