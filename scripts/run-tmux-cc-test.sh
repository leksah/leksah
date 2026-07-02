#!/bin/sh
# Build and run the standalone tmux control-mode protocol test.
# Needs ghc (boot libs only) and tmux on PATH; uses a throwaway tmux server
# on socket "leksah-cc-test" (never the user's default or leksah's socket).
set -e
cd "$(dirname "$0")/.."
out=$(mktemp -d)
trap 'rm -rf "$out"; tmux -L leksah-cc-test kill-server 2>/dev/null || true' EXIT
ghc -O0 -threaded -Wall -isrc -outputdir "$out" -o "$out/tmux-cc-test" scripts/tmux-cc-test.hs
"$out/tmux-cc-test"
