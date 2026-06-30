#!/usr/bin/env bash
# Trigger a dev relaunch of a running leksah web UI that was started by
# leksah-nix.sh with --develop-leksah.
#
# leksah (in develop-leksah mode) polls for a request file and, when it appears,
# exits with code 2 -- exactly as rebuilding the leksah package from inside the
# IDE does -- so leksah-nix.sh's loop rebuilds and relaunches it.  Run this after
# a *successful* `cabal build` so a broken build never tears down the session.
#
# Usage: ./dev-relaunch.sh [wkwebview|warp|webkitgtk]   (the arg is only used in
# the message; the trigger file relaunches whichever dev leksah is running.)
set -euo pipefail

UI="${1:-wkwebview}"

TRIGGER="$HOME/.leksah/relaunch-request"
mkdir -p "$HOME/.leksah"
: > "$TRIGGER"

# Belt-and-suspenders: also send SIGUSR1 in case a build wires that up.
pkill -USR1 -f "build/leksah-$UI/leksah-$UI" 2>/dev/null || true

echo "Requested relaunch via $TRIGGER -- leksah will exit(2); leksah-nix.sh rebuilds and relaunches."
