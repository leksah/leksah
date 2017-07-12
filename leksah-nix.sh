#!/bin/sh

nix-shell --show-trace -j 8 --command './leksah.sh '$@ || exit
