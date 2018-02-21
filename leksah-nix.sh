#!/bin/sh

# In case this is set to MacPorts or something
export XDG_DATA_DIRS=

nix-shell --show-trace -j 4 --cores 5 -A shells.ghc --run '. ./leksah.sh '$@ || exit
