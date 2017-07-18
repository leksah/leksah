#!/bin/sh

# In case this is set to MacPorts or something
export XDG_DATA_DIRS=

nix-shell --show-trace -j 8 --command '. ./leksah.sh '$@ || exit
