#!/bin/sh

#  cd ../yi/yi || exit
#  cabal clean || exit

cd ../ltk || exit
cabal clean || exit

cd ../leksah-server || exit
cabal clean || exit

cd ../leksah || exit
cabal clean || exit
