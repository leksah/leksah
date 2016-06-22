#!/bin/bash -ex

echo $PATH
export LC_ALL=C.UTF-8

if [[ -d .cabal && -d .ghc ]]; then
    cp -a .cabal .ghc /root
    cabal update
    cabal new-build
else
    cabal update
    cabal new-build ghcjs-dom
fi

# update the cache
rm -rf .cabal
cp -a /root/.cabal ./
rm -rf .ghc
cp -a /root/.ghc ./