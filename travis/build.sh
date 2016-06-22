#!/bin/bash -ex

echo $PATH
export LC_ALL=C.UTF-8

if [[ -d .cabal && -d .ghc ]]; then
    cp -a .cabal .ghc /root
fi

cabal update

if ghc-pkg --package-db /root/.cabal/store/ghc-`ghc --numeric-version`/package.db list | grep ghcjs-dom; then
    cabal new-build
else
    cabal new-build ghcjs-dom
fi

# update the cache
rm -rf .cabal
cp -a /root/.cabal ./
rm -rf .ghc
cp -a /root/.ghc ./