#!/bin/sh -ex

echo $PATH
export LC_ALL=C.UTF-8

if [ -d .cabal ]; then cp -a .cabal /root; fi
if [ -d .ghc ]; then cp -a .ghc /root; fi

cabal --version
ghc --version
cabal update
cabal new-build ghcjs-dom || cabal new-build ghcjs-dom

# update the cache in case we time out later
rm -rf .cabal
cp -a /root/.cabal ./
rm -rf .ghc
cp -a /root/.ghc ./

cabal new-build || cabal new-build
cabal sdist

# update the cache
rm -rf .cabal
cp -a /root/.cabal ./
rm -rf .ghc
cp -a /root/.ghc ./