#!/bin/sh -ex

echo $PATH
export LC_ALL=C.UTF-8

if [ -d .cabal ]; then cp -a .cabal /root; fi
if [ -d .ghc ]; then cp -a .ghc /root; fi

cabal --version
ghc --version
cabal update
cabal install aeson aeson-pretty
cabal install vendor/haskell-gi vendor/haskell-gi-base

# update the cache in case we time out later
rm -rf .cabal
cp -a /root/.cabal ./
rm -rf .ghc
cp -a /root/.ghc ./

cd vendor/haskell-gi/bindings
./PKGS.sh
for a in `./PKGS.sh`; do ./genBuildInfo.hs $a; done
cd ../../..
cp cabal.project.gi-bindings cabal.project
cabal new-build
cabal sdist

# update the cache
rm -rf .cabal
cp -a /root/.cabal ./
rm -rf .ghc
cp -a /root/.ghc ./