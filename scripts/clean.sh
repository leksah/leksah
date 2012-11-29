#!/bin/sh -ex

rm -rf vendor/*/dist
rm -rf vendor/gtk2hs/*/dist

cabal clean || exit
