#!/bin/sh -ex

rm -rf vendor/*/dist \
       vendor/gtk2hs/*/dist \
       vendor/*/vendor/*/dist \
       vendor/*/vendor/gtk2hs/*/dist

cabal clean || exit
