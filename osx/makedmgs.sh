#!/bin/sh -ex

export GHCVERSION=-7.8.3
scripts/clean.sh
osx/makedmg.sh
export GHCVERSION=-7.10.1
scripts/clean.sh
osx/makedmg.sh
export GHCVERSION=
scripts/clean.sh
osx/makedmg.sh

