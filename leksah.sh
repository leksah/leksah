#!/bin/sh

LEKSAH_SERVER_VERSION='0.16.2.0'
LEKSAH_VERSION='0.16.2.2'

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [[ "$(cabal --numeric-version | cut -f 1 -d '.')" == "1" ]]; then
  echo "Using Cabal 1"
  LEKSAH_SERVER_BUILD_DIR="$DIR"'/dist-newstyle/build/x86_64-osx/ghc-8.2.1/leksah-server-'"$LEKSAH_SERVER_VERSION"'/c/leksah-server/build'
  LEKSAH_BUILD_DIR="$DIR"'/dist-newstyle/build/x86_64-osx/ghc-8.2.1/leksah-'"$LEKSAH_VERSION"'/c/leksah/build'
elif [[ "$(cabal --numeric-version | cut -f 1 -d '.')" = "2" ]]; then
  echo "Using Cabal 2"
  LEKSAH_SERVER_BUILD_DIR="$DIR"'/dist-newstyle/build/leksah-server-'"$LEKSAH_SERVER_VERSION"'/build'
  LEKSAH_BUILD_DIR="$DIR"'/dist-newstyle/build/leksah-'"$LEKSAH_VERSION"'/build'
else
  echo "Unknown cabal version $(cabal --numeric-version)"
fi

cabal new-build -j4 exe:leksah-server exe:leksah exe:leksahecho || exit
PATH="$LEKSAH_SERVER_BUILD_DIR"'/leksah-server:'"$LEKSAH_SERVER_BUILD_DIR"'/leksahecho:'"$PATH" leksah_datadir=`pwd` "$LEKSAH_BUILD_DIR"'/leksah/leksah' $@
