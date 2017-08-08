#!/bin/sh

LEKSAH_SERVER_VERSION='0.16.2.0'
LEKSAH_VERSION='0.16.2.2'

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [[ "$(uname -s)" == "Darwin" ]]; then
  OS="osx"
else
  OS="linux"
fi

ARCH="$(uname -m)"

if [[ "$GHCVER" == "" ]]; then
  GHCVER="$(ghc --numeric-version)"
fi

if [[ "$(cabal --numeric-version | cut -f 1 -d '.')" == "1" ]]; then
  echo "Using Cabal 1"
  LEKSAH_SERVER_BUILD_DIR="$DIR/dist-newstyle/build/leksah-server-$LEKSAH_SERVER_VERSION/build"
  LEKSAH_BUILD_DIR="$DIR/dist-newstyle/build/leksah-$LEKSAH_VERSION/build"
elif [[ "$(cabal --numeric-version | cut -f 1 -d '.')" = "2" ]]; then
  echo "Using Cabal 2"
  LEKSAH_SERVER_BUILD_DIR="$DIR/dist-newstyle/build/$ARCH-$OS/ghc-$GHCVER/leksah-server-$LEKSAH_SERVER_VERSION/c/leksah-server/build"
  LEKSAH_BUILD_DIR="$DIR/dist-newstyle/build/$ARCH-$OS/ghc-$GHCVER/leksah-$LEKSAH_VERSION/c/leksah/build"
else
  echo "Unknown cabal version $(cabal --numeric-version)"
fi

cabal new-configure --with-ghc="ghc-$GHCVER"
cabal new-build -j4 exe:leksah-server exe:leksah exe:leksahecho || exit
PATH="$LEKSAH_SERVER_BUILD_DIR/leksah-server:$LEKSAH_SERVER_BUILD_DIR/leksahecho:$PATH" leksah_datadir="$DIR" "$LEKSAH_BUILD_DIR/leksah/leksah" $@
