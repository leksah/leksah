#!/bin/bash

LEKSAH_SERVER_VERSION='0.16.3.0'
LEKSAH_VERSION='0.16.3.0'

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
  export PATH="$LEKSAH_SERVER_BUILD_DIR/leksah-server:$LEKSAH_SERVER_BUILD_DIR/leksahecho:$PATH"
  RUN_LEKSAH="$LEKSAH_BUILD_DIR/leksah/leksah"
elif [[ "$(cabal --numeric-version | cut -f 1 -d '.')" = "2" ]]; then
  echo "Using Cabal 2"
  LEKSAH_SERVER_BUILD_DIR="$DIR/dist-newstyle/build/$ARCH-$OS/ghc-$GHCVER/leksah-server-$LEKSAH_SERVER_VERSION"
  LEKSAH_BUILD_DIR="$DIR/dist-newstyle/build/$ARCH-$OS/ghc-$GHCVER/leksah-$LEKSAH_VERSION"
  export PATH="$LEKSAH_SERVER_BUILD_DIR/c/leksah-server/build/leksah-server:$LEKSAH_SERVER_BUILD_DIR/c/leksah-server/build/leksahecho:$PATH"
  export PATH="$LEKSAH_SERVER_BUILD_DIR/x/leksah-server/build/leksah-server:$LEKSAH_SERVER_BUILD_DIR/x/leksah-server/build/leksahecho:$PATH"
  export PATH="$LEKSAH_BUILD_DIR/x/leksah/build/leksah:$LEKSAH_BUILD_DIR/c/leksah/build/leksah:$PATH"
  RUN_LEKSAH="leksah"
  echo $PATH
else
  echo "Unknown cabal version $(cabal --numeric-version)"
fi

export leksah_datadir="$DIR"

cabal new-configure --with-ghc="ghc-$GHCVER"
LEKSAH_EXIT_CODE=2
while [ $LEKSAH_EXIT_CODE -eq 2 ]; do
  cabal new-build exe:leksah-server exe:leksah exe:leksahecho || read -n 1 -s -r -p "Build failed.  Press any key to attempt to run last built version."
  $RUN_LEKSAH --develop-leksah $@
  LEKSAH_EXIT_CODE=$?
done

