#!/bin/bash

LEKSAH_SERVER_VERSION=`grep '^version: ' vendor/leksah-server/leksah-server.cabal | sed 's|version: ||' | tr -d '\r'`
LEKSAH_VERSION=`grep '^version: ' leksah.cabal | sed 's|version: ||' | tr -d '\r'`
GHCVER=8.0.2

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [[ "$(uname -s)" == "Darwin" ]]; then
  OS="osx"
else
  OS="linux"
fi

ARCH="$(uname -m)"

LEKSAH_SERVER_BUILD_DIR="$DIR/dist-newstyle/build/$ARCH-$OS/ghc-$GHCVER/leksah-server-$LEKSAH_SERVER_VERSION"
LEKSAH_BUILD_DIR="$DIR/dist-newstyle/build/$ARCH-$OS/ghc-$GHCVER/leksah-$LEKSAH_VERSION"
export PATH="$LEKSAH_SERVER_BUILD_DIR/c/leksah-server/build/leksah-server:$LEKSAH_SERVER_BUILD_DIR/c/leksahecho/build/leksahecho:$PATH"
export PATH="$LEKSAH_SERVER_BUILD_DIR/x/leksah-server/build/leksah-server:$LEKSAH_SERVER_BUILD_DIR/x/leksahecho/build/leksahecho:$PATH"
export PATH="$LEKSAH_BUILD_DIR/x/leksah/build/leksah:$LEKSAH_BUILD_DIR/c/leksah/build/leksah:$PATH"
RUN_LEKSAH="$LEKSAH_BUILD_DIR/c/leksah/build/leksah/leksah"

export leksah_datadir="$DIR"

nix-shell --show-trace -j 4 --cores 5 -A shells.ghc --run 'cabal new-build exe:leksah-server exe:leksah exe:leksahecho' || read -n 1 -s -r -p "Build failed.  Press any key to attempt to run last built version."

LEKSAH_EXIT_CODE=2
while [ $LEKSAH_EXIT_CODE -eq 2 ]; do
  hash -r
  launch-leksah $RUN_LEKSAH --develop-leksah $@
  LEKSAH_EXIT_CODE=$?
done

