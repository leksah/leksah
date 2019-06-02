#!/usr/bin/env bash

if [ $# -eq 0 ]
  then
    echo "Usage: ./leksah-nix.sh GHCVER [LEKSAH_ARGS]"
    echo
    echo "Examples: ./leksah-nix.sh ghc843"
    echo "          ./leksah-nix.sh ghc822 --verbosity=DEBUG"
    echo
    echo "For details of other LEKSAH_ARGS run: ./leksah-nix ghc822 --help"
    exit 1
fi

GHCARG=$1
shift

if [ "$GHCARG" != "ghc864" ]
  then
    echo "Please use ./leksah-nix.sh ghc864 for now (support for othe ghc versions needs more work)"
    exit 1
fi

LEKSAH_SERVER_VERSION=`grep '^version: ' vendor/leksah-server/leksah-server.cabal | sed 's|version: *||' | tr -d '\r'`
# CABAL_HELPER_VERSION=`grep '^version: ' vendor/HaRe/submodules/cabal-helper/cabal-helper.cabal | sed 's|version: *||' | tr -d '\r'`
LEKSAH_VERSION=`grep '^version: ' leksah.cabal | sed 's|version: ||' | tr -d '\r'`
GHCNIX=`nix-shell --argstr haskellCompiler "$GHCARG" --run 'which ghc'`
if [ "$GHCNIX" == "" ]
  then
    echo "$GHCARG not found"
    exit 1
fi
GHCVER=`$GHCNIX --numeric-version`
LAUNCH_LEKSAH=`nix-build --argstr haskelCompiler "$GHCARG" -A nix-tools._raw.launch-leksah`/bin/launch-leksah

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [[ "$(uname -s)" == "Darwin" ]]; then
  OS="osx"
else
  OS="linux"
fi

ARCH="$(uname -m)"

LEKSAH_SERVER_BUILD_DIR="$DIR/dist-newstyle/build/$ARCH-$OS/ghc-$GHCVER/leksah-server-$LEKSAH_SERVER_VERSION"
# CABAL_HELPER_BUILD_DIR="$DIR/dist-newstyle/build/$ARCH-$OS/ghc-$GHCVER/cabal-helper-$CABAL_HELPER_VERSION"
LEKSAH_BUILD_DIR="$DIR/dist-newstyle/build/$ARCH-$OS/ghc-$GHCVER/leksah-$LEKSAH_VERSION"
export PATH="$LEKSAH_SERVER_BUILD_DIR/c/leksah-server/build/leksah-server:$LEKSAH_SERVER_BUILD_DIR/c/leksahecho/build/leksahecho:$PATH"
export PATH="$LEKSAH_SERVER_BUILD_DIR/x/leksah-server/build/leksah-server:$LEKSAH_SERVER_BUILD_DIR/x/leksahecho/build/leksahecho:$PATH"
# export PATH="$CABAL_HELPER_BUILD_DIR/x/cabal-helper-wrapper/build/cabal-helper-wrapper:$PATH"
export PATH="$LEKSAH_BUILD_DIR/x/leksah/build/leksah:$LEKSAH_BUILD_DIR/c/leksah/build/leksah:$PATH"
RUN_LEKSAH="$LEKSAH_BUILD_DIR/x/leksah/build/leksah/leksah"

export leksah_datadir="$DIR"
export leksah_server_datadir="$DIR/vendor/leksah-server"
export vcsgui_datadir="$DIR/vendor/haskellVCSGUI/vcsgui"

rm -f .ghc.environment.* cabal.project.local
$LAUNCH_LEKSAH nix-shell --show-trace -j 4 --cores 5 --argstr haskellCompiler "$GHCARG" --run \
  'cabal new-build exe:leksah-server exe:leksah exe:leksahecho exe:vcswrapper exe:vcsgui exe:vcsgui-askpass' \
    || read -n 1 -s -r -p "Build failed.  Press any key to attempt to run last built version."
rm -f .ghc.environment.* cabal.project.local

LEKSAH_EXIT_CODE=2
while [ $LEKSAH_EXIT_CODE -eq 2 ]; do
  hash -r
  $LAUNCH_LEKSAH $RUN_LEKSAH --develop-leksah $@
  LEKSAH_EXIT_CODE=$?
done

