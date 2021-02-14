#!/usr/bin/env bash

if [ $# -eq 0 ]
  then
    echo "Usage: ./leksah-nix.sh GHCVER [LEKSAH_ARGS]"
    echo
    echo "Examples: ./leksah-nix.sh ghc865"
    echo "          ./leksah-nix.sh ghc884 --verbosity=DEBUG"
    echo
    echo "For details of other LEKSAH_ARGS run: ./leksah-nix ghc865 --help"
    exit 1
fi

GHCARG=$1
shift

if [[ "$GHCARG" != "ghc865" && "$GHCARG" != "ghc883" && "$GHCARG" != "ghc884" && "$GHCARG" != "ghc8101" && "$GHCARG" != "ghc8102" && "$GHCARG" != "ghc8104" ]]; then
    echo "Please use ./leksah-nix.sh ghc865 or ghc884 or ghc8104"
    exit 1
fi

LAUNCH_LEKSAH=`nix-build --argstr compiler-nix-name "$GHCARG" -A launch-leksah`/bin/launch-leksah

LEKSAH_EXIT_CODE=2
while [ $LEKSAH_EXIT_CODE -eq 2 ]; do
  rm -f .ghc.environment.* cabal.project.local
  mkdir -p bin
  $LAUNCH_LEKSAH nix-shell --show-trace -j 4 --cores 5 --argstr compiler-nix-name "$GHCARG" --run \
    "cabal v2-install --installdir bin/$GHCARG --overwrite-policy=always exe:leksah-server exe:leksah exe:leksahecho exe:vcswrapper exe:vcsgui exe:vcsgui-askpass" \
      || read -n 1 -s -r -p "Build failed.  Press any key to attempt to run last built version."
  rm -f .ghc.environment.* cabal.project.local

  PATH=$(pwd)/bin/$GHCARG:$PATH $LAUNCH_LEKSAH ./bin/$GHCARG/leksah --develop-leksah $@
  LEKSAH_EXIT_CODE=$?
done

