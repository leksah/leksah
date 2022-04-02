#!/usr/bin/env bash -e

if [ $# -eq 0 ]
  then
    echo "Usage: ./leksah-nix.sh GHCVER [LEKSAH_ARGS]"
    echo
    echo "Examples: ./leksah-nix.sh ghc8107"
    echo "          ./leksah-nix.sh ghc884 --verbosity=DEBUG"
    echo
    echo "For details of other LEKSAH_ARGS run: ./leksah-nix ghc865 --help"
    exit 1
fi

GHCARG=$1
shift

if [[ "$GHCARG" != "ghc865" && "$GHCARG" != "ghc884" && "$GHCARG" != "ghc8105" && "$GHCARG" != "ghc8107" ]]; then
    echo "Please use ./leksah-nix.sh ghc865 or ghc884 or ghc8107"
    exit 1
fi

# NIX_ARGS='--system x86_64-darwin'
LAUNCH_LEKSAH=`nix-build $NIX_ARGS --argstr compiler-nix-name "$GHCARG" -A launch-leksah`/bin/launch-leksah

LEKSAH_EXIT_CODE=2
while [ $LEKSAH_EXIT_CODE -eq 2 ]; do
  rm -f .ghc.environment.* cabal.project.local
  mkdir -p bin
  # $LAUNCH_LEKSAH
  nix-shell $NIX_ARGS --show-trace -j 4 --cores 5 --argstr compiler-nix-name "$GHCARG" --run \
    "cabal v2-install --installdir bin/$GHCARG --overwrite-policy=always exe:leksah-server exe:leksah exe:leksahecho exe:vcswrapper exe:vcsgui exe:vcsgui-askpass" \
      || read -n 1 -s -r -p "Build failed.  Press any key to attempt to run last built version."
  rm -f .ghc.environment.* cabal.project.local

  PATH=$(pwd)/bin/$GHCARG:$PATH $LAUNCH_LEKSAH ./bin/$GHCARG/leksah --develop-leksah $@
  LEKSAH_EXIT_CODE=$?
done

