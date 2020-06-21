#!/usr/bin/env bash

case "$(cabal --numeric-version | cut -f 1 -d '.')" in
    1|2)
      echo ERROR : Please upgrade to cabal-install 3
      exit 0
      ;;

  3)
      ;;

  *)
      echo "Unknown cabal version $(cabal --numeric-version)"
      ;;
esac

LEKSAH_EXIT_CODE=2
while [ $LEKSAH_EXIT_CODE -eq 2 ]; do
  cabal v2-install --installdir bin exe:leksah-server exe:leksah exe:leksahecho || read -n 1 -s -r -p "Build failed.  Press any key to attempt to run last built version."
  ./bin/leksah --develop-leksah $@
  LEKSAH_EXIT_CODE=$?
done
