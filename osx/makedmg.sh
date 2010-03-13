#!/bin/sh

cd ../leksah-server || exit
cabal clean || exit
cabal install --prefix="$HOME/gtk/inst" --extra-lib-dirs="$HOME/gtk/inst/lib" || exit

cd ../leksah
cabal clean || exit
cabal install --prefix="$HOME/gtk/inst" --extra-lib-dirs="$HOME/gtk/inst/lib" || exit

cd osx || exit

if test -e "$HOME/Desktop/Leksah"; then
    rm -rf "$HOME/Desktop/Leksah"
fi
jhbuild run ./bundle.sh || exit

LEKSAH_DMG="$HOME/leksah/leksah-web/httpdocs/Leksah-$1.dmg"
if test -e "$LEKSAH_DMG"; then
   rm "$LEKSAH_DMG"
fi
hdiutil create -srcfolder "$HOME/Desktop/Leksah" "$LEKSAH_DMG" || exit

