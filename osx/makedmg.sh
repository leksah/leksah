#!/bin/sh

chmod +x osx/*.sh || exit

cd ../yi || exit
cabal clean || exit
DYLD_LIBRARY_PATH="/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/ImageIO.framework/Versions/A/Resources:$HOME/gtk/inst/lib:$DYLD_LIBRARY_PATH" cabal install -f pango --prefix="$HOME/gtk/inst" --extra-lib-dirs="$HOME/gtk/inst/lib" || exit

cd ../leksah-server || exit
cabal clean || exit
cabal configure --prefix="$HOME/gtk/inst" --extra-lib-dirs="$HOME/gtk/inst/lib" --datasubdir="leksah-0.8" || exit
cabal build
runhaskell Setup.lhs install

cd ../leksah
cabal clean || exit
cabal configure -f yi --prefix="$HOME/gtk/inst" --extra-lib-dirs="$HOME/gtk/inst/lib" --datasubdir="leksah-0.8" || exit
cabal build
runhaskell Setup.lhs install

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

