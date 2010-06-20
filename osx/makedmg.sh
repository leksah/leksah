#!/bin/sh

. scripts/stage.sh || exit

sed 's|\<ctrl\>|\<meta\>|' <data/keymap.lkshk >osx/keymap.lkshk

cd osx || exit

chmod +x *.sh || exit

if test -e "Leksah"; then
    rm -rf "Leksah"
fi
jhbuild run ./bundle.sh || exit

LEKSAH_DMG="$LEKSAH_X_X_X_X.dmg"
if test -e "$LEKSAH_DMG"; then
   rm "$LEKSAH_DMG"
fi
hdiutil create -srcfolder "Leksah" "$LEKSAH_DMG" || exit

