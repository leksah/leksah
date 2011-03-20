#!/bin/sh

. scripts/stage.sh || exit

sed -e 's|\<ctrl\>|\<meta\>|' -e 's|\<alt\>|\<control\>|' <data/keymap.lkshk >osx/keymap.lkshk
cat osx/osxkeymap.lkshk >>osx/keymap.lkshk

cd osx || exit

chmod +x *.sh || exit

if test -e "Leksah"; then
    rm -rf "Leksah"
fi
jhbuild run ./bundle.sh || exit

# sed -e "s|@executable_path/../||" -i "" Leksah/Leksah.app/Contents/Resources/etc/gtk-2.0/gdk-pixbuf.loaders || exit

LEKSAH_DMG="$LEKSAH_X_X_X_X.dmg"
if test -e "$LEKSAH_DMG"; then
   rm "$LEKSAH_DMG"
fi
hdiutil create -srcfolder "Leksah" "$LEKSAH_DMG" || exit

