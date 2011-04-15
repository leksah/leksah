#!/bin/sh

. scripts/stage.sh || exit

sed -e 's|\<ctrl\>|\<meta\>|' -e 's|\<alt\>|\<control\>|' <data/keymap.lkshk >osx/keymap.lkshk
cat osx/osxkeymap.lkshk >>osx/keymap.lkshk
touch osx/prefs.lkshp

cd osx || exit

chmod +x *.sh || exit

if test -e "Leksah"; then
    rm -rf "Leksah"
fi
jhbuild run ./bundle.sh || exit

mv Leksah/Leksah.app/Contents/MacOS/Leksah-bin Leksah/Leksah.app/Contents/Resources/bin/leksah || exit
for f in `find Leksah/Leksah.app/Contents/Resources/lib -name '*.so' -o -name '*.dylib'` Leksah/Leksah.app/Contents/Resources/bin/*; do
    otool -L $f | grep "$GTK_PREFIX/.*(" | sed -e "s|.*$GTK_PREFIX/\([^ ]*\).*|\1|" | xargs -I {} install_name_tool -change $GTK_PREFIX/{} @executable_path/../{} $f || exit
done

sed -i "" -e "s|@executable_path/../Resources/|@executable_path/../|" Leksah/Leksah.app/Contents/Resources/etc/gtk-2.0/gdk-pixbuf.loaders || exit

LEKSAH_DMG="$LEKSAH_X_X_X_X.dmg"
if test -e "$LEKSAH_DMG"; then
   rm "$LEKSAH_DMG"
fi
hdiutil create -srcfolder "Leksah" "$LEKSAH_DMG" || exit

