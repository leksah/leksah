#!/bin/sh

cabal install gtk -fhave-quartz-gtk

. scripts/stage.sh || exit

sed -e 's|TextView Font: *\"Monospace 10\"|TextView Font: "Monospace 14"|' -e 's|Browser: *\"firefox\"|Browser:       \"open\"|' <data/prefs.lkshp >osx/prefs.lkshp

sed -e 's|\<ctrl\>|\<meta\>|' \
    -e 's|\<alt\>|\<control\>|' \
    -e 's|^--\<meta\>x |\<meta\>x |' \
    -e 's|^--\<meta\>c |\<meta\>c |' \
    -e 's|^--\<meta\>v |\<meta\>v |' \
    <data/keymap.lkshk >osx/keymap.lkshk
cat osx/osxkeymap.lkshk >>osx/keymap.lkshk

cd osx || exit

chmod +x bundle.sh launcher.sh || exit

echo Bundle Leksah
if test -e "Leksah"; then
    rm -rf "Leksah"
fi
DYLD_LIBRARY_PATH= jhbuild run ./bundle.sh || exit

echo Fixing library bindings
mv Leksah/Leksah.app/Contents/MacOS/Leksah-bin Leksah/Leksah.app/Contents/Resources/bin/leksah || exit
for f in `find Leksah/Leksah.app/Contents/Resources/lib -name '*.so' -o -name '*.dylib'` Leksah/Leksah.app/Contents/Resources/bin/*; do
    otool -L $f | grep "$GTK_PREFIX/.*(" | sed -e "s|.*$GTK_PREFIX/\([^ ]*\).*|\1|" | xargs -I {} install_name_tool -change $GTK_PREFIX/{} @executable_path/../{} $f || exit
done

echo Fixing pixbuf loader paths
sed -i "" -e "s|@executable_path/../Resources/|@executable_path/../|" Leksah/Leksah.app/Contents/Resources/etc/gtk-2.0/gdk-pixbuf.loaders || exit

LEKSAH_DMG="$LEKSAH_X_X_X_X_GHC_X_X_X.dmg"
if test -e "$LEKSAH_DMG"; then
   rm "$LEKSAH_DMG"
fi
hdiutil create -srcfolder "Leksah" "$LEKSAH_DMG" || exit

