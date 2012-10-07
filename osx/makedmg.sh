#!/bin/sh

ghc-pkg unregister --force gtk
cabal install Cabal
cd ../gtk2hs/gtk || exit
cabal clean || exit
cabal install -fgtk3 -fhave-quartz-gtk
cd ../../gtksourceview || exit
cabal clean || exit
cabal install
cd ../ige-mac-integration || exit
cabal clean || exit
cabal install

cd ../leksah || exit

. scripts/stage.sh || exit

export GHC_USER_PREFIX=$HOME/Library/Haskell/ghc-`ghc --numeric-version`/lib
export LEKSAH_PREFIX=$GHC_USER_PREFIX/$LEKSAH_X_X_X_X
export LEKSAH_SERVER_PREFIX=$GHC_USER_PREFIX/$LEKSAH_SERVER_X_X_X_X

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
sed -i "" -e "s|@executable_path/../Resources/|@executable_path/../|" Leksah/Leksah.app/Contents/Resources/etc/gtk-3.0/gdk-pixbuf.loaders || exit

echo Fixing pango module paths
sed -i "" -e "s|@executable_path/../Resources/|@executable_path/../|" Leksah/Leksah.app/Contents/Resources/etc/pango/pango.modules || exit

LEKSAH_DMG="$LEKSAH_X_X_X_X_GHC_X_X_X.dmg"
if test -e "$LEKSAH_DMG"; then
   rm "$LEKSAH_DMG"
fi
hdiutil create -srcfolder "Leksah" "$LEKSAH_DMG" || exit

