#!/bin/sh -ex

cd ../leksah || exit

. scripts/stage.sh || exit

GHCVERSIONNUM=`ghc$GHCVERSION --numeric-version`

export CABAL_STORE=$HOME/.cabal/store/ghc-$GHCVERSIONNUM
export LEKSAH_BIN_DIR=`pwd`/dist-newstyle/build/x86_64-osx/ghc-$GHCVERSIONNUM/$LEKSAH_X_X_X_X
export LEKSAH_PREFIX=`pwd`
export LEKSAH_SERVER_BIN_DIR=`pwd`/dist-newstyle/build/x86_64-osx/ghc-$GHCVERSIONNUM/$LEKSAH_SERVER_X_X_X_X
export LEKSAH_SERVER_PREFIX=`pwd`/vendor/leksah-server
export VCSGUI_BIN_DIR=`pwd`/`ls -d dist-newstyle/build/x86_64-osx/ghc-$GHCVERSIONNUM/vcsgui-*`
export VCSGUI_PREFIX=`pwd`/vendor/haskellVCSGUI/vcsgui
export HLINT_PREFIX=$CABAL_STORE/`sed 's/^.*\(hlint-[0-9\.]*-[0-9a-f]*\).*$/\1/' < dist-newstyle/cache/plan.json`

#export SANDBOX_BIN_DIR=$PWD/.cabal-sandbox
#export SANDBOX_SHARE=$PWD/.cabal-sandbox/share/x86_64-osx-ghc-`ghc$GHCVERSION --numeric-version`
#export LEKSAH_BIN_DIR=$SANDBOX_BIN_DIR
#export LEKSAH_PREFIX=$SANDBOX_SHARE/$LEKSAH_X_X_X_X
#export LEKSAH_SERVER_BIN_DIR=$SANDBOX_BIN_DIR
#export LEKSAH_SERVER_PREFIX=$SANDBOX_SHARE/$LEKSAH_SERVER_X_X_X_X
#export VCSGUI_BIN_DIR=$SANDBOX_BIN_DIR
#export VCSGUI_PREFIX=$SANDBOX_SHARE/`ghc-pkg$GHCVERSION list |grep '^ *vcsgui-' | head -n1 | tr -d ' \n'`
#export HLINT_PREFIX=$SANDBOX_SHARE/`ghc-pkg$GHCVERSION list |grep '^ *hlint-' | head -n1 | tr -d ' \n'`

sed -e 's|TextView Font: *\"Monospace 10\"|TextView Font: "Monospace 14"|' -e 's|Browser: *\"firefox\"|Browser:       \"open\"|' <data/prefs.lkshp >osx/prefs.lkshp

sed -e 's|\<ctrl\>|\<meta\>|' \
    -e 's|\<alt\>|\<control\>|' \
    -e 's|^\<meta\>q |--\<meta\>q |' \
    <data/keymap.lkshk >osx/keymap.lkshk
cat osx/osxkeymap.lkshk >>osx/keymap.lkshk

cd osx || exit

chmod +x bundle.sh launcher.sh || exit

echo Bundle Leksah
if test -e "Leksah"; then
    rm -rf "Leksah"
fi
JHBUILD_PREFIX=/opt/local ./bundle.sh || exit

echo Fixing library bindings
mv Leksah/Leksah.app/Contents/MacOS/Leksah-bin Leksah/Leksah.app/Contents/Resources/bin/leksah || exit
for f in `find Leksah/Leksah.app/Contents/Resources/lib -name '*.so' -o -name '*.dylib'` Leksah/Leksah.app/Contents/Resources/bin/* Leksah/Leksah.app/Contents/Resources/libexec/*; do
    chmod +w $f
    otool -L $f | grep "$GTK_PREFIX/.*(" | sed -e "s|.*$GTK_PREFIX/\([^ ]*\).*|\1|" | xargs -I {} install_name_tool -change $GTK_PREFIX/{} @executable_path/../{} $f || exit
    chmod -w $f
done

for f in Leksah/Leksah.app/Contents/Resources/libexec/gst-plugin-scanner; do
    chmod +w $f
    otool -L $f | grep "$GTK_PREFIX/.*(" | sed -e "s|.*$GTK_PREFIX/\([^ ]*\).*|\1|" | xargs -I {} install_name_tool -change $GTK_PREFIX/{} @executable_path/../../{} $f || exit
    chmod -w $f
done

echo Fixing immodules paths
sed -i "" -e "s|/opt/local/|@executable_path/../|" Leksah/Leksah.app/Contents/Resources/etc/gtk-3.0/gtk.immodules || exit
sed -i "" -e "s|@executable_path/../Resources/|@executable_path/../|" Leksah/Leksah.app/Contents/Resources/etc/gtk-3.0/gtk.immodules || exit

echo Fixing pixbuf loader paths
sed -i "" -e "s|@executable_path/../Resources/|@executable_path/../|" Leksah/Leksah.app/Contents/Resources/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache || exit

LEKSAH_DMG="$LEKSAH_X_X_X_X_GHC_X_X_X.dmg"
if test -e "$LEKSAH_DMG"; then
   rm "$LEKSAH_DMG"
fi
hdiutil create -size 800m -srcfolder "Leksah" "$LEKSAH_DMG" || exit


