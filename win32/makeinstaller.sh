#!/bin/sh

. scripts/stage.sh || exit

export CABAL_USER_BIN=$HOME/AppData/Roaming/cabal/bin
export GHC_USER_PREFIX=$HOME/AppData/Roaming/cabal/x86_64-windows-ghc-`ghc$GHCVERSION --numeric-version`
export LEKSAH_PREFIX=$GHC_USER_PREFIX
export LEKSAH_SERVER_PREFIX=$GHC_USER_PREFIX
export HLINT_X_X_X_X=`ghc-pkg$GHCVERSION list |grep '^ *hlint-' | tail -n1 | tr -d ' \n'`
export PRETTY_SHOW_X_X=`ghc-pkg$GHCVERSION list |grep '^ *pretty-show-' | tail -n1 | tr -d ' \n'`
export GHCJS_CODEMIRROR_X_X_X_X=`ghc-pkg$GHCVERSION list |grep '^ *ghcjs-codemirror-' | tail -n1 | tr -d ' \n'`

sed 's|\<ctrl\>q|\<alt\>F4|' <data/keymap.lkshk >"$LEKSAH_PREFIX/$LEKSAH_X_X_X_X/data/keymap.lkshk" || exit

cd win32 || exit

export CURL_PREFIX=`pkg-config --libs-only-L libcurl | sed 's|^-L||' | sed 's|/lib *$||'`
export MINGW_BIN=`cmd //C echo \`dirname \\\`which gcc\\\`\``
export PATH=$PATH:/c/Program\ Files\ \(x86\)/NSIS:/c/Program\ Files/NSIS
#makensis leksah.nsi

export PATH=$PATH:/c/Program\ Files\ \(x86\)/WiX\ Toolset\ v3.8/bin:/c/Program\ Files/WiX\ Toolset\ v3.8/bin
mkdir -p SourceDir
cp -ru leksah.bat SourceDir
cp -ru leksah-server.bat SourceDir
cp -ru leksah-rebuild-metadata.bat SourceDir
cp -ru "$GTK_PREFIX"/etc SourceDir
mkdir -p SourceDir/leksah
cp -ru "$LEKSAH_PREFIX/$LEKSAH_X_X_X_X"/data SourceDir/leksah
cp -ru "$LEKSAH_PREFIX/$LEKSAH_X_X_X_X"/language-specs SourceDir/leksah
cp -ru "$LEKSAH_PREFIX/$LEKSAH_X_X_X_X"/pics SourceDir/leksah
cp -u  "$LEKSAH_PREFIX/$LEKSAH_X_X_X_X"/LICENSE SourceDir/leksah
cp -u  "$LEKSAH_PREFIX/$LEKSAH_X_X_X_X"/Readme.md SourceDir/leksah
mkdir -p SourceDir/hlint
cp -u  "$LEKSAH_PREFIX/$HLINT_X_X_X_X"/* SourceDir/hlint
mkdir -p SourceDir/pretty-show
cp -ru "$LEKSAH_PREFIX/$PRETTY_SHOW_X_X"/style SourceDir/pretty-show
mkdir -p SourceDir/ghcjs-codemirror
cp -ru "$LEKSAH_PREFIX/$GHCJS_CODEMIRROR_X_X_X_X"/mode SourceDir/ghcjs-codemirror
cp -ru "$LEKSAH_PREFIX/$GHCJS_CODEMIRROR_X_X_X_X"/lib SourceDir/ghcjs-codemirror
cp -ru "$LEKSAH_PREFIX/$GHCJS_CODEMIRROR_X_X_X_X"/keymap SourceDir/ghcjs-codemirror
cp -ru "$LEKSAH_PREFIX/$GHCJS_CODEMIRROR_X_X_X_X"/theme SourceDir/ghcjs-codemirror
mkdir -p SourceDir/bin
cp -u  "$CABAL_USER_BIN"/leksah.exe SourceDir/bin
cp -u  "$CABAL_USER_BIN"/vcswrapper.exe SourceDir/bin
cp -u  "$CABAL_USER_BIN"/vcsgui.exe SourceDir/bin
cp -u  "$CABAL_USER_BIN"/vcsgui-askpass.exe SourceDir/bin
cp -u  "$CABAL_USER_BIN"/leksah-server.exe SourceDir/bin
cp -u  "$CABAL_USER_BIN"/leksahecho.exe SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/iconv.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/icudata50.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/icui18n50.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/icule50.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/icuuc50.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libatk-1.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libbz2-1.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libcairo-2.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libcairo-gobject-2.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libcrypto-10.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libcurl-4.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libenchant.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libexpat-1.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libffi-6.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libfontconfig-1.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libfreetype-6.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libgailutil-3-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libgcc_s_sjlj-1.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libgdk-3-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libgdk_pixbuf-2.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libgio-2.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libglib-2.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libgmodule-2.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libgobject-2.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libgst*-1.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libgthread-2.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libgtk-3-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libgtksourceview-3.0-1.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libharfbuzz-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libharfbuzz-icu-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libidn-11.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libintl-8.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libjavascriptcoregtk-3.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libjpeg-62.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libpango-1.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libpangocairo-1.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libpangoft2-1.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libpangowin32-1.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libpixman-1-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libpng16-16.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libsoup-2.4-1.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libsqlite3-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libssh2-1.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libssl-10.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libstdc++-6.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libxml2-2.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libxslt-1.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libwebkitgtk-3.0-0.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libwebp-4.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/libwinpthread-1.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/zlib1.dll SourceDir/bin
cp -u  "$GTK_PREFIX"/bin/pkg-config.exe SourceDir/bin
mkdir -p SourceDir/share
cp -ru "$GTK_PREFIX"/share/themes SourceDir/share
cp -ru "$GTK_PREFIX"/share/glib-2.0 SourceDir/share
cp -ru "$GTK_PREFIX"/share/gtksourceview-3.0 SourceDir/share
mkdir -p SourceDir/libexec
cp -ru "$GTK_PREFIX"/libexec/gstreamer-1.0 SourceDir/libexec

cp -ru "$GTK_PREFIX"/include SourceDir
cp -ru "$GTK_PREFIX"/lib SourceDir

# mkdir -p SourceDir/fonts
# cp -ru /c/Windows/Fonts/DejaVuS*.ttf SourceDir

heat dir SourceDir -srd -gg -sfrag -template fragment -out heat.wxs -cg Leksah -dr INSTALLDIR -sreg suppress registry harvesting || exit
candle heat.wxs || exit
candle leksah.wxs || exit
light -ext WixUIExtension heat.wixobj leksah.wixobj -out $LEKSAH_X_X_X_X.msi || exit

