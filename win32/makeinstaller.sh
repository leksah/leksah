#!/bin/sh -ex

if ["$WINE" -eq ""]
then
  WINEPATH_U=echo
  WINE_APPDATA=$APPDATA
  LN='cp -uH'
  LNDIR='cp -ruH'
else
  WINE_APPDATA=`$WINE cmd /C echo %APPDATA% | tr -d '\r\n'`
  LN='ln -s'
  LNDIR='ln -s'
fi

. scripts/stage.sh || exit

export CABAL_USER_BIN=`$WINEPATH_U "$WINE_APPDATA"/cabal/bin`
export GHC_USER_PREFIX=`$WINEPATH_U "$WINE_APPDATA"/cabal/x86_64-windows-ghc-\`$WINE ghc$GHCVERSION --numeric-version | tr -d '\r\n'\``
export LEKSAH_PREFIX=$GHC_USER_PREFIX
export LEKSAH_SERVER_PREFIX=$GHC_USER_PREFIX
export HLINT_X_X_X_X=`$WINE ghc-pkg$GHCVERSION list |grep '^ *hlint-' | tail -n1 | tr -d ' \r\n'`
export PRETTY_SHOW_X_X=`$WINE ghc-pkg$GHCVERSION list |grep '^ *pretty-show-' | tail -n1 | tr -d ' \r\n'`
export GHCJS_CODEMIRROR_X_X_X_X=`$WINE ghc-pkg$GHCVERSION list |grep '^ *ghcjs-codemirror-' | tail -n1 | tr -d ' \r\n'`
export GTK_PREFIX_U=`$WINEPATH_U "$GTK_PREFIX"`

cd win32 || exit

export PATH=$PATH:/c/Program\ Files\ \(x86\)/WiX\ Toolset\ v3.8/bin:/c/Program\ Files/WiX\ Toolset\ v3.8/bin
mkdir -p SourceDir
cp -u leksah.bat SourceDir
cp -u leksah-server.bat SourceDir
cp -u leksah-rebuild-metadata.bat SourceDir
$LNDIR etc SourceDir # has to be done before copying the gtk etc folder since this has the right settings.ini
$LNDIR "$GTK_PREFIX_U"/etc SourceDir
$LNDIR share SourceDir
mkdir -p SourceDir/leksah
$LNDIR ../data SourceDir/leksah
sed 's|\<ctrl\>q|\<alt\>F4|' <../data/keymap.lkshk >"SourceDir/leksah/data/keymap.lkshk" || exit
$LNDIR ../language-specs SourceDir/leksah
$LNDIR ../pics SourceDir/leksah
$LN  ../LICENSE SourceDir/leksah
$LN  ../Readme.md SourceDir/leksah
mkdir -p SourceDir/hlint
# $LNDIR "$LEKSAH_PREFIX/$HLINT_X_X_X_X"/* SourceDir/hlint
# TODO put this back once JavaScriptCore works win mingw webkitgtk
# mkdir -p SourceDir/pretty-show
# $LNDIR "$LEKSAH_PREFIX/$PRETTY_SHOW_X_X"/style SourceDir/pretty-show
# mkdir -p SourceDir/pretty-show/style
# $LN "$LEKSAH_PREFIX/$PRETTY_SHOW_X_X"/style/pretty-show.css SourceDir/pretty-show/style
# mkdir -p SourceDir/ghcjs-codemirror
# $LNDIR "$LEKSAH_PREFIX/$GHCJS_CODEMIRROR_X_X_X_X"/mode SourceDir/ghcjs-codemirror
# $LNDIR "$LEKSAH_PREFIX/$GHCJS_CODEMIRROR_X_X_X_X"/lib SourceDir/ghcjs-codemirror
# $LNDIR "$LEKSAH_PREFIX/$GHCJS_CODEMIRROR_X_X_X_X"/keymap SourceDir/ghcjs-codemirror
# $LNDIR "$LEKSAH_PREFIX/$GHCJS_CODEMIRROR_X_X_X_X"/theme SourceDir/ghcjs-codemirror
mkdir -p SourceDir/bin
$LN  "../dist-newstyle/build/x86_64-windows/ghc-$GHC_VER/$LEKSAH_X_X_X_X"/x/leksah/build/leksah/leksah.exe SourceDir/bin
$LN  "../dist-newstyle/build/x86_64-windows/ghc-$GHC_VER/vcswrapper-0.2.0"/x/vcsgui/build/vcswrapper/vcswrapper.exe SourceDir/bin
$LN  "../dist-newstyle/build/x86_64-windows/ghc-$GHC_VER/vcsgui-0.3.0.0"/x/vcsgui/build/vcsgui/vcsgui.exe SourceDir/bin
$LN  "../dist-newstyle/build/x86_64-windows/ghc-$GHC_VER/vcsgui-0.3.0.0"/x/vcsgui-askpass/build/vcsgui-askpass/vcsgui-askpass.exe SourceDir/bin
$LN  "../dist-newstyle/build/x86_64-windows/ghc-$GHC_VER/$LEKSAH_SERVER_X_X_X_X"/x/leksah-server/build/leksah-server/leksah-server.exe SourceDir/bin
$LN  "../dist-newstyle/build/x86_64-windows/ghc-$GHC_VER/$LEKSAH_SERVER_X_X_X_X"/x/leksahecho/build/leksahecho/leksahecho.exe SourceDir/bin
$LN  "../dist-newstyle/build/x86_64-windows/ghc-$GHC_VER/$LEKSAH_SERVER_X_X_X_X"/x/leksahtrue/build/leksahtrue/leksahtrue.exe SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libiconv-2.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libicu*.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libatk-1.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libbz2-1.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libcairo-2.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libcairo-gobject-2.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libdbus*.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libdatrie-1.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libenchant-2.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libepoxy-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libexpat-1.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libexslt-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libffi-6.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libfontconfig-1.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libfreetype-6.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libfribidi-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgailutil-3-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgcc_s_seh-1.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgdk-3-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgdk_pixbuf-2.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgeoclue-*.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgio-2.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libglib-2.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgmodule-2.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgmp-10.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgnutls-*.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgobject-2.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgraphite2.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgst*-1.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgthread-2.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgtk-3-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libgtksourceview-3.0-1.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libharfbuzz-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libharfbuzz-icu-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libhogweed-*.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libidn-*.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libintl-8.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libjavascriptcoregtk-3.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libjpeg-*.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/liblzma-*.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libnettle-*.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/liborc-*.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libp11-kit-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libpango-1.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libpangocairo-1.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libpangoft2-1.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libpangowin32-1.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libpcre-*.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libpixman-1-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libpng16-16.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libsoup-2.4-1.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libsqlite3-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libstdc++-6.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libtasn1-6.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libthai-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libxml2-2.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libxslt-1.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libwebkitgtk-3.0-0.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libwebp*.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/libwinpthread-1.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/zlib1.dll SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/pkg-config.exe SourceDir/bin
$LN  "$GTK_PREFIX_U"/bin/xsltproc.exe SourceDir/bin
mkdir -p SourceDir/share
$LNDIR "$GTK_PREFIX_U"/share/themes SourceDir/share
$LNDIR "$GTK_PREFIX_U"/share/icons SourceDir/share
$LNDIR "$GTK_PREFIX_U"/share/glib-2.0 SourceDir/share
$LNDIR "$GTK_PREFIX_U"/share/gtksourceview-3.0 SourceDir/share
mkdir -p SourceDir/libexec
$LNDIR "$GTK_PREFIX_U"/libexec/gstreamer-1.0 SourceDir/libexec

$LNDIR "$GTK_PREFIX_U"/include SourceDir
$LNDIR "$GTK_PREFIX_U"/lib SourceDir

strip SourceDir/bin/*.exe

# mkdir -p SourceDir/fonts
# cp -ru /c/Windows/Fonts/DejaVuS*.ttf SourceDir
export WINEPREFIX=~/.wine32
export WINEARCH=win32
if ["$WINE" -eq ""]
then
  heat dir SourceDir -srd -gg -sfrag -template fragment -out heat.wxs -cg Leksah -dr INSTALLDIR -sreg suppress registry harvesting || exit
  LIGHT=light
else
  find SourceDir -follow | wixl-heat -p SourceDir/ --component-group Leksah --directory-ref INSTALLDIR > heat.wxs || exit
  LIGHT='light -sval'
fi
xsltproc heat.xslt heat.wxs > heatfixed.wxs
$WINE candle heatfixed.wxs || exit
$WINE candle leksah.wxs || exit
$WINE $LIGHT -ext WixUIExtension heatfixed.wixobj leksah.wixobj -out $LEKSAH_X_X_X_X_GHC_X_X_X.msi || exit
