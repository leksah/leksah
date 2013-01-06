#!/bin/sh

ghc-pkg unregister leksah || true
ghc-pkg unregister leksah-server || true
ghc-pkg unregister ltk || true

export FULL_VERSION=`grep '^version: ' leksah.cabal | sed 's|version: ||' | tr -d '\r'`
export SHORT_VERSION=`echo $FULL_VERSION | sed 's|\.[0-9]*\.[0-9]*$||'`
export LEKSAH_X_X_X_X=leksah-$FULL_VERSION
export LEKSAH_X_X=leksah-$SHORT_VERSION

export GHC_VER=`ghc --numeric-version`
export LEKSAH_X_X_X_X_GHC_X_X_X=leksah-$FULL_VERSION-ghc-$GHC_VER

if test "`uname`" = "Darwin"; then
  export GTK_PREFIX=`pkg-config --libs-only-L gtk+-3.0 | sed 's|^-L||' | sed 's|/lib *$||'`
else
  export GTK_PREFIX=`pkg-config --libs-only-L gtk+-2.0 | sed 's|^-L||' | sed 's|/lib *$||'`
fi
echo Staging Leksah in $GTK_PREFIX

# Needed for installing curl package on windows
export CPPFLAGS=`pkg-config --cflags-only-I libcurl`

# Only used by OS X
# export DYLD_LIBRARY_PATH="/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/ImageIO.framework/Versions/A/Resources:$GTK_PREFIX/lib:$DYLD_LIBRARY_PATH"

# Gtk2Hs needs the latest Cabal to install properly
cabal install Cabal

echo darcs:http://code.haskell.org/gtksourceview > sources.txt
echo https://github.com/leksah/ltk >> sources.txt
echo https://github.com/leksah/leksah-server >> sources.txt
echo https://github.com/leksah/haskellVCSWrapper.git >> sources.txt
echo https://github.com/leksah/haskellVCSGUI.git >> sources.txt
echo ./ >> sources.txt

export LEKSAH_CONFIG_ARGS="--extra-lib-dirs="$GTK_PREFIX/lib" --datasubdir="$LEKSAH_X_X" -flibcurl"
if test "`uname`" = "Darwin"; then
    export LEKSAH_CONFIG_ARGS="$LEKSAH_CONFIG_ARGS -fgtk3 -fhave-quartz-gtk"
else
  if [ "$GHC_VER" != "7.0.3" ] && [ "$GHC_VER" != "7.0.4" ] && [ "$GHC_VER" != "7.6.1" ]; then
    echo https://github.com/yi-editor/yi.git >> sources.txt
    export LEKSAH_CONFIG_ARGS="$LEKSAH_CONFIG_ARGS -fyi -f-vty -f-dyre -fpango"
  fi
fi

cabal-meta install $LEKSAH_CONFIG_ARGS || exit

export SERVER_VERSION=`grep '^version: ' vendor/leksah-server/leksah-server.cabal | sed 's|version: ||' | tr -d '\r'`
export LEKSAH_SERVER_X_X_X_X=leksah-server-$SERVER_VERSION

