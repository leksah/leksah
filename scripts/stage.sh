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

export GTK_PREFIX=`pkg-config --libs-only-L gtk+-3.0 | sed 's|^-L||' | sed 's|/lib *$||'`

echo Staging Leksah in $GTK_PREFIX

# These don't like all the extra options we pass (CPPFLAGS and --extra-lib-dirs)
cabal install text || true
cabal install parsec || true
cabal install network --constraint='text>=0.11.3.1' --constraint='parsec>=3.1.3' || true
cabal install uniplate --constraint='text>=0.11.3.1' --constraint='parsec>=3.1.3' || true

# Needed for installing curl package on windows
export CPPFLAGS=`pkg-config --cflags-only-I libcurl`

# Only used by OS X
# export DYLD_LIBRARY_PATH="/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/ImageIO.framework/Versions/A/Resources:$GTK_PREFIX/lib:$DYLD_LIBRARY_PATH"

# Gtk2Hs needs the latest Cabal to install properly
cabal install Cabal || true

echo darcs:http://code.haskell.org/gtksourceview > sources.txt
echo https://github.com/leksah/ltk >> sources.txt
echo https://github.com/leksah/leksah-server >> sources.txt
echo https://github.com/hamishmack/vado.git >> sources.txt
echo https://github.com/leksah/haskellVCSWrapper.git >> sources.txt
echo https://github.com/leksah/haskellVCSGUI.git >> sources.txt

if [ "$GHC_VER" != "7.0.3" ] && [ "$GHC_VER" != "7.0.4" ]; then
        echo https://github.com/leksah/pretty-show.git >> sources.txt
        echo darcs:http://code.haskell.org/webkit >> sources.txt
        echo git://github.com/ghcjs/webkit-javascriptcore.git >> sources.txt
        echo https://github.com/ghcjs/ghcjs-dom.git >> sources.txt
        echo https://github.com/ghcjs/jsc.git >> sources.txt
        echo https://github.com/ghcjs/CodeMirror.git >> sources.txt
fi

# echo ./vendor/gtk2hs >> sources.txt
echo ./ >> sources.txt

if test "`uname`" = "Darwin"; then
    if [ "$GHC_VER" != "7.0.3" ] && [ "$GHC_VER" != "7.0.4" ]; then
        cabal-meta install -fhave-quartz-gtk -flibcurl -fwebkit || exit
    else
        cabal-meta install -fhave-quartz-gtk -flibcurl -f-webkit || exit
    fi
else
    HPDIR=`which ghc` || exit
    HPDIR=`dirname "$HPDIR"` || exit
    HPDIR=`dirname "$HPDIR"` || exit
    if [ "$GHC_VER" != "7.0.3" ] && [ "$GHC_VER" != "7.0.4" ]; then
        cabal-meta install --extra-lib-dirs="$HPDIR"/mingw/lib --extra-lib-dirs=/c/MinGWRPM/lib -flibcurl -fwebkit || bash || exit
    else
        cabal-meta install --extra-lib-dirs="$HPDIR"/mingw/lib --extra-lib-dirs=/c/MinGWRPM/lib -flibcurl -f-webkit || bash || exit
    fi
#  if [ "$GHC_VER" != "7.0.3" ] && [ "$GHC_VER" != "7.0.4" ] && [ "$GHC_VER" != "7.6.1" ]; then
#    echo https://github.com/yi-editor/yi.git >> sources.txt
#    export LEKSAH_CONFIG_ARGS="$LEKSAH_CONFIG_ARGS -fyi -f-vty -f-dyre -fpango"
#  fi
fi

export SERVER_VERSION=`grep '^version: ' vendor/leksah-server/leksah-server.cabal | sed 's|version: ||' | tr -d '\r'`
export LEKSAH_SERVER_X_X_X_X=leksah-server-$SERVER_VERSION

