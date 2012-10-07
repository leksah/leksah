#!/bin/sh

export FULL_VERSION=`grep '^version: ' leksah.cabal | sed 's|version: ||' | tr -d '\r'`
export SHORT_VERSION=`echo $FULL_VERSION | sed 's|\.[0-9]*\.[0-9]*$||'`
export LEKSAH_X_X_X_X=leksah-$FULL_VERSION
export LEKSAH_X_X=leksah-$SHORT_VERSION

export GHC_VER=`ghc --numeric-version`
export LEKSAH_X_X_X_X_GHC_X_X_X=leksah-$FULL_VERSION-ghc-$GHC_VER

export SERVER_VERSION=`grep '^version: ' ../leksah-server/leksah-server.cabal | sed 's|version: ||' | tr -d '\r'`
export LEKSAH_SERVER_X_X_X_X=leksah-server-$SERVER_VERSION

export GTK_PREFIX=`pkg-config --libs-only-L gtk+-3.0 | sed 's|^-L||' | sed 's|/lib *$||'`
echo Staging Leksah in $GTK_PREFIX

# Needed for installing curl package on windows
export CPPFLAGS=`pkg-config --cflags-only-I libcurl`

# Only used by OS X
# export DYLD_LIBRARY_PATH="/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/ImageIO.framework/Versions/A/Resources:$GTK_PREFIX/lib:$DYLD_LIBRARY_PATH"

# Gtk2Hs needs the latest Cabal to install properly
cabal install Cabal

#if [ "$GHC_VER" != "6.12.3" ]; then
#  LEKSAH_YI_FLAGS="yi -dyre"
#  cd ../yi/yi || exit
#  cabal clean || exit
#  cabal install --only-dependencies || exit
#  cabal configure --flags="pango -vte -vty" --extra-lib-dirs="$GTK_PREFIX/lib" || exit
#  cabal build || exit
#  cabal copy || exit
#  cabal register || exit
#  cd ..
#fi

cd ../ltk || exit
cabal clean || exit
cabal install --only-dependencies || exit
cabal configure || exit
cabal build || exit
cabal copy || exit
cabal register || exit

export LEKSAH_CONFIG_ARGS="--extra-lib-dirs="$GTK_PREFIX/lib" --datasubdir="$LEKSAH_X_X""

cd ../leksah-server || exit
cabal clean || exit
cabal install --flags="libcurl" --only-dependencies || exit
cabal configure --flags="libcurl" $LEKSAH_CONFIG_ARGS || exit
cabal build || exit
cabal copy || exit
cabal register || exit

cd ../leksah || exit
cabal clean || exit
cabal install --flags="$LEKSAH_YI_FLAGS" --only-dependencies || exit
cabal configure --flags="$LEKSAH_YI_FLAGS" $LEKSAH_CONFIG_ARGS || exit
cabal build || exit
cabal copy || exit
cabal register || exit
