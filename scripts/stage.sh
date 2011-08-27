#!/bin/sh

export FULL_VERSION=`grep '^version: ' leksah.cabal | sed 's|version: ||'`
export SHORT_VERSION=`echo $FULL_VERSION | sed 's|\.[0-9]*\.[0-9]*$||'`
export LEKSAH_X_X_X_X=leksah-$FULL_VERSION
export LEKSAH_X_X=leksah-$SHORT_VERSION

export GHC_VER=`ghc --numeric-version`
export LEKSAH_X_X_X_X_GHC_X_X_X=leksah-$FULL_VERSION-ghc-$GHC_VER

export GTK_PREFIX=`pkg-config --libs-only-L gtk+-2.0 | sed 's|^-L||' | sed 's|/lib *$||'`
echo Staging Leksah in $GTK_PREFIX

# Only used by OS X
# export DYLD_LIBRARY_PATH="/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/ImageIO.framework/Versions/A/Resources:$GTK_PREFIX/lib:$DYLD_LIBRARY_PATH"

if [ "$GHC_VER" != "6.12.3" ]; then
  LEKSAH_YI_FLAGS="yi -dyre"
  cd ../yi/yi || exit
  cabal clean || exit
  cabal configure --flags="pango -vte -vty" --prefix=$GTK_PREFIX --extra-lib-dirs="$GTK_PREFIX/lib" || exit
  cabal build || exit
  cabal copy || exit
  cabal register || exit
  cd ..
fi

cd ../ltk || exit
cabal clean || exit
cabal configure || exit
cabal build || exit
cabal copy || exit
cabal register || exit

export LEKSAH_CONFIG_ARGS="--prefix="$GTK_PREFIX" --datadir="$GTK_PREFIX/share" --extra-lib-dirs="$GTK_PREFIX/lib" --datasubdir="$LEKSAH_X_X""

cd ../leksah-server || exit
cabal clean || exit
cabal configure --flags="libcurl" $LEKSAH_CONFIG_ARGS || exit
cabal build || exit
cabal copy || exit
cabal register || exit

cd ../leksah || exit
cabal clean || exit
cabal configure --flags="$LEKSAH_YI_FLAGS" $LEKSAH_CONFIG_ARGS || exit
cabal build || exit
cabal copy || exit
cabal register || exit
