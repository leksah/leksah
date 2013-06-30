#!/bin/sh

. scripts/stage.sh || exit

export GHC_USER_PREFIX=$HOME/AppData/Roaming/cabal
export LEKSAH_PREFIX=$GHC_USER_PREFIX
export LEKSAH_SERVER_PREFIX=$GHC_USER_PREFIX
export HLINT_X_X_X_X=`ghc-pkg list |grep '^ *hlint-' | tail -n1 | tr -d ' \n'`
export PRETTY_SHOW_X_X=`ghc-pkg list |grep '^ *pretty-show-' | tail -n1 | tr -d ' \n'`
export GHCJS_CODEMIRROR_X_X_X_X=`ghc-pkg list |grep '^ *ghcjs-codemirror-' | tail -n1 | tr -d ' \n'`

sed 's|\<ctrl\>q|\<alt\>F4|' <data/keymap.lkshk >"$LEKSAH_PREFIX/$LEKSAH_X_X_X_X/data/keymap.lkshk" || exit

cd win32 || exit

export CURL_PREFIX=`pkg-config --libs-only-L libcurl | sed 's|^-L||' | sed 's|/lib *$||'`
export MINGW_BIN=`cmd //C echo \`dirname \\\`which gcc\\\`\``
export PATH=$PATH:/c/Program\ Files\ \(x86\)/NSIS:/c/Program\ Files/NSIS
makensis leksah.nsi

