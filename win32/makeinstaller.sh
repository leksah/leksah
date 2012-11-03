#!/bin/sh

. scripts/stage.sh || exit

export GHC_USER_PREFIX=$HOME/AppData/Roaming/cabal
export LEKSAH_PREFIX=$GHC_USER_PREFIX
export LEKSAH_SERVER_PREFIX=$GHC_USER_PREFIX

sed 's|\<ctrl\>q|\<alt\>F4|' <data/keymap.lkshk >"$LEKSAH_PREFIX/$LEKSAH_X_X/keymap.lkshk"

cd win32 || exit

export CURL_PREFIX=`pkg-config --libs-only-L libcurl | sed 's|^-L||' | sed 's|/lib *$||'`
export MINGW_BIN=`cmd //C echo \`dirname \\\`which gcc\\\`\``
export PATH=$PATH:/c/Program\ Files\ \(x86\)/NSIS:/c/Program\ Files/NSIS
makensis leksah.nsi

