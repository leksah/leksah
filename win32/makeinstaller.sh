#!/bin/sh

. scripts/stage.sh || exit

cd win32 || exit

export CURL_PREFIX=`pkg-config --libs-only-L libcurl | sed 's|^-L||' | sed 's|/lib *$||'`
export PATH=$PATH:/c/Program\ Files\ \(x86\)/NSIS:/c/Program\ Files/NSIS
makensis leksah.nsi

