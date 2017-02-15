#!/bin/sh

VERSION='0.16.2.2'
LEKSAH_SERVER_BUILD_DIR=`pwd`'/dist-newstyle/build/leksah-server-'"$VERSION"'/build'
LEKSAH_BUILD_DIR='dist-newstyle/build/leksah-'"$VERSION"'/build'

cabal new-build -j4 exe:leksah-server exe:leksah exe:leksahecho || exit
PATH="$LEKSAH_SERVER_BUILD_DIR"'/leksah-server:'"$LEKSAH_SERVER_BUILD_DIR"'/leksahecho:'"$PATH" leksah_datadir=`pwd` ./"$LEKSAH_BUILD_DIR"/leksah/leksah $@

