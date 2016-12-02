#!/bin/sh

cabal new-build -j4 exe:leksah-server exe:leksah exe:leksahecho || exit
PATH=`pwd`/dist-newstyle/build/leksah-server-0.16.2.0/build/leksah-server:`pwd`/dist-newstyle/build/leksah-server-0.16.2.0/build/leksahecho:$PATH leksah_datadir=`pwd` ./dist-newstyle/build/leksah-0.16.2.0/build/leksah/leksah $@

