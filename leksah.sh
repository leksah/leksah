#!/bin/sh

cabal new-build exe:leksah-server exe:leksah exe:leksahecho || exit
PATH=`pwd`/dist-newstyle/build/leksah-server-0.16.1.0/build/leksah-server:`pwd`/dist-newstyle/build/leksah-server-0.16.1.0/build/leksahecho:$PATH leksah_datadir=`pwd` dist-newstyle/build/leksah-0.16.1.0/build/leksah/leksah $@

