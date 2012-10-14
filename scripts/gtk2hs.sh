#!/bin/sh

ghc-pkg unregister --force leksah
ghc-pkg unregister --force leksah-server
ghc-pkg unregister --force ltk
ghc-pkg unregister --force gtksourceview2
ghc-pkg unregister --force gtk-mac-integration
ghc-pkg unregister --force gtk
cabal install Cabal
cd ../gtk2hs/gtk || exit
cabal clean || exit
cabal install -fgtk3 -fhave-quartz-gtk
cd ../../gtksourceview || exit
cabal clean || exit
cabal install -fgtk3
cd ../ige-mac-integration || exit
cabal clean || exit
cabal install

cd ../leksah || exit


