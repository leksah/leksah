#!/bin/sh

ghc-pkg unregister --force leksah
ghc-pkg unregister --force leksah-server
ghc-pkg unregister --force ltk
ghc-pkg unregister --force gtksourceview2
ghc-pkg unregister --force gtk-mac-integration
ghc-pkg unregister --force gtk
cabal install Cabal

cd ../gtk2hs/gtk || exit
rm -rf dist
if test "`uname`" = "Darwin"; then
  cabal install -fgtk3 -fhave-quartz-gtk || exit
else
  cabal install || exit
fi

cd ../../gtksourceview || exit
rm -rf dist
if test "`uname`" = "Darwin"; then
  cabal install -fgtk3 || exit
else
  cabal install || exit
fi

if test "`uname`" = "Darwin"; then
  cd ../gtk-mac-integration || exit
  cabal clean || exit
  cabal install || exit
fi
cd ../leksah || exit


