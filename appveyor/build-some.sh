#!/bin/sh

if [ ! -d cache1 ]; then
  cabal update
  cabal install -j1 alex happy --disable-documentation
  cabal install -j1 haskell-gi --disable-documentation
  cabal install -j1 gi-cairo gi-gdk gi-gdkpixbuf gi-gio gi-glib gi-gobject gi-gtk gi-gtksource gi-javascriptcore gi-pango gi-webkit gi-atk gi-soup \
          -f-enable-overloading --disable-documentation --constraint='haskell-gi-overloading==0.*'
  mkdir cache1
  mv $APPDATA/ghc $APPDATA/cabal cache1
elif [ ! -d cache2 ]; then
  mv cache1/* $APPDATA
  cabal update
  cabal install -j1 gi-gtk-hs vendor/ltk ghcjs-dom --disable-documentation --force-reinstalls --constraint='haskell-gi-overloading==0.*'
  mkdir cache2
  mv $APPDATA/ghc $APPDATA/cabal cache2
elif [ ! -d cache3 ]; then
  mv cache2/* $APPDATA
  cabal update
  cabal install -j1 ./ vendor/ltk vendor/leksah-server vendor/haskellVCSWrapper/vcswrapper vendor/haskellVCSGUI/vcsgui -fwebkit -f-yi -fpango -f-vty --only-dependencies
  mkdir cache3
  mv $APPDATA/ghc $APPDATA/cabal cache3
else
  mv cache3/* $APPDATA
  cabal update
  ./win32/makeinstaller.sh
fi

