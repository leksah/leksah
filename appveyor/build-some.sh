#!/bin/sh

if [ ! -d cache1 ]; then
  cabal update
  cabal install alex happy --disable-documentation
  cabal install haskell-gi --disable-documentation
  cabal install gi-cairo gi-gdk gi-gdkpixbuf gi-gio gi-glib gi-gobject gi-gtk gi-gtksource gi-javascriptcore gi-pango gi-webkit gi-atk gi-soup -f-overloaded-methods -f-overloaded-signals -f-overloaded-properties
  mkdir cache1
  mv $APPDATA/ghc $APPDATA/cabal cache1
elif [ ! -d cache2 ]; then
  mv cache1/* $APPDATA
  cabal update
  cabal install vendor/gi-gtk-hs vendor/ltk vendor/jsaddle-dom vendor/ghcjs-dom --force-reinstalls
  mkdir cache2
  mv $APPDATA/ghc $APPDATA/cabal cache2
elif [ ! -d cache3 ]; then
  mv cache2/* $APPDATA
  cabal update
  cabal install ./ vendor/ltk vendor/leksah-server vendor/haskellVCSWrapper/vcswrapper vendor/haskellVCSGUI/vcsgui -fwebkit -f-yi -fpango -f-vty --only-dependencies
  mkdir cache3
  mv $APPDATA/ghc $APPDATA/cabal cache3
else
  mv cache3/* $APPDATA
  cabal update
  ./win32/makeinstaller.sh
fi

