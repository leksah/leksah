#!/bin/sh

if [ ! -d cache1 ]; then
  cabal update
  cabal --version
  cabal new-build gi-cairo gi-gdk gi-gdkpixbuf gi-gio gi-glib gi-gobject gi-gtk gi-gtksource gi-javascriptcore gi-pango gi-webkit gi-atk gi-soup \
          -f-enable-overloading --disable-documentation --constraint='haskell-gi-overloading==0.*'
  mkdir cache1
  mv $APPDATA/ghc $APPDATA/cabal cache1
elif [ ! -d cache2 ]; then
  mv cache1/* $APPDATA
  cabal update
  cabal new-build gi-gtk-hs vendor/ltk --disable-documentation --constraint='haskell-gi-overloading==0.*'
  mkdir cache2
  mv $APPDATA/ghc $APPDATA/cabal cache2
elif [ ! -d cache3 ]; then
  mv cache2/* $APPDATA
  cabal update
  cabal new-build ./ vendor/ltk vendor/leksah-server vendor/haskellVCSWrapper/vcswrapper vendor/haskellVCSGUI/vcsgui -fwebkit -f-yi -fpango -f-vty --constraint='haskell-gi-overloading==0.*' --only-dependencies
  mkdir cache3
  mv $APPDATA/ghc $APPDATA/cabal cache3
else
  mv cache3/* $APPDATA
  cabal update
  ./win32/makeinstaller.sh
fi

