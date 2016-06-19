#!/bin/sh

if [ ! -d cache1 ]; then
  cabal update
  cabal install alex happy aeson-pretty --disable-documentation
  cabal install vendor/haskell-gi-base vendor/haskell-gi --force-reinstalls
  cd vendor/haskell-gi/bindings
  ghc --make genBuildInfo.hs
  for a in `./PKGS.sh`; do ./genBuildInfo.hs $a; done
  cabal install ./cairo ./Gdk ./GdkPixbuf ./Gio ./GLib ./GObject ./Gtk ./GtkSource ./JavaScriptCore-3.0 ./Pango ./WebKit ./Atk ./Soup -f-overloaded-methods -f-overloaded-signals -f-overloaded-properties
  cd ../../..
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

