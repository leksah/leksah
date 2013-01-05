#!/bin/sh -ex

ghc-pkg unregister --force leksah
ghc-pkg unregister --force leksah-server
ghc-pkg unregister --force ltk
ghc-pkg unregister --force yi
ghc-pkg unregister --force gtksourceview2
ghc-pkg unregister --force gtk-mac-integration
ghc-pkg unregister --force gtk
ghc-pkg unregister --force pango
ghc-pkg unregister --force cairo
ghc-pkg unregister --force gio
ghc-pkg unregister --force glib

rm -rf vendor/*/dist \
       vendor/gtk2hs/*/dist \
       vendor/yi/*/dist \
       vendor/*/vendor/*/dist \
       vendor/*/vendor/gtk2hs/*/dist

cabal clean || exit
