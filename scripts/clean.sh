#!/bin/sh -ex

ghc-pkg$GHCVERSION unregister --force leksah || true
ghc-pkg$GHCVERSION unregister --force leksah-server || true
ghc-pkg$GHCVERSION unregister --force ltk || true
ghc-pkg$GHCVERSION unregister --force vcsgui || true
ghc-pkg$GHCVERSION unregister --force vado || true
ghc-pkg$GHCVERSION unregister --force yi || true
ghc-pkg$GHCVERSION unregister --force ghcjs-dom || true
ghc-pkg$GHCVERSION unregister --force jsc || true
ghc-pkg$GHCVERSION unregister --force gtksourceview2 || true
ghc-pkg$GHCVERSION unregister --force gtksourceview3 || true
ghc-pkg$GHCVERSION unregister --force webkit-javascriptcore || true
ghc-pkg$GHCVERSION unregister --force webkit3-javascriptcore || true
ghc-pkg$GHCVERSION unregister --force webkit || true
ghc-pkg$GHCVERSION unregister --force webkit3 || true
ghc-pkg$GHCVERSION unregister --force gtk-mac-integration || true
ghc-pkg$GHCVERSION unregister --force gtk || true
ghc-pkg$GHCVERSION unregister --force gtk3 || true
ghc-pkg$GHCVERSION unregister --force pango || true
ghc-pkg$GHCVERSION unregister --force cairo || true
ghc-pkg$GHCVERSION unregister --force gio || true
ghc-pkg$GHCVERSION unregister --force glib || true
ghc-pkg$GHCVERSION unregister --force hlint || true

rm -rf vendor/*/dist \
       vendor/gtk2hs/*/dist \
       vendor/yi/*/dist \
       vendor/*/*/dist \
       vendor/*/vendor/*/dist \
       vendor/*/vendor/*/*/dist \
       vendor/*/vendor/gtk2hs/*/dist || true

cabal clean || exit
