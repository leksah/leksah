#!/bin/sh -ex

ghc-pkg unregister --force leksah || true
ghc-pkg unregister --force leksah-server || true
ghc-pkg unregister --force ltk || true
ghc-pkg unregister --force vado || true
ghc-pkg unregister --force yi || true
ghc-pkg unregister --force gtksourceview2 || true
ghc-pkg unregister --force gtk-mac-integration || true
ghc-pkg unregister --force gtk || true
ghc-pkg unregister --force pango || true
ghc-pkg unregister --force cairo || true
ghc-pkg unregister --force gio || true
ghc-pkg unregister --force glib || true
ghc-pkg unregister --force hlint || true

rm -rf vendor/*/dist \
       vendor/gtk2hs/*/dist \
       vendor/yi/*/dist \
       vendor/*/*/dist \
       vendor/*/vendor/*/dist \
       vendor/*/vendor/*/*/dist \
       vendor/*/vendor/gtk2hs/*/dist || true

cabal clean || exit
