#!/bin/sh

nix-shell -j 8 --command 'PKG_CONFIG_PATH=`nix-build ./vendor/nixpkgs -A 'gtk3.dev'`/lib/pkgconfig:`nix-build ./vendor/nixpkgs -A 'pango.dev'`/lib/pkgconfig:`nix-build ./vendor/nixpkgs -A 'glib.dev'`/lib/pkgconfig:`nix-build ./vendor/nixpkgs -A 'cairo.dev'`/lib/pkgconfig:`nix-build ./vendor/nixpkgs -A 'gdk_pixbuf.dev'`/lib/pkgconfig:`nix-build ./vendor/nixpkgs -A 'atk.dev'`/lib/pkgconfig XDG_DATA_DIRS=$XDG_ICON_DIRS ./leksah.sh' || exit
