#!/bin/sh -ex

jhbuild bootstrap --skip=libiconv
jhbuild build meta-gtk-osx-bootstrap

# Gtk 2
# jhbuild build meta-gtk-osx-core

# Gtk 3
jhbuild build meta-gtk-osx-gtk3

jhbuild build gtk-engines
jhbuild build gtksourceview

