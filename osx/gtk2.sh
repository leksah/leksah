#!/bin/sh -ex

jhbuild bootstrap --skip=libiconv
jhbuild build meta-gtk-osx-bootstrap
jhbuild build freetype

jhbuild build meta-gtk-osx-core
jhbuild build gtk-engines
jhbuild build gtksourceview

