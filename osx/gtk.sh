#!/bin/sh

jhbuild bootstrap --skip=libiconv || exit
jhbuild build meta-gtk-osx-bootstrap || exit
jhbuild build meta-gtk-osx-gtk3 || exit
jhbuild build gtk-engines || exit
jhbuild build gtksourceview || exit

