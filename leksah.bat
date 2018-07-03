SET PATH=C:\tools\msys64\mingw64\bin;C:\tools\msys64\usr\bin;%PATH%
SET PKG_CONFIG_PATH=C:\tools\msys64\mingw64\lib\pkgconfig
SET XDG_DATA_DIRS=C:\tools\msys64\mingw64\share
pacman -S --needed mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-gobject-introspection mingw64/mingw-w64-x86_64-gtksourceview3 mingw64/mingw-w64-x86_64-webkitgtk3
bash leksah.sh
