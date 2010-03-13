set PATH=%~dp0\bin;%PATH%

set XDG_CONFIG_DIRS=%~dp0etc\xdg
set XDG_DATA_DIRS=%~dp0share
set GTK_DATA_PREFIX=%~dp0
set GTK_EXE_PREFIX=%~dp0
set GTK_PATH=%~dp0

set GTK2_RC_FILES=%~dp0etc\gtk-2.0\gtkrc
set GTK_IM_MODULE_FILE=%~dp0etc\gtk-2.0\gtk.immodules
set GDK_PIXBUF_MODULE_FILE=%~dp0etc\gtk-2.0\gdk-pixbuf.loaders
set PANGO_RC_FILE="%~dp0etc\pango\pangorc

set leksah_bindir=%~dp0bin
set leksah_libdir=%~dp0leksah-server-0.8\ghc-6.10.4
set leksah_datadir=%~dp0leksah-server-0.8
set leksah_libexecdir=%~dp0bin

leksah-server.exe --server=11111

pause