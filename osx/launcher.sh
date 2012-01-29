#!/bin/sh

if test -e ~/.profile; then
    export PATH=`. ~/.profile;echo $PATH`
fi

if test "x$IGE_DEBUG_LAUNCHER" != x; then
    set -x
fi

if test "x$IGE_DEBUG_GDB" != x; then
    EXEC="gdb --args"
else
    EXEC=exec
fi

name="`basename "$0"`"
tmp=`dirname "$0"`
tmp=`cd "$tmp";pwd`
tmp=`dirname "$tmp"`
bundle=`dirname "$tmp"`
bundle_contents="$bundle"/Contents
bundle_res="$bundle_contents"/Resources
bundle_lib="$bundle_res"/lib
bundle_bin="$bundle_res"/bin
bundle_data="$bundle_res"/share
bundle_etc="$bundle_res"/etc

# export DYLD_LIBRARY_PATH="$bundle_lib"
export XDG_CONFIG_DIRS="$bundle_etc"/xdg
export XDG_DATA_DIRS="$bundle_data"
export GTK_DATA_PREFIX="$bundle_res"
export GTK_EXE_PREFIX="$bundle_res"
export GTK_PATH="$bundle_res"
export PATH="$bundle_bin":$PATH

export GTK2_RC_FILES="$bundle_etc/gtk-2.0/gtkrc"
export GTK_IM_MODULE_FILE="$bundle_etc/gtk-2.0/gtk.immodules"
export GDK_PIXBUF_MODULE_FILE="$bundle_etc/gtk-2.0/gdk-pixbuf.loaders"
export PANGO_RC_FILE="$bundle_etc/pango/pangorc"

export leksah_bindir="$bundle_bin"
export leksah_libdir="$bundle_lib/leksah-0.12/ghc-7.0.2"
export leksah_datadir="$bundle_data/leksah-0.12"
export leksah_libexecdir="$bundle_res/libexec"

# We need a UTF-8 locale.
lang=`defaults read .GlobalPreferences AppleLocale 2>/dev/null`
if test "$?" != "0"; then
  lang=`defaults read .GlobalPreferences AppleCollationOrder 2>/dev/null | sed 's/_.*//'`
fi
if test "$?" == "0"; then
    export LANG="`grep \"\`echo $lang\`_\" /usr/share/locale/locale.alias | \
  tail -n1 | sed 's/\./ /' | awk '{print $2}'`.UTF-8"
fi

if test -f "$bundle_lib/charset.alias"; then
    export CHARSETALIASDIR="$bundle_lib"
fi

# Extra arguments can be added in environment.sh.
EXTRA_ARGS=
if test -f "$bundle_res/environment.sh"; then
  source "$bundle_res/environment.sh"
fi

# Strip out the argument added by the OS.
if [ x`echo "x$1" | sed -e "s/^x-psn_.*//"` == x ]; then
    shift 1
fi

$EXEC "$bundle_contents/Resources/bin/leksah" $* $EXTRA_ARGS
