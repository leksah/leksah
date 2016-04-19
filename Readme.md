# Leksah, an Integrated Development Environment for Haskell

[![Build Status](https://secure.travis-ci.org/leksah/leksah.png?branch=vcs)](http://travis-ci.org/leksah/leksah)

[Leksah](http://leksah.org/) aims to integrate various Haskell development
tools to provide a practical and pleasant development environment.
The user interface is a mix of GTK+ and WebKit based components.

Documentation can be found on [leksah.org](http://leksah.org/).

## Installation

Requirements: ghc >= 7.8.3 cabal >= 1.18

You can get Leksah up and running quickly on Windows and OS X using the
[official binaries](https://github.com/leksah/leksah/wiki/download).

Some Linux distributions include Leksah packages, but building from source
on Linux is normally relatively easy.

## Building From Source

### Building on Linux

Install the GtkSourceView and WebKitGtk development packages for your distribution:

    sudo apt-get install libgtksourceview-3.0-dev libwebkitgtk-3.0-dev

Make sure `$HOME/.cabal/bin` is in your `PATH` then:
    
    cabal update
    cabal install Cabal cabal-install
    cabal install alex happy
    cabal install gtk2hs-buildtools
    cabal install leksah
    leksah

### Building on OS X

[Install MacPorts](https://www.macports.org/install.php).

Make sure `/opt/local/bin` and `$HOME/Libraries/Haskell/bin` are in your `PATH`.

To avoid a dependency on X11 add the following to `/opt/local/etc/macports/variants.conf`:

    -x11 +no_x11 +quartz +gtk3

Use MacPorts to install `python27` and `rsync` (sometimes it stops to ask for these to be
activated so if you do them first it might help):

    sudo port install python27 rsync

Use MacPorts to install GHC and the C libraries needed by Leksah (this will take a long time):
     
    sudo port install ghc gtk3 webkit-gtk3 gtksourceview3 gtk-osx-application-gtk3 adwaita-icon-theme

Update Cabal and cabal-install

	cabal update
	cabal install Cabal cabal-install

Make sure the right `cabal` made it into your `PATH`.

    cabal --version

Check that the versions match the ones you just installed (if not check the symbolic links in
`$HOME/Libraries/Haskell/bin`).

Install `gtk2hs-buildtools` and `leksah`:

    cabal install regex-tdfa-text --ghc-options=-XFlexibleContexts
    cabal install alex happy
    cabal install gtk2hs-buildtools
    cabal install leksah
    leksah

### Building on Windows

Install [GHC](https://www.haskell.org/downloads/windows).

Update MinGW if necessary.  The GHC installers currently come with old versions of
MinGW and you will probably need to replace it with one that comes with gcc 4.8.1.
The current 64bit GHC installer seems to be happy to work with newer MinGW.
It may not be possible to use the current 32bit GHC installers at all.
Make sure you replace MinGW so that GHC will find the new one (just adding
it to the `PATH` will not work). The MinGW used by GHC is typically in a location
like `C:\Program Files\MinGHC-7.10.1\ghc-7.10.1\mingw`.  Move it out of the way
and put a newer one in its place.

[MinGW version used to build the Leksah binaries](http://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/4.9.2/threads-posix/seh/x86_64-4.9.2-release-posix-seh-rt_v4-rev2.7z)

Install the C libraries needed by Leksah.  The easiest way to do this is to install
Leksah using the MSI files.  They include pkg-config and all the C libraries needed.

Make sure `C:\Leksah\bin` and `%APPDATA%\cabal\bin` are in your `PATH` and build:

    cabal update
    cabal install Cabal
    cabal install alex happy
    cabal install gtk2hs-buildtools
    cabal install leksah
    leksah
   
### Building Leksah for Windows using Docker

It may seem crazy, but this is currently the best way to bootstrap Leksah for
Windows from source.  This is mostly because Fedora and SUSE have a much
more complete set of MinGW packages than any thing else (including Windows).

Get the leksah source:

    git clone https://github.com/leksah/leksah
    cd leksah
    git submodule update --init

Get the Fedora 23 docker image and load it:

    wget https://download.fedoraproject.org/pub/fedora/linux/releases/23/Docker/x86_64/Fedora-Docker-Base-23-20151030.x86_64.tar.xz
    sudo docker load -i Fedora-Docker-Base-23-20151030.x86_64.tar.xz

Build Leksah using the Dockerfile:

    sudo docker build -t leksah/build .

Copy the resulting msi file out of the container (version number in the file name will match the one in the leksah.cabal file):

    sudo docker run --rm --volume $HOME/output:/output leksah/build cp /leksah/win32/leksah-0.16.0.0-ghc-7.10.3.msi /output


### Building Leksah from GitHub

We have just completed a port of Leksah from Gtk2Hs to haskell-gi.  Not all
of the code is in Hackage yet so to build it you can either use [Xobl](xobl/Readme.md)
or follow the instructions below.

First install the following C libraries for your OS using the instructions
above (these are the Fedora names your OS may have different names):

    gobject-introspection-devel
    atk-devel
    libsoup-devel
    webkitgtk3-devel
    webkitgtk4-devel
    gtksourceview3-devel
    poppler-glib-devel
    vte291-devel
    libnotify-devel
    gstreamer1-devel
    gstreamer1-plugins-base-devel

Clone lots of stuff:

    git clone https://github.com/haskell-gi/haskell-gi-base.git
    git clone https://github.com/haskell-gi/haskell-gi.git
    git clone https://github.com/gtk2hs/gi-gtk-hs.git
    git clone https://github.com/ghcjs/jsaddle.git
    git clone https://github.com/ghcjs/jsaddle-dom.git
    git clone https://github.com/ghcjs/ghcjs-dom.git
    git clone https://github.com/leksah/leksah.git
    cd leksah
    git submodule update --init
    cd ..

Install some tools (gtk2hs-buildtools is still needed for webkitgtk3-javascriptcore):

    cabal install alex happy
    cabal install gtk2hs-buildtools

Install haskell-gi (it check the genBindings output to see if you need to install more -devel packages):

    cabal install ./haskell-gi-base ./haskell-gi
    cd haskell-gi/bindings
    ./genBindings.sh
    ./buildAll.sh
    cd ../..

Install the rest:

    cd leksah
    cabal install ../gi-gtk-hs ../jsaddle ../jsaddle-dom ../ghcjs-dom ./ ./vendor/ltk ./vendor/leksah-server ./vendor/haskellVCSGUI/vcsgui

#### Using Stack instead of cabal-install

As above but with two changes:

* Skip the `./buildAll.sh` haskell-gi script.
* Instead of the very last `cabal install ...` command run `stack build`.

Note: If you intend to do any development work on Leksah, do not use stack for now as it adds 10-12s to every build (not a problem for a one off build, but annoying when you are making changes).  This will probably be fixed once new haskell-gi packages added to hackage or once the [stack issue](https://github.com/commercialhaskell/stack/issues/2041) is resolved.
