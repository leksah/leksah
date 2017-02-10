# Leksah, an Integrated Development Environment for Haskell

[![Build Status](https://secure.travis-ci.org/leksah/leksah.png)](http://travis-ci.org/leksah/leksah)

[Leksah](http://leksah.org/) aims to integrate various Haskell development
tools to provide a practical and pleasant development environment.
The user interface is a mix of GTK+ and WebKit based components.

Documentation can be found on [leksah.org](http://leksah.org/).

## Getting Leksah
### Installation
Leksah requires: **[Haskell Platform](https://www.haskell.org/platform/) [>= 8.0.1](https://www.haskell.org/platform/contents.html)** OR **[ghc >= 7.10.3](https://www.haskell.org/ghc/download)**, **[cabal-install >= 1.24](https://www.haskell.org/cabal/download.html)**.

* **Windows** and **OS X**: [official binaries](https://github.com/leksah/leksah/wiki/download)
* **Linux**: [Build from source](https://github.com/leksah/leksah#building-from-source)

### Building from source
We have just completed a port of Leksah from Gtk2Hs to haskell-gi. Not all of the code is in Hackage yet so to build it you can either use [Xobl](xobl/Readme.md) or follow the instructions below.

#### Step 1: Install C libraries

##### Fedora
`sudo dnf install gobject-introspection-devel webkitgtk4-devel gtksourceview3-devel`

##### Ubuntu/Debian
`sudo apt-get install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev`

##### Arch Linux
`sudo pacman -S gobject-introspection gobject-introspection-runtime gtksourceview3 webkit2gtk`

##### OS X MacPorts
`sudo port install gobject-introspection webkit2-gtk gtksourceview3 gtk-osx-application-gtk3 adwaita-icon-theme`

You will also need to build a MacPorts compatible of GHC. First install GHC some other way then unpack the source for the GHC version you want to use and run:

    sudo port install libxslt gmp ncurses libiconv llvm-3.5 libffi
    ./configure --prefix=$HOME/ghc-8.0.1 --with-iconv-includes=/opt/local/include --with-iconv-libraries=/opt/local/lib --with-gmp-includes=/opt/local/include --with-gmp-libraries=/opt/local/lib --with-system-libffi --with-ffi-includes=/opt/local/lib/libffi-3.2.1/include --with-ffi-libraries=/opt/local/lib --with-nm=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/nm-classic
    make
    make install

Then make sure the `$HOME/ghc-8.0.1/bin` is in your $PATH.

##### OS X Homebrew
It might be possible to build Leksah using Homebrew now we have switched to WebKit 2.  If you can figure it out please send us the details or better yet a pull request to update this file.  Raise an issue if you try and it does not work.

##### Windows MSYS2
Install:
* [MSYS2](https://msys2.github.io/)
* [Chocolatey](https://chocolatey.org/)

Then in Bash shell with administrator privileges execute:

    choco install ghc
    pacman -S mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-gobject-introspection mingw64/mingw-w64-x86_64-gtksourceview3 mingw64/mingw-w64-x86_64-webkitgtk3

#### (Cabal variant) Step 2.a: Build
##### Step 2.a.1: Install extra tools

    cabal update
    cabal install alex happy
    cabal install haskell-gi

(make sure `~/.cabal/bin` is in PATH)

##### Step 2.a.2: Clone the repo and its submodules

    git clone --recursive https://github.com/leksah/leksah.git
    cd leksah
    
##### Step 2.a.3: Build and run Leksah

###### OS X using MacPorts

    XDG_DATA_DIRS=/opt/local/share ./leksah.sh

###### Other OS

    ./leksah.sh

#### (Stack variant) Step 2.b: Build

First install the C libraries needed as described in
"[Step 1: Install C libraries](https://github.com/leksah/leksah#step-1-install-c-libraries)" above.

```
git clone --recursive https://github.com/leksah/leksah.git
cd leksah
stack setup --upgrade-cabal
stack install alex happy
stack install haskell-gi
stack install gtk2hs-buildtools
stack install
stack exec leksah
```

For Mac OS replace the last `stack install` with:

```
stack install --stack-yaml stack.osx.yaml
```
