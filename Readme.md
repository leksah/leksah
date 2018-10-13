# Leksah, an Integrated Development Environment for Haskell

[![Build Status](https://secure.travis-ci.org/leksah/leksah.png)](http://travis-ci.org/leksah/leksah)

[Leksah](http://leksah.org/) aims to integrate various Haskell development
tools to provide a practical and pleasant development environment.
The user interface is a mix of GTK+ and WebKit based components.

Documentation can be found on [leksah.org](http://leksah.org/).

## Leksah's Nix Support

Nix is great and we have added some features to make it easier
to use Nix projects with Leksah.

If your project has a `default.nix` file along side it (in the same directory
as your `cabal.project` file), leksah can use `nix-shell -A shells.ghc`
to make a cached environment for running `ghc` and `ghci`.  Click on the Nix
button on the toolbar to set this up for the active project
(if the `ghcjs` build button is active it will also build a
cached environment for `nix-shell -A shells.ghc`).

If you change your `default.nix` file click the Nix button again to refresh
the cached environment.  Caching the environment in this way makes calls
to `cabal new-build` faster (avoiding the startup overhead of `nix-shell`).

A great way to set up a suitable `default.nix` for your project is
described in [project-development.md](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md).
This works even if you are not planning on using reflex in your project.

## Getting Leksah

### Nix (Recommended for Linux and macOS users)

Install [Nix](https://nixos.org/nix/).

Then download, build and run Leksah with:

```
git clone --recursive https://github.com/leksah/leksah.git
cd leksah
./leksah-nix.sh ghc843
```

On macOS the Leksah window start below other active application windows you can use
Command+Shift+Tab to bring it to the top
([issue 461](https://github.com/leksah/leksah/issues/461)).

The `ghc843` argument indicates Leksah should be built using GHC 8.4.3.  Leksah works best if
it is built with the same version of GHC that your projects use.  To work on a project that
uses GHC 8.2.2, just exit leksah and run `./leksah-nix.sh ghc822`.

### Chocolatey and MSYS2 (Recommended for Windows users)

Install [Chocolatey](https://chocolatey.org/).

Right click on `Command Prompt` and choose `Run as Administrator`.  In the window run:
```shell
choco install ghc --version 8.2.2
choco install msys2
```

Close the `Command Prompt` window and open a new one (not as administrator).  This time run:
```shell
git clone --recursive https://github.com/leksah/leksah.git
cd leksah
leksah.bat
```

### Alternative Installation Method

Leksah requires `ghc --version` >=8.2.2 and `cabal --version` >=2.0. To get them go to **[haskell.og/download](https://www.haskell.org/downloads)** and choose the **Minimal GHC** or **Haskell Platform**.

#### Step 1: Install C libraries

##### Fedora
```shell
sudo dnf install gobject-introspection-devel webkitgtk4-devel gtksourceview3-devel
```

##### Ubuntu/Debian
```shell
sudo apt-get install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
```

##### Arch Linux
```shell
sudo pacman -S gobject-introspection gobject-introspection-runtime gtksourceview3 webkit2gtk
```

##### macOS with MacPorts
```shell
sudo port install gobject-introspection webkit2-gtk gtksourceview3 gtk-osx-application-gtk3 adwaita-icon-theme`
```
You will also need to build a MacPorts compatible of GHC. First install GHC some other way then unpack the source for the GHC version you want to use and run:
```shell
sudo port install libxslt gmp ncurses libiconv llvm-3.5 libffi
./configure --prefix=$HOME/ghc-8.4.3 --with-iconv-includes=/opt/local/include --with-iconv-libraries=/opt/local/lib --with-gmp-includes=/opt/local/include --with-gmp-libraries=/opt/local/lib --with-system-libffi --with-ffi-includes=/opt/local/lib/libffi-3.2.1/include --with-ffi-libraries=/opt/local/lib --with-nm=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/nm-classic
make
make install
echo 'PATH: '"$PATH"
```

Make sure the `$HOME/ghc-8.4.3/bin` is present in PATH.

##### macOS with Homebrew
It might be possible to build Leksah using Homebrew now we have switched to WebKit 2.  If you can figure it out please send us the details or better yet a pull request to update this file.  Raise an issue if you try and it does not work.

##### FreeBSD
```shell
pkg install devel/gobject-introspection x11-toolkits/gtksourceview3 www/webkit2-gtk3
```

#### Step 2: Clone repository and its submodules
```shell
git clone --recursive https://github.com/leksah/leksah.git
cd leksah
```

#### Step 3.a: Build - cabal new-build variant
##### Step 3.a.1: Install extra tools
```shell
cabal update
cabal install alex happy
cabal install haskell-gi
```
Make sure `~/.cabal/bin` is present in the PATH (*Windows:* Make sure `%APPDATA%\cabal\bin` is present in the PATH).

##### Step 3.a.2: Build and run Leksah
###### macOS using MacPorts
```shell
XDG_DATA_DIRS=/opt/local/share ./leksah.sh
```
###### Other OS
```shell
./leksah.sh
```

#### Step 3.b: Build - stack variant
##### Step 3.b.1: Install extra tools
```shell
stack setup --upgrade-cabal
stack install alex happy
stack install haskell-gi
stack install gtk2hs-buildtools
```

Leksah needs `cabal` for the metadate feature to work correctly
(even when using `stack`):

```shell
stack install cabal-install
cabal update
```

##### Step 3.b.2: Build and run Leksah
###### macOS using MacPorts
```shell
XDG_DATA_DIRS=/opt/local/share stack install
stack exec --no-ghc-package-path leksah
```

###### Other OS with Gtk+ 3.20 (or newer)
```shell
stack install
stack exec --no-ghc-package-path leksah
```

###### Other OS with Gtk+ 3.18 (e.g. Ubuntu 16.04)
```shell
stack install --flag ltk:-gtk-320 --flag leksah:-gtk-320
stack exec --no-ghc-package-path leksah
```

