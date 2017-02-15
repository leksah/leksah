# Leksah, an Integrated Development Environment for Haskell

[![Build Status](https://secure.travis-ci.org/leksah/leksah.png)](http://travis-ci.org/leksah/leksah)

[Leksah](http://leksah.org/) aims to integrate various Haskell development
tools to provide a practical and pleasant development environment.
The user interface is a mix of GTK+ and WebKit based components.

Documentation can be found on [leksah.org](http://leksah.org/).

## Getting Leksah
### Installation
Leksah requires `ghc --version` >=7.10.3 and `cabal --version` >=1.24. To get them go to **[haskell.og/download](https://www.haskell.org/downloads)** and choose the **Minimal GHC** or **Haskell Platform**.

* **Windows** [latest github version built with AppVeyor](https://ci.appveyor.com/project/hamishmack/leksah/build/artifacts)
* **OS X**: [official binaries](https://github.com/leksah/leksah/wiki/download)
* **Linux**: [build from source](https://github.com/leksah/leksah#building-from-source)

### Building from source
We have completed a port of Leksah from Gtk2Hs to haskell-gi. Not all of the code is in Hackage yet so to build it you can either use [Xobl](xobl/Readme.md) or follow the instructions below.

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

##### OS X MacPorts
```shell
sudo port install gobject-introspection webkit2-gtk gtksourceview3 gtk-osx-application-gtk3 adwaita-icon-theme`
```
You will also need to build a MacPorts compatible of GHC. First install GHC some other way then unpack the source for the GHC version you want to use and run:
```shell
sudo port install libxslt gmp ncurses libiconv llvm-3.5 libffi
./configure --prefix=$HOME/ghc-8.0.1 --with-iconv-includes=/opt/local/include --with-iconv-libraries=/opt/local/lib --with-gmp-includes=/opt/local/include --with-gmp-libraries=/opt/local/lib --with-system-libffi --with-ffi-includes=/opt/local/lib/libffi-3.2.1/include --with-ffi-libraries=/opt/local/lib --with-nm=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/nm-classic
make
make install
echo 'PATH: '"$PATH"
```

Make sure the `$HOME/ghc-8.0.1/bin` is present in PATH.

##### OS X Homebrew
It might be possible to build Leksah using Homebrew now we have switched to WebKit 2.  If you can figure it out please send us the details or better yet a pull request to update this file.  Raise an issue if you try and it does not work.

##### Windows MSYS2
Install:
* [MSYS2](https://msys2.github.io/)
* [Chocolatey](https://chocolatey.org/)

Then in Bash shell with administrator privileges execute:
```shell
choco install ghc
pacman -S mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-gobject-introspection mingw64/mingw-w64-x86_64-gtksourceview3 mingw64/mingw-w64-x86_64-webkitgtk3
```

Set the following environment variables:
```shell
SET PATH=%APPDATA%\cabal\bin;C:\msys64\mingw64\bin;C:\msys64\usr\bin;C:\ProgramData\chocolatey\lib\ghc\tools\ghc-8.0.2\bin;C:\ProgramData\chocolatey\lib\cabal\tools;%PATH%
SET PKG_CONFIG_PATH=C:\msys64\mingw64\lib\pkgconfig
SET XDG_DATA_DIRS=C:\msys64\mingw64\share
```
(change `C:\ProgramData\chocolatey\lib\ghc\tools\ghc-8.0.2\bin` if a newer version is installed)

#### Step 2: Clone repository and its submodules
```shell
git clone --recursive https://github.com/leksah/leksah.git
cd leksah
```

#### (Cabal variant) Step 3.a: Build
##### Step 3.a.1: Install extra tools
```shell
cabal update
cabal install alex happy
cabal install haskell-gi
echo 'PATH: '"$PATH"
```
Make sure `~/.cabal/bin` is present in PATH of `echo "$SHELL"` (*Windows:* Make sure `%APPDATA%\cabal\bin` is present in PATH).

To add cabal path to BASH:
```shell
sed -i 's;^PATH=.*;PATH='"$PATH"':'"$HOME"'/.cabal/bin;' ~/.bashrc
```

If ZSH is your `$SHELL` - instead change file ~/.zshrc.

##### Step 3.a.2: Build and run Leksah
###### OS X using MacPorts
```shell
XDG_DATA_DIRS=/opt/local/share ./leksah.sh
```
###### All other OS'es
```shell
./leksah.sh
```

#### (Stack variant) Step 3.b: Build
##### Step 3.b.1: Install extra tools
```shell
stack setup --upgrade-cabal
stack install alex happy
stack install haskell-gi
stack install gtk2hs-buildtools
```
##### Step 3.b.2: Build and run Leksah
###### For Mac OS
```shell
stack install --stack-yaml stack.osx.yaml
stack exec leksah
```

###### All other OS'es
```shell
stack install
stack exec leksah
```
