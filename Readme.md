# Leksah, an Integrated Development Environment for Haskell

[Leksah](http://leksah.org/) aims to integrate various Haskell development
tools to provide a practical and pleasant development environment.

Leksah has several interchangeable front ends that share most of their code.
The original one is built with GTK+; the newer ones render the UI with
[reflex-dom](https://reflex-frp.org/):

* **`leksah`** — the GTK+ UI.
* **`leksah-warp`** — serves the UI over HTTP; open it in a browser at
  <http://127.0.0.1:3367/> (GHC 9.14).
* **`leksah-wkwebview`** — a native macOS window using WKWebView (GHC 9.14).
* **`leksah-webkitgtk`** — a native window using WebKitGTK (GHC 9.14).

Leksah supports GHC 9.6.7 through 9.14.

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

Leksah uses the cached builds provided by [IOHK](https://iohk.io). Setting these
up will allow you to use their prebuilt GHC binaries and packages. This is
*highly recommended*.

If you're using [NixOS](https://nixos.org/) then follow the instructions located
at: https://github.com/input-output-hk/plutus#iohk-binary-cache.

Otherwise you can use the following instructions for adding the caches to your
local Nix install: https://github.com/input-output-hk/cardano-sl/blob/master/docs/nix.md#binary-cache

Then download, build and run Leksah with:

```
git clone --recursive https://github.com/leksah/leksah.git
cd leksah
./leksah-nix.sh ghc98
```

`leksah-nix.sh` takes the GHC version and, optionally, which front end to run:

```
./leksah-nix.sh GHCVER [UI] [LEKSAH_ARGS]

  GHCVER : ghc96, ghc98, ghc910, ghc912 or ghc914
           (GHC 9.6.7 - 9.14; ghc914 is the default)
  UI     : gtk (default), warp, wkwebview or webkitgtk
           (the web UIs require ghc914)
```

For example, the native macOS WebKit UI:

```
./leksah-nix.sh ghc914 wkwebview
```

or the browser UI (then open <http://127.0.0.1:3367/>):

```
./leksah-nix.sh ghc914 warp
```

On macOS the Leksah window starts below other active application windows; use
Command+Shift+Tab to bring it to the top
([issue 461](https://github.com/leksah/leksah/issues/461)).

Leksah works best when it is built with the same version of GHC that your
projects use.  To switch, exit Leksah and re-run `leksah-nix.sh` with a
different GHC version (e.g. `./leksah-nix.sh ghc96`).

### Chocolatey and MSYS2 (Recommended for Windows users)

> **⚠️ TODO: this section is out of date.** It still installs GHC 8.8.4 and
> predates the move to the Nix flake (GHC 9.6.7–9.14) and the web front ends.
> It needs updating (or removing) — until then, prefer the Nix instructions above.

Install [Chocolatey](https://chocolatey.org/).

Right click on `Command Prompt` and choose `Run as Administrator`.  In the window run:
```shell
choco install ghc --version 8.8.4
choco install msys2
```

Close the `Command Prompt` window and open a new one (not as administrator).  This time run:
```shell
git clone --recursive https://github.com/leksah/leksah.git
cd leksah
leksah.bat
```

### Alternative Installation Method

> **⚠️ TODO: this section is out of date.** It targets GHC ≥8.2.2 with the old
> `leksah.sh` / `stack` builds (and references like GHC 8.4.3, `llvm-3.5`,
> WebKit/GtkSourceView 3) from before the move to the Nix flake (GHC 9.6.7–9.14)
> and the web front ends. The Nix instructions above are the supported build
> path; the steps below need revising and may not work as written.

Leksah requires `ghc --version` >=8.2.2 and `cabal --version` >=2.0. To get them go to **[haskell.og/download](https://www.haskell.org/downloads)** and choose the **Minimal GHC** or **Haskell Platform**.

#### Step 1: Install C libraries

##### Fedora
```shell
sudo dnf install gobject-introspection-devel webkitgtk4-devel gtksourceview3-devel
```

##### Ubuntu/Debian
```shell
sudo apt-get install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev libtinfo-dev
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

