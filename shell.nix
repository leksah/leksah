{ nixpkgs ? import ./vendor/nixpkgs {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;
  lib = import (nixpkgs.path + "/pkgs/development/haskell-modules/lib.nix") { pkgs = nixpkgs; };

in with lib;
let
  combineOverrides = old: new: (old // new) // {
    overrides = self: super:
      let oldOverrides = old.overrides self super;
      in oldOverrides // new.overrides self (super // oldOverrides);
  };
  makeRecursivelyOverridable = x: old: x.override old // {
    override = new: makeRecursivelyOverridable x (combineOverrides old new);
  };
  appendGIFlags = p: appendConfigureFlag p "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
  fixCairoGI = p: overrideCabal p (drv: {
    preCompileBuildDriver = (drv.preCompileBuildDriver or "") + ''
      export LD_LIBRARY_PATH="${nixpkgs.cairo}/lib"
    '';
  });
  extendHaskellPackages = haskellPackages: makeRecursivelyOverridable haskellPackages {
    overrides = self: super:
      let jsaddlePkgs = import ./vendor/jsaddle self;
          ghcjsDom = import ./vendor/ghcjs-dom self;
      in {
        jsaddle = jsaddlePkgs.jsaddle;
        jsaddle-warp = dontCheck jsaddlePkgs.jsaddle-warp;
        jsaddle-wkwebview = overrideCabal jsaddlePkgs.jsaddle-wkwebview;
        jsaddle-webkit2gtk = jsaddlePkgs.jsaddle-webkit2gtk;
        jsaddle-dom = overrideCabal (self.callPackage ./jsaddle-dom {}) (drv: {
          # On macOS, the jsaddle-dom build will run out of file handles the first time it runs
          preBuild = ''./setup build || true'';
        });
        ghcjs-dom-jsaddle = dontHaddock ghcjsDom.ghcjs-dom-jsaddle;
        ghcjs-dom-jsffi = ghcjsDom.ghcjs-dom-jsffi;
        ghcjs-dom = dontCheck (dontHaddock ghcjsDom.ghcjs-dom);

        gi-atk = appendGIFlags super.gi-atk;
        gi-cairo = appendGIFlags (fixCairoGI super.gi-cairo);
        gi-gdk = appendGIFlags (fixCairoGI super.gi-gdk);
        gi-gdkpixbuf = appendGIFlags super.gi-gdkpixbuf;
        gi-gio = appendGIFlags super.gi-gio;
        gi-glib = appendGIFlags super.gi-glib;
        gi-gobject = appendGIFlags super.gi-gobject;
        gi-gtk = appendGIFlags (fixCairoGI super.gi-gtk);
        gi-javascriptcore = appendGIFlags super.gi-javascriptcore_4_0_11;
        gi-pango = appendGIFlags (fixCairoGI super.gi-pango);
        gi-soup = appendGIFlags super.gi-soup;
        gi-webkit2 = appendGIFlags (fixCairoGI super.gi-webkit2);
        gi-gtksource = appendGIFlags (fixCairoGI super.gi-gtksource);
        gi-gtkosxapplication = appendGIFlags (fixCairoGI (super.gi-gtkosxapplication.override {
          gtk-mac-integration-gtk3 = pkgs.gtk-mac-integration-gtk3;
        }));
        haskell-gi = super.haskell-gi;
        haskell-gi-base = super.haskell-gi-base;
        webkit2gtk3-javascriptcore = overrideCabal super.webkit2gtk3-javascriptcore (drv: {
          preConfigure = ''
            mkdir dispatch
            sed 's|^\(typedef void [(]\)\^\(dispatch_block_t[)][(]void[)];\)$|\1\2|' <"${pkgs.stdenv.cc.libc}/include/dispatch/object.h" >dispatch/object.h
            '';
        });
     };
  };

  f = { mkDerivation, array, base, base-compat, binary
      , binary-shared, blaze-html, bytestring, Cabal, conduit, containers
      , cpphs, deepseq, directory, executable-path, filepath, fsnotify, ghc
      , ghcjs-codemirror, gi-cairo, gi-gdk, gi-gdkpixbuf, gi-gio, gi-glib
      , gi-gobject, gi-gtk, gi-gtk-hs, gi-gtkosxapplication, gi-gtksource
      , gi-pango, gi-webkit2, haskell-gi-base, haskell-src-exts
      , hlint, hslogger, HTTP, mtl, network
      , network-uri, old-time, parsec, pretty, pretty-show, QuickCheck
      , regex-base, regex-tdfa, regex-tdfa-text, shakespeare, split
      , stdenv, stm, strict, text, time, transformers, unix, utf8-string
      , vado, vcsgui, vcswrapper, call-stack, HUnit, doctest, hspec, gnome3
      , pkgconfig, darwin, buildPackages
      }:
      mkDerivation {
        pname = "leksah";
        version = "0.16.2.2";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          array base base-compat binary binary-shared blaze-html bytestring
          Cabal conduit containers cpphs deepseq directory executable-path
          filepath fsnotify ghc ghcjs-codemirror gi-cairo gi-gdk gi-gdkpixbuf gi-gio
          gi-glib gi-gobject gi-gtk gi-gtk-hs
          gi-gtksource gi-pango gi-webkit2 haskell-gi-base haskell-src-exts
          hlint hslogger HTTP mtl network network-uri
          old-time parsec pretty pretty-show QuickCheck regex-base regex-tdfa
          regex-tdfa-text shakespeare split stm strict text time transformers
          unix utf8-string vado call-stack HUnit doctest
          hspec gnome3.defaultIconTheme pkgconfig gnome3.gtk
        ] ++ (if stdenv.isDarwin then [
          gi-gtkosxapplication
          darwin.libobjc
          buildPackages.darwin.apple_sdk.frameworks.Cocoa
          buildPackages.darwin.apple_sdk.libs.xpc
          (buildPackages.osx_sdk or null)
        ] else []);
        buildDepends = [ nixpkgs.cabal-install ];
        buildTools = [ nixpkgs.cabal-install ];
        libraryPkgconfigDepends = [ nixpkgs.gtk3 ];
        executableHaskellDepends = [ base ];
        postInstall = "echo Hello!";
        shellHook = ''
          export PKG_CONFIG_PATH=${nixpkgs.gtk3.dev}/lib/pkgconfig:${nixpkgs.pango.dev}/lib/pkgconfig:${nixpkgs.glib.dev}/lib/pkgconfig:${nixpkgs.cairo.dev}/lib/pkgconfig:${nixpkgs.gdk_pixbuf.dev}/lib/pkgconfig:${nixpkgs.atk.dev}/lib/pkgconfig
          export CFLAGS=$NIX_CFLAGS_COMPILE
          export XDG_DATA_DIRS=${nixpkgs.glib.dev}/share:${nixpkgs.gtk3}/share/gsettings/${nixpkgs.gtk3.name}:$XDG_DATA_DIRS:$XDG_ICON_DIRS
          export XDG_CONFIG_HOME=/Users/hamish/.config
          '';
        homepage = "http://www.leksah.org";
        description = "Haskell IDE written in Haskell";
        license = "GPL";
      };

  ghc = extendHaskellPackages nixpkgs.pkgs.haskell.packages.ghc802;
  haskellPackages = extendHaskellPackages (if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler});

  drv = ghc.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
