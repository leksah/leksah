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
  extendHaskellPackages = haskellPackages: makeRecursivelyOverridable haskellPackages {
    overrides = self: super:
      let jsaddlePkgs = import ./vendor/jsaddle self;
          ghcjsDom = import ./vendor/ghcjs-dom self;
      in {
        jsaddle = jsaddlePkgs.jsaddle;
        jsaddle-warp = dontCheck jsaddlePkgs.jsaddle-warp;
        jsaddle-wkwebview = overrideCabal jsaddlePkgs.jsaddle-wkwebview (drv: {
        });
        jsaddle-webkit2gtk = jsaddlePkgs.jsaddle-webkit2gtk;
        jsaddle-dom = overrideCabal (self.callPackage ./jsaddle-dom {}) (drv: {
          # On macOS, the jsaddle-dom build will run out of file handles the first time it runs
          preBuild = ''./setup build || true'';
        });
        ghcjs-dom-jsaddle = dontHaddock ghcjsDom.ghcjs-dom-jsaddle;
        ghcjs-dom-jsffi = ghcjsDom.ghcjs-dom-jsffi;
        ghcjs-dom = dontCheck (dontHaddock ghcjsDom.ghcjs-dom);

        gi-atk = appendConfigureFlag super.gi-atk "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        gi-cairo = appendConfigureFlag (overrideCabal super.gi-cairo (drv: {
          preCompileBuildDriver = (drv.preCompileBuildDriver or "") + ''
            export LD_LIBRARY_PATH="${nixpkgs.cairo}/lib"
          '';
        })) "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        gi-gdk = appendConfigureFlag (overrideCabal super.gi-gdk (drv: {
          preCompileBuildDriver = (drv.preCompileBuildDriver or "") + ''
            export LD_LIBRARY_PATH="${nixpkgs.cairo}/lib"
          '';
        })) "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        gi-gdkpixbuf = appendConfigureFlag super.gi-gdkpixbuf "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        gi-gio = appendConfigureFlag super.gi-gio "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        gi-glib = appendConfigureFlag super.gi-glib "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        gi-gobject = appendConfigureFlag super.gi-gobject "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        gi-gtk = appendConfigureFlag (overrideCabal super.gi-gtk (drv: {
          preCompileBuildDriver = (drv.preCompileBuildDriver or "") + ''
            export LD_LIBRARY_PATH="${nixpkgs.cairo}/lib"
          '';
        })) "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        gi-javascriptcore = super.gi-javascriptcore_4_0_11;
        gi-pango = appendConfigureFlag (overrideCabal super.gi-pango (drv: {
          preCompileBuildDriver = (drv.preCompileBuildDriver or "") + ''
            export LD_LIBRARY_PATH="${nixpkgs.cairo}/lib"
          '';
        })) "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        gi-soup = appendConfigureFlag super.gi-soup "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        gi-webkit = appendConfigureFlag (super.gi-webkit.override {
          # webkitgtk = nixpkgs.webkitgtk24x;
        }) "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        gi-webkit2 = appendConfigureFlag (overrideCabal (super.gi-webkit2.override {
            webkitgtk = nixpkgs.webkitgtk214x;
          }) (drv: {
          preCompileBuildDriver = (drv.preCompileBuildDriver or "") + ''
            export LD_LIBRARY_PATH="${nixpkgs.cairo}/lib"
          '';
        })) "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        gi-gtksource = appendConfigureFlag (overrideCabal super.gi-gtksource (drv: {
          preCompileBuildDriver = (drv.preCompileBuildDriver or "") + ''
            export LD_LIBRARY_PATH="${nixpkgs.cairo}/lib"
          '';
        })) "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        gi-gtkosxapplication = appendConfigureFlag (overrideCabal super.gi-gtkosxapplication (drv: {
          preCompileBuildDriver = (drv.preCompileBuildDriver or "") + ''
            export LD_LIBRARY_PATH="${nixpkgs.cairo}/lib"
          '';
        })) "-f-overloaded-methods -f-overloaded-signals -f-overloaded-properties";
        haskell-gi = super.haskell-gi;
        haskell-gi-base = super.haskell-gi-base;
        webkit2gtk3-javascriptcore = super.webkit2gtk3-javascriptcore.override {
          webkitgtk = nixpkgs.webkitgtk214x;
        };
      };
  };

  f = { mkDerivation, array, base, base-compat, binary
      , binary-shared, blaze-html, bytestring, Cabal, conduit, containers
      , cpphs, deepseq, directory, executable-path, filepath, ghc
      , ghcjs-codemirror, gi-cairo, gi-gdk, gi-gdkpixbuf, gi-gio, gi-glib
      , gi-gobject, gi-gtk, gi-gtk-hs, gi-gtkosxapplication, gi-gtksource
      , gi-pango, gi-webkit2, haskell-gi-base, haskell-src-exts
      , hlint, hslogger, HTTP, mtl, network
      , network-uri, old-time, parsec, pretty, pretty-show, QuickCheck
      , regex-base, regex-tdfa, regex-tdfa-text, shakespeare, split
      , stdenv, stm, strict, text, time, transformers, unix, utf8-string
      , vado, vcsgui, vcswrapper, call-stack, HUnit, doctest, hspec
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
          filepath ghc ghcjs-codemirror gi-cairo gi-gdk gi-gdkpixbuf gi-gio
          gi-glib gi-gobject gi-gtk gi-gtk-hs gi-gtkosxapplication
          gi-gtksource gi-pango gi-webkit2 haskell-gi-base haskell-src-exts
          hlint hslogger HTTP mtl network network-uri
          old-time parsec pretty pretty-show QuickCheck regex-base regex-tdfa
          regex-tdfa-text shakespeare split stm strict text time transformers
          unix utf8-string vado vcsgui vcswrapper call-stack HUnit doctest
          hspec
        ];
        buildTools = [ nixpkgs.cabal-install ];
        libraryPkgconfigDepends = [ nixpkgs.gtk3 ];
        executableHaskellDepends = [ base ];
        postInstall = "echo Hello";
        homepage = "http://www.leksah.org";
        description = "Haskell IDE written in Haskell";
        license = "GPL";
      };

  haskellPackages = extendHaskellPackages (if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler});

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
