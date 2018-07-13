{ nixpkgs ?
    # Default for CI reproducibility, optionally override in your configuration.nix.
    if (import <nixpkgs> {}).stdenv.isDarwin
      then
        (import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
          owner = "hamishmack"; repo = "nixpkgs";
          rev = "0abfa434814e08fa88fa9680800bcda4d4724af0";
          sha256 = "0vaqp4qn138dqa0q72084ph09ncskbdxlnrvqfynl7lr12jncz6p";
        }) {})
      else
        (import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
          owner = "hamishmack"; repo = "nixpkgs";
          rev = "ee1faf5881aeb0d1c17c4fd1e1882d5281ab1bef";
          sha256 = "1s4p6404kzyzx89f8x2cwrbhkr5nmzxy9vf8jrvzazi0crwpsm1h";
        }) {})
}:

with nixpkgs.pkgs.haskell.lib;

let
  cleanSrc =
    builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
      nixpkgs.lib.all (i: toString i != path) [ ./.DS_Store ./osx/Leksah ./osx/keymap.lkshk ./osx/prefs.lkshp ./win32/SourceDir ./default.nix ./vendor ./result ]
        && nixpkgs.lib.all (i: i != baseNameOf path) [ ".git" "dist-newstyle" "cabal.project.local" "dist" ".stack-work" ".vagrant" ".DS_Store" ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasSuffix i path)) [ ".dmg" ".msi" ".exe" ".lkshf" ".wixobj" ".wixpdb" ".wxs" ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasPrefix i (baseNameOf path))) [ "result-" ".ghc.environment." ]
        # TODO: what else?
      ) ./.;

  filterSubmodule = src:
    builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
      nixpkgs.lib.all (i: toString i != path) [ ./.DS_Store ./default.nix ]
        && nixpkgs.lib.all (i: i != baseNameOf path) [ ".git" "dist-newstyle" "cabal.project.local" "dist" ".stack-work" ".vagrant" ".DS_Store" ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasSuffix i path)) [ ".lkshf" ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasPrefix i (baseNameOf path))) [ ".ghc.environment." ]
        # TODO: what else?
      ) src;

  jsaddle-github = nixpkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "7eb50cb73a7cbc31ec16916f85a3a89164b4908b";
    sha256 = "1dq99q12ibvsm6jz35jxmcv154n0jcb0k8lfhnx5c28ckgk3g8q7";
  };

  ghcjs-dom-github = nixpkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-dom";
    rev = "a6d51fcf0e79e7de50a0cb2088042e323133a7a8";
    sha256 = "06g0vvgvxxxzw9h2jqwkykam5jpngk6xlph29jyg92c00jms2bl4";
  };

  launch-leksah-script = nixpkgs.writeShellScriptBin "launch-leksah" ''
    "$@"
  '';

  extendedHaskellPackages = compiler: haskellPackages: (haskellPackages.extend (packageSourceOverrides {
      leksah = cleanSrc;
      ltk = filterSubmodule ./vendor/ltk;
      leksah-server = filterSubmodule ./vendor/leksah-server;
      vcswrapper = filterSubmodule ./vendor/haskellVCSWrapper/vcswrapper;
      vcsgui = filterSubmodule ./vendor/haskellVCSGUI/vcsgui;
      git = nixpkgs.fetchFromGitHub {
        owner = "hamishmack";
        repo = "hs-git";
        rev = "c829ec133f603e376177deaa57a333ef84308af3";
        sha256 = "1hcl5izpcpi9sjfvmv1g5si1wg0jw9v9wbk5hvn30q67xcxwx891";
      };
    })).extend( self: super:
      let jsaddlePkgs = import jsaddle-github self;
          ghcjsDom = import ghcjs-dom-github self;
      in {
        haddock-library = if compiler == "ghc822"
                            then dontHaddock (self.callHackage "haddock-library" "1.4.4" {})
                            else if compiler == "ghc842" || compiler == "ghc843"
                              then dontCheck (dontHaddock (self.callHackage "haddock-library" "1.6.0" {}))
                              else super.haddock-library;
        haddock-api = if compiler == "ghc822"
                            then self.callHackage "haddock-api" "2.18.1" {}
                            else if compiler == "ghc842" || compiler == "ghc843"
                              then dontCheck (self.callHackage "haddock-api" "2.20.0" {})
                              else super.haddock-api;
        jsaddle = jsaddlePkgs.jsaddle;
        jsaddle-warp = jsaddlePkgs.jsaddle-warp;
        jsaddle-dom = import (nixpkgs.fetchFromGitHub {
           owner = "ghcjs";
           repo = "jsaddle-dom";
           rev = "bb8516918043d9164c05d016f37448ac13f6fe71";
           sha256 = "1a79y1prgynjhgfp7nfp3xws9bybyh0wdis44vfhkmc6bvz425fl";
        }) self;
        vado = doJailbreak super.vado;
        criterion = doJailbreak super.criterion;
        ghcjs-dom-jsaddle = ghcjsDom.ghcjs-dom-jsaddle;
        gi-gtkosxapplication = super.gi-gtkosxapplication.override {
          gtk-mac-integration-gtk3 = nixpkgs.gtk-mac-integration-gtk3;
        };

        # This is a fix for macOS that may be needed again one day
        # webkit2gtk3-javascriptcore = overrideCabal super.webkit2gtk3-javascriptcore (drv: {
        #   preConfigure = ''
        #     mkdir dispatch
        #     sed 's|^\(typedef void [(]\)\^\(dispatch_block_t[)][(]void[)];\)$|\1\2|' <"${nixpkgs.stdenv.cc.libc}/include/dispatch/object.h" >dispatch/object.h
        #     '';
        # });

        # Cabal2nix adds gtk3 to libraryPkgconfigDepends instead of gnome3.gtk.
        gtk3 = nixpkgs.pkgs.gnome3.gtk;

        # FIXME: do we really need them as Git submodules?
        ltk = overrideCabal (appendConfigureFlag super.ltk "-f-check-gtk-version")
                            (oldAttrs: {
          # On macOS we wind up with too many arguments being passed to clang
          libraryPkgconfigDepends = [];
        });
        leksah = overrideCabal (appendConfigureFlag super.leksah "-f-check-gtk-version")
                            (oldAttrs: with nixpkgs; {
          # On macOS we wind up with too many arguments being passed to clang
          libraryPkgconfigDepends = [];
          libraryHaskellDepends = (oldAttrs.libraryHaskellDepends or [])
            ++ (if stdenv.isDarwin then [
                  darwin.libobjc
                  buildPackages.darwin.apple_sdk.frameworks.Cocoa
                  buildPackages.darwin.apple_sdk.frameworks.CoreFoundation
                  buildPackages.darwin.apple_sdk.libs.xpc
                  (buildPackages.osx_sdk or null)
                ] else []);
        });

        launch-leksah = nixpkgs.stdenv.mkDerivation {
          name = "launch-leksah";
          nativeBuildInputs = with nixpkgs; [ wrapGAppsHook makeWrapper ];
          buildInputs = with nixpkgs; [
            gnome3.gtk
            gnome3.dconf
            gnome3.defaultIconTheme
            gnome3.gsettings_desktop_schemas
          ];
          src = ./linux;
          buildPhase = ''
              mkdir -p $out
            '';
          installPhase = ''
            mkdir -p $out/bin
            ln -s ${launch-leksah-script}/bin/launch-leksah $out/bin
            cp launch-leksah/Info.plist $out/bin
            wrapProgram $out/bin/launch-leksah \
              --suffix 'PATH' ':' "${self.ghcWithPackages (self: [])}/bin" \
              --prefix 'PATH' ':' "${nixpkgs.cabal-install}/bin" \
              --suffix 'LD_LIBRARY_PATH' ':' "${nixpkgs.cairo}/lib" \
              --set 'XDG_DATA_DIRS' ""
          '';
        };

        wrapped-leksah = suffix: nixpkgs.stdenv.mkDerivation {
          name = "leksah" + suffix;
          nativeBuildInputs = with nixpkgs; [ wrapGAppsHook makeWrapper ];
          buildInputs = with nixpkgs; [
            gnome3.gtk
            gnome3.dconf
            gnome3.defaultIconTheme
            gnome3.gsettings_desktop_schemas
          ];
          src = ./linux;
          buildPhase =
            if nixpkgs.stdenv.isLinux then ''
              mkdir -p $out/share
              cp -r * $out/share/
            '' else ''
              mkdir -p $out
            '';
          installPhase = ''
            mkdir -p $out/bin
            ln -s ${self.leksah}/bin/leksah $out/bin/leksah${suffix}
            wrapProgram $out/bin/leksah${suffix} \
              --prefix 'PATH' ':' "${self.leksah-server}/bin" \
              --prefix 'PATH' ':' "${self.vcsgui}/bin" \
              --suffix 'PATH' ':' "${self.ghcWithPackages (self: [])}/bin" \
              --suffix 'PATH' ':' "${nixpkgs.cabal-install}/bin" \
              --suffix 'LD_LIBRARY_PATH' ':' "${nixpkgs.cairo}/lib" \
              --set 'XDG_DATA_DIRS' ""
          '';
        };

        # TODO: optionally add:
        # • yi >=0.12.4 && <0.13,
        # • yi-language >=0.2.0 && <0.3,
        # • yi-rope >=0.7.0.1 && <0.8
      }
    );

  ghc802 = extendedHaskellPackages "ghc802" nixpkgs.pkgs.haskell.packages.ghc802;
  ghc822 = extendedHaskellPackages "ghc822" nixpkgs.pkgs.haskell.packages.ghc822;
  ghc843 = extendedHaskellPackages "ghc843" nixpkgs.pkgs.haskell.packages.ghc843;
  ghc = ghc802;
  
  leksah = ghc.wrapped-leksah "";
  leksah-ghc802 = ghc802.wrapped-leksah "-ghc802";
  leksah-ghc822 = ghc822.wrapped-leksah "-ghc822";
  leksah-ghc843 = ghc843.wrapped-leksah "-ghc843";

in leksah // {
  inherit ghc ghc802 ghc822 ghc843;
  inherit leksah-ghc802 leksah-ghc822 leksah-ghc843;
  inherit (ghc) launch-leksah;
  
  shells = {
    ghc = ghc.shellFor {
      packages = p: [ p.leksah p.leksah-server p.ltk p.vcswrapper p.vcsgui ];
    };
  };
}
