{ nixpkgs ?
    # Default for CI reproducibility, optionally override in your configuration.nix.
    if (import <nixpkgs> {}).stdenv.isDarwin
      then
        (import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
          owner = "hamishmack"; repo = "nixpkgs";
          # rev = "51ec8f0541329e3823650d36fbacb33ed00f323c";
          # sha256 = "1kq1iy641a57yjz8xsz24vwqicxx965r2r5pm7j4z6ag5m4q76d3";
          
          # leksah-nixpkgs: branched from older nexpkgs commit (GHC 8.4.2 and broken gi-webkit2)
          rev = "41aedcaabe2ee4c9c3ad9c72968e5bcbd221169c";
          sha256 = "0b4akz2smh5mcv43mkz50sbv2p24xcl78qcy44mrmngqyn1yf9kk";
          # rev = "cc547faaa4e0571268e9e2619be69060ddcb73be";
          # sha256 = "1az3fa7vbvriqzv3camlfjfl97pnpm1mwyk2r1n9kf8ficwf6gic";
        }) {})
      else
        (import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
          owner = "obsidiansystems"; repo = "nixpkgs";
          rev = "b2d07e093fd14adca8d1b6b7a4bb522cf7416ce0";
          sha256 = "1y0yf0z9c1l91k3xaivki3sayl4lmixmndlxnjmglcibk9kjbvcj";

          # obsidian reunification
          # rev = "a6183e41fee5e9a18b856bca77f56d8b0485061b";
          # sha256 = "14mg0gnzld6b0ixajvssdm1yklsqfcbgjq80wdcxa8mzhp8s41hk";

          # rev = "f44c16401e71aac01028a75f46bd52480f9b49b0";
          # sha256 = "01whjsdyc9xbxj7jdf4x2d9byr7kzccbzh785695ppbj1pn3ddzv";
        }) {})
, compiler ? "ghc802" # TODO: try using "default"?
, haskellPackages ? if compiler == "default"
                      then nixpkgs.pkgs.haskellPackages
                      else nixpkgs.pkgs.haskell.packages.${compiler}
}:

with nixpkgs.pkgs.haskell.lib;

let

  inherit (nixpkgs) pkgs;

  filterSubmodule = src:
    builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
      nixpkgs.lib.all (i: toString i != path) [ ./.DS_Store ./default.nix ]
        && nixpkgs.lib.all (i: i != baseNameOf path) [ ".git" "dist-newstyle" "cabal.project.local" "dist" ".stack-work" ".vagrant" ".DS_Store" ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasSuffix i path)) [ ".lkshf" ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasPrefix i (baseNameOf path))) [ ".ghc.environment." ]
        # TODO: what else?
      ) src;

  fixCairoGI = p: overrideCabal p (drv: {
    preCompileBuildDriver = (drv.preCompileBuildDriver or "") + ''
      export LD_LIBRARY_PATH="${pkgs.cairo}/lib"
    '';
  });

  jsaddle-github = ../aspen/focus/reflex-platform/jsaddle;
  #nixpkgs.fetchFromGitHub {
  #  owner = "ghcjs";
  #  repo = "jsaddle";
  #  rev = "93b45846b200fd21021f7f3c0d3356c30c8f8d9c";
  #  sha256 = "1v0wnzfr508v709qyad8xw7z6r2nrs2cc61bwby516b04iv7vvy7";
  #};
  ghcjs-dom-github = nixpkgs.fetchFromGitHub {
     owner = "ghcjs";
     repo = "ghcjs-dom";
     rev = "a6d51fcf0e79e7de50a0cb2088042e323133a7a8";
     sha256 = "06g0vvgvxxxzw9h2jqwkykam5jpngk6xlph29jyg92c00jms2bl4";
  };

  extendedHaskellPackages = haskellPackages.override {
    overrides = self: super:
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
        jsaddle-dom = import (pkgs.fetchFromGitHub {
           owner = "ghcjs";
           repo = "jsaddle-dom";
           rev = "bb8516918043d9164c05d016f37448ac13f6fe71";
           sha256 = "1a79y1prgynjhgfp7nfp3xws9bybyh0wdis44vfhkmc6bvz425fl";
        }) self;
        vado = doJailbreak super.vado;
        criterion = doJailbreak super.criterion;
        ghcjs-dom-jsaddle = ghcjsDom.ghcjs-dom-jsaddle;
        gi-gtkosxapplication = super.gi-gtkosxapplication.override {
          gtk-mac-integration-gtk3 = pkgs.gtk-mac-integration-gtk3;
        };
        webkit2gtk3-javascriptcore = overrideCabal super.webkit2gtk3-javascriptcore (drv: {
          preConfigure = ''
            mkdir dispatch
            sed 's|^\(typedef void [(]\)\^\(dispatch_block_t[)][(]void[)];\)$|\1\2|' <"${pkgs.stdenv.cc.libc}/include/dispatch/object.h" >dispatch/object.h
            '';
        });
        # Cabal2nix adds gtk3 to libraryPkgconfigDepends instead of gnome3.gtk.
        gtk3 = pkgs.gnome3.gtk;

        # FIXME: do we really need them as Git submodules?
        vcswrapper = self.callCabal2nix "vcswrapper" (filterSubmodule ./vendor/haskellVCSWrapper/vcswrapper) {};
        vcsgui = self.callCabal2nix "vcsgui" (filterSubmodule ./vendor/haskellVCSGUI/vcsgui) {};
        git = self.callCabal2nix "git" (filterSubmodule ../hs-git) {};
        ltk = overrideCabal (pkgs.haskell.lib.appendConfigureFlag (self.callCabal2nix "ltk" (filterSubmodule ./vendor/ltk) {}) "-f-check-gtk-version")
                            (oldAttrs: with pkgs; with extendedHaskellPackages; {
          # On macOS we wind up with too many arguments being passed to clang
          libraryPkgconfigDepends = [];
        });
        leksah-server = self.callCabal2nix "leksah-server" (filterSubmodule ./vendor/leksah-server) {};

        # TODO: optionally add:
        # • yi >=0.12.4 && <0.13,
        # • yi-language >=0.2.0 && <0.3,
        # • yi-rope >=0.7.0.1 && <0.8
      };
  };

  cleanSrc =
    builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
      nixpkgs.lib.all (i: toString i != path) [ ./.DS_Store ./osx/Leksah ./osx/keymap.lkshk ./osx/prefs.lkshp ./win32/SourceDir ./default.nix ./vendor ./result ]
        && nixpkgs.lib.all (i: i != baseNameOf path) [ ".git" "dist-newstyle" "cabal.project.local" "dist" ".stack-work" ".vagrant" ".DS_Store" ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasSuffix i path)) [ ".dmg" ".msi" ".exe" ".lkshf" ".wixobj" ".wixpdb" ".wxs" ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasPrefix i (baseNameOf path))) [ ".ghc.environment." ]
        # TODO: what else?
      ) ./.;

  drv = overrideCabal (pkgs.haskell.lib.appendConfigureFlag (extendedHaskellPackages.callCabal2nix "leksah" cleanSrc {}) "-f-check-gtk-version")
                      (oldAttrs: with pkgs; with extendedHaskellPackages; {
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

  # Work around bug in slightly old nixpkgs.writeShellScriptBin
  writeShellScriptBin = name : text :
    nixpkgs.writeTextFile {
      inherit name;
      executable = true;
      destination = "/bin/${name}";
      text = ''
        #!${nixpkgs.stdenv.shell}
        ${text}
      '';
      checkPhase = ''
        ${nixpkgs.stdenv.shell} -n $out/bin/${name}
      '';
    };

  launch-leksah-script = writeShellScriptBin "launch-leksah" ''
    "$@"
  '';

  launch-leksah = nixpkgs.stdenv.mkDerivation {
      name = "launch-leksah";
      nativeBuildInputs = with pkgs; [ wrapGAppsHook makeWrapper ];
      buildInputs = with pkgs; [
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
          --suffix 'PATH' ':' "${extendedHaskellPackages.ghcWithPackages (self: [])}/bin" \
          --suffix 'PATH' ':' "${extendedHaskellPackages.cabal-install}/bin" \
          --suffix 'LD_LIBRARY_PATH' ':' "${pkgs.cairo}/lib" \
          --set 'XDG_DATA_DIRS' ""
      '';
  };

  leksah = nixpkgs.stdenv.mkDerivation {
      name = "leksah";
      nativeBuildInputs = with pkgs; [ wrapGAppsHook makeWrapper ];
      buildInputs = with pkgs; [
        gnome3.gtk
        gnome3.dconf
        gnome3.defaultIconTheme
        gnome3.gsettings_desktop_schemas
      ];
      src = ./linux;
      buildPhase =
        if pkgs.stdenv.isLinux then ''
          mkdir -p $out/share
          cp -r * $out/share/
        '' else ''
          mkdir -p $out
        '';
      installPhase = ''
        mkdir -p $out/bin
        ln -s ${drv}/bin/leksah $out/bin
        wrapProgram $out/bin/leksah \
          --prefix 'PATH' ':' "${extendedHaskellPackages.leksah-server}/bin" \
          --prefix 'PATH' ':' "${extendedHaskellPackages.vcsgui}/bin" \
          --suffix 'PATH' ':' "${extendedHaskellPackages.ghcWithPackages (self: [])}/bin" \
          --suffix 'PATH' ':' "${extendedHaskellPackages.cabal-install}/bin" \
          --suffix 'LD_LIBRARY_PATH' ':' "${pkgs.cairo}/lib" \
          --set 'XDG_DATA_DIRS' ""
      '';
  };

  env = pkgs.stdenv.lib.overrideDerivation drv.env (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      extendedHaskellPackages.leksah-server
      extendedHaskellPackages.vcsgui
#      extendedHaskellPackages.cabal-install
      # TODO: perhaps add some additional stuff to nix-shell PATH
    ];
    src = ./linux;
    shellHook = ''
      export CFLAGS="$NIX_CFLAGS_COMPILE" # TODO: why is this needed?
      export XDG_DATA_DIRS="$GSETTINGS_SCHEMAS_PATH:$XDG_DATA_DIRS" # TODO: how to do this better?
      export LD_LIBRARY_PATH="${pkgs.cairo}/lib"
    '';
    installPhase = ''
    '';
  });
  shells = {
    ghc = env;
  };
in leksah // {
  inherit env shells cleanSrc;
  ghc = extendedHaskellPackages // { inherit leksah launch-leksah; };
}
