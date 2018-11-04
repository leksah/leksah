{ nixpkgs ?
    # Default for CI reproducibility, optionally override in your configuration.nix.
    if (import <nixpkgs> {}).stdenv.isDarwin
      then
        (import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
          owner = "hamishmack"; repo = "nixpkgs";
          rev = "38f730fe9c5da285d8a7e52f9c319b871391dcd6"; # branch leksah-nixpkgs-11
          sha256 = "0jcyyxm6bci2kfba3937pglcplild56vcgasxygcnyah665zgy20";
        }) {})
      else
        (import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
          owner = "hamishmack"; repo = "nixpkgs";
          rev = "1a4b5101de556738c8b8d9941f15c4b2d852f6c0"; # branch gnome-3.30
          sha256 = "0fac2jjkjn1ad5zgnmmcwmgkh40h0dj0s5vcfl2qml9lqzb6ja1b";
        }) {})
, compiler ? "ghc844"
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

  haskell-gi-github = nixpkgs.fetchFromGitHub {
    owner = "haskell-gi";
    repo = "haskell-gi";
    rev = "41335d28392d95751877fbafdc439dace63357a7";
    sha256 = "1gmm96b5y6ikikn3am1zsjrkasc11llfzij2f6bvv7bfk8w61csv";
  };

  jsaddle-github = nixpkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "68208be806c49a2a0c9f037dfac85feae10a8c80";
    sha256 = "0acj0x716ikfb08ndib36jmwxkwq399lvkip46sfkh1ynn0pvc1c";
  };

  ghcjs-dom-github = nixpkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-dom";
    rev = "a6d51fcf0e79e7de50a0cb2088042e323133a7a8";
    sha256 = "06g0vvgvxxxzw9h2jqwkykam5jpngk6xlph29jyg92c00jms2bl4";
  };

  haddock-ghc86 = nixpkgs.fetchFromGitHub {
    owner = "haskell";
    repo = "haddock";
    rev = "39f591b945bc3e507c3c54ba762b26cb0fb9ded7";
    sha256 = "08hr6pg8dbb71f0hm6fqx0dxscmnpkp618anmjs5jqc492k7xbwg";
  };

  launch-leksah-script = nixpkgs.writeShellScriptBin "launch-leksah" ''
    "$@"
  '';

  extendedHaskellPackages = compiler: haskellPackages: (haskellPackages.extend (packageSourceOverrides {
      leksah         = cleanSrc;
      ltk            = filterSubmodule ./vendor/ltk;
      leksah-server  = filterSubmodule ./vendor/leksah-server;
      vcswrapper     = filterSubmodule ./vendor/haskellVCSWrapper/vcswrapper;
      vcsgui         = filterSubmodule ./vendor/haskellVCSGUI/vcsgui;
      system-fileio  = filterSubmodule ./vendor/haskell-filesystem/system-fileio;
      cabal-plan     = nixpkgs.fetchFromGitHub {
        owner = "haskell-hvr";
        repo = "cabal-plan";
        rev = "7e9bc5d83cbaeca3fa6327d645775aee5f48c662";
        sha256 = "1kv0vm9fpy8nvppjjsjjqybscq4c7pi2y13nvdmaxhjralcrh44c";
      };
      brittany       = filterSubmodule ./vendor/brittany;
      HaRe           = filterSubmodule ./vendor/HaRe;
      cabal-helper   = filterSubmodule ./vendor/HaRe/submodules/cabal-helper;
      ghc-exactprint = builtins.fetchTarball {
        url = "http://hackage.haskell.org/package/ghc-exactprint-0.5.8.2/ghc-exactprint-0.5.8.2.tar.gz";
        sha256 = "1y5bvnazplpjv38398vxg2vq1477jn1l5ymwg3vrxr3w4llywlsv";
      };
      ghc-mod        = filterSubmodule ./vendor/HaRe/submodules/ghc-mod;
      ghc-mod-core   = filterSubmodule ./vendor/HaRe/submodules/ghc-mod/core;
      git = nixpkgs.fetchFromGitHub {
        owner = "hamishmack";
        repo = "hs-git";
        rev = "c829ec133f603e376177deaa57a333ef84308af3";
        sha256 = "1hcl5izpcpi9sjfvmv1g5si1wg0jw9v9wbk5hvn30q67xcxwx891";
      };
      haskell-gi-overloading = "0.0";
      gi-javascriptcore = builtins.fetchTarball {
        url = "http://hackage.haskell.org/package/gi-javascriptcore-4.0.16/gi-javascriptcore-4.0.16.tar.gz";
        sha256 = "1wznff7qhr11d4cjmvz8fij7p54qbx5ibbr4vlbds5ml2ms2gx0p";
      };
      base-compat-batteries = "0.10.4";
      contravariant = "1.5";

#      Stuff that GHC 8.6.1 might need
#      http-types = "0.12.2";
#      aeson = "1.4.1.0";
#      haskell-src-exts = "1.20.3";
#      concurrent-output = "1.10.7";
#      unliftio = "0.2.8.1";
#      lifted-async = "0.10.0.3";
#      hedgehog = "0.6.1";
#      semigroupoids = "5.3.1";
#      base-orphans = "0.8";
#      free = "5.1";
#      lens = "4.17";
#      criterion = "1.5.1.0";
#      microlens-th = "${nixpkgs.fetchFromGitHub {
#        owner = "monadfix";
#        repo = "microlens";
#        rev = "b7a3f9c3c87343975aa8d67a8e238a92c132f62c";
#        sha256 = "1y0dbq9bhfsp6zdxrdpw9ifpqf91iadfd2rmr6szb8nilx04lyx5";
#      }}/microlens-th";
#      multistate = nixpkgs.fetchFromGitHub {
#        owner = "lspitzner";
#        repo = "multistate";
#        rev = "5c24daac43df086be952ffa6cb9d361681b44f6e";
#        sha256 = "18fnvv1rn9i7v65mfrqzdx54560h6z6sv0mxdyq8a3si946n189g";
#      };
#      haddock-library = "${haddock-ghc86}/haddock-library";
#      haddock-api = "${haddock-ghc86}/haddock-api";
#      hslogger = nixpkgs.fetchFromGitHub {
#        owner = "hamishmack";
#        repo = "hslogger";
#        rev = "e6cc40e095a5be77ede6aca8cb92d570b8c338d5";
#        sha256 = "0kn0zphiagx9raajkf6lvbz310hzadiqhvlc2g31h8slfqa02042";
#      };
#      memory = nixpkgs.fetchFromGitHub {
#        owner = "vincenthz";
#        repo = "hs-memory";
#        rev = "feee6256e19ed178dc75b071dc54983bc6320f26";
#        sha256 = "1vv1js5asaxbahvryxlxch14x3jq15a09xixhz330m3d77zfyiw9";
#      };
    })).extend( self: super:
      let jsaddlePkgs = import jsaddle-github self;
          ghcjsDom = import ghcjs-dom-github self;
      in {
        haddock-library = if compiler == "ghc822"
                            then dontCheck (dontHaddock (self.callHackage "haddock-library" "1.4.4" {}))
                            else if compiler == "ghc842" || compiler == "ghc843" || compiler == "ghc844"
                              then dontCheck (dontHaddock (self.callHackage "haddock-library" "1.6.0" {}))
                              else super.haddock-library;
        haddock-api = if compiler == "ghc822"
                            then dontCheck (self.callHackage "haddock-api" "2.18.1" {})
                            else if compiler == "ghc842" || compiler == "ghc843" || compiler == "ghc844"
                              then dontCheck (self.callHackage "haddock-api" "2.20.0" {})
                              else super.haddock-api;
        jsaddle = doJailbreak (jsaddlePkgs.jsaddle);
        jsaddle-warp = dontCheck jsaddlePkgs.jsaddle-warp;
        jsaddle-dom = import (nixpkgs.fetchFromGitHub {
           owner = "ghcjs";
           repo = "jsaddle-dom";
           rev = "bb8516918043d9164c05d016f37448ac13f6fe71";
           sha256 = "1a79y1prgynjhgfp7nfp3xws9bybyh0wdis44vfhkmc6bvz425fl";
        }) self;

        vado = doJailbreak super.vado;
        criterion = doJailbreak super.criterion;
        ghcjs-dom-jsaddle = ghcjsDom.ghcjs-dom-jsaddle;
        haskell-gi = super.haskell-gi.overrideAttrs (drv: {
          src = "${haskell-gi-github}";
        });
        haskell-gi-base = super.haskell-gi-base.overrideAttrs (drv: {
          src = "${haskell-gi-github}/base";
        });
        gi-gdk = super.gi-gdk.overrideAttrs(drv: {strictDeps = true;});
        gi-gtk = super.gi-gtk.overrideAttrs(drv: {strictDeps = true;});
        gi-gtksource = super.gi-gtksource.overrideAttrs(drv: {strictDeps = true;});
        gi-gtkosxapplication = (super.gi-gtkosxapplication.override {
          gtk-mac-integration-gtk3 = nixpkgs.gtk-mac-integration-gtk3;
        }).overrideAttrs(drv: {strictDeps = true;});

        cabal-helper        = dontCheck super.cabal-helper;
        
        cabal-plan          = if compiler == "ghc802" then null else super.cabal-plan;
        hlint               = dontCheck super.hlint;
        constrained-dynamic = doJailbreak super.constrained-dynamic;
        megaparsec          = dontCheck super.megaparsec;
        ghc-exactprint      = dontCheck super.ghc-exactprint;
        leksah-server       = dontCheck super.leksah-server;
        HaRe                = dontHaddock (dontCheck super.HaRe);
        text-replace        = doJailbreak super.text-replace;
        system-fileio       = dontCheck super.system-fileio;
        ref-tf              = doJailbreak super.ref-tf;
        concurrent-output   = doJailbreak super.concurrent-output;

#        Stuff that GHC 8.6.1 might need
#        czipwith            = doJailbreak super.czipwith;
#        data-tree-print     = doJailbreak super.data-tree-print;
#        neat-interpolation = dontCheck super.neat-interpolation;
#        HTF = null;
#        polyparse = null;
#        cpphs = null;
#        brittany = null;
#        haskell-src-exts = dontCheck super.haskell-src-exts;
#        vector-binary-instances = doJailbreak super.vector-binary-instances;
#        butcher = null;
#        monad-par = dontCheck super.monad-par;
#        http-types = dontCheck super.http-types;
#        aeson = doJailbreak super.aeson;

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
              --suffix 'PATH' ':' "${self.doctest}/bin" \
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
              --prefix 'PATH' ':' "${nixpkgs.cabal-install}/bin" \
              --suffix 'PATH' ':' "${self.doctest}/bin" \
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
  ghc844 = extendedHaskellPackages "ghc844" nixpkgs.pkgs.haskell.packages.ghc844;
  ghc = extendedHaskellPackages compiler nixpkgs.pkgs.haskell.packages.${compiler};

  leksah = ghc.wrapped-leksah "";
  leksah-ghc802 = ghc802.wrapped-leksah "-ghc802";
  leksah-ghc822 = ghc822.wrapped-leksah "-ghc822";
  leksah-ghc843 = ghc843.wrapped-leksah "-ghc843";
  leksah-ghc844 = ghc844.wrapped-leksah "-ghc844";

in leksah // {
  inherit ghc ghc802 ghc822 ghc843 ghc844;
  inherit leksah-ghc802 leksah-ghc822 leksah-ghc843 leksah-ghc844;
  inherit (ghc) launch-leksah;

  shells = {
    ghc = ghc.shellFor {
      packages = p: [ p.leksah p.leksah-server p.ltk p.vcswrapper p.vcsgui p.HaRe p.cabal-helper p.ghc-exactprint p.ghc-mod p.ghc-mod-core ];
    };
  };
}
