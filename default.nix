{ nixpkgs ?
    # Default for CI reproducibility, optionally override in your configuration.nix.
    (import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "NixOS"; repo = "nixpkgs";
      rev = "c8e7aab0c8bae8a49ec5bd87ace65b237c8e3d18";
      sha256 = "0dq2ymqygc6dadrlm1jcbqsg7w34yihb7gss9yk42lknajzvm9pm";
    }) {})
, compiler ? "ghc802" # TODO: try using "default"?
, haskellPackages ? if compiler == "default"
                      then nixpkgs.pkgs.haskellPackages
                      else nixpkgs.pkgs.haskell.packages.${compiler}
}:

with nixpkgs.pkgs.haskell.lib;

let

  inherit (nixpkgs) pkgs;

  fixCairoGI = p: overrideCabal p (drv: {
    preCompileBuildDriver = (drv.preCompileBuildDriver or "") + ''
      export LD_LIBRARY_PATH="${pkgs.cairo}/lib"
    '';
  });

  extendedHaskellPackages = haskellPackages.override {
    overrides = self: super:
      let
        jsaddlePkgs = import ./vendor/jsaddle   self; # FIXME: unused?   # TODO: if not, use `fetchFromGitHub`?
        ghcjsDom    = import ./vendor/ghcjs-dom self; # TODO: use `fetchFromGitHub`?
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

        gi-cairo = fixCairoGI super.gi-cairo;
        gi-gdk = fixCairoGI super.gi-gdk;
        gi-gtk = fixCairoGI super.gi-gtk;
        gi-javascriptcore = super.gi-javascriptcore_4_0_11;
        gi-pango = fixCairoGI super.gi-pango;
        gi-webkit2 = fixCairoGI super.gi-webkit2;
        gi-gtksource = fixCairoGI super.gi-gtksource;
        gi-gtkosxapplication = fixCairoGI (super.gi-gtkosxapplication.override {
          gtk-mac-integration-gtk3 = pkgs.gtk-mac-integration-gtk3;
        });
        webkit2gtk3-javascriptcore = overrideCabal super.webkit2gtk3-javascriptcore (drv: {
          preConfigure = ''
            mkdir dispatch
            sed 's|^\(typedef void [(]\)\^\(dispatch_block_t[)][(]void[)];\)$|\1\2|' <"${pkgs.stdenv.cc.libc}/include/dispatch/object.h" >dispatch/object.h
            '';
        });

        haskell-gi-overloading = super.haskell-gi-overloading_0_0;

        # FIXME: do we really need them as Git submodules?
        vcswrapper = self.callCabal2nix "vcswrapper" ./vendor/haskellVCSWrapper/vcswrapper {};
        vcsgui = self.callCabal2nix "vcsgui" ./vendor/haskellVCSGUI/vcsgui {};
        ltk = overrideCabal (self.callCabal2nix "ltk" ./vendor/ltk {}) (drv: {
          libraryPkgconfigDepends = with pkgs; [ gnome3.gtk.dev ] ++ (if stdenv.isDarwin then [ gtk-mac-integration-gtk3 ] else []);
        });
        leksah-server = dontCheck (self.callCabal2nix "leksah-server" ./vendor/leksah-server {}); # FIXME: really `dontCheck`?

        # TODO: optionally add:
        # • yi >=0.12.4 && <0.13,
        # • yi-language >=0.2.0 && <0.3,
        # • yi-rope >=0.7.0.1 && <0.8
      };
  };

  cleanSrc =
    builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
      nixpkgs.lib.all (i: toString i != path) [ ./.DS_Store ./osx/Leksah ./osx/keymap.lkshk ./osx/prefs.lkshp ./win32/SourceDir ./default.nix ]
        && nixpkgs.lib.all (i: i != baseNameOf path) [ ".git" "dist-newstyle" "cabal.project.local" "dist" ".stack-work" ".vagrant" ".DS_Store" ]
      	&& nixpkgs.lib.all (i: !(nixpkgs.lib.hasSuffix i path)) [ ".dmg" ".msi" ".exe" ".lkshf" ".wixobj" ".wixpdb" ".wxs" ]
      	# TODO: what else?
      ) ./.;

  drv = overrideCabal (extendedHaskellPackages.callCabal2nix "leksah" cleanSrc {}) (oldAttrs: with pkgs; with extendedHaskellPackages; {

    libraryHaskellDepends = (oldAttrs.libraryHaskellDepends or [])
      ++ (if stdenv.isDarwin then [
            gi-gtkosxapplication
            gtk-mac-integration-gtk3 # TODO: does this need to be in 2 places?
            darwin.libobjc
            buildPackages.darwin.apple_sdk.frameworks.Cocoa
            buildPackages.darwin.apple_sdk.libs.xpc
            (buildPackages.osx_sdk or null)
          ] else []);

    buildDepends = (oldAttrs.buildDepends or [])
      ++ [ happy alex gnome3.dconf ];

    libraryPkgconfigDepends = (oldAttrs.libraryPkgconfigDepends or [])
      ++ [ gnome3.gtk.dev gnome3.gtksourceview gnome3.webkitgtk cairo gnome3.gsettings_desktop_schemas ]
      ++ (if stdenv.isDarwin then [ gtk-mac-integration-gtk3 ] else []);

  });

  build = pkgs.stdenv.lib.overrideDerivation drv (oldAttrs: {
    nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ (with pkgs; [ wrapGAppsHook makeWrapper ]);
    postInstall = if pkgs.stdenv.isLinux then ''
      mkdir -p $out/share
      cp -r linux/. $out/share/
    '' else "";
    postFixup = ''
      wrapProgram $out/bin/leksah \
        --prefix 'PATH' ':' "${extendedHaskellPackages.leksah-server}/bin"
    '';
  });

  env = pkgs.stdenv.lib.overrideDerivation build.env (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      extendedHaskellPackages.leksah-server
      # TODO: perhaps add some additional stuff to nix-shell PATH
    ];
    shellHook = ''
      export CFLAGS="$NIX_CFLAGS_COMPILE" # TODO: why is this needed?
      export XDG_DATA_DIRS="$GSETTINGS_SCHEMAS_PATH:$XDG_DATA_DIRS" # TODO: how to do this better?
    '';
  });

in build // { inherit env; }
