{ pkgs ? import nixpkgs (haskellNixpkgsArgs // {
    overlays = haskellNixpkgsArgs.overlays ++ [ (import ./nix/overlays/gtk-debug.nix) ];
  } // (if system == null then {} else { inherit system; }))
, nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/input-output-hk/nixpkgs/archive/d08a74315610096187a4e9da5998ef10e80de370.tar.gz";
    sha256 = "16i3n5p4h86aswj7y7accmkkgrrkc0xvgy7fl7d3bsv955rc5900";
  }
, haskellNixpkgsArgs ? import (builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/4a24da99c1c9783414372bdd0686f79f4e60bfe9.tar.gz";
    sha256 = "10w8mixdkqd194i68h22y11nl46imnc26f8xgxra33slr040paxb";
  })
, haskellCompiler ? "ghc865"
, system    ? null
}:
let
  cabalPatch = pkgs.fetchpatch {
    url = "https://patch-diff.githubusercontent.com/raw/haskell/cabal/pull/6055.diff";
    sha256 = "145g7s3z9q8d18pxgyngvixgsm6gmwh1rgkzkhacy4krqiq0qyvx";
    stripLen = 1;
  };
  project = pkgs.haskell-nix.cabalProject' {
    name = "leksah";
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    pkg-def-extras = [ pkgs.ghc-boot-packages.${haskellCompiler} ];
    ghc = pkgs.buildPackages.pkgs.haskell.compiler.${haskellCompiler};
    modules = [
      { reinstallableLibGhc = true;
        nonReinstallablePkgs =
          [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
            "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
            # ghcjs custom packages
            "ghcjs-prim" "ghcjs-th" ];
      }
      ({ config, ...}: {
        packages.Cabal.patches = [ cabalPatch ];
        packages.haddock-api.components.library.doHaddock = false;
        # packages.leksah.components.sublibs.leksah-nogtk.doHaddock = false;
        packages.gi-gtk.components.setup.frameworks = pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.Cocoa;
        packages.gi-gtkosxapplication.components.setup.frameworks = pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.Cocoa;
        packages.gi-gtksource.components.setup.frameworks = pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.Cocoa;
        packages.gi-gtk-hs.components.library.frameworks = pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.Cocoa;
        packages.vcsgui.components.library.frameworks = pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.Carbon;
        packages.vcsgui.components.exes.vcsgui.frameworks = pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.Carbon;
        packages.ltk.components.library.frameworks = pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.Carbon;
        packages.leksah.components.library.frameworks = pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.CoreGraphics;
      })
    ];
  };
  launch-leksah-script = pkgs.writeShellScriptBin "launch-leksah" ''
    "$@"
  '';
  launch-leksah = pkgs.stdenv.mkDerivation {
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
          --prefix 'PATH' ':' "${pkgs.cabal-install}/bin" \
          --suffix 'PATH' ':' "${project.hsPkgs.doctest.components.exes.doctest}/bin" \
          --suffix 'LD_LIBRARY_PATH' ':' "${pkgs.cairo}/lib" \
          --set 'XDG_DATA_DIRS' ""
        '';
  };
  wrapped-leksah = pkgs.stdenv.mkDerivation {
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
        ln -s ${project.hsPkgs.leksah.components.exes.leksah}/bin/leksah $out/bin/leksah
        wrapProgram $out/bin/leksah \
          --prefix 'PATH' ':' "${project.hsPkgs.leksah-server.components.exes.leksah-server}/bin" \
          --prefix 'PATH' ':' "${project.hsPkgs.vcsgui.components.exes.vcsgui}/bin" \
          --prefix 'PATH' ':' "${pkgs.cabal-install}/bin" \
          --suffix 'PATH' ':' "${project.hsPkgs.doctest.components.exes.doctest}/bin" \
          --suffix 'LD_LIBRARY_PATH' ':' "${pkgs.cairo}/lib" \
          --set 'XDG_DATA_DIRS' ""
      '';
  };
  shells = {
    ghc = (project.hsPkgs.shellFor {
        packages = ps: with ps; [
          leksah-server
          leksah
          ltk ];
      }).overrideAttrs (oldAttrs: {
        buildInputs = pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.Carbon;
      });
  };
in
  project // {
    inherit shells launch-leksah wrapped-leksah pkgs;
  }

