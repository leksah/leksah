{ pkgs ? import nixpkgs ({
    overlays = (import (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/e61ccef60aa3505740a563dff06e8dd48404a205.tar.gz";
        sha256 = "0fbhqnaqxaf4r57xia1kwpfpq5bbzqqcc476yvhq8z1b2drpqgfp";
      } + "/overlays")
    ) ++ [ (import ./nix/overlays/gtk-debug.nix) ];
    inherit config system crossSystem;
  })
, nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/61f0936d1cd73760312712615233cd80195a9b47.tar.gz";
    sha256 = "1fkmp99lxd827km8mk3cqqsfmgzpj0rvaz5hgdmgzzyji70fa2f8";
  }
, haskellCompiler ? "ghc865"
, config ? {}
, system ? builtins.currentSystem
, crossSystem ? null
}:
let
  cabalPatch = pkgs.fetchpatch {
    url = "https://patch-diff.githubusercontent.com/raw/haskell/cabal/pull/6055.diff";
    sha256 = "145g7s3z9q8d18pxgyngvixgsm6gmwh1rgkzkhacy4krqiq0qyvx";
    stripLen = 1;
  };
  project = pkgs.haskell-nix.cabalProject' {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    pkg-def-extras = [ pkgs.ghc-boot-packages.${haskellCompiler} ];
    ghc = pkgs.buildPackages.pkgs.haskell.compiler.${haskellCompiler};
    modules = [
      { reinstallableLibGhc = true; }
      ({ config, ...}: {
        packages.Cabal.patches = [ cabalPatch ];
        packages.haddock-api.components.library.doHaddock = false;
        # packages.leksah.components.sublibs.leksah-nogtk.doHaddock = false;
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
    ghc = (project.hsPkgs.shellFor {}).overrideAttrs (oldAttrs: {
      shellHook = (oldAttrs.shellHook or "") + ''
        unset CABAL_CONFIG
      '';
    });
  };
in
  project // {
    inherit shells launch-leksah wrapped-leksah;
  }

