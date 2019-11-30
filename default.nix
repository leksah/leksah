{ pkgs ? import nixpkgs (haskellNixpkgsArgs // {
    overlays = haskellNixpkgsArgs.overlays ++ [ (import ./nix/overlays/gtk-debug.nix) ];
  } // (if system == null then {} else { inherit system; }))
, nixpkgs ? haskellNixSrc + "/nixpkgs"
, haskellNixpkgsArgs ? import haskellNixSrc
, haskellNixSrc ? builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/d23547027242fc0d7da4591b1bdf8e5aed96a770.tar.gz";
    sha256 = "0ygjzyfg9w4jilhb09607hmy08azq0rvfj49yyzcml0qzy6zgvc6";
  }
, haskellCompiler ? "ghc865"
, system ? null
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
    ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};
    modules = [
      { reinstallableLibGhc = true; }
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
        packages.leksah.components.library.libs = pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.darwin.libobjc;
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

