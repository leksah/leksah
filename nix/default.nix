{ pkgs ? import <nixpkgs> {}
, iohk-extras ? {}
, iohk-module ? {}
, haskell
, ...
}:
let
  cleanSrc =
    pkgs.lib.cleanSourceWith {
      src = ./..;
      filter = path: type:
             pkgs.lib.all (i: i != baseNameOf path) [ "result" ".git" "dist-newstyle" "cabal.project.local" "dist" ".stack-work" ".vagrant" ".DS_Store" ]
          && pkgs.lib.all (i: !(pkgs.lib.hasSuffix i path)) [ ".dmg" ".msi" ".exe" ".lkshf" ".wixobj" ".wixpdb" ".wxs" "~" ]
          && pkgs.lib.all (i: !(pkgs.lib.hasPrefix i (baseNameOf path))) [ "result-" ".ghc.environment." ];
    };
  # our packages
  plan = haskell.callCabalProjectToNix {
    src = cleanSrc;
    index-sha256 = "1gpazw5sadv2allfcji9gbknfxc0zcxxmv4l9k5rn33x6bwz19f3";
  };

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  compiler = (plan.pkgs.extras {}).compiler or
             (plan.pkgs.pkgs {}).compiler;

  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = plan.pkgs;
    # The extras allow extension or restriction of the set of
    # packages we are interested in. By using the stack-pkgs.extras
    # we restrict our package set to the ones provided in stack.yaml.
    pkg-def-extras = [
      (hackage: { libiserv = {}; })
      iohk-extras.${compiler.nix-name}
    ];
    modules = [
      # the iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.  For now we need to
      # list the packages that require template haskell
      # explicity here.
      iohk-module
      { reinstallableLibGhc = true; }
      ({ config, ...}: {
        packages.ghc.patches = [ ./patches/ghc/MR948--32bit-cross-th.patch ];
        packages.reflex.components.library.doExactConfig = true;
        packages.reflex-dom-core.components.library.doExactConfig = true;
        packages.reflex-dom-svg.components.library.doExactConfig = true;
        packages.haddock-api.components.library.doHaddock = false;
        packages.gi-gtkosxapplication.components.library.doExactConfig = true;
      })
    ];
  };

  nixpkgs = pkgs;
  launch-leksah-script = nixpkgs.writeShellScriptBin "launch-leksah" ''
    "$@"
  '';

in {
  plan-nix = plan.nix;
  inherit (pkgSet) config;
  inherit (pkgSet.config) hsPkgs;
  inherit pkgs;
  launch-leksah = nixpkgs.stdenv.mkDerivation {
      name = "launch-leksah";
      nativeBuildInputs = with nixpkgs; [ wrapGAppsHook makeWrapper ];
      buildInputs = with nixpkgs; [
        gnome3.gtk
        gnome3.dconf
        gnome3.defaultIconTheme
        gnome3.gsettings_desktop_schemas
      ];
      src = ../linux;
      buildPhase = ''
          mkdir -p $out
        '';
      installPhase = ''
        mkdir -p $out/bin
        ln -s ${launch-leksah-script}/bin/launch-leksah $out/bin
        cp launch-leksah/Info.plist $out/bin
        wrapProgram $out/bin/launch-leksah \
          --prefix 'PATH' ':' "${nixpkgs.cabal-install}/bin" \
          --suffix 'PATH' ':' "${pkgSet.config.hsPkgs.doctest.components.exes.doctest}/bin" \
          --suffix 'LD_LIBRARY_PATH' ':' "${nixpkgs.cairo}/lib" \
          --set 'XDG_DATA_DIRS' ""
        '';
  };
  wrapped-leksah = nixpkgs.stdenv.mkDerivation {
      name = "leksah";
      nativeBuildInputs = with nixpkgs; [ wrapGAppsHook makeWrapper ];
      buildInputs = with nixpkgs; [
        gnome3.gtk
        gnome3.dconf
        gnome3.defaultIconTheme
        gnome3.gsettings_desktop_schemas
      ];
      src = ../linux;
      buildPhase =
        if nixpkgs.stdenv.isLinux then ''
          mkdir -p $out/share
          cp -r * $out/share/
        '' else ''
          mkdir -p $out
        '';
      installPhase = ''
        mkdir -p $out/bin
        ln -s ${pkgSet.config.hsPkgs.leksah.components.exes.leksah}/bin/leksah $out/bin/leksah
        wrapProgram $out/bin/leksah \
          --prefix 'PATH' ':' "${pkgSet.config.hsPkgs.leksah-server.components.exes.leksah-server}/bin" \
          --prefix 'PATH' ':' "${pkgSet.config.hsPkgs.vcsgui.components.exes.vcsgui}/bin" \
          --prefix 'PATH' ':' "${nixpkgs.cabal-install}/bin" \
          --suffix 'PATH' ':' "${pkgSet.config.hsPkgs.doctest.components.exes.doctest}/bin" \
          --suffix 'LD_LIBRARY_PATH' ':' "${nixpkgs.cairo}/lib" \
          --set 'XDG_DATA_DIRS' ""
      '';
  };
}
