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
      filter = path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
        pkgs.lib.all (i: toString i != path) [
        	../.DS_Store ../osx/Leksah ../osx/keymap.lkshk ../osx/prefs.lkshp ../win32/SourceDir ../default.nix ../result
        	../nix ../lib.nix ../nix-tools.nix ../default.nix ../shell.nix
            # Old submodules not used any more
        	../vendor/HaRe
        	../vendor/brittany
        	../vendor/cabal-helper
        	../vendor/gi-gtk-hs
        	../vendor/haskell-filesystem
        	../vendor/hs-git
        	../vendor/reflex-platform
        	../vendor/yi
          ]
          && pkgs.lib.all (i: i != baseNameOf path) [ ".git" "dist-newstyle" "cabal.project.local" "dist" ".stack-work" ".vagrant" ".DS_Store" ]
          && pkgs.lib.all (i: !(pkgs.lib.hasSuffix i path)) [ ".dmg" ".msi" ".exe" ".lkshf" ".wixobj" ".wixpdb" ".wxs" "~" ]
          && pkgs.lib.all (i: !(pkgs.lib.hasPrefix i (baseNameOf path))) [ "result-" ".ghc.environment." ];
    };
  # our packages
  plan-nix = haskell.callCabalProjectToNix {
    src = cleanSrc;
    index-state = "2019-05-23T00:00:00Z";
    inherit (pkgs) ghc;
  };
  plan-pkgs = import "${plan-nix}";

  cabalPatch = pkgs.fetchpatch {
    url = "https://patch-diff.githubusercontent.com/raw/haskell/cabal/pull/6055.diff";
    sha256 = "145g7s3z9q8d18pxgyngvixgsm6gmwh1rgkzkhacy4krqiq0qyvx";
    stripLen = 1;
  };

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  compiler = (plan-pkgs.extras {}).compiler or
             (plan-pkgs.pkgs {}).compiler;

  pkgSet = haskell.mkCabalProjectPkgSet {
    inherit plan-pkgs;
    # The extras allow extension or restriction of the set of
    # packages we are interested in. By using the stack-pkgs.extras
    # we restrict our package set to the ones provided in stack.yaml.
    pkg-def-extras = [
      iohk-extras.${compiler.nix-name}
      # this one is missing from the plan.json; we can't yet force
      # os/arch with cabal to produce plans that are valid for multiple
      # os/archs. Luckily mac/linux are close enough to have mostly the
      # same build plan for windows however we need some hand holding for
      # now.
      (hackage: { mintty = hackage.mintty."0.1.2".revisions.default; })
      # (hackage: { reflex = hackage.reflex."0.6.1".revisions.default // { doExactConfig = true; }; })
      # (hackage: {
      #  reflex.revision = hackage.reflex."0.6.1".revisions.default;
      #  reflex.components.library.doExactConfig = true;
      #  reflex.flags.debug-trace-events = false; })
    ];
    modules = [
      # the iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.  For now we need to
      # list the packages that require template haskell
      # explicity here.
      iohk-module
      ({ config, ...}: {
        packages.Cabal.patches = [ cabalPatch ];
        packages.reflex.components.library.doExactConfig = true;
        packages.reflex-dom-core.components.library.doExactConfig = true;
        packages.reflex-dom-svg.components.library.doExactConfig = true;
        packages.haddock-api.components.library.doHaddock = false;
      })
    ];
  };

  nixpkgs = pkgs;
  launch-leksah-script = nixpkgs.writeShellScriptBin "launch-leksah" ''
    "$@"
  '';

in
  pkgSet.config.hsPkgs // {
    _config = pkgSet.config;
    inherit plan-nix cleanSrc;
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
