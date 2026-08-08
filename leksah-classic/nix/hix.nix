{ pkgs, config, ... }:
(
let
  isGhc914sh = config.compiler-nix-name == "ghc914-sh";
  # GHC 9.14 (stable-haskell fork): C-only "clib" packages have no Haskell
  # deps, so no rts unit lands in their per-component package DB and GHC's
  # mkUnitState check panics ("The RTS for rts:nonthreaded-nodebug is
  # missing").  The fork's -no-rts flag bypasses the check.  (libyaml-clib
  # reaches this project through hlint → yaml.)
  # This must live in the cabal.project text (not nix-module ghcOptions):
  # package ghc-options enter cabal's UnitId hash, so the v2 slice builder's
  # plan-time and build-time views have to agree on them.
  clibNoRts = pkgs.lib.optionalString isGhc914sh ''
    package libyaml-clib
      ghc-options: -no-rts
  '';
  # Hackage packages whose released code doesn't compile against the
  # stable-haskell fork's Cabal 3.17 / ghc-9.14 API.  The fix is applied to the
  # hackage source here and the PATCHED source is handed to the cabal solver as
  # a local `packages:` entry, so the plan and the build see the same code.
  patchedHackage = name: version: patch: pkgs.applyPatches {
    name = "${name}-${version}-patched";
    src = pkgs.haskell-nix.hackageTarball { inherit name version; };
    patches = [ patch ];
  };
  # cabal-doctest: custom-setup dep of haskell-gi (the whole gi-* GTK3 stack);
  # the fork's Cabal 3.17 split Verbosity into VerbosityFlags + handles.  Its
  # tarball .cabal caps Cabal <3.16, hence the allow-newer (the patched code is
  # CPP-guarded for both APIs).
  cabalDoctestPatched = pkgs.lib.optionalString isGhc914sh ''
    packages: ${patchedHackage "cabal-doctest" "1.0.12" ./patches/cabal-doctest-cabal-3.17.patch}
    allow-newer: cabal-doctest:Cabal
  '';
in
{
  projectFileName = "cabal.project";
  cabalProjectLocal = clibNoRts + cabalDoctestPatched;
  # The same compiler the classic IDE was built with while it still lived in
  # the main project's plan: the stable-haskell GHC 9.14 fork.  (The package's
  # own frozen bounds — base <4.20, containers <0.7 — are lifted by the
  # `allow-newer: *:base` etc. in cabal.project, as they were there.)
  compiler-nix-name = "ghc914-sh";
  name = "leksah-classic";
  # v2 slice builds (as in the main project).  mkForce: hkm/stable-haskell's
  # cabal-project.nix sets builderVersion itself, so a plain assignment
  # collides ("conflicting definition values").
  builderVersion = pkgs.lib.mkForce 2;
  # No cross targets: this is a native GTK3 desktop app.
  crossPlatforms = _: [];
  modules = [({pkgs, lib, config, ...}: let
      inherit (config) hsPkgs;
      inherit (pkgs.stdenv.hostPlatform) isWindows;
    in {
      packages.leksah-server.components.exes.leksah-server.build-tools =
        lib.optionals (!isWindows) [
          pkgs.makeWrapper
        ];
      packages.leksah-server.components.exes.leksah-server.postInstall =
        lib.optionalString (!isWindows) ''
        wrapProgram $out/bin/leksah-server \
          --prefix 'PATH' ':' "${pkgs.haskell-nix.tool config.compiler.nix-name "cabal" "latest"}/bin" \
          --suffix 'PATH' ':' "${pkgs.haskell-nix.compiler.${config.compiler.nix-name}}/bin"
      '';
      packages.leksah-classic.components.exes.leksah-classic.build-tools =
        lib.optionals (!isWindows) [
          pkgs.wrapGAppsHook3
          pkgs.makeWrapper
        ];
      packages.leksah-classic.components.exes.leksah-classic.libs =
        lib.optionals (!isWindows) [
          pkgs.gtk3
          pkgs.dconf
          pkgs.adwaita-icon-theme
          pkgs.gsettings-desktop-schemas
        ];
      # leksah-classic shells out to leksah-server (metadata) and vcsgui (VCS
      # dialogs), and wants cabal/ghc on PATH for the builds it drives.
      packages.leksah-classic.components.exes.leksah-classic.postInstall =
        lib.optionalString (!isWindows) ''
        ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
          mkdir -p $out/share
          cp -r ${../linux} $out/share/linux
        ''}
        wrapProgram $out/bin/leksah-classic \
          --prefix 'PATH' ':' "${hsPkgs.leksah-server.components.exes.leksah-server}/bin" \
          --prefix 'PATH' ':' "${hsPkgs.vcsgui.components.exes.vcsgui}/bin" \
          --prefix 'PATH' ':' "${pkgs.haskell-nix.tool config.compiler.nix-name "cabal" "latest"}/bin" \
          --suffix 'PATH' ':' "${pkgs.haskell-nix.compiler.${config.compiler.nix-name}}/bin" \
          --suffix 'PATH' ':' "${hsPkgs.doctest.components.exes.doctest}/bin" \
          --suffix 'LD_LIBRARY_PATH' ':' "${pkgs.cairo}/lib" \
          --set 'XDG_DATA_DIRS' ""
      '';
    })
  ];
  shell = {
    withHoogle = false;
    crossPlatforms = _: [];
    packages = ps: with ps; [
      leksah-classic
      leksah-server
      ltk
      vcsgui
      vcswrapper
    ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin gi-gtkosxapplication;
    tools = {
      # The stable-haskell cabal FORK (Cabal 3.17) — the SAME cabal the v2
      # slice builder runs; mainline cabal can't see the v2 composed store and
      # would re-plan/rebuild everything.
      cabal = pkgs.pkgsBuildBuild.haskell-nix.v2-cabal-install;
    };
    buildInputs = [
      pkgs.gobject-introspection
      pkgs.pkg-config
      pkgs.gtk3
      pkgs.gtksourceview3
    ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.gtk-mac-integration;
  };
})
