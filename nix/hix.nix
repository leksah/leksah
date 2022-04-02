{ pkgs, config, ... }:
(
let
  sources = import ./sources.nix {};
  optionalAttrs = b: a: if b then a else {};
  system = null;
in
rec {
    haskellNix = import sources."haskellNix" {};
    overlays = [ (import ./overlays/gtk-debug.nix) ];
    projectFileName = "cabal.project";
    compiler-nix-name = pkgs.lib.mkDefault "ghc8107";
    name = "leksah";
    modules = [({pkgs, config, ...}: let inherit (config) hsPkgs; in {
        packages.reflex.components.tests.hlint.buildable = pkgs.lib.mkForce false;
        packages.reflex.components.tests.RequesterT.buildable = pkgs.lib.mkForce false;
        packages.reflex.components.tests.QueryT.buildable = pkgs.lib.mkForce false;
        packages.reflex.components.tests.EventWriterT.buildable = pkgs.lib.mkForce false;
        packages.reflex.components.tests.DebugCycles.buildable = pkgs.lib.mkForce false;
        packages.leksah-server.components.exes.leksah-server.build-tools = [
          pkgs.makeWrapper
        ];
        packages.leksah-server.components.exes.leksah-server.postInstall = ''
          wrapProgram $out/bin/leksah-server \
            --prefix 'PATH' ':' "${pkgs.haskell-nix.tool config.compiler.nix-name "cabal" "latest"}/bin" \
            --suffix 'PATH' ':' "${pkgs.haskell-nix.compiler.${config.compiler.nix-name}}/bin"
        '';
        packages.leksah.components.exes.leksah.build-tools = [
          pkgs.wrapGAppsHook
          pkgs.makeWrapper
        ];
        packages.leksah.components.exes.leksah.libs = [
          pkgs.gtk3
          pkgs.dconf
          pkgs.gnome3.adwaita-icon-theme
          pkgs.gsettings-desktop-schemas
        ];
        packages.leksah.components.exes.leksah.postInstall = ''
          ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
            mkdir -p $out/share
            cp -r ${../linux} $out/share/
          ''}
          wrapProgram $out/bin/leksah \
            --prefix 'PATH' ':' "${hsPkgs.leksah-server.components.exes.leksah-server}/bin" \
            --prefix 'PATH' ':' "${hsPkgs.vcsgui.components.exes.vcsgui}/bin" \
            --prefix 'PATH' ':' "${pkgs.haskell-nix.tool config.compiler.nix-name "cabal" "latest"}/bin" \
            --suffix 'PATH' ':' "${pkgs.haskell-nix.compiler.${config.compiler.nix-name}}/bin" \
            --suffix 'PATH' ':' "${hsPkgs.doctest.components.exes.doctest}/bin" \
            --suffix 'LD_LIBRARY_PATH' ':' "${pkgs.cairo}/lib" \
            --set 'XDG_DATA_DIRS' ""
        '';
        packages.leksah.components.exes.leksah-warp.build-tools = [
          pkgs.makeWrapper
        ];
        packages.leksah.components.exes.leksah-warp.postInstall = ''
          ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
            mkdir -p $out/share
            cp -r ${../linux} $out/share/
          ''}
          wrapProgram $out/bin/leksah-warp \
            --prefix 'PATH' ':' "${hsPkgs.leksah-server.components.exes.leksah-server}/bin" \
            --prefix 'PATH' ':' "${pkgs.haskell-nix.tool config.compiler.nix-name "cabal" "latest"}/bin" \
            --suffix 'PATH' ':' "${pkgs.haskell-nix.compiler.${config.compiler.nix-name}}/bin" \
            --suffix 'PATH' ':' "${hsPkgs.doctest.components.exes.doctest}/bin" \
            --set 'XDG_DATA_DIRS' ""
        '';
        packages.haddock-api.components.library.doHaddock = config.compiler.nix-name != "ghc865";
      })
      { # Allow Cabal to reinstall
        nonReinstallablePkgs =
          [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
            "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
            # ghcjs custom packages
            "ghcjs-prim" "ghcjs-th"
            "ghc-boot"
            "ghc" "Win32" "array" "binary" "bytestring" "containers"
            "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
            # "ghci" "haskeline"
            "hpc"
            "mtl" "parsec" "process" "text" "time" "transformers"
            "unix" "xhtml" "terminfo"
            # "stm"
          ];
      }
    ];
    shell = {
      withHoogle = false;
      packages = ps: with ps; [
        leksah-server
        leksah
        ltk
      ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin gi-gtkosxapplication;
      tools = {
        cabal = "latest";
      };
      buildInputs = [
        # pkgs.stack
        pkgs.gobject-introspection
        pkgs.pkgconfig
        pkgs.gtk3
        pkgs.gtksourceview3
      ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.gtk-mac-integration;
    };
})
