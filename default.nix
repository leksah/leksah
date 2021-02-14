{ compiler-nix-name ? "ghc8104"
, system ? null
}:
let
  sources = import ./nix/sources.nix {};
  haskellNix = import sources."haskellNix" {};
  pkgs = import haskellNix.sources.nixpkgs-unstable (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [ (import ./nix/overlays/gtk-debug.nix) ];
  } // (if system == null then {} else { inherit system; }));
  project = pkgs.haskell-nix.project' {
    inherit compiler-nix-name;
    name = "leksah";
    #src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
    #  src = pkgs.haskell-nix.haskellLib.cleanGits {
    #    src = ../.; name = "leksah"; gitDirs = [ "leksah" "reflex" "reflex-dom" ];
    #  };
    #  subDir = "leksah";
    #  includeSiblings = true;
    #};
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "leksah"; };
    projectFileName = "cabal.project";
    modules = [{
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
            --prefix 'PATH' ':' "${project.tool "cabal" "latest"}/bin" \
            --suffix 'PATH' ':' "${pkgs.haskell-nix.compiler.${compiler-nix-name}}/bin"
        '';
        packages.leksah.components.exes.leksah.build-tools = [
          pkgs.wrapGAppsHook
          pkgs.makeWrapper
        ];
        packages.leksah.components.exes.leksah.libs = [
          pkgs.gnome3.gtk
          pkgs.gnome3.dconf
          pkgs.gnome3.defaultIconTheme
          pkgs.gnome3.gsettings_desktop_schemas
        ];
        packages.leksah.components.exes.leksah.postInstall = ''
          ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
            mkdir -p $out/share
            cp -r ${./linux} $out/share/
          ''}
          wrapProgram $out/bin/leksah \
            --prefix 'PATH' ':' "${project.hsPkgs.leksah-server.components.exes.leksah-server}/bin" \
            --prefix 'PATH' ':' "${project.hsPkgs.vcsgui.components.exes.vcsgui}/bin" \
            --prefix 'PATH' ':' "${project.tool "cabal" "latest"}/bin" \
            --suffix 'PATH' ':' "${pkgs.haskell-nix.compiler.${compiler-nix-name}}/bin" \
            --suffix 'PATH' ':' "${project.hsPkgs.doctest.components.exes.doctest}/bin" \
            --suffix 'LD_LIBRARY_PATH' ':' "${pkgs.cairo}/lib" \
            --set 'XDG_DATA_DIRS' ""
        '';
        packages.leksah.components.exes.leksah-warp.build-tools = [
          pkgs.makeWrapper
        ];
        packages.leksah.components.exes.leksah-warp.postInstall = ''
          ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
            mkdir -p $out/share
            cp -r ${./linux} $out/share/
          ''}
          wrapProgram $out/bin/leksah-warp \
            --prefix 'PATH' ':' "${project.hsPkgs.leksah-server.components.exes.leksah-server}/bin" \
            --prefix 'PATH' ':' "${project.tool "cabal" "latest"}/bin" \
            --suffix 'PATH' ':' "${pkgs.haskell-nix.compiler.${compiler-nix-name}}/bin" \
            --suffix 'PATH' ':' "${project.hsPkgs.doctest.components.exes.doctest}/bin" \
            --set 'XDG_DATA_DIRS' ""
        '';
      }
      (pkgs.lib.optionalAttrs (compiler-nix-name == "ghc865") {
        packages.haddock-api.components.library.doHaddock = false;
      })
      (pkgs.lib.optionalAttrs (compiler-nix-name == "ghc8102" || compiler-nix-name == "ghc8103" || compiler-nix-name == "ghc8104") {
        packages.haddock-api.src = sources.haddock-ghc8102 + "/haddock-api";
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
          --prefix 'PATH' ':' "${project.tool "cabal" "latest"}/bin" \
          --suffix 'PATH' ':' "${pkgs.haskell-nix.compiler.${compiler-nix-name}}/bin" \
          --suffix 'PATH' ':' "${project.hsPkgs.doctest.components.exes.doctest}/bin" \
          --suffix 'LD_LIBRARY_PATH' ':' "${pkgs.cairo}/lib" \
          --set 'XDG_DATA_DIRS' ""
        '';
  };
  shells = {
    ghc = (project.shellFor {
        packages = ps: with ps; [
          leksah-server
          leksah
          ltk ];
        tools = {
          cabal = "latest";
        };
        buildInputs = [
          pkgs.stack
          pkgs.gobject-introspection
          pkgs.pkgconfig
          pkgs.gtk3
          pkgs.gtksourceview3
        ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.gtk-mac-integration;
      });
  };
in project.hsPkgs.leksah.components.exes.leksah // {
    inherit shells launch-leksah pkgs;
  }

