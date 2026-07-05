{ pkgs, config, ... }:
(
let
  sources = import ./sources.nix {};
  optionalAttrs = b: a: if b then a else {};
  system = null;
in
rec {
    projectFileName = "cabal.project";
    compiler-nix-name = "ghc914";
    builderVersion = 2;
    flake.variants = {
      "ghc96".compiler-nix-name = pkgs.lib.mkForce "ghc96";
      "ghc98".compiler-nix-name = pkgs.lib.mkForce "ghc98";
      "ghc910".compiler-nix-name = pkgs.lib.mkForce "ghc910";
      "ghc912".compiler-nix-name = pkgs.lib.mkForce "ghc912";
      "ghc914".compiler-nix-name = pkgs.lib.mkForce "ghc914";
    };
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
          pkgs.wrapGAppsHook3
          pkgs.makeWrapper
        ];
        packages.leksah.components.exes.leksah.libs = [
          pkgs.gtk3
          pkgs.dconf
          pkgs.adwaita-icon-theme
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
      })
    ];
    shell = {
      withHoogle = false;
      packages = ps: with ps; [
        leksah-server
        leksah
        ltk
        vcsgui
        vcswrapper
      ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin gi-gtkosxapplication;
      tools = {
        cabal = "latest";
        # Build HLS from its master branch rather than hackage: released HLS
        # can't solve for GHC 9.14 (hie-compat caps base < 4.22), but master's
        # cabal.project uses allow-newer to support it.  Passing `src` overrides
        # the `mkDefault` hackage source in modules/hackage-project.nix, so the
        # tool is built from this tree's own cabal.project.
        haskell-language-server.src = pkgs.hls-github-src;
      };
      buildInputs = [
        # (pkgs.vscode-with-extensions.override {
        #   vscodeExtensions = with pkgs.vscode-extensions; [
        #     justusadam.language-haskell
        #     haskell.haskell
        #     github.copilot
        #     github.copilot-chat
        #   ];
        # })
        # pkgs.stack
        pkgs.gobject-introspection
        pkgs.pkg-config
        pkgs.gtk3
        pkgs.gtksourceview3
        # Persistent web-UI terminals: each leksah terminal attaches to a tmux
        # session (on a private socket), so shells survive a leksah restart.
        pkgs.tmux
        # Image tooling for verifying UI changes (crop/convert screenshots).
        pkgs.buildPackages.imagemagick
      ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.gtk-mac-integration;
    };
})
