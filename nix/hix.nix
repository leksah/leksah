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
    # v2 slice builds for the native platforms (what leksah's own incremental
    # builds use).  The mingw cross must use the classic builder: v2 compiles
    # custom Setup.hs (entropy, ghc-paths) with the cross GHC, producing a
    # setup.exe the Linux build host can't run; v1's setup-builder uses the
    # build compiler.  projectCross re-evaluates this module with the cross
    # pkgs, so the condition picks the right builder per platform.
    builderVersion = if pkgs.stdenv.hostPlatform.isWindows then 1 else 2;
    flake.variants = {
      "ghc96".compiler-nix-name = pkgs.lib.mkForce "ghc96";
      "ghc98".compiler-nix-name = pkgs.lib.mkForce "ghc98";
      "ghc910".compiler-nix-name = pkgs.lib.mkForce "ghc910";
      "ghc912".compiler-nix-name = pkgs.lib.mkForce "ghc912";
      "ghc914".compiler-nix-name = pkgs.lib.mkForce "ghc914";
    };
    name = "leksah";
    # Windows cross (leksah-webview2), built from the x86_64-linux builder
    # only: TH runs under wine/iserv there; darwin can't host the mingw iserv.
    crossPlatforms = p:
      pkgs.lib.optionals (pkgs.stdenv.hostPlatform.system == "x86_64-linux")
        [ p.ucrt64 ];
    modules = [({pkgs, lib, config, ...}: let
        inherit (config) hsPkgs;
        inherit (pkgs.stdenv.hostPlatform) isWindows;
        # WebView2.h for jsaddle-webview2's C shim (compile time) and
        # WebView2Loader.dll for the installed exe (run time).  Only the
        # header is needed at build time — the DLL is loaded dynamically.
        webview2-sdk = pkgs.pkgsBuildBuild.fetchzip {
          url = "https://www.nuget.org/api/v2/package/Microsoft.Web.WebView2/1.0.4022.49";
          extension = "zip";
          stripRoot = false;
          hash = "sha256-RoVh4A/Pg9/40kHtIIsC916QgPkB8TnDeOvN4ptPNM4=";
        };
      in {
        packages.reflex.components.tests.hlint.buildable = pkgs.lib.mkForce false;
        packages.reflex.components.tests.RequesterT.buildable = pkgs.lib.mkForce false;
        packages.reflex.components.tests.QueryT.buildable = pkgs.lib.mkForce false;
        packages.reflex.components.tests.EventWriterT.buildable = pkgs.lib.mkForce false;
        packages.reflex.components.tests.DebugCycles.buildable = pkgs.lib.mkForce false;
        # jsaddle-webview2's C shim needs WebView2.h on the include path (see
        # webview2-sdk above); nothing from the SDK is linked.
        package-keys = ["jsaddle-webview2"];
        packages.jsaddle-webview2.components.library.configureFlags =
          lib.optionals isWindows
            [ "--extra-include-dirs=${webview2-sdk}/build/native/include" ];
        # Match the jsaddle project's proven mingw config.
        enableStatic = lib.mkIf isWindows true;
        # bitvec's SIMD cbits use __builtin_cpu_supports, whose __cpu_model
        # (libgcc) symbol the iserv RTS linker can't resolve when loading the
        # unit for TH under wine; the flag drops the cbits.
        packages.bitvec.flags.simd = lib.mkIf isWindows (lib.mkForce false);
        # Ship the loader DLL next to the exe (it is LoadLibrary'd at startup).
        packages.leksah.components.exes.leksah-webview2.postInstall =
          lib.optionalString isWindows ''
            cp ${webview2-sdk}/runtimes/win-x64/native/WebView2Loader.dll $out/bin/
          '';
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
        packages.leksah.components.exes.leksah.build-tools =
          lib.optionals (!isWindows) [
            pkgs.wrapGAppsHook3
            pkgs.makeWrapper
          ];
        packages.leksah.components.exes.leksah.libs =
          lib.optionals (!isWindows) [
            pkgs.gtk3
            pkgs.dconf
            pkgs.adwaita-icon-theme
            pkgs.gsettings-desktop-schemas
          ];
        packages.leksah.components.exes.leksah.postInstall =
          lib.optionalString (!isWindows) ''
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
        packages.leksah.components.exes.leksah-warp.build-tools =
          lib.optionals (!isWindows) [
            pkgs.makeWrapper
          ];
        packages.leksah.components.exes.leksah-warp.postInstall =
          lib.optionalString (!isWindows) ''
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
        # The GTK4/WebKitGTK 6.0 front end (Linux only).  WebKit needs the
        # gsettings schemas at runtime, so unlike the gtk3 wrappers this one
        # extends XDG_DATA_DIRS rather than clearing it (wrapGAppsHook4's
        # setup hook doesn't survive the component builder's phase order, so
        # the wrapper sets the env explicitly).
        packages.leksah.components.exes.leksah-webkitgtk.build-tools =
          lib.optionals pkgs.stdenv.hostPlatform.isLinux [
            pkgs.makeWrapper
          ];
        packages.leksah.components.exes.leksah-webkitgtk.libs =
          lib.optionals pkgs.stdenv.hostPlatform.isLinux [
            pkgs.gtk4
            pkgs.webkitgtk_6_0
            pkgs.dconf
            pkgs.adwaita-icon-theme
            pkgs.gsettings-desktop-schemas
          ];
        packages.leksah.components.exes.leksah-webkitgtk.postInstall =
          lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
          mkdir -p $out/share
          cp -r ${../linux} $out/share/
          wrapProgram $out/bin/leksah-webkitgtk \
            --prefix 'PATH' ':' "${hsPkgs.leksah-server.components.exes.leksah-server}/bin" \
            --prefix 'PATH' ':' "${pkgs.haskell-nix.tool config.compiler.nix-name "cabal" "latest"}/bin" \
            --suffix 'PATH' ':' "${pkgs.haskell-nix.compiler.${config.compiler.nix-name}}/bin" \
            --suffix 'PATH' ':' "${hsPkgs.doctest.components.exes.doctest}/bin" \
            --prefix 'XDG_DATA_DIRS' ':' "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}" \
            --prefix 'XDG_DATA_DIRS' ':' "${pkgs.gtk4}/share/gsettings-schemas/${pkgs.gtk4.name}" \
            --prefix 'XDG_DATA_DIRS' ':' "${pkgs.adwaita-icon-theme}/share"
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
