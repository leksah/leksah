{ pkgs, config, ... }:
(
let
  sources = import ./sources.nix {};
  optionalAttrs = b: a: if b then a else {};
  system = null;
  isGhc914sh = config.compiler-nix-name == "ghc914-sh";
  # GHC 9.14 (stable-haskell fork): C-only "clib" packages have no Haskell
  # deps, so no rts unit lands in their per-component package DB and GHC's
  # mkUnitState check panics ("The RTS for rts:nonthreaded-nodebug is
  # missing").  The fork's -no-rts flag bypasses the check — the same
  # treatment the compiler's own libffi-clib gets during its bootstrap.
  # This must live in the cabal.project text (not nix-module ghcOptions):
  # package ghc-options enter cabal's UnitId hash, so the v2 slice
  # builder's plan-time and build-time views have to agree on them.
  clibNoRts = pkgs.lib.optionalString isGhc914sh ''
    package libyaml-clib
      ghc-options: -no-rts
  '';
  # Hackage packages whose released code doesn't compile against the
  # stable-haskell fork's Cabal 3.17 / ghc-9.14 API.  The fix is applied
  # to the hackage source here and the PATCHED source is handed to the
  # cabal solver as a local `packages:` entry (cabalProjectLocal below),
  # so the plan and the build see the same code — patching behind the
  # solver's back would leave plan-nix computed from the unpatched index.
  patchedHackage = name: version: patch: pkgs.applyPatches {
    name = "${name}-${version}-patched";
    src = pkgs.haskell-nix.hackageTarball { inherit name version; };
    patches = [ patch ];
  };
  # cabal-doctest: custom-setup dep of xml-conduit and haskell-gi; the
  # fork's Cabal 3.17 split Verbosity into VerbosityFlags + handles.
  # Its tarball .cabal caps Cabal <3.16, hence the allow-newer (the
  # patched code is CPP-guarded for both APIs).
  cabalDoctestPatched = pkgs.lib.optionalString isGhc914sh ''
    packages: ${patchedHackage "cabal-doctest" "1.0.12" ./patches/cabal-doctest-cabal-3.17.patch}
    allow-newer: cabal-doctest:Cabal
  '';
  # cabal-add (hls-cabal-plugin dep): the fork's Cabal-syntax 3.17
  # runParseResult yields PErrorWithSource, not PError.
  # ghc-exactprint 1.14 targets mainline ghc-9.14's AST; the fork moved
  # the INLINE/RULES phase SourceText from the Activation constructors
  # into ActivationAnn's new aa_phase field.
  hlsDepsPatched = pkgs.lib.optionalString isGhc914sh ''
    packages: ${patchedHackage "cabal-add" "0.2" ./patches/cabal-add-cabal-syntax-3.17.patch}
    packages: ${patchedHackage "ghc-exactprint" "1.14.0.0" ./patches/ghc-exactprint-1.14-stable-ghc-9.14.patch}
  '';
  # HLS's own hls-cabal-plugin doesn't compile against the fork's Cabal-syntax
  # 3.17: CondTree lost its middle type param, and runParseResult now wraps
  # warnings/errors in *WithSource.  Patch the git source and hand the patched
  # tree to the tool as its `src`, so the solver plans against the code the
  # build compiles (same rule as the hackage deps above; the patch is
  # CPP-guarded on Cabal_syntax >= 3.17, so it's a no-op for other compilers).
  hlsSrc =
    if isGhc914sh
    then pkgs.applyPatches {
      name = "haskell-language-server-src-patched";
      src = pkgs.hls-github-src;
      patches = [ ./patches/hls-cabal-plugin-cabal-syntax-3.17.patch ];
    }
    else pkgs.hls-github-src;
in
rec {
    projectFileName = "cabal.project";
    cabalProjectLocal = clibNoRts + cabalDoctestPatched;
    # ghc914-sh: the stable-haskell GHC 9.14 (haskell.nix -hl branch) that can
    # cross-compile from darwin to Linux (musl) via hyper-linux.
    compiler-nix-name = "ghc914-sh";
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
      "ghc914".compiler-nix-name = pkgs.lib.mkForce "ghc914-sh";
    };
    name = "leksah";
    # Cross targets exposed as flake packages (NOT pulled into the dev shell —
    # see `shell.crossPlatforms` below, which forces it empty so the native dev
    # loop never builds a cross GHC):
    #   * x86_64-linux host: ucrt64 (Windows leksah-webview2; TH via wine/iserv).
    #   * aarch64-darwin host: aarch64-linux-musl (verified darwin→linux target
    #     of the -hl branch; TH/tests run under hyper-linux `hl`) AND ucrt64, so
    #     the `leksah-linux` / `leksah-windows` apps have something to run.
    crossPlatforms = p:
      pkgs.lib.optionals (pkgs.stdenv.hostPlatform.system == "x86_64-linux")
        [ p.ucrt64 ]
      ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin
        [ p.aarch64-multiplatform-musl p.ucrt64 ];
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
      # Keep the interactive dev shell NATIVE: the top-level `crossPlatforms`
      # feeds the flake's cross packages, but if the shell inherited it too,
      # `nix develop` (the leksah-nix.sh dev loop) would build a cross GHC just
      # to enter the shell.  Cross builds go through `nix build`/`nix run`.
      crossPlatforms = _: [];
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
        haskell-language-server = {
          src = hlsSrc;
          # HLS's hls-cabal-plugin pulls cabal-add, which caps Cabal-syntax <3.17
          # and can't solve against GHC 9.14's boot Cabal-syntax 3.17.  Relax that
          # bound so the plan resolves (cabal-add still targets the 3.17 API).
          cabalProjectLocal =
            "allow-newer: cabal-add:Cabal-syntax, cabal-add:Cabal\n"
            + clibNoRts + hlsDepsPatched;
        };
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
