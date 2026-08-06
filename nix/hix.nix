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
  # hslogger hard-depends on network (for its syslog/growl handlers), and
  # network does not build for the GHC JavaScript backend.  For the JS cross
  # (projectCross re-evaluates this module with the cross pkgs, so
  # hostPlatform is the ghcjs platform here) hand the solver a patched
  # hslogger whose network handlers/deps sit behind `if !os(ghcjs)` —
  # System.Log.Logger et al still build, so the pervasive debugM logging
  # works (console) in the browser.
  hsloggerNoNetworkJs = pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isGhcjs ''
    packages: ${patchedHackage "hslogger" "1.3.2.0" ./patches/hslogger-no-network-js.patch}
  '';
  # reflex-dom-core hard-selects its jsffi-flavored `src-ghcjs` module on
  # arch(javascript), which only typechecks against ghcjs-dom's jsffi
  # flavor.  Leksah's web UI is written against the JSADDLE flavor
  # (cabal.project pins `ghcjs-dom -jsffi` for the JS build), so patch the
  # conditional to keep `src-ghcjs` for legacy GHCJS only and use the
  # jsaddle-flavored `src-ghc` module on the GHC JS backend (it is pure
  # jsaddle and compiles there; JSM = IO).  The JS-arch deps are kept.
  reflexDomCoreJsaddleJs = pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isGhcjs ''
    packages: ${patchedHackage "reflex-dom-core" "0.8.1.4" ./patches/reflex-dom-core-js-jsaddle.patch}
  '';
  # Flags for the ghci multi-repl (leksah.sh --ghci).  The ObjC glue is kept OUT
  # of the Haskell archives (`-objc-in-library`) and preloaded as dylibs instead:
  # GHCi's RTS linker loads ObjC .o's but never registers their classes with the
  # ObjC runtime — only dyld does — so archived ObjC would be dead in the repl.
  # `+no-hlint` drops the ghc-lib-parser subtractor the interpreter can't handle.
  # These flags enter cabal's UnitId hash (the reason ghci uses its own builddir,
  # dist-ghci-<ver>), so the store must be seeded from a plan built with the SAME
  # flags — that is what the `ghci914` flake variant below is for.  NB: these are
  # the exact `--constraint`s leksah.sh --ghci passes to `cabal repl`; keep the
  # two in sync.
  ghciModeFlags = ''
    constraints: leksah -objc-in-library
    constraints: leksah +no-hlint
    constraints: jsaddle-wkwebview -objc-in-library
  '';
  # cabal-add (hls-cabal-plugin dep): the fork's Cabal-syntax 3.17
  # runParseResult yields PErrorWithSource, not PError.
  # ghc-exactprint 1.14 targets mainline ghc-9.14's AST; the fork moved
  # the INLINE/RULES phase SourceText from the Activation constructors
  # into ActivationAnn's new aa_phase field.
  # cabal-install-parsers (cabal-add dep): the fork's ParseResult gained a
  # source type parameter (`ParseResult src a`) alongside the same
  # *WithSource error/warning wrappers.
  hlsDepsPatched = pkgs.lib.optionalString isGhc914sh ''
    packages: ${patchedHackage "cabal-add" "0.2" ./patches/cabal-add-cabal-syntax-3.17.patch}
    packages: ${patchedHackage "cabal-install-parsers" "0.6.3" ./patches/cabal-install-parsers-cabal-syntax-3.17.patch}
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
      patches = [
        ./patches/hls-cabal-plugin-cabal-syntax-3.17.patch
        # ghc914-sh reports the project GHC as "9.14" but HLS is compiled
        # against its library (cProjectVersion "9.14.0"); relax ghcide's exact
        # version-equality guard so it loads ghc914-sh projects.
        ./patches/ghcide-relax-ghc-version-check.patch
      ];
    }
    else pkgs.hls-github-src;
in
rec {
    projectFileName = "cabal.project";
    cabalProjectLocal = clibNoRts + cabalDoctestPatched + hsloggerNoNetworkJs
      + reflexDomCoreJsaddleJs;
    # ghc914-sh: the stable-haskell GHC 9.14 (haskell.nix hkm/stable-haskell
    # branch) that can cross-compile from darwin to Linux (musl) via hyper-linux.
    compiler-nix-name = "ghc914-sh";
    # v2 slice builds for the native platforms (what leksah's own incremental
    # builds use).  The mingw cross must use the classic builder: v2 compiles
    # custom Setup.hs (entropy, ghc-paths) with the cross GHC, producing a
    # setup.exe the Linux build host can't run; v1's setup-builder uses the
    # build compiler.  projectCross re-evaluates this module with the cross
    # pkgs, so the condition picks the right builder per platform.
    # mkForce: hkm/stable-haskell's cabal-project.nix now sets builderVersion
    # itself (=2), so a plain assignment here collides ("conflicting definition
    # values"); the override takes priority for both the native and cross evals.
    builderVersion = pkgs.lib.mkForce
      (if pkgs.stdenv.hostPlatform.isWindows then 1 else 2);
    # GHC-version variants disabled for now (takes too long to plan them all)
    # flake.variants = {
    #   "ghc96".compiler-nix-name = pkgs.lib.mkForce "ghc96";
    #   "ghc98".compiler-nix-name = pkgs.lib.mkForce "ghc98";
    #   "ghc910".compiler-nix-name = pkgs.lib.mkForce "ghc910";
    #   "ghc912".compiler-nix-name = pkgs.lib.mkForce "ghc912";
    #  "ghc914".compiler-nix-name = pkgs.lib.mkForce "ghc914-sh";
    # };
    # The ghci multi-repl's store-seeding variant: the base project plus the
    # ghci-mode cabal flags (see `ghciModeFlags`).  `cabalProjectLocal` has type
    # `lines`, so this concatenates onto the base — no mkForce needed.  Unlike
    # the GHC-version variants above it keeps compiler-nix-name, so it re-uses
    # the default plan for every dependency except the flag-flipped ones
    # (jsaddle-wkwebview mainly) and plans fast.  leksah.sh --ghci runs
    # `haskell-nix-cabal-store-sync` inside this variant's dev shell to populate
    # dist-ghci-<ver>/store with prebuilt slices, so the first `cabal repl` no
    # longer rebuilds every shared dependency from source.
    flake.variants.ghci914.cabalProjectLocal = ghciModeFlags;
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
        [ p.aarch64-multiplatform-musl p.ucrt64 ]
      ++ [ p.ghcjs ];
    modules = [({pkgs, lib, config, ...}: let
        inherit (config) hsPkgs;
        inherit (pkgs.stdenv.hostPlatform) isWindows;
        # The GHC JavaScript backend (javascript-unknown-ghcjs).  nixpkgs gives
        # this platform NO C compiler by design (pkgs/stdenv/cross/default.nix:
        # `targetPlatform.isGhcjs` → `cc = throw "no C compiler …"`), because the
        # JS backend's cbits/RTS are handled by emscripten *inside* the GHC
        # toolchain, not via stdenv.cc.  So the JS build must not reference any
        # native C library (gtk3, cairo, …) — doing so forces that throw.  Every
        # native-GUI module attr below is therefore gated `&& !isJS`.
        isJS = pkgs.stdenv.hostPlatform.isGhcjs;
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
        # The native web front end (exe:leksah — WKWebView on macOS,
        # GTK4/WebKitGTK 6.0 on Linux, WebView2 on Windows).  On Linux WebKit
        # needs the gsettings schemas at runtime, so the wrapper extends
        # XDG_DATA_DIRS rather than clearing it (wrapGAppsHook4's setup hook
        # doesn't survive the component builder's phase order, so the wrapper
        # sets the env explicitly).
        packages.leksah.components.exes.leksah.build-tools =
          lib.optionals (!isWindows && !isJS) [
            pkgs.makeWrapper
          ];
        packages.leksah.components.exes.leksah.libs =
          lib.optionals pkgs.stdenv.hostPlatform.isLinux [
            pkgs.gtk4
            pkgs.webkitgtk_6_0
            pkgs.dconf
            pkgs.adwaita-icon-theme
            pkgs.gsettings-desktop-schemas
          ];
        packages.leksah.components.exes.leksah.postInstall =
          # Ship the loader DLL next to the exe (it is LoadLibrary'd at startup).
          lib.optionalString isWindows ''
            cp ${webview2-sdk}/runtimes/win-x64/native/WebView2Loader.dll $out/bin/
          '' + lib.optionalString (!isWindows && !isJS) ''
          ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
            mkdir -p $out/share
            cp -r ${../linux} $out/share/
          ''}
          wrapProgram $out/bin/leksah \
            --prefix 'PATH' ':' "${pkgs.haskell-nix.tool config.compiler.nix-name "cabal" "latest"}/bin" \
            --suffix 'PATH' ':' "${pkgs.haskell-nix.compiler.${config.compiler.nix-name}}/bin" \
            ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
            --prefix 'XDG_DATA_DIRS' ':' "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}" \
            --prefix 'XDG_DATA_DIRS' ':' "${pkgs.gtk4}/share/gsettings-schemas/${pkgs.gtk4.name}" \
            --prefix 'XDG_DATA_DIRS' ':' "${pkgs.adwaita-icon-theme}/share" \
            ''} --argv0 leksah
        '';
        # The classic GTK3 IDE (its own frozen package, leksah-classic/).
        packages.leksah-classic.components.exes.leksah-classic.build-tools =
          lib.optionals (!isWindows && !isJS) [
            pkgs.wrapGAppsHook3
            pkgs.makeWrapper
          ];
        packages.leksah-classic.components.exes.leksah-classic.libs =
          lib.optionals (!isWindows && !isJS) [
            pkgs.gtk3
            pkgs.dconf
            pkgs.adwaita-icon-theme
            pkgs.gsettings-desktop-schemas
          ];
        packages.leksah-classic.components.exes.leksah-classic.postInstall =
          lib.optionalString (!isWindows && !isJS) ''
          ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
            mkdir -p $out/share
            cp -r ${../linux} $out/share/
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
        packages.leksah.components.exes.leksah-warp.build-tools =
          lib.optionals (!isWindows && !isJS) [
            pkgs.makeWrapper
          ];
        packages.leksah.components.exes.leksah-warp.postInstall =
          lib.optionalString (!isWindows && !isJS) ''
          ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
            mkdir -p $out/share
            cp -r ${../linux} $out/share/
          ''}
          wrapProgram $out/bin/leksah-warp \
            --prefix 'PATH' ':' "${pkgs.haskell-nix.tool config.compiler.nix-name "cabal" "latest"}/bin" \
            --suffix 'PATH' ':' "${pkgs.haskell-nix.compiler.${config.compiler.nix-name}}/bin" \
            --set 'XDG_DATA_DIRS' ""
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
        leksah-classic
        ltk
        vcsgui
        vcswrapper
      ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin gi-gtkosxapplication;
      tools = {
        # The stable-haskell cabal FORK (Cabal 3.17, distStoreDirLayout →
        # ~/.cabal/store/host/<platform>/package.conf.d) — the SAME cabal the
        # v2 slice builder runs.  Mainline cabal ("latest") reads the old
        # ghc-<ver>/package.db layout, so it can't see the v2 composed store
        # and `cabal build --dry-run` re-plans/rebuilds everything.  Passed as
        # a prebuilt derivation: shell-for-v2.nix puts it straight on PATH
        # (it can't be rebuilt via haskell-nix.tool under ghc914-sh, which has
        # no nixpkgs-prebuilt GHC — v2-cabal-install builds itself with a
        # nixpkgs ghc9141).
        cabal = pkgs.pkgsBuildBuild.haskell-nix.v2-cabal-install;
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
          # -dynamic: HLS's `dynamic` flag (default True) adds `-dynamic` to the
          # exe's ghc-options, but ghc914-sh is a static GHC and every slice is
          # built static-only (`shared: False`), so there are no dyn libs to
          # link — build the exe the static way like the rest of the project.
          cabalProjectLocal =
            "allow-newer: cabal-add:Cabal-syntax, cabal-add:Cabal\n"
            + "constraints: haskell-language-server -dynamic\n"
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
        # Nix language server (nix-community/nixd) for .nix files (IDE.LSP).
        pkgs.nixd
      ] ++ pkgs.lib.optional pkgs.stdenv.isDarwin pkgs.gtk-mac-integration;
    };
})
