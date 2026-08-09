{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/hkm/stable-haskell";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  # hyper-linux runs aarch64-linux / x86_64-linux ELF binaries on Apple Silicon
  # via Hypervisor.framework.  haskell.nix's darwin→linux cross (the -hl branch
  # above) reads it as the `hyper-linux` nixpkgs overlay attribute and runs its
  # `hl` in place of qemu for TH (iserv) and tests; the `leksah-linux` app below
  # uses the same `hl` to run the aarch64-linux-musl build on macOS.
  inputs.hyper-linux.url = "github:zw3rk/hyper-linux";
  inputs.flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  # jsaddle (core), jsaddle-wkwebview, jsaddle-terminal, jsaddle-webkitgtk and
  # jsaddle-webview2 live in the jsaddle monorepo; wire it so the haskell.nix
  # planner resolves the source-repository-package in cabal.project without a
  # network fetch (pure eval).
  inputs.jsaddle-terminal-src.url = "github:ghcjs/jsaddle/d9873936e47050361899414e4f640931779c3110";
  inputs.jsaddle-terminal-src.flake = false;
  # ffcabal lives in its own repo now; same wiring as above.
  inputs.ffcabal-src.url = "github:leksah/ffcabal/ad54e7188587423e60d34b76334b526f5e36deed";
  inputs.ffcabal-src.flake = false;
  # HLS built from its master branch: its cabal.project uses allow-newer to
  # support GHC 9.14, which no hackage-released HLS does yet (hie-compat caps
  # base < 4.22).  Consumed as a tool `src` in nix/hix.nix.
  inputs.hls-github.url = "github:haskell/haskell-language-server";
  inputs.hls-github.flake = false;
  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
#        "x86_64-darwin"
#        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            # Expose the HLS source tree so nix/hix.nix can use it as a tool `src`.
            hls-github-src = inputs.hls-github;
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = "aarch64-darwin";
                # Wire the source-repository-packages in cabal.project to flake
                # inputs so the haskell.nix planner resolves them without a
                # network fetch (pure eval).
                inputMap = {
                  "https://github.com/ghcjs/jsaddle/d9873936e47050361899414e4f640931779c3110" = inputs.jsaddle-terminal-src;
                  "https://github.com/leksah/ffcabal/ad54e7188587423e60d34b76334b526f5e36deed" = inputs.ffcabal-src;
                };
              };
          } // prev.lib.optionalAttrs (system == "aarch64-darwin") {
            # The `hl` runner the haskell.nix darwin→linux cross looks up as
            # `pkgsBuildBuild.hyper-linux` (there is no in-repo pin).  Only
            # aarch64-darwin has a hyper-linux package.
            "hyper-linux" = inputs.hyper-linux.packages.${system}.default;
          })
        ];
        pkgs = import nixpkgs { inherit system overlays;
          config = haskellNix.config // {
            allowUnfreePredicate = pkg: builtins.elem (pkg.pname or (builtins.parseDrvName pkg.name).name) [
              "vscode"
              "vscode-with-extensions"
              "vscode-extension-github-copilot"
              "vscode-extension-github-copilot-chat"
            ];
          };
        };
        flake = pkgs.hixProject.flake {};
        # Headless runtime smoke test for the GTK4/WebKitGTK 6.0 front end.
        extraChecks = pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
          leksah-webkitgtk-smoke = pkgs.callPackage ./nix/webkitgtk-smoke.nix {
            leksah-webkitgtk = flake.packages."leksah:exe:leksah";
            leksah-cmd = flake.packages."leksah:exe:leksah-cmd";
            leksah-src = ./.;
          };
        };
      in flake // {
        legacyPackages = pkgs;
        checks = flake.checks // extraChecks;
        hydraJobs = flake.hydraJobs // { checks = (flake.hydraJobs.checks or {}) // extraChecks; };
        # A novice-friendly Windows Setup.exe for the leksah-webview2 front end,
        # built entirely on the x86_64-linux cross builder (makensis runs there;
        # the webview2 exe is cross-compiled to mingw).  Only meaningful where
        # crossPlatforms yields ucrt64, i.e. x86_64-linux (see nix/hix.nix).
        packages = flake.packages
          // {
          # The leksah.org site root: homepage + the in-browser demo (the
          # ghcjs cross builds of leksah and the sandpit breakout game),
          # assembled by a Haskell program compiled inside the derivation
          # (see nix/website.nix).  Mirror the output to leksah.github.io.
          leksah-website = import ./nix/website.nix {
            inherit pkgs;
            src = ./.;
            leksah-js = flake.packages."javascript-unknown-ghcjs:leksah:exe:leksah";
            breakout-js = flake.packages."javascript-unknown-ghcjs:breakout:exe:breakout";
          };
        }
          // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          leksah-windows-installer = import ./nix/windows-installer.nix {
            inherit pkgs;
            leksah = flake.packages."x86_64-w64-mingw32:leksah:exe:leksah";
            src = ./.;
            version = "0.17.0.0";
          };
        }
          # A novice-friendly macOS artifact for the leksah-wkwebview front end:
          # a relocatable .app (dylib closure bundled + rebased, ad-hoc signed)
          # and a drag-to-Applications .dmg.  The .dmg step shells out to hdiutil,
          # so it builds only where nix has `sandbox = false` (see nix/macos-dmg.nix).
          // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin (
          let
            leksah-macos-app = import ./nix/macos-app.nix {
              inherit pkgs;
              leksah = flake.packages."leksah:exe:leksah";
              src = ./.;
              version = "0.17.0.0";
            };
          in {
            inherit leksah-macos-app;
            leksah-macos-dmg = import ./nix/macos-dmg.nix {
              inherit pkgs;
              app = leksah-macos-app;
              version = "0.17.0.0";
            };
          });
        apps = flake.apps
        # macOS runners for the cross-compiled builds: `hl` (hyper-linux) runs
        # the aarch64-linux-musl ELF directly on Apple Silicon, and wine runs the
        # Windows exe.  Both cross builds come from crossPlatforms in nix/hix.nix.
        // pkgs.lib.optionalAttrs (system == "aarch64-darwin") {
          # nix run .#leksah-linux — the aarch64-unknown-linux-musl leksah-warp
          # (the browser/warp front end: no native GTK, so it's the one that
          # runs headless under hl; connect at http://127.0.0.1:3367/).
          leksah-linux = {
            type = "app";
            program = (pkgs.writeShellScriptBin "leksah-linux" ''
              exec ${inputs.hyper-linux.packages.${system}.default}/bin/hl \
                ${flake.packages."aarch64-unknown-linux-musl:leksah:exe:leksah-warp"}/bin/leksah-warp "$@"
            '') + "/bin/leksah-linux";
          };
          # nix run .#leksah-windows — the x86_64-w64-mingw32 leksah-webview2 exe
          # under wine (WebView2Loader.dll sits next to the exe, so wine finds it).
          # On Apple Silicon wine needs Rosetta 2 for the x86_64 guest.
          leksah-windows = {
            type = "app";
            program = (pkgs.writeShellScriptBin "leksah-windows" ''
              exec ${pkgs.wine64}/bin/wine64 \
                ${flake.packages."x86_64-w64-mingw32:leksah:exe:leksah"}/bin/leksah.exe "$@"
            '') + "/bin/leksah-windows";
          };
        };
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
