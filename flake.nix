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
  # Patched haddock-api (Haddock.Types exposed for leksah-server's
  # IDE.Metainfo.SourceCollectorH), per GHC.  9.6-9.10 are the upstream
  # release branches + the patch; 9.12/9.14 are the GHC in-tree haddock-api
  # (no standalone upstream branch past 9.10) + the patch.  See leksah/haddock.
  inputs.haddock-ghc96.url = "github:leksah/haddock/ghc-9.6";
  inputs.haddock-ghc96.flake = false;
  inputs.haddock-ghc98.url = "github:leksah/haddock/ghc-9.8";
  inputs.haddock-ghc98.flake = false;
  inputs.haddock-ghc910.url = "github:leksah/haddock/ghc-9.10";
  inputs.haddock-ghc910.flake = false;
  inputs.haddock-ghc912.url = "github:leksah/haddock/ghc-9.12";
  inputs.haddock-ghc912.flake = false;
  inputs.haddock-ghc914.url = "github:leksah/haddock/ghc-9.14";
  inputs.haddock-ghc914.flake = false;
  # jsaddle (core), jsaddle-wkwebview, jsaddle-terminal, jsaddle-webkitgtk and
  # jsaddle-webview2 live in the jsaddle monorepo; wire it so the haskell.nix
  # planner resolves the source-repository-package in cabal.project without a
  # network fetch (pure eval).
  inputs.jsaddle-terminal-src.url = "github:ghcjs/jsaddle/8ea768493731d3b3483e303e8592d3fc8163c9b0";
  inputs.jsaddle-terminal-src.flake = false;
  # ffcabal lives in its own repo now; same wiring as above.
  inputs.ffcabal-src.url = "github:leksah/ffcabal/03d5d8f99b41db4ea8af74712354ff334112ae5c";
  inputs.ffcabal-src.flake = false;
  # HLS built from its master branch: its cabal.project uses allow-newer to
  # support GHC 9.14, which no hackage-released HLS does yet (hie-compat caps
  # base < 4.22).  Consumed as a tool `src` in nix/hix.nix.
  inputs.hls-github.url = "github:haskell/haskell-language-server";
  inputs.hls-github.flake = false;
  # leksah-server, ltk and the Haskell VCS libs (vcswrapper/vcsgui) were git
  # submodules under vendor/; they are now source-repository-packages in
  # cabal.project.  Wire each to a flake input (same as ffcabal/jsaddle) so the
  # haskell.nix planner resolves them without a network fetch (pure eval) and a
  # plain `nix develop .#` works — no ?submodules=1 needed.
  inputs.leksah-server-src.url = "github:leksah/leksah-server/0b24e1705d3caef0bf7b7552724cf514554d8a13";
  inputs.leksah-server-src.flake = false;
  inputs.ltk-src.url = "github:leksah/ltk/cea1aedf86f1223c6fc2f1a7a9a69cc8bf94603f";
  inputs.ltk-src.flake = false;
  inputs.haskellvcswrapper-src.url = "github:leksah/haskellVCSWrapper/b77a455d4250223a6bde047aa0901df72dfb9c7f";
  inputs.haskellvcswrapper-src.flake = false;
  inputs.haskellvcsgui-src.url = "github:leksah/haskellVCSGUI/fbdd7bfaefb49b35a956b79e2958a826e6e86f66";
  inputs.haskellvcsgui-src.flake = false;
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
                # Wire the patched haddock-api branches (leksah/haddock) so the
                # haskell.nix planner resolves the source-repository-packages in
                # cabal.project without a network fetch (pure eval).
                inputMap = {
                  "https://github.com/leksah/haddock/ghc-9.6"  = inputs.haddock-ghc96;
                  "https://github.com/leksah/haddock/ghc-9.8"  = inputs.haddock-ghc98;
                  "https://github.com/leksah/haddock/ghc-9.10" = inputs.haddock-ghc910;
                  "https://github.com/leksah/haddock/ghc-9.12" = inputs.haddock-ghc912;
                  "https://github.com/leksah/haddock/ghc-9.14" = inputs.haddock-ghc914;
                  "https://github.com/ghcjs/jsaddle/8ea768493731d3b3483e303e8592d3fc8163c9b0" = inputs.jsaddle-terminal-src;
                  "https://github.com/leksah/ffcabal/03d5d8f99b41db4ea8af74712354ff334112ae5c" = inputs.ffcabal-src;
                  "https://github.com/leksah/leksah-server/0b24e1705d3caef0bf7b7552724cf514554d8a13" = inputs.leksah-server-src;
                  "https://github.com/leksah/ltk/cea1aedf86f1223c6fc2f1a7a9a69cc8bf94603f" = inputs.ltk-src;
                  "https://github.com/leksah/haskellVCSWrapper/b77a455d4250223a6bde047aa0901df72dfb9c7f" = inputs.haskellvcswrapper-src;
                  "https://github.com/leksah/haskellVCSGUI/fbdd7bfaefb49b35a956b79e2958a826e6e86f66" = inputs.haskellvcsgui-src;
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
        launch-leksah-script = pkgs.writeShellScriptBin "launch-leksah" ''
          "$@"
        '';
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
          // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          leksah-windows-installer = import ./nix/windows-installer.nix {
            inherit pkgs;
            leksah = flake.packages."x86_64-w64-mingw32:leksah:exe:leksah";
            leksah-server = flake.packages."x86_64-w64-mingw32:leksah-server:exe:leksah-server";
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
              leksah-server = flake.packages."leksah-server:exe:leksah-server";
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
        apps = flake.apps // {
          launch-leksah.type = "app";
          launch-leksah.program = (pkgs.stdenv.mkDerivation {
            name = "launch-leksah";
            nativeBuildInputs = with pkgs; [ wrapGAppsHook makeWrapper ];
            buildInputs = with pkgs; [
              gtk3
              dconf
              gnome3.adwaita-icon-theme
              gsettings-desktop-schemas
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
                --suffix 'PATH' ':' "${pkgs.hixProject.hsPkgs.doctest.components.exes.doctest}/bin" \
                --suffix 'LD_LIBRARY_PATH' ':' "${pkgs.cairo}/lib" \
                --suffix 'FONTCONFIG_PATH' ':' "${pkgs.fontconfig.out}/etc/fonts" \
                --set 'XDG_DATA_DIRS' ""
              '';
          }) + "/bin/launch-leksah";
        }
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
