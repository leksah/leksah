{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
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
                # Wire the patched haddock-api branches (leksah/haddock) so the
                # haskell.nix planner resolves the source-repository-packages in
                # cabal.project without a network fetch (pure eval).
                inputMap = {
                  "https://github.com/leksah/haddock/ghc-9.6"  = inputs.haddock-ghc96;
                  "https://github.com/leksah/haddock/ghc-9.8"  = inputs.haddock-ghc98;
                  "https://github.com/leksah/haddock/ghc-9.10" = inputs.haddock-ghc910;
                  "https://github.com/leksah/haddock/ghc-9.12" = inputs.haddock-ghc912;
                  "https://github.com/leksah/haddock/ghc-9.14" = inputs.haddock-ghc914;
                };
              };
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
      in flake // {
        legacyPackages = pkgs;
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
