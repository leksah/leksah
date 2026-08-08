{
  # Standalone flake for leksah-classic, the frozen GPLv2 GTK3 IDE.  It is
  # deliberately independent of the flake in the parent directory (which builds
  # the Apache-2.0 web-UI leksah): this directory is meant to be liftable into
  # its own repository unchanged.
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/hkm/stable-haskell";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  # Patched haddock-api (Haddock.Types exposed for leksah-server's
  # IDE.Metainfo.SourceCollectorH), per GHC.  9.6-9.10 are the upstream release
  # branches + the patch; 9.12/9.14 are the GHC in-tree haddock-api (no
  # standalone upstream branch past 9.10) + the patch.  See leksah/haddock.
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
  # leksah-server, ltk and the Haskell VCS libs (vcswrapper/vcsgui) are
  # source-repository-packages in cabal.project.  Wire each to a flake input so
  # the haskell.nix planner resolves them without a network fetch (pure eval).
  inputs.leksah-server-src.url = "github:leksah/leksah-server/cde8bb8db19dad008fe8a5cd7c1628b4eba14fdd";
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
        "aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = "aarch64-darwin";
                inputMap = {
                  "https://github.com/leksah/haddock/ghc-9.6" = inputs.haddock-ghc96;
                  "https://github.com/leksah/haddock/ghc-9.8" = inputs.haddock-ghc98;
                  "https://github.com/leksah/haddock/ghc-9.10" = inputs.haddock-ghc910;
                  "https://github.com/leksah/haddock/ghc-9.12" = inputs.haddock-ghc912;
                  "https://github.com/leksah/haddock/ghc-9.14" = inputs.haddock-ghc914;
                  "https://github.com/leksah/leksah-server/cde8bb8db19dad008fe8a5cd7c1628b4eba14fdd" = inputs.leksah-server-src;
                  "https://github.com/leksah/ltk/cea1aedf86f1223c6fc2f1a7a9a69cc8bf94603f" = inputs.ltk-src;
                  "https://github.com/leksah/haskellVCSWrapper/b77a455d4250223a6bde047aa0901df72dfb9c7f" = inputs.haskellvcswrapper-src;
                  "https://github.com/leksah/haskellVCSGUI/fbdd7bfaefb49b35a956b79e2958a826e6e86f66" = inputs.haskellvcsgui-src;
                };
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // {
        legacyPackages = pkgs;
        packages = flake.packages // {
          default = flake.packages."leksah-classic:exe:leksah-classic";
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
