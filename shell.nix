# To use this run `nix-shell`.  In the shell run `cabal new-build asterius`
# to build the asterius executables.  Check that the resulting binaries
# are in the PATH with `ahc-pkg list --global`.
{ haskellCompiler ? "ghc865" }:
let
  nixpkgs = import <nixpkgs> {};

  nix-tools = (import ./. { inherit haskellCompiler; }).nix-tools;
  hsPkgs = nix-tools._raw._config.hsPkgs;
  cabalSystem = builtins.replaceStrings ["-darwin"] ["-osx"] nixpkgs.stdenv.system;
in (hsPkgs.shellFor {
    packages = ps: with ps; [ leksah ltk ];
  }).overrideAttrs (oldAttrs: {
    shellHook = (oldAttrs.shellHook or "") + ''
      unset CABAL_CONFIG
    '';
  })
