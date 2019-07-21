{ haskellCompiler ? "ghc865"
, config ? {}
, system ? builtins.currentSystem
, crossSystem ? null
, ... }@args:
let
  localLib = import ./nix/lib.nix { inherit haskellCompiler config system crossSystem; };

  filteredArgs = localLib.pkgs.lib.filterAttrs (n: v: n != "haskellCompiler") args;
  default-nix = localLib.nix-tools.default-nix ./nix/default.nix filteredArgs;
  inherit (default-nix) nix-tools;
  inherit (nix-tools._raw) plan-nix pkgs hsPkgs;

  cabalSystem = builtins.replaceStrings ["-darwin"] ["-osx"] pkgs.stdenv.system;
  shells = {
    ghc = (hsPkgs.shellFor {
      packages = ps: with ps; [
        leksah-server
        leksah
        ltk ];
    }).overrideAttrs (oldAttrs: {
      shellHook = (oldAttrs.shellHook or "") + ''
        unset CABAL_CONFIG
      '';
    });
  };
in
  default-nix // {
    inherit plan-nix shells;
    inherit (nix-tools._raw) launch-leksah wrapped-leksah;
  }
