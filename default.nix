{ haskellCompiler ? "ghc865"
, config ? {}
, system ? builtins.currentSystem
, crossSystem ? null
, ... }@args:
let
  localLib = import ./nix/lib.nix { inherit haskellCompiler config system crossSystem; };

  filteredArgs = localLib.pkgs.lib.filterAttrs (n: v: n != "haskellCompiler") args;
  default-nix = localLib.nix-tools.default-nix ./nix/default.nix filteredArgs;
  inherit ((localLib.nix-tools.default-nix ./nix/plan-only.nix filteredArgs).nix-tools._raw) plan-nix;

  shell-nix-tools = (localLib.nix-tools.default-nix ./nix/shell-only.nix filteredArgs).nix-tools;
  inherit (shell-nix-tools._raw) pkgs hsPkgs;
  cabalSystem = builtins.replaceStrings ["-darwin"] ["-osx"] pkgs.stdenv.system;
  shells = {
    ghc = (hsPkgs.shellFor {
      packages = ps: with ps; [
        leksah
        ltk ];
    }).overrideAttrs (oldAttrs: {
      shellHook = (oldAttrs.shellHook or "") + ''
        unset CABAL_CONFIG
      '';
    });
  };
in
  default-nix //
    { inherit plan-nix shells; }
