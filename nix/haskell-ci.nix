# haskell-ci (https://github.com/haskell-CI/haskell-ci), which generates
# .github/workflows/haskell-ci.yml from cabal.project and the packages'
# `tested-with:` fields.  Built from its GitHub master with haskell.nix: the
# newest release on Hackage (0.18.1, Feb 2024) predates GHC 9.14 and refuses a
# `tested-with: ghc ==9.14.1`.  The source is the `haskell-ci-src` flake input.
#
#   nix run .#haskell-ci -- regenerate     # rewrite the workflow in place
{ pkgs, src }:
let
  project = pkgs.haskell-nix.cabalProject' {
    inherit src;
    name = "haskell-ci";
    # GHC 9.14.1 — haskell-ci's own tested-with covers it.
    compiler-nix-name = "ghc9141";
    # haskell-ci's cabal.project builds itself with -Werror; a newer compiler
    # than its CI used may warn where it didn't, and a warning should not stop
    # us from using the tool.
    cabalProjectLocal = ''
      package haskell-ci
        ghc-options: -Wwarn
      tests: False
    '';
  };
in project.hsPkgs.haskell-ci.components.exes.haskell-ci
