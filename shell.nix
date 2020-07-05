{ sourcesOverride ? {}
, compiler-nix-name ? "ghc8101"
, ...}@args:
(import ./. args).shells.ghc
