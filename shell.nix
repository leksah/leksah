{ sourcesOverride ? {}
, compiler-nix-name ? "ghc8101"
}:
(import ./. { inherit sourcesOverride compiler-nix-name; }).shells.ghc
