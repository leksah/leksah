{ sourcesOverride ? {}
, compiler-nix-name ? "ghc884"
}:
(import ./. { inherit sourcesOverride compiler-nix-name; }).shells.ghc
