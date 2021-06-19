{ compiler-nix-name ? "ghc8105"
}:
(import ./. { inherit compiler-nix-name; }).shells.ghc
