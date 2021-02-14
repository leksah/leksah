{ compiler-nix-name ? "ghc8104"
}:
(import ./. { inherit compiler-nix-name; }).shells.ghc
