{ compiler-nix-name ? "ghc8102"
}:
(import ./. { inherit compiler-nix-name; }).shells.ghc
