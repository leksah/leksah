{ compiler-nix-name ? "ghc8107"
}:
(import ./. { inherit compiler-nix-name; }).shell
