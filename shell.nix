{ haskellCompiler ? "ghc883" }:
(import ./. { inherit haskellCompiler; }).shells.ghc
    
