{ sourcesOverride ? {}
, haskellCompiler ? "ghc883" }:
(import ./. { inherit sourcesOverride haskellCompiler; }).shells.ghc
    
