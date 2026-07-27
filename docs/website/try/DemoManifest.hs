-- | The demo's file manifest, shared by the try/ generators.
--
-- 'sourceFiles' maps real repo paths (relative to the repo root) to the demo
-- mock-FS paths they are packed at (window.leksahDemoFiles, read by
-- IDE.Web.FS).  gen-demo-files.hs packs them; gen-demo-hovers.hs keys its
-- hover map by the DEMO paths (that is the path both the editor and — after
-- LeksahTermLinks resolves an Update(...) header against the project file
-- set — the terminal sends with a hover request), while querying
-- haskell-language-server over the REAL files.
--
-- src/IDE/Web/Instance.hs is packed (under the breakout tree, the demo's one
-- project) because the demo's Claude terminal window shows Update() diff
-- blocks against it — packing it makes those blocks link/hover targets.
module DemoManifest (sourceFiles, hoverRoots) where

-- | (real repo path, demo mock-FS path), in pack order.
sourceFiles :: [(FilePath, FilePath)]
sourceFiles =
  [ ("sandpit/cabal.project",             "/demo/cabal.project")
  , ("sandpit/breakout/breakout.cabal",   "/demo/breakout/breakout.cabal")
  , ("sandpit/breakout/app/Main.hs",      "/demo/breakout/app/Main.hs")
  , ("sandpit/breakout/LICENSE",          "/demo/breakout/LICENSE")
  , ("src/IDE/Web/Instance.hs",           "/demo/breakout/src/IDE/Web/Instance.hs")
  ]

-- | Haskell sources swept for editor hovers, and the project root (relative
-- to the repo root) whose haskell-language-server serves each.  "." is the
-- leksah repo itself (hie.yaml pins src/ to lib:leksah-nogtk).
hoverRoots :: [(FilePath, FilePath)]
hoverRoots =
  [ ("src/IDE/Web/Instance.hs",      ".")
  , ("sandpit/breakout/app/Main.hs", "sandpit")
  ]
