-- | Thin runghc wrapper around "PatchRts" for the manual dev loop (the
-- rewrites themselves — env via globalThis.leksahDemoEnv, catchable FS
-- errnos, bounds-tolerant static DataViews, emscripten HEAP8/HEAPU8
-- exports — are documented at the module).
--
-- Usage: runghc patch-rts.hs [leksah.js]   (idempotent, strict: every
-- pattern must match — the right mode for the full leksah.js; the site
-- derivation patches breakout.js with PatchRts.Lenient instead, since a
-- small program links fewer RTS shims).
module Main (main) where

import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)

import qualified PatchRts

main :: IO ()
main = do
  args <- getArgs
  PatchRts.run PatchRts.Strict (fromMaybe "leksah.js" (listToMaybe args))
