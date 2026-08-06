-- | Thin runghc wrapper around "GenDemoFiles" for the manual dev loop.
--
-- Run from docs/website/try (needs runghc from the dev shell):
--   runghc gen-demo-files.hs
-- The site derivation calls GenDemoFiles.run directly (assemble-site.hs).
module Main (main) where

import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.Exit (die)

import qualified GenDemoFiles

main :: IO ()
main = do
  here <- doesFileExist "index.html"
  unless here $ die "gen-demo-files.hs: run from docs/website/try"
  GenDemoFiles.run "../../.." "demo-files.js"
