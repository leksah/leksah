-- | Thin runghc wrapper around "GenDemoTerminals" for the manual dev loop.
--
-- Run from docs/website/try:  runghc gen-demo-terminals.hs
-- The site derivation calls GenDemoTerminals.run directly (assemble-site.hs).
module Main (main) where

import qualified GenDemoTerminals

main :: IO ()
main = GenDemoTerminals.run "terminals" "demo-terminals.js"
