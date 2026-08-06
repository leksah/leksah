-- | Thin runghc wrapper around "GenXtermCss" for the manual dev loop.
--
-- Called by hand from docs/website/try:
--   runghc gen-xterm-css.hs [../xterm/xterm.css]
-- The site derivation calls GenXtermCss.run directly (assemble-site.hs).
module Main (main) where

import System.Environment (getArgs)

import qualified GenXtermCss

main :: IO ()
main = do
  args <- getArgs
  let cssPath = case args of { (p:_) -> p; [] -> "../xterm/xterm.css" }
  GenXtermCss.run cssPath "demo-xterm-css.js"
