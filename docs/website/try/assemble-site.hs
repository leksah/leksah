{-# LANGUAGE LambdaCase #-}
-- | Assemble the complete leksah.org site root — the Haskell replacement for
-- copy-assets.sh's orchestration, written to run inside a nix derivation
-- (nix\/website.nix compiles it with the project's native ghc and runs it
-- with the compiled JS apps as inputs; boot libraries only, like the rest of
-- docs\/website\/try).
--
--   assemble-site --repo REPO --leksah-js FILE --breakout-js FILE --out DIR
--
-- Everything is copied by an explicit whitelist, so working files
-- (the .hs sources, terminals\/*.ans, .gitignore) never leak
-- onto the public site.  Files are copied by read+write (never copyFile):
-- the inputs live in the read-only nix store, and a byte copy gives the new
-- file normal writable permissions — which the in-place PatchRts step needs.
module Main (main) where

import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, isSuffixOf, sort)
import System.Directory
       (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
        listDirectory)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), takeDirectory)

import qualified GenDemoFiles
import qualified GenDemoTerminals
import qualified GenXtermCss
import qualified PatchRts

data Opts = Opts
  { optRepo       :: FilePath
  , optLeksahJs   :: FilePath
  , optBreakoutJs :: FilePath
  , optOut        :: FilePath
  }

parseOpts :: [String] -> Maybe Opts
parseOpts = go (Opts "" "" "" "")
  where
    go o = \case
      []                        -> if any null [optRepo o, optLeksahJs o,
                                                optBreakoutJs o, optOut o]
                                   then Nothing else Just o
      ("--repo"        : v : r) -> go o { optRepo = v } r
      ("--leksah-js"   : v : r) -> go o { optLeksahJs = v } r
      ("--breakout-js" : v : r) -> go o { optBreakoutJs = v } r
      ("--out"         : v : r) -> go o { optOut = v } r
      _                         -> Nothing

-- Byte copy (see the module header for why not copyFile), creating the
-- destination's directory.
cp :: FilePath -> FilePath -> IO ()
cp src dst = do
  ok <- doesFileExist src
  unless ok $ die ("assemble-site: missing " ++ src)
  createDirectoryIfMissing True (takeDirectory dst)
  BS.readFile src >>= BS.writeFile dst

-- Copy a directory tree, optionally filtering files by name.
cpTree :: (FilePath -> Bool) -> FilePath -> FilePath -> IO ()
cpTree keep src dst = do
  ok <- doesDirectoryExist src
  unless ok $ die ("assemble-site: missing directory " ++ src)
  entries <- sort <$> listDirectory src
  forM_ entries $ \e -> do
    let s = src </> e
        d = dst </> e
    isDir <- doesDirectoryExist s
    if isDir then cpTree keep s d
             else when (keep e) (cp s d)

main :: IO ()
main = do
  opts <- getArgs >>= \args -> case parseOpts args of
    Just o  -> return o
    Nothing -> die ("usage: assemble-site --repo REPO --leksah-js FILE"
                    ++ " --breakout-js FILE --out DIR")
  let repo = optRepo opts
      out  = optOut opts
      site = repo </> "docs/website"
      try' = site </> "try"

  -- 1. The hand-written site files, whitelisted.
  cp (site </> "index.html") (out </> "index.html")
  -- The screencast placeholders (raw captures stay out of the site).
  cpTree (\f -> not (".raw.webm" `isSuffixOf` f) && not ("." `isPrefixOf` f))
         (site </> "media") (out </> "media")
  forM_ [ "index.html", "index-split.html", "shims.js", "autotest.js"
        , "demo-agents.js" ] $ \f ->
    cp (try' </> f) (out </> "try" </> f)
  -- Committed, NOT regenerated here: its generator needs a live
  -- haskell-language-server over the real repo (gen-demo-hovers.hs).
  cp (try' </> "demo-hovers.js") (out </> "try/demo-hovers.js")
  cp (try' </> "breakout/index.html") (out </> "try/breakout/index.html")

  -- 2. The shared bundles at the site root (root-absolute /cm6, /xterm,
  -- /pics, /fonts — the app's generated CSS references them absolutely).
  cp (repo </> "leksah/cm6/leksah-cm6.js") (out </> "cm6/leksah-cm6.js")
  xterms <- filter (\f -> f == "xterm.css" || f == "xterm.js"
                          || ("addon-" `isPrefixOf` f && ".js" `isSuffixOf` f))
              <$> listDirectory (repo </> "leksah/xterm")
  forM_ xterms $ \f -> cp (repo </> "leksah/xterm" </> f) (out </> "xterm" </> f)
  cpTree (const True) (repo </> "leksah/pics")  (out </> "pics")
  cpTree (const True) (repo </> "leksah/fonts") (out </> "fonts")

  -- 3. The generated page seeds.
  GenXtermCss.run (repo </> "leksah/xterm/xterm.css") (out </> "try/demo-xterm-css.js")
  GenDemoFiles.run repo (out </> "try/demo-files.js")
  GenDemoTerminals.run (try' </> "terminals") (out </> "try/demo-terminals.js")

  -- 4. The compiled apps, patched in place (see PatchRts).  leksah.js links
  -- the full RTS + emscripten glue, so every pattern must match; breakout is
  -- a small program that links fewer shims, so absent patterns are logged,
  -- not fatal.
  cp (optLeksahJs opts) (out </> "try/leksah.js")
  PatchRts.run PatchRts.Strict (out </> "try/leksah.js")
  cp (optBreakoutJs opts) (out </> "try/breakout/breakout.js")
  PatchRts.run PatchRts.Lenient (out </> "try/breakout/breakout.js")

  putStrLn ("assemble-site: site root written to " ++ out)
