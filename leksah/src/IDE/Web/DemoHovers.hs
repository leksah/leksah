{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Precomputed hover tooltips for the in-browser web demo.
--
-- The hosting page (docs/website/try) defines @window.leksahDemoHovers@ —
-- @{ file: { line: [[colStart, colEnd, markdown], …] } }@ with 0-based
-- lines\/columns and files keyed by their demo (mock-FS) paths.  The content
-- is real @textDocument/hover@ output: gen-demo-hovers.py runs a live
-- haskell-language-server over the repo files the demo packs and captures
-- the response at every identifier span, so "IDE.LSP"'s ghcjs branch can be
-- a pure lookup here — the demo's stand-in for the language server.
--
-- Span keying (rather than identifier keying) matches how hovers arrive:
-- the editor sends whatever column the mouse is over inside a token, and
-- HLS content differs per occurrence anyway (local vs top-level, usage vs
-- definition).  Same page-seeded pattern as "IDE.Web.FS"; native stub is
-- inert.
module IDE.Web.DemoHovers
  ( demoHover
  ) where

import Data.Text (Text)

#if defined(ghcjs_HOST_OS)

import Data.Aeson (eitherDecodeStrict)
import Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as M
import Data.Text.Encoding (encodeUtf8)
import GHC.IO (unsafePerformIO)
import Language.Javascript.JSaddle (eval, valToText)
import Text.Read (readMaybe)

-- file -> 0-based line -> [(colStart, colEnd, markdown)]
type HoverMap = M.Map FilePath (M.Map Int [(Int, Int, Text)])

{-# NOINLINE hoverState #-}
hoverState :: IORef (Maybe HoverMap)
hoverState = unsafePerformIO (newIORef Nothing)

getHovers :: IO HoverMap
getHovers = readIORef hoverState >>= \case
  Just m  -> return m
  Nothing -> do
    txt <- valToText =<< eval ("JSON.stringify(window.leksahDemoHovers || {})" :: Text)
    -- JSON object keys are strings, so lines arrive as text; spans as arrays
    -- [start, end, markdown].
    let decode = M.map (M.fromList
                         . concatMap (\(k, v) -> maybe [] (\n -> [(n, v)]) (readMaybe k))
                         . M.toList)
        m = case eitherDecodeStrict (encodeUtf8 txt) of
              Right (kv :: M.Map FilePath (M.Map String [(Int, Int, Text)])) -> decode kv
              Left _ -> M.empty
    atomicWriteIORef hoverState (Just m)
    return m

-- | The precomputed hover at @(line, ch)@ (0-based) of @file@, if the demo
-- shipped one: the span whose @[colStart, colEnd)@ contains @ch@.  The file
-- is matched exactly first, then by unique path suffix (terminal hovers can
-- arrive with a repo-relative path).
demoHover :: FilePath -> Int -> Int -> IO (Maybe Text)
demoHover file line ch = do
  m <- getHovers
  let byFile = case M.lookup file m of
        Just x  -> Just x
        Nothing -> case [ v | (k, v) <- M.toList m, ('/' : file) `isSuffixOf` ('/' : k) ] of
                     [x] -> Just x
                     _   -> Nothing
  return $ do
    lm <- byFile
    spans <- M.lookup line lm
    case [ md | (s, e, md) <- spans, s <= ch && ch < e ] of
      (md:_) -> Just md
      []     -> Nothing

#else

demoHover :: FilePath -> Int -> Int -> IO (Maybe Text)
demoHover _ _ _ = return Nothing

#endif
