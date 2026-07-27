{-# LANGUAGE OverloadedStrings #-}
-- | Compose the demo's Claude Code terminal window (10-claude.ans).
--
-- Takes the committed raw-claude-capture.ans — a live `tmux capture-pane -e`
-- of a real Claude Code session working on this repo, taken at 100 columns —
-- and splices in the Update(src/IDE/Web/Instance.hs) diff blocks from
-- docs/development/lsp-hover-terminal-sample.txt (colorized in the genuine
-- Claude diff style copied from the capture), so the demo terminal carries
-- hover targets whose gutter line numbers match the real Instance.hs.
--
-- The splice point is the live status region (spinner + task list + prompt
-- box) at the bottom of the capture, so the blocks read as part of the
-- transcript.  All OSC sequences are stripped: they carry absolute file://
-- paths and a claude.ai session URL that must not ship in a public demo
-- page (the visible text is kept — only the invisible link payloads go).
--
-- Run from this directory:  runghc compose-claude.hs
module Main (main) where

import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.Char (isDigit, isSpace)
import Data.List (findIndex)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import System.Exit (die)
import Text.Printf (printf)

rawFile, fixtureFile, outFile :: FilePath
rawFile = "raw-claude-capture.ans"
fixtureFile = "../../../development/lsp-hover-terminal-sample.txt"
outFile = "10-claude.ans"

-- Ports of the CSI/OSC strip regexes (self-contained, like the Python was):
--   CSI: \x1b\[[0-9;:?]*[ -/]*[@-~]     OSC: \x1b\][^\x07\x1b]*(\x07|\x1b\\)
stripOsc :: Text -> Text
stripOsc t = case T.breakOn "\x1b]" t of
  (pre, rest)
    | T.null rest -> pre
    | otherwise ->
        let body = T.drop 2 rest
            (run, after) = T.span (\c -> c /= '\x07' && c /= '\x1b') body
        in case () of
          _ | Just more <- T.stripPrefix "\x07" after -> pre <> stripOsc more
            | Just more <- T.stripPrefix "\x1b\\" after -> pre <> stripOsc more
            | otherwise -> pre <> "\x1b]" <> run <> stripOsc after

stripCsi :: Text -> Text
stripCsi t = case T.breakOn "\x1b[" t of
  (pre, rest)
    | T.null rest -> pre
    | otherwise ->
        let body = T.drop 2 rest
            (params, r1) = T.span (`elem` ("0123456789;:?" :: String)) body
            (inter, r2)  = T.span (\c -> c >= ' ' && c <= '/') r1
        in case T.uncons r2 of
          Just (f, more) | f >= '@' && f <= '~' -> pre <> stripCsi more
          _ -> pre <> "\x1b[" <> params <> inter <> stripCsi r2

visible :: Text -> Text
visible = stripCsi . stripOsc

-- ---------------------------------------------------------------------------
-- Colorize the fixture's two Update blocks in the capture's own diff style:
--   header   ⏺ Update(path)            green bullet, bold verb
--   summary  ⎿  Updated …               dim
--   context  <indent>NNN    code        dim number, plain code
--   removal  <indent>NNN -  code        red on dark red, padded to width
--   addition <indent>NNN +  code        green on dark green, padded to width
-- Visible columns are preserved exactly — SGR only — so the GUT/ID regexes
-- (terminalLinksJs and gen-demo-hovers.hs) see the same layout they match
-- in a real Claude pane.
-- ---------------------------------------------------------------------------
width :: Int
width = 100

-- ^● Update\((.+)\)$
hdrMatch :: Text -> Maybe Text
hdrMatch l = do
  mid <- T.stripPrefix "\9679 Update(" l >>= T.stripSuffix ")"
  guard (not (T.null mid))
  pure mid

-- ^  ⎿  (.*)$
summaryMatch :: Text -> Maybe Text
summaryMatch = T.stripPrefix "  \x23BF  "

-- ^(\s+)(\d+) ([-+ ])  (.*)$
gutMatch :: Text -> Maybe (Text, Text, Char, Text)
gutMatch l = do
  let (ind, r1) = T.span isSpace l
  guard (not (T.null ind))
  let (num, r2) = T.span isDigit r1
  guard (not (T.null num))
  ('\x20', r3) <- T.uncons r2
  (mark, r4) <- T.uncons r3
  guard (mark `elem` ("-+ " :: String))
  code <- T.stripPrefix "  " r4
  pure (ind, num, mark, code)

colorize :: Text -> Text
colorize line
  | Just path <- hdrMatch line =
      "\x1b[38;5;114m\x23FA\x1b[39m \x1b[1mUpdate\x1b[0m(" <> path <> ")"
  | Just rest <- summaryMatch line =
      -- NB the second "space" after ⎿ is a NO-BREAK SPACE, faithfully ported
      -- from the original (and from the real Claude capture style).
      "\x1b[38;5;246m  \x23BF \xA0" <> rest <> "\x1b[39m"
  | Just (ind, num, mark, code) <- gutMatch line =
      if mark == ' '
        then ind <> "\x1b[2m\x1b[38;5;231m" <> num <> " \x1b[0m\x1b[38;5;231m   " <> code <> "\x1b[39m"
        else
          let (fg, bg) = if mark == '+' then ("38;5;77", "48;5;22") else ("38;5;167", "48;5;52")
              body = num <> " " <> T.singleton mark <> "  " <> code
              pad = T.replicate (max 0 (width - T.length ind - T.length body)) " "
          in ind <> "\x1b[" <> fg <> "m\x1b[" <> bg <> "m" <> body
                 <> "\x1b[38;5;231m" <> pad <> "\x1b[39m\x1b[49m"
  | otherwise = line

main :: IO ()
main = do
  raw <- TE.decodeUtf8 <$> BS.readFile rawFile
  let ls = T.splitOn "\n" (stripOsc raw)
  spliceAt <- case findIndex (\l -> "\183 " `T.isPrefixOf` T.stripStart (visible l)) ls of
    Just i  -> pure i
    Nothing -> die "compose-claude.hs: no spinner line found in raw capture"
  fixture <- TE.decodeUtf8 <$> BS.readFile fixtureFile
  let fls = T.splitOn "\n" fixture
  firstHdr <- case findIndex ("\9679 Update(" `T.isPrefixOf`) fls of
    Just i  -> pure i
    Nothing -> die "compose-claude.hs: no Update( header in fixture"
  let blocks = [colorize l | l <- drop firstHdr fls, l /= ""]
      -- Re-insert the blank line between the two blocks.
      outBlocks = foldl
        (\acc l -> if "\x1b[38;5;114m" `T.isPrefixOf` l && not (null acc)
                     then acc ++ ["", l] else acc ++ [l])
        [] blocks
      lead = "\x1b[38;5;231m\x23FA\x1b[39m Reapplying the Instance.hs port-handling edits:"
      composed = take spliceAt ls ++ [lead, ""] ++ outBlocks ++ [""] ++ drop spliceAt ls
  BS.writeFile outFile (TE.encodeUtf8 (T.intercalate "\n" composed))
  printf "wrote %s (%d lines)\n" outFile (length composed)
