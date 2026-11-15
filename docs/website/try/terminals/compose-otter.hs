{-# LANGUAGE OverloadedStrings #-}
-- | Compose the demo's narrow agent transcript (30-Otter.ans): the Claude Code
-- session the demo shows BESIDE the breakout game, in the showcase window.
--
-- Unlike 10-Kiwi.ans (a real capture, 100 columns wide) this one is
-- written by hand, in the same colours as the capture, and kept within
-- 'width' columns so it fits its narrow leaf when the demo is not full
-- screen.  It tells a small story in the game's own vocabulary: Otter (the
-- power-ups agent in demo-agents.js) adding a power-up to app/Main.hs.  The
-- context lines come from the real game; the added lines are the story's.
--
-- Run from this directory:  runghc compose-otter.hs
module Main (main) where

import Control.Monad (forM_, when)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import System.Exit (die)

outFile :: FilePath
outFile = "30-Otter.ans"

-- | The widest a line may be, in columns.
width :: Int
width = 34

esc :: Text -> Text
esc s = "\ESC[" <> s <> "m"

fg :: Int -> Text
fg n = esc ("38;5;" <> T.pack (show n))

bg :: Int -> Text
bg n = esc ("48;5;" <> T.pack (show n))

reset, bold, dim, fgOff, bgOff :: Text
reset = esc "0"; bold = esc "1"; dim = esc "2"; fgOff = esc "39"; bgOff = esc "49"

-- | A visible line: its text with escapes, and its width in columns.
data Line = Line Text Int

plain :: Text -> Line
plain t = Line t (T.length t)

-- | The user's prompt, as Claude Code echoes it.
prompt :: [Text] -> [Line]
prompt (l : ls) = Line (fg 246 <> "> " <> fgOff <> l) (2 + T.length l)
                : [ Line ("  " <> x) (2 + T.length x) | x <- ls ]
prompt [] = []

-- | What the assistant says: a white bullet, then wrapped text.
say :: [Text] -> [Line]
say (l : ls) = Line (fg 231 <> "⏺" <> fgOff <> " " <> l) (2 + T.length l)
             : [ Line ("  " <> x) (2 + T.length x) | x <- ls ]
say [] = []

-- | A tool call (green bullet) and its one-line result.
tool :: Text -> Text -> Text -> [Line]
tool name arg result =
  [ Line (fg 114 <> "⏺" <> fgOff <> " " <> bold <> name <> reset <> "(" <> arg <> ")")
         (2 + T.length name + T.length arg + 2)
  , Line (fg 246 <> "  ⎿  " <> fgOff <> result) (5 + visible result) ]

-- | The columns a text takes on screen: its length without SGR escapes.
visible :: Text -> Int
visible t = case T.breakOn "\ESC[" t of
  (pre, rest)
    | T.null rest -> T.length pre
    | otherwise   -> T.length pre + visible (T.drop 1 (T.dropWhile (/= 'm') rest))

-- | Diff lines: line number, then context / added / removed code.
data D = C Int Text | A Int Text | R Int Text

diff :: [D] -> [Line]
diff = map one
  where
    pad t = t <> T.replicate (codeW - T.length t) " "
    codeW = width - 9
    num n = T.justifyRight 4 ' ' (T.pack (show n))
    one (C n t) = Line ("  " <> dim <> fg 231 <> num n <> reset <> fg 231 <> "   " <> t <> fgOff)
                       (9 + T.length t)
    one (A n t) = Line ("  " <> fg 77 <> bg 22 <> num n <> " + " <> fg 231 <> pad t <> fgOff <> bgOff)
                       (9 + max codeW (T.length t))
    one (R n t) = Line ("  " <> fg 167 <> bg 52 <> num n <> " - " <> fg 231 <> pad t <> fgOff <> bgOff)
                       (9 + max codeW (T.length t))

blank :: [Line]
blank = [plain ""]

-- | The live status line and the prompt box at the bottom of the pane.
footer :: [Line]
footer =
  [ Line (fg 174 <> "✻" <> fgOff <> " " <> fg 216 <> "Tuning drop rates…" <> fg 246 <> " (1m 12s)" <> fgOff) 29
  , plain ""
  , Line (fg 37 <> T.replicate width "─" <> fgOff) width
  , Line (fg 246 <> "❯ " <> fgOff) 2
  , Line (fg 37 <> T.replicate width "─" <> fgOff) width ]

transcript :: [Line]
transcript = concat
  [ prompt [ "a caught power-up should"
           , "widen the paddle for a while" ]
  , blank
  , say [ "I'll drop power-ups from a"
        , "few bricks, and widen the"
        , "paddle when it catches one."
        , "State first:" ]
  , blank
  , tool "Update" "app/Main.hs" ("Added " <> bold <> "2" <> reset <> " lines")
  , diff [ C 106 ", score, lives :: Int"
         , A 107 ", drops :: [Drop]"
         , A 108 ", grow  :: Int"
         , C 109 ", phase :: Phase" ]
  , blank
  , say [ "Now physics moves the drops"
        , "and the paddle catches them:" ]
  , blank
  , tool "Update" "app/Main.hs" ("Added " <> bold <> "3" <> reset <> " lines")
  , diff [ C 171 "hit = overlaps cx cy"
         , A 172 "(got, ds') = partition"
         , A 173 "  onPad (drops st)"
         , A 174 "grow' = 240 * length got"
         , C 175 "st' = st { bricks = left" ]
  , blank
  , tool "Bash" "cabal build" (fg 114 <> "Build succeeded" <> fgOff)
  , blank
  , say [ "Drops now widen the paddle." ]
  , blank
  , footer ]

main :: IO ()
main = do
  forM_ (zip [1 :: Int ..] transcript) $ \(i, Line _ w) ->
    when (w > width) $ die ("line " <> show i <> " is " <> show w <> " columns, over " <> show width)
  BS.writeFile outFile (encodeUtf8 (T.unlines [ t | Line t _ <- transcript ]))
  putStrLn ("wrote " <> outFile <> " (" <> show (length transcript) <> " lines, <= "
            <> show width <> " columns)")
