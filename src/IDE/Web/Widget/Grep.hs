{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Grep
  ( grepCss
  , grepWidget
  , GrepResult(..)
  , runGrep
  ) where

import Control.Exception (catch, SomeException)

import Data.Bits (testBit)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, null)

import Text.Read (readMaybe)

import System.Process (readProcessWithExitCode)

import Clay
       (overflow, auto, height, pct, whiteSpace, nowrap, grey, color, bold,
        fontWeight, paddingRight, vGradient, backgroundImage, cursorDefault,
        background, padding, px, (?), Css, Color(..), Cursor(..), cursor)

import Reflex
       (Dynamic, Event, switchDyn, leftmost, current, tag)
import Reflex.Dom.Core
       (MonadWidget, divClass, elClass, elDynAttr', simpleList, dynText,
        domEvent, EventName(..), (=:))

import IDE.Core.CTypes (SrcSpan(..))
import IDE.Web.Events (GrepEvents(..), FindbarEvents)
import IDE.Web.Widget.Findbar (findSelection)

-- | One match from a workspace grep: the (absolute) file, 1-based line, and the
-- text of the matching line.
data GrepResult = GrepResult
  { grepFile    :: FilePath
  , grepLine    :: Int
  , grepContext :: Text
  } deriving (Eq, Show)

grepCss :: Css
grepCss = do
    ".grep" ? do
        backgroundImage (vGradient (Rgba 32 32 32 1.0) (Rgba 16 16 16 1.0))
        height (pct 100)
        overflow auto
    ".grep .grep-item" ? do
        whiteSpace nowrap
        cursor cursorDefault
    ".grep .grep-item.selected" ?
        background (Rgba 30 88 209 1.0)
    ".grep .grep-loc" ? do
        color grey
        fontWeight bold
        paddingRight (px 8)
    ".grep .grep-empty" ? do
        color grey
        padding (px 8) (px 8) (px 8) (px 8)

grepWidget
  :: forall t m . MonadWidget t m
  => Dynamic t [GrepResult]      -- ^ the current grep results
  -> Event t FindbarEvents       -- ^ find within the results (highlights matches)
  -> m (Event t GrepEvents)
grepWidget resultsD findE = divClass "grep" $ do
  -- Find-within selects a result by index (reuse the shared list-find helper),
  -- highlighting the matching row.
  findSelD <- findSelection findE (zip [0 :: Int ..] . map grepLabel <$> resultsD)
  let withIdx = zip [0 :: Int ..] <$> resultsD
  clicksD <- simpleList withIdx $ \itemD -> do
    let resD = snd <$> itemD
        idxD = fst <$> itemD
        clsD = (\i sel -> "class" =: ("grep-item" <> if Just i == sel then " selected" else ""))
                 <$> idxD <*> findSelD
    (e, _) <- elDynAttr' "div" clsD $ do
      elClass "span" "grep-loc" $
        dynText $ (\r -> T.pack (grepFile r) <> ":" <> T.pack (show (grepLine r)) <> ":") <$> resD
      dynText $ grepContext <$> resD
    return $ (GrepGoto . resultToSpan) <$> tag (current resD) (domEvent Click e)
  return $ switchDyn (leftmost <$> clicksD)

grepLabel :: GrepResult -> Text
grepLabel r = T.pack (grepFile r) <> ":" <> grepContext r

resultToSpan :: GrepResult -> SrcSpan
resultToSpan r = SrcSpan (grepFile r) (grepLine r) 0 (grepLine r) 0

-- | Grep the given directories (the caller orders them active-package first) for
-- the query, honouring the find-bar flags (bit 0 = case sensitive, bit 1 = whole
-- word; the pattern is always treated as an extended regexp, as in the GTK UI).
-- Returns up to 1000 matches as (file, line, line-text).
runGrep :: Text -> Int -> [FilePath] -> IO [GrepResult]
runGrep q flags dirs
  | T.null q || null dirs = return []
  | otherwise = (`catch` \(_ :: SomeException) -> return []) $ do
      let cs    = testBit flags 0
          wordB = testBit flags 1
          args  = (if cs then [] else ["-i"])
               ++ (if wordB then ["-w"] else [])
               ++ [ "-rEnI"
                  , "--exclude=*~"
                  , "--exclude-dir=.git", "--exclude-dir=.svn", "--exclude-dir=_darcs"
                  , "--exclude-dir=dist", "--exclude-dir=dist-newstyle", "--exclude-dir=dist-ghcjs"
                  , T.unpack q ]
               ++ dirs
      (_ec, out, _err) <- readProcessWithExitCode "grep" args ""
      return $ take 1000 $ mapMaybe parseGrepLine (lines out)

-- | Parse a @grep -n@ output line: @path:line:matched text@.  File paths can't
-- contain a newline and rarely contain a colon, so split on the first two.
parseGrepLine :: String -> Maybe GrepResult
parseGrepLine l =
  case break (== ':') l of
    (file, ':':rest) ->
      case break (== ':') rest of
        (lineStr, ':':ctx) -> (\n -> GrepResult file n (T.pack ctx)) <$> readMaybe lineStr
        _                  -> Nothing
    _ -> Nothing
