{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad (forM)

import Data.Bits (testBit)
import Data.List (partition)
import qualified Data.Map as M (fromListWith, toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, null, unwords)
import Data.Text.Encoding (decodeUtf8Lenient)

import Text.Read (readMaybe)

import System.Process (readProcessWithExitCode)

import IDE.Utils.RemoteExec (runSsh, shellQuote)
import IDE.Utils.RemotePath (isRemotePath, parseRemotePath, renderRemotePath)

import Clay
       (overflow, auto, height, pct, whiteSpace, nowrap, color, bold,
        fontWeight, paddingRight, cursorDefault,
        background, padding, px, (?), Css, Cursor(..), cursor)

import Reflex
       (Dynamic, Event, switchDyn, leftmost, current, tag)
import Reflex.Dom.Core
       (divClass, elClass, elDynAttr', simpleList, dynText,
        domEvent, EventName(..), (=:))

import IDE.Web.Theme (selectionColor, dimColor)
import IDE.Problems.Types (Loc(..), Pos(..), pointRange)
import IDE.Web.Events (GrepEvents(..), FindbarEvents)
import IDE.Web.Widget.Findbar (findSelection)
import IDE.Web.Frame (MonadWidget)

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
        -- Flat black (via the bottom-bar pane's black background), like the Changes pane.
        height (pct 100)
        overflow auto
    ".grep .grep-item" ? do
        whiteSpace nowrap
        cursor cursorDefault
    ".grep .grep-item.selected" ?
        background selectionColor
    ".grep .grep-loc" ? do
        color dimColor
        fontWeight bold
        paddingRight (px 8)
    ".grep .grep-empty" ? do
        color dimColor
        padding (px 8) (px 8) (px 8) (px 8)

grepWidget
  :: forall t m . MonadWidget t m
  => Dynamic t [GrepResult]      -- ^ the current grep results
  -> Event t FindbarEvents       -- ^ find within the results (highlights matches)
  -> m (Event t GrepEvents)
grepWidget resultsD findE = divClass "grep leksah-nav" $ do
  -- Find-within selects a result by index (reuse the shared list-find helper),
  -- highlighting the matching row.
  findSelD <- findSelection findE (zip [0 :: Int ..] . map grepLabel <$> resultsD)
  let withIdx = zip [0 :: Int ..] <$> resultsD
  clicksD <- simpleList withIdx $ \itemD -> do
    let resD = snd <$> itemD
        idxD = fst <$> itemD
        clsD = (\i sel -> "class" =: ("grep-item leksah-nav-item" <> if Just i == sel then " selected" else ""))
                 <$> idxD <*> findSelD
    (e, _) <- elDynAttr' "div" clsD $ do
      elClass "span" "grep-loc" $
        dynText $ (\r -> T.pack (grepFile r) <> ":" <> T.pack (show (grepLine r)) <> ":") <$> resD
      dynText $ grepContext <$> resD
    return $ (GrepGoto . resultToLoc) <$> tag (current resD) (domEvent Click e)
  return $ switchDyn (leftmost <$> clicksD)

grepLabel :: GrepResult -> Text
grepLabel r = T.pack (grepFile r) <> ":" <> grepContext r

-- | Grep output lines are 1-based; 'Pos' lines are 0-based.
resultToLoc :: GrepResult -> Loc
resultToLoc r = Loc (grepFile r) (pointRange (Pos (grepLine r - 1) 0))

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
          (remoteDirs, localDirs) = partition isRemotePath dirs
      localResults <- if null localDirs then return [] else do
          (_ec, out, _err) <- readProcessWithExitCode "grep" (args ++ localDirs) ""
          return $ mapMaybe parseGrepLine (lines out)
      -- Remote dirs, grouped per host: ONE ssh exec per host running the
      -- identical grep; result paths get their ssh://host prefix back so
      -- clicking a match opens the remote file.  grep exit 1 = no matches.
      remoteResults <- fmap concat . forM (groupByHost remoteDirs) $ \(host, rdirs) ->
          (do (_code, out, _) <- runSsh host
                  ("exec grep " <> T.unwords (map (shellQuote . T.pack) (args ++ rdirs)))
                  [] mempty
              return [ r { grepFile = renderRemotePath host (grepFile r) }
                     | r <- mapMaybe parseGrepLine (lines (T.unpack (decodeUtf8Lenient out))) ])
            `catch` \(_ :: SomeException) -> return []
      return $ take 1000 (localResults ++ remoteResults)
  where
    groupByHost rdirs = M.toList $ M.fromListWith (++)
        [ (host, [rdir]) | Just (host, rdir) <- map parseRemotePath rdirs ]

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
