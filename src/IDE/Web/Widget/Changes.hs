{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-- | A \"Changes\" pane (sharing the bottom location with Errors/Log): a flat
-- list of the workspace's modified files, decorated with the same VS Code-style
-- git cues as the workspace tree (status colour + a single-letter badge) and an
-- added/deleted line count.  Clicking a row opens that file in the editor.
--
-- The change set is recomputed on a slow poll (and whenever the workspace's
-- project directories change), so it stays current as files are edited/saved;
-- 'holdUniqDyn' keeps the DOM from churning when nothing actually changed.
module IDE.Web.Widget.Changes
  ( changesCss
  , changesWidget
  ) where

import Control.Exception (catch, SomeException)
import Control.Lens (to, (^..), preview, _Just)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Char (isSpace)
import Data.List (nub, dropWhileEnd, sortOn, isPrefixOf, tails)
import Data.Map (Map)
import qualified Data.Map as M (fromList, lookup, elems, null)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Time.Clock (NominalDiffTime)

import Clay
       (overflow, auto, height, pct, whiteSpace, nowrap, grey, color,
        padding, paddingLeft, fontWeight, bold, px, rgb, background, (?), Css,
        Cursor(..), cursorDefault)

import System.Exit (ExitCode(..))
import System.FilePath ((</>), dropFileName, makeRelative)
import System.Process (readProcessWithExitCode)

import Reflex
       (holdDyn, holdUniqDyn, listViewWithKey, never, ffor, switchHold,
        fmapMaybe, ffilter, leftmost, tag, current, updated, performEvent,
        constDyn, getPostBuild, tickLossyFromPostBuildTime, Dynamic, Event)
import Reflex.Dom.Core
       (MonadWidget, divClass, el, elClass, elDynAttr', elDynClass, dynText,
        text, dyn, domEvent, EventName(..), (=:))

import IDE.Core.State (IDE, workspace, wsFile, wsProjects, pjDir, pjKey)
import IDE.Web.Events (ChangesEvents(..), FindbarEvents)
import IDE.Web.Widget.FileTree (GitStatus(..), gitClass, gitBadge)
import IDE.Web.Widget.Findbar (findSelection)
import IDE.Web.Widget.Tree (scrollIntoViewNearest)

-- | A changed file: its absolute path, git status, and (where git can report
-- them) the number of added/deleted lines.
data FileChange = FileChange
  { changePath    :: FilePath
  , changeStatus  :: GitStatus
  , changeAdded   :: Maybe Int
  , changeDeleted :: Maybe Int
  } deriving (Eq)

-- How often to re-scan the workspace for changes.
pollInterval :: NominalDiffTime
pollInterval = 3

changesCss :: Css
changesCss = do
    ".changes" ? do
        height (pct 100)
        overflow auto
    ".changes .change-item" ? do
        whiteSpace nowrap
        cursor cursorDefault
        padding (px 1) (px 4) (px 1) (px 4)
    ".changes .change-item.selected" ?
        background (rgb 0x1e 0x58 0xd1)
    ".changes .change-badge" ? do
        paddingLeft (px 0)
        fontWeight bold
    ".changes .change-counts" ? paddingLeft (px 8)
    ".changes .change-added"   ? color (rgb 0x73 0xc9 0x91)
    ".changes .change-deleted" ? color (rgb 0xc7 0x4e 0x39)
    ".changes .changes-hint" ? do
        color grey
        padding (px 8) (px 8) (px 8) (px 8)

changesWidget
  :: forall t m . MonadWidget t m
  => Dynamic t IDE
  -> Event t FindbarEvents
  -> m (Event t ChangesEvents)
changesWidget ide findE = divClass "changes" $ do
  -- The distinct project directories of the open workspace.
  dirsD <- holdUniqDyn $ nub . (^.. workspace . _Just . wsProjects . traverse . to (pjDir . pjKey)) <$> ide
  -- The workspace file's directory; paths are shown relative to it.
  wsDirD <- holdUniqDyn $ maybe "" dropFileName . preview (workspace . _Just . wsFile) <$> ide
  postBuild <- getPostBuild
  tick <- tickLossyFromPostBuildTime pollInterval
  let refreshE = leftmost [ tag (current dirsD) postBuild
                          , tag (current dirsD) tick
                          , updated dirsD ]
  scanE <- performEvent $ ffor refreshE $ \dirs ->
      liftIO $ mconcat <$> mapM gitChanges dirs
  changesD <- holdUniqDyn =<< holdDyn mempty scanE
  -- Find selects a changed file: match its workspace-relative path; the row is
  -- highlighted and scrolled into view (see changeRow).
  let findItemsD = (\wsDir -> map (\c -> (changePath c, T.pack (makeRelative wsDir (changePath c))))
                       . sortOn changePath . M.elems) <$> wsDirD <*> changesD
  findSelD <- findSelection findE findItemsD
  switchHold never =<< dyn (ffor changesD $ \changes ->
    if M.null changes
      then do
        divClass "changes-hint" $ text "No changes in the workspace."
        return never
      else el "ul" $ do
        -- Key by path so rows are stable; sort for a stable, readable order.
        let keyed = M.fromList [ (changePath c, c) | c <- sortOn changePath (M.elems changes) ]
        fmapMaybe (listToMaybe . M.elems) <$>
          listViewWithKey (constDyn keyed) (\path -> changeRow wsDirD findSelD path))

-- | One row: status badge, workspace-relative path (status-coloured, with the
-- absolute path as a hover tooltip), and +added/-deleted.
changeRow
  :: MonadWidget t m
  => Dynamic t FilePath          -- ^ workspace directory (relative-path base)
  -> Dynamic t (Maybe FilePath)  -- ^ the find-selected path (highlighted/scrolled)
  -> FilePath                    -- ^ this row's path (the list key)
  -> Dynamic t FileChange
  -> m (Event t ChangesEvents)
changeRow wsDirD findSelD path cD = do
  let st = changeStatus <$> cD
      isSelD = (== Just path) <$> findSelD
      -- Absolute path on the row as a tooltip, since the label is relative.
      attrsD = (\c sel -> "class" =: ("change-item" <> if sel then " selected" else "")
                        <> "title" =: T.pack (changePath c)) <$> cD <*> isSelD
  (e, _) <- elDynAttr' "li" attrsD $ do
    -- Badge and path both take the status colour (reusing the global .git-*
    -- colour classes the workspace tree defines).
    elDynClass "span" (("change-badge " <>) . gitClass <$> st) . dynText $ gitBadge <$> st
    text " "
    elDynClass "span" (("git-name " <>) . gitClass <$> st) . dynText $
      (\wsDir c -> T.pack (makeRelative wsDir (changePath c))) <$> wsDirD <*> cD
    elClass "span" "change-counts" $ do
      elClass "span" "change-added"   . dynText $ maybe "" (\n -> "+" <> T.pack (show n)) . changeAdded   <$> cD
      text " "
      elClass "span" "change-deleted" . dynText $ maybe "" (\n -> "\x2212" <> T.pack (show n)) . changeDeleted <$> cD
  -- Scroll this row into view when find selects it.
  pb <- getPostBuild
  scrollIntoViewNearest (ffilter id $ leftmost [updated isSelD, tag (current isSelD) pb]) e
  return $ ChangesOpen . changePath <$> tag (current cD) (domEvent Click e)

-- | Collect the changed files for the git repo containing @dir@.  Best-effort:
-- returns empty if @dir@ is not in a repo or git is unavailable.
gitChanges :: FilePath -> IO (Map FilePath FileChange)
gitChanges dir = (`catch` \(_ :: SomeException) -> return mempty) $ do
  (rootRC, root, _) <- readProcessWithExitCode "git" ["-C", dir, "rev-parse", "--show-toplevel"] ""
  case rootRC of
    ExitSuccess -> do
      let root' = dropWhileEnd isSpace root
      (rc, out, _) <- readProcessWithExitCode "git"
        ["-c", "core.quotePath=false", "-C", dir, "status", "--porcelain", "-uall"] ""
      -- Added/deleted line counts for tracked changes relative to HEAD.
      (_, nout, _) <- readProcessWithExitCode "git"
        ["-c", "core.quotePath=false", "-C", dir, "diff", "--numstat", "HEAD"] ""
      let counts = parseNumstat root' nout
          mk (p, st) = (p, FileChange p st (fst <$> M.lookup p counts) (snd <$> M.lookup p counts))
      return . M.fromList . map mk $ if rc == ExitSuccess then parseStatus root' out else []
    _ -> return mempty
  where
    parseStatus root out =
      [ (root </> afterArrow path, st)
      | line <- lines out
      , length line >= 4
      , let (code, rest) = splitAt 2 line
      , Just st <- [classifyStatus code]
      , let path = drop 1 rest  -- the separating space after the XY code
      ]
    parseNumstat root out = M.fromList
      [ (root </> path, (readInt a, readInt d))
      | line <- lines out
      , (a, rest1) <- [break (== '\t') line]
      , let rest1' = drop 1 rest1
      , (d, rest2) <- [break (== '\t') rest1']
      , let path = drop 1 rest2
      , not (null path)
      ]
    readInt s = case reads s of { [(n, _)] -> n; _ -> 0 }  -- "-" (binary) -> 0
    afterArrow p = case [ s | s <- tails p, " -> " `isPrefixOf` s ] of
      (s:_) -> drop 4 s
      []    -> p

-- | Classify a porcelain XY status code (kept in step with the workspace tree).
classifyStatus :: String -> Maybe GitStatus
classifyStatus = \case
  ('?':_)   -> Just Untracked
  code@(x:y:_)
    | x == 'U' || y == 'U' || code == "AA" || code == "DD" -> Just Conflict
    | x == 'R' || y == 'R'                                 -> Just Renamed
    | x == 'A'                                             -> Just Added
    | x == 'D' || y == 'D'                                 -> Just Deleted
    | x == 'M' || x == 'T' || y == 'M' || y == 'T'         -> Just Modified
  _ -> Nothing
