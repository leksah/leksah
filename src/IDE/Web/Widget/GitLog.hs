{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
-- | A git log viewer, opened from the workspace git tree by clicking a branch.
--
-- Four panes in one center tab: a scrollable commit list drawn as a multi-lane
-- commit graph (a reflex list — one row per commit, the graph column an inline
-- SVG whose lanes are computed from the parent DAG); a commit-detail pane (the
-- full message, author, date, hash, parents); a selectable list of the files
-- the selected commit changed; and a side-by-side CodeMirror diff (the same
-- @MergeView@ the editor uses, via @LeksahCM.showDiff@) of the selected file's
-- change against its first parent.
--
-- All git access goes through the remote-aware 'IDE.Git.runGit' seam and runs
-- off the reflex frame thread, so a remote (@ssh:\/\/@) checkout works too.  The
-- browser/JS demo has no git ('runGit' fails there) → the panes are just empty.
module IDE.Web.Widget.GitLog
  ( gitLogCss
  , gitLogWidget
  , gitLogSplitJs
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.))

import Data.List (find, findIndex, elemIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
       (pack, unpack, take, strip, null, words, splitOn, dropWhile, lines, unlines)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Text.Read (readMaybe)
import System.Exit (ExitCode(..))

import Clay ((?), (-:), Css)

import Language.Javascript.JSaddle (jsg, js1, js4, liftJSM)

import Reflex
       (holdDyn, getPostBuild, performEvent_, newTriggerEvent, ffor, leftmost,
        tag, current, updated, switchDyn, fmapMaybe, never, Dynamic, Event)
import Reflex.Dom.Core
       (MonadWidget, divClass, elClass, elClass', elAttr', elDynAttr',
        elDynHtmlAttr', elDynClass, dynText, text, dyn, simpleList, domEvent,
        EventName(..), _element_raw, blank, (=:))

import IDE.Git (runGit, runGitBatch)
import IDE.Web.Events (GitLogEvents)
import IDE.Web.Widget.Editor (ensureMonacoLoaded)
import IDE.Web.Widget.FileTree (GitStatus(..), gitClass)

-- | A parsed @git log@ entry.
data Commit = Commit
  { cHash    :: Text
  , cParents :: [Text]
  , cAuthor  :: Text
  , cTime    :: Int      -- ^ author time, unix seconds
  , cSubject :: Text
  , cBody    :: Text
  } deriving (Eq)

-- | One file a commit changed: its git status (for the same status colour the
-- workspace tree / Changes pane use) and git's added/deleted line counts
-- ('Nothing' = binary, git reports @-@).
data DiffFile = DiffFile
  { dfPath    :: FilePath
  , dfStatus  :: GitStatus
  , dfAdded   :: Maybe Int
  , dfDeleted :: Maybe Int
  } deriving (Eq)

-- How many commits to load (a viewer, not a full history browser).
logLimit :: Int
logLimit = 500

--------------------------------------------------------------------------------
-- Widget

gitLogWidget
  :: forall t m . MonadWidget t m
  => Bool             -- ^ render diffs with the Monaco backend (the pref)
  -> FilePath -> Text -> m (Event t GitLogEvents)
gitLogWidget useMonaco dir branch = divClass "gitlog" $ do
  -- Load the log off-frame (several are large; a remote one is an ssh round trip).
  (commitsE, fireCommits) <- newTriggerEvent
  pb <- getPostBuild
  performEvent_ $ ffor pb $ \_ ->
      liftIO . void . forkIO $ gitLog dir branch >>= fireCommits
  commitsD <- holdDyn [] commitsE
  -- Pair each commit with its graph row.  Every row's graph is drawn at the
  -- SAME width (the max lane count across the log) so the hash/subject/author
  -- columns line up down the list instead of shifting with each row's lanes.
  let rowsD = ffor commitsD $ \cs ->
        let grs = layoutGraph cs
            maxCols = maximum (1 : map grCols grs)
        in [ (maxCols, c, g) | (c, g) <- zip cs grs ]

  rec
    selHashD <- holdDyn Nothing (Just <$> commitClickE)
    let selCommitD = (\mh cs -> mh >>= \h -> find ((== h) . cHash) cs)
                       <$> selHashD <*> commitsD
    -- Top: the commit graph/list (left) and the detail + changed-files (right).
    (commitClickE, selFileD) <- divClass "gitlog-top" $ do
        cClickE <- divClass "gitlog-commits" $ do
            evsD <- simpleList rowsD (commitRow selHashD)
            return $ switchDyn (leftmost <$> evsD)
        gitLogDivider "v"   -- drag to resize the commit list / detail split
        sFileD <- divClass "gitlog-side" $ do
            -- Commit detail (rebuilt on selection change).
            _ <- divClass "gitlog-detail" $ dyn $ ffor selCommitD $ \case
                Nothing -> divClass "gitlog-hint" $ text "Select a commit."
                Just c  -> commitDetail c
            gitLogDivider "h"   -- drag to resize the detail / files split
            -- Changed files for the selected commit.
            (filesE, fireFiles) <- newTriggerEvent
            performEvent_ $ ffor (updated selCommitD) $ \case
                Nothing -> liftIO (fireFiles [])
                Just c  -> liftIO . void . forkIO $ commitFiles dir (cHash c) >>= fireFiles
            filesD <- holdDyn [] filesE
            rec
              fileClickE <- divClass "gitlog-files" $ do
                  evsD <- simpleList filesD (fileRow selFD)
                  return $ switchDyn (leftmost <$> evsD)
              selFD <- holdDyn Nothing $ leftmost
                  [ Just <$> fileClickE, Nothing <$ updated selCommitD ]
            return selFD
        return (cClickE, sFileD)
    gitLogDivider "h"   -- drag to resize the upper panes / diff split
    -- Side-by-side diff of the selected file — full width, under both panes.
    diffPane useMonaco dir selCommitD selFileD
  return (never :: Event t GitLogEvents)

-- | A draggable splitter between two flex panes: @"v"@ is a vertical bar that
-- resizes the pane on its left (by width), @"h"@ a horizontal bar that resizes
-- the pane above it (by height).  Armed via the raw handle by 'gitLogSplitJs'.
gitLogDivider :: MonadWidget t m => Text -> m ()
gitLogDivider kind = do
  (e, _) <- elAttr' "div" ("class" =: "gitlog-divider" <> "data-split" =: kind) blank
  pb <- getPostBuild
  performEvent_ $ ffor pb $ \_ -> liftJSM . void $
      jsg ("LeksahGitLogSplit" :: Text) ^. js1 ("arm" :: Text) (_element_raw e)

-- | @window.LeksahGitLogSplit.arm(divider)@ — live drag-to-resize.  The divider
-- resizes its PREVIOUS flex sibling (the other sibling is @flex:1 1 0@ and takes
-- the slack); @data-split="v"@ resizes by width, @"h"@ by height.  Eval'd once
-- at window startup from "IDE.Web.Main".
gitLogSplitJs :: Text
gitLogSplitJs = T.unlines
  [ "window.LeksahGitLogSplit = { arm: function(el){"
  , "  el.addEventListener('mousedown', function(e){"
  , "    if (e.button !== 0) return;"
  , "    var prev = el.previousElementSibling; if (!prev) return;"
  , "    e.preventDefault(); e.stopPropagation();"
  , "    var xAxis = el.getAttribute('data-split') === 'v';"
  , "    var root = el.closest('.gitlog');"
  , "    var start = xAxis ? e.clientX : e.clientY;"
  , "    var r = prev.getBoundingClientRect();"
  , "    var base = xAxis ? r.width : r.height;"
  , "    if (root) root.classList.add('resizing');"
  , "    el.classList.add('dragging');"
  , "    function mv(e2){"
  , "      var d = (xAxis ? e2.clientX : e2.clientY) - start;"
  , "      prev.style.flex = '0 0 ' + Math.max(60, base + d) + 'px';"
  , "    }"
  , "    function up(){"
  , "      document.removeEventListener('mousemove', mv);"
  , "      document.removeEventListener('mouseup', up);"
  , "      if (root) root.classList.remove('resizing');"
  , "      el.classList.remove('dragging');"
  , "    }"
  , "    document.addEventListener('mousemove', mv);"
  , "    document.addEventListener('mouseup', up);"
  , "  });"
  , "} };"
  ]

-- | One commit row: graph cell (inline SVG), short hash, subject, author, date.
commitRow
  :: MonadWidget t m
  => Dynamic t (Maybe Text)
  -> Dynamic t (Int, Commit, GraphRow)
  -> m (Event t Text)
commitRow selHashD itemD = do
  let cD  = (\(_, c, _) -> c) <$> itemD
      grD = (\(mx, _, g) -> (mx, g)) <$> itemD
      hD  = cHash <$> cD
      selD = (\s h -> s == Just h) <$> selHashD <*> hD
      attrsD = ffor selD $ \sel ->
          "class" =: ("gitlog-commit leksah-nav-item" <> if sel then " selected" else "")
  (e, _) <- elDynAttr' "div" attrsD $ do
      _ <- elDynHtmlAttr' "div" ("class" =: "gitlog-graph") (uncurry rowSvg <$> grD)
      elClass "span" "gitlog-hash"    $ dynText (T.take 8 <$> hD)
      elClass "span" "gitlog-subject" $ dynText (cSubject <$> cD)
      elClass "span" "gitlog-author"  $ dynText (cAuthor <$> cD)
      elClass "span" "gitlog-date"    $ dynText (fmtDate . cTime <$> cD)
  return $ tag (current hD) (domEvent Click e)

-- | The detail pane for one commit.
commitDetail :: MonadWidget t m => Commit -> m ()
commitDetail c = do
  elClass "div" "gitlog-detail-subject" $ text (cSubject c)
  elClass "div" "gitlog-detail-meta" $ do
      text (cHash c)
      text "  •  "
      text (cAuthor c)
      text "  •  "
      text (fmtDate (cTime c))
  if T.null (T.strip (cBody c))
    then blank
    else elClass "pre" "gitlog-detail-body" $ text (cBody c)

-- | One changed-file row.
fileRow
  :: MonadWidget t m
  => Dynamic t (Maybe DiffFile)
  -> Dynamic t DiffFile
  -> m (Event t DiffFile)
fileRow selFileD dfD = do
  let selD = (\s df -> s == Just df) <$> selFileD <*> dfD
      attrsD = ffor selD $ \sel ->
          "class" =: ("gitlog-file leksah-nav-item" <> if sel then " selected" else "")
  (e, _) <- elDynAttr' "div" attrsD $ do
      -- Same status colour the workspace tree / Changes pane use (the global
      -- @git-name@ + @.git-*@ classes).
      elDynClass "span" (("gitlog-file-path git-name " <>) . gitClass . dfStatus <$> dfD) $
          dynText (T.pack . dfPath <$> dfD)
      elClass "span" "gitlog-file-counts" $ do
          elClass "span" "gitlog-added"   $ dynText (maybe "" (\n -> "+" <> T.pack (show n)) . dfAdded   <$> dfD)
          text " "
          elClass "span" "gitlog-deleted" $ dynText (maybe "" (\n -> "\x2212" <> T.pack (show n)) . dfDeleted <$> dfD)
  return $ tag (current dfD) (domEvent Click e)

-- | The side-by-side diff: reuse the editor backend's standalone diff view —
-- the CodeMirror MergeView via @LeksahCM.showDiff@, or Monaco's DiffEditor via
-- @LeksahMonaco.showDiff@ when the Monaco pref is on (old\/parent left,
-- new\/commit right).  Mounts into a captured element by its raw handle —
-- never a postBuild querySelector (batched-DOM: the element isn't attached yet).
diffPane
  :: forall t m . MonadWidget t m
  => Bool
  -> FilePath
  -> Dynamic t (Maybe Commit)
  -> Dynamic t (Maybe DiffFile)
  -> m ()
diffPane useMonaco dir selCommitD selFileD = do
  let apiNs :: Text
      apiNs = if useMonaco then "LeksahMonaco" else "LeksahCM"
  (diffEl, _) <- elClass' "div" "gitlog-diffview" blank
  -- Clear the diff whenever the commit changes (a new file must be picked).
  performEvent_ $ ffor (updated selCommitD) $ \_ -> liftJSM . void $
      jsg apiNs ^. js1 ("destroyDiff" :: Text) (_element_raw diffEl)
  let reqE = fmapMaybe id $ updated ((\mc mf -> (,) <$> mc <*> mf) <$> selCommitD <*> selFileD)
  (contentE, fireContent) <- newTriggerEvent
  performEvent_ $ ffor reqE $ \(c, df) ->
      liftIO . void . forkIO $ do
          let path = dfPath df
          new <- gitShowFile dir (cHash c) path
          old <- gitShowFile dir (cHash c <> "^") path
          fireContent (T.pack path, old, new)
  performEvent_ $ ffor contentE $ \(path, old, new) -> liftJSM . void $ do
      when useMonaco ensureMonacoLoaded
      jsg apiNs ^. js4 ("showDiff" :: Text) (_element_raw diffEl) path old new

--------------------------------------------------------------------------------
-- git

-- | Load up to 'logLimit' commits for @branch@.  Fields are separated by 0x1f
-- and records by 0x1e so a multi-line body survives intact.
gitLog :: FilePath -> Text -> IO [Commit]
gitLog dir branch = do
  r <- try (runGit dir
        [ "log", "-n", T.pack (show logLimit)
        , "--pretty=format:%H%x1f%P%x1f%an%x1f%at%x1f%s%x1f%b%x1e", branch ])
  return $ case r :: Either SomeException (ExitCode, Text, Text) of
      Right (ExitSuccess, out, _) -> parseLog out
      _                           -> []

parseLog :: Text -> [Commit]
parseLog out =
  [ Commit h (T.words p) a (fromMaybe 0 (readMaybe (T.unpack tm))) s (T.strip body)
  | entry <- T.splitOn "\x1e" out
  , let entry' = T.dropWhile (\ch -> ch == '\n' || ch == '\r') entry
  , not (T.null (T.strip entry'))
  , (h : p : a : tm : s : bs) <- [T.splitOn "\x1f" entry']
  , let body = case bs of { (b:_) -> b; [] -> "" }
  ]

-- | The files a commit changed.  One round trip: @--name-status@ gives the
-- status letter per file (→ the status colour), @--numstat@ the added/deleted
-- counts; both with an empty @--format=@ to drop the commit header.
commitFiles :: FilePath -> Text -> IO [DiffFile]
commitFiles dir h = do
  r <- try (runGitBatch dir
        [ ["show", "--name-status", "--format=", h]
        , ["show", "--numstat",     "--format=", h] ])
  return $ case r :: Either SomeException [(ExitCode, Text, Text)] of
      Right ((ExitSuccess, nameOut, _) : (_, numOut, _) : _) ->
          mergeFiles (parseNameStatus nameOut) (parseNumstat numOut)
      _ -> []

-- | Attach each name-status file its numstat counts (by path; a rename's
-- counts may not match and are then left blank — acceptable).
mergeFiles :: [(FilePath, GitStatus)] -> [(FilePath, (Maybe Int, Maybe Int))] -> [DiffFile]
mergeFiles namest counts =
  [ DiffFile p st a d
  | (p, st) <- namest
  , let (a, d) = fromMaybe (Nothing, Nothing) (lookup p counts) ]

-- | @git show --name-status@ lines: @M\\tpath@, @A\\tpath@, @D\\tpath@,
-- @R100\\told\\tnew@ (rename → keep the new path).
parseNameStatus :: Text -> [(FilePath, GitStatus)]
parseNameStatus out =
  [ (T.unpack (last paths), classifyNameStatus code)
  | line <- T.lines out
  , not (T.null (T.strip line))
  , code : paths <- [T.splitOn "\t" line]
  , not (null paths)
  ]

classifyNameStatus :: Text -> GitStatus
classifyNameStatus code = case T.take 1 code of
  "A" -> Added
  "D" -> Deleted
  "R" -> Renamed
  "C" -> Renamed
  "U" -> Conflict
  _   -> Modified   -- M, T, and anything else

parseNumstat :: Text -> [(FilePath, (Maybe Int, Maybe Int))]
parseNumstat out =
  [ (T.unpack path, (readCount a, readCount d))
  | line <- lines (T.unpack out)
  , (a, rest1) <- [break (== '\t') line]
  , not (null a)
  , let rest1' = drop 1 rest1
  , (d, rest2) <- [break (== '\t') rest1']
  , let path = T.pack (drop 1 rest2)
  , not (T.null path)
  ]
  where readCount s = readMaybe s :: Maybe Int   -- "-" (binary) -> Nothing

-- | The contents of @path@ at @rev@ (empty on failure — a new file has no
-- parent blob, and the first commit has no @^@ parent).
gitShowFile :: FilePath -> Text -> FilePath -> IO Text
gitShowFile dir rev path = do
  r <- try (runGit dir ["show", rev <> ":" <> T.pack path])
  return $ case r :: Either SomeException (ExitCode, Text, Text) of
      Right (ExitSuccess, out, _) -> out
      _                           -> ""

fmtDate :: Int -> Text
fmtDate t = T.pack $
    formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (posixSecondsToUTCTime (fromIntegral t))

--------------------------------------------------------------------------------
-- Commit-graph lane layout

-- | Per-row graph geometry, in lane columns (0-based).
data GraphRow = GraphRow
  { grNode    :: Int          -- ^ the commit's column
  , grCols    :: Int          -- ^ column count (SVG width)
  , grIn      :: [Int]        -- ^ top-edge columns feeding into the node
  , grOut     :: [Int]        -- ^ bottom-edge columns leaving the node (parents)
  , grThrough :: [(Int, Int)] -- ^ (topCol, botCol) lanes passing through the row
  } deriving (Eq)

-- | Assign every commit a lane and produce per-row line geometry.  Standard
-- lane packing: a lane \"waits for\" a hash; a commit takes the (first) lane
-- waiting for it (merging any others in), then its first parent continues that
-- lane and extra parents open new lanes (reusing a lane that already waits for
-- them).  Commits arrive newest-first, as @git log@ emits them.
layoutGraph :: [Commit] -> [GraphRow]
layoutGraph = go []
  where
    go :: [Maybe Text] -> [Commit] -> [GraphRow]
    go _ [] = []
    go lanes (c : cs) =
      let h        = cHash c
          incoming = lanes
          cIxs     = [ i | (i, Just x) <- zip [0 ..] incoming, x == h ]
          col      = case cIxs of { (i : _) -> i; [] -> firstFree incoming }
          cleared  = [ if l == Just h then Nothing else l | l <- padTo (col + 1) incoming ]
          (out0, pcols) = assignParents (cParents c) col cleared
          outT     = trimNothings out0
          through  = [ (i, j)
                     | (i, Just x) <- zip [0 ..] incoming, x /= h
                     , Just j <- [elemIndex (Just x) outT] ]
          used     = col : cIxs ++ pcols ++ concatMap (\(a, b) -> [a, b]) through
          cols     = maximum (1 : map (+ 1) used)
      in GraphRow col cols cIxs pcols through : go outT cs

-- | Place a commit's parents into the cleared lane list, preferring the node's
-- own column for the first parent; returns the new lanes and the parent columns.
assignParents :: [Text] -> Int -> [Maybe Text] -> ([Maybe Text], [Int])
assignParents ps col = \lanes0 -> go lanes0 [] (zip [0 ..] ps)
  where
    go lanes acc [] = (lanes, reverse acc)
    go lanes acc ((idx, p) : rest) =
      case elemIndex (Just p) lanes of
        Just k  -> go lanes (k : acc) rest
        Nothing ->
          let k = if idx == (0 :: Int) && isFree col lanes then col else firstFree lanes
          in go (setAt k (Just p) lanes) (k : acc) rest
    isFree i xs = i >= length xs || xs !! i == Nothing

padTo :: Int -> [Maybe Text] -> [Maybe Text]
padTo n xs = xs ++ replicate (n - length xs) Nothing

setAt :: Int -> Maybe Text -> [Maybe Text] -> [Maybe Text]
setAt i v xs = let xs' = padTo (i + 1) xs in take i xs' ++ [v] ++ drop (i + 1) xs'

firstFree :: [Maybe Text] -> Int
firstFree xs = fromMaybe (length xs) (findIndex (== Nothing) xs)

trimNothings :: [Maybe Text] -> [Maybe Text]
trimNothings = reverse . dropWhile (== Nothing) . reverse

-- | Render one graph row as an inline SVG (parsed in HTML context, so the
-- @<svg>@ subtree is namespaced correctly).  Columns are 14px wide, rows 22px.
rowSvg :: Int -> GraphRow -> Text
rowSvg totalCols gr = svg
  where
    cw = 14 :: Double
    rh = 22 :: Double
    cy = rh / 2
    xf c = fromIntegral c * cw + cw / 2
    w    = fromIntegral (max 1 totalCols) * cw   -- uniform across all rows
    nd   = grNode gr
    d :: Double -> Text
    d = T.pack . show
    seg x1 y1 x2 y2 col =
        "<line x1=\"" <> d x1 <> "\" y1=\"" <> d y1 <> "\" x2=\"" <> d x2
          <> "\" y2=\"" <> d y2 <> "\" stroke=\"" <> col
          <> "\" stroke-width=\"2\" fill=\"none\"/>"
    through = [ seg (xf a) 0 (xf b) rh (laneColor b) | (a, b) <- grThrough gr ]
    ins     = [ seg (xf i) 0 (xf nd) cy (laneColor nd) | i <- grIn gr ]
    outs    = [ seg (xf nd) cy (xf k) rh (laneColor k) | k <- grOut gr ]
    dot     = "<circle cx=\"" <> d (xf nd) <> "\" cy=\"" <> d cy
                <> "\" r=\"3.5\" fill=\"" <> laneColor nd <> "\"/>"
    svg = "<svg width=\"" <> d w <> "\" height=\"" <> d rh
            <> "\" xmlns=\"http://www.w3.org/2000/svg\" style=\"display:block\">"
            <> mconcat (through ++ ins ++ outs) <> dot <> "</svg>"

laneColor :: Int -> Text
laneColor c = laneColors !! (c `mod` length laneColors)

laneColors :: [Text]
laneColors =
  [ "#e0533d", "#d9a441", "#5aa15a", "#3b9ec7", "#8a6fd1", "#c76ba6" ]

--------------------------------------------------------------------------------
-- CSS

gitLogCss :: Css
gitLogCss = do
    ".gitlog" ? do
        "display"        -: "flex"
        "flex-direction" -: "column"
        "height"         -: "100%"
        "overflow"       -: "hidden"
    -- Upper area: commit list (left) beside detail + changed files (right).
    ".gitlog-top" ? do
        "display"     -: "flex"
        "flex"        -: "0 0 55%"
        "min-height"  -: "0"
        "overflow"    -: "hidden"
    ".gitlog-commits" ? do
        "flex"          -: "0 0 45%"
        "overflow"      -: "auto"
    ".gitlog-side" ? do
        "flex"           -: "1 1 0"
        "display"        -: "flex"
        "flex-direction" -: "column"
        "min-height"     -: "0"
        "overflow"       -: "hidden"
    ".gitlog-detail" ? do
        "flex"       -: "0 0 30%"
        "overflow"   -: "auto"
        "padding"    -: "4px 8px"
    -- Draggable splitters between the panes.
    ".gitlog-divider" ? do
        "flex"       -: "0 0 3px"
        "background" -: "var(--leksah-border, #333)"
    ".gitlog-divider[data-split=v]" ? ("cursor" -: "col-resize")
    ".gitlog-divider[data-split=h]" ? ("cursor" -: "row-resize")
    ".gitlog-divider:hover" ? ("background" -: "var(--leksah-selection, #1e58d1)")
    ".gitlog-divider.dragging" ? ("background" -: "var(--leksah-selection, #1e58d1)")
    -- During a drag, don't let the panes' own contents (CodeMirror, the lists)
    -- swallow the pointer or start a text selection.
    ".gitlog.resizing" ? ("user-select" -: "none")
    ".gitlog.resizing .gitlog-commits" ? ("pointer-events" -: "none")
    ".gitlog.resizing .gitlog-files" ? ("pointer-events" -: "none")
    ".gitlog.resizing .gitlog-diffview" ? ("pointer-events" -: "none")
    ".gitlog-detail-subject" ? do
        "font-weight" -: "bold"
        "white-space" -: "pre-wrap"
    ".gitlog-detail-meta" ? do
        "color"       -: "grey"
        "font-size"   -: "90%"
        "padding"     -: "2px 0"
    ".gitlog-detail-body" ? do
        "white-space" -: "pre-wrap"
        "margin"      -: "4px 0 0 0"
        "font-family" -: "var(--leksah-mono, monospace)"
    ".gitlog-files" ? do
        "flex"          -: "1 1 0"
        "min-height"    -: "0"
        "overflow"      -: "auto"
    -- Full-width diff, below both upper panes.
    ".gitlog-diffview" ? do
        "flex"       -: "1 1 0"
        "overflow"   -: "hidden"
        "min-height" -: "0"
    ".gitlog-diffview .cm-mergeView" ? ("height" -: "100%")
    ".gitlog-commit" ? do
        "display"     -: "flex"
        "align-items" -: "center"
        "white-space" -: "nowrap"
        "cursor"      -: "default"
        "padding"     -: "0 4px"
        "line-height" -: "22px"
    ".gitlog-commit.selected" ? ("background" -: "var(--leksah-selection, #1e58d1)")
    ".gitlog-file.selected"   ? ("background" -: "var(--leksah-selection, #1e58d1)")
    ".gitlog-graph" ? do
        "flex"         -: "0 0 auto"
        "margin-right" -: "4px"
    ".gitlog-hash" ? do
        "color"        -: "grey"
        "font-family"  -: "var(--leksah-mono, monospace)"
        "margin-right" -: "6px"
    ".gitlog-subject" ? do
        "flex"          -: "1 1 auto"
        "overflow"      -: "hidden"
        "text-overflow" -: "ellipsis"
    -- Fixed-width column so committer names line up down the list.
    ".gitlog-author" ? do
        "flex"          -: "0 0 11em"
        "text-align"    -: "right"
        "overflow"      -: "hidden"
        "text-overflow" -: "ellipsis"
        "color"         -: "grey"
        "font-size"     -: "90%"
        "margin-left"   -: "8px"
    ".gitlog-date" ? do
        "color"                 -: "grey"
        "font-size"             -: "90%"
        "margin-left"           -: "8px"
        -- Fixed-width digits so the dates line up as a column.
        "font-variant-numeric"  -: "tabular-nums"
        "font-feature-settings" -: "\"tnum\""
    ".gitlog-file" ? do
        "display"     -: "flex"
        "white-space" -: "nowrap"
        "cursor"      -: "default"
        "padding"     -: "0 8px"
    ".gitlog-file-path"   ? ("flex" -: "1 1 auto")
    ".gitlog-file-counts" ? ("padding-left" -: "8px")
    ".gitlog-added"   ? ("color" -: "#73c991")
    ".gitlog-deleted" ? ("color" -: "#c74e39")
    ".gitlog-hint" ? do
        "color"   -: "grey"
        "padding" -: "8px"
