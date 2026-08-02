{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
-- | Side-by-side comparison of the N approaches of one compare-N queue group
-- (the same prompt run in N worktrees — see 'IDE.Web.ClaudeQueue.queueAdd').
-- One column per approach worktree, all showing the SAME selected file's diff
-- against each worktree's merge base, so the differing solutions line up on
-- screen.  A file strip above the columns lists the union of changed files
-- ("path k/N" = changed in k of the N approaches); each column links into the
-- full Review pane (comment loop, merge, PR, archive) for the approach that
-- wins.
--
-- Opened from a tagged card's Compare action in the Tasks pane, through the
-- 'IDE.Web.ClaudeQueue.requestCompare' bridge.  Diff mechanics (backends,
-- CSS) are the Review pane's; the scan reuses 'reviewFiles'.
module IDE.Web.Widget.Compare
  ( compareCss
  , compareWidget
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Lens ((^.))
import Control.Monad (void, forM, forM_, when)
import Control.Monad.IO.Class (liftIO)

import Data.List (nub, sort)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)

import System.FilePath ((</>))

import Clay ((?), (-:), Css)

import Language.Javascript.JSaddle (jsg, js1, js4, liftJSM)

import Reflex
       (Dynamic, Event, holdDyn, getPostBuild, performEvent_, newTriggerEvent,
        ffor, leftmost, tag, current, updated, never, attach)
import Reflex.Dom.Core
       (MonadWidget, divClass, elClass, elClass', elAttr', elDynAttr', dynText,
        dyn_, text, domEvent, EventName(..), _element_raw, blank, (=:))

import IDE.Web.ClaudeQueue (QueueTask(..), queueList)
import IDE.Web.Events (CompareEvents)
import IDE.Web.Widget.Editor (ensureMonacoLoaded)
import IDE.Web.Widget.GitLog (DiffFile(..), gitShowFile)
import IDE.Web.Widget.Review (reviewFiles)
import IDE.Web.Worktree (ReviewInfo(..), scanReview, requestReview)

-- | One approach column's data, resolved off-frame per scan.
data Approach = Approach
  { apTag   :: Text          -- ^ "approach k/N" (or the branch when untagged)
  , apInfo  :: ReviewInfo
  , apFiles :: [DiffFile]
  } deriving (Eq)

compareWidget
  :: forall t m . MonadWidget t m
  => Bool             -- ^ render diffs with the Monaco backend (the pref)
  -> FilePath         -- ^ the compare group's project dir
  -> Text             -- ^ the group's shared prompt
  -> Event t ()       -- ^ tab (re)selected — rescan, like the Plan pane
  -> m (Event t CompareEvents)
compareWidget useMonaco dir prompt selectE = divClass "gitlog compare" $ do
  pb <- getPostBuild
  (scanE, fireScan) <- newTriggerEvent
  let rescan = void . forkIO $ do
        ts <- queueList
        let grp = [ t | t <- ts, qtDir t == dir, qtPrompt t == prompt ]
        cols <- fmap catMaybes . forM grp $ \t -> case qtWorktree t of
          Nothing -> return Nothing
          Just wt -> scanReview wt >>= \case
            Left _   -> return Nothing   -- archived/removed worktree
            Right ri -> do
              fs <- reviewFiles ri
              let tg = if T.null (qtTag t) then riBranch ri else qtTag t
              return (Just (Approach tg ri fs))
        fireScan cols
  performEvent_ $ liftIO rescan <$ leftmost [pb, selectE]
  colsD <- holdDyn [] scanE

  -- The shared file selection: one path, driven from the strip (or a scan
  -- default), shown by EVERY column at once — that is the whole point.
  (fileClickE, fireFileClick) <- newTriggerEvent
  rec selFD <- holdDyn Nothing $ leftmost
        [ Just <$> fileClickE
        , ffor (attach (current selFD) scanE) $ \(msel, cols) ->
            let union = unionFiles cols
            in case msel of
                 Just s | s `elem` union -> Just s
                 _                       -> listToMaybe union ]

  -- Header.
  divClass "review-header" $ do
    elClass "span" "review-title" $ text "Compare approaches"
    elClass "span" "compare-prompt" $ text (shorten 90 prompt)
    elClass "span" "review-totals" . dynText $ ffor colsD $ \cols ->
        case length cols of
          0 -> "no approach worktrees (started yet?)"
          n -> T.pack (show n) <> " approaches"
    (refrEl, _) <- elAttr' "button" ("class" =: "review-btn") $ text "Refresh"
    performEvent_ $ liftIO rescan <$ domEvent Click refrEl

  -- Union-of-changed-files strip; clicking a chip selects that file in every
  -- column.  Re-rendered per scan (the strip is cheap), selection highlight
  -- stays live via selFD.
  dyn_ $ ffor colsD $ \cols -> divClass "compare-filestrip" $
    forM_ (unionFiles cols) $ \p -> do
      let k = length [ () | a <- cols, p `elem` map dfPath (apFiles a) ]
      (e, _) <- elDynAttr' "div"
          (ffor selFD $ \s -> "class" =:
             ("compare-file-chip" <> if s == Just p then " selected" else "")) $ do
        text (T.pack p)
        when (length cols > 1) . elClass "span" "compare-file-count" . text $
          " " <> T.pack (show k) <> "/" <> T.pack (show (length cols))
      performEvent_ $ ffor (domEvent Click e) $ \_ -> liftIO (fireFileClick p)

  -- The columns.  Re-rendered per scan; each column's diff tracks selFD.
  divClass "compare-columns" . dyn_ $ ffor colsD $ \cols ->
    if null cols
      then divClass "compare-empty" $
             text "No worktrees for this group yet — queued approaches get one\
                  \ when they start (see the Tasks pane)."
      else forM_ cols $ compareColumn useMonaco selFD
  return (never :: Event t CompareEvents)

-- | One approach: header (tag, branch, totals, Review) + the shared file's
-- diff (old = the approach's merge base, new = its working file — the same
-- semantics as the Review pane).
compareColumn
  :: MonadWidget t m
  => Bool -> Dynamic t (Maybe FilePath) -> Approach -> m ()
compareColumn useMonaco selFD (Approach tg ri fs) = divClass "compare-col" $ do
  divClass "compare-col-header" $ do
    elClass "span" "tasks-tag" $ text tg
    elClass "span" "compare-col-branch" $ text (riBranch ri)
    elClass "span" "review-totals" $ text totals
    -- The selected file untouched by THIS approach is a finding, not an
    -- error — say so instead of showing a silently empty diff.
    elClass "span" "compare-col-note" . dynText $ ffor selFD $ \case
        Just p | p `notElem` map dfPath fs -> "· unchanged here"
        _                                  -> ""
    (rvEl, _) <- elAttr' "button" ("class" =: "review-btn") $ text "Review"
    performEvent_ $ ffor (domEvent Click rvEl) $ \_ ->
        liftIO (requestReview (riRoot ri))
  (diffEl, _) <- elClass' "div" "compare-diff gitlog-diffview" blank
  (contentE, fireContent) <- newTriggerEvent
  pbc <- getPostBuild
  performEvent_ $ ffor (leftmost [updated selFD, tag (current selFD) pbc]) $ \case
      Just p  -> liftIO . void . forkIO $ do
          old <- gitShowFile (riRoot ri) (riMergeBase ri) p
          new <- either (\(_ :: SomeException) -> "") id
                   <$> try (T.readFile (riRoot ri </> p))
          fireContent (Just (T.pack p, old, new))
      Nothing -> liftIO (fireContent Nothing)
  performEvent_ $ ffor contentE $ \case
      Nothing -> liftJSM . void $
          jsg apiNs ^. js1 ("destroyDiff" :: Text) (_element_raw diffEl)
      Just (path, old, new) -> liftJSM . void $ do
          when useMonaco ensureMonacoLoaded
          jsg apiNs ^. js4 ("showDiff" :: Text) (_element_raw diffEl) path old new
  where
    apiNs :: Text
    apiNs = if useMonaco then "LeksahMonaco" else "LeksahCM"
    totals =
      let a = sum [ n | f <- fs, Just n <- [dfAdded f] ]
          d = sum [ n | f <- fs, Just n <- [dfDeleted f] ]
      in if null fs then "no changes" else
           T.pack (show (length fs)) <> " files  +" <> T.pack (show a)
             <> " \x2212" <> T.pack (show d)

-- | Every path changed in ANY approach, sorted (the file strip's order).
unionFiles :: [Approach] -> [FilePath]
unionFiles cols = sort (nub [ dfPath f | a <- cols, f <- apFiles a ])

shorten :: Int -> Text -> Text
shorten n t = if T.length t <= n then t else T.take (n - 1) t <> "…"

compareCss :: Css
compareCss = do
    ".compare-prompt" ? do
        "color"         -: "var(--leksah-fg-dim)"
        "font-size"     -: "12px"
        "overflow"      -: "hidden"
        "text-overflow" -: "ellipsis"
        "white-space"   -: "nowrap"
        "flex"          -: "1 1 auto"
        "min-width"     -: "0"
    ".compare-filestrip" ? do
        "display"       -: "flex"
        "flex-wrap"     -: "wrap"
        "gap"           -: "6px"
        "padding"       -: "5px 8px"
        "flex"          -: "0 0 auto"
        "border-bottom" -: "1px solid var(--leksah-border, #333)"
    ".compare-file-chip" ? do
        "padding"       -: "1px 8px"
        "border"        -: "1px solid var(--leksah-border-control)"
        "border-radius" -: "9px"
        "cursor"        -: "pointer"
        "font-size"     -: "12px"
        "white-space"   -: "nowrap"
    ".compare-file-chip:hover" ?
        ("background" -: "var(--leksah-hover)")
    ".compare-file-chip.selected" ? do
        "background" -: "var(--leksah-selection)"
        "color"      -: "var(--leksah-on-accent)"
    ".compare-file-count" ?
        ("color" -: "var(--leksah-fg-dim)")
    ".compare-file-chip.selected .compare-file-count" ?
        ("color" -: "var(--leksah-on-accent)")
    ".compare-columns" ? do
        "flex"       -: "1 1 0"
        "display"    -: "flex"
        "min-height" -: "0"
        "overflow-x" -: "auto"
    ".compare-col" ? do
        "flex"           -: "1 1 0"
        "min-width"      -: "360px"
        "display"        -: "flex"
        "flex-direction" -: "column"
        "min-height"     -: "0"
        "border-left"    -: "1px solid var(--leksah-border, #333)"
    ".compare-col:first-child" ? ("border-left" -: "none")
    ".compare-col-header" ? do
        "display"       -: "flex"
        "align-items"   -: "center"
        "gap"           -: "8px"
        "padding"       -: "4px 8px"
        "flex"          -: "0 0 auto"
        "border-bottom" -: "1px solid var(--leksah-border, #333)"
    ".compare-col-branch" ? do
        "overflow"      -: "hidden"
        "text-overflow" -: "ellipsis"
        "white-space"   -: "nowrap"
        "min-width"     -: "0"
        "flex"          -: "1 1 auto"
        "color"         -: "var(--leksah-fg-dim)"
        "font-size"     -: "12px"
    ".compare-col-note" ? do
        "color"       -: "#d29922"
        "font-size"   -: "12px"
        "white-space" -: "nowrap"
    ".compare .compare-diff" ? do
        "flex"       -: "1 1 0"
        "min-height" -: "0"
        "overflow"   -: "hidden"
    ".compare-empty" ? do
        "padding" -: "16px"
        "color"   -: "var(--leksah-fg-dim)"
