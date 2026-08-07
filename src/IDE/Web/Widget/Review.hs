{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
-- reflex-dom deprecates 'textInput'; leksah still uses it deliberately.
{-# OPTIONS_GHC -Wno-deprecations #-}
-- | The agent-change review pane (Claude plan Stage 2): everything a Claude
-- worktree changed, reviewed against its recorded base branch, with the
-- lifecycle actions (push + PR via @gh@, merge into the base, archive the
-- worktree) and Orca's best trick — comment on diff lines and send the note
-- straight into the worktree's Claude session as @\@file#La-Lb note@.
--
-- Opened from the git tree ("Review Changes…" \/ "Review Worktree…") through
-- the 'IDE.Web.Worktree.requestReview' bridge; works for any local checkout
-- (base = recorded @leksah-base@ → upstream → @origin\/HEAD@ → master\/main),
-- not only worktrees — merge\/archive just hide without a main checkout.
--
-- Diff mechanics (backends, CSS, splitters) are shared with the git log
-- viewer ("IDE.Web.Widget.GitLog"); the diff is committed + uncommitted work
-- vs the merge base, with the WORKING file on the right — what would land if
-- merged now.
module IDE.Web.Widget.Review
  ( reviewCss
  , reviewWidget
  , reviewFiles
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Lens ((^.), (.~))
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)

import Data.Default (def)
import Data.Function ((&))
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)

import System.Exit (ExitCode(..))
import System.FilePath ((</>), dropTrailingPathSeparator)
import System.Process (readProcessWithExitCode)

import Clay ((?), (-:), Css)

import Language.Javascript.JSaddle (jsg, js1, js4, liftJSM, eval, valToText)

import Reflex
       (Dynamic, Event, holdDyn, getPostBuild, performEvent_, newTriggerEvent,
        ffor, leftmost, tag, current, updated, switchDyn, never, constDyn,
        simpleList, attach)
import Reflex.Dom.Core
       (MonadWidget, divClass, elClass, elClass', elAttr', elDynAttr', dynText,
        dyn_, text, domEvent, EventName(..), _element_raw, blank, (=:),
        textInput, _textInput_value, attributes)

import IDE.App (appWorkspace, withApp)
import IDE.Git (runGitBatch)
import IDE.Web.Claude (claudeKeyFor)
import IDE.Web.Events (ReviewEvents)
import IDE.Web.GitInfo (openUrl)
import IDE.Web.ReplTmux (liveRunPanes, sendKeysTo)
import IDE.Web.Widget.Editor (ensureMonacoLoaded)
import IDE.Web.Widget.FileTree (GitStatus(..), gitClass, gitBadge)
import IDE.Web.Widget.GitLog
       (DiffFile(..), parseNameStatus, parseNumstat, mergeFiles, gitShowFile,
        gitLogDivider)
import IDE.Web.Worktree
       (ReviewInfo(..), scanReview, worktreeMerge, worktreePushPR,
        worktreeArchive)
import IDE.Workspace (workspaceRemoveProject)
import IDE.Ws.Types (ProjectKey(..))

--------------------------------------------------------------------------------
-- Widget

reviewWidget
  :: forall t m . MonadWidget t m
  => Bool             -- ^ render diffs with the Monaco backend (the pref)
  -> FilePath -> m (Event t ReviewEvents)
reviewWidget useMonaco dir = divClass "gitlog review" $ do
  pb <- getPostBuild
  -- The selection-reading helper for the comment box (idempotent global).
  performEvent_ $ ffor pb $ \_ -> liftJSM . void $ eval reviewJs
  -- Resolve the review target + file list off-frame; Refresh rescans.
  (infoE,  fireInfo)  <- newTriggerEvent
  (filesE, fireFiles) <- newTriggerEvent
  (statusE, fireStatus) <- newTriggerEvent
  let rescan = void . forkIO $ scanReview dir >>= \case
        Left err -> fireInfo (Left err) >> fireFiles []
        Right ri -> do
          fireInfo (Right ri)
          reviewFiles ri >>= fireFiles
  performEvent_ $ liftIO rescan <$ pb
  infoD  <- holdDyn (Left "scanning…") infoE
  filesD <- holdDyn [] filesE
  statusD <- holdDyn "" statusE

  -- Header: what is reviewed, totals, lifecycle actions, status line.
  divClass "review-header" $ do
    elClass "span" "review-title" . dynText $ ffor infoD $ \case
        Left err -> err
        Right ri -> riBranch ri <> " → " <> riBase ri
    elClass "span" "review-totals" . dynText $ ffor filesD $ \fs ->
        let a = sum [ n | f <- fs, Just n <- [dfAdded f] ]
            d = sum [ n | f <- fs, Just n <- [dfDeleted f] ]
        in if null fs then "no changes" else
             T.pack (show (length fs)) <> " files  +" <> T.pack (show a)
               <> " \x2212" <> T.pack (show d)
    elClass "span" "review-status" $ dynText statusD
    divClass "review-actions" $ do
      let act name run = do
            (e, _) <- elAttr' "button" ("class" =: "review-btn") $ text name
            performEvent_ $ ffor (tag (current infoD) (domEvent Click e)) $ \case
                Left _   -> return ()
                Right ri -> liftIO . void . forkIO $ run ri
            return ()
      act "Push & PR" $ \ri -> do
          fireStatus "pushing…"
          worktreePushPR ri >>= \case
            Left err  -> fireStatus err
            Right url -> fireStatus ("PR: " <> url) >> openUrl url
      -- Merge / Archive only make sense for a linked worktree.
      dyn_ $ ffor infoD $ \case
        Right ri | Just _ <- riMainRoot ri -> do
          act ("Merge into " <> riBase ri) $ \ri' -> do
              fireStatus "merging…"
              worktreeMerge ri' >>= \case
                Left err  -> fireStatus err
                Right msg -> fireStatus msg
          -- Archive is destructive: first click arms, second performs.
          rec armedD <- holdDyn False (True <$ domEvent Click archEl)
              (archEl, _) <- elDynAttr' "button"
                  (ffor armedD $ \armed -> "class" =:
                     ("review-btn" <> if armed then " review-btn-danger" else "")) $
                  dynText (ffor armedD $ \armed ->
                     if armed then "Archive — really?" else "Archive Worktree")
          performEvent_ $ ffor (tag ((,) <$> current armedD <*> current infoD)
                                    (domEvent Click archEl)) $ \case
              (True, Right ri') -> liftIO . void . forkIO $ do
                  fireStatus "archiving…"
                  worktreeArchive ri' >>= \case
                    Left err -> fireStatus err
                    Right () -> do
                      -- Drop the (now deleted) worktree project from the
                      -- workspace; the tab itself closes with ⌘W.
                      withApp $ \app -> workspaceRemoveProject
                        (appWorkspace app) (ProjectKey "dir" (riRoot ri') Nothing)
                      fireStatus "archived — worktree removed; close this tab"
              _ -> return ()
        _ -> return ()
      (refrEl, _) <- elAttr' "button" ("class" =: "review-btn") $ text "Refresh"
      performEvent_ $ liftIO rescan <$ domEvent Click refrEl

  -- Changed files (left) — selecting one loads its diff below.
  rec
    fileClickE <- divClass "review-files gitlog-files" $ do
        evsD <- simpleList (sortOn dfPath <$> filesD) (reviewFileRow selFD)
        return $ switchDyn (leftmost <$> evsD)
    selFD <- holdDyn Nothing $ leftmost
        [ Just <$> fileClickE
          -- Keep the selection across a rescan when the file is still there;
          -- drop it (clearing the diff) when it isn't.
        , ffor (attach (current selFD) filesE) $ \(msel, fs) ->
            msel >>= \s -> if any ((== dfPath s) . dfPath) fs then Just s else Nothing
        ]
  gitLogDivider "h"

  -- Comment bar: selected diff lines + note → the worktree's Claude session.
  (commentTi, sendClickE) <- divClass "review-comment" $ do
      ti <- textInput $ def
        & attributes .~ constDyn
            ("placeholder" =: "comment on the selected diff lines — sent to \
                              \this worktree's Claude session as @file#Lx-Ly note"
             <> "style" =: "flex:1 1 auto;min-width:0")
      (sendEl, _) <- elAttr' "button" ("class" =: "review-btn") $ text "Send to Claude"
      return (ti, domEvent Click sendEl)

  -- The diff itself (full width, bottom).  Old = merge base, new = WORKING file.
  (diffEl, _) <- elClass' "div" "review-diffview gitlog-diffview" blank
  let apiNs :: Text
      apiNs = if useMonaco then "LeksahMonaco" else "LeksahCM"
  (contentE, fireContent) <- newTriggerEvent
  performEvent_ $ ffor (updated ((,) <$> infoD <*> selFD)) $ \case
      (Right ri, Just df) -> liftIO . void . forkIO $ do
          old <- gitShowFile (riRoot ri) (riMergeBase ri) (dfPath df)
          new <- either (\(_ :: SomeException) -> "") id
                   <$> try (T.readFile (riRoot ri </> dfPath df))
          fireContent (Just (T.pack (dfPath df), old, new))
      _ -> liftIO (fireContent Nothing)
  performEvent_ $ ffor contentE $ \case
      Nothing -> liftJSM . void $
          jsg apiNs ^. js1 ("destroyDiff" :: Text) (_element_raw diffEl)
      Just (path, old, new) -> liftJSM . void $ do
          when useMonaco ensureMonacoLoaded
          jsg apiNs ^. js4 ("showDiff" :: Text) (_element_raw diffEl) path old new

  -- Send: read the selected line range from the mounted diff, compose the
  -- reference, type it into the session's pane.
  performEvent_ $ ffor (tag ((,,) <$> current infoD <*> current selFD
                                  <*> current (_textInput_value commentTi))
                            sendClickE) $ \(einfo, msel, note) ->
    case (einfo, msel) of
      (Right ri, Just df) | not (T.null (T.strip note)) -> do
          lns <- liftJSM $ valToText =<< jsg ("LeksahReview" :: Text)
                   ^. js1 ("selLines" :: Text) (_element_raw diffEl)
          let suffix = case T.words lns of
                [a, b] | a == b    -> "#L" <> a
                       | otherwise -> "#L" <> a <> "-L" <> b
                _                  -> ""
              msg = "@" <> T.pack (dfPath df) <> suffix <> " " <> T.strip note <> " "
          liftIO . void . forkIO $
            sendToClaude (riRoot ri) msg >>= fireStatus
      (Left err, _)  -> liftIO (fireStatus err)
      (_, Nothing)   -> liftIO (fireStatus "select a changed file first")
      _              -> liftIO (fireStatus "type a comment first")

  return (never :: Event t ReviewEvents)

-- | One changed-file row (gitlog CSS + the tree's status colours/badges).
reviewFileRow
  :: MonadWidget t m
  => Dynamic t (Maybe DiffFile)
  -> Dynamic t DiffFile
  -> m (Event t DiffFile)
reviewFileRow selFileD dfD = do
  let selD = (\s df -> s == Just df) <$> selFileD <*> dfD
      attrsD = ffor selD $ \sel ->
          "class" =: ("gitlog-file leksah-nav-item" <> if sel then " selected" else "")
  (e, _) <- elDynAttr' "div" attrsD . dyn_ $ ffor dfD $ \df -> do
      let cls = gitClass (dfStatus df)
      elClass "span" ("git-change-badge " <> cls) $ text (gitBadge (dfStatus df))
      text " "
      elClass "span" ("gitlog-file-path git-name " <> cls) $ text (T.pack (dfPath df))
      elClass "span" "gitlog-file-counts" $ do
          elClass "span" "gitlog-added"   $ text (maybe "" (\n -> "+" <> T.pack (show n)) (dfAdded df))
          text " "
          elClass "span" "gitlog-deleted" $ text (maybe "" (\n -> "\x2212" <> T.pack (show n)) (dfDeleted df))
  return $ tag (current dfD) (domEvent Click e)

--------------------------------------------------------------------------------
-- IO

-- | Everything changed vs the merge base — committed AND working tree
-- (@git diff \<merge-base\>@), plus untracked files.
reviewFiles :: ReviewInfo -> IO [DiffFile]
reviewFiles ri = do
  r <- try (runGitBatch (riRoot ri)
        [ ["-c", "core.quotePath=false", "diff", "--name-status", riMergeBase ri]
        , ["-c", "core.quotePath=false", "diff", "--numstat",     riMergeBase ri]
        , ["-c", "core.quotePath=false", "status", "--porcelain", "-uall"] ])
  return $ case r :: Either SomeException [(ExitCode, Text, Text)] of
    Right [(ExitSuccess, nameOut, _), (_, numOut, _), (_, stOut, _)] ->
        let tracked = mergeFiles (parseNameStatus nameOut) (parseNumstat numOut)
            untracked = [ DiffFile (T.unpack p) Untracked Nothing Nothing
                        | l <- T.lines stOut
                        , Just p <- [T.stripPrefix "?? " l] ]
        in tracked ++ untracked
    _ -> []

-- | Type @txt@ into the most-recently-used live Claude pane for this checkout
-- (the same run-key match 'IDE.Web.Claude.activateMruClaude' uses).  Returns a
-- status-line message either way.
sendToClaude :: FilePath -> Text -> IO Text
sendToClaude root txt = do
  let base = T.pack (dropTrailingPathSeparator root) <> "#claude"
  panes <- filter (\(k, _, _, _) -> claudeKeyFor base k) <$> liveRunPanes
  case panes of
    [] -> return "no Claude session is running in this checkout — start one from its Claude node"
    ((_, _, _, pid) : _) -> do
      ok <- sendKeysTo pid ["-l", T.unpack txt]
      return $ if ok then "sent to the Claude session"
                     else "send failed (tmux send-keys)"

-- | @window.LeksahReview.selLines(diffEl)@ → @\"a b\"@ (1-based start\/end lines
-- of the selection on the NEW side of the mounted diff) or @\"\"@.  Handles both
-- backends via the handles they stash on the container.
reviewJs :: Text
reviewJs = T.unlines
  [ "if (!window.LeksahReview) window.LeksahReview = { selLines: function(el){"
  , "  if (!el) return '';"
  , "  var d = el.__leksahDiff;"                       -- CodeMirror MergeView
  , "  if (d && d.b) {"
  , "    var s = d.b.state.selection.main;"
  , "    if (s.from === 0 && s.to === 0) return '';"
  , "    var a = d.b.state.doc.lineAt(s.from).number;"
  , "    var b = d.b.state.doc.lineAt(s.to).number;"
  , "    return a + ' ' + b;"
  , "  }"
  , "  var m = el.__leksahMonacoDiff;"                 -- Monaco DiffEditor
  , "  if (m) {"
  , "    var sel = m.diffEditor.getModifiedEditor().getSelection();"
  , "    if (!sel) return '';"
  , "    return sel.startLineNumber + ' ' + sel.endLineNumber;"
  , "  }"
  , "  return '';"
  , "} };"
  ]

--------------------------------------------------------------------------------
-- CSS (the pane reuses .gitlog's layout + file-row styles; only the header,
-- comment bar and buttons are new)

reviewCss :: Css
reviewCss = do
    ".review-header" ? do
        "display"     -: "flex"
        "align-items" -: "center"
        "gap"         -: "10px"
        "flex"        -: "0 0 auto"
        "padding"     -: "4px 8px"
        "border-bottom" -: "1px solid var(--leksah-border, #333)"
    ".review-title"  ? do
        "font-weight" -: "bold"
        "white-space" -: "nowrap"
    ".review-totals" ? do
        "color"       -: "var(--leksah-fg-dim)"
        "white-space" -: "nowrap"
    ".review-status" ? do
        "flex"          -: "1 1 auto"
        "color"         -: "var(--leksah-fg-dim)"
        "font-size"     -: "12px"
        "overflow"      -: "hidden"
        "text-overflow" -: "ellipsis"
        "white-space"   -: "nowrap"
        "min-width"     -: "0"
    ".review-actions" ? do
        "display" -: "flex"
        "gap"     -: "6px"
        "flex"    -: "0 0 auto"
    ".review-btn" ? do
        "padding" -: "2px 10px"
    ".review-btn-danger" ? do
        "background" -: "#c0392b"
        "color"      -: "#fff"
    ".review .review-files" ? do
        "flex"     -: "0 0 30%"
        "overflow" -: "auto"
    ".review-comment" ? do
        "display"     -: "flex"
        "gap"         -: "6px"
        "flex"        -: "0 0 auto"
        "padding"     -: "4px 8px"
        "align-items" -: "center"
        "border-top"    -: "1px solid var(--leksah-border, #333)"
        "border-bottom" -: "1px solid var(--leksah-border, #333)"
    ".review .review-diffview" ? do
        "flex"       -: "1 1 0"
        "min-height" -: "0"
        "overflow"   -: "hidden"
