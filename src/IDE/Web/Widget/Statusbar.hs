{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
-- 'stateLabel' needs a catch-all for the gtk build's extra IDEState constructor
-- (IsCompleting), which the nogtk stub lacks — so that same catch-all reads as
-- redundant in the nogtk build.  Silence the (nogtk-only) overlap here.
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module IDE.Web.Widget.Statusbar
  ( statusbarCss
  , statusbarWidget
  ) where

import Control.Lens (view, (^.))
import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M (toList)
import Data.Text (Text)
import qualified Data.Text as T (intercalate, pack)
import System.FilePath (takeFileName, dropTrailingPathSeparator)

import Clay
       (vGradient, backgroundImage, whiteSpace, nowrap, fontSize, px,
        color, paddingRight, padding, inlineBlock, cursor, pointer,
        textDecoration, underline, (?), (-:), Css)
import qualified Clay (display)

import Reflex
       (holdUniqDyn, holdDyn, never, Dynamic, Event, current, updated, tag,
        leftmost, ffor)
import Reflex.Dom.Core
       (MonadWidget, divClass, dynText, dyn_, getPostBuild, performEvent_,
        newTriggerEvent, domEvent, EventName(Click), elClass', elClass,
        elAttr, (=:), text)

import IDE.Core.CTypes (packageIdentifierToString)
import IDE.Core.State
       (IDE, IDEState(..), currentState, activeProject, activePack,
        activeComponent, activeProjectLogRefs, logRefType, LogRefType(..),
        ipdPackageId, pjDir, pjKey)
import IDE.Web.GitInfo
       (ActiveGitInfo(..), scanActiveGitInfo, emptyGitInfo, openUrl)
import IDE.Web.LocalRefresh (registerLocalRefresh)
import IDE.Web.Theme (barTopColor, barBottomColor, fgMutedColor)
import IDE.Web.Events (StatusbarEvents)

statusbarCss :: Css
statusbarCss = do
    ".statusbar" ? do
        backgroundImage (vGradient barTopColor barBottomColor)
        whiteSpace nowrap
        fontSize (px 12)
        color fgMutedColor
        padding (px 2) (px 8) (px 2) (px 8)
    ".statusbar .sb-section" ? do
        Clay.display inlineBlock
        paddingRight (px 16)
    ".statusbar .sb-pr" ? do
        cursor pointer
        textDecoration underline
    -- Severity counts: an icon (error/warning/hint) followed by its number,
    -- replacing the old "Errors:/Warnings:/Hints:" text labels.
    ".statusbar .sb-count" ? do
        Clay.display inlineBlock
        paddingRight (px 10)
    ".statusbar .sb-count-icon" ? do
        "height" -: "13px"
        "width" -: "13px"
        "vertical-align" -: "text-bottom"
        "margin-right" -: "3px"
    -- Change totals: additions green, deletions red.
    ".statusbar .sb-add" ? ("color" -: "#3fb950")
    ".statusbar .sb-del" ? ("color" -: "#f85149")

-- The active project (its directory name), then — only when the project has
-- an active package — the package and component.  A package-less project (e.g.
-- a Rust crate) shows just its name, never "No active package".
activeLabel :: IDE -> Text
activeLabel ide = case ide ^. activeProject of
  Nothing   -> ""
  Just proj ->
      T.pack (takeFileName (dropTrailingPathSeparator (pjDir (pjKey proj))))
        <> case ide ^. activePack of
             Nothing  -> ""
             Just pkg -> "  \x203A  " <> packageIdentifierToString (ipdPackageId pkg)
                       <> maybe "" (\c -> " (" <> c <> ")") (ide ^. activeComponent)

-- | Remote-activity blurb: @⇅ host (n)@ for each host with ssh ops in flight
-- (build, grep, git, HLS start, file read), empty when the link is quiet.
-- Fed from 'IDE.Utils.RemoteExec.remoteInFlight'.
remoteActivity :: Map Text Int -> Text
remoteActivity m = case [ (h, n) | (h, n) <- M.toList m, n > 0 ] of
    [] -> ""
    xs -> "⇅ " <> T.intercalate "  " [ h <> " (" <> T.pack (show n) <> ")" | (h, n) <- xs ]

stateLabel :: IDEState -> Text
stateLabel = \case
  IsStartingUp   -> "Starting up…"
  IsRunning      -> "Ready"
  IsShuttingDown -> "Shutting down…"
  -- IDEState has an extra constructor in the gtk front end (IsCompleting) but
  -- not in the nogtk stub, so this fallthrough is required for exhaustiveness
  -- there while looking redundant here — see the -Wno-overlapping-patterns note.
  _              -> ""

statusbarWidget
  :: MonadWidget t m
  => Dynamic t IDE
  -> Dynamic t (Map Text Int)  -- ^ in-flight remote (ssh) ops per host
  -> m (Event t StatusbarEvents)
statusbarWidget ide remoteActD = divClass "statusbar" $ do
  (divClass "sb-section sb-package" . dynText) =<< holdUniqDyn (activeLabel <$> ide)
  gitStatusSection ide
  countsD <- holdUniqDyn (counts . toList . activeProjectLogRefs <$> ide)
  countsSection countsD
  (divClass "sb-section sb-state"   . dynText) =<< holdUniqDyn (stateLabel . view currentState <$> ide)
  (divClass "sb-section sb-remote"  . dynText) =<< holdUniqDyn (remoteActivity <$> remoteActD)
  return never
  where
    counts refs =
      let n p = length (filter p refs)
          errs  = n (\r -> logRefType r `elem` [ErrorRef, TestFailureRef])
          warns = n ((== WarningRef) . logRefType)
          lints = n ((== LintRef) . logRefType)
      in (errs, warns, lints)

-- | Error / warning / hint counts as severity-icon + number (no text labels).
-- Errors and warnings always show; hints only when non-zero.
countsSection :: MonadWidget t m => Dynamic t (Int, Int, Int) -> m ()
countsSection countsD = divClass "sb-section sb-counts" $ do
    sevCount "/pics/sev-error.svg"   =<< holdUniqDyn ((\(e,_,_) -> e) <$> countsD)
    sevCount "/pics/sev-warning.svg" =<< holdUniqDyn ((\(_,w,_) -> w) <$> countsD)
    hintsD <- holdUniqDyn ((\(_,_,l) -> l) <$> countsD)
    dyn_ $ ffor ((> 0) <$> hintsD) $ \present ->
        when present $ sevCount "/pics/sev-hint.svg" hintsD
  where
    sevCount src nD = elClass "span" "sb-count" $ do
        elAttr "img" ("class" =: "sb-count-icon" <> "src" =: src) (return ())
        dynText (T.pack . show <$> nD)

-- | Git summary for the active project: current branch, open PR number
-- (clickable → opens the PR), and change totals — uncommitted working-tree
-- @+/-@ and, separately, commits-ahead-of-upstream @↑ +/-@.  Re-scans when the
-- active project changes and on any local filesystem refresh; hidden when the
-- active project is not a git checkout.
gitStatusSection :: MonadWidget t m => Dynamic t IDE -> m ()
gitStatusSection ide = do
    activeDirD <- holdUniqDyn $ fmap (pjDir . pjKey) . view activeProject <$> ide
    (giE, fireGi)          <- newTriggerEvent
    (refreshE, fireRefresh) <- newTriggerEvent
    liftIO . void $ registerLocalRefresh (const (fireRefresh ()))
    pb <- getPostBuild
    let scanE = leftmost [() <$ updated activeDirD, refreshE, pb]
    performEvent_ $ ffor (tag (current activeDirD) scanE) $ \case
        Nothing  -> liftIO (fireGi emptyGitInfo)
        Just dir -> liftIO . void . forkIO $ scanActiveGitInfo dir >>= fireGi
    giD <- holdDyn emptyGitInfo giE
    divClass "sb-section sb-git" $ do
        dynText $ maybe "" ("\x2387 " <>) . agiBranch <$> giD
        dyn_ $ ffor (agiPr <$> giD) $ \case
            Nothing       -> return ()
            Just (n, url) -> do
                (e, _) <- elClass' "span" "sb-pr" $ text ("  #" <> T.pack (show n))
                performEvent_ $ liftIO (openUrl url) <$ domEvent Click e
        changesPart ""       (agiWork  <$> giD)
        changesPart "\x2191" (agiAhead <$> giD)
  where
    -- '+N' additions in green, '−N' deletions in red; nothing when both zero.
    -- @pre@ is a leading marker (e.g. ↑ for commits-ahead-of-upstream).
    changesPart pre adD = do
      d <- holdUniqDyn adD
      dyn_ $ ffor d $ \(a, del) ->
        when (a /= 0 || del /= 0) $ do
          text ("  " <> pre)
          elClass "span" "sb-add" $ text ("+" <> tshow a)
          text " "
          elClass "span" "sb-del" $ text ("\x2212" <> tshow del)
    tshow = T.pack . show
