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

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M (toList)
import Data.Text (Text)
import qualified Data.Text as T (intercalate, pack)

import Clay
       (vGradient, backgroundImage, whiteSpace, nowrap, fontSize, px,
        color, paddingRight, padding, inlineBlock, (?), Css, Color(..))
import qualified Clay (display)

import Reflex (holdUniqDyn, never, Dynamic, Event)
import Reflex.Dom.Core (MonadWidget, divClass, dynText)

import IDE.Core.CTypes (packageIdentifierToString)
import IDE.Core.State
       (IDE, IDEState(..), currentState, activePack, activeComponent,
        allLogRefs, logRefType, LogRefType(..), ipdPackageId)
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

-- The active package (and component, if set).
activeLabel :: IDE -> Text
activeLabel ide = case ide ^. activePack of
  Nothing  -> "No active package"
  Just pkg -> packageIdentifierToString (ipdPackageId pkg)
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
  (divClass "sb-section sb-counts"  . dynText) =<< holdUniqDyn (counts . toList . view allLogRefs <$> ide)
  (divClass "sb-section sb-state"   . dynText) =<< holdUniqDyn (stateLabel . view currentState <$> ide)
  (divClass "sb-section sb-remote"  . dynText) =<< holdUniqDyn (remoteActivity <$> remoteActD)
  return never
  where
    counts refs =
      let n p = length (filter p refs)
          tshow = T.pack . show
          errs  = n (\r -> logRefType r `elem` [ErrorRef, TestFailureRef])
          warns = n ((== WarningRef) . logRefType)
          lints = n ((== LintRef) . logRefType)
      in "Errors: " <> tshow errs <> "  Warnings: " <> tshow warns
         <> (if lints > 0 then "  Hints: " <> tshow lints else "")
