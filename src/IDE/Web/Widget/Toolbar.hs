{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Toolbar
  ( toolbarCss
  , toolbarWidget
  ) where

import Control.Lens (view, to)

import Data.Bool (bool)
import Data.Text (Text)

import Clay
       (transitionDuration, sec, transitionDelay,
        opacity, (|+), visibility, absolute, position, inlineBlock,
        display, nowrap, whiteSpace, vGradient, backgroundImage,
        borderRadius, padding, hover, (#), background, margin, px, width,
        height, (?), Css, hidden, visible, Color(..))

import Reflex (constDyn, holdUniqDyn, leftmost, ffor, Dynamic)
import Reflex.Dom.Core
       (elDynAttr', elDynAttr, text, dynText, MonadWidget, (=:), elAttr, divClass,
        Event, domEvent, EventName(..))

import IDE.Core.State (IDE, prefs, tallVisibility, wide1Visibility, TallVisibility(..))
import IDE.Web.Events (ToolbarEvents(..))
import IDE.Web.Command (commandImageAndTip, commandToggleTallPane
  , commandToggleWide1Pane, Command(..)
  , commandAddModule, commandRefreshNix, commandPackageClean
  , commandPackageBuild, commandPackageRun, commandPackageRunJavascript
  , commandToggleBackgroundBuild, commandToggleNative, commandToggleJavaScript
  , commandToggleDebug, commandToggleMakeDocs, commandToggleTest
  , commandToggleRunBenchmarks, commandToggleMakeDependents
  , commandToggleShowIgnored, commandToggleShowHidden
  , commandUpdateWorkspaceInfo, commandDebugStep, commandDebugStepLocal
  , commandDebugStepModule, commandDebugContinue, commandGetToggleState)

toolbarCss :: Css
toolbarCss = do
    ".toolbar" ? do
        backgroundImage (vGradient (Rgba 32 32 32 1.0) (Rgba 16 16 16 1.0))
        whiteSpace nowrap
    ".toolbar-item" ?
        display inlineBlock
    ".tooltip" ? do
        position absolute
        padding (px 3) (px 3) (px 3) (px 3)
        borderRadius (px 3) (px 3) (px 3) (px 3)
        background (Rgba 64 64 64 1.0)
        visibility hidden
        opacity 0
        transitionDelay (sec 0)
        transitionDuration (sec 0)
    ".toolbar-button" ? do
        height (px 22)
        width (px 22)
        borderRadius (px 3) (px 3) (px 3) (px 3)
        padding (px 3) (px 3) (px 3) (px 3)
        margin (px 0) (px 0) (px 0) (px 0)
    ".toolbar-button" # hover ?
        background (Rgba 61 96 150 1.0)
    ".toolbar-button" # hover |+ ".tooltip" ? do
        visibility visible
        opacity 1
        transitionDelay (sec 1)
        transitionDuration (sec 0.2)
    ".toggled .toolbar-button" ?
        background (Rgba 30 88 209 1.0)
    ".toggled .toolbar-button" # hover ?
        background (Rgba 61 96 150 1.0)
    -- The 3-state side-pane button: shown = solid, auto-hide = dim, hidden = none.
    ".tall-state-show .toolbar-button" ?
        background (Rgba 30 88 209 1.0)
    ".tall-state-auto .toolbar-button" ?
        background (Rgba 30 88 209 0.45)
    -- The 3-state bottom-pane button: shown = solid, auto-hide = dim, hidden = none.
    ".wide1-state-show .toolbar-button" ?
        background (Rgba 30 88 209 1.0)
    ".wide1-state-auto .toolbar-button" ?
        background (Rgba 30 88 209 0.45)

toolbarButton
  :: MonadWidget t m
  => Dynamic t IDE
  -> Command
  -> m (Event t ToolbarEvents)
toolbarButton ide cmd = do
  let (src, tip) = commandImageAndTip cmd
  attrD <- case commandGetToggleState cmd of
    Just f -> fmap (("class" =:) . ("toolbar-item" <>) . bool "" " toggled") <$>
                holdUniqDyn (f <$> ide)
    Nothing -> return $ constDyn ("class" =: "toolbar-item")
  (e, _) <- elDynAttr' "div" attrD $ do
    elAttr "img" ("src" =: src <> "class" =: "toolbar-button") $ return ()
    divClass "tooltip" $ text tip
  return $ ToolbarCommand cmd <$ domEvent Click e

-- | The side ("tall") pane visibility button: one icon, cycling
-- show -> auto-hide -> hide, with the current state shown by its class/tooltip.
tallToggleButton
  :: MonadWidget t m
  => Dynamic t IDE
  -> m (Event t ToolbarEvents)
tallToggleButton ide = do
  visD <- holdUniqDyn $ view (prefs . to tallVisibility) <$> ide
  let (src, _) = commandImageAndTip commandToggleTallPane
      attrD = ffor visD $ \v -> "class" =: ("toolbar-item " <> stateClass v)
  (e, _) <- elDynAttr' "div" attrD $ do
    elAttr "img" ("src" =: src <> "class" =: "toolbar-button") $ return ()
    divClass "tooltip" $ dynText (tip <$> visD)
  return $ ToolbarCommand commandToggleTallPane <$ domEvent Click e
  where
    stateClass :: TallVisibility -> Text
    stateClass TallShow     = "tall-state-show"
    stateClass TallAutoHide = "tall-state-auto"
    stateClass TallHide     = "tall-state-hide"
    tip :: TallVisibility -> Text
    tip TallShow     = "Side pane: shown — click to auto-hide"
    tip TallAutoHide = "Side pane: auto-hide — click to hide"
    tip TallHide     = "Side pane: hidden — click to show"

-- | The bottom ("wide1") pane visibility button: one icon cycling
-- show -> auto-hide -> hide, mirroring 'tallToggleButton' for the bottom row.
wide1ToggleButton
  :: MonadWidget t m
  => Dynamic t IDE
  -> m (Event t ToolbarEvents)
wide1ToggleButton ide = do
  visD <- holdUniqDyn $ view (prefs . to wide1Visibility) <$> ide
  let (src, _) = commandImageAndTip commandToggleWide1Pane
      attrD = ffor visD $ \v -> "class" =: ("toolbar-item " <> stateClass v)
  (e, _) <- elDynAttr' "div" attrD $ do
    elAttr "img" ("src" =: src <> "class" =: "toolbar-button") $ return ()
    divClass "tooltip" $ dynText (tip <$> visD)
  return $ ToolbarCommand commandToggleWide1Pane <$ domEvent Click e
  where
    stateClass :: TallVisibility -> Text
    stateClass TallShow     = "wide1-state-show"
    stateClass TallAutoHide = "wide1-state-auto"
    stateClass TallHide     = "wide1-state-hide"
    tip :: TallVisibility -> Text
    tip TallShow     = "Bottom pane: shown — click to auto-hide"
    tip TallAutoHide = "Bottom pane: auto-hide — click to hide"
    tip TallHide     = "Bottom pane: hidden — click to show"

toolbarWidget
  :: MonadWidget t m
  => Dynamic t IDE
  -> m (Event t ToolbarEvents)
toolbarWidget ide =
  divClass "toolbar" $ do
    tallE  <- tallToggleButton ide
    wide1E <- wide1ToggleButton ide
    rest   <- mapM (toolbarButton ide)
      [ commandAddModule
      , CommandFileOpen
      , CommandFileSave
      , CommandFind
      , commandRefreshNix
      , commandPackageClean
      , commandPackageBuild
      , commandPackageRun
      , commandPackageRunJavascript
      , CommandNextError
      , CommandPreviousError
      , commandToggleBackgroundBuild
      , commandToggleNative
      , commandToggleJavaScript
      , commandToggleDebug
      , commandToggleMakeDocs
      , commandToggleTest
      , commandToggleRunBenchmarks
      , commandToggleMakeDependents
      , commandToggleShowIgnored
      , commandToggleShowHidden
      , commandUpdateWorkspaceInfo
      , commandDebugStep
      , commandDebugStepLocal
      , commandDebugStepModule
      , commandDebugContinue
      ]
    return $ leftmost (tallE : wide1E : rest)
