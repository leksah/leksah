{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Toolbar
  ( toolbarCss
  , toolbarWidget
  ) where

import Data.Bool (bool)

import Clay
       (transitionDuration, sec, transitionDelay,
        opacity, (|+), visibility, absolute, position, inlineBlock,
        display, nowrap, whiteSpace, vGradient, backgroundImage,
        borderRadius, padding, hover, (#), background, margin, px, width,
        height, (?), Css, hidden, visible, Color(..))

import Reflex (constDyn, holdUniqDyn, leftmost, Dynamic, holdUniqDyn)
import Reflex.Dom.Core
       (elDynAttr', text, MonadWidget, (=:), elAttr, divClass,
        Event, domEvent, EventName(..))

import IDE.Core.State (IDE)
import IDE.Web.Events (ToolbarEvents(..))
import IDE.Web.Command (commandImageAndTip, Command(..)
  , commandAddModule, commandRefreshNix, commandPackageClean
  , commandPackageBuild, commandPackageRun, commandPackageRunJavascript
  , commandToggleBackgroundBuild, commandToggleNative, commandToggleJavaScript
  , commandToggleDebug, commandToggleMakeDocs, commandToggleTest
  , commandToggleRunBenchmarks, commandToggleMakeDependents
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

toolbarWidget
  :: MonadWidget t m
  => Dynamic t IDE
  -> m (Event t ToolbarEvents)
toolbarWidget ide =
  divClass "toolbar" $
    leftmost <$> mapM (toolbarButton ide)
      [ commandAddModule
      , CommandFileOpen
      , CommandFileSave
      , CommandUndo
      , CommandRedo
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
      , commandUpdateWorkspaceInfo
      , commandDebugStep
      , commandDebugStepLocal
      , commandDebugStepModule
      , commandDebugContinue
      ]
