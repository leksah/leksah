{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
module IDE.Web.Widget.Toolbar
  ( toolbarCss
  , toolbarWidget
  ) where

import Control.Monad (void)
import Data.Bool (bool)
import Data.Text (Text)
import Language.Javascript.JSaddle (liftJSM, eval, valToText)

import Clay
       (transitionDuration, sec, transitionDelay,
        opacity, (|+), (|>), visibility, absolute, position, inlineBlock,
        display, nowrap, whiteSpace, (-:), overflow,
        borderRadius, padding, hover, (#), background, margin, px, width,
        height, (?), Css, hidden, visible, zIndex, Color(..))

import Reflex
       (constDyn, current, getPostBuild, holdDyn, holdUniqDyn, leftmost,
        ffor, tag, Dynamic)
import Reflex.Dom.Core
       (elDynAttr, elDynAttr', text, dynText, (=:), elAttr, divClass,
        Event, domEvent, EventName(..))

import IDE.Web.Theme (selectionColor, selectionColorFaint, dimOpacity, surfaceHiColor, accentHoverColor)
import IDE.Web.Ctx (Ctx(..))
import IDE.Web.Model (TallVisibility(..))
import IDE.Web.Events (ToolbarEvents(..))
import IDE.Web.Command (commandImageAndTip, commandToggleTallPane
  , commandToggleWide1Pane, Command(..)
  , commandAddModule, commandRefreshNix, commandPackageClean
  , commandPackageBuild, commandPackageRun, commandPackageRunJavascript
  , commandToggleBackgroundBuild, commandToggleNative, commandToggleJavaScript
  , commandToggleDebug, commandToggleMakeDocs, commandToggleTest
  , commandToggleRunBenchmarks, commandToggleMakeDependents
  , commandToggleShowIgnored, commandToggleShowHidden
  , commandGetToggleState
  , commandClaudeNew)
import IDE.Web.Frame (MonadWidget, performEvent, performEvent_)

toolbarCss :: Css
toolbarCss = do
    ".toolbar" ? do
        whiteSpace nowrap
        -- The strip doubles as the window's title bar (mac): dragging it must
        -- move the window, never select the details text.  The native monitor
        -- consumes the mouse-down on macOS; this covers the other front ends
        -- and any path the monitor misses.
        "user-select" -: "none"
        "-webkit-user-select" -: "none"
        "cursor" -: "default"
    -- The button strip is collapsible (hidden by default): a max-width
    -- transition on the wrapper slides the buttons out from behind the toggle
    -- and pushes the active-pane details to the right — and back again.
    ".toolbar-items" ? do
        display inlineBlock
        whiteSpace nowrap
        overflow hidden
        "vertical-align" -: "top"
        "max-width" -: "1200px"
        "transition" -: "max-width 0.25s ease"
    ".toolbar-items.toolbar-items-hidden" ?
        ("max-width" -: "0")
    -- What the window is showing (active tab: icon, label, dim path), inline
    -- after the toggle / buttons.  Its own overflow-hidden so a long path
    -- never wraps the 28px strip.
    ".toolbar-active-pane" ? do
        display inlineBlock
        whiteSpace nowrap
        overflow hidden
        "vertical-align" -: "top"
        height (px 28)
        "line-height" -: "28px"
        "margin-left" -: "10px"
        "font-size" -: "12px"
    -- Explicit size: the tree/file SVGs have no intrinsic width, so an
    -- unconstrained <img> renders enormous and (before the height cap above)
    -- grew the strip down over the panes below.
    ".toolbar-active-pane img.tab-icon" ? do
        width (px 16)
        height (px 16)
        "vertical-align" -: "middle"
        "margin-right" -: "5px"
    ".toolbar-active-pane .toolbar-pane-path" ? do
        opacity dimOpacity
        "margin-left" -: "8px"
        "font-size" -: "11px"
    ".toolbar-item" ? do
        display inlineBlock
        -- The hover / toggled / pane-state background lives HERE on the wrapper,
        -- not on the <img> below: in light mode the icon <img> is colour-inverted
        -- (filter: invert) to render black, and a filter on the <img> inverts its
        -- background too — which would turn the pale-blue hover into a dark
        -- colour.  The wrapper is not filtered, so its background keeps its hue.
        borderRadius (px 3) (px 3) (px 3) (px 3)
    ".tooltip" ? do
        position absolute
        -- Above the tab bar below it: the tooltip drops into the tab row, which
        -- comes later in the DOM and would otherwise paint over it.
        zIndex 50
        padding (px 3) (px 3) (px 3) (px 3)
        borderRadius (px 3) (px 3) (px 3) (px 3)
        background surfaceHiColor
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
        -- De-emphasis instead of emphasis: toolbar icons sit dimmed to light
        -- grey and brighten to full opacity on hover (below) or when their
        -- command is toggled / a pane state is active (the background rules
        -- further down each restore full brightness too).
        opacity dimOpacity
    ".toolbar-item" # hover ? background accentHoverColor
    ".toolbar-item" # hover |> ".toolbar-button" ? opacity 1
    ".toolbar-item" # hover |> ".tooltip" ? do
        visibility visible
        opacity 1
        transitionDelay (sec 1)
        transitionDuration (sec 0.2)
    ".toolbar-item.toggled" ? background selectionColor
    ".toolbar-item.toggled" # hover ? background accentHoverColor
    -- The 3-state side-pane button: shown = solid, auto-hide = dim, hidden = none.
    ".toolbar-item.tall-state-show" ? background selectionColor
    ".toolbar-item.tall-state-auto" ? background selectionColorFaint
    -- The 3-state bottom-pane button: shown = solid, auto-hide = dim, hidden = none.
    ".toolbar-item.wide1-state-show" ? background selectionColor
    ".toolbar-item.wide1-state-auto" ? background selectionColorFaint
    -- Toggled / active-state buttons keep their icon at full brightness (their
    -- highlight background is on the wrapper above).
    ".toolbar-item.toggled .toolbar-button" ? opacity 1
    ".toolbar-item.tall-state-show .toolbar-button" ? opacity 1
    ".toolbar-item.tall-state-auto .toolbar-button" ? opacity 1
    ".toolbar-item.wide1-state-show .toolbar-button" ? opacity 1
    ".toolbar-item.wide1-state-auto .toolbar-button" ? opacity 1

toolbarButton
  :: MonadWidget t m
  => Ctx t
  -> Command
  -> m (Event t ToolbarEvents)
toolbarButton ctx cmd = do
  let (src, tip) = commandImageAndTip cmd
  attrD <- case commandGetToggleState cmd of
    Just f -> fmap (("class" =:) . ("toolbar-item" <>) . bool "" " toggled") <$>
                holdUniqDyn (f <$> cCfg ctx)
    Nothing -> return $ constDyn ("class" =: "toolbar-item")
  (e, _) <- elDynAttr' "div" attrD $ do
    elAttr "img" ("src" =: src <> "class" =: "toolbar-button") $ return ()
    divClass "tooltip" $ text tip
  return $ ToolbarCommand cmd <$ domEvent Click e

-- | The side ("tall") pane visibility button: one icon, cycling
-- show -> auto-hide -> hide, with the current state shown by its class/tooltip.
tallToggleButton
  :: MonadWidget t m
  => Dynamic t TallVisibility   -- ^ this window's side-pane visibility
  -> m (Event t ToolbarEvents)
tallToggleButton visInD = do
  visD <- holdUniqDyn visInD
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
  => Dynamic t TallVisibility   -- ^ this window's bottom-pane visibility
  -> m (Event t ToolbarEvents)
wide1ToggleButton visInD = do
  visD <- holdUniqDyn visInD
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

-- | The toolbar strip: a toggle button, the (collapsible) button row, then
-- the active-pane details.  The buttons are HIDDEN by default — the strip
-- normally reads @[toggle] [what this window is showing]@ — and the choice
-- persists per machine (localStorage, like the resizable bars).  Showing the
-- buttons slides the details to the right; hiding slides them back.
toolbarWidget
  :: MonadWidget t m
  => Ctx t
  -> Dynamic t TallVisibility   -- ^ this window's side-pane visibility
  -> Dynamic t TallVisibility   -- ^ this window's bottom-pane visibility
  -> m ()                       -- ^ active-pane details, rendered after the buttons
  -> m (Event t ToolbarEvents)
toolbarWidget ctx tallVisD wide1VisD detailsW =
  divClass "toolbar" $ do
    -- Always present, ahead of the toggle: the side/bottom pane visibility
    -- buttons — layout controls the user reaches for constantly, not build
    -- commands, so they never hide with the rest.
    tallE  <- tallToggleButton tallVisD
    wide1E <- wide1ToggleButton wide1VisD
    pb    <- getPostBuild
    initE <- performEvent $
      liftJSM (valToText =<< eval
        ("localStorage.getItem('leksahToolbarShown') || '0'" :: Text)) <$ pb
    rec shownD <- holdDyn False $ leftmost
          [ (== "1") <$> initE
          , not <$> tag (current shownD) toggleE ]
        toggleE <- do
          let attrD = ffor shownD $ \s -> "class" =:
                ("toolbar-item toolbar-toggle" <> bool "" " toggled" s)
          (e, _) <- elDynAttr' "div" attrD $ do
            elAttr "img" ("src" =: "/pics/toolbar.svg"
                       <> "class" =: "toolbar-button") $ return ()
            divClass "tooltip" $
              dynText (bool "Show the toolbar" "Hide the toolbar" <$> shownD)
          return (domEvent Click e)
    -- Written on the CLICK (old value tagged, so store its negation), not on
    -- updates — the localStorage-seeded initial update must not write back.
    performEvent_ $ ffor (tag (current shownD) toggleE) $ \was ->
      liftJSM . void $ eval
        ("localStorage.setItem('leksahToolbarShown','"
         <> (if was then "0" else "1") <> "')" :: Text)
    let itemsAttrD = ffor shownD $ \s -> "class" =:
          ("toolbar-items" <> bool " toolbar-items-hidden" "" s)
    evs <- elDynAttr "div" itemsAttrD $
      mapM (toolbarButton ctx)
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
        , commandClaudeNew
        ]
    divClass "toolbar-active-pane" detailsW
    return $ leftmost (tallE : wide1E : evs)
