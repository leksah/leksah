{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The \"Terminals\" tree pane (lives on the side, like the Workspace and
-- Metadata trees).  It mirrors the tmux hierarchy backing the terminals:
--
--   * sessions — one per open terminal (a @leksah-N@ tmux session), labelled
--     with its window title; click to bring it up in the editor area, ✕ to kill
--     it (with a confirm step).  This is the level the rest of leksah tracks.
--   * windows  — the tmux windows of that session; click to switch the session
--     to that window (and bring the session up).
--   * panes    — the panes of that window; click to make it the active pane.
--
-- The window/pane levels are polled from tmux ('listTerminalTree'); the session
-- level comes from the open-terminals list maintained in 'IDE.Web.Main'.  The
-- terminals themselves render in the editor area ('IDE.Web.Widget.Terminal').
module IDE.Web.Widget.Terminals
  ( terminalsCss
  , terminalsWidget
  ) where

import Control.Monad.IO.Class (liftIO)

import qualified Data.Map as M (fromList, elems, findWithDefault)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)

import Clay
       (overflow, auto, height, pct, padding, px, (-:), display, flex,
        width, background, color, white, borderStyle, borderRadius,
        backgroundImage, vGradient, fontSize, fontWeight, bold, hover, grey,
        (#), cursor, cursorDefault, (?), Css, Color(..), None(..), Cursor(..))
import Clay.Stylesheet (key)

import Reflex
       (holdUniqDyn, listViewWithKey, leftmost, fmapMaybe, ffor, holdDyn,
        switchHold, never, performEvent, getPostBuild, tickLossyFromPostBuildTime,
        updated, Dynamic, Event)
import Reflex.Dom.Core
       (MonadWidget, divClass, el, elClass, elClass', elDynAttr', dyn, dynText,
        text, domEvent, EventName(..), (=:))

import IDE.Web.Events (TerminalsEvents(..))
import IDE.Web.Widget.Terminal
       (TmuxWindow(..), TmuxPane(..), listTerminalTree, killTmuxWindow,
        killTmuxPane)
import IDE.Web.Widget.Tree (treeItem)

-- | How often the window/pane levels are refreshed from tmux.
terminalsPollInterval :: NominalDiffTime
terminalsPollInterval = 2

-- | Shared positioning for the close control's contents: pinned to the right of
-- its (relative) slot and vertically centred, out of normal flow so it overlays
-- rather than reflows.
closeOverlay :: Css
closeOverlay = do
    "position" -: "absolute"
    "right" -: "0"
    "top" -: "50%"
    "transform" -: "translateY(-50%)"
    "white-space" -: "nowrap"

terminalsCss :: Css
terminalsCss = do
    ".terminals" ? do
        height (pct 100)
        overflow auto
        -- The expand/collapse triangles are SVG; give them a visible fill (the
        -- shared tree rules only set it under .workspace / .metadata otherwise).
        key "fill" grey
        -- The tree text is not editable, so keep the normal arrow cursor.
        cursor cursorDefault
        -- A uniform right inset: every row's right edge (and so every close
        -- button) lines up here, clear of the scrollbar.
        "padding-right" -: "8px"
    -- All buttons in this pane share a dark look; the default native button is
    -- light and clashes with the dark UI.
    ".terminals button" ? do
        color white
        borderStyle none
        borderRadius (px 3) (px 3) (px 3) (px 3)
        backgroundImage (vGradient (Rgba 64 64 64 1.0) (Rgba 40 40 40 1.0))
        fontSize (px 13)
        cursor cursorDefault
    ".terminals button" # hover ?
        backgroundImage (vGradient (Rgba 84 84 84 1.0) (Rgba 60 60 60 1.0))
    ".terminals .terminals-new" ? do
        width (pct 100)
        padding (px 4) (px 4) (px 4) (px 4)
    ".terminals li" ? do
        -- No horizontal padding: the deeper levels are indented by the nested
        -- <ul> margins, and any right padding would compound per level and step
        -- each level's right edge (and close button) inward.  So every row's
        -- right edge is the pane's content edge, lining the close buttons up.
        padding (px 2) (px 0) (px 2) (px 0)
        -- Lay the row out with flexbox so the expand triangle and close button
        -- stay on one line, vertically centred, and aren't pulled into the
        -- label's word-wrapping.  The label takes the middle; the (expanded)
        -- children wrap onto their own full-width line below.
        display flex
        "flex-wrap" -: "wrap"
        "align-items" -: "center"
    -- Clickable labels (session title, window, pane): take the free space and
    -- wrap internally, long space-less paths included.  `min-width: 0` lets the
    -- flex item shrink enough to wrap.
    ".terminals .terminals-label" ? do
        cursor cursorDefault
        "flex" -: "1"
        "min-width" -: "0"
        "overflow-wrap" -: "anywhere"
    -- The expander and the expanded children don't take part in the wrapping:
    -- the triangle keeps its size; the children occupy their own line.
    ".terminals .tree-expand" ? ("flex" -: "0 0 auto")
    ".terminals .tree-children" ? ("flex-basis" -: "100%")
    -- The close control: a fixed-width slot pinned to the row's right edge, so it
    -- never changes the label's width.  Its contents are absolutely positioned,
    -- so swapping the ✕ for Kill / Cancel doesn't reflow the row — the wider
    -- confirm buttons just overlap the title to their left.  align-self stretch
    -- makes the slot as tall as the (possibly wrapped) label so its contents
    -- centre against it.
    ".terminals .terminals-close-slot" ? do
        "flex" -: "0 0 auto"
        width (px 16)
        "position" -: "relative"
        "align-self" -: "stretch"
    ".terminals .terminals-close-slot > button" ? closeOverlay
    ".terminals .terminals-confirm" ? do
        closeOverlay
        display flex
        -- Mask the title underneath the confirm buttons.
        background (Rgba 32 32 32 1.0)
    -- The focused session (shown in the editor area) is highlighted.
    ".terminals .terminals-active" ?
        background (Rgba 30 88 209 1.0)
    -- tmux's current window / active pane shown in bold.
    ".terminals .terminals-current" ? fontWeight bold
    -- Compact close (✕) / kill / cancel buttons.
    ".terminals .terminal-close" ? do
        padding (px 0) (px 4) (px 0) (px 4)
        fontSize (px 11)
    ".terminals .terminal-cancel" ? do
        padding (px 0) (px 4) (px 0) (px 4)
        fontSize (px 11)

-- | A node event is either a local tmux side effect to run here (killing a
-- window/pane: it doesn't touch leksah's session list/tabs, so it needn't go up
-- to 'IDE.Web.Main') or a 'TerminalsEvents' to bubble up (select anything, or
-- kill a whole session).
type NodeEvent = Either (IO ()) TerminalsEvents

terminalsWidget
  :: forall t m . MonadWidget t m
  => Dynamic t (Maybe Int)     -- ^ the focused session's id (highlighted)
  -> Dynamic t [(Int, Text)]   -- ^ open sessions: id and current title
  -> m (Event t TerminalsEvents)
terminalsWidget activeD listD = divClass "terminals" $ do
  newE <- (NewTerminal <$) . domEvent Click . fst <$> elClass' "button" "terminals-new" (text "+ New Terminal")
  -- Poll tmux for the window/pane hierarchy: on first build, on a timer, and
  -- whenever the open-session list changes (so a just-created session fills in
  -- promptly rather than waiting for the next tick).
  postBuild <- getPostBuild
  tick <- tickLossyFromPostBuildTime terminalsPollInterval
  rec
    polledE <- performEvent $ ffor (leftmost [() <$ postBuild, () <$ tick, () <$ updated listD]) $ \_ ->
        liftIO listTerminalTree
    -- A window/pane kill: run it, then immediately re-read the tree (in the same
    -- action, so the order is fixed) so the row goes away at once rather than on
    -- the next poll tick.
    killedE <- performEvent $ ffor killActE $ \act -> liftIO (act >> listTerminalTree)
    treeD <- holdDyn mempty (leftmost [polledE, killedE])
    -- One entry per session: its title and its tmux windows.
    itemsD <- holdUniqDyn $
        (\sessions tree -> M.fromList
            [ (n, (title, M.findWithDefault [] n tree)) | (n, title) <- sessions ])
          <$> listD <*> treeD
    rowsE <- el "ul" $ listViewWithKey itemsD $ \n vD ->
      sessionNode ((== Just n) <$> activeD) n vD
    let nodeE    = fmapMaybe (listToMaybe . M.elems) rowsE
        killActE = fmapMaybe (either Just (const Nothing)) nodeE
        bubbleE  = fmapMaybe (either (const Nothing) Just) nodeE
  return $ leftmost [newE, bubbleE]

-- | A session node: the title (click to select) with a ✕ that asks to confirm
-- before killing, and the session's tmux windows as children.
sessionNode
  :: MonadWidget t m
  => Dynamic t Bool -> Int -> Dynamic t (Text, [TmuxWindow])
  -> m (Event t NodeEvent)
sessionNode activeD n vD =
  treeItem "terminals-session" False
    (sessionRow activeD n (fst <$> vD))
    (el "ul" $ windowsTree n (snd <$> vD))

-- | The session row: its (highlightable) title and the inline close confirm.
sessionRow
  :: MonadWidget t m
  => Dynamic t Bool -> Int -> Dynamic t Text -> m (Event t NodeEvent)
sessionRow activeD n titleD = do
  let labelAttrs = ffor activeD $ \a ->
        "class" =: ("terminals-label" <> if a then " terminals-active" else "")
  (labelEl, _) <- elDynAttr' "span" labelAttrs $ dynText titleD
  killE <- confirmClose
  return $ leftmost [ Right (SelectTerminal n) <$ domEvent Click labelEl
                    , Right (CloseTerminal n)  <$ killE ]

-- | The windows of a session, each expandable into its panes.
windowsTree
  :: MonadWidget t m
  => Int -> Dynamic t [TmuxWindow] -> m (Event t NodeEvent)
windowsTree n windowsD =
  fmapMaybe (listToMaybe . M.elems) <$>
    listViewWithKey (M.fromList . map (\w -> (twIndex w, w)) <$> windowsD)
      (\widx wD ->
        treeItem "terminals-window" False
          (do let attrs = ffor wD $ \w ->
                    "class" =: ("terminals-label" <> if twActive w then " terminals-current" else "")
              (e, _) <- elDynAttr' "span" attrs $ dynText (twLabel <$> wD)
              killE <- confirmClose
              return $ leftmost [ Right (SelectTerminalWindow n widx) <$ domEvent Click e
                                , Left (killTmuxWindow n widx) <$ killE ])
          (el "ul" $ panesTree n widx (twPanes <$> wD)))

-- | The panes of a window (leaves).
panesTree
  :: MonadWidget t m
  => Int -> Int -> Dynamic t [TmuxPane] -> m (Event t NodeEvent)
panesTree n widx panesD =
  fmapMaybe (listToMaybe . M.elems) <$>
    listViewWithKey (M.fromList . map (\p -> (tpIndex p, p)) <$> panesD)
      (\pidx pD -> el "li" $ do
        let attrs = ffor pD $ \p ->
              "class" =: ("terminals-label" <> if tpActive p then " terminals-current" else "")
        (e, _) <- elDynAttr' "span" attrs $ dynText (tpLabel <$> pD)
        killE <- confirmClose
        return $ leftmost [ Right (SelectTerminalPane n widx pidx) <$ domEvent Click e
                          , Left (killTmuxPane n widx pidx) <$ killE ])

-- | An inline close control: a ✕ that turns into Kill / Cancel and fires once
-- Kill is confirmed.  Done inline rather than with window.confirm (which doesn't
-- work in this web view) or a native UI delegate (which would clash with
-- jsaddle's bridge).
confirmClose :: MonadWidget t m => m (Event t ())
confirmClose = elClass "div" "terminals-close-slot" $ mdo
  confirmingD <- holdDyn False setConfirmE
  rowE <- dyn $ ffor confirmingD $ \confirming ->
    if confirming
      then elClass "div" "terminals-confirm" $ do
        (killEl, _)   <- elClass' "button" "terminal-close"  $ text "Kill"
        (cancelEl, _) <- elClass' "button" "terminal-cancel" $ text "Cancel"
        return ( leftmost [ False <$ domEvent Click killEl, False <$ domEvent Click cancelEl ]
               , domEvent Click killEl )
      else do
        (xEl, _) <- elClass' "button" "terminal-close" $ text "✕"
        return ( True <$ domEvent Click xEl, never )
  setConfirmE <- switchHold never (fst <$> rowE)
  switchHold never (snd <$> rowE)
