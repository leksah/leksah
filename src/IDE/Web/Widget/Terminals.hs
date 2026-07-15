{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- reflex-dom deprecates 'textInput' in favour of the lower-level
-- 'inputElement'.  The terminal-rename field deliberately uses 'textInput'
-- (its value/keydown/hasFocus accessors are exactly what the commit/cancel
-- logic needs); migrating this focus-sensitive field carries regression risk
-- for no behavioural gain, so silence the deprecation here.
{-# OPTIONS_GHC -Wno-deprecations #-}
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
  , sessionAlert
  , windowAlert
  , windowAlertSrc
  , sessionAlertSrc
  ) where

import Control.Concurrent (forkIO)
import Control.Lens ((.~), (^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Default (def)
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as M (fromList, elems, empty, lookup)
import Data.Set (Set)
import qualified Data.Set as S (member)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (breakOn, drop, null)

import Clay
       (overflow, auto, height, pct, padding, px, (-:), display, flex,
        width, background, color, white, borderStyle, borderRadius,
        backgroundImage, vGradient, fontSize, fontWeight, bold, hover, grey,
        opacity, (#), cursor, cursorDefault, (?), Css, Color(..), None(..), Cursor(..))
import Clay.Stylesheet (key)

import Reflex
       (holdUniqDyn, listViewWithKey, leftmost, fmapMaybe, ffor, ffilter, holdDyn,
        switchHold, switchDyn, never, constDyn, tagPromptlyDyn, newTriggerEvent,
        performEvent_, getPostBuild, debounce, updated,
        Dynamic, Event)
import Reflex.Dom.Core
       (MonadWidget, divClass, el, elClass, elClass', elAttr, elAttr', elDynAttr', dyn,
        blank, dynText, text, domEvent, EventName(..), (=:), textInput, attributes,
        widgetHold, textInputConfig_initialValue, _textInput_value,
        _textInput_keydown, _textInput_hasFocus)
import Language.Javascript.JSaddle (liftJSM, jsg, js1, fun, eval)

import IDE.Web.Theme (selectionColor, hoverColor, dimColor, dimOpacity)
import IDE.Web.Events (TerminalsEvents(..))
import IDE.Web.Widget.Terminal
       (TmuxWindow(..), TmuxPane(..), listTerminalTree, killTmuxWindow,
        killTmuxPane, newTmuxWindow, zoomTmuxPane, breakTmuxPane,
        renameTmuxSession, renameTmuxWindow)
import IDE.Web.Widget.Tree (treeItem)
import IDE.Web.TerminalRefresh (registerTerminalRefresh, ensureTerminalMonitor)

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
    -- Top-level tree uls sit flush with the pane's left edge (like the Workspace
    -- tree's ul.projects); only nested uls get the global 20px indent.
    ".terminals > ul" ?
        ("margin-left" -: "0px")
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
    -- Compact management glyphs (new window / zoom / break / rename) sitting in a
    -- row, kept subtle until hovered so they don't shout over the labels.
    ".terminals .terminals-action" ? do
        padding (px 0) (px 4) (px 0) (px 4)
        "margin-left" -: "2px"
        fontSize (px 11)
        "opacity" -: "0.55"
    ".terminals .terminals-action" # hover ? ("opacity" -: "1")
    -- Hovering any of a row's buttons highlights the whole row line — label
    -- through the area behind the button — with the (configurable) hover
    -- colour; the button itself keeps its normal look.  Clipped to the first
    -- line so it doesn't bleed over an expanded subtree, and scoped to the
    -- row's OWN buttons (child rows live under .tree-children / nested uls,
    -- which the direct-child paths don't reach).
    ".terminals li:has(> button:hover), .terminals li:has(> .terminals-rename-slot button:hover), .terminals li:has(> .terminals-close-slot button:hover)" ? do
        backgroundImage (vGradient hoverColor hoverColor)
        "background-size" -: "100% 20px"
        "background-repeat" -: "no-repeat"
    -- The inline rename box: dark to match, and *absolutely positioned* so it
    -- overlays the row (over the old name) rather than sitting in the flex flow —
    -- that way it doesn't grow the row height (shifting siblings) and it spans the
    -- width so a long name fits.  Vertically centred within the (relative) row.
    ".terminals .terminals-rename" ? do
        "position" -: "absolute"
        -- left/right chosen so the edit box's *text* lands exactly where the
        -- label's text sits (left: 8px + 4px padding = 12px; right: 52px clears
        -- the action glyphs) — see the live-measured alignment in the notes.
        "left" -: "8px"
        "right" -: "52px"
        -- Anchor to the *top* of the row (not the li centre): when the session is
        -- expanded the li also contains the child window/pane rows, so a 50%/
        -- translateY centring would drop the box into the middle of the whole
        -- subtree.  top:1px overlays the first (session) line, matching the label.
        "top" -: "1px"
        "box-sizing" -: "border-box"
        "z-index" -: "2"
        color white
        background (Rgba 30 30 30 1.0)
        borderStyle none
        borderRadius (px 3) (px 3) (px 3) (px 3)
        fontSize (px 13)
        padding (px 1) (px 4) (px 1) (px 4)
    -- Top-level host rows ("Local", remote hosts): bold, like section heads.
    ".terminals .terminals-host-label" ? do
        fontWeight bold
        "flex" -: "1"
        "min-width" -: "0"
    ".terminals li" ? do
        -- No horizontal padding: the deeper levels are indented by the nested
        -- <ul> margins, and any right padding would compound per level and step
        -- each level's right edge (and close button) inward.  So every row's
        -- right edge is the pane's content edge, lining the close buttons up.
        padding (px 1) (px 0) (px 1) (px 0)
        -- Positioning context for the absolutely-overlaid rename box.  No min
        -- height: the natural row height already gives the box room, and 22px
        -- made the rows look too airy.
        "position" -: "relative"
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
        -- Match the 2px left inset the Workspace tree's .tree-item rows have, so
        -- the row icons line up across all three trees.
        "padding-left" -: "2px"
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
    -- De-emphasis instead of emphasis (matching the Workspace tree): every
    -- host/session/window/pane label + icon is dimmed to light grey by default —
    -- "Local", the remote server names, and all sessions read grey.  Only the
    -- *active-pane chain* is lit white: the focused session (open in the editor),
    -- its machine (host head), and the current window / active pane WITHIN that
    -- session.  Everything else — other machines, other sessions, and their own
    -- current windows — stays grey.  (The white overrides must follow this dim
    -- rule — they share its specificity and win only by source order.)
    ".terminals .terminals-label" ? color dimColor
    ".terminals .terminals-label img.tree-icon" ? opacity dimOpacity
    -- The focused session (shown in the editor area): highlighted + white.
    ".terminals .terminals-active" ? do
        background selectionColor
        color white
    ".terminals .terminals-active img.tree-icon" ? opacity 1
    -- The active session's machine: the host row whose subtree holds the focused
    -- session (`:has(.terminals-active)`) — its own direct host label goes white.
    ".terminals li:has(.terminals-active) > .terminals-host-label" ? color white
    ".terminals li:has(.terminals-active) > .terminals-host-label img.tree-icon" ? opacity 1
    -- The current window / active pane, but ONLY inside the focused session
    -- (`> .terminals-active` = that session li's own label) — a background
    -- session's current window/pane stays grey.
    ".terminals li:has(> .terminals-active) .terminals-current" ? color white
    ".terminals li:has(> .terminals-active) .terminals-current img.tree-icon" ? opacity 1
    -- The keyboard-nav cursor uses the shared '.leksah-nav-current' highlight,
    -- which is a blue FILL by default (see 'IDE.Web.Layout') — a SECOND blue row
    -- alongside the active terminal, which reads as confusing.  In this tree draw
    -- it as a blue OUTLINE (same blue) instead, so the ONLY blue fill is the
    -- active terminal.  When the cursor sits on the active row it keeps that fill
    -- (':not(.terminals-active)').
    ".terminals .leksah-nav-item.leksah-nav-current:not(.terminals-active)" ? do
        "background" -: "transparent"
        "box-shadow" -: "inset 0 0 0 1px var(--leksah-selection)"
    -- The state-carrying window/session icon encodes its meaning in colour, so it
    -- is never dimmed (would mute the white/yellow attention states).
    ".terminals .terminals-label img.term-alert-icon" ? opacity 1
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
  => Dynamic t (Maybe Text)    -- ^ the focused session's id (highlighted)
  -> Dynamic t (Set Text)      -- ^ sessions wanting attention (viewed-window bell → 🔔)
  -> Dynamic t [Text]          -- ^ remote ssh hosts (prefs ∪ open ssh:// tabs)
  -> Dynamic t (Map Text (Bool, Map Text (Text, [TmuxWindow])))
                               -- ^ each host's (reachable, sessions) — the
                               --   shared ssh poll in 'IDE.Web.Main' (one ssh
                               --   per host, feeding this tree AND the flipper)
  -> m (Event t TerminalsEvents)
terminalsWidget activeD attnD remoteHostsD hostTreesD = divClass "terminals leksah-nav" $ do
  -- Read the whole session/window/pane tree (keyed by session id, each carrying
  -- its current name): on first build, on a "new session" click, and — instead
  -- of a timer — whenever the persistent tmux control-mode monitor
  -- ('IDE.Web.TerminalRefresh') reports a structural change (an external rename,
  -- a window added inside a terminal, …).  Re-reading refreshes a renamed
  -- session's label (the id key is unchanged, so only the display updates).
  postBuild <- getPostBuild
  -- The monitor fires a burst of notifications for one logical change (e.g. a
  -- window add is add + layout-change + rename); debounce so we read the tree
  -- once it settles rather than once per line.
  (refreshE, fireRefresh) <- newTriggerEvent
  _ <- liftIO $ registerTerminalRefresh (fireRefresh ())
  refreshE' <- debounce 0.3 refreshE
  rec
    -- Reads run OFF the reflex thread (tmux subprocesses — a synchronous
    -- performEvent here hitches the whole UI, keystrokes included).  Each read
    -- also (idempotently) ensures the monitor is running, so it re-arms after a
    -- server restart when the user next opens a terminal.
    (polledE, firePolled) <- newTriggerEvent
    performEvent_ $ ffor (leftmost [() <$ postBuild, refreshE', () <$ newE]) $ \_ ->
        liftIO . void . forkIO $ ensureTerminalMonitor >> listTerminalTree >>= firePolled
    -- A window/pane kill: run it, then immediately re-read the tree (in the same
    -- action, so the order is fixed) so the row goes away at once rather than on
    -- the next poll tick.
    (killedE, fireKilled) <- newTriggerEvent
    performEvent_ $ ffor killActE $ \act ->
        liftIO . void . forkIO $ (act >> listTerminalTree) >>= fireKilled
    -- Every live session (id -> (name, windows)); this is the tree directly.
    itemsD <- holdUniqDyn =<< holdDyn mempty (leftmost [polledE, killedE])
    -- "Local": the host node for leksah's own tmux server, expanded by default;
    -- its "+" glyph replaces the old full-width "New Session" button.
    localE <- el "ul" $ treeItem "terminals-host" True
        (hostRow "Local" NewTerminal "New local session")
        (el "ul" $ fmapMaybe (listToMaybe . M.elems) <$> listViewWithKey itemsD (\n vD ->
            sessionNode ((== Just n) <$> activeD) (S.member n <$> attnD) n vD))
    let killActE = fmapMaybe (either Just (const Nothing)) localE
        newE     = fmapMaybe (\e -> case e of NewTerminal -> Just (); _ -> Nothing) bubbleLocalE
        bubbleLocalE = fmapMaybe (either (const Nothing) Just) localE
  -- One node per remote host, sessions/windows/panes over ssh (select-only);
  -- the data comes from the shared per-host poll in 'IDE.Web.Main'.
  remoteE <- el "ul" $ listViewWithKey (M.fromList . map (\h -> (h, ())) <$> remoteHostsD)
      (\host _ -> remoteHostNode activeD host
          (fromMaybe (True, M.empty) . M.lookup host <$> hostTreesD))
  return $ leftmost [ bubbleLocalE
                    , fmapMaybe (\m -> listToMaybe (M.elems m)
                                        >>= either (const Nothing) Just) remoteE ]

-- | A leading B&W node icon (a @/pics/*.svg@) for a Terminals-tree row.
termIcon :: MonadWidget t m => Text -> m ()
termIcon name = elAttr "img" ("class" =: "tree-icon" <> "src" =: ("/pics/" <> name)) blank

-- | A top-level host row: bold label plus the "+" new-session glyph.
hostRow :: MonadWidget t m => Text -> TerminalsEvents -> Text -> m (Event t NodeEvent)
hostRow label newEv tip = do
    elClass "span" "terminals-label terminals-host-label" $ do
        termIcon "tree-host-local.svg"
        text label
    newE <- actionBtn "+" tip
    pure $ Right newEv <$ newE

-- | A remote host: its tmux tree from the shared per-host ssh poll (see
-- 'IDE.Web.Main'); unreachable hosts keep the last-known tree and mark the
-- label.  Rows are select-only (no manage glyphs yet) — selections
-- open/steer the host's control-mode tabs.
remoteHostNode
  :: MonadWidget t m
  => Dynamic t (Maybe Text) -> Text -> Dynamic t (Bool, Map Text (Text, [TmuxWindow]))
  -> m (Event t NodeEvent)
remoteHostNode activeD host treeD = do
    reachD <- holdUniqDyn (fst <$> treeD)
    itemsD <- holdUniqDyn (snd <$> treeD)
    treeItem "terminals-host" True
        (do let lblD = ffor reachD $ \r -> host <> (if r then "" else "  (unreachable)")
            -- Clicking the server row brings up its one per-server control-mode
            -- connection (its 'leksah' session, created if missing).
            (lbl, _) <- elClass' "span" "terminals-label terminals-host-label leksah-nav-item" $ do
                termIcon "tree-host-remote.svg"
                dynText lblD
            newE <- actionBtn "+" ("New session on " <> host)
            pure $ leftmost [ Right (NewRemoteTerminal host) <$ newE
                            , Right (SelectRemoteHost host)  <$ domEvent Click lbl ])
        (el "ul" $ fmapMaybe (listToMaybe . M.elems) <$> listViewWithKey itemsD (\sid vD ->
            remoteSessionNode activeD host sid vD))

remoteSessionNode
  :: MonadWidget t m
  => Dynamic t (Maybe Text) -> Text -> Text -> Dynamic t (Text, [TmuxWindow]) -> m (Event t NodeEvent)
remoteSessionNode activeD host sid vD =
  treeItem "terminals-session" True
    (do -- The focused remote session (its tab is keyed "ssh://host#target",
        -- where target is either the session id or its current name — see
        -- resolveRemoteKey in IDE.Web.Main): mark it 'terminals-active' so it,
        -- its host, and its current window light white like a local one.
        let labelAttrs = (\a (nm, _) ->
              "class" =: ("terminals-label leksah-nav-item"
                <> if a == Just ("ssh://" <> host <> "#" <> sid)
                      || a == Just ("ssh://" <> host <> "#" <> nm)
                   then " terminals-active" else ""))
              <$> activeD <*> vD
        (lbl, _) <- elDynAttr' "span" labelAttrs $ do
            termSessIcon (constDyn False) (snd <$> vD)
            dynText (fst <$> vD)
        renE    <- renameControl (fst <$> vD) (RenameRemoteTerminalSession host sid)
        newWinE <- actionBtn "+" "New window in this session"
        killE   <- confirmClose
        -- The select event carries the session's CURRENT name too, so the
        -- handler can match a tab keyed by name (cc-connect HOST#NAME); the
        -- close event likewise, to drop that tab.
        pure $ leftmost
          [ (\(nm, _) -> Right (SelectRemoteTerminal host sid nm))
              <$> tagPromptlyDyn vD (domEvent Click lbl)
          , Right <$> renE
          , Right (NewRemoteTerminalWindow host sid) <$ newWinE
          , (\(nm, _) -> Right (CloseRemoteTerminal host sid nm))
              <$> tagPromptlyDyn vD killE ])
    (el "ul" $ remoteWindowsTree host sid (fst <$> vD) (snd <$> vD))

remoteWindowsTree
  :: MonadWidget t m
  => Text -> Text -> Dynamic t Text -> Dynamic t [TmuxWindow] -> m (Event t NodeEvent)
remoteWindowsTree host sid nameD windowsD =
  fmapMaybe (listToMaybe . M.elems) <$>
    listViewWithKey (M.fromList . map (\w -> (twIndex w, w)) <$> windowsD)
      (\widx wD ->
        treeItem "terminals-window" False
          (do let attrs = ffor wD $ \w ->
                    "class" =: ("terminals-label leksah-nav-item" <> if twActive w then " terminals-current" else "")
              (e, _) <- elDynAttr' "span" attrs $ do
                    termWinIcon wD
                    dynText (twLabel <$> wD)
              renE  <- renameControl (windowRawName <$> wD) (RenameRemoteTerminalWindow host sid widx)
              killE <- confirmClose
              pure $ leftmost
                [ (\nm -> Right (SelectRemoteTerminalWindow host sid nm widx))
                    <$> tagPromptlyDyn nameD (domEvent Click e)
                , Right <$> renE
                , Right (KillRemoteTerminalWindow host sid widx) <$ killE ])
          (el "ul" $ remotePanesTree host sid nameD widx (twPanes <$> wD)))

remotePanesTree
  :: MonadWidget t m
  => Text -> Text -> Dynamic t Text -> Int -> Dynamic t [TmuxPane] -> m (Event t NodeEvent)
remotePanesTree host sid nameD widx panesD =
  fmapMaybe (listToMaybe . M.elems) <$>
    listViewWithKey (M.fromList . map (\p -> (tpIndex p, p)) <$> panesD)
      (\pidx pD -> el "li" $ do
        let attrs = ffor pD $ \p ->
              "class" =: ("terminals-label leksah-nav-item" <> if tpActive p then " terminals-current" else "")
        (e, _) <- elDynAttr' "span" attrs $ do
              termIcon "tree-pane.svg"
              dynText (tpLabel <$> pD)
        zoomE  <- actionBtn "⤢" "Zoom / unzoom this pane"
        breakE <- actionBtn "↗" "Break this pane out into its own window"
        killE  <- confirmClose
        pure $ leftmost
          [ (\nm -> Right (SelectRemoteTerminalPane host sid nm widx pidx))
              <$> tagPromptlyDyn nameD (domEvent Click e)
          , Right (ZoomRemoteTerminalPane host sid widx pidx)  <$ zoomE
          , Right (BreakRemoteTerminalPane host sid widx pidx) <$ breakE
          , Right (KillRemoteTerminalPane host sid widx pidx)  <$ killE ])

-- | A session node: the name (click to select) with a ✕ that asks to confirm
-- before killing, and the session's tmux windows as children.
sessionNode
  :: MonadWidget t m
  => Dynamic t Bool -> Dynamic t Bool -> Text
  -> Dynamic t (Text, [TmuxWindow])
  -> m (Event t NodeEvent)
sessionNode activeD attnD n vD =
  treeItem "terminals-session" True
    (sessionRow activeD attnD n vD)
    (el "ul" $ windowsTree n (snd <$> vD))

-- | A one-glyph badge for a window's tmux alert state, highest priority first:
-- 🔔 bell (rang the bell — e.g. Claude Code wants input), ● activity (new
-- output), ○ silence (gone quiet — idle/done).  Empty when there's no alert.
windowAlert :: TmuxWindow -> Text
windowAlert w
  | twBell w     = " \128276"
  | twActivity w = " \9679"
  | twSilence w  = " \9675"
  | otherwise    = ""

-- | The leading terminal-window icon whose fill/colour encodes the same alert
-- state that 'windowAlert' used to append as a text glyph — so the notification
-- rides the window icon itself instead of a trailing 🔔/●/○.  The window icon
-- has a fillable bottom half; the mapping (see the user-confirmed legend):
--   * bell (needs input)       → all yellow (filled)
--   * activity (new output)    → all white, filled circle
--   * silence (went quiet/done)→ white frame, dark fill ("empty circle")
--   * active window, no output → grey, empty (you're on it)
--   * otherwise (seen output)  → grey, bottom-half fill
windowAlertSrc :: TmuxWindow -> Text
windowAlertSrc w
  | twBell w     = "/pics/tree-window-bell.svg"
  | twActivity w = "/pics/tree-window-activity.svg"
  | twSilence w  = "/pics/tree-window-silence.svg"
  | twActive w   = "/pics/tree-window-calm.svg"
  | otherwise    = "/pics/tree-window-idle.svg"

-- | Render the state-carrying window icon for a tree row (dynamic in the
-- window's alert flags).  'term-alert-icon' keeps its own colour — it is
-- exempt from both the non-selected dimming and the mono/colour icon swap.
termWinIcon :: MonadWidget t m => Dynamic t TmuxWindow -> m ()
termWinIcon wD = void $ elDynAttr' "img"
    (ffor wD $ \w -> "class" =: "tree-icon term-alert-icon" <> "src" =: windowAlertSrc w) blank

-- | The strongest alert among a session's windows (for the session row badge).
sessionAlert :: [TmuxWindow] -> Text
sessionAlert ws
  | any twBell ws     = " \128276"
  | any twActivity ws = " \9679"
  | any twSilence ws  = " \9675"
  | otherwise         = ""

-- | The session-glyph counterpart of 'windowAlertSrc': the same fill/colour
-- state language on the @>_@ session icon, from the strongest alert among the
-- session's windows.  (No calm/empty variant — a session's resting state is
-- the grey bottom-fill 'idle'.)
sessionAlertSrc :: [TmuxWindow] -> Text
sessionAlertSrc ws
  | any twBell ws     = "/pics/tree-session-bell.svg"
  | any twActivity ws = "/pics/tree-session-activity.svg"
  | any twSilence ws  = "/pics/tree-session-silence.svg"
  | otherwise         = "/pics/tree-session-idle.svg"

-- | Render the state-carrying session icon (dynamic in the session's windows);
-- a leksah-tracked bell (first arg — the viewed-window bell tmux's hook skips)
-- forces the bell icon.  Like 'termWinIcon', 'term-alert-icon' keeps its colour
-- (exempt from dimming and the mono/colour swap).
termSessIcon :: MonadWidget t m => Dynamic t Bool -> Dynamic t [TmuxWindow] -> m ()
termSessIcon attnD wsD = void $ elDynAttr' "img"
    ((\att ws -> "class" =: "tree-icon term-alert-icon"
              <> "src" =: if att then "/pics/tree-session-bell.svg" else sessionAlertSrc ws)
       <$> attnD <*> wsD) blank

-- | The session row: its (highlightable) title and the inline close confirm.
sessionRow
  :: MonadWidget t m
  => Dynamic t Bool -> Dynamic t Bool -> Text
  -> Dynamic t (Text, [TmuxWindow])
  -> m (Event t NodeEvent)
sessionRow activeD attnD n vD = do
  let labelAttrs = ffor activeD $ \a ->
        "class" =: ("terminals-label leksah-nav-item" <> if a then " terminals-active" else "")
      -- The session's alert now rides its leading icon (see 'termSessIcon'), so
      -- an alert in a window shows on the (possibly collapsed) session row too —
      -- the name itself is shown plain.
      rawNameD     = fst <$> vD
  -- Label the session by its tmux name (from the poll, so a rename shows up),
  -- while the row is keyed by the stable session id @n@.
  (labelEl, _) <- elDynAttr' "span" labelAttrs $ do
      termSessIcon attnD (snd <$> vD)
      dynText rawNameD
  renE <- renameControl rawNameD (renameTmuxSession n)
  newWinE <- actionBtn "+" "New window in this session"
  killE <- confirmClose
  return $ leftmost [ Right (SelectTerminal n) <$ domEvent Click labelEl
                    , Left <$> renE
                    , Left (newTmuxWindow n) <$ newWinE
                    , Right (CloseTerminal n)  <$ killE ]

-- | The windows of a session (by session id), each expandable into its panes.
windowsTree
  :: MonadWidget t m
  => Text -> Dynamic t [TmuxWindow] -> m (Event t NodeEvent)
windowsTree n windowsD =
  fmapMaybe (listToMaybe . M.elems) <$>
    listViewWithKey (M.fromList . map (\w -> (twIndex w, w)) <$> windowsD)
      (\widx wD ->
        treeItem "terminals-window" False
          (do let attrs = ffor wD $ \w ->
                    "class" =: ("terminals-label leksah-nav-item" <> if twActive w then " terminals-current" else "")
              (e, _) <- elDynAttr' "span" attrs $ do
                    termWinIcon wD
                    dynText (twLabel <$> wD)
              renE <- renameControl (windowRawName <$> wD) (renameTmuxWindow n widx)
              killE <- confirmClose
              return $ leftmost [ Right (SelectTerminalWindow n widx) <$ domEvent Click e
                                , Left <$> renE
                                , Left (killTmuxWindow n widx) <$ killE ])
          (el "ul" $ panesTree n widx (twPanes <$> wD)))

-- | The panes of a window (leaves).
panesTree
  :: MonadWidget t m
  => Text -> Int -> Dynamic t [TmuxPane] -> m (Event t NodeEvent)
panesTree n widx panesD =
  fmapMaybe (listToMaybe . M.elems) <$>
    listViewWithKey (M.fromList . map (\p -> (tpIndex p, p)) <$> panesD)
      (\pidx pD -> el "li" $ do
        let attrs = ffor pD $ \p ->
              "class" =: ("terminals-label leksah-nav-item" <> if tpActive p then " terminals-current" else "")
        (e, _) <- elDynAttr' "span" attrs $ do
              termIcon "tree-pane.svg"
              dynText (tpLabel <$> pD)
        zoomE  <- actionBtn "⤢" "Zoom / unzoom this pane"
        breakE <- actionBtn "↗" "Break this pane out into its own window"
        killE <- confirmClose
        return $ leftmost [ Right (SelectTerminalPane n widx pidx) <$ domEvent Click e
                          , Left (zoomTmuxPane n widx pidx)  <$ zoomE
                          , Left (breakTmuxPane n widx pidx) <$ breakE
                          , Left (killTmuxPane n widx pidx)  <$ killE ])

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

-- | A compact glyph button in a tree row (new window / zoom / break); its click
-- becomes a @Left@ 'NodeEvent' carrying a tmux action that's run and then re-polls
-- the tree, exactly like the kill (✕) control.
actionBtn :: MonadWidget t m => Text -> Text -> m (Event t ())
actionBtn glyph tip = do
  (e, _) <- elAttr' "button" ("class" =: "terminals-action" <> "title" =: tip) $ text glyph
  return (domEvent Click e)

-- | Inline rename: a ✎ button that swaps in a text box prefilled with the
-- current name (captured when the button is clicked, so a background poll can't
-- clobber what you're typing); Enter commits (runs @rename newName@ as a Left
-- action, which re-polls the tree), Esc cancels.  Either collapses back to ✎.
-- Polymorphic in what a commit produces: a local rename yields the tmux @IO ()@
-- to run (bubbled as a @Left@ NodeEvent); a remote one yields a 'TerminalsEvents'
-- (a @Right@) so 'IDE.Web.Main' can run it over ssh and re-poll the host.
renameControl :: MonadWidget t m => Dynamic t Text -> (Text -> a) -> m (Event t a)
renameControl nameD rename = elClass "span" "terminals-rename-slot" $ do
  -- Two states, swapped with widgetHold (which — unlike `dyn` — surfaces the
  -- *initial* widget's events too, so the ✎ click and the commit are actually
  -- wired).  switchDyn is prompt, so a commit fires on the same frame it collapses.
  let editButton = do
        (b, _) <- elAttr' "button" ("class" =: "terminals-action" <> "title" =: "Rename") $ text "✎"
        pure (Just <$> tagPromptlyDyn nameD (domEvent Click b), never)
      editField nm = do
        ti <- textInput $ def
                & textInputConfig_initialValue .~ nm
                & attributes .~ constDyn ("class" =: "terminals-rename")
        -- Focus + select the field once it's in the DOM (a fresh input isn't
        -- focused automatically, so keystrokes/Enter would otherwise never reach
        -- it).  Deferred to the next animation frame so it runs after the ✎
        -- click's own focus handling settles; select() highlights the text so
        -- typing replaces it.
        pb <- getPostBuild
        performEvent_ $ ffor pb $ \_ -> liftJSM . void $
          jsg ("window" :: Text) ^. js1 ("requestAnimationFrame" :: Text)
            (fun $ \_ _ _ -> void $ eval
              ("var i=document.querySelector('.terminals-rename');\
               \ if(i){i.focus();i.select();}" :: Text))
        -- Use keydown, not keypress: keypress is unreliable in WKWebView for
        -- Enter and never fires for Esc (a non-printable key), so the commit /
        -- cancel would silently do nothing.
        let kd = _textInput_keydown ti
            -- Close on Enter/Esc, and also when the field loses focus (clicking
            -- away) — otherwise an abandoned edit box is left behind, and opening
            -- several stacks them up.  Only Enter (below) actually commits.
            blurE  = () <$ ffilter not (updated (_textInput_hasFocus ti))
            closeE = leftmost [ () <$ ffilter (\k -> k == 13 || k == 27) kd, blurE ]
        pure ( Nothing <$ closeE
             , rename <$> tagPromptlyDyn (_textInput_value ti) (ffilter (== 13) kd) )
  rec
    wD <- widgetHold editButton (ffor startStopE (maybe editButton editField))
    let startStopE = switchDyn (fst <$> wD)   -- Just = start editing, Nothing = stop
        commitE    = switchDyn (snd <$> wD)
  pure commitE

-- | The bare window name, dropping the "idx: " prefix that 'twLabel' prepends.
windowRawName :: TmuxWindow -> Text
windowRawName w = case T.breakOn ": " (twLabel w) of
  (_, r) | not (T.null r) -> T.drop 2 r
  _                       -> twLabel w
