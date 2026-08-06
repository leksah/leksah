{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | A tmux control-mode terminal widget (iTerm2-style \"-CC\" support,
-- phase 2): instead of one xterm.js attached to the whole session over a PTY
-- (see 'IDE.Web.Widget.Terminal'), ONE 'IDE.Web.TmuxCC' client speaks the
-- control protocol and each tmux pane of the current window renders as its
-- own xterm.js.  Enabled per new terminal tab by the \"Use tmux control
-- mode\" preference ('terminalControlMode'); @ssh:\/\/host[#session]@ keys
-- (remote terminals) always use it.
--
-- Sizing is FEED-FORWARD, the way iTerm2\/Ghostty do it: the cell size is a
-- known constant (measured once per page from the font, cached in
-- @LeksahTerm.cellMetrics@), the client size sent to tmux is
-- @floor(container \/ cell)@, and each pane is absolutely positioned at its
-- tmux layout rectangle @(x,y,w,h) × cell@ — pane box ≡ pane grid by
-- construction, tmux's separator cells are the gutters, and the only unused
-- space is the sub-cell remainder at the container edge.  Nothing is
-- rendered-then-corrected.
--
-- Panes are KEYED WIDGETS (phase 3): every pane of every window in the
-- session gets one long-lived xterm, keyed by pane id — a layout change just
-- moves/resizes the existing xterms (scrollback, selection and parser state
-- survive), a window switch toggles per-window containers (display:none), so
-- it is instant and stateful, and only a genuinely NEW pane replays (with up
-- to 1000 lines of history seeded into its scrollback).  Hidden windows'
-- xterms keep consuming their %output, so they are always current.
--
-- Each pane also carries the classic widget's affordances: clickable
-- file-path/identifier links, OSC 8 hyperlinks, find-bar search and the
-- bell.  Still to come: underlay holes.
#if defined(ghcjs_HOST_OS)

-- Browser build: there is no tmux, so no control client and no live pane
-- layout to render — but the demo's SESSION-BACKED leksah windows are exactly
-- ONE canned session each ('listTerminalTree' synthesizes one window with one
-- pane per entry of @window.leksahDemoTerminals@).  The split tree therefore
-- collapses to a single pane, and that pane is what the classic widget
-- already draws in the browser: an xterm seeded from the canned dump keyed by
-- the session id ('IDE.Web.DemoTerminals').  So the leksah-window tab
-- delegates to it, keeping the demo's terminals — links, hover and all.
-- SESSIONLESS windows (pure native views — the demo's showcase
-- editor-beside-browser split) render fully via 'IDE.Web.Widget.LwView',
-- which is CPP-free and shared with the native build.
module IDE.Web.Widget.TerminalCC
  ( terminalCCWidget
  ) where

import Data.Text (Text)
import Reflex (Dynamic, Event)
import Reflex.Dom.Core (MonadWidget)
import IDE.Core.State (IDE, TabKey)
import IDE.Web.Events (TerminalEvents)
import IDE.Web.Widget.Terminal (terminalWidget)

-- | The demo's single-pane stand-in for the control-mode window renderer:
-- the session's canned dump, drawn by the classic terminal widget.
terminalCCWidget
  :: forall t m . MonadWidget t m
  => Dynamic t IDE
  -> Text                -- ^ leksah window id — unused: one window, one pane
  -> Text                -- ^ session id, which is also the canned dump's key
  -> Event t ()
  -> (TabKey -> Event t () -> Dynamic t Bool -> m ())
  -> Dynamic t (Maybe (Text, Bool))
  -> (Text -> Bool -> m ())
  -> m (Event t TerminalEvents)
terminalCCWidget ide _lwId sessionId selectedE _leafViewW _closeMenuD _renderCloseMenu =
    terminalWidget ide sessionId selectedE

#else
module IDE.Web.Widget.TerminalCC
  ( terminalCCWidget
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (try, SomeException)
import Control.Lens ((^.))
import Control.Monad (forM, forM_, forever, unless, when, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.Char (isAlphaNum, ord)
import Data.IORef
       (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef,
        writeIORef)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Numeric (showHex)
import System.Process (createProcess, proc)
import Text.Read (readMaybe)

import Reflex
       (Dynamic, Event, attachWith, current, ffilter, ffor, fmapMaybe,
        foldDyn, delay, gate, getPostBuild, holdDyn, holdUniqDyn, leftmost,
        never, newTriggerEvent, performEvent, performEvent_, switchHold,
        tag, updated)
import Reflex.Dom.Core
       (MonadWidget, blank, divClass, domEvent, dyn, dyn_, elAttr,
        elAttr', elDynAttr, elDynAttr', listWithKey, text,
        widgetHold, _element_raw, EventName(Click, Keydown), (=:))
import Language.Javascript.JSaddle
       (JSM, JSVal, MakeObject, fun, js, js0, js1, js2, js3, js4, jsg,
        jss, liftJSM, new, obj, valIsNull, valIsUndefined, valToBool,
        valToNumber, valToText)

import IDE.Core.CTypes (SrcSpan(..))
import IDE.Core.State
       (IDE, TabKey, focusLog, leksahWindows,
        LeksahWindow(..), PaneContent(..), PaneKind(..), LeafId(..),
        readIDE, reflectIDE)
import IDE.Web.Events (TerminalEvents(..))
import IDE.Web.IDERefStore (getGlobalIDERef)
import IDE.Web.WindowBridge (setFocusedLeaf)
import IDE.Web.ReplTmux (tmuxSocket)
import IDE.Web.SplitLayout
       (leafRects, LeafRect(..), treeDividers, NativeDivider(..), resizeNode,
        singlePaneWindow, lwWindowIds, paneForWindow)
import IDE.Web.Widget.LwView (publishLwGeom, modifyLeksahWindow)
import IDE.Web.SnapRequest (requestSnapPane)
import IDE.Web.TerminalInput
       (registerTerminalCC, unregisterTerminalCC, registerCCStop,
        unregisterCCStop, registerTerminalSplits,
        unregisterTerminalSplits, registerTerminalFocus, unregisterTerminalFocus,
        isActiveTerminal)
import IDE.Web.HostFlags (getBrowserHosted, flipHintText)
import IDE.Web.JsaddleTunnel
       (registerTunnelSync, unregisterTunnelSync, tunnelSyncReply)
import IDE.Web.TmuxCC
import IDE.Web.ThreadPriority (ThreadPriority(..), forkPriorityThread)
import IDE.Web.Widget.Menu (menu)
import IDE.Web.Widget.Metadata (lookupIdentLocations)
import qualified IDE.LSP as LSP
import qualified Language.Javascript.JSaddle.Terminal.Protocol as P

-- | Leksah-window widget: renders ONE leksah window (a wide0 tab) — its
-- native split tree of panes, each pane a whole tmux window (its tmux panes
-- at their exact layout rectangles) or a native view.  One control client on
-- the window's backing tmux session (a session backing several leksah
-- windows gets one client per tab).
terminalCCWidget
  :: forall t m . MonadWidget t m
  => Dynamic t IDE
  -> Text                -- ^ leksah window id (\"lw-3\") — the tab identity
                         --   and the key into 'leksahWindows'
  -> Text                -- ^ backing tmux session id (\"$3\") or
                         --   \"ssh://host[#target]\"
  -> Event t ()          -- ^ fires when this tab is selected
  -> (TabKey -> Event t () -> Dynamic t Bool -> m ())
                         -- ^ builds a native VIEW PANE ('PaneView' — an editor /
                         --   git log living directly in the split layout, no
                         --   tmux pane underneath); the event is \"take keyboard
                         --   focus now\" (fired by the focus reconciler for the
                         --   focused leaf only), the Dynamic is \"this leaf is
                         --   the window's focused leaf\" (gates grab-on-create)
  -> Dynamic t (Maybe (Text, Bool))
                         -- ^ ⌘W close-menu target: (pane %id, multi-pane?), so
                         --   the matching pane renders the menu inside itself
  -> (Text -> Bool -> m ())
                         -- ^ render the close menu for (pane %id, multi-pane?)
  -> m (Event t TerminalEvents)
terminalCCWidget ide lwId sessionId selectedE leafViewW closeMenuD renderCloseMenu = do
    pb <- getPostBuild
    (evE, fireEv) <- newTriggerEvent
    (ccStartedE, fireCCStarted) <- newTriggerEvent
    -- Pane-level affordances (same contract as the classic widget): a clicked
    -- project-file path asks to open it; a Ctrl/Cmd-clicked identifier asks
    -- for a metadata lookup; the bell raises attention.  The panes are keyed
    -- widgets deep inside listWithKey, so they report through these fires.
    (linkE, triggerLink) <- newTriggerEvent
    (lookupE, triggerLookup) <- newTriggerEvent
    (bellE, triggerBell) <- newTriggerEvent
    -- Hovering a file link in a pane's output asks the LSP layer for a tooltip
    -- (file diagnostics + a symbol hover when a line is known); the reply is
    -- fired here and pushed back into JS (LeksahTermLinks.resolveHover) on this
    -- window's own reflex network — never from the LSP client thread.
    (hoverRespE, fireHoverResp) <- newTriggerEvent
    -- A pane split/kill inside the session widget changes the pane set (and so a
    -- window's pane count); fired from there to poke the IDE's tree poll at once
    -- (see 'paneSetChangedE' use in the return / TerminalTreeChanged).
    (paneSetChangedE, firePaneSetChanged) <- newTriggerEvent
    -- The session's *current* window just closed (fired from inside when a
    -- %window-close names the window csCurrent still points at); the IDE picks
    -- the ⌘1 button rather than tmux's default replacement.
    (activeWinClosedE, fireActiveWinClosed) <- newTriggerEvent
    -- The leksah user gave a pane focus from within leksah (a ⌘-number split
    -- select — fired from the session widget below).  Carries the tmux pane id;
    -- bubbles up as 'TerminalPaneFocused' to float the pane to the flipper MRU.
    (paneFocusE, firePaneFocus) <- newTriggerEvent
    -- A (remote) connection dropped: carries the human-readable reason (ssh/tmux
    -- stderr + the %exit reason) to show in place of the pane, so the tab doesn't
    -- just vanish; 'retryE' re-runs the connection when the user clicks Retry.
    (connErrE, fireConnErr) <- newTriggerEvent
    (retryE, fireRetry) <- newTriggerEvent
    (closeErrE, fireCloseErr) <- newTriggerEvent
    -- Whether the Retry/error page is currently shown in place of the panes
    -- (set when a connection drops, cleared when one (re)connects).  Lets us
    -- tell "the user navigated to a tab already showing the Retry page" apart
    -- from ordinary tab selection.
    inErrorD <- holdDyn False $ leftmost [ True <$ connErrE, False <$ ccStartedE ]
    -- "Went to the Retry page": it appeared (connErrE — flag False, the IDE
    -- floats it only if it's the visible tab) OR the user deliberately selected
    -- this tab while it was showing (gate on inErrorD — flag True, always float).
    let connErrShownWE = leftmost
          [ TerminalConnErrShown False <$  connErrE
          , TerminalConnErrShown True  <$  gate (current inErrorD) selectedE ]
    -- A Retry attempt actually reconnected: the FIRST real session data (a
    -- layout notification) to arrive after a Retry click.  A failed ssh retry
    -- produces only EvExit (→ the error view again), never a layout, so this
    -- stays silent for it.  'armedD' is set by each Retry and cleared by that
    -- first layout, so 'reconnectedE' fires exactly once per successful retry
    -- (never during ordinary session activity, when no Retry preceded it).
    let layoutE = fmapMaybe layoutOf evE
    armedD <- foldDyn ($) False $ leftmost [ const True <$ retryE, const False <$ layoutE ]
    let reconnectedE = gate (current armedD) layoutE
    let isRemote = "ssh://" `T.isPrefixOf` sessionId
        -- The error view shown in place of the panes when a (remote) connection
        -- drops: the reason, plus Retry (re-run the connection) and Close (drop
        -- the tab).  Fills the tab; no absolute positioning needed since it
        -- REPLACES the session UI via the widgetHold below.
        -- 'tabindex=-1' so the view itself can hold focus and receive key
        -- events; Retry is focused as soon as it appears (below) so the whole
        -- thing is keyboard-driven: Enter/Space on the focused button, Tab
        -- between Retry and Close, and Escape closes the tab.
        connErrorView msg = do
            (box, _) <- elAttr' "div"
                      ("class" =: "terminal-cc-error" <> "tabindex" =: "-1"
                      <> "style" =: ("height:100%;box-sizing:border-box;overflow:auto"
                                     <> ";padding:14px;background:var(--leksah-bg);color:var(--leksah-fg-muted)"
                                     <> ";font:13px/1.5 Menlo,Monaco,monospace")) $ do
                elAttr "div" ("style" =: "color:#ff7b72;font-weight:bold;margin-bottom:8px") $
                    text ("Connection to " <> sessionId <> " failed")
                elAttr "pre" ("style" =: "white-space:pre-wrap;margin:0 0 14px 0") $ text msg
                (rb, _) <- elAttr' "button"
                    ("style" =: "padding:4px 12px;margin-right:8px;cursor:pointer") $ text "Retry"
                (cb, _) <- elAttr' "button"
                    ("style" =: "padding:4px 12px;cursor:pointer") $ text "Close"
                -- Focus Retry when the error view appears, so a keyboard user
                -- lands on it without hunting for the buttons.  Re-assert once
                -- after a beat: the widgetHold swap tears down the old xterm just
                -- after this builds, and removing a focused xterm bounces focus
                -- to <body>, so the immediate focus alone is lost.  0.1s beats
                -- the teardown yet is too quick for the user to have Tabbed away.
                pbErr <- getPostBuild
                reErr  <- delay 0.1 pbErr
                performEvent_ $ ffor (leftmost [pbErr, reErr]) $ \_ -> liftJSM $
                    void $ _element_raw rb ^. js0 ("focus" :: Text)
                performEvent_ $ liftIO (fireRetry ())    <$ domEvent Click rb
                performEvent_ $ liftIO (fireCloseErr ()) <$ domEvent Click cb
            -- Escape (from anywhere in the view — it bubbles from the buttons)
            -- closes the tab, mirroring the Close button.
            performEvent_ $ liftIO (fireCloseErr ())
                <$ ffilter (== 27) (domEvent Keydown box)
    -- jsaddle-terminal tunnels (vendor/jsaddle-terminal): a pane app can
    -- handshake over its own stdout/stdin (OSC-5799 frames in %output,
    -- RS frames injected via send-keys -H) and render as an iframe over its
    -- pane.  tunnelEvE = per-pane iframe generation (Just gen = (re)build,
    -- Nothing = tunnel closed); batchEvE = BATCH JSON bound for the iframe.
    -- The scanner runs in the drain thread (pure IO — the sync path must
    -- never wait on the browser), so these refs are its shared state.
    (tunnelEvE, fireTunnelEv) <- newTriggerEvent
    (batchEvE, fireBatchEv) <- newTriggerEvent
    tunnelsRef <- liftIO $ newIORef (M.empty :: M.Map PaneId TunnelInfo)
    scansRef <- liftIO $ newIORef (M.empty :: M.Map PaneId P.OscScan)
    closedRef <- liftIO $ newIORef (S.empty :: S.Set PaneId)
    -- Id of this widget's control-client teardown registration (see
    -- registerCCStop), read back by the EvExit cleanup below.
    ccStopIdRef <- liftIO $ newIORef (0 :: Integer)
    -- Registration ids of this widget's TerminalInput registry entries (CC
    -- runner / split selector / focus callbacks): id-guarded so this widget's
    -- EvExit can never unregister a REPLACEMENT widget's entries for the same
    -- session (several leksah windows can share one session).
    ccRegIdRef     <- liftIO $ newIORef (0 :: Integer)
    splitsRegIdRef <- liftIO $ newIORef (0 :: Integer)
    focusRegIdsRef <- liftIO $ newIORef ([] :: [(Text, Integer)])
    -- \"Take keyboard focus\" pulses for the native VIEW leaves, fired by the
    -- focus reconciler below for the focused leaf only (each leaf's widget
    -- filters by its own id).
    (viewFocusE, fireViewFocus) <- newTriggerEvent
    let paneCbs = PaneCallbacks
          { pcLink   = triggerLink
          , pcLookup = triggerLookup
          , pcBell   = triggerBell ()
          , pcHover  = fireHoverResp
          }

    -- Start the control client once the widget exists; drain its events into
    -- reflex from a background thread.  "ssh://host" attaches-or-creates the
    -- remote "leksah" session; "ssh://host#$3" attaches that exact session;
    -- otherwise it's a local session id on leksah's socket.  ssh runs without
    -- a PTY (plain pipes), so the protocol stream is identical either way.
    performEvent_ $ ffor (leftmost [pb, retryE]) $ \_ -> liftIO . void . forkIO $ do
        cc <- case T.stripPrefix "ssh://" sessionId of
            Just hostTarget ->
                let (host, hash) = T.breakOn "#" hostTarget
                    attach = case T.stripPrefix "#" hash of
                        Just target | not (T.null target) ->
                            ["attach-session", "-t", T.unpack target]
                        _ -> ["new-session", "-A", "-s", "leksah"]
                    -- ssh joins the remote command's words with spaces and hands
                    -- them to the login shell, so a tmux session id like "$0"
                    -- (what the Terminals tree keys a listed remote session by)
                    -- would be expanded by the remote shell — to the shell's own
                    -- name — unless every word is single-quoted for it.  Without
                    -- this, attaching any listed remote session ("ssh://host#$N")
                    -- ran `attach-session -t <shellname>`, no such session, and
                    -- the connection closed at once.  Same escaping as 'sshTmux'.
                    shellQuote s = "'" <> concatMap esc s <> "'"
                    esc '\'' = "'\\''"
                    esc c    = [c]
                    remoteCmd = unwords (map shellQuote ("tmux" : "-C" : attach))
                in startCCWith
                    [ "ssh", "-o", "ConnectTimeout=10", "-o", "BatchMode=yes"
                    , T.unpack host, remoteCmd ]
            Nothing -> startCC ["-L", tmuxSocket] ["attach-session", "-t", T.unpack sessionId]
        -- Batched drain: everything queued is taken at once and consecutive
        -- same-pane output merged, so a scroll-storm burst is ONE reflex
        -- event + ONE xterm.write instead of thousands (which starved the
        -- jsaddle bridge and made typing choppy).  Every pane's output is
        -- sieved through the jsaddle-terminal frame scanner here — before
        -- pausedRef routing, so flow-control pause/replay (which replays
        -- screen TEXT, never raw escapes) can neither eat nor duplicate a
        -- frame; residual bytes flow on as ordinary EvOutput.
        -- Priority-raised bound thread: this drain feeds the jsaddle bridge and
        -- xterm, so keeping it scheduled under heavy background CPU load is what
        -- keeps typing/scrolling smooth while compilations run.
        drainTid <- forkPriorityThread High . forever $ ccEventsBatch cc >>= mapM_
                 (routeTunnelEv cc sessionId tunnelsRef scansRef closedRef
                                fireEv fireTunnelEv fireBatchEv)
                 . coalesceOutputs
        -- Reap any control client left for this LEKSAH WINDOW by the OS window
        -- its tab just moved away from (reflex-dom gives this widget no
        -- destructor; see registerCCStop): stop drops the drain thread first so
        -- the detach can't push a spurious %exit into a dead network.  Keyed by
        -- leksah window id — several leksah windows on one session each keep
        -- their own client by design.
        myStopId <- registerCCStop lwId (killThread drainTid >> stopCC cc)
        writeIORef ccStopIdRef myStopId
        -- NB initialSync runs from the session widget below, NOT here: its
        -- layout events would race the widgetHold swap — the foldDyn that
        -- consumes them doesn't exist yet, and events fired before it builds
        -- are dropped (seen as permanently blank panes under startup load).
        fireCCStarted cc

    -- The current tunnel generation of each pane (drives the per-pane iframe
    -- widgets), and BATCH forwarding into the iframes (all-async JSM; order
    -- is preserved by jsaddle's command channel).
    tunnelGenD <- foldDyn
        (\(p, mg) m -> maybe (M.delete p m) (\g -> M.insert p g m) mg)
        M.empty tunnelEvE
    -- Live xterm instances of this widget, keyed by pane id — disposed and
    -- re-created when the layout re-renders (dyn_ gives no destructors, so
    -- the previous generation is torn down explicitly).  Also the OWNERSHIP
    -- map for output routing: with several leksah windows on one session,
    -- only the widget rendering a pane may act on its %output (see below).
    termsRef <- liftIO $ newIORef (M.empty :: M.Map PaneId JSVal)

    -- Panes carrying a leksah view overlay (an editor / git log converted to
    -- a pane): shared IDE state, so every OS window renders the same overlay.
    -- The IORef mirror is for the focus helpers below (plain JSM, no reflex).
    -- Ownership-gated like %output: the tunnel key is global, so a sibling
    -- leksah window's client would otherwise run every batch a second time.
    performEvent_ $ ffor batchEvE $ \(pane, json) -> do
        terms <- liftIO $ readIORef termsRef
        when (M.member pane terms) . liftJSM . void $
            jsg ("LeksahJsaddlePane" :: Text) ^. js2 ("runBatch" :: Text)
                (tunnelUrlKey sessionId pane) json
    -- LSP hover reply -> fill the pane's floating tooltip (this window's context).
    performEvent_ $ ffor hoverRespE $ \(rid, mt) -> liftJSM . void $
        jsg ("LeksahTermLinks" :: Text) ^. js2 ("resolveHover" :: Text)
            (rid :: Int) (fromMaybe "" mt)

    -- Panes being (re)synced by a capture-based replay: output routing per
    -- pane is Normal (absent), 'PauseDropping' (stale pre-capture output is
    -- discarded), or 'PauseGotCap' (capture arrived, waiting for the state
    -- line; post-capture output is buffered to apply AFTER the replay).
    -- Driven by 'requestReplay' — used for a fresh xterm's initial fill AND
    -- for tmux flow control: with @pause-after@ set, tmux pauses any pane
    -- we fall >1s behind on (%pause) rather than queueing unbounded output,
    -- and we jump ahead to the current screen instead of replaying the
    -- backlog (what iTerm2 does).
    pausedRef <- liftIO $ newIORef (M.empty :: M.Map PaneId (UTCTime, PauseState))

    -- The session widget proper appears once the client is up.
    _ <- widgetHold (divClass "terminal-cc-empty" $ text "(connecting…)") $
      ffor (leftmost [Right <$> ccStartedE, Left <$> connErrE]) $ \case
       Left errMsg -> connErrorView errMsg
       Right cc -> do
            -- Register this tab's control channel for the Terminal menu's
            -- pane commands (split/select/resize/…): they run verbatim —
            -- the control client's current window/pane IS the displayed one.
            liftIO $ registerTerminalCC sessionId (ccSend cc)
                       >>= writeIORef ccRegIdRef
            -- A tunnel closed (the app sent BYE or died): the xterm — which
            -- kept consuming non-frame output all along — comes back into
            -- view; refresh it from the pane's current screen.
            performEvent_ $ ffor (fmapMaybe
                    (\(p, mg) -> case mg of Nothing -> Just p; _ -> Nothing)
                    tunnelEvE) $ \p -> liftIO $ do
                terms <- readIORef termsRef
                when (M.member p terms) $
                    requestReplay cc pausedRef ReplayScreenOnly p
            -- Initial sync now that this widget (and its foldDyn below) exists
            -- and is subscribed — see the race note above.
            pbSync <- getPostBuild
            performEvent_ $ ffor pbSync $ \_ ->
                liftIO . void . forkIO $ initialSync cc fireEv
            -- A remote connection dropping (ssh/tmux exited) shows WHY in place
            -- of the panes, rather than the tab silently vanishing: gather the
            -- captured stderr and the %exit reason and swap in the error view.
            when isRemote $
                performEvent_ $ ffor (fmapMaybe (\case EvExit r -> Just r; _ -> Nothing) evE) $
                    \reason -> liftIO $ do
                        errTxt <- ccStderrText cc
                        fireConnErr (formatConnErr reason errTxt)
            -- Window/layout state folded from notifications.
            stD <- foldDyn ($) (CCState M.empty Nothing Nothing) $ leftmost
                [ ffor (fmapMaybe layoutOf evE) $ \(w, l) s ->
                      s { csLayouts = M.insert w l (csLayouts s) }
                , ffor (fmapMaybe closedWin evE) $ \w s ->
                      s { csLayouts = M.delete w (csLayouts s) }
                , ffor (fmapMaybe sessionWinOf evE) $ \(sess, w) s ->
                      -- The attached session's current window changed.  Accept it
                      -- when it matches the initialSync wildcard (""), the local
                      -- $id / ssh:// label (sessionId), OR — crucially for remote
                      -- tabs, whose notifications carry the REMOTE tmux's own $id
                      -- rather than the ssh:// label — the attached session id
                      -- learned from %session-changed (csSession).  Without the
                      -- last clause a remote window switch (select-window) is
                      -- silently dropped and every remote window shows the same one.
                      if sess == "" || sess == sessionId || Just sess == csSession s
                        then s { csCurrent = Just w }
                        else s
                -- Attaching to a different session (switch-client): adopt it,
                -- and — only when it genuinely changed from a prior session —
                -- drop the old session's windows so the re-sync below repaints
                -- just the new one's.  The first attach merely records it (the
                -- postBuild initialSync already loaded that session's windows).
                , ffor (fmapMaybe (\case EvSessionChanged s _ -> Just s; _ -> Nothing) evE) $ \s c ->
                      case csSession c of
                          Just old | old /= s -> c { csSession = Just s
                                                   , csLayouts = M.empty
                                                   , csCurrent = Nothing }
                          _                   -> c { csSession = Just s }
                ]
            -- On a genuine session switch, re-query the now-current session's
            -- windows/panes (tmux pushes none on switch-client).  Driven off
            -- csSession changing to a NON-first value — the initial attach is
            -- handled by the postBuild initialSync.
            sessSwitchE <- do
                sessD <- holdUniqDyn (csSession <$> stD)
                pure $ fmapMaybe id (tag (current sessD) (updated sessD))
            performEvent_ $ ffor sessSwitchE $ \_ ->
                liftIO . void . forkIO $ initialSync cc fireEv
            -- The cell size is a page-wide constant (font metrics, measured
            -- once and cached); fetch it before rendering so pane rectangles
            -- come from real metrics, never estimates.
            metricsE <- performEvent $ ffor pbSync $ \_ -> liftJSM getCellMetrics
            metricsD <- holdUniqDyn =<< holdDyn Nothing (Just <$> metricsE)

            -- Feed-forward client sizing: floor(container / cell), sent to
            -- tmux whenever it changes (initial build + ResizeObserver).
            -- Besides the client size, the size is also pinned onto the
            -- DISPLAYED window ('ccResizeWindow'): with @window-size latest@
            -- another attached (regular) client wins the window size as soon
            -- as the user types in it — a control client can never become
            -- \"latest\" — and our view would be clipped by a row or column;
            -- the per-window size clamps the window to our view regardless.
            lastSizeRef <- liftIO $ newIORef (0 :: Int, 0 :: Int)
            containerRef <- liftIO $ newIORef Nothing
            -- Per-LEAF window clamps (refresh-client -C @win:WxH): each stack
            -- leaf clamps exactly its ACTIVE window to the leaf's own grid, so
            -- different leaves hold different windows at different sizes at
            -- once.  All clamp traffic is serialised through one lock so
            -- clear-old/set-new can't interleave; the registry lets a dying
            -- leaf's clamp be cleared (reflex gives leaves no destructors).
            leafClampsRef <- liftIO $ newIORef (M.empty :: M.Map LeafId WindowId)
            clampLock <- liftIO $ newMVar ()
            -- Windows this client has already pinned to manual sizing: the
            -- set-option round trip runs once per window, not once per
            -- resize step (a divider drag is a storm of clamp updates).
            manualWinsRef <- liftIO $ newIORef (S.empty :: S.Set WindowId)
            let clampLeaf :: LeafId -> Maybe (WindowId, Int, Int) -> IO ()
                clampLeaf lid mb = withMVar clampLock $ \_ -> do
                    old <- atomicModifyIORef' leafClampsRef $ \m ->
                        ( maybe (M.delete lid m)
                                (\(w', _, _) -> M.insert lid w' m) mb
                        , M.lookup lid m )
                    let clearOld ow = do
                            ccClearWindowSize cc ow
                            atomicModifyIORef' manualWinsRef $ \s ->
                                (S.delete ow s, ())
                    case (old, mb) of
                      (Just ow, Just (nw, _, _)) | ow /= nw -> clearOld ow
                      (Just ow, Nothing)                    -> clearOld ow
                      _                                     -> return ()
                    forM_ mb $ \(w', c, r) -> do
                        fresh <- atomicModifyIORef' manualWinsRef $ \s ->
                            (S.insert w' s, not (S.member w' s))
                        when fresh $ ccSetWindowManual cc w'
                        ccResizeWindow cc w' c r
                -- The plain client size stays container-sized at the global
                -- font: a ceiling for windows no leaf currently clamps.
                applySize :: Int -> Int -> IO ()
                applySize cols rows = ccResize cc cols rows
                refit :: JSM ()
                refit = do
                    mbC <- liftIO $ readIORef containerRef
                    forM_ mbC $ \c -> do
                        w <- valToNumber =<< c ^. js ("clientWidth" :: Text)
                        h <- valToNumber =<< c ^. js ("clientHeight" :: Text)
                        (cw, ch) <- getCellMetrics
                        -- Reserve a uniform 'terminalPanePad' inset on every
                        -- side of every pane by shrinking the whole grid by
                        -- 2·pad per axis (styleOf shifts it back by pad and pads
                        -- each pane), so the padding never eats into a cell.
                        let pad  = terminalPanePad
                            cols = max 20 (floor ((w - 2*pad) / cw) :: Int)
                            rows = max 5 (floor ((h - 2*pad) / ch) :: Int)
                        when (w > 0 && h > 0) $ do
                            changed <- liftIO $ atomicModifyIORef' lastSizeRef $ \old ->
                                ((cols, rows), old /= (cols, rows))
                            when changed . liftIO . void . forkIO $ applySize cols rows
                -- Repaint a pane's xterm on the next animation frame.  xterm.js
                -- cannot render into a zero-size element, so a pane first built
                -- while its window/tab was hidden (display:none) has its content
                -- buffered but never painted — it shows blank until forced to
                -- redraw (a resize, e.g. a manual split, was the only trigger).
                -- Deferred to rAF so display:block has been applied first.
                repaintTerm :: JSVal -> JSM ()
                repaintTerm term = void $
                    jsg ("window" :: Text) ^. js1 ("requestAnimationFrame" :: Text)
                        (fun $ \_ _ _ -> do
                            rows <- valToNumber =<< term ^. js ("rows" :: Text)
                            void $ term ^. js2 ("refresh" :: Text)
                                (0 :: Int) (max 0 (round rows - 1) :: Int))
            -- (The old displayed-window clamp mover lived here; clamps are
            -- now per leaf — see 'clampLeaf' above.  The old follow-tmux
            -- handler — %session-window-changed activating a stack member —
            -- went with the stacks: every owned tmux window is visible in
            -- its own pane now, so tmux's "current window" no longer drives
            -- what this tab displays.)
            -- The window's active tmux pane, highlighted by turning the
            -- divider lines on its perimeter green.  Tracked via
            -- %window-pane-changed (also triggered by our own focus →
            -- select-pane), applied to the .terminal-cc-hl segments that
            -- 'renderHlSegments' lays (hidden) along every pane's gutter edges.
            activePaneRef <- liftIO $ newIORef (Nothing :: Maybe PaneId)
            -- Set by an explicit focus request (a repl launched into this session
            -- via the workspace tree): the window/pane it selects arrives
            -- asynchronously, and the keyboard is still on whatever launched it, so
            -- the usual "only follow focus if it was already ours" gate would skip
            -- it.  While set, the deferred window/pane update focuses regardless;
            -- see 'registerTerminalFocus' below.
            pendingFocusRef <- liftIO $ newIORef False
            -- A pane the focus reconciler wants the keyboard in whose xterm
            -- does not exist yet (a fresh ⌘D split): consumed by 'paneWidget'
            -- the moment that xterm mounts — the deterministic replacement
            -- for polling until the pane becomes focusable.
            pendingMountRef <- liftIO $ newIORef (Nothing :: Maybe PaneId)
            -- Is the window's focused leaf a VIEW leaf (browser \/ editor \/ git
            -- log) rather than a tmux one?  Then the active-pane ring belongs to
            -- that leaf's own '.pane-chrome' and NO tmux pane may draw one.
            -- tmux's active pane is a property of the SESSION: it doesn't move
            -- when leksah focus goes to a view leaf, so its ring stayed lit and
            -- the window showed TWO active panes — with the terminal's crisp
            -- full ring reading as the active one, clicking a browser pane (or
            -- its address bar) looked like it did nothing at all.
            viewFocusedRef <- liftIO $ newIORef False
            -- The leksah window this tab renders (shared model state); also
            -- gates output ownership and the focus paths below.
            lwOwnD <- holdUniqDyn $ M.lookup lwId . (^. leksahWindows) <$> ide
            let applyActive :: JSM ()
                applyActive = do
                    mbC <- liftIO $ readIORef containerRef
                    mbP <- liftIO $ readIORef activePaneRef
                    viewFoc <- liftIO $ readIORef viewFocusedRef
                    forM_ mbC $ \c ->
                        applyPaneHighlight c (if viewFoc then Nothing else mbP)
                containerHasFocus :: JSM Bool
                containerHasFocus = do
                    mbC <- liftIO $ readIORef containerRef
                    case mbC of
                      Nothing -> return False
                      Just c  -> do
                        ae <- jsg ("document" :: Text) ^. js ("activeElement" :: Text)
                        valToBool =<< c ^. js1 ("contains" :: Text) ae
                -- Does the ACTIVE pane specifically hold the keyboard (not just
                -- some pane in the container)?  A fresh ⌘D split makes the NEW
                -- pane active before its xterm widget is built, while the OLD
                -- pane still holds focus — so 'containerHasFocus' is true too
                -- early and the retry below would stop before the split is
                -- focusable.  Returns False while the active pane's xterm does
                -- not yet exist, so the retry waits for it; for a tunnel pane
                -- (hidden xterm, iframe holds the keyboard) it falls back to
                -- container focus.
                activePaneHasFocus :: JSM Bool
                activePaneHasFocus = do
                    mbP <- liftIO $ readIORef activePaneRef
                    case mbP of
                      Nothing -> containerHasFocus
                      Just p  -> do
                        tunnels <- liftIO $ readIORef tunnelsRef
                        if M.member p tunnels
                          then containerHasFocus
                          else do
                            terms <- liftIO $ readIORef termsRef
                            case M.lookup p terms of
                              Nothing   -> return False
                              Just term -> do
                                el <- term ^. js ("element" :: Text)
                                nul <- valIsNull el
                                if nul then return False else do
                                  ae <- jsg ("document" :: Text) ^. js ("activeElement" :: Text)
                                  valToBool =<< el ^. js1 ("contains" :: Text) ae
                focusActivePane :: JSM ()
                focusActivePane = do
                    mbP <- liftIO $ readIORef activePaneRef
                    terms <- liftIO $ readIORef termsRef
                    focusLog $ "[" <> T.unpack sessionId <> "] focusActivePane -> term.focus() pane="
                        <> show (T.unpack <$> mbP)
                    forM_ mbP $ \p -> do
                        -- The xterm (hidden and a no-op focus for a tunnel pane)
                        forM_ (M.lookup p terms) $ \term ->
                            void $ term ^. js0 ("focus" :: Text)
                        -- …and the tunnel iframe, if this pane has one (the JS
                        -- side no-ops when the pane isn't a tunnel or already
                        -- holds focus — see leksahJsaddlePaneJs).
                        void $ jsg ("LeksahJsaddlePane" :: Text)
                            ^. js1 ("focus" :: Text) (tunnelUrlKey sessionId p)
                    -- Safety net: if the keyboard still isn't in this terminal (no
                    -- active pane recorded yet, or its xterm wasn't focusable),
                    -- focus the visible container's textarea directly — so
                    -- activation never dead-ends on <body> waiting for a pane id.
                    --
                    -- BUT only when the active pane has NO target of its own to
                    -- focus (mbP unrecorded, or its xterm/tunnel not built yet).
                    -- The querySelector grabs the FIRST .xterm-helper-textarea in
                    -- the container — in a split that is the wrong pane, and
                    -- focusing it fires that pane's focus→select-pane, flipping
                    -- tmux's active pane away from the one we're activating.  That
                    -- one mis-focus desyncs "focused pane" from "tmux active pane"
                    -- and the two re-assert against each other forever (the focus
                    -- oscillation).  When mbP DOES have a term/tunnel we already
                    -- focused it above; focusActivePaneSoon retries on later frames
                    -- if it wasn't laid out yet, so we must NOT fall back here.
                    tunnels <- liftIO $ readIORef tunnelsRef
                    let haveActiveTarget =
                          maybe False (\p -> M.member p terms || M.member p tunnels) mbP
                    inFocus <- containerHasFocus
                    unless (inFocus || haveActiveTarget) $ do
                        mbC <- liftIO $ readIORef containerRef
                        forM_ mbC $ \c -> do
                            ta <- c ^. js1 ("querySelector" :: Text)
                                    (".xterm-helper-textarea" :: Text)
                            nul <- valIsNull ta
                            unless nul $ void $ ta ^. js0 ("focus" :: Text)
                -- Focusing an element is a no-op until it is actually visible, and
                -- selecting a tab flips it visibility:hidden→visible via a DOM
                -- write that jsaddle-wkwebview dispatches asynchronously — so a
                -- fixed number of rAFs races the flip (the focus lands on <body>,
                -- hence the "click twice" symptom).  Instead retry each animation
                -- frame until the focus actually sticks (the ACTIVE pane holds
                -- it), for up to ~0.5s.  Waiting on 'activePaneHasFocus' rather
                -- than mere container focus also covers a fresh ⌘D split, whose
                -- new pane's xterm is created a beat after it becomes active
                -- (see that helper).  The loop stops the instant it succeeds, so
                -- once the pane has the keyboard it can't be yanked back later.
                focusActivePaneSoon :: JSM ()
                focusActivePaneSoon = do
                    focusLog $ "[" <> T.unpack sessionId <> "] focusActivePaneSoon START"
                    let go n = do
                            focusActivePane
                            ok <- activePaneHasFocus
                            if ok
                              then focusLog $ "[" <> T.unpack sessionId
                                     <> "] focusActivePaneSoon DONE (stuck) at n=" <> show n
                              else when (n > (0 :: Int)) $
                                void $ jsg ("window" :: Text)
                                    ^. js1 ("requestAnimationFrame" :: Text)
                                        (fun $ \_ _ _ -> go (n - 1))
                     in go (30 :: Int)
                -- Highlight the new active pane and, IF this terminal already
                -- owned the keyboard, hand the keyboard to it too: activating
                -- a pane (menu select-split, a fresh ⌘D split) should mean
                -- typing goes there — but a background session's pane change
                -- must not steal focus from the editor.  Focus orphaned on
                -- <body> counts as ours: killing the focused pane (`exit`,
                -- kill-pane) disposes its xterm and drops the keyboard there
                -- (focus() inside a hidden background tab is a no-op, so this
                -- can't steal from a visible editor).
                followActive :: JSM ()
                followActive = do
                    applyActive
                    had <- containerHasFocus
                    ae <- jsg ("document" :: Text) ^. js ("activeElement" :: Text)
                    tagName <- valToText =<< ae ^. js ("tagName" :: Text)
                    mbP <- liftIO $ readIORef activePaneRef
                    focusLog $ "[" <> T.unpack sessionId <> "] followActive pane="
                        <> show (T.unpack <$> mbP) <> " had=" <> show had
                        <> " activeEl=" <> T.unpack tagName
                        <> " -> " <> (if had || tagName == "BODY" then "focusActivePaneSoon" else "no-focus")
                    -- focusActivePaneSoon (not focusActivePane): on a ⌘D split
                    -- the new pane is active before its xterm exists, so retry
                    -- until it is focusable — an immediate focus would land on
                    -- the old pane (which still holds the keyboard) and stop.
                    when (had || tagName == "BODY") focusActivePaneSoon
            -- A window switch hides the focused pane's container
            -- (display:none), which silently drops keyboard focus onto
            -- <body>.  The switch handler below asks tmux for the NEW
            -- window's active pane and this restores highlight + focus —
            -- focussing only when the keyboard was ours (still inside the
            -- container, or orphaned on <body> by the hide).
            (winFocusE, fireWinFocus) <- newTriggerEvent
            performEvent_ $ ffor winFocusE $ \p -> do
                liftIO $ writeIORef activePaneRef (Just p)
                liftJSM $ do
                    applyActive
                    had <- containerHasFocus
                    ae <- jsg ("document" :: Text) ^. js ("activeElement" :: Text)
                    tagName <- valToText =<< ae ^. js ("tagName" :: Text)
                    pend <- liftIO $ readIORef pendingFocusRef
                    -- A pending request only claims the keyboard while this is
                    -- still the terminal on screen — a slow (remote) connection
                    -- whose window finally arrives after the user moved on must
                    -- not steal focus (had/BODY still cover "we already own it").
                    desired <- liftIO $ isActiveTerminal sessionId
                    let want = had || tagName == "BODY" || (pend && desired)
                    focusLog $ "[" <> T.unpack sessionId <> "] winFocusE pane=" <> T.unpack p
                        <> " had=" <> show had <> " activeEl=" <> T.unpack tagName
                        <> " pend=" <> show pend <> " desired=" <> show desired
                        <> " -> " <> (if want then "focusActivePaneSoon" else "no-focus")
                    when want $ do
                        -- Soon + the mount hook, not the one-shot: the pane's
                        -- xterm may not be built/visible yet (a remote window
                        -- arriving, a window switch mid-mount) — the one-shot
                        -- silently dropped the keyboard on <body> then.
                        liftIO $ writeIORef pendingMountRef (Just p)
                        focusActivePaneSoon
                        liftIO $ writeIORef pendingFocusRef False
            -- Explicit focus requests (a workspace repl button launches a repl in
            -- this session): flag it so the imminent window switch focuses the new
            -- pane past the gate, and — for the case where the window is already
            -- current, or the switch already arrived — force a focus a moment later
            -- (the flag is then still set only if nothing consumed it).
            (focusReqE, fireFocusReq) <- newTriggerEvent
            -- Registered under BOTH the session id (repl-into-session paths)
            -- and the leksah-window id (⌥-open / ⌘D / open-into-leaf paths,
            -- which know the window, and — with several windows on one
            -- session — must reach THIS window, not whichever registered the
            -- session key last).  Requests are sticky (see
            -- 'IDE.Web.TerminalInput'), so no caller needs retry timers.
            liftIO $ do
                i1 <- registerTerminalFocus sessionId (fireFocusReq ())
                i2 <- registerTerminalFocus lwId (fireFocusReq ())
                writeIORef focusRegIdsRef [(sessionId, i1), (lwId, i2)]
            performEvent_ $ ffor focusReqE $ \_ ->
                liftIO $ writeIORef pendingFocusRef True
            -- A fresh connection can take a while to bring its window/panes up —
            -- a remote ssh one especially (handshake + the initial layout query),
            -- seconds after this first 0.15s attempt.  Until it does there is no
            -- pane to focus, so retry the forced focus on a widening schedule
            -- until the active pane's xterm actually takes the keyboard, then
            -- clear the pending flag.  Each attempt is gated on this STILL being
            -- leksah's active (shown) terminal ('isActiveTerminal'), so a slow
            -- connection that only completes after the user has moved on never
            -- steals focus (a hidden tab's focus() would no-op regardless — but
            -- the check also stops us pointlessly retrying).
            forcedFocusE <- fmap leftmost . forM [0.15, 0.5, 1.0, 2.0, 3.5, 5.0] $
                              \d -> delay d focusReqE
            performEvent_ $ ffor forcedFocusE $ \_ -> do
                pend    <- liftIO $ readIORef pendingFocusRef
                desired <- liftIO $ isActiveTerminal sessionId
                when (pend && desired) $ liftJSM $ do
                    applyActive
                    focusActivePane
                    -- Only give up the pending flag once the keyboard has really
                    -- landed in the active pane; otherwise a later retry gets it
                    -- when the (still-connecting) pane finally exists.
                    ok <- activePaneHasFocus
                    when ok . liftIO $ writeIORef pendingFocusRef False
            -- ⌘N (numbered split navigation): the displayed window's panes in
            -- layout (reading) order — the numbering the ⌘-held badges show.
            -- Kept in an IORef so the selector (invoked from outside reflex,
            -- via the TerminalInput registry) can read it; the selection runs
            -- back through a trigger event so it can focus the pane too
            -- (navigating TO a pane hands it the keyboard, unlike a
            -- background %window-pane-changed).
            curPanesRef <- liftIO $ newIORef ([] :: [PaneId])
            curPanesD <- holdUniqDyn $ ffor stD $ \s ->
                case csCurrent s >>= (`M.lookup` csLayouts s) of
                    Just l  -> [ p | (p, _, _, _, _) <- layoutPanes l ]
                    Nothing -> []
            performEvent_ $ ffor (updated curPanesD) $ liftIO . writeIORef curPanesRef
            (selSplitE, fireSelSplit) <- newTriggerEvent
            liftIO $ registerTerminalSplits sessionId fireSelSplit
                       >>= writeIORef splitsRegIdRef
            performEvent_ $ ffor selSplitE $ \n -> do
                panes <- liftIO $ readIORef curPanesRef
                case drop (n - 1) panes of
                  (p : _) | n >= 1 -> do
                      focusLog $ "[" <> T.unpack sessionId <> "] selSplitE n=" <> show n
                          <> " pane=" <> T.unpack p <> " -> select-pane + focusActivePane"
                      liftIO $ ccSend cc ("select-pane -t " <> p)
                      liftIO $ writeIORef activePaneRef (Just p)
                      -- Leksah-issued select: float this pane to the flipper MRU.
                      liftIO $ firePaneFocus p
                      liftJSM $ do
                          applyActive
                          focusActivePane
                  _ -> return ()
            -- Layout changes (splits, divider drags) rebuild the highlight
            -- boxes hidden; the rebuild's own postBuild reapplies, but belt
            -- and braces: reapply again shortly after the dust settles.
            layoutSettledE <- delay 0.15 (fmapMaybe layoutOf evE)
            performEvent_ $ ffor layoutSettledE $ \_ -> liftJSM applyActive
            -- Scope guards: tmux broadcasts %session-window-changed (and can
            -- surface other sessions' window events) to every control client,
            -- so act only on our own session's — a foreign pane id in
            -- activePaneRef breaks the highlight, and a foreign window id
            -- would be queried/focused wrongly (see 'currentWin').
            performEvent_ $ ffor (attachWith (\(st, mlw) ev -> (st, mlw, ev))
                                    ((,) <$> current stD <*> current lwOwnD)
                                    evE) $ \(st, mlw, ev) -> case ev of
                EvWindowPaneChanged w p | w `M.member` csLayouts st -> do
                    old <- liftIO $ readIORef activePaneRef
                    focusLog $ "[" <> T.unpack sessionId <> "] EvWindowPaneChanged win="
                        <> T.unpack w <> " pane=" <> T.unpack p
                        <> " (was " <> show (T.unpack <$> old) <> ") -> writeActive+followActive"
                    liftIO $ writeIORef activePaneRef (Just p)
                    liftJSM followActive
                -- The displayed window itself closed (its last pane exited): tmux
                -- will pick a replacement by its own rule, but the IDE overrides
                -- that to the ⌘1 button.  Sampled csCurrent still names it here
                -- (the %session-window-changed that moves it arrives in a later
                -- frame), so this fires only for the *active* window's close.
                _ | Just w <- closedWin ev, csCurrent st == Just w ->
                    liftIO $ fireActiveWinClosed ()
                EvSessionWindowChanged s w
                  | s == "" || s == sessionId || Just s == csSession st ->
                    case mlw of
                      -- A leksah-window tab: the session's current window is
                      -- MODEL state here — record the owning leaf as focused
                      -- and let the focus reconciler (below) turn that into
                      -- DOM focus.  A window some OTHER leksah window owns is
                      -- none of our business: acting on it wrote a foreign
                      -- pane into activePaneRef, whose focus fallback then
                      -- grabbed the FIRST textarea — the mis-focus that seeds
                      -- the focus↔select-pane oscillation.
                      Just lw -> forM_ (paneForWindow w lw) $ \l ->
                          liftIO . void . forkIO $ setFocusedLeaf lwId l
                      -- Remote tabs (no model): classic follow-the-current-
                      -- window behaviour.
                      Nothing -> liftIO . void . forkIO $ do
                        r <- ccCommand cc ("display-message -p -t " <> w
                                           <> " -F '#{pane_id}'")
                        case r of
                          Right (ln : _) | p <- T.strip ln, not (T.null p) ->
                              fireWinFocus p
                          _ -> return ()
                -- Flow control: tmux paused a pane we fell behind on.  If we
                -- render it (all panes of the session, now), jump ahead to
                -- its current screen; otherwise leave it paused (its xterm
                -- gets a requestReplay — which resumes — on creation).
                EvPause p -> liftIO $ do
                    terms <- readIORef termsRef
                    when (M.member p terms) $
                        requestReplay cc pausedRef ReplayScreenOnly p
                _ -> return ()
            -- Selecting this tab: repaint every pane — the tab is revealed by
            -- a visibility:hidden→visible flip (see Tabs.hs) that no resize
            -- observer sees, so an xterm whose window container was hidden
            -- when it was last drawn gets a nudge here.  Keyboard focus is
            -- the reconciler's job (it has a selected arm and focuses the
            -- FOCUSED leaf — which may be an editor view, not a pane); only a
            -- model-less remote tab keeps the classic focus-on-select here.
            performEvent_ $ ffor (tag (current lwOwnD) selectedE) $ \mlw -> liftJSM $ do
                focusLog $ "[" <> T.unpack sessionId <> "] tab selectedE -> repaint"
                    <> (if isNothing mlw then " + focusActivePaneSoon (remote)" else "")
                terms <- liftIO $ readIORef termsRef
                forM_ (M.elems terms) repaintTerm
                when (isNothing mlw) focusActivePaneSoon
            -- Panes that left the session (kill-pane, window closed): their
            -- keyed widgets are torn down by listWithKey below, but the
            -- xterms are JS objects we own — dispose and unregister them.
            allPanesD <- holdUniqDyn $ ffor stD $ \s -> S.fromList
                [ p | l <- M.elems (csLayouts s), (p, _, _, _, _) <- layoutPanes l ]
            -- PER-PANE OUTPUT GATING: tmux sends every pane's %output to ALL
            -- of a session's control clients, but this widget only displays
            -- its own leksah window's panes — the rest is dead weight (parsed,
            -- decoded, then dropped by the ownership filter below).  Tell
            -- tmux to stop sending them to THIS client (refresh-client -A
            -- "%p:off" is per-client; the owning window's client still gets
            -- everything).  'requestReplay' re-enables (":on") whenever a
            -- widget of ours creates a pane's xterm, so panes gated off here
            -- recover when ownership changes (stray adoption, conversion
            -- re-hosts, the remote current-window switch).  Remote tabs have
            -- no map entry and display tmux's current window — gate to that.
            let ownedWinsOf s mlw = case mlw of
                    Just lw -> S.fromList (lwWindowIds lw)
                    Nothing -> maybe S.empty S.singleton (csCurrent s)
            foreignPanesD <- holdUniqDyn $
                (\s mlw -> S.fromList
                    [ p | (w, l) <- M.toList (csLayouts s)
                        , not (w `S.member` ownedWinsOf s mlw)
                        , (p, _, _, _, _) <- layoutPanes l ])
                  <$> stD <*> lwOwnD
            performEvent_ $ ffor
                (attachWith (\old new -> S.toList (S.difference new old))
                    (current foreignPanesD) (updated foreignPanesD)) $
                \newlyForeign -> liftIO $ do
                    -- NEVER gate off a pane whose xterm we render: the
                    -- foreign set is derived from the shared model, which can
                    -- lag the tmux events by a frame — an \":off\" landing
                    -- after the pane's mount-time \":on\" left the pane DEAF
                    -- (typing echoed nothing until an incidental replay).
                    -- termsRef is the ground truth of \"we render it\", read
                    -- at send time on the same frame thread as the mount.
                    terms <- readIORef termsRef
                    forM_ (filter (`M.notMember` terms) newlyForeign) $ \p -> do
                        focusLog $ "[" <> T.unpack sessionId
                            <> "] output gate OFF pane=" <> T.unpack p
                        ccSend cc ("refresh-client -A \"" <> p <> ":off\"")
            -- ══ MODEL-DRIVEN FOCUS RECONCILER ═══════════════════════════════
            -- 'lwFocused' is the single within-window focus authority: every
            -- mutation writes it (⌘D/⌥-open splits, flip commits, ⌘W closes,
            -- the reconcile's neighbour succession, DOM focusin) — and THIS
            -- is the one place that turns it into DOM keyboard focus.  Runs
            -- when the model changes, when the tab is selected (one frame
            -- later, so the visibility flip has landed), when an explicit
            -- focus request arrives (the sticky registry), and once at build
            -- for the restored state.  Guarded like 'followActive': only when
            -- the keyboard was already ours, orphaned on <body>, or
            -- explicitly requested — a background tab's model change can
            -- never steal from an editor (and focus() inside a hidden tab is
            -- a no-op regardless).
            focusedContentD <- holdUniqDyn $ (\mlw -> do
                    lw <- mlw
                    l  <- lwFocused lw
                    pc <- M.lookup l (lwPanes lw)
                    pure (l, pcKind pc)) <$> lwOwnD
            -- Keep 'viewFocusedRef' (which gates the tmux ring, above) in step
            -- with the focused leaf's KIND, and re-apply the highlight — this is
            -- what turns the terminal's ring off when a view leaf takes focus,
            -- and back on when a tmux leaf does.
            performEvent_ $ ffor
                (leftmost [updated focusedContentD, tag (current focusedContentD) pbSync]) $
                \mc -> do
                    liftIO . writeIORef viewFocusedRef $ case mc of
                        Just (_, PaneView{}) -> True
                        _                    -> False
                    liftJSM applyActive
            (paneResolvedE, firePaneResolved) <- newTriggerEvent
            selectedSettledE <- delay 0 selectedE
            -- One frame after the request, so the pendingFocusRef write (its
            -- own focusReqE handler above) is in place before we read it.
            focusReqSettledE <- delay 0 focusReqE
            -- Selecting the tab is an EXPLICIT navigation: the keyboard
            -- follows into the focused leaf unconditionally (the classic
            -- behaviour) — the ours-or-body guard applies only to the model
            -- and build arms, where a background change must not steal from
            -- e.g. the workspace tree.
            -- The pulse arms (tab select, focus request, build) carry no
            -- content: the handler reads 'lwFocused' from the shared model
            -- AT EXECUTION TIME rather than tagging a frame-start sample —
            -- a flip commit writes lwFocused and selects the tab in the
            -- same frame, and the stale sample made this focus the OLD
            -- leaf, whose async focusin then raced (and sometimes beat)
            -- the flip's write: the flipper landed on the previous pane.
            let readFocusedContent = getGlobalIDERef >>= \case
                    Nothing   -> pure Nothing
                    Just ideR -> (`reflectIDE` ideR) $ do
                        lws <- readIDE leksahWindows
                        pure $ do
                            lw <- M.lookup lwId lws
                            l  <- lwFocused lw
                            pc <- M.lookup l (lwPanes lw)
                            pure (l, pcKind pc)
                reconcileFocusE = leftmost
                  [ fmap ((,) False . Just) (fmapMaybe id (updated focusedContentD))
                  , (True, Nothing)  <$ selectedSettledE
                  , (False, Nothing) <$ leftmost [focusReqSettledE, pbSync]
                  ]
            performEvent_ $ ffor reconcileFocusE $ \(forced, mgiven) -> do
              mcontent <- maybe (liftIO readFocusedContent) (pure . Just) mgiven
              forM_ mcontent $ \(lid, kind) -> do
                had <- liftJSM containerHasFocus
                ae <- liftJSM $ jsg ("document" :: Text) ^. js ("activeElement" :: Text)
                tagName <- liftJSM $ valToText =<< ae ^. js ("tagName" :: Text)
                pend <- liftIO $ readIORef pendingFocusRef
                desired <- liftIO $ isActiveTerminal sessionId
                let want = forced || had || tagName == "BODY" || (pend && desired)
                focusLog $ "[" <> T.unpack sessionId <> "] reconcileFocus kind="
                    <> (case kind of PaneView{} -> "view"
                                     PaneTmux w -> "tmux " <> T.unpack w)
                    <> " had=" <> show had <> " activeEl=" <> T.unpack tagName
                    <> " pend=" <> show pend
                    <> " -> " <> (if want then "focus" else "no-focus")
                when want $ do
                  liftIO $ writeIORef pendingFocusRef False
                  case kind of
                    PaneView _ -> do
                        liftIO $ writeIORef pendingMountRef Nothing
                        liftIO $ fireViewFocus lid
                    -- Resolve the tmux window's ACTIVE pane on the control
                    -- channel (off the frame thread); the focus itself runs
                    -- back on the reflex thread below.
                    PaneTmux w -> liftIO . void . forkIO $ do
                        r <- ccCommand cc ("display-message -p -t " <> w
                                           <> " -F '#{pane_id}'")
                        case r of
                          Right (ln : _) | p <- T.strip ln, not (T.null p) ->
                              firePaneResolved p
                          _ -> return ()
            performEvent_ $ ffor paneResolvedE $ \p -> do
                liftIO $ writeIORef activePaneRef (Just p)
                -- Deterministic handoff when the pane's xterm isn't built yet
                -- (a fresh ⌘D split): 'paneWidget' consumes this the moment
                -- it mounts; 'focusActivePaneSoon' covers the already-built
                -- case (and the visibility flip of a just-selected tab).  An
                -- already-mounted pane must NOT arm the mount hook — a later
                -- rebuild of its xterm (a font change) would steal focus.
                terms <- liftIO $ readIORef termsRef
                liftIO $ writeIORef pendingMountRef
                    (if p `M.member` terms then Nothing else Just p)
                liftJSM $ do
                    applyActive
                    focusActivePaneSoon
            -- The pane set changed (split/kill): poke the IDE's tree poll so the
            -- ⌘-number offset and tab badges track the new pane count at once
            -- (resize leaves the set unchanged, so this stays quiet).
            performEvent_ $ ffor (updated allPanesD) $ \_ -> liftIO (firePaneSetChanged ())
            performEvent_ $ ffor (updated allPanesD) $ \alive -> do
                gone <- liftIO $ atomicModifyIORef' termsRef $ \m ->
                    let (keep, dead) = M.partitionWithKey (\k _ -> k `S.member` alive) m
                    in (keep, dead)
                liftIO $ modifyIORef' pausedRef
                    (M.filterWithKey (\k _ -> k `S.member` alive))
                -- Tunnels of departed panes: drop the state and sync route
                -- (the iframe widget is torn down with its keyed paneWidget).
                goneTunnels <- liftIO $ atomicModifyIORef' tunnelsRef $ \m ->
                    let (keep, dead) = M.partitionWithKey (\k _ -> k `S.member` alive) m
                    in (keep, M.keys dead)
                liftIO $ forM_ goneTunnels $ \p -> do
                    unregisterTunnelSync (tunnelUrlKey sessionId p)
                    fireTunnelEv (p, Nothing)
                liftJSM $ do
                    forM_ (M.toList gone) $ \(p, term) -> do
                        void $ term ^. js0 ("dispose" :: Text)
                        void $ jsg ("LeksahTerm" :: Text)
                            ^. js1 ("unregister" :: Text) (paneKey sessionId p)
                    -- Disposing the focused pane's xterm orphans the keyboard
                    -- on <body>; hand it to the (new) active pane — but ONLY if the
                    -- recorded active pane is still live.  Right after killing the
                    -- focused pane (e.g. `exit`), its %window-pane-changed may not
                    -- have arrived yet, so activePaneRef can still name the dead
                    -- pane; focusActivePane would then fall through to its "focus
                    -- the first textarea" safety net and select pane 1 — which
                    -- fights tmux's own new-active-pane select, leaving the active
                    -- pane oscillating between the two.  Skip here in that case; the
                    -- imminent %window-pane-changed sets the right pane and its
                    -- followActive (BODY branch) focuses it.
                    when (not (M.null gone)) $ do
                        ae <- jsg ("document" :: Text) ^. js ("activeElement" :: Text)
                        tagName <- valToText =<< ae ^. js ("tagName" :: Text)
                        mbP <- liftIO $ readIORef activePaneRef
                        liveTerms <- liftIO $ readIORef termsRef
                        let liveActive = maybe False (`M.member` liveTerms) mbP
                        when (tagName == "BODY" && liveActive) focusActivePane
            -- Split-tree geometry for the ⌘-drag pane-move preview.  Straight
            -- from the shared map (NOT lwD): a remote tab's synthesized
            -- window must publish null, not a fake single-pane universe.
            geomD <- holdUniqDyn $
                (\i -> (\lw -> (lwTree lw, lwZoomed lw))
                         <$> M.lookup lwId (i ^. leksahWindows)) <$> ide
            pbGeom <- getPostBuild
            performEvent_ $
                ffor (leftmost [updated geomD, tag (current geomD) pbGeom]) $
                    liftJSM . publishLwGeom lwId
            (containerEl, _) <- elAttr' "div"
                -- Pull back over the .area-wide{0,1} 3px left/top padding
                -- (negative margins + matching size bump) so this container —
                -- and thus the pane/divider/shadow geometry laid inside it —
                -- has its origin on the side/top edge line, not 3px in.  The
                -- text gap the padding gives is re-added per edge-pane in
                -- 'styleOf' (padding-left/top on the char-0/row-0 panes) so the
                -- boxes still reach the line while the text stays clear of it.
                ("class" =: "terminal terminal-cc"
                 <> "data-lw" =: lwId
                 <> "style" =: ("position:relative;overflow:hidden"
                                <> ";margin-left:-3px;margin-top:-3px"
                                <> ";width:calc(100% + 3px);height:calc(100% + 3px)")) $
                -- The cell size is a page constant; everything below builds
                -- once it is known.  One container per WINDOW, all built and
                -- kept (hidden ones display:none, their xterms staying
                -- current from %output), each holding a KEYED widget per
                -- pane: layout changes only move/resize the existing xterms.
                dyn_ $ ffor metricsD $ \case
                    Nothing -> divClass "terminal-cc-empty" $ text "(connecting…)"
                    -- (The measured GLOBAL cell size gates the build — leaves
                    -- measure their own per-font cells now, so the value
                    -- itself is unused here.)
                    Just _cell -> do
                        let windowsD = csLayouts <$> stD
                            currentD = csCurrent <$> stD
                        -- The leksah window this tab renders, straight from
                        -- the shared map (Main's reconcile keeps it valid; a
                        -- pane whose tmux window just died renders empty for
                        -- the moment until the reconcile collapses it, and a
                        -- vanished leksah window renders nothing — its tab is
                        -- about to close).  REMOTE (ssh://) tabs have no map
                        -- entry: they synthesize a single full-area pane
                        -- following tmux's current window — the classic
                        -- one-window-at-a-time view, with zero persistence
                        -- (layout mutations no-op on the absent map entry).
                        lwD <- holdUniqDyn $
                            (\i cur -> case M.lookup lwId (i ^. leksahWindows) of
                                Just lw -> Just lw
                                Nothing -> (\c -> singlePaneWindow Nothing
                                              (PaneContent (PaneTmux c) Nothing))
                                             <$> cur)
                              <$> ide <*> currentD
                        let rectsD = maybe M.empty
                                       (\lw -> leafRects (lwZoomed lw) (lwTree lw))
                                     <$> lwD
                            pct v = T.pack (show (v * (100 :: Double))) <> "%"
                            leafStyle :: LeafId -> LeafRect -> M.Map Text Text
                            leafStyle (LeafId n) r =
                                  -- edge-left marks a leaf flush with the
                                  -- container's left edge; the glow overlay's
                                  -- border-left gating keys on it (terminalCss).
                                  "class" =: ("terminal-cc-leaf"
                                              <> (if lrX r == 0 then " edge-left" else ""))
                               <> "data-leaf" =: T.pack (show n)
                               <> "style" =: ("position:absolute;box-sizing:border-box"
                                    <> ";left:"   <> pct (lrX r)
                                    <> ";top:"    <> pct (lrY r)
                                    <> ";width:"  <> pct (lrW r)
                                    <> ";height:" <> pct (lrH r)
                                    <> ";display:" <> (if lrVisible r then "block" else "none"))
                        -- One keyed widget per PANE (leaf); splits/ratio
                        -- changes/zooms only restyle existing leaves (xterms
                        -- survive).
                        _ <- listWithKey rectsD $ \lid rectD0 -> do
                            rectD <- holdUniqDyn rectD0
                            paneD <- holdUniqDyn $
                                (>>= (M.lookup lid . lwPanes)) <$> lwD
                            -- The pane's tmux window (Nothing for a view pane).
                            tmuxD <- holdUniqDyn $
                                (\case Just (PaneContent (PaneTmux w) _) -> Just w
                                       _                                 -> Nothing)
                                  <$> paneD
                            -- A VIEW pane (an editor / git log living directly
                            -- in the layout) renders as a covering layer over
                            -- the (then empty) terminal chrome, so the tmux
                            -- machinery needs no structural dispatch.
                            viewD <- holdUniqDyn $
                                (\case Just (PaneContent (PaneView k) _) -> Just k
                                       _                                 -> Nothing)
                                  <$> paneD
                            -- The pane's font-size override (⌘+/⌘−; Nothing =
                            -- follow the global monospace pref).
                            fontD <- holdUniqDyn $ (>>= pcFontSize) <$> paneD
                            (leafEl, _) <- elDynAttr' "div" (leafStyle lid <$> rectD) $ do
                              -- Per-pane FONT: the body below (xterms,
                              -- geometry, clamps) is built for one cell size,
                              -- so a font-size change rebuilds it — a rare
                              -- user action, and requestReplay refills the
                              -- fresh xterms exactly as when a window moves
                              -- between panes.  The override's cell size is
                              -- measured first (the metricsD pattern).
                              dyn_ $ ffor fontD $ \mbFont -> do
                                pbFont <- getPostBuild
                                cellLE <- performEvent $ ffor pbFont $ \_ ->
                                    liftJSM (getCellMetricsFor mbFont)
                                cellLD <- holdUniqDyn =<< holdDyn Nothing (Just <$> cellLE)
                                dyn_ $ ffor cellLD $ \mbCell -> forM_ mbCell $ \cellL -> do
                                  -- The pane body: its tmux window's panes at
                                  -- their tmux layout rectangles.  Keyed by
                                  -- window id so the (rare) case of a leaf
                                  -- changing windows tears down cleanly.
                                  (bodyEl, _) <- elAttr' "div" ("class" =: "terminal-cc-leaf-body") $ do
                                    let winsOfLeafD = (\mbW wins -> maybe M.empty
                                              (\w -> M.restrictKeys wins (S.fromList [w])) mbW)
                                            <$> tmuxD <*> windowsD
                                    _ <- listWithKey winsOfLeafD $ \_wid layD -> do
                                        -- Visibility follows the LEAF (a
                                        -- zoomed sibling hides it); the pane
                                        -- has one window, always shown.
                                        let visD = lrVisible <$> rectD
                                        elAttr "div"
                                            ("class" =: "terminal-cc-window"
                                             <> "style" =: "position:absolute;left:0;top:0;right:0;bottom:0") $ do
                                            layUniqD <- holdUniqDyn layD
                                            let panesD = (\l -> M.fromList
                                                    [ (p, (x, y, w, h))
                                                    | (p, x, y, w, h) <- layoutPanes l ])
                                                  <$> layUniqD
                                                -- Layout size in cells: a pane at the
                                                -- layout's right/bottom edge fills to the
                                                -- container edge (see paneWidget).
                                                dimsD = (\l -> (lW l, lH l)) <$> layUniqD
                                            _ <- listWithKey panesD $ \pane rectD' ->
                                                paneWidget cc sessionId paneCbs termsRef
                                                           pausedRef tunnelsRef activePaneRef
                                                           pendingMountRef cellL mbFont pane rectD'
                                                           dimsD (M.lookup pane <$> tunnelGenD)
                                                           (void (ffilter (== pane) paneFocusE))
                                                           closeMenuD renderCloseMenu
                                            -- Repaint this window's panes when the leaf
                                            -- comes back into view (un-zoom): their xterms
                                            -- may have been built hidden (display:none) and
                                            -- so never painted (see 'repaintTerm').  The
                                            -- un-hide doesn't resize the container, so the
                                            -- ResizeObserver below won't cover this.
                                            performEvent_ $
                                                ffor (tag (current layUniqD) (ffilter id (updated visD))) $ \l ->
                                                    liftJSM $ do
                                                        terms <- liftIO $ readIORef termsRef
                                                        forM_ (layoutPanes l) $ \(p, _, _, _, _) ->
                                                            forM_ (M.lookup p terms) repaintTerm
                                            -- Dividers, highlight segments and shortcut
                                            -- badges are plain divs — cheap to rebuild
                                            -- per layout change.
                                            dyn_ $ ffor layUniqD $ \l -> do
                                                renderDividers cc cellL l
                                                renderHlSegments cellL l
                                                renderShortcutBadges cellL l
                                                pbHl <- getPostBuild
                                                performEvent_ $ ffor pbHl $ \_ ->
                                                    liftJSM applyActive
                                    return ()
                                  -- Per-leaf sizing: measure the leaf body (its
                                  -- own ResizeObserver — the leaf resizes with
                                  -- divider drags and splits, not just the
                                  -- container) and clamp the pane's tmux
                                  -- window to this leaf's grid (see 'clampLeaf').
                                  (leafSizeE, fireLeafSize) <- newTriggerEvent
                                  pbLeaf <- getPostBuild
                                  performEvent_ $ ffor pbLeaf $ \_ -> liftJSM $ do
                                      let (cw, ch) = cellL
                                          measure = do
                                            w <- valToNumber =<< _element_raw bodyEl ^. js ("clientWidth" :: Text)
                                            h <- valToNumber =<< _element_raw bodyEl ^. js ("clientHeight" :: Text)
                                            let pad  = terminalPanePad
                                                cols = max 20 (floor ((w - 2*pad) / cw) :: Int)
                                                rows = max 5 (floor ((h - 2*pad) / ch) :: Int)
                                            when (w > 0 && h > 0) . liftIO $
                                                fireLeafSize (cols, rows)
                                      measure
                                      roL <- new (jsg ("ResizeObserver" :: Text))
                                                 (fun $ \_ _ _ -> measure)
                                      void $ roL ^. js1 ("observe" :: Text) (_element_raw bodyEl)
                                  leafSizeD <- holdUniqDyn =<< holdDyn (0, 0) leafSizeE
                                  clampTargetD <- holdUniqDyn $
                                      (\mbW (c, r) vis ->
                                            if vis && c > 0 then (\w -> (w, c, r)) <$> mbW
                                                            else Nothing)
                                        <$> tmuxD <*> leafSizeD <*> (lrVisible <$> rectD)
                                  performEvent_ $ ffor (updated clampTargetD) $ \mb ->
                                      liftIO . void . forkIO $ clampLeaf lid mb
                              -- The view leaf's covering layer (see viewD).
                              -- Its font override rides a CSS-var wrapper
                              -- (CodeMirror reads --leksah-mono-size live);
                              -- Monaco snapshots its font at creation, so a
                              -- change also pokes editors inside via
                              -- leksahSetLeafFont (0 = back to the global).
                              -- Focus: the leaf receives ITS OWN pulse from
                              -- the focus reconciler (never the shared tab
                              -- select — every view grabbing the keyboard on
                              -- tab select is how focus became a race), and
                              -- grabs-on-create only while it IS the focused
                              -- leaf.
                              amFocusedD <- holdUniqDyn $
                                  (\mlw -> (lwFocused =<< mlw) == Just lid) <$> lwD
                              dyn_ $ ffor viewD $ \case
                                  Nothing -> return ()
                                  Just k  -> do
                                    (vEl, _) <- elDynAttr' "div"
                                        ((\mf -> "class" =: "terminal-cc-view-leaf"
                                             <> maybe mempty
                                                  (\n -> "style" =:
                                                     ("--leksah-mono-size:"
                                                      <> T.pack (show n) <> "px")) mf)
                                          <$> fontD) $ leafViewW k
                                                (void (ffilter (== lid) viewFocusE))
                                                amFocusedD
                                    -- The view leaf's chrome: always-on grey
                                    -- ring; the focused leaf's ring brightens
                                    -- (gated by the tab's .tab-active — see
                                    -- terminalCss ".pane-chrome").  Tmux
                                    -- leaves get theirs per pane instead
                                    -- ('renderHlSegments').
                                    elDynAttr "div"
                                        ((\f -> "class" =: ("pane-chrome"
                                            <> (if f then " active" else "")))
                                          <$> amFocusedD)
                                        blank
                                    performEvent_ $ ffor (updated fontD) $ \mf ->
                                        liftJSM . void $ jsg ("window" :: Text)
                                            ^. js2 ("leksahSetLeafFont" :: Text)
                                                (_element_raw vEl)
                                                (maybe (0 :: Int) id mf)
                            -- Focus entering this leaf makes it the layout's
                            -- focused leaf — the target of ⌘S/⌘+/⌘−/splits.
                            -- A mouse-DOWN counts too, because a click does not
                            -- always MOVE DOM focus: while a native browser view
                            -- holds the keyboard the page's activeElement stays
                            -- on whatever it was (typically the neighbouring
                            -- terminal's textarea), so clicking back into that
                            -- terminal re-focuses an element that never lost
                            -- focus — WebKit fires nothing and the focused leaf
                            -- stayed on the browser pane.
                            pbFoc <- getPostBuild
                            performEvent_ $ ffor pbFoc $ \_ -> liftJSM $ do
                                let el  = _element_raw leafEl
                                    hit = fun $ \_ _ _ -> liftIO . void . forkIO $
                                              setFocusedLeaf lwId lid
                                void $ el ^. js2 ("addEventListener" :: Text)
                                    ("focusin" :: Text) hit
                                void $ el ^. js2 ("addEventListener" :: Text)
                                    ("mousedown" :: Text) hit
                        -- A leaf that left the layout (close/merge) must not
                        -- leave its window clamped (reflex tears the widget
                        -- down without a destructor).
                        performEvent_ $ ffor (updated rectsD) $ \rs ->
                            liftIO . void . forkIO $ do
                                m <- readIORef leafClampsRef
                                forM_ (M.keys m) $ \l ->
                                    unless (l `M.member` rs) $ clampLeaf l Nothing
                        -- (Windows shown by OTHER leksah windows need no
                        -- attention from this client: clamps are MANUAL
                        -- window sizes now — see 'ccResizeWindow' — pinned
                        -- by the owning tab against every other client.)
                        -- Native dividers between leaves: a cheap layer
                        -- rebuilt per tree-shape change.  Dragging one calls
                        -- back with the px delta on mouseup (pure JS —
                        -- jsaddle events are async, so the drag itself never
                        -- goes through Haskell), converted to a node-relative
                        -- fraction and folded into the shared layout.
                        let dividersD = maybe [] (treeDividers . lwTree) <$> lwD
                        dividersUniqD <- holdUniqDyn dividersD
                        dyn_ $ ffor dividersUniqD $ mapM_ $ \nd -> do
                            let styleND =
                                    "position:absolute;z-index:6"
                                    <> (if ndVertical nd
                                          then ";cursor:col-resize;width:7px"
                                            <> ";left:calc(" <> pct (ndX nd) <> " - 3px)"
                                            <> ";top:" <> pct (ndY nd)
                                            <> ";height:" <> pct (ndLen nd)
                                          else ";cursor:row-resize;height:7px"
                                            <> ";top:calc(" <> pct (ndY nd) <> " - 3px)"
                                            <> ";left:" <> pct (ndX nd)
                                            <> ";width:" <> pct (ndLen nd))
                                -- The visible 1px mid-grey separator centred
                                -- in the 7px grab strip (the CC tmux dividers'
                                -- .divider-line, same technique).
                                lineStyle
                                  | ndVertical nd =
                                      "position:absolute;left:3px;top:0;bottom:0;width:1px"
                                  | otherwise =
                                      "position:absolute;top:3px;left:0;right:0;height:1px"
                            (dEl, _) <- elAttr' "div"
                                ("class" =: "terminal-cc-native-divider"
                                 <> "style" =: styleND) $
                                elAttr "div" ("class" =: "divider-line"
                                              <> "style" =: lineStyle) blank
                            pbD <- getPostBuild
                            performEvent_ $ ffor pbD $ \_ -> liftJSM $ do
                                let raw = _element_raw dEl
                                raw ^. jss ("__leksahNativeResize" :: Text)
                                    (fun $ \_ _ args -> case args of
                                      (dv : _) -> do
                                        d <- valToNumber dv
                                        mbC <- liftIO $ readIORef containerRef
                                        forM_ mbC $ \c -> do
                                          rect <- c ^. js0 ("getBoundingClientRect" :: Text)
                                          ext <- valToNumber =<< rect ^. js
                                              (if ndVertical nd then "width" :: Text
                                                                else "height")
                                          let deltaFrac = d / max 1 (ext * ndAxis nd)
                                          liftIO . void . forkIO $
                                              modifyLeksahWindow lwId $ \lw ->
                                                  lw { lwTree = resizeNode (ndPath nd)
                                                                 (ndIndex nd) deltaFrac
                                                                 (lwTree lw) }
                                      _ -> return ())
                                void $ jsg ("LeksahNativeDrag" :: Text)
                                    ^. js2 ("arm" :: Text) raw (ndVertical nd)
                        return ()
            liftIO $ writeIORef containerRef (Just (_element_raw containerEl))
            pb2 <- getPostBuild
            performEvent_ $ ffor pb2 $ \_ -> liftJSM $ do
                refit
                -- The container going display:none→block (its wide0 tab being
                -- selected) fires the observer with a 0→N size change; repaint
                -- every pane then, since 'refit' alone dedupes by size and would
                -- skip a re-show at the same dimensions, leaving panes blank.
                ro <- new (jsg ("ResizeObserver" :: Text)) (fun $ \_ _ _ -> do
                        refit
                        terms <- liftIO $ readIORef termsRef
                        forM_ (M.elems terms) repaintTerm)
                void $ ro ^. js1 ("observe" :: Text) (_element_raw containerEl)

    -- Route %output to the pane's xterm (via the pause states above), and
    -- assemble capture-based replays from their stream-ordered replies.
    -- OWNERSHIP FILTER: several leksah windows can show ONE tmux session
    -- (one control client each), and tmux sends every pane's %output to ALL
    -- of the session's clients — but the xterm registry is global, so
    -- without the filter every widget writes every byte and each pane's
    -- xterm receives its output once PER LEKSAH WINDOW of the session.
    -- Double-applied output scrambles any TUI whose redraws are
    -- cursor-relative (ink/Claude Code) and duplicates plain log lines.
    -- Only the widget whose pane map holds the xterm may write.
    performEvent_ $ ffor evE $ \case
        EvOutput pane dat -> do
            terms <- liftIO $ readIORef termsRef
            when (M.member pane terms) $ do
              st <- liftIO $ readIORef pausedRef
              case M.lookup pane st of
                Nothing            -> do
                    -- Latency trace: the display half of the KEY record above.
                    focusLog $ "[" <> T.unpack sessionId <> "] ECHO pane="
                        <> T.unpack pane <> " bytes=" <> show (BS.length dat)
                    liftJSM $ writePane sessionId pane dat
                Just (_, PauseDropping) -> return ()   -- stale: the capture will include it
                Just (t0, PauseGotCap cap buf) -> liftIO $
                    writeIORef pausedRef (M.insert pane (t0, PauseGotCap cap (dat : buf)) st)
        EvReply rtag res
          | Just p <- T.stripPrefix "cap:" rtag -> liftIO $ case res of
              Right ls -> modifyIORef' pausedRef
                  -- Keep the replay's START time: the staleness escape hatch in
                  -- 'requestReplay' measures the whole cap→cur round trip.
                  (\st -> M.insert p ( maybe (posixSecondsToUTCTime 0) fst (M.lookup p st)
                                     , PauseGotCap ls [] ) st)
              Left e   -> do   -- give up: resume raw
                  focusLog $ "[" <> T.unpack sessionId <> "] replay cap FAILED pane="
                      <> T.unpack p <> ": " <> T.unpack (T.strip e)
                  modifyIORef' pausedRef (M.delete p)
          | Just p <- T.stripPrefix "cur:" rtag -> do
              st <- liftIO $ readIORef pausedRef
              case M.lookup p st of
                Just (_, PauseGotCap cap buf) -> do
                    liftJSM $ do
                        case res of
                          Right (stLine : _) ->
                              writePane sessionId p (buildReplay cap stLine)
                          -- No state line: skip the replay (screen keeps
                          -- whatever it had) — but the BUFFERED live output
                          -- must still flush, or everything typed since the
                          -- capture silently vanishes (a deaf pane).
                          _ -> focusLog $ "[" <> T.unpack sessionId
                                  <> "] replay cur FAILED pane=" <> T.unpack p
                        forM_ (reverse buf) $ writePane sessionId p
                    liftIO $ writeIORef pausedRef (M.delete p st)
                _ -> liftIO $ modifyIORef' pausedRef (M.delete p)
          | otherwise -> return ()
        _ -> return ()

    -- The client is gone: stop offering its control channel to the menu,
    -- and drop any tunnels (their sync routes must not outlive the client).
    performEvent_ $ ffor evE $ \case
        EvExit _ -> liftIO $ do
            -- All registry drops are id-guarded, so this widget's teardown
            -- can never unregister a NEWER widget's entries for the same
            -- session/window (several leksah windows can share one session).
            readIORef ccRegIdRef >>= unregisterTerminalCC sessionId
            -- Drop this client's teardown (and kill its now-idle drain thread).
            readIORef ccStopIdRef >>= unregisterCCStop sessionId
            readIORef splitsRegIdRef >>= unregisterTerminalSplits sessionId
            readIORef focusRegIdsRef >>= mapM_ (uncurry unregisterTerminalFocus)
            tunnels <- atomicModifyIORef' tunnelsRef $ \m -> (M.empty, M.keys m)
            forM_ tunnels $ \p -> do
                unregisterTunnelSync (tunnelUrlKey sessionId p)
                fireTunnelEv (p, Nothing)
        _        -> return ()

    -- Navigation from a clicked file path.
    let fileGotoE = (\(f, l, c) -> SrcSpan f l c l c) <$> linkE
    -- Navigation from a Ctrl/Cmd-clicked identifier: look it up in the
    -- metadata.  No match -> nothing; one match -> jump straight there;
    -- several -> pop up a chooser of module names at the click position and
    -- jump to the picked one.  (Same flow as the classic widget.)
    let optsE = attachWith (\i (tok, x, y) -> (lookupIdentLocations tok i, x, y))
                  (current ide) lookupE
        singleGotoE = fmapMaybe (\(opts, _, _) -> case opts of [(_, sp)] -> Just sp; _ -> Nothing) optsE
        multiE      = fmapMaybe (\(opts, x, y) -> if length opts > 1 then Just (x, y, opts) else Nothing) optsE
    rec chooserD <- holdDyn Nothing $ leftmost [ Just <$> multiE, Nothing <$ chosenE ]
        chosenE <- switchHold never =<< dyn (ffor chooserD $ \case
          Nothing           -> return never
          Just (x, y, opts) ->
            elAttr "div" ("class" =: "context-menu"
                <> "style" =: T.pack ("position:fixed;left:" <> show x <> "px;top:" <> show y <> "px")) $
              menu [ pure (lbl, sp) | (lbl, sp) <- opts ])
    let gotoE = TerminalGoto <$> leftmost [ fileGotoE, singleGotoE, chosenE ]

    -- Events for the rest of the IDE: window renames update the tab title;
    -- the client exiting (session ended) closes the tab — the same contract
    -- as the PTY widget's reader-EOF path.
    return $ leftmost
      [ gotoE
      , TerminalBell <$ bellE
      , fmapMaybe (\case
          EvWindowRenamed _ nm -> Just (TerminalTitle nm)
          -- A LOCAL client exiting (session ended) closes the tab.  A REMOTE
          -- one does NOT auto-close: it swaps in the error view instead (see
          -- 'connErrorView'/fireConnErr), so a dropped connection stays visible;
          -- its Close button closes the tab via 'closeErrE' below.
          EvExit _ | not isRemote -> Just TerminalExited
          -- windows created/closed: poke the trees (renames go via
          -- TerminalTitle above and reach the tree on its next poll)
          EvWindowAdd _        -> Just TerminalTreeChanged
          EvWindowClose _      -> Just TerminalTreeChanged
          _                    -> Nothing) evE
      -- Close button in the remote error view: drop the tab.
      , TerminalExited <$ closeErrE
      -- A pane split/kill (⌘D etc.) changes a window's pane count but only
      -- emits %layout-change, not %window-add — so poke the tree poll off the
      -- pane set itself (fired from inside the session widget), else the
      -- ⌘-number offset (and tab badges) lag until the next unrelated poll.
      , TerminalTreeChanged <$ paneSetChangedE
      -- The active window closed: the IDE activates the ⌘1 button (below).
      , TerminalActiveWinClosed <$ activeWinClosedE
      -- A leksah-issued pane focus (⌘-number split select): float it to the MRU.
      , TerminalPaneFocused <$> paneFocusE
      -- Went to the Retry page (appeared here, or selected while showing):
      -- float this terminal to the flipper front (IDE gates the appear case).
      , connErrShownWE
      -- A Retry reconnected: refocus the window if this tab is still active.
      , TerminalReconnected <$ reconnectedE ]

-- | The message shown when a remote connection drops: the tmux @%exit@ reason
-- (if any) plus whatever the child wrote to stderr (ssh's "Permission denied",
-- "Could not resolve hostname", host-key failures, …).  Empty stderr is common
-- for a clean detach, so it's only appended when present.
formatConnErr :: Maybe Text -> Text -> Text
formatConnErr reason err =
    let hdr = maybe "Connection closed." (\r -> "Connection closed: " <> r) reason
        e   = T.strip err
    in if T.null e then hdr else hdr <> "\n\n" <> e

-- | The page-wide terminal cell size in CSS px ('LeksahTerm.cellMetrics',
-- measured once from the font); a conservative fallback if measurement is
-- somehow impossible.
getCellMetrics :: JSM (Double, Double)
getCellMetrics = getCellMetricsFor Nothing

-- | Cell size for a specific font-size override ('Nothing' = the global
-- monospace pref) — per-LEAF fonts in the native split layouts.  Cached per
-- (family, size) on the JS side.
getCellMetricsFor :: Maybe Int -> JSM (Double, Double)
getCellMetricsFor mbSz = do
    v <- case mbSz of
      Nothing -> jsg ("LeksahTerm" :: Text) ^. js0 ("cellMetrics" :: Text)
      Just sz -> jsg ("LeksahTerm" :: Text) ^. js1 ("cellMetrics" :: Text) sz
    nul <- valIsNull v
    und <- valIsUndefined v
    if nul || und
      then return (7.8, 15)
      else (,) <$> (valToNumber =<< v ^. js ("w" :: Text))
               <*> (valToNumber =<< v ^. js ("h" :: Text))

layoutOf :: TmuxEvent -> Maybe (WindowId, Layout)
layoutOf (EvLayoutChange w (Just l) _ _) = Just (w, l)
layoutOf _ = Nothing

closedWin :: TmuxEvent -> Maybe WindowId
closedWin (EvWindowClose w) = Just w
closedWin (EvUnlinkedWindowClose w) = Just w
closedWin _ = Nothing

-- | The (session, window) of a %session-window-changed notification.  tmux
-- broadcasts it for EVERY session to every control client, so the fold that
-- consumes this MUST filter to this widget's own session (a foreign session's
-- window id set as csCurrent matches nothing in csLayouts, hiding every window
-- container — the tab shows blank).  The filter lives in the fold because a
-- remote tab must match on the attached session id (csSession, the remote
-- tmux's $id) rather than its ssh:// label.  The empty session id is
-- 'initialSync''s wildcard for its own seed event.
sessionWinOf :: TmuxEvent -> Maybe (SessionId, WindowId)
sessionWinOf (EvSessionWindowChanged s w) = Just (s, w)
sessionWinOf _                            = Nothing

data CCState = CCState
  { csLayouts :: M.Map WindowId Layout
  , csCurrent :: Maybe WindowId
  -- | The tmux session this control client is currently attached to.  One
  -- connection per server can be pointed at any of the server's sessions with
  -- @switch-client@; on such a switch tmux only emits @%session-changed@ (no
  -- window layouts), so the widget clears its windows and re-syncs (see the
  -- 'EvSessionChanged' handling).  'Nothing' until the first attach.
  , csSession :: Maybe SessionId
  }

paneKey :: Text -> PaneId -> Text
paneKey sess pane = sess <> "/" <> pane

-- | Per-pane jsaddle-terminal tunnel bookkeeping (IO layer, shared between
-- the drain thread's scanner and the iframe widgets): the iframe generation
-- (bumped when a NEW app run HELLOs so the iframe is rebuilt fresh), whether
-- the iframe reported ready (only then do BATCH frames flow to it), and the
-- run's HELLO payload — retries of the same run are byte-identical, which is
-- how a late retry (its ACK still in flight) is told apart from a restart.
data TunnelInfo = TunnelInfo
  { tiGen    :: Int
  , tiActive :: Bool
  , tiRun    :: BS.ByteString
  }

-- | URL/JS-safe pane key for the tunnel frame/sync routes: bytes outside
-- [A-Za-z0-9.] (including @_@ itself, keeping the mapping injective) become
-- @_XX@ (or @_uXXXX@ beyond Latin-1).
tunnelUrlKey :: Text -> PaneId -> Text
tunnelUrlKey sess pane = T.concatMap esc (paneKey sess pane)
  where
    esc c | isAlphaNum c && ord c < 128 = T.singleton c
          | c == '.' = T.singleton c
          | ord c < 0x100 = "_" <> T.pack (pad 2 (showHex (ord c) ""))
          | otherwise = "_u" <> T.pack (pad 4 (showHex (ord c) ""))
    pad n s = replicate (n - length s) '0' <> s

-- | Route one control-mode event through the jsaddle-terminal frame scanner
-- (runs on the drain thread, pure IO — the SYNC round trip depends on this
-- never waiting on the browser): frames are dispatched to the tunnel
-- machinery, everything else — including a tunnelled pane's non-frame bytes —
-- flows on to the normal 'EvOutput' path.
routeTunnelEv
  :: CC -> Text                           -- ^ client + session (SYNC plumbing)
  -> IORef (M.Map PaneId TunnelInfo)      -- ^ tunnel states
  -> IORef (M.Map PaneId P.OscScan)       -- ^ per-pane scanner buffers
  -> IORef (S.Set PaneId)                 -- ^ zombies already told to CLOSE
  -> (TmuxEvent -> IO ())                 -- ^ the normal event fire
  -> ((PaneId, Maybe Int) -> IO ())       -- ^ iframe generation up/down
  -> ((PaneId, Text) -> IO ())            -- ^ BATCH JSON for the pane's iframe
  -> TmuxEvent -> IO ()
routeTunnelEv cc sessionId tunnelsRef scansRef closedRef fireEv fireTunnelEv fireBatchEv = \case
    EvOutput pane dat -> do
        scans <- readIORef scansRef
        let st0 = M.findWithDefault P.emptyOscScan pane scans
            (passthrough, frames, st') = P.scanOsc st0 dat
        writeIORef scansRef (M.insert pane st' scans)
        mapM_ (frameEv pane) frames
        unless (BS.null passthrough) $ fireEv (EvOutput pane passthrough)
    ev -> fireEv ev
  where
    frameEv pane f = case P.frameType f of
        P.Hello -> do
            let runId = P.framePayload f
            act <- atomicModifyIORef' tunnelsRef $ \m -> case M.lookup pane m of
                Just ti | tiRun ti == runId ->
                    -- The same run again: a retry whose ACK is still in
                    -- flight (ignore), or one that crossed our ACK on the
                    -- wire (re-ACK — the exe ignores a duplicate).
                    (m, if tiActive ti then ReAck else Ignore)
                Just ti ->
                    -- A different run: the app restarted — rebuild fresh.
                    let g = tiGen ti + 1
                    in (M.insert pane (TunnelInfo g False runId) m, Build g)
                Nothing -> (M.insert pane (TunnelInfo 0 False runId) m, Build 0)
            case act of
                Ignore  -> return ()
                ReAck   -> sendAck pane
                Build g -> do
                    -- a fresh app run: it may be CLOSEd again if it zombies
                    atomicModifyIORef' closedRef $ \s -> (S.delete pane s, ())
                    -- (Re)arm the sync route for this pane: SYNC frames go
                    -- straight to its stdin from the Warp thread.
                    registerTunnelSync (tunnelUrlKey sessionId pane) $ \bs ->
                        ccSendBytesBig cc pane (P.encodeRs (P.Frame P.Sync bs))
                    fireTunnelEv (pane, Just g)
        P.Batch -> do
            m <- readIORef tunnelsRef
            case M.lookup pane m of
                Just ti | tiActive ti ->
                    fireBatchEv (pane, decodeUtf8 (P.framePayload f))
                Just _ -> return ()   -- iframe still building: drop (pre-ACK)
                -- A tunnel-mode app we know nothing about: it was ACKed by a
                -- previous leksah run (restart mid-session).  Its jsaddle
                -- state can't be re-adopted, so tell it to exit cleanly —
                -- the user gets their shell back and can just rerun it.
                Nothing -> sendClose pane
        -- The Batch answering a pending SYNC: complete the blocked XHR.
        P.SyncReply -> do
            m <- readIORef tunnelsRef
            case M.lookup pane m of
                Just _  -> tunnelSyncReply (tunnelUrlKey sessionId pane)
                                           (P.framePayload f)
                Nothing -> sendClose pane   -- zombie (see above)
        P.Bye -> do
            atomicModifyIORef' tunnelsRef $ \m -> (M.delete pane m, ())
            unregisterTunnelSync (tunnelUrlKey sessionId pane)
            fireTunnelEv (pane, Nothing)
        -- Show LOG payloads in the (hidden, but current) xterm.
        P.Log -> fireEv (EvOutput pane (P.framePayload f <> "\r\n"))
        -- IDE→exe types arriving from the exe: protocol misuse; ignore.
        _ -> return ()
    sendAck pane = ccSendBytes cc pane (P.encodeRs (P.Frame P.Ack tunnelAckPayload))
    -- Once per pane (until a HELLO re-arms it): after the zombie exits, any
    -- further injected frames would land in its SHELL as junk keystrokes.
    sendClose pane = do
        firstTime <- atomicModifyIORef' closedRef $ \s ->
            (S.insert pane s, not (S.member pane s))
        when firstTime $
            ccSendBytes cc pane (P.encodeRs (P.Frame P.Close "{}"))

-- | What the HELLO handler decided (see 'routeTunnelEv').
data HelloAction = Ignore | ReAck | Build Int

-- | The ACK payload (protocol version + capabilities leksah offers).
tunnelAckPayload :: BS.ByteString
tunnelAckPayload = "{\"proto\":1,\"caps\":[\"sync\"]}"

-- 'sessionlessLwWidget' (the renderer for windows with no backing session),
-- 'publishLwGeom' and 'modifyLeksahWindow' live in 'IDE.Web.Widget.LwView'
-- (CPP-free — the in-browser demo uses the same renderer).

-- | Uniform padding (CSS px) inset around every pane's terminal grid.  The
-- whole cell grid is shifted right/down by this and shrunk by twice it (see
-- 'refit'), so the reserved space becomes an even gap on all sides of every
-- pane rather than being clipped off the last row/column.
terminalPanePad :: Double
terminalPanePad = 4

pxAt :: Int -> Double -> Text
pxAt n cell = T.pack (show (round (terminalPanePad + fromIntegral n * cell) :: Int)) <> "px"

-- | Exact pixel span of @n@ cells starting at cell @o@ (avoids the drift
-- of rounding the width independently of the position).
pxSpan :: Int -> Int -> Double -> Text
pxSpan o n cell =
    T.pack (show ((round (fromIntegral (o + n) * cell)
                   - round (fromIntegral o * cell)) :: Int)) <> "px"

-- | Half a cell, as a px length (the divider lines overhang the gutter by
-- this much at each end).
halfPx :: Double -> Text
halfPx cell = T.pack (show (cell / 2)) <> "px"

-- | What a pane's xterm reports up to the session widget (which owns the
-- reflex events): clicked file link (path, line, column), Ctrl/Cmd
-- identifier lookup (token, click x, y), bell.
data PaneCallbacks = PaneCallbacks
  { pcLink   :: (FilePath, Int, Int) -> IO ()
  , pcLookup :: (Text, Int, Int) -> IO ()
  , pcBell   :: IO ()
  , pcHover  :: (Int, Maybe Text) -> IO ()
  }

-- | ONE pane, as a keyed widget that LIVES ACROSS LAYOUT CHANGES: the div's
-- geometry is a dynamic style (moves are pure attribute updates), a rect
-- change resizes the existing xterm's grid in place, and only the widget's
-- CREATION replays content — so splits/resizes/window switches no longer
-- flash, and scrollback/selection/parser state survive them.
paneWidget
  :: MonadWidget t m
  => CC -> Text -> PaneCallbacks
  -> IORef (M.Map PaneId JSVal) -> IORef (M.Map PaneId (UTCTime, PauseState))
  -> IORef (M.Map PaneId TunnelInfo)
  -> IORef (Maybe PaneId)    -- ^ tmux's current active pane (echo-suppression)
  -> IORef (Maybe PaneId)    -- ^ focus-on-mount request from the reconciler:
                             --   the keyboard belongs in this pane as soon as
                             --   its xterm exists (a fresh ⌘D split)
  -> (Double, Double)        -- ^ cell size (already measured for mbFont)
  -> Maybe Int               -- ^ the leaf's font-size override, if any
  -> PaneId -> Dynamic t (Int, Int, Int, Int)
  -> Dynamic t (Int, Int)    -- ^ layout size in cells (for edge panes)
  -> Dynamic t (Maybe Int)   -- ^ jsaddle-terminal tunnel generation (Just = iframe)
  -> Event t ()              -- ^ this pane was selected (⌘-number split select)
  -> Dynamic t (Maybe (Text, Bool)) -- ^ ⌘W close-menu target: (pane %id, multi-pane?)
  -> (Text -> Bool -> m ())  -- ^ render the close menu for (pane %id, multi-pane?)
  -> m ()
paneWidget cc sessionId cbs termsRef pausedRef tunnelsRef activePaneRef pendingMountRef (cw, ch) mbFont pane rectD0 dimsD0 tunnelD0 _overlaySelE closeMenuD renderCloseMenu = do
    rectD <- holdUniqDyn rectD0
    dimsD <- holdUniqDyn dimsD0
    tunnelD <- holdUniqDyn tunnelD0
    -- The pane box uses the SAME extents as the active-pane shadow marker
    -- ('renderHlSegments'), so the two line up exactly: at the layout's outer
    -- edges it is flush with the container (covering the sub-cell remainder —
    -- the client area isn't an exact multiple of the cell size), and on an
    -- INTERNAL edge it reaches half a cell into the gutter, to the centre where
    -- the divider line is drawn — so adjacent panes meet at the line with no
    -- gap.  Outer left/top are clamped to 0 (rather than the marker's clipped
    -- ‑½‑cell overhang) so the iframe that fills the pane loses no content.
    -- (The transparency/snap hole reads this element's box via
    -- 'leksahComputeHole', so it tracks the same extents.)
    let styleOf (x, y, w, h) (lw, lh) =
            let p n     = T.pack (show (n :: Int)) <> "px"
                pad     = terminalPanePad
                -- The grid is shifted right/down by 'pad' and shrunk by 2·pad
                -- (see 'refit'); combined with box-sizing:border-box + a uniform
                -- 'pad' padding on every pane, that reserved space becomes an
                -- even inset on all four sides of the terminal grid (against the
                -- window edge lines for outer panes, against the divider lines
                -- for inner ones) instead of clipping the last row/column.
                lI      = round (max 0 (pad + fromIntegral x * cw - cw/2))
                tI      = round (max 0 (pad + fromIntegral y * ch - ch/2))
                rI      = round (pad + fromIntegral (x+w) * cw + cw/2)
                bI      = round (pad + fromIntegral (y+h) * ch + ch/2)
            in "position:absolute;overflow:hidden;box-sizing:border-box"
               <> ";padding:" <> p (round pad)
               <> ";left:" <> p lI <> ";top:" <> p tI
               <> (if x + w >= lw then ";right:0"  else ";width:"  <> p (rI - lI))
               <> (if y + h >= lh then ";bottom:0" else ";height:" <> p (bI - tI))
        key = tunnelUrlKey sessionId pane
    (paneEl, _) <- elDynAttr' "div"
        ((\r d mt -> "class" =: ("terminal-cc-pane"
                               <> maybe "" (const " terminal-cc-pane-tunnel") mt)
                 -- data-pane = the tmux %id, so a flip target (published by
                 -- reflex as a %id) can be located in the DOM to hang a ⌘` hint.
                 <> "data-pane" =: pane
                 <> "style" =: styleOf r d) <$> rectD <*> dimsD <*> tunnelD) $ do
        -- ⌘W close menu, rendered INSIDE this pane so CSS centres it (no JS
        -- geometry) — shown only for the pane the menu currently targets.  The
        -- render function comes from IDE.Web.Main (it owns the menu logic).
        let menuHereD = ffor closeMenuD $ \mt -> case mt of
                          Just (p, m) | p == pane -> Just m
                          _                       -> Nothing
        dyn_ $ ffor menuHereD $ \mm -> case mm of
          Nothing    -> blank
          Just multi -> renderCloseMenu pane multi
        -- jsaddle-terminal overlay: while a tunnel generation is active an
        -- iframe (keyed by the generation, so an app restart rebuilds it)
        -- covers the pane; the hidden xterm keeps consuming non-frame output.
        dyn_ $ ffor tunnelD $ \case
          Nothing -> do
            pbN <- getPostBuild
            performEvent_ $ ffor pbN $ \_ -> liftJSM . void $
                jsg ("LeksahJsaddlePane" :: Text) ^. js1 ("unregister" :: Text) key
          Just _gen -> do
            (ifrEl, _) <- elAttr' "iframe"
                ("class" =: "terminal-cc-iframe"
                 <> "src" =: ("/jsaddle-terminal/frame/" <> key)) blank
            -- Results are re-sequenced before injection: leksah's jsaddle
            -- runs 'fun' callbacks on forked threads, so two results can
            -- race here, and the exe's jsaddle silently drops stale batch
            -- numbers — misordering corrupts, so restore the seq order the
            -- iframe stamped.  Fresh per iframe (its seq restarts at 0).
            nextSeqRef <- liftIO $ newIORef (0 :: Int)
            reorderRef <- liftIO $ newIORef (M.empty :: M.Map Int BS.ByteString)
            pbI <- getPostBuild
            performEvent_ $ ffor pbI $ \_ -> liftJSM . void $
                jsg ("LeksahJsaddlePane" :: Text) ^. js3 ("register" :: Text)
                    key (_element_raw ifrEl)
                    (fun $ \_ _ args -> case args of
                        (tV : sV : dV : _) -> valToText tV >>= \case
                            -- The runtime is installed: complete the
                            -- handshake — BATCH frames may flow now.
                            "ready" -> liftIO $ do
                                atomicModifyIORef' tunnelsRef $ \m ->
                                    (M.adjust (\ti -> ti { tiActive = True }) pane m, ())
                                ccSendBytes cc pane $
                                    P.encodeRs (P.Frame P.Ack tunnelAckPayload)
                            "results" -> do
                                sq <- valToNumber sV
                                d  <- valToText dV
                                liftIO $ do
                                    modifyIORef' reorderRef
                                        (M.insert (round sq) (encodeUtf8 d))
                                    let drain = do
                                          nxt <- readIORef nextSeqRef
                                          buf <- readIORef reorderRef
                                          forM_ (M.lookup nxt buf) $ \bs -> do
                                              writeIORef reorderRef (M.delete nxt buf)
                                              writeIORef nextSeqRef (nxt + 1)
                                              ccSendBytesBig cc pane
                                                  (P.encodeRs (P.Frame P.Results bs))
                                              drain
                                    drain
                            -- The iframe gained keyboard focus (user clicked
                            -- into the app): make its tmux pane the active one,
                            -- exactly as an xterm's textarea focus does.  The
                            -- resulting %window-pane-changed moves the
                            -- highlight/shadow (see followActive).
                            "focus" -> liftIO $ do
                                -- Same echo-suppression as the xterm textarea
                                -- focus handler below: only select-pane on a
                                -- genuine user pane change, never as an echo of
                                -- our own follow-focus (see that comment).
                                active <- readIORef activePaneRef
                                if active == Just pane
                                  then focusLog $ "[" <> T.unpack sessionId <> "] tunnel iframe FOCUS pane="
                                         <> T.unpack pane <> " == active -> skip select-pane (echo)"
                                  else do
                                    focusLog $ "[" <> T.unpack sessionId <> "] tunnel iframe FOCUS pane="
                                        <> T.unpack pane <> " (active " <> show (T.unpack <$> active)
                                        <> ") -> select-pane"
                                    ccSend cc ("select-pane -t " <> pane)
                            _ -> return ()
                        _ -> return ())
        -- (The ⌘D pane-overlay rendering lived here until the native split
        -- layouts replaced it: an editor is a 'PaneView' pane now, never a
        -- layer over a tmux pane.)
    pb <- getPostBuild
    performEvent_ $ ffor (tag (current rectD) pb) $ \(_, _, w, h) -> liftJSM $ do
        term <- new (jsg ("Terminal" :: Text)) ()
        opts <- term ^. js ("options" :: Text)
        -- Monospace font/size from the prefs-driven window globals (see the note
        -- in "IDE.Web.Widget.Terminal"): the cell-metrics probe waits for the font
        -- to load before measuring, so a system font like Monaco fits correctly.
        win <- jsg ("window" :: Text)
        monoFam <- win ^. js ("__leksahMonoFamily" :: Text)
        monoSz  <- win ^. js ("__leksahMonoSize" :: Text)
        _ <- opts ^. jss ("fontFamily" :: Text) monoFam
        -- The leaf's font-size override, if any (per-leaf fonts) — must match
        -- the cell size this widget was built with or rows clip.
        case mbFont of
          Just sz -> void $ opts ^. jss ("fontSize" :: Text) sz
          Nothing -> void $ opts ^. jss ("fontSize" :: Text) monoSz
        -- Line/letter spacing tuned to match a native terminal (see the note in
        -- "IDE.Web.Widget.Terminal"); the cell-metrics probe uses the same values.
        _ <- opts ^. jss ("lineHeight" :: Text) (1.07 :: Double)
        _ <- opts ^. jss ("letterSpacing" :: Text) (-0.5 :: Double)
        _ <- opts ^. jss ("allowProposedApi" :: Text) True
        -- Unicode 11 widths, as in the classic widget — emoji are
        -- width 2 to tmux and the apps, so xterm must agree.
        uni <- new (jsg ("Unicode11Addon" :: Text) ^. js ("Unicode11Addon" :: Text)) ()
        _ <- term ^. js1 ("loadAddon" :: Text) uni
        unicodeApi <- term ^. js ("unicode" :: Text)
        _ <- unicodeApi ^. jss ("activeVersion" :: Text) ("11" :: Text)
        -- OSC 8 hyperlinks (tmux forwards them): hover shows the URL,
        -- clicking an http(s) link opens the browser (snapping it over
        -- this pane only when Command was held), file:// opens an editor —
        -- exactly the classic widget's handler.
        oscHandler <- jsg ("LeksahOscLinks" :: Text) ^. js2 ("makeHandler" :: Text)
            (fun $ \_ _ as -> case as of
                (u : snapV : _) -> do
                    url  <- valToText u
                    snap <- valToBool snapV
                    liftIO $ do
                        _ <- (try (void $ createProcess (proc "open" [T.unpack url]))
                                :: IO (Either SomeException ()))
                        when snap $ requestSnapPane pane
                _ -> return ())
            (fun $ \_ _ as -> case as of
                (pV : lV : cV : _) -> do
                    path <- valToText pV
                    ln   <- valToNumber lV
                    col  <- valToNumber cV
                    liftIO $ pcLink cbs (T.unpack path, max 1 (round ln), max 1 (round col))
                _ -> return ())
        _ <- opts ^. jss ("linkHandler" :: Text) oscHandler
        _ <- jsg ("LeksahTerm" :: Text) ^. js2 ("register" :: Text)
                 (paneKey sessionId pane) term
        _ <- term ^. js1 ("open" :: Text) (_element_raw paneEl)
        -- Make file paths / identifiers in the output clickable (see
        -- 'terminalLinksJs'); same callbacks as the classic widget.
        _ <- jsg ("LeksahTermLinks" :: Text) ^. js4 ("attach" :: Text) term
                (fun $ \_ _ as -> case as of
                    (p : l : c : _) -> do
                        path <- valToText p
                        ln   <- valToNumber l
                        col  <- valToNumber c
                        liftIO $ pcLink cbs (T.unpack path, round ln :: Int, round col :: Int)
                    _ -> return ())
                (fun $ \_ _ as -> case as of
                    (t : x : y : _) -> do
                        tok <- valToText t
                        cx  <- valToNumber x
                        cy  <- valToNumber y
                        liftIO $ pcLookup cbs (tok, round cx :: Int, round cy :: Int)
                    _ -> return ())
                -- Hover: (file, hoverLine, hoverCol, requestId) -> LSP tooltip -> JS.
                -- hoverCol < 0 means "column unknown" (file summary only).
                (fun $ \_ _ as -> case as of
                    (fV : lV : cV : rV : _) -> do
                        path <- valToText fV
                        hl   <- valToNumber lV
                        hc   <- valToNumber cV
                        rid  <- valToNumber rV
                        let mline = let n = round hl :: Int in if n > 0 then Just n else Nothing
                            mcol  = let n = round hc :: Int in if n >= 0 then Just n else Nothing
                        liftIO $ LSP.requestTerminalHover (T.unpack path) mline mcol $ \mt ->
                            pcHover cbs (round rid :: Int, mt)
                    _ -> return ())
        -- The find bar searches the FOCUSED pane: register the SearchAddon
        -- on xterm's own root element (the innermost .terminal the focus
        -- sits in — see onFocusPane in the cm6 bundle).
        termRoot <- term ^. js ("element" :: Text)
        _ <- jsg ("LeksahCM" :: Text) ^. js2 ("loadTerminalSearch" :: Text) term termRoot
        -- Inline images: SIXEL, iTerm2 OSC 1337 and the kitty graphics
        -- protocol.  A program in a tmux pane wraps these in tmux's DCS
        -- passthrough, which reaches a control-mode client verbatim —
        -- IDE.Web.KittyGraphics unwraps it before xterm sees it.  storageLimit
        -- caps the per-terminal image cache (MB); CC layouts have many panes, so
        -- keep it modest.
        imgOpts <- obj
        _ <- imgOpts ^. jss ("storageLimit" :: Text) (32 :: Int)
        img <- new (jsg ("ImageAddon" :: Text) ^. js ("ImageAddon" :: Text)) [imgOpts]
        _ <- term ^. js1 ("loadAddon" :: Text) img
        -- OSC 52 writes land on the system clipboard (vim yank, tmux
        -- copy-mode over ssh, …).
        clip <- new (jsg ("ClipboardAddon" :: Text) ^. js ("ClipboardAddon" :: Text)) ()
        _ <- term ^. js1 ("loadAddon" :: Text) clip
        -- Bell (Claude Code's needs-input signal) → leksah attention.
        _ <- term ^. js1 ("onBell" :: Text) (fun $ \_ _ _ -> liftIO (pcBell cbs))
        -- the grid IS the tmux pane's cells; the box already matches
        _ <- term ^. js2 ("resize" :: Text) w h
        -- Keystrokes → tmux.  ccSendBytes is fire-and-forget, so no forkIO:
        -- sending inline keeps keystroke ORDER (concurrent forks could race
        -- for the submit lock and swap two fast keypresses).
        _ <- term ^. js1 ("onData" :: Text) (fun $ \_ _ args -> case args of
                (d : _) -> do
                    s <- valToText d
                    -- Latency trace (µs-stamped, off unless the focus logger
                    -- is on): pairs with the ECHO record in the %output
                    -- write path, so a \"typing is slow\" episode shows
                    -- exactly which hop eats the time.
                    focusLog $ "[" <> T.unpack sessionId <> "] KEY pane="
                        <> T.unpack pane <> " bytes=" <> show (T.length s)
                    liftIO $ ccSendBytes cc pane (encodeUtf8 s)
                _ -> return ())
        -- Intercept the tmux C-b prefix (when the pref is on).  Crucial for CC
        -- tabs: keystrokes here are send-keys'd into the pane, so a raw C-b never
        -- reaches tmux's prefix handling — the interceptor turns C-b chords into
        -- tmux commands on the control channel instead (see window.LeksahTmux).
        _ <- jsg ("LeksahTmux" :: Text) ^. js1 ("attach" :: Text) term
        -- Keep tmux's active pane in step with keyboard focus, so the
        -- Terminal menu's pane commands (split/resize/…, which act on
        -- the current pane) target the pane the user is typing in.
        ta <- term ^. js ("textarea" :: Text)
        _ <- ta ^. js2 ("addEventListener" :: Text) ("focus" :: Text)
                (fun $ \_ _ _ -> liftIO $ do
                    -- Only tell tmux to select this pane when the focus is a
                    -- GENUINE user pane change — i.e. the pane we just gained
                    -- focus on is NOT already tmux's active pane.  When
                    -- 'focusActivePane' focuses the active pane (following a
                    -- %window-pane-changed), this listener also fires; echoing
                    -- select-pane there is redundant and, if focus and tmux's
                    -- active pane are momentarily out of sync (window/app
                    -- switch into a multi-pane split), sustains an infinite
                    -- focus↔select-pane oscillation.  Skipping the echo lets it
                    -- settle: a real click on the OTHER pane still differs from
                    -- activePaneRef and selects it.
                    active <- readIORef activePaneRef
                    if active == Just pane
                      then focusLog $ "[" <> T.unpack sessionId <> "] xterm textarea FOCUS pane="
                             <> T.unpack pane <> " == active -> skip select-pane (echo)"
                      else do
                        focusLog $ "[" <> T.unpack sessionId <> "] xterm textarea FOCUS pane="
                            <> T.unpack pane <> " (active " <> show (T.unpack <$> active)
                            <> ") -> select-pane"
                        ccSend cc ("select-pane -t " <> pane))
        liftIO . atomicModifyIORef' termsRef $ \m ->
            (M.insert pane term m, ())
        -- fill the fresh xterm from the pane's current screen + recent
        -- history (also resumes the pane if flow control paused it)
        liftIO $ requestReplay cc pausedRef ReplayWithHistory pane
        -- Focus-on-mount: the reconciler wanted the keyboard here before this
        -- xterm existed (a fresh ⌘D split) — deterministic, no polling.
        pendM <- liftIO $ readIORef pendingMountRef
        when (pendM == Just pane) $ do
            liftIO $ writeIORef pendingMountRef Nothing
            focusLog $ "[" <> T.unpack sessionId <> "] mount-focus pane=" <> T.unpack pane
            void $ term ^. js0 ("focus" :: Text)
    -- Layout moved/resized this pane: match the xterm grid to the new
    -- cell rect (the app redraws itself on the SIGWINCH tmux sends it;
    -- xterm reflows its own buffer) — no replay, no re-creation.
    performEvent_ $ ffor (updated rectD) $ \(_, _, w, h) -> liftJSM $ do
        terms <- liftIO $ readIORef termsRef
        forM_ (M.lookup pane terms) $ \term ->
            void $ term ^. js2 ("resize" :: Text) w h
    -- (A 0.4s post-resize \"settle\" replay lived here — added against stale
    -- cells in diff-based TUIs after resize storms, which turned out to be
    -- the duplicated-output bug (the ownership filter above) all along.  It
    -- also masked panes left deaf by output-gating races; those are fixed
    -- deterministically now, so the wall-clock hack is gone.  If stale cells
    -- ever reappear, the correct re-add is a replay keyed on the LAST resize
    -- command's stream-ordered reply, never a timer.)

-- | The per-pane position markers of one window's layout: an invisible box
-- per pane, EXACTLY over the pane's visual box (the same extents as
-- paneWidget's styleOf, clamps included — the box must be the pane's true
-- location because the active-pane glow overlay anchors to it via CSS
-- anchor positioning, and anchor() reads the UNCLIPPED layout box: the old
-- unclamped −½-cell overhang put the anchored glow's top edge half a cell
-- above the leaf).  The ACTIVE pane's box holds class @active@, which
-- carries the anchor-name the glow ties to; 'applyPaneHighlight' moves the
-- class — pure class toggles, no re-render.
renderHlSegments :: MonadWidget t m => (Double, Double) -> Layout -> m ()
renderHlSegments (cw, ch) l =
    forM_ (layoutPanes l) $ \(pane, x, y, w, h) ->
        -- Half a cell into the gutters (to the divider-line centres, where
        -- adjacent pane boxes meet), clamped at the container's left/top;
        -- flush with the container at the layout's right/bottom edges
        -- (covering the sub-cell remainder) — styleOf, verbatim.
        let px v = T.pack (show (round v :: Int)) <> "px"
            pad  = terminalPanePad
            lI   = max 0 (pad + fromIntegral x * cw - cw / 2)
            tI   = max 0 (pad + fromIntegral y * ch - ch / 2)
            rI   = pad + fromIntegral (x + w) * cw + cw / 2
            bI   = pad + fromIntegral (y + h) * ch + ch / 2
        in elAttr "div"
            -- edge-left marks a pane flush with the window's left edge (layout
            -- column 0); the glow overlay's border-left gating keys on it — a
            -- pane whose left edge is an interior divider must keep the white
            -- ring line (terminalCss).
            ("class" =: ("terminal-cc-hl"
                         <> (if x == 0 then " edge-left" else ""))
             <> "data-pane" =: pane
             <> "style" =: ("position:absolute;pointer-events:none"
                            <> ";left:" <> px lI
                            <> ";top:"  <> px tI
                            <> (if x + w >= lW l
                                  then ";right:0"
                                  else ";width:"  <> px (rI - lI))
                            <> (if y + h >= lH l
                                  then ";bottom:0"
                                  else ";height:" <> px (bI - tI))))
            blank

-- | The ⌘-held navigation badges of one window's layout: pane N (layout /
-- reading order, the numbering 'registerTerminalSplits' selects by) gets a
-- \"⌘N\" badge at its top-left corner.  Hidden by default; shown by
-- @body.leksah-show-badges@ while ⌘ is held (see badgesJs in
-- "IDE.Web.Main") — and only when the preference enabled the feature.
renderShortcutBadges :: MonadWidget t m => (Double, Double) -> Layout -> m ()
renderShortcutBadges (cw, ch) l =
    -- Pane numbers only mean anything on a split (⌘1…⌘P); a lone pane's window
    -- is navigated to by its wide0 tab badge instead, so show none here.
    when (length (layoutPanes l) >= 2) $
    forM_ (zip [1 :: Int ..] (layoutPanes l)) $ \(n, (pane, x, y, _, _)) ->
        when (n <= 9) . elAttr "div"
            -- data-pane lets hintsJs reveal the ⌘` suffix on the flip target's
            -- badge, so a ⌘N pane that's ALSO the flip destination reads "⌘N ⌘`".
            ("class" =: "leksah-shortcut-badge"
             <> "data-pane" =: pane
             <> "style" =: ("position:absolute;left:" <> pxAt x cw
                            <> ";top:" <> pxAt y ch <> ";z-index:6")) $ do
            text ("\8984" <> T.pack (show n))
            browser <- liftIO getBrowserHosted
            elAttr "span" ("class" =: "leksah-flip-suffix") $ text (flipHintText browser)

-- | One window's pane dividers: tmux's separator cells are blank gutters
-- here (a full cell wide/tall); each becomes a grab strip with a crisp 1px
-- line centered in it (the way iTerm2 fills its tmux dividers), and dragging
-- it resizes the split — the line ghosts with the pointer (in JS, see
-- 'dividerDragJs'), and on drop the whole-cell delta runs @resize-pane@ on
-- the divider's target pane; the resulting %layout-change moves the panes.
renderDividers :: MonadWidget t m => CC -> (Double, Double) -> Layout -> m ()
renderDividers cc (cw, ch) l =
    forM_ (layoutDividers l) $ \(vert, x, y, w, h, target) -> do
        -- A gutter reaching the layout's right/bottom edge anchors to the
        -- CONTAINER edge, so the line (and the grab strip) runs through the
        -- sub-cell remainder the cell grid leaves there.
        (dEl, _) <- elAttr' "div"
            ("class" =: ("terminal-cc-divider " <> (if vert then "vert" else "horiz"))
             <> "style" =: ("position:absolute"
                            <> ";left:" <> pxAt x cw
                            <> ";top:"  <> pxAt y ch
                            <> (if x + w >= lW l
                                  then ";right:0"
                                  else ";width:"  <> pxSpan x w cw)
                            <> (if y + h >= lH l
                                  then ";bottom:0"
                                  else ";height:" <> pxSpan y h ch))) $
            -- The line extends half a cell beyond the gutter at each end, so
            -- crossing/tee-ing dividers meet at the junction centres instead
            -- of leaving a gap (the container clips the overhang at edges).
            elAttr "div"
                ("class" =: "divider-line"
                 <> "style" =: (if vert
                      then "position:absolute;left:calc(50% - 0.5px);width:1px;top:-"
                           <> halfPx ch <> ";bottom:-" <> halfPx ch
                      else "position:absolute;top:calc(50% - 0.5px);height:1px;left:-"
                           <> halfPx cw <> ";right:-" <> halfPx cw))
                blank
        pb <- getPostBuild
        performEvent_ $ ffor pb $ \_ -> liftJSM $ do
            let raw = _element_raw dEl
                dir pos neg n = if n > 0 then (pos, n) else (neg, negate n)
            _ <- raw ^. jss ("__leksahResize" :: Text) (fun $ \_ _ args -> case args of
                    (d : _) -> do
                        n <- valToNumber d
                        let (flag, amount) = if vert
                              then dir "-R" "-L" (truncate n :: Int)
                              else dir "-D" "-U" (truncate n :: Int)
                        when (amount /= 0) . liftIO $
                            ccSend cc ("resize-pane -t " <> target <> " "
                                       <> flag <> " " <> T.pack (show amount))
                    _ -> return ())
            void $ jsg ("LeksahDividerDrag" :: Text) ^. js3 ("arm" :: Text)
                       raw vert (if vert then cw else ch)

-- | Mark the chrome box (see 'renderHlSegments') belonging to pane @mbP@ as
-- @active@ (the max-contrast ring) and unmark all others.  The boxes stay
-- visible either way — they carry every pane's always-on grey ring.
applyPaneHighlight :: MakeObject e => e -> Maybe PaneId -> JSM ()
applyPaneHighlight c mbP = do
    els <- c ^. js1 ("querySelectorAll" :: Text) (".terminal-cc-hl" :: Text)
    len <- valToNumber =<< els ^. js ("length" :: Text)
    forM_ [0 .. (floor len - 1) :: Int] $ \i -> do
        e <- els ^. js1 ("item" :: Text) i
        pn <- valToText =<< e ^. js1 ("getAttribute" :: Text) ("data-pane" :: Text)
        cl <- e ^. js ("classList" :: Text)
        void $ cl ^. js2 ("toggle" :: Text) ("active" :: Text) (Just pn == mbP)

-- | Initial state sync (an attach replays nothing): current window + layouts.
-- Pane content is replayed per-pane by 'requestReplay' when its xterm is created.
initialSync :: CC -> (TmuxEvent -> IO ()) -> IO ()
initialSync cc fire = do
    -- Flow control: rather than queueing unbounded output for a pane we
    -- can't keep up with, tmux pauses it (%pause) once we're >1s behind and
    -- we jump ahead to its current screen ('requestReplay').  Safe to arm at
    -- attach time again: pause recovery is now coalesced and screen-only
    -- (see 'requestReplay'), so a busy pane at attach can no longer livelock
    -- the boot in a pause→full-replay→pause loop.
    _ <- ccCommand cc "refresh-client -f pause-after=1"
    r <- ccCommand cc "list-windows -F '#{window_id}\t#{window_active}\t#{window_layout}'"
    case r of
      Left _ -> return ()
      Right ls -> forM_ ls $ \l -> case T.splitOn "\t" l of
          [w, act, lay] -> do
              forM_ (parseLayout lay) $ \parsed ->
                  fire (EvLayoutChange w (Just parsed) Nothing
                            (if act == "1" then "*" else ""))
              when (act == "1") $ fire (EvSessionWindowChanged "" w)
          _ -> return ()
    -- Seed the active-pane highlight; changes then arrive as
    -- %window-pane-changed notifications.
    ap <- ccCommand cc "display-message -p -F '#{window_id}\t#{pane_id}'"
    case ap of
      Right (ln : _) | [w, p] <- T.splitOn "\t" ln ->
          fire (EvWindowPaneChanged w p)
      _ -> return ()

-- | How much of a pane 'requestReplay' reproduces.
data ReplayDepth
  = ReplayWithHistory  -- ^ a fresh xterm: current screen + recent scrollback
  | ReplayScreenOnly   -- ^ resync/pause recovery of an existing xterm: the
                       --   current screen alone

-- | Output routing while a pane is being (re)synced — see 'requestReplay'.
data PauseState
  = PauseDropping                       -- ^ waiting for the capture; drop stale output
  | PauseGotCap [Text] [BS.ByteString]  -- ^ capture arrived; buffering newer output
                                        --   (reversed) until the state line lands

-- | @LeksahTerm.write@: base64 the raw bytes into the pane's xterm.
writePane :: Text -> PaneId -> BS.ByteString -> JSM ()
writePane sess pane dat = void $
    jsg ("LeksahTerm" :: Text) ^. js2 ("write" :: Text)
        (paneKey sess pane) (decodeUtf8 (B64.encode dat))

-- | (Re)fill a pane's xterm from tmux's current screen, race-free: resume
-- the pane (a no-op unless flow control paused it), then fetch the capture
-- and the cursor/mode state as STREAM-ORDERED tagged replies.  Any %output
-- that arrives before the capture reply is state the capture already
-- includes (dropped); anything between capture and state line is buffered
-- and applied after the replay is written (see the 'EvReply' handling).
-- Used both to fill a freshly created xterm and to jump ahead after a
-- flow-control %pause instead of replaying the backlog.
requestReplay :: CC -> IORef (M.Map PaneId (UTCTime, PauseState)) -> ReplayDepth -> PaneId -> IO ()
requestReplay cc pausedRef depth p = do
  st  <- readIORef pausedRef
  now <- getCurrentTime
  -- Coalesce: if a replay is already in flight for this pane, don't issue
  -- another — the pending capture already includes everything up to now and
  -- its completion resumes the pane.  Without this, a busy pane + pause-after
  -- flow control LIVELOCKED fresh boots: each %pause triggered a full
  -- capture, applying it took long enough for the pane to fall >1s behind
  -- again, so it re-paused before the previous replay finished — forever
  -- (presents as the frame thread BlockedOnMVar behind jsaddle and a
  -- static-skeleton UI; see docs/development/debugging-web-ui-freezes.md).
  --
  -- AGE-BOUNDED: a replay whose cap:/cur: reply got lost (client hiccup,
  -- reload boundary) leaves its entry behind, and an unconditional guard
  -- then blocks every healing replay forever — a permanently DEAF pane
  -- (EvOutput dropped/buffered against a capture that never lands; seen
  -- live within hours of the unconditional version).  An in-flight entry
  -- older than 10s is treated as lost and replaced; a genuine slow replay
  -- re-issued at that point just costs one duplicate screen-size capture.
  let inFlight = case M.lookup p st of
        Just (t0, _) -> diffUTCTime now t0 < 10
        Nothing      -> False
  unless inFlight $ do
    modifyIORef' pausedRef (M.insert p (now, PauseDropping))
    -- Re-enable the pane's output for this client first: panes gated off by
    -- the foreign-pane gating (see terminalCCWidget) come back through here
    -- when a widget of ours takes ownership and builds their xterm.
    ccSend cc ("refresh-client -A \"" <> p <> ":on\"")
    ccSend cc ("refresh-client -A \"" <> p <> ":continue\"")
    -- ReplayWithHistory (-S -1000, fresh xterms only): seed up to 1000 lines
    -- of history too — written before the visible screen they land in the
    -- fresh xterm's scrollback.  A pane on the ALTERNATE screen has no
    -- history to capture (tmux clamps to the screen), so TUIs are unaffected.
    -- Resyncs of an EXISTING xterm must be ReplayScreenOnly: its scrollback
    -- already holds the history, so a -S -1000 replay would APPEND 1000
    -- duplicate lines to it (the replay's 2J clears the screen, not the
    -- scrollback) — and the 40x-smaller capture is what keeps pause recovery
    -- cheap enough to never fall behind again.
    ccCommandTagged cc ("cap:" <> p)
        ("capture-pane -t " <> p <> " -p -e -J"
         <> case depth of ReplayWithHistory -> " -S -1000"
                          ReplayScreenOnly  -> "")
    ccCommandTagged cc ("cur:" <> p)
        ("display-message -p -t " <> p <> " -F '"
         <> T.intercalate "\t"
              [ "#{cursor_x}", "#{cursor_y}"
              , "#{alternate_on}", "#{cursor_flag}", "#{wrap_flag}"
              , "#{origin_flag}", "#{insert_flag}"
              , "#{keypad_cursor_flag}", "#{keypad_flag}"
              , "#{mouse_standard_flag}", "#{mouse_button_flag}"
              , "#{mouse_any_flag}", "#{mouse_sgr_flag}", "#{mouse_utf8_flag}"
              ]
         <> "'")

-- | Build the escape stream that reproduces a pane in a fresh (or stale)
-- xterm, from a @capture-pane -e -J@ body and the state line requested by
-- 'requestReplay'.
--
-- MODES FIRST: @capture-pane@ reproduces the pixels but not the terminal
-- STATE — if the app entered the alternate screen, enabled mouse reporting,
-- hid the cursor, … before we attached, xterm never saw those sequences and
-- its state machine diverges from the pane's (e.g. a TUI on the alt screen
-- scroll-scrambles rows in an xterm still on the normal buffer: stale lines
-- linger where tmux cleared, the prompt floats a line high).  tmux tracks the
-- pane's modes, so re-emit them as the escape sequences the app once sent.
--
-- Then clear + home + capture with NO trailing newline: a trailing @\\r\\n@
-- after a full-height capture scrolls the replayed image up one row, leaving
-- everything drawn at replay time one line above later absolutely-addressed
-- output (seen as a TUI prompt floating a line too high).  The cursor is then
-- restored to where tmux says the pane's cursor is.
buildReplay :: [Text] -> Text -> BS.ByteString
buildReplay body stLine =
    encodeUtf8 $
        modes <> csi "2J" <> csi "H" <> T.intercalate "\r\n" body <> moveCursor
  where
    fields = T.splitOn "\t" stLine
    at i = case drop i fields of (f : _) -> f; [] -> ""
    flag i on off = case at i of
      "1" -> on
      "0" -> off
      _   -> ""
    modes = T.concat
      [ flag  2 (csi "?1049h") ""          -- alternate screen (clears it)
      , flag  3 "" (csi "?25l")            -- cursor hidden
      , flag  4 "" (csi "?7l")             -- autowrap off
      , flag  5 (csi "?6h") ""             -- origin mode
      , flag  6 (csi "4h") ""              -- insert mode
      , flag  7 (csi "?1h") ""             -- application cursor keys
      , flag  8 "\ESC=" ""                 -- application keypad
      , flag  9 (csi "?1000h") ""          -- mouse: clicks
      , flag 10 (csi "?1002h") ""          -- mouse: + drag
      , flag 11 (csi "?1003h") ""          -- mouse: any motion
      , flag 12 (csi "?1006h") ""          -- mouse: SGR encoding
      , flag 13 (csi "?1005h") ""          -- mouse: UTF-8 encoding
      ]
    moveCursor = case (readMaybe (T.unpack (at 0)), readMaybe (T.unpack (at 1))) of
      (Just x, Just y) ->
          csi (show (y + 1 :: Int) <> ";" <> show (x + 1 :: Int) <> "H")
      _ -> ""
    csi s = "\ESC[" <> T.pack s

#endif
