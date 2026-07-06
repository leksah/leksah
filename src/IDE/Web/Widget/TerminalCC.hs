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
module IDE.Web.Widget.TerminalCC
  ( terminalCCWidget
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Lens ((^.))
import Control.Monad (forM_, forever, unless, when, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.Char (isAlphaNum, ord)
import Data.IORef
       (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef,
        writeIORef)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Numeric (showHex)
import System.Process (createProcess, proc)
import Text.Read (readMaybe)

import Reflex
       (Dynamic, Event, attachWith, current, ffilter, ffor, fmapMaybe, foldDyn,
        delay, getPostBuild, holdDyn, holdUniqDyn, leftmost, never,
        newTriggerEvent, performEvent, performEvent_, switchHold, tag,
        updated)
import Reflex.Dom.Core
       (MonadWidget, blank, divClass, domEvent, dyn, dyn_, elAttr, elAttr',
        elDynAttr, elDynAttr', listWithKey, text, widgetHold, _element_raw,
        EventName(Click), (=:))
import Language.Javascript.JSaddle
       (JSM, JSVal, MakeObject, fun, js, js0, js1, js2, js3, jsg, jss,
        liftJSM, new, obj, valIsNull, valIsUndefined, valToBool, valToNumber,
        valToText)

import IDE.Core.CTypes (SrcSpan(..))
import IDE.Core.State (IDE)
import IDE.Web.Events (TerminalEvents(..))
import IDE.Web.SnapRequest (requestSnapPane)
import IDE.Web.TerminalInput
       (registerTerminalCC, unregisterTerminalCC, registerTerminalSplits,
        unregisterTerminalSplits, registerTerminalFocus, unregisterTerminalFocus)
import IDE.Web.JsaddleTunnel
       (registerTunnelSync, unregisterTunnelSync, tunnelSyncReply)
import IDE.Web.TmuxCC
import IDE.Web.Widget.Menu (menu)
import IDE.Web.Widget.Metadata (lookupIdentLocations)
import qualified Language.Javascript.JSaddle.Terminal.Protocol as P

-- | Session-level widget: one control client; the current window's panes at
-- their exact tmux layout rectangles.  Same shape as 'terminalWidget' so
-- Main.hs can swap them.
terminalCCWidget
  :: forall t m . MonadWidget t m
  => Dynamic t IDE
  -> Text                -- ^ tmux session id (\"$3\") or \"ssh://host[#target]\"
  -> Event t ()          -- ^ fires when this tab is selected
  -> m (Event t TerminalEvents)
terminalCCWidget ide sessionId selectedE = do
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
    -- A pane split/kill inside the session widget changes the pane set (and so a
    -- window's pane count); fired from there to poke the IDE's tree poll at once
    -- (see 'paneSetChangedE' use in the return / TerminalTreeChanged).
    (paneSetChangedE, firePaneSetChanged) <- newTriggerEvent
    -- The session's *current* window just closed (fired from inside when a
    -- %window-close names the window csCurrent still points at); the IDE picks
    -- the ⌘1 button rather than tmux's default replacement.
    (activeWinClosedE, fireActiveWinClosed) <- newTriggerEvent
    -- A (remote) connection dropped: carries the human-readable reason (ssh/tmux
    -- stderr + the %exit reason) to show in place of the pane, so the tab doesn't
    -- just vanish; 'retryE' re-runs the connection when the user clicks Retry.
    (connErrE, fireConnErr) <- newTriggerEvent
    (retryE, fireRetry) <- newTriggerEvent
    (closeErrE, fireCloseErr) <- newTriggerEvent
    let isRemote = "ssh://" `T.isPrefixOf` sessionId
        -- The error view shown in place of the panes when a (remote) connection
        -- drops: the reason, plus Retry (re-run the connection) and Close (drop
        -- the tab).  Fills the tab; no absolute positioning needed since it
        -- REPLACES the session UI via the widgetHold below.
        connErrorView msg =
            elAttr "div" ("class" =: "terminal-cc-error"
                      <> "style" =: ("height:100%;box-sizing:border-box;overflow:auto"
                                     <> ";padding:14px;background:#111;color:#ddd"
                                     <> ";font:13px/1.5 Menlo,Monaco,monospace")) $ do
                elAttr "div" ("style" =: "color:#ff7b72;font-weight:bold;margin-bottom:8px") $
                    text ("Connection to " <> sessionId <> " failed")
                elAttr "pre" ("style" =: "white-space:pre-wrap;margin:0 0 14px 0") $ text msg
                (rb, _) <- elAttr' "button"
                    ("style" =: "padding:4px 12px;margin-right:8px;cursor:pointer") $ text "Retry"
                (cb, _) <- elAttr' "button"
                    ("style" =: "padding:4px 12px;cursor:pointer") $ text "Close"
                performEvent_ $ liftIO (fireRetry ())    <$ domEvent Click rb
                performEvent_ $ liftIO (fireCloseErr ()) <$ domEvent Click cb
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
    let paneCbs = PaneCallbacks
          { pcLink   = triggerLink
          , pcLookup = triggerLookup
          , pcBell   = triggerBell ()
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
                in startCCWith $
                    [ "ssh", "-o", "ConnectTimeout=10", "-o", "BatchMode=yes"
                    , T.unpack host, "tmux", "-C" ] ++ attach
            Nothing -> startCC ["-L", "leksah"] ["attach-session", "-t", T.unpack sessionId]
        -- Batched drain: everything queued is taken at once and consecutive
        -- same-pane output merged, so a scroll-storm burst is ONE reflex
        -- event + ONE xterm.write instead of thousands (which starved the
        -- jsaddle bridge and made typing choppy).  Every pane's output is
        -- sieved through the jsaddle-terminal frame scanner here — before
        -- pausedRef routing, so flow-control pause/replay (which replays
        -- screen TEXT, never raw escapes) can neither eat nor duplicate a
        -- frame; residual bytes flow on as ordinary EvOutput.
        _ <- forkIO . forever $ ccEventsBatch cc >>= mapM_
                 (routeTunnelEv cc sessionId tunnelsRef scansRef closedRef
                                fireEv fireTunnelEv fireBatchEv)
                 . coalesceOutputs
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
    performEvent_ $ ffor batchEvE $ \(pane, json) -> liftJSM . void $
        jsg ("LeksahJsaddlePane" :: Text) ^. js2 ("runBatch" :: Text)
            (tunnelUrlKey sessionId pane) json

    -- Live xterm instances of this widget, keyed by pane id — disposed and
    -- re-created when the layout re-renders (dyn_ gives no destructors, so
    -- the previous generation is torn down explicitly).
    termsRef <- liftIO $ newIORef (M.empty :: M.Map PaneId JSVal)

    -- Panes being (re)synced by a capture-based replay: output routing per
    -- pane is Normal (absent), 'PauseDropping' (stale pre-capture output is
    -- discarded), or 'PauseGotCap' (capture arrived, waiting for the state
    -- line; post-capture output is buffered to apply AFTER the replay).
    -- Driven by 'requestReplay' — used for a fresh xterm's initial fill AND
    -- for tmux flow control: with @pause-after@ set, tmux pauses any pane
    -- we fall >1s behind on (%pause) rather than queueing unbounded output,
    -- and we jump ahead to the current screen instead of replaying the
    -- backlog (what iTerm2 does).
    pausedRef <- liftIO $ newIORef (M.empty :: M.Map PaneId PauseState)

    -- The session widget proper appears once the client is up.
    _ <- widgetHold (divClass "terminal-cc-empty" $ text "(connecting…)") $
      ffor (leftmost [Right <$> ccStartedE, Left <$> connErrE]) $ \case
       Left errMsg -> connErrorView errMsg
       Right cc -> do
            -- Register this tab's control channel for the Terminal menu's
            -- pane commands (split/select/resize/…): they run verbatim —
            -- the control client's current window/pane IS the displayed one.
            liftIO . registerTerminalCC sessionId $ ccSend cc
            -- A tunnel closed (the app sent BYE or died): the xterm — which
            -- kept consuming non-frame output all along — comes back into
            -- view; refresh it from the pane's current screen.
            performEvent_ $ ffor (fmapMaybe
                    (\(p, mg) -> case mg of Nothing -> Just p; _ -> Nothing)
                    tunnelEvE) $ \p ->
                liftIO $ requestReplay cc pausedRef p
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
                , ffor (fmapMaybe (currentWin sessionId) evE) $ \w s ->
                      s { csCurrent = Just w }
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
            curWinRef <- liftIO $ newIORef (Nothing :: Maybe WindowId)
            let applySize :: Int -> Int -> IO ()
                applySize cols rows = do
                    ccResize cc cols rows
                    mbW <- liftIO $ readIORef curWinRef
                    forM_ mbW $ \wid -> ccResizeWindow cc wid cols rows
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
            -- Keep the per-window clamp on whichever window is displayed:
            -- move it when the session's current window changes.
            curWinD <- holdUniqDyn $ csCurrent <$> stD
            performEvent_ $ ffor (updated curWinD) $ \mbW -> liftIO $ do
                mbOld <- atomicModifyIORef' curWinRef $ \o -> (mbW, o)
                void . forkIO $ do
                    when (mbOld /= mbW) $
                        forM_ mbOld $ \ow -> ccClearWindowSize cc ow
                    (c, r) <- liftIO $ readIORef lastSizeRef
                    when (c > 0) $ forM_ mbW $ \nw -> ccResizeWindow cc nw c r
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
            let applyActive :: JSM ()
                applyActive = do
                    mbC <- liftIO $ readIORef containerRef
                    mbP <- liftIO $ readIORef activePaneRef
                    forM_ mbC $ \c -> applyPaneHighlight c mbP
                    -- Reposition the single top-level shadow overlay over the
                    -- (now-updated) visible active-pane marker.
                    void $ jsg ("window" :: Text) ^. js0 ("leksahUpdatePaneHl" :: Text)
                containerHasFocus :: JSM Bool
                containerHasFocus = do
                    mbC <- liftIO $ readIORef containerRef
                    case mbC of
                      Nothing -> return False
                      Just c  -> do
                        ae <- jsg ("document" :: Text) ^. js ("activeElement" :: Text)
                        valToBool =<< c ^. js1 ("contains" :: Text) ae
                focusActivePane :: JSM ()
                focusActivePane = do
                    mbP <- liftIO $ readIORef activePaneRef
                    terms <- liftIO $ readIORef termsRef
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
                    inFocus <- containerHasFocus
                    unless inFocus $ do
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
                -- frame until the focus actually sticks (the container holds it),
                -- for up to ~0.5s.  The loop stops the instant it succeeds, so once
                -- the pane has the keyboard it can't be yanked back later.
                focusActivePaneSoon :: JSM ()
                focusActivePaneSoon =
                    let go n = do
                            focusActivePane
                            ok <- containerHasFocus
                            unless ok . when (n > (0 :: Int)) $
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
                    when (had || tagName == "BODY") focusActivePane
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
                    when (had || tagName == "BODY" || pend) $ do
                        focusActivePane
                        liftIO $ writeIORef pendingFocusRef False
            -- Explicit focus requests (a workspace repl button launches a repl in
            -- this session): flag it so the imminent window switch focuses the new
            -- pane past the gate, and — for the case where the window is already
            -- current, or the switch already arrived — force a focus a moment later
            -- (the flag is then still set only if nothing consumed it).
            (focusReqE, fireFocusReq) <- newTriggerEvent
            liftIO $ registerTerminalFocus sessionId (fireFocusReq ())
            performEvent_ $ ffor focusReqE $ \_ ->
                liftIO $ writeIORef pendingFocusRef True
            forcedFocusE <- delay 0.15 focusReqE
            performEvent_ $ ffor forcedFocusE $ \_ -> do
                pend <- liftIO $ readIORef pendingFocusRef
                when pend $ liftJSM $ do
                    applyActive
                    focusActivePane
                    liftIO $ writeIORef pendingFocusRef False
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
            performEvent_ $ ffor selSplitE $ \n -> do
                panes <- liftIO $ readIORef curPanesRef
                case drop (n - 1) panes of
                  (p : _) | n >= 1 -> do
                      liftIO $ ccSend cc ("select-pane -t " <> p)
                      liftIO $ writeIORef activePaneRef (Just p)
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
            performEvent_ $ ffor (attachWith (,) (current stD) evE) $ \(st, ev) -> case ev of
                EvWindowPaneChanged w p | w `M.member` csLayouts st -> do
                    liftIO $ writeIORef activePaneRef (Just p)
                    liftJSM followActive
                -- The displayed window itself closed (its last pane exited): tmux
                -- will pick a replacement by its own rule, but the IDE overrides
                -- that to the ⌘1 button.  Sampled csCurrent still names it here
                -- (the %session-window-changed that moves it arrives in a later
                -- frame), so this fires only for the *active* window's close.
                _ | Just w <- closedWin ev, csCurrent st == Just w ->
                    liftIO $ fireActiveWinClosed ()
                EvSessionWindowChanged s w | s == "" || s == sessionId ->
                    liftIO . void . forkIO $ do
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
                    when (M.member p terms) $ requestReplay cc pausedRef p
                _ -> return ()
            -- Selecting this tab focuses its active pane (the classic
            -- widget's behaviour), so typing works without an extra click.
            -- Also repaint every pane: the tab is revealed by a
            -- visibility:hidden→visible flip (see Tabs.hs) that no resize
            -- observer sees, so an xterm whose window container was hidden
            -- when it was last drawn gets a nudge here.
            performEvent_ $ ffor selectedE $ \_ -> liftJSM $ do
                terms <- liftIO $ readIORef termsRef
                forM_ (M.elems terms) repaintTerm
                focusActivePaneSoon
            -- Panes that left the session (kill-pane, window closed): their
            -- keyed widgets are torn down by listWithKey below, but the
            -- xterms are JS objects we own — dispose and unregister them.
            allPanesD <- holdUniqDyn $ ffor stD $ \s -> S.fromList
                [ p | l <- M.elems (csLayouts s), (p, _, _, _, _) <- layoutPanes l ]
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
                    -- on <body>; hand it to the (new) active pane.
                    when (not (M.null gone)) $ do
                        ae <- jsg ("document" :: Text) ^. js ("activeElement" :: Text)
                        tagName <- valToText =<< ae ^. js ("tagName" :: Text)
                        when (tagName == "BODY") focusActivePane
            (containerEl, _) <- elAttr' "div"
                -- Pull back over the .area-wide{0,1} 3px left/top padding
                -- (negative margins + matching size bump) so this container —
                -- and thus the pane/divider/shadow geometry laid inside it —
                -- has its origin on the side/top edge line, not 3px in.  The
                -- text gap the padding gives is re-added per edge-pane in
                -- 'styleOf' (padding-left/top on the char-0/row-0 panes) so the
                -- boxes still reach the line while the text stays clear of it.
                ("class" =: "terminal terminal-cc"
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
                    Just cell -> do
                        let windowsD = csLayouts <$> stD
                            currentD = csCurrent <$> stD
                        _ <- listWithKey windowsD $ \wid layD -> do
                            let visD = (== Just wid) <$> currentD
                            elDynAttr "div"
                                ((\v -> "class" =: "terminal-cc-window"
                                     <> "style" =: ("position:absolute;left:0;top:0;right:0;bottom:0;display:"
                                                    <> (if v then "block" else "none")))
                                  <$> visD) $ do
                                layUniqD <- holdUniqDyn layD
                                let panesD = ffor layUniqD $ \l -> M.fromList
                                        [ (p, (x, y, w, h))
                                        | (p, x, y, w, h) <- layoutPanes l ]
                                    -- Layout size in cells: a pane at the
                                    -- layout's right/bottom edge fills to the
                                    -- container edge (see paneWidget).
                                    dimsD = (\l -> (lW l, lH l)) <$> layUniqD
                                _ <- listWithKey panesD $ \pane rectD ->
                                    paneWidget cc sessionId paneCbs termsRef
                                               pausedRef tunnelsRef cell pane rectD
                                               dimsD (M.lookup pane <$> tunnelGenD)
                                -- Repaint this window's panes when it becomes
                                -- visible: their xterms may have been built
                                -- hidden (display:none) and so never painted
                                -- (see 'repaintTerm').  A window switch within
                                -- the session doesn't resize the container, so
                                -- the ResizeObserver below won't cover this.
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
                                    renderDividers cc cell l
                                    renderHlSegments cell l
                                    renderShortcutBadges cell l
                                    pbHl <- getPostBuild
                                    performEvent_ $ ffor pbHl $ \_ ->
                                        liftJSM applyActive
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
                        forM_ (M.elems terms) repaintTerm
                        -- The active pane moved/resized — re-place the shadow overlay.
                        void $ jsg ("window" :: Text) ^. js0 ("leksahUpdatePaneHl" :: Text))
                void $ ro ^. js1 ("observe" :: Text) (_element_raw containerEl)

    -- Route %output to the pane's xterm (via the pause states above), and
    -- assemble capture-based replays from their stream-ordered replies.
    performEvent_ $ ffor evE $ \case
        EvOutput pane dat -> do
            st <- liftIO $ readIORef pausedRef
            case M.lookup pane st of
              Nothing            -> liftJSM $ writePane sessionId pane dat
              Just PauseDropping -> return ()   -- stale: the capture will include it
              Just (PauseGotCap cap buf) -> liftIO $
                  writeIORef pausedRef (M.insert pane (PauseGotCap cap (dat : buf)) st)
        EvReply rtag res
          | Just p <- T.stripPrefix "cap:" rtag -> liftIO $ case res of
              Right ls -> modifyIORef' pausedRef (M.insert p (PauseGotCap ls []))
              Left _   -> modifyIORef' pausedRef (M.delete p)   -- give up: resume raw
          | Just p <- T.stripPrefix "cur:" rtag -> do
              st <- liftIO $ readIORef pausedRef
              case (M.lookup p st, res) of
                (Just (PauseGotCap cap buf), Right (stLine : _)) -> do
                    liftJSM $ do
                        writePane sessionId p (buildReplay cap stLine)
                        forM_ (reverse buf) $ writePane sessionId p
                    liftIO $ writeIORef pausedRef (M.delete p st)
                _ -> liftIO $ modifyIORef' pausedRef (M.delete p)
          | otherwise -> return ()
        _ -> return ()

    -- The client is gone: stop offering its control channel to the menu,
    -- and drop any tunnels (their sync routes must not outlive the client).
    performEvent_ $ ffor evE $ \case
        EvExit _ -> liftIO $ do
            unregisterTerminalCC sessionId
            unregisterTerminalSplits sessionId
            unregisterTerminalFocus sessionId
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
      , TerminalActiveWinClosed <$ activeWinClosedE ]

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
getCellMetrics = do
    v <- jsg ("LeksahTerm" :: Text) ^. js0 ("cellMetrics" :: Text)
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

-- | The attached session's current window changed.  tmux broadcasts
-- %session-window-changed for EVERY session to every control client, so the
-- event MUST be filtered to this widget's own session — a foreign session's
-- window id set as csCurrent matches nothing in csLayouts, hiding every
-- window container (the tab shows blank).  The empty session id is
-- 'initialSync''s wildcard for its own seed event.
currentWin :: Text -> TmuxEvent -> Maybe WindowId
currentWin sess (EvSessionWindowChanged s w)
  | s == "" || s == sess = Just w
currentWin _ _ = Nothing

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
  }

-- | ONE pane, as a keyed widget that LIVES ACROSS LAYOUT CHANGES: the div's
-- geometry is a dynamic style (moves are pure attribute updates), a rect
-- change resizes the existing xterm's grid in place, and only the widget's
-- CREATION replays content — so splits/resizes/window switches no longer
-- flash, and scrollback/selection/parser state survive them.
paneWidget
  :: MonadWidget t m
  => CC -> Text -> PaneCallbacks
  -> IORef (M.Map PaneId JSVal) -> IORef (M.Map PaneId PauseState)
  -> IORef (M.Map PaneId TunnelInfo)
  -> (Double, Double) -> PaneId -> Dynamic t (Int, Int, Int, Int)
  -> Dynamic t (Int, Int)    -- ^ layout size in cells (for edge panes)
  -> Dynamic t (Maybe Int)   -- ^ jsaddle-terminal tunnel generation (Just = iframe)
  -> m ()
paneWidget cc sessionId cbs termsRef pausedRef tunnelsRef (cw, ch) pane rectD0 dimsD0 tunnelD0 = do
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
                 <> "style" =: styleOf r d) <$> rectD <*> dimsD <*> tunnelD) $
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
                            "focus" -> liftIO $ ccSend cc ("select-pane -t " <> pane)
                            _ -> return ()
                        _ -> return ())
    pb <- getPostBuild
    performEvent_ $ ffor (tag (current rectD) pb) $ \(_, _, w, h) -> liftJSM $ do
        term <- new (jsg ("Terminal" :: Text)) ()
        opts <- term ^. js ("options" :: Text)
        _ <- opts ^. jss ("fontFamily" :: Text)
                ("Menlo, Monaco, \"Courier New\", monospace" :: Text)
        _ <- opts ^. jss ("fontSize" :: Text) (13 :: Int)
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
        _ <- jsg ("LeksahTermLinks" :: Text) ^. js3 ("attach" :: Text) term
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
        -- The find bar searches the FOCUSED pane: register the SearchAddon
        -- on xterm's own root element (the innermost .terminal the focus
        -- sits in — see onFocusPane in the cm6 bundle).
        termRoot <- term ^. js ("element" :: Text)
        _ <- jsg ("LeksahCM" :: Text) ^. js2 ("loadTerminalSearch" :: Text) term termRoot
        -- Inline images (SIXEL / iTerm2 OSC 1337) — tmux forwards them via
        -- allow-passthrough.  storageLimit caps the per-terminal image cache
        -- (MB); CC layouts have many panes, so keep it modest.
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
                    liftIO $ ccSendBytes cc pane (encodeUtf8 s)
                _ -> return ())
        -- Keep tmux's active pane in step with keyboard focus, so the
        -- Terminal menu's pane commands (split/resize/…, which act on
        -- the current pane) target the pane the user is typing in.
        ta <- term ^. js ("textarea" :: Text)
        _ <- ta ^. js2 ("addEventListener" :: Text) ("focus" :: Text)
                (fun $ \_ _ _ -> liftIO $
                    ccSend cc ("select-pane -t " <> pane))
        liftIO . atomicModifyIORef' termsRef $ \m ->
            (M.insert pane term m, ())
        -- fill the fresh xterm from the pane's current screen + recent
        -- history (also resumes the pane if flow control paused it)
        liftIO $ requestReplay cc pausedRef pane
    -- Layout moved/resized this pane: match the xterm grid to the new
    -- cell rect (the app redraws itself on the SIGWINCH tmux sends it;
    -- xterm reflows its own buffer) — no replay, no re-creation.
    performEvent_ $ ffor (updated rectD) $ \(_, _, w, h) -> liftJSM $ do
        terms <- liftIO $ readIORef termsRef
        forM_ (M.lookup pane terms) $ \term ->
            void $ term ^. js2 ("resize" :: Text) w h

-- | The active-pane highlight of one window's layout: every pane gets a
-- (hidden) transparent box exactly over its rectangle whose mid-grey
-- box-shadow (see terminalCss) marks it as active.  'applyPaneHighlight'
-- shows the active pane's box and hides the rest — pure style toggles, no
-- re-render.
renderHlSegments :: MonadWidget t m => (Double, Double) -> Layout -> m ()
renderHlSegments (cw, ch) l =
    forM_ (layoutPanes l) $ \(pane, x, y, w, h) ->
        -- Half a cell bigger than the pane in every direction, so the box's
        -- edges sit exactly on the divider lines (which run through the
        -- middle of the gutter cells); clipped at the container's top/left.
        -- A pane at the layout's right/bottom edge anchors to the CONTAINER
        -- edge instead, covering the sub-cell remainder the cell grid leaves
        -- there.
        let px v = T.pack (show (round v :: Int)) <> "px"
            pad  = terminalPanePad
        in elAttr "div"
            ("class" =: "terminal-cc-hl"
             <> "data-pane" =: pane
             <> "style" =: ("position:absolute;display:none;pointer-events:none"
                            <> ";left:" <> px (pad + fromIntegral x * cw - cw / 2)
                            <> ";top:"  <> px (pad + fromIntegral y * ch - ch / 2)
                            <> (if x + w >= lW l
                                  then ";right:0"
                                  else ";width:"  <> px (fromIntegral w * cw + cw))
                            <> (if y + h >= lH l
                                  then ";bottom:0"
                                  else ";height:" <> px (fromIntegral h * ch + ch))))
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
    forM_ (zip [1 :: Int ..] (layoutPanes l)) $ \(n, (_, x, y, _, _)) ->
        when (n <= 9) . elAttr "div"
            ("class" =: "leksah-shortcut-badge"
             <> "style" =: ("position:absolute;left:" <> pxAt x cw
                            <> ";top:" <> pxAt y ch <> ";z-index:6")) $
            text ("\8984" <> T.pack (show n))

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

-- | Show the highlight box (see 'renderHlSegments') belonging to pane
-- @mbP@ and hide all others — the active pane gets the shadowed outline.
applyPaneHighlight :: MakeObject e => e -> Maybe PaneId -> JSM ()
applyPaneHighlight c mbP = do
    els <- c ^. js1 ("querySelectorAll" :: Text) (".terminal-cc-hl" :: Text)
    len <- valToNumber =<< els ^. js ("length" :: Text)
    forM_ [0 .. (floor len - 1) :: Int] $ \i -> do
        e <- els ^. js1 ("item" :: Text) i
        pn <- valToText =<< e ^. js1 ("getAttribute" :: Text) ("data-pane" :: Text)
        st <- e ^. js ("style" :: Text)
        void $ st ^. jss ("display" :: Text)
            (if Just pn == mbP then "block" else "none" :: Text)

-- | Initial state sync (an attach replays nothing): current window + layouts.
-- Pane content is replayed per-pane by 'requestReplay' when its xterm is created.
initialSync :: CC -> (TmuxEvent -> IO ()) -> IO ()
initialSync cc fire = do
    -- Flow control: rather than queueing unbounded output for a pane we
    -- can't keep up with, tmux pauses it (%pause) once we're >1s behind and
    -- we jump ahead to its current screen ('requestReplay').
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
requestReplay :: CC -> IORef (M.Map PaneId PauseState) -> PaneId -> IO ()
requestReplay cc pausedRef p = do
    modifyIORef' pausedRef (M.insert p PauseDropping)
    ccSend cc ("refresh-client -A \"" <> p <> ":continue\"")
    -- -S -1000: seed up to 1000 lines of history too — written before the
    -- visible screen they land in the fresh xterm's scrollback.  A pane on
    -- the ALTERNATE screen has no history to capture (tmux clamps to the
    -- screen), so TUIs are unaffected.
    ccCommandTagged cc ("cap:" <> p)
        ("capture-pane -t " <> p <> " -p -e -J -S -1000")
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
