{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
-- Phase-2 scope (see docs/tmux-control-mode.md §4): a layout change
-- re-renders the window's panes (visible content is replayed via
-- @capture-pane@; scrollback isn't) — keyed per-pane widgets are phase 3;
-- no underlay holes, find-bar search, or link providers in CC panes yet.
module IDE.Web.Widget.TerminalCC
  ( terminalCCWidget
  ) where

import Control.Concurrent (forkIO)
import Control.Lens ((^.))
import Control.Monad (forM_, forever, when, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.IORef
       (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef,
        writeIORef)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Read (readMaybe)

import Reflex
       (Dynamic, Event, ffor, fmapMaybe, foldDyn, getPostBuild, holdDyn,
        holdUniqDyn, leftmost, newTriggerEvent, performEvent, performEvent_,
        updated)
import Reflex.Dom.Core
       (MonadWidget, blank, divClass, dyn_, elAttr, elAttr', text,
        widgetHold, _element_raw, (=:))
import Language.Javascript.JSaddle
       (JSM, JSVal, MakeObject, fun, js, js0, js1, js2, js3, jsg, jss,
        liftJSM, new, valIsNull, valIsUndefined, valToBool, valToNumber,
        valToText)

import IDE.Core.State (IDE)
import IDE.Web.Events (TerminalEvents(..))
import IDE.Web.TerminalInput (registerTerminalCC, unregisterTerminalCC)
import IDE.Web.TmuxCC

-- | Session-level widget: one control client; the current window's panes at
-- their exact tmux layout rectangles.  Same shape as 'terminalWidget' so
-- Main.hs can swap them.
terminalCCWidget
  :: forall t m . MonadWidget t m
  => Dynamic t IDE
  -> Text                -- ^ tmux session id (\"$3\") or \"ssh://host[#target]\"
  -> Event t ()          -- ^ fires when this tab is selected
  -> m (Event t TerminalEvents)
terminalCCWidget _ide sessionId selectedE = do
    pb <- getPostBuild
    (evE, fireEv) <- newTriggerEvent
    (ccStartedE, fireCCStarted) <- newTriggerEvent

    -- Start the control client once the widget exists; drain its events into
    -- reflex from a background thread.  "ssh://host" attaches-or-creates the
    -- remote "leksah" session; "ssh://host#$3" attaches that exact session;
    -- otherwise it's a local session id on leksah's socket.  ssh runs without
    -- a PTY (plain pipes), so the protocol stream is identical either way.
    performEvent_ $ ffor pb $ \_ -> liftIO . void . forkIO $ do
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
        -- jsaddle bridge and made typing choppy).
        _ <- forkIO . forever $ ccEventsBatch cc >>= mapM_ fireEv . coalesceOutputs
        -- NB initialSync runs from the session widget below, NOT here: its
        -- layout events would race the widgetHold swap — the foldDyn that
        -- consumes them doesn't exist yet, and events fired before it builds
        -- are dropped (seen as permanently blank panes under startup load).
        fireCCStarted cc

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
        ffor ccStartedE $ \cc -> do
            -- Register this tab's control channel for the Terminal menu's
            -- pane commands (split/select/resize/…): they run verbatim —
            -- the control client's current window/pane IS the displayed one.
            liftIO . registerTerminalCC sessionId $ ccSend cc
            -- Initial sync now that this widget (and its foldDyn below) exists
            -- and is subscribed — see the race note above.
            pbSync <- getPostBuild
            performEvent_ $ ffor pbSync $ \_ ->
                liftIO . void . forkIO $ initialSync cc fireEv
            -- Window/layout state folded from notifications.
            stD <- foldDyn ($) (CCState M.empty Nothing) $ leftmost
                [ ffor (fmapMaybe layoutOf evE) $ \(w, l) s ->
                      s { csLayouts = M.insert w l (csLayouts s) }
                , ffor (fmapMaybe closedWin evE) $ \w s ->
                      s { csLayouts = M.delete w (csLayouts s) }
                , ffor (fmapMaybe currentWin evE) $ \w s ->
                      s { csCurrent = Just w }
                ]
            layoutD <- holdUniqDyn $ ffor stD $ \s ->
                csCurrent s >>= (`M.lookup` csLayouts s)
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
                        let cols = max 20 (floor (w / cw) :: Int)
                            rows = max 5 (floor (h / ch) :: Int)
                        when (w > 0 && h > 0) $ do
                            changed <- liftIO $ atomicModifyIORef' lastSizeRef $ \old ->
                                ((cols, rows), old /= (cols, rows))
                            when changed . liftIO . void . forkIO $ applySize cols rows
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
            -- renderLayout lays (hidden) along every pane's gutter edges.
            activePaneRef <- liftIO $ newIORef (Nothing :: Maybe PaneId)
            -- Whether the container held keyboard focus just before a layout
            -- re-render (which disposes the focused xterm and drops focus on
            -- the floor) — if it did, focus is restored to the active pane.
            hadFocusRef <- liftIO $ newIORef False
            let applyActive :: JSM ()
                applyActive = do
                    mbC <- liftIO $ readIORef containerRef
                    mbP <- liftIO $ readIORef activePaneRef
                    forM_ mbC $ \c -> applyPaneHighlight c mbP
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
                    forM_ (mbP >>= (`M.lookup` terms)) $ \term ->
                        void $ term ^. js0 ("focus" :: Text)
                -- Highlight the new active pane and, IF this terminal already
                -- owned the keyboard, hand the keyboard to it too: activating
                -- a pane (menu select-split, a fresh ⌘D split) should mean
                -- typing goes there — but a background session's pane change
                -- must not steal focus from the editor.
                followActive :: JSM ()
                followActive = do
                    applyActive
                    had <- (||) <$> containerHasFocus
                                <*> liftIO (readIORef hadFocusRef)
                    liftIO $ writeIORef hadFocusRef False
                    when had focusActivePane
            performEvent_ $ ffor evE $ \case
                EvWindowPaneChanged _ p -> do
                    liftIO $ writeIORef activePaneRef (Just p)
                    liftJSM followActive
                -- Flow control: tmux paused a pane we fell behind on.  If we
                -- display it, jump ahead to its current screen; if not, leave
                -- it paused — no point receiving output nobody renders (its
                -- xterm gets a requestReplay — which resumes — on creation).
                EvPause p -> liftIO $ do
                    terms <- readIORef termsRef
                    when (M.member p terms) $ requestReplay cc pausedRef p
                _ -> return ()
            -- Selecting this tab focuses its active pane (the classic
            -- widget's behaviour), so typing works without an extra click.
            performEvent_ $ ffor selectedE $ \_ -> liftJSM focusActivePane
            (containerEl, _) <- elAttr' "div"
                ("class" =: "terminal terminal-cc"
                 <> "style" =: "position:relative;width:100%;height:100%;overflow:hidden") $
                dyn_ $ ffor ((,) <$> layoutD <*> metricsD) $ \case
                    (Just l, Just cell) -> do
                        -- Note focus BEFORE disposing: the teardown destroys
                        -- the focused textarea, after which nobody remembers.
                        had <- liftJSM containerHasFocus
                        when had . liftIO $ writeIORef hadFocusRef True
                        disposeAll termsRef
                        renderLayout cc sessionId termsRef pausedRef cell followActive l
                    _ -> divClass "terminal-cc-empty" $ text "(connecting…)"
            liftIO $ writeIORef containerRef (Just (_element_raw containerEl))
            pb2 <- getPostBuild
            performEvent_ $ ffor pb2 $ \_ -> liftJSM $ do
                refit
                ro <- new (jsg ("ResizeObserver" :: Text)) (fun $ \_ _ _ -> refit)
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
        EvReply tag res
          | Just p <- T.stripPrefix "cap:" tag -> liftIO $ case res of
              Right ls -> modifyIORef' pausedRef (M.insert p (PauseGotCap ls []))
              Left _   -> modifyIORef' pausedRef (M.delete p)   -- give up: resume raw
          | Just p <- T.stripPrefix "cur:" tag -> do
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

    -- The client is gone: stop offering its control channel to the menu.
    performEvent_ $ ffor evE $ \case
        EvExit _ -> liftIO $ unregisterTerminalCC sessionId
        _        -> return ()

    -- Events for the rest of the IDE: window renames update the tab title;
    -- the client exiting (session ended) closes the tab — the same contract
    -- as the PTY widget's reader-EOF path.
    return $ fmapMaybe (\case
        EvWindowRenamed _ nm -> Just (TerminalTitle nm)
        EvExit _             -> Just TerminalExited
        _                    -> Nothing) evE

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

currentWin :: TmuxEvent -> Maybe WindowId
currentWin (EvSessionWindowChanged _ w) = Just w
currentWin _ = Nothing

data CCState = CCState
  { csLayouts :: M.Map WindowId Layout
  , csCurrent :: Maybe WindowId
  }

paneKey :: Text -> PaneId -> Text
paneKey sess pane = sess <> "/" <> pane

-- | Dispose (and drop) every xterm this widget created.
disposeAll :: MonadWidget t m => IORef (M.Map PaneId JSVal) -> m ()
disposeAll termsRef = do
    old <- liftIO $ atomicModifyIORef' termsRef (\m -> (M.empty, m))
    forM_ (M.toList old) $ \(_p, term) -> liftJSM $
        void (term ^. js0 ("dispose" :: Text))

-- | Render the window's panes at their EXACT tmux layout rectangles:
-- absolute position @(x,y) × cell@, size @(w,h) × cell@ — each pane's DOM
-- box is precisely its grid, and the separator cells between panes become
-- the visible gutters.  Every (re)created xterm starts empty, so its visible
-- content is replayed (@capture-pane -e@) through the normal 'EvOutput'
-- route.
renderLayout
  :: MonadWidget t m
  => CC -> Text -> IORef (M.Map PaneId JSVal) -> IORef (M.Map PaneId PauseState)
  -> (Double, Double) -> JSM () -> Layout -> m ()
renderLayout cc sessionId termsRef pausedRef (cw, ch) applyHl l = do
    forM_ (layoutPanes l) $ \(pane, x, y, w, h) -> paneDiv pane x y w h
    -- Active-pane highlight segments: for every pane, a (hidden) 1px line
    -- along each of its sides that faces a gutter, centered in that gutter
    -- exactly over the grey divider line.  'applyPaneHighlight' shows the
    -- segments of the active pane (green perimeter) and hides the rest —
    -- toggling needs no re-render, so xterms aren't disturbed by focus moves.
    forM_ (layoutPanes l) $ \(pane, x, y, w, h) -> do
        let seg geo = elAttr "div"
                ("class" =: "terminal-cc-hl"
                 <> "data-pane" =: pane
                 <> "style" =: ("position:absolute;display:none"
                                <> ";pointer-events:none;background:rgb(80,200,120);" <> geo))
                blank
        when (x > 0) . seg $
            "width:1px;left:" <> pxMid (x - 1) cw
            <> ";top:" <> pxAt y ch <> ";height:" <> pxSpan y h ch
        when (x + w < lW l) . seg $
            "width:1px;left:" <> pxMid (x + w) cw
            <> ";top:" <> pxAt y ch <> ";height:" <> pxSpan y h ch
        when (y > 0) . seg $
            "height:1px;top:" <> pxMid (y - 1) ch
            <> ";left:" <> pxAt x cw <> ";width:" <> pxSpan x w cw
        when (y + h < lH l) . seg $
            "height:1px;top:" <> pxMid (y + h) ch
            <> ";left:" <> pxAt x cw <> ";width:" <> pxSpan x w cw
    pbHl <- getPostBuild
    performEvent_ $ ffor pbHl $ \_ -> liftJSM applyHl
    -- tmux's separator cells between panes are blank gutters here (a full
    -- cell: ~8px wide / 15px tall).  Each becomes a grab strip with a crisp
    -- 1px line centered in it (the way iTerm2 fills its tmux dividers), and
    -- dragging it resizes the split: the line ghosts with the pointer (in
    -- JS — see 'dividerDragJs'), and on drop the whole-cell delta runs
    -- @resize-pane@ on the divider's target pane; the resulting
    -- %layout-change re-renders everything at the new rectangles.
    forM_ (layoutDividers l) $ \(vert, x, y, w, h, target) -> do
        (dEl, _) <- elAttr' "div"
            ("class" =: ("terminal-cc-divider " <> (if vert then "vert" else "horiz"))
             <> "style" =: ("position:absolute"
                            <> ";left:"   <> pxAt x cw
                            <> ";top:"    <> pxAt y ch
                            <> ";width:"  <> pxSpan x w cw
                            <> ";height:" <> pxSpan y h ch)) $
            elAttr "div"
                ("class" =: "divider-line"
                 <> "style" =: (if vert
                      then "position:absolute;left:calc(50% - 0.5px);top:0;bottom:0;width:1px"
                      else "position:absolute;top:calc(50% - 0.5px);left:0;right:0;height:1px"))
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
  where
    pxAt :: Int -> Double -> Text
    pxAt n cell = T.pack (show (round (fromIntegral n * cell) :: Int)) <> "px"

    -- Exact pixel span of @n@ cells starting at cell @o@ (avoids the drift
    -- of rounding the width independently of the position).
    pxSpan :: Int -> Int -> Double -> Text
    pxSpan o n cell =
        T.pack (show ((round (fromIntegral (o + n) * cell)
                       - round (fromIntegral o * cell)) :: Int)) <> "px"

    -- The pixel at the middle of separator cell @n@ (where the 1px divider
    -- line is drawn; highlight segments sit exactly over it).
    pxMid :: Int -> Double -> Text
    pxMid n cell =
        T.pack (show (round ((fromIntegral n + 0.5) * cell - 0.5) :: Int)) <> "px"

    paneDiv pane x y w h = do
        (paneEl, _) <- elAttr' "div"
            ("class" =: "terminal-cc-pane"
             <> "style" =: ("position:absolute;overflow:hidden"
                            <> ";left:" <> pxAt x cw <> ";top:" <> pxAt y ch
                            <> ";width:" <> pxAt w cw <> ";height:" <> pxAt h ch))
            blank
        pb <- getPostBuild
        performEvent_ $ ffor pb $ \_ -> liftJSM $ do
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
            _ <- jsg ("LeksahTerm" :: Text) ^. js2 ("register" :: Text)
                     (paneKey sessionId pane) term
            _ <- term ^. js1 ("open" :: Text) (_element_raw paneEl)
            -- the grid IS the tmux pane's cells; the box already matches
            _ <- term ^. js2 ("resize" :: Text) w h
            -- keystrokes → tmux (send-keys -H).  ccCommand blocks on the
            -- reply, so hop off the jsaddle callback thread.
            -- Keystrokes → tmux.  ccSendBytes is fire-and-forget now, so no
            -- forkIO: sending inline keeps keystroke ORDER (concurrent forks
            -- could race for the submit lock and swap two fast keypresses).
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
            -- fill the fresh xterm from the pane's current screen (also
            -- resumes the pane if flow control had paused it while hidden)
            liftIO $ requestReplay cc pausedRef pane

-- | Show the highlight segments (see 'renderLayout') belonging to pane
-- @mbP@ and hide all others — the active pane's perimeter turns green.
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
-- Pane content is replayed per-pane by 'replayPane' when its xterm is created.
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
    ccCommandTagged cc ("cap:" <> p) ("capture-pane -t " <> p <> " -p -e -J")
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
