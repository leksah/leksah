{-# LANGUAGE LambdaCase #-}
-- | Routing for the process-global "act on the active window" bridges.
--
-- Menu/native commands (File ▸ Close/Save, Edit ▸ Find, the app-menu Settings…,
-- the native Open dialog) drop tokens on single-consumer 'Chan's
-- ('IDE.Web.CloseRequest' et al.) because the native menu can only run a
-- command's 'IDEAction' and can't touch the reflex network directly.  With one
-- window a single per-network drain sufficed.  With several OS windows each
-- running its own reflex network, N drains all block on the same 'readChan' and
-- a token would be handed to an arbitrary window — Cmd+W could close a pane in
-- the wrong window.
--
-- So the drains are centralised here: exactly one drain per 'Chan' (started once
-- from 'newIDE'), each reading the token then firing the trigger of the
-- currently-active window, looked up in a registry every network populates at
-- attach.  Each window's per-network 'activePaneD' then resolves which pane the
-- fired event acts on, so that logic stays untouched.
module IDE.Web.WindowBridge
  ( WindowBridge(..)
  , registerWindowBridge
  , unregisterWindowBridge
  , startWindowBridgeDrains
  , registerResync
  , unregisterResync
  , notifyResync
  , resyncStates
  , closeWindowMerge
  , setFocusedLeaf
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, killThread, ThreadId)
import GHC.Conc.Sync (labelThread)
import Control.Concurrent.MVar
       (MVar, readMVar, newMVar, newEmptyMVar, takeMVar, tryPutMVar, tryReadMVar, withMVar)
import Control.Lens ((^.), (.~), (%~), (&), over)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import IDE.Core.State (reflectIDE, readIDE, modifyIDE_)
import IDE.Core.Types
       (IDERef, WindowId, activeWindow, webWindows, wwWide0, wwActive,
        leksahWindows, flipMru, FlipItem(..), LeksahWindow(..),
        PaneContent(..), PaneKind(..), LeafId(..))
import IDE.Web.IDERefStore (getGlobalIDERef)

import IDE.Web.CloseRequest (nextCloseRequest)
import IDE.Web.SaveRequest (nextSaveRequest)
import IDE.Web.FindRequest (nextFindRequest)
import IDE.Web.PreferencesRequest (nextPreferencesRequest)
import IDE.Web.ShortcutsRequest (nextShortcutsRequest)
import IDE.Web.BrowserRequest (nextBrowserRequest)
import IDE.Web.KeymapRequest (nextKeymapCommand)
import IDE.Web.OpenFileRequest (nextOpenedFile)
import IDE.Web.Command (Command)

-- | One window's set of "act on me" triggers (the per-network reflex fire
-- functions, already partially applied to their unit argument where relevant).
data WindowBridge = WindowBridge
  { wbClose      :: IO ()             -- ^ close the active pane in this window
  , wbSave       :: Maybe (MVar ()) -> IO ()
                                      -- ^ save the active editor in this window
                                      --   (acking the caller's completion slot
                                      --   once the write settles, if given)
  , wbFind       :: IO ()             -- ^ toggle the find bar in this window
  , wbPrefs      :: IO ()             -- ^ show the Preferences pane in this window
  , wbShortcuts  :: IO ()             -- ^ show the Shortcuts pane in this window
  , wbBrowser    :: IO ()             -- ^ open a new browser pane in this window
  , wbKeymap     :: Command -> IO ()  -- ^ inject a stream-handled command (no
                                      --   'IDEAction') into this window's
                                      --   keymap event stream
  , wbOpenedFile :: FilePath -> IO () -- ^ open a natively-chosen file in this window
  }

{-# NOINLINE bridgeRegistry #-}
bridgeRegistry :: IORef (Map WindowId WindowBridge)
bridgeRegistry = unsafePerformIO (newIORef M.empty)

-- Resync notifier: the event-driven replacement for the old fixed-rate MVar
-- poll.  A 'newTriggerEvent' fire is only a non-blocking @writeChan@ — the
-- reflex frame ALWAYS runs on the target window's own @processAsyncEvents@
-- host thread, regardless of who called the fire (reflex TriggerEvent/Base).
-- The hazard that froze an earlier event-driven attempt ("broadcastResync")
-- was NOT the calling thread but the RATE: one un-coalesced fire per
-- 'modifyIDE_' flooded frames that all contend on the single global Spider
-- lock (every window shares @SpiderTimelineEnv Global@) while each frame does
-- synchronous jsaddle JS.  So the invariant here is COALESCING: per window, a
-- binary signal 'MVar' plus an ack — the notifier thread fires at most one
-- resync frame at a time (fire, then block until the frame's handler acks),
-- and a mutation burst collapses into the already-set signal.  Do not remove
-- the ack, and do not fire per-mutation.

{-# NOINLINE resyncSignals #-}
resyncSignals :: IORef (Map WindowId (MVar (), MVar (), ThreadId))
resyncSignals = unsafePerformIO (newIORef M.empty)

-- A process-global lock so at most ONE window's resync frame is in flight across
-- the whole app (held fire→ack).  The multi-window freeze was two windows'
-- reflex frames doing synchronous jsaddle flushes CONCURRENTLY on the single
-- shared WKWebView main-thread bridge: one frame thread wedged BlockedOnMVar
-- forever (its async 'js eval' bridge stayed alive — the sync path is what
-- deadlocks), while the other window kept running (the windows do NOT share one
-- Spider lock, so their frames genuinely overlap).  Serializing resync delivery
-- here removes the concurrency and the wedge.  (Under a pathological same-tick
-- mutation burst a background window can lag a version or two, but it catches up
-- via the heartbeat; real cross-window interaction never approaches that rate.)
{-# NOINLINE resyncGlobalLock #-}
resyncGlobalLock :: MVar ()
resyncGlobalLock = unsafePerformIO (newMVar ())

-- | Register a window's resync at network attach: @fire@ is the window's
-- 'newTriggerEvent' fire; @ack@ is put by the window's resync frame handler
-- once it has read the fresh shared state.  Forks the per-window notifier
-- thread implementing the coalescing loop above.
registerResync :: WindowId -> IO () -> MVar () -> IO ()
registerResync wid fire ack = do
  sig <- newEmptyMVar
  tid <- forkIO . forever $
    takeMVar sig >> withMVar resyncGlobalLock (\_ -> fire >> takeMVar ack)
  labelThread tid ("resync-notifier-" <> show wid)
  atomicModifyIORef' resyncSignals (\m -> (M.insert wid (sig, ack, tid) m, ()))

-- | Drop a window's resync when it closes (kills its notifier thread).
unregisterResync :: WindowId -> IO ()
unregisterResync wid = do
  mOld <- atomicModifyIORef' resyncSignals (\m -> (M.delete wid m, M.lookup wid m))
  mapM_ (\(_, _, tid) -> killThread tid) mOld

-- | Installed in the ideR trigger slot ('modifyIDEM' runs it after every
-- mutation, on the mutating thread): only non-blocking 'tryPutMVar's — no
-- reflex, no JS, no locks on the mutating thread.
notifyResync :: IO ()
notifyResync = readIORef resyncSignals >>= mapM_ (\(sig, _, _) -> void (tryPutMVar sig ()))

-- | Diagnostic snapshot for `leksah-cmd resync-state`: each window's signal/ack
-- occupancy.  In a healthy idle instance both are empty; a wedged window shows
-- sig=full (mutations arriving) and reveals whether the notifier is stuck
-- pre-fire (ack empty, frame never ran) or the handler acked but the notifier
-- never woke (ack full).
resyncStates :: IO [(WindowId, Bool, Bool)]
resyncStates = do
  m <- readIORef resyncSignals
  mapM (\(wid, (sig, ack, _)) -> do
          s <- tryReadMVar sig
          a <- tryReadMVar ack
          return (wid, s /= Nothing, a /= Nothing))
       (M.toList m)

-- | A window's reflex network registers its triggers when it attaches.
registerWindowBridge :: WindowId -> WindowBridge -> IO ()
registerWindowBridge wid b =
  atomicModifyIORef' bridgeRegistry (\m -> (M.insert wid b m, ()))

-- | Drop a window's triggers when it closes (see the native close-merge).
unregisterWindowBridge :: WindowId -> IO ()
unregisterWindowBridge wid =
  atomicModifyIORef' bridgeRegistry (\m -> (M.delete wid m, ()))

-- | The native window-close handler, shared by every front end.  When OS window
-- @wid@ closes: drop its bridge + resync, then merge its wide0 tabs into the
-- frontmost remaining window (its 'activeWindow', else the lowest-id one), so no
-- open editor/terminal is orphaned.  Closing the LAST window runs @quit@ — each
-- platform passes its own (hard @exitImmediately@ on macOS; @applicationQuit@ on
-- GTK; the message-loop teardown on Windows).  A no-op if @wid@ is already gone
-- (a double close) or no 'IDERef' has been published yet.  Keeping the merge here
-- (rather than per platform) guarantees macOS/Linux/Windows behave identically;
-- the @quit@ callback is the only platform-specific bit.  Mirrors what
-- 'IDE.Web.MacMenu.leksah_window_closing' used to do inline.
closeWindowMerge :: IO () -> WindowId -> IO ()
closeWindowMerge quit wid = getGlobalIDERef >>= \case
  Nothing   -> return ()
  Just ideR -> do
    unregisterWindowBridge wid
    unregisterResync wid
    (`reflectIDE` ideR) $ do
      wins <- readIDE webWindows
      act  <- readIDE activeWindow
      case M.lookup wid wins of
        Nothing      -> return ()   -- already merged/gone
        Just closing -> do
          let others = M.delete wid wins
          case M.keys others of
            [] -> liftIO quit                             -- last window → quit
            _  -> do
              let target = case act of
                             Just a | a /= wid, M.member a others -> a
                             _ -> fst (M.findMin others)
                  merge tw = tw & wwWide0  %~ (++ closing ^. wwWide0)
                                & wwActive %~ (<|> closing ^. wwActive)
              modifyIDE_ $ \i -> i & webWindows   .~ M.adjust merge target others
                                   & activeWindow .~ Just target

-- | The bridge of the frontmost (active) window, falling back to any registered
-- window (there is always at least one live window while a token can arrive).
activeBridge :: IDERef -> IO (Maybe WindowBridge)
activeBridge ideR = do
  ide <- snd <$> readMVar ideR
  reg <- readIORef bridgeRegistry
  let byActive = (ide ^. activeWindow) >>= (`M.lookup` reg)
  return $ case byActive of
    Just b  -> Just b
    Nothing -> snd <$> M.lookupMin reg

route :: IDERef -> (WindowBridge -> IO ()) -> IO ()
route ideR fire = activeBridge ideR >>= mapM_ fire

-- | Start the single process-wide drain per bridge 'Chan'.  Called once from
-- 'newIDE'; each drain blocks on its 'Chan' and fires the active window's
-- trigger for every token.
startWindowBridgeDrains :: IDERef -> IO ()
startWindowBridgeDrains ideR = do
  let drain name act = forkIO (forever act) >>= (`labelThread` name)
  drain "bridge-drain-close" $ nextCloseRequest       >>  route ideR wbClose
  drain "bridge-drain-save"  $
    nextSaveRequest >>= \mv -> route ideR (`wbSave` mv)
  drain "bridge-drain-find"  $ nextFindRequest        >>  route ideR wbFind
  drain "bridge-drain-prefs" $ nextPreferencesRequest >>  route ideR wbPrefs
  drain "bridge-drain-shortcuts" $ nextShortcutsRequest >> route ideR wbShortcuts
  drain "bridge-drain-browser" $ nextBrowserRequest     >>  route ideR wbBrowser
  drain "bridge-drain-keymap" $
    nextKeymapCommand >>= \c -> route ideR (`wbKeymap` c)
  drain "bridge-drain-open"  $
    nextOpenedFile >>= \fp -> route ideR (`wbOpenedFile` fp)

-- Moved here from IDE.Web.Widget.TerminalCC: it is pure IDE-state logic (no
-- tmux, no processes), and TerminalCC is stubbed out wholesale for the GHC JS
-- backend, which left the browser build unable to import it at all.  Window
-- focus state belongs with the rest of the window bridge anyway.
-- | Focus entered a native pane: record it as the leksah window's focused
-- pane (the target of ⌘+/⌘− and future splits) and, for a VIEW pane, float its
-- flipper entry.  Pre-checked so a no-op never bumps the resync version
-- (focusin fires on every click).
--
-- The 'FlipView' float is this function's job because a view pane has no other
-- recency signal: a tmux pane click publishes its pane id (@leksahPaneFocus@,
-- termActivityJs) and a tab click promotes through @lwTabFlipE@, but focus
-- landing in a browser \/ editor \/ git-log LEAF — by click, by ⌥-open, or from
-- a click inside a native browser view (@leksahBrowserActivate@) — only ever
-- reached 'lwFocused', so the pane took the active ring without moving to the
-- front of the flipper.
setFocusedLeaf :: Text -> LeafId -> IO ()
setFocusedLeaf i lid@(LeafId l) = getGlobalIDERef >>= mapM_ (\ideR ->
    (`reflectIDE` ideR) $ do
        lws <- readIDE leksahWindows
        mru <- readIDE flipMru
        let mlw       = M.lookup i lws
            present   = maybe False ((lid `M.member`) . lwPanes) mlw
            needFocus = fmap lwFocused mlw /= Just (Just lid)
            isView    = case pcKind <$> (M.lookup lid . lwPanes =<< mlw) of
                          Just PaneView{} -> True
                          _               -> False
            item      = FlipView i l
            needFloat = isView && take 1 mru /= [item]
        when (present && (needFocus || needFloat)) . modifyIDE_ $
              (if needFocus
                 then over leksahWindows
                        (M.adjust (\lw -> lw { lwFocused = Just lid }) i)
                 else id)
            . (if needFloat
                 then over flipMru ((item :) . filter (/= item))
                 else id))
