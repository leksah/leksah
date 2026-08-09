{-# LANGUAGE LambdaCase #-}
-- SPDX-License-Identifier: Apache-2.0

-- | Routing for the process-global "act on the active window" bridges.
--
-- Menu/native commands (File ▸ Close/Save, Edit ▸ Find, the app-menu
-- Settings…, the native Open dialog) drop tokens on single-consumer
-- 'Chan's ('IDE.Web.CloseRequest' et al.) because the native menu can only
-- run an 'AppAction' and can't touch the reflex network directly.  With
-- one window a single per-network drain sufficed.  With several OS windows
-- each running its own reflex network, N drains all block on the same
-- 'readChan' and a token would be handed to an arbitrary window — Cmd+W
-- could close a pane in the wrong window.
--
-- So the drains are centralised here: exactly one drain per 'Chan'
-- (started once at boot), each reading the token then firing the trigger
-- of the currently-active window, looked up in a registry every network
-- populates at attach.  Each window's per-network @activePaneD@ then
-- resolves which pane the fired event acts on, so that logic stays
-- untouched.
--
-- The old whole-state resync machinery (per-window signal/ack notifier
-- threads, the global delivery lock, the version guard) is gone: state now
-- reaches windows per-cell, push-based, through @cellDyn@ — a trigger fire
-- per cell write, already async and already per-window.
module IDE.Web.WindowBridge
  ( WindowBridge(..)
  , registerWindowBridge
  , unregisterWindowBridge
  , startWindowBridgeDrains
  , closeWindowMerge
  , setFocusedLeaf
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import GHC.Conc.Sync (labelThread)
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.), (.~), (%~), (&), over, view)
import Control.Monad (forever, when)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import IDE.App (App, appUi, withApp)
import IDE.Reactive (modifyCell, readCell)
import IDE.Web.Model
       (FlipItem(..), LeafId(..), LeksahWindow(..), PaneContent(..),
        PaneKind(..), TabKey, WindowId, activeWindow, flipMru, leksahWindows,
        webWindows, wwActive, wwWide0)

import IDE.Web.CloseRequest (nextCloseRequest)
import IDE.Web.SaveRequest (nextSaveRequest)
import IDE.Web.FindRequest (nextFindRequest)
import IDE.Web.PreferencesRequest (nextPreferencesRequest)
import IDE.Web.ShortcutsRequest (nextShortcutsRequest)
import IDE.Web.BrowserRequest (nextBrowserRequest)
import IDE.Web.KeymapRequest (nextKeymapCommand)
import IDE.Web.OpenFileRequest (nextOpenedFile)
import IDE.Web.Command (Command)
import IDE.Web.RegionGrabRequest (nextRegionGrab)
import IDE.Web.AIContextRequest (AIAction, nextAIAction)
import IDE.Web.NewLwRequest (nextNewLwRequest, nextFontConvert, nextConsolidate)
import IDE.Web.GitLogRequest (nextGitLogRequest)
import IDE.Web.Worktree (nextNewWorktreeRequest, nextReviewRequest)
import IDE.Web.ClaudeQueue
       (nextTaskQueueRequest, nextPlanReviewRequest, nextCompareRequest)
import IDE.Web.TransparencyRequest (nextToggleTransparency)
import IDE.Web.SnapRequest (SnapReq, nextSnapRequest)
import IDE.Web.ConvertRequest (nextConvertRequest)
import IDE.Web.SplitOpenRequest (SplitTarget, nextSplitOpenRequest)
import IDE.Web.AddRemoteRequest (nextAddRemoteRequest)
import IDE.Web.AddServerRequest (nextAddServerRequest)
import IDE.Web.RemoteSettingsRequest (nextRemoteSettings)
import IDE.Ws.Types (ProjectKey)

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
                                      --   'AppAction') into this window's
                                      --   keymap event stream
  , wbOpenedFile :: FilePath -> IO () -- ^ open a natively-chosen file in this window
    -- Everything below arrives from a context menu, a tree button, an
    -- 'AppAction' or the command socket — never from the reflex network — and
    -- so had its own per-window drain until they were centralised here.
  , wbRegionGrab :: Maybe Text -> IO ()
                                      -- ^ start the screen-region picker
  , wbAIAction   :: AIAction -> IO () -- ^ open the AI picker with this payload
  , wbNewLw      :: (Text, Text) -> IO ()
                                      -- ^ a fresh @(session, tmux window)@:
                                      --   mint its leksah window and open it
  , wbFontConvert :: (Text, Text, Int -> Maybe Int) -> IO ()
                                      -- ^ isolate a tmux pane before resizing it
  , wbConsolidate :: Text -> IO ()    -- ^ re-consolidate one leksah window
  , wbGitLog     :: (FilePath, Text) -> IO ()
                                      -- ^ open the git-log pane for @(dir, branch)@
  , wbReview     :: FilePath -> IO () -- ^ open the Review pane for a checkout
  , wbTasks      :: FilePath -> IO () -- ^ open the Claude task queue, seeded
                                      --   with this directory
  , wbPlanReview :: (FilePath, Text) -> IO ()
                                      -- ^ open the plan-review pane
  , wbCompare    :: (FilePath, Text) -> IO ()
                                      -- ^ open the compare-approaches pane
  , wbToggleTransparency :: IO ()     -- ^ toggle the active pane's transparency
  , wbSnap       :: SnapReq -> IO ()  -- ^ snap another app's window onto a pane
  , wbConvert    :: (TabKey, Bool) -> IO ()
                                      -- ^ convert a tab into a pane (or back)
  , wbSplitOpen  :: (SplitTarget, Bool) -> IO ()
                                      -- ^ ⌥-open: split the active pane with this
  , wbAddRemote  :: IO ()             -- ^ Project ▸ Add Remote… modal
  , wbNewWorktree :: FilePath -> IO ()
                                      -- ^ New Claude Session in Worktree… modal
  , wbAddServer  :: IO ()             -- ^ Add Server… modal
  , wbRemoteSettings :: ProjectKey -> IO ()
                                      -- ^ per-project remote settings modal
  }

{-# NOINLINE bridgeRegistry #-}
bridgeRegistry :: IORef (Map WindowId WindowBridge)
bridgeRegistry = unsafePerformIO (newIORef M.empty)

-- | A window's reflex network registers its triggers when it attaches.
registerWindowBridge :: WindowId -> WindowBridge -> IO ()
registerWindowBridge wid b =
  atomicModifyIORef' bridgeRegistry (\m -> (M.insert wid b m, ()))

-- | Drop a window's triggers when it closes (see the native close-merge).
unregisterWindowBridge :: WindowId -> IO ()
unregisterWindowBridge wid =
  atomicModifyIORef' bridgeRegistry (\m -> (M.delete wid m, ()))

-- | The native window-close handler, shared by every front end.  When OS
-- window @wid@ closes: drop its bridge, then merge its wide0 tabs into the
-- frontmost remaining window (its 'activeWindow', else the lowest-id one), so
-- no open editor/terminal is orphaned.  Closing the LAST window runs @quit@ —
-- each platform passes its own (hard @exitImmediately@ on macOS;
-- @applicationQuit@ on GTK; the message-loop teardown on Windows).  A no-op if
-- @wid@ is already gone (a double close) or the app hasn't booted yet.
closeWindowMerge :: IO () -> WindowId -> IO ()
closeWindowMerge quit wid = withApp $ \app -> do
    unregisterWindowBridge wid
    ui <- readCell (appUi app)
    let wins = ui ^. webWindows
    case M.lookup wid wins of
      Nothing -> return ()   -- already merged/gone
      Just _  -> do
        let others = M.delete wid wins
        case M.keys others of
          [] -> quit                                    -- last window → quit
          _  ->
            -- Re-derive inside the write so a concurrent edit can't be lost.
            modifyCell (appUi app) $ \u ->
              case M.lookup wid (u ^. webWindows) of
                Nothing -> u
                Just cl ->
                  let rest = M.delete wid (u ^. webWindows)
                      target = case u ^. activeWindow of
                                 Just a | a /= wid, M.member a rest -> a
                                 _ -> fst (M.findMin rest)
                      merge tw = tw & wwWide0  %~ (++ cl ^. wwWide0)
                                    & wwActive %~ (<|> cl ^. wwActive)
                  in u & webWindows   .~ M.adjust merge target rest
                       & activeWindow .~ Just target

-- | The bridge of the frontmost (active) window, falling back to any
-- registered window (there is always at least one live window while a token
-- can arrive).
activeBridge :: App -> IO (Maybe WindowBridge)
activeBridge app = do
  act <- view activeWindow <$> readCell (appUi app)
  reg <- readIORef bridgeRegistry
  return $ case act >>= (`M.lookup` reg) of
    Just b  -> Just b
    Nothing -> snd <$> M.lookupMin reg

route :: App -> (WindowBridge -> IO ()) -> IO ()
route app fire = activeBridge app >>= mapM_ fire

-- | Start the single process-wide drain per bridge 'Chan'.  Called once at
-- boot; each drain blocks on its 'Chan' and fires the active window's
-- trigger for every token.
startWindowBridgeDrains :: App -> IO ()
startWindowBridgeDrains app = do
  let drain name act = forkIO (forever act) >>= (`labelThread` name)
  drain "bridge-drain-close" $ nextCloseRequest       >>  route app wbClose
  drain "bridge-drain-save"  $
    nextSaveRequest >>= \mv -> route app (`wbSave` mv)
  drain "bridge-drain-find"  $ nextFindRequest        >>  route app wbFind
  drain "bridge-drain-prefs" $ nextPreferencesRequest >>  route app wbPrefs
  drain "bridge-drain-shortcuts" $ nextShortcutsRequest >> route app wbShortcuts
  drain "bridge-drain-browser" $ nextBrowserRequest     >>  route app wbBrowser
  drain "bridge-drain-keymap" $
    nextKeymapCommand >>= \c -> route app (`wbKeymap` c)
  drain "bridge-drain-open"  $
    nextOpenedFile >>= \fp -> route app (`wbOpenedFile` fp)
  -- Migrated from per-window drains in "IDE.Web.Main" (each was one
  -- @forkIO . forever $ nextX >>= fireX@ inside the per-window network, i.e.
  -- N readers of one 'Chan' — so every one of these opened its pane or dialog
  -- in an ARBITRARY window).  Same rule as the ones above: the frontmost
  -- window acts, because that is the window whose menu, tree or context menu
  -- the user just used.
  drain "bridge-drain-region"  $
    nextRegionGrab >>= \t -> route app (`wbRegionGrab` t)
  drain "bridge-drain-ai"      $
    nextAIAction >>= \a -> route app (`wbAIAction` a)
  drain "bridge-drain-newlw"   $
    nextNewLwRequest >>= \r -> route app (`wbNewLw` r)
  drain "bridge-drain-fontconv" $
    nextFontConvert >>= \r -> route app (`wbFontConvert` r)
  drain "bridge-drain-consolidate" $
    nextConsolidate >>= \i -> route app (`wbConsolidate` i)
  drain "bridge-drain-gitlog"  $
    nextGitLogRequest >>= \r -> route app (`wbGitLog` r)
  drain "bridge-drain-review"  $
    nextReviewRequest >>= \d -> route app (`wbReview` d)
  drain "bridge-drain-tasks"   $
    nextTaskQueueRequest >>= \d -> route app (`wbTasks` d)
  drain "bridge-drain-plan"    $
    nextPlanReviewRequest >>= \r -> route app (`wbPlanReview` r)
  drain "bridge-drain-compare" $
    nextCompareRequest >>= \r -> route app (`wbCompare` r)
  drain "bridge-drain-transparency" $
    nextToggleTransparency >> route app wbToggleTransparency
  drain "bridge-drain-snap"    $
    nextSnapRequest >>= \r -> route app (`wbSnap` r)
  drain "bridge-drain-convert" $
    nextConvertRequest >>= \r -> route app (`wbConvert` r)
  drain "bridge-drain-splitopen" $
    nextSplitOpenRequest >>= \r -> route app (`wbSplitOpen` r)
  drain "bridge-drain-addremote" $
    nextAddRemoteRequest >> route app wbAddRemote
  drain "bridge-drain-newworktree" $
    nextNewWorktreeRequest >>= \d -> route app (`wbNewWorktree` d)
  drain "bridge-drain-addserver" $
    nextAddServerRequest >> route app wbAddServer
  drain "bridge-drain-remotesettings" $
    nextRemoteSettings >>= \pk -> route app (`wbRemoteSettings` pk)

-- | Focus entered a native pane: record it as the leksah window's focused
-- pane (the target of ⌘+/⌘− and future splits) and, for a VIEW pane, float
-- its flipper entry.  Pre-checked so a no-op never writes the cell (focusin
-- fires on every click).
--
-- The 'FlipView' float is this function's job because a view pane has no
-- other recency signal: a tmux pane click publishes its pane id
-- (@leksahPaneFocus@, termActivityJs) and a tab click promotes through
-- @lwTabFlipE@, but focus landing in a browser \/ editor \/ git-log LEAF —
-- by click, by ⌥-open, or from a click inside a native browser view
-- (@leksahBrowserActivate@) — only ever reached 'lwFocused', so the pane
-- took the active ring without moving to the front of the flipper.
setFocusedLeaf :: Text -> LeafId -> IO ()
setFocusedLeaf i lid@(LeafId l) = withApp $ \app -> do
    ui <- readCell (appUi app)
    let lws       = ui ^. leksahWindows
        mru       = ui ^. flipMru
        mlw       = M.lookup i lws
        present   = maybe False ((lid `M.member`) . lwPanes) mlw
        needFocus = fmap lwFocused mlw /= Just (Just lid)
        isView    = case pcKind <$> (M.lookup lid . lwPanes =<< mlw) of
                      Just PaneView{} -> True
                      _               -> False
        item      = FlipView i l
        needFloat = isView && take 1 mru /= [item]
    when (present && (needFocus || needFloat)) . modifyCell (appUi app) $
          (if needFocus
             then over leksahWindows
                    (M.adjust (\lw -> lw { lwFocused = Just lid }) i)
             else id)
        . (if needFloat
             then over flipMru ((item :) . filter (/= item))
             else id)
