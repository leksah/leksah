{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
-- | A process-global "the local tmux session/window/pane tree changed"
-- notification — the tmux twin of 'IDE.Web.LocalRefresh'.  The Terminals side
-- pane used to poll @tmux list-panes -a@ every 2s just to catch external
-- renames / window adds; instead a single persistent tmux control-mode
-- (@tmux -C@) monitor client watches the whole server and fires one of these on
-- any structural notification (@%sessions-changed@, @%session-renamed@,
-- @%window-add@\/@-close@\/@-renamed@, @%unlinked-window-*@, …).  No wall-clock
-- timer ticks while the IDE is idle.
--
-- The monitor attaches to a dedicated hidden session ('monitorSessionName') via
-- @new-session -A@ so it exists for leksah's whole lifetime regardless of how
-- many real terminals are open; that session is filtered out of the Terminals
-- tree by 'IDE.Web.Widget.Terminal.parsePaneTree'.  A single client attached to
-- one session receives notifications for /every/ session on the server (other
-- sessions' window changes arrive as @%unlinked-window-*@), so one monitor
-- covers the whole tree.
--
-- Fan-out registry (not a Chan), same shape as 'IDE.Web.RemoteRefresh' /
-- 'IDE.Web.LocalRefresh': each Terminals widget registers the fire side of its
-- own reflex trigger.  No payload — the listener just re-reads the tree.
module IDE.Web.TerminalRefresh
  ( monitorSessionName
  , requestTerminalRefresh
  , registerTerminalRefresh
  , ensureTerminalMonitor
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M (delete, elems, empty, insert)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

#if !defined(ghcjs_HOST_OS)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import qualified Data.Text as T (unpack)
import System.Directory (findExecutable)

import IDE.Web.ReplTmux (tmuxSocket)
import IDE.Web.TmuxCC (TmuxEvent(..), ccEvents, startCC)
#endif

-- | Name of the dedicated hidden tmux session the monitor attaches to.  It is
-- created on leksah's private server (@-L leksah@) and filtered out of the
-- Terminals tree, so it never shows up as a terminal.
monitorSessionName :: Text
monitorSessionName = "__leksah_monitor"

{-# NOINLINE handlers #-}
handlers :: IORef (Int, Map Int (IO ()))
handlers = unsafePerformIO (newIORef (0, M.empty))

-- | Notify every registered listener that the tmux tree changed (usually a
-- reflex trigger fire — cheap and non-blocking).  Callable from any thread;
-- listener exceptions are swallowed so one dead widget can't break the rest.
requestTerminalRefresh :: IO ()
requestTerminalRefresh = do
    (_, m) <- readIORef handlers
    mapM_ (\h -> h `catch` \(_ :: SomeException) -> return ()) (M.elems m)

-- | Register a listener; returns the unregister action.
registerTerminalRefresh :: IO () -> IO (IO ())
registerTerminalRefresh h = do
    k <- atomicModifyIORef' handlers $ \(n, m) ->
            ((n + 1, M.insert n h m), n)
    return . void $ atomicModifyIORef' handlers $ \(n, m) ->
            ((n, M.delete k m), ())

#if defined(ghcjs_HOST_OS)
-- | No tmux in the browser demo; nothing to monitor.
ensureTerminalMonitor :: IO ()
ensureTerminalMonitor = return ()
#else
{-# NOINLINE monitorRunning #-}
monitorRunning :: MVar Bool
monitorRunning = unsafePerformIO (newMVar False)

-- | Idempotently start the single persistent control-mode monitor.  Safe to
-- call from every window's post-build and after any terminal action: at most
-- one monitor thread runs at a time, and it re-arms here if it has exited
-- (server killed, monitor session removed).  A no-op when tmux is absent, so it
-- degrades to the widget's own on-action re-reads rather than looping.
ensureTerminalMonitor :: IO ()
ensureTerminalMonitor = modifyMVar_ monitorRunning $ \running ->
    if running then return True
    else findExecutable "tmux" >>= \case
        Nothing -> return False
        Just _  -> do
            void . forkIO $ monitorLoop `catch` \(_ :: SomeException) -> markStopped
            return True

-- | Attach a control-mode client to the hidden monitor session and fire a
-- refresh on every structural notification.  @%output@ (pane data) is ignored —
-- the tree only changes on the other notifications.  On @%exit@ / EOF the
-- monitor stops and 'ensureTerminalMonitor' will restart it on the next call.
monitorLoop :: IO ()
monitorLoop = do
    cc <- startCC ["-L", tmuxSocket]
              [ "new-session", "-A", "-s", T.unpack monitorSessionName
              , "-x", "80", "-y", "24" ]
    let go = ccEvents cc >>= \case
                EvExit _           -> markStopped
                EvOutput{}         -> go
                EvExtendedOutput{} -> go
                _                  -> requestTerminalRefresh >> go
    go

markStopped :: IO ()
markStopped = modifyMVar_ monitorRunning (const (return False))
#endif
