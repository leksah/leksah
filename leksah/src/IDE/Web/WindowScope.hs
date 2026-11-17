{-# LANGUAGE LambdaCase #-}
-- SPDX-License-Identifier: Apache-2.0

-- | The lifetime of one OS window's reflex network.
--
-- Closing a window used to remove only the NSWindow: its WKWebView (kept
-- alive on purpose, see @leksah_new_window@), its jsaddle context, its frame
-- thread and everything the network subscribed to ran on until the process
-- exited.  Twelve windows' worth of polling, heartbeats and cell fan-out in a
-- session that showed one window was measured at ~70% idle CPU.
--
-- A 'WindowScope' collects what a window's network must undo when the window
-- closes, and 'closeWindowScope' runs it.  Subscriptions register themselves
-- through the CURRENT scope ('onCurrentScopeClose'), which "IDE.Web.Attach"
-- sets around the window's build and around each of its frames.  That is
-- sound because builds and frames of every window run under the one process
-- wide frame lock, so exactly one window's network is ever running code that
-- can subscribe.
--
-- Closing never kills the frame thread: every window shares ONE Spider
-- timeline, and an asynchronous exception mid-frame could leave that shared
-- state inconsistent for the windows that remain.  Instead the frame loop
-- checks 'scopeIsClosed' between batches, and a close wakes it (see
-- 'IDE.Web.Attach').
module IDE.Web.WindowScope
  ( WindowScope
  , newWindowScope
  , scopeIsClosed
  , onScopeClose
  , withCurrentScope
  , onCurrentScopeClose
  , forkInCurrentScope
  , registerWindowScope
  , closeWindowScope
  , markFrameLoopDone
    -- * A browser connection going away (the warp front end)
  , ConnectionGone(..)
  , registerContextWindow
  , registerContextThread
  , connectionClosed
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Exception (Exception, SomeException, finally, throwTo, try)
import Control.Monad (forM, void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

-- | The cleanups ('Nothing' once closed; otherwise newest first — one ref, so
-- registering and closing are each a single atomic step and a cleanup runs
-- exactly once however the two race), and the frame loop's exit signal.
data WindowScope = WindowScope (IORef (Maybe [IO ()])) (MVar ())

newWindowScope :: IO WindowScope
newWindowScope = WindowScope <$> newIORef (Just []) <*> newEmptyMVar

scopeIsClosed :: WindowScope -> IO Bool
scopeIsClosed (WindowScope r _) = maybe True (const False) <$> readIORef r

-- | Called by the frame loop as it exits ("IDE.Web.Attach").
markFrameLoopDone :: WindowScope -> IO ()
markFrameLoopDone (WindowScope _ done) = void (tryPutMVar done ())

-- | Run @act@ when the scope closes — at once if it already has.
onScopeClose :: WindowScope -> IO () -> IO ()
onScopeClose (WindowScope r _) act = do
  closed <- atomicModifyIORef' r $ \case
    Nothing -> (Nothing, True)
    Just cs -> (Just (act : cs), False)
  if closed then runCleanup act else pure ()

{-# NOINLINE currentScopeRef #-}
currentScopeRef :: IORef (Maybe WindowScope)
currentScopeRef = unsafePerformIO (newIORef Nothing)

-- | Make @s@ the current scope while @act@ runs (a build or a frame, under
-- the frame lock).
withCurrentScope :: WindowScope -> IO a -> IO a
withCurrentScope s act = do
  prev <- readIORef currentScopeRef
  writeIORef currentScopeRef (Just s)
  act `finally` writeIORef currentScopeRef prev

-- | Register a cleanup with whichever window's network is running.  Outside
-- any window (process-level code, the warp front end before attach) there is
-- nothing to tie it to, and it is dropped — exactly the old behaviour.
onCurrentScopeClose :: IO () -> IO ()
onCurrentScopeClose act =
  readIORef currentScopeRef >>= mapM_ (`onScopeClose` act)

-- | 'forkIO' for a window-lived loop: the thread is killed when the window
-- closes.  (Only for threads OFF the frame thread — they never run a frame,
-- so killing them cannot interrupt one.)
forkInCurrentScope :: IO () -> IO ThreadId
forkInCurrentScope body = do
  tid <- forkIO body
  onCurrentScopeClose (killThread tid)
  pure tid

{-# NOINLINE scopesRef #-}
scopesRef :: IORef (M.Map Int WindowScope)
scopesRef = unsafePerformIO (newIORef M.empty)

-- | Record window @wid@'s scope so the native close path can find it.
registerWindowScope :: Int -> WindowScope -> IO ()
registerWindowScope wid s = atomicModifyIORef' scopesRef $ \m -> (M.insert wid s m, ())

-- | Close window @wid@'s scope: mark it closed, then run its cleanups (oldest
-- first, each on its own guard so one failure can't stop the rest — one of
-- them wakes the frame loop), then wait (bounded, ~5s) for the frame loop to
-- finish the frame it may be in and exit.  Answers whether it did: only after
-- that is it safe to cut the window's jsaddle context, since a frame still
-- running would see its JS calls fail and report a spurious aborted frame.
-- 'Nothing' for an unknown (or already closed) window.
closeWindowScope :: Int -> IO (Maybe Bool)
closeWindowScope wid = do
  ms <- atomicModifyIORef' scopesRef $ \m -> (M.delete wid m, M.lookup wid m)
  forM ms $ \(WindowScope r done) -> do
    cs <- atomicModifyIORef' r $ \m -> (Nothing, maybe [] id m)
    mapM_ runCleanup (reverse cs)
    maybe False (const True) <$> timeout 5000000 (readMVar done)

runCleanup :: IO () -> IO ()
runCleanup act = void (try act :: IO (Either SomeException ()))

--------------------------------------------------------------------------------
-- A browser connection going away
--
-- In the warp front end a window IS a browser connection (a jsaddle context),
-- and a reload or a closed tab ends it with no window-close event.  A frame
-- waiting for a JavaScript reply then waits for ever — under the frame lock
-- every window shares, so no window, the reloaded one included, can open
-- again.  So each connection's window and frame thread are recorded here
-- under its jsaddle context id, and when the connection goes
-- ('IDE.Web.WarpJSaddle'), 'connectionClosed' closes the window's scope (its
-- frame loop stops between frames) and — only if that does not finish within
-- the scope's bound, i.e. a frame is stuck waiting on the dead browser —
-- raises 'ConnectionGone' in the frame thread: in a JavaScript wait it is
-- caught where a failing JavaScript call would be, the frame is abandoned and
-- the frame lock released.

-- | The window's browser connection has gone.
data ConnectionGone = ConnectionGone deriving Show
instance Exception ConnectionGone

{-# NOINLINE contextsRef #-}
contextsRef :: IORef (M.Map Int64 (Maybe Int, [ThreadId]))
contextsRef = unsafePerformIO (newIORef M.empty)

-- | Window @wid@ runs in jsaddle context @ctx@.
registerContextWindow :: Int64 -> Int -> IO ()
registerContextWindow ctx wid = atomicModifyIORef' contextsRef $ \m ->
  (M.insertWith (\_ (_, ts) -> (Just wid, ts)) ctx (Just wid, []) m, ())

-- | Thread @t@ (a window's frame loop) belongs to jsaddle context @ctx@.
registerContextThread :: Int64 -> ThreadId -> IO ()
registerContextThread ctx t = atomicModifyIORef' contextsRef $ \m ->
  (M.insertWith (\_ (w, ts) -> (w, t : ts)) ctx (Nothing, [t]) m, ())

-- | Context @ctx@'s browser connection has gone: see above.
connectionClosed :: Int64 -> IO ()
connectionClosed ctx = do
  (mwid, ts) <- atomicModifyIORef' contextsRef $ \m ->
    (M.delete ctx m, M.findWithDefault (Nothing, []) ctx m)
  stopped <- maybe (return Nothing) closeWindowScope mwid
  case stopped of
    Just True -> return ()
    _ -> mapM_ (`throwTo` ConnectionGone) ts
