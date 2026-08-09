{-# LANGUAGE OverloadedStrings #-}
-- | A process-global bridge for the jsaddle-terminal SYNC round trip.
--
-- A synchronous JS→Haskell callback inside a tunnel iframe is delivered as a
-- __blocking__ XHR to leksah's own local web server
-- (@POST /jsaddle-terminal/sync/<key>@).  The Warp handler must answer with
-- the next jsaddle @Batch@, which only the pane's exe can produce — so the
-- request is round-tripped over the pane's stdin/stdout: SYNC frame in,
-- SYNCR frame out.  Both ends of that trip live OUTSIDE the browser: the
-- Warp thread here, and the control-client drain thread in
-- "IDE.Web.Widget.TerminalCC".  That is essential — the blocking XHR freezes
-- the page's whole JS event loop, so nothing on this path may wait on
-- leksah's own jsaddle.
--
-- Tunnels register a sender per pane key ('registerTunnelSync', called when
-- a pane's HELLO arrives); the Warp route calls 'tunnelSyncRequest'; the
-- drain thread completes it with 'tunnelSyncReply' when the SYNCR frame
-- comes back.  One request may be outstanding per pane at a time — the
-- blocking XHR serialises them by construction.
module IDE.Web.JsaddleTunnel
  ( registerTunnelSync
  , unregisterTunnelSync
  , tunnelSyncRequest
  , tunnelSyncReply
  ) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.ByteString (ByteString)
import Data.IORef
       (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

data SyncSlot = SyncSlot
  { ssSend    :: ByteString -> IO ()             -- ^ SYNC frame → pane stdin
  , ssPending :: IORef (Maybe (MVar ByteString)) -- ^ the waiting Warp thread
  }

{-# NOINLINE tunnelRegistry #-}
tunnelRegistry :: IORef (M.Map Text SyncSlot)
tunnelRegistry = unsafePerformIO (newIORef M.empty)

-- | Register pane @key@'s SYNC sender (its tunnel came up).
registerTunnelSync :: Text -> (ByteString -> IO ()) -> IO ()
registerTunnelSync key send = do
  pending <- newIORef Nothing
  atomicModifyIORef' tunnelRegistry $ \m ->
    (M.insert key (SyncSlot send pending) m, ())

-- | Forget pane @key@'s tunnel (BYE / pane gone / tab closed).
unregisterTunnelSync :: Text -> IO ()
unregisterTunnelSync key =
  atomicModifyIORef' tunnelRegistry $ \m -> (M.delete key m, ())

-- | The Warp route: forward the sync 'Results' JSON to the pane's exe and
-- wait (≤30s) for the answering 'Batch' JSON.  'Nothing' = no such tunnel
-- or timeout.
tunnelSyncRequest :: Text -> ByteString -> IO (Maybe ByteString)
tunnelSyncRequest key resultsJson = do
  reg <- readIORef tunnelRegistry
  case M.lookup key reg of
    Nothing -> return Nothing
    Just slot -> do
      mv <- newEmptyMVar
      atomicModifyIORef' (ssPending slot) $ \_ -> (Just mv, ())
      ssSend slot resultsJson
      timeout (30 * 1000000) (takeMVar mv)

-- | The drain thread: a SYNCR frame arrived for pane @key@ — hand its Batch
-- to the waiting Warp thread (no-op if nothing waits, e.g. it timed out).
tunnelSyncReply :: Text -> ByteString -> IO ()
tunnelSyncReply key batchJson = do
  reg <- readIORef tunnelRegistry
  case M.lookup key reg of
    Nothing -> return ()
    Just slot -> do
      mmv <- atomicModifyIORef' (ssPending slot) $ \p -> (Nothing, p)
      mapM_ (`putMVar` batchJson) mmv
