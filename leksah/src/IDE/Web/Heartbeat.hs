-- SPDX-License-Identifier: Apache-2.0

-- | When a window's reflex frame thread was last seen alive.
--
-- The per-window heartbeat tick stamps this; the control socket reports its
-- age.  That distinguishes the two failure modes that look identical from
-- outside: a wedged UI still answers the socket (a separate thread) and still
-- evaluates JS, so \"the socket replied\" is not evidence that the event
-- network is running.  A stale stamp is.
--
-- A process-global 'IORef' rather than IDE state on purpose: the reader is the
-- command server, which must not touch the 'MVar' the frame thread might be
-- blocked on.
module IDE.Web.Heartbeat
  ( beat
  , lastBeatAge
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE lastBeat #-}
lastBeat :: IORef (Maybe UTCTime)
lastBeat = unsafePerformIO (newIORef Nothing)

-- | Record that a frame thread just ran.
beat :: IO ()
beat = writeIORef lastBeat . Just =<< getCurrentTime

-- | How long since any frame thread was last seen, or 'Nothing' if none has
-- reported yet (the UI is still building).
lastBeatAge :: IO (Maybe NominalDiffTime)
lastBeatAge = do
    mb <- readIORef lastBeat
    case mb of
        Nothing -> return Nothing
        Just t  -> Just . (`diffUTCTime` t) <$> getCurrentTime
