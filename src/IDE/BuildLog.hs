-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

-- | The build log is a byte stream, not a text model.  Tools write raw
-- output here (colour and all — run them with colour forced); each
-- window's log pane is a read-only xterm.js that replays the bounded
-- history on mount and then follows live.  There is no line model, no
-- parsing, no markup on the Haskell side — diagnostics come from a
-- separate tap of the same stream ("IDE.Problems.Parse").
--
-- The pane-side xterm should set @convertEol: true@ (tool output has bare
-- @\\n@) and @disableStdin: true@.
module IDE.BuildLog
  ( BuildLog
  , newBuildLog
  , blWrite
  , blNote
  , blClear
  , blAttach
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.IORef
       (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text.Encoding as T

-- | How much scrollback survives for late-attaching panes.  Dropped from
-- the front in whole chunks; a live xterm keeps its own (longer)
-- scrollback regardless.
historyCap :: Int
historyCap = 4 * 1024 * 1024

data BuildLog = BuildLog
    { blLock :: MVar ()                 -- ^ serializes write/clear/attach
    , blHist :: IORef (Seq ByteString, Int)
    , blSubs :: IORef (IntMap (ByteString -> IO ()))
    , blNext :: IORef Int
    }

newBuildLog :: IO BuildLog
newBuildLog = BuildLog
    <$> newMVar ()
    <*> newIORef (Seq.empty, 0)
    <*> newIORef IM.empty
    <*> newIORef 0

-- | Append raw tool output: recorded in history, pushed to every
-- attached pane.  Callable from any thread; subscribers must not block
-- (an xterm feed queues, it does not wait).
blWrite :: BuildLog -> ByteString -> IO ()
blWrite _ chunk | BS.null chunk = return ()
blWrite bl chunk = withMVar (blLock bl) $ \() -> do
    atomicModifyIORef' (blHist bl) $ \(h, n) ->
        (trim (h |> chunk, n + BS.length chunk), ())
    subs <- readIORef (blSubs bl)
    mapM_ ($ chunk) (IM.elems subs)
  where
    trim (h, n)
        | n <= historyCap = (h, n)
        | otherwise = case Seq.viewl h of
            Seq.EmptyL  -> (h, n)
            c Seq.:< h' -> trim (h', n - BS.length c)

-- | Write a line of the IDE's own commentary ("build started", a spawn
-- failure) — dim, bracketed, CRLF-terminated.
blNote :: BuildLog -> Text -> IO ()
blNote bl t = blWrite bl $
    "\ESC[2m[" <> T.encodeUtf8 t <> "]\ESC[0m\r\n"

-- | Drop the history and wipe every attached pane (a terminal clear
-- sequence does the wiping).
blClear :: BuildLog -> IO ()
blClear bl = withMVar (blLock bl) $ \() -> do
    writeIORef (blHist bl) (Seq.empty, 0)
    subs <- readIORef (blSubs bl)
    mapM_ ($ "\ESC[3J\ESC[2J\ESC[H") (IM.elems subs)

-- | Attach a pane: returns the history to replay first and the detach
-- action.  Atomic with writes — nothing is lost or duplicated between
-- the replay snapshot and the first live chunk.
blAttach :: BuildLog -> (ByteString -> IO ()) -> IO (ByteString, IO ())
blAttach bl k = withMVar (blLock bl) $ \() -> do
    i <- atomicModifyIORef' (blNext bl) (\i -> (i + 1, i))
    atomicModifyIORef' (blSubs bl) (\m -> (IM.insert i k m, ()))
    (h, _) <- readIORef (blHist bl)
    let detach = atomicModifyIORef' (blSubs bl) (\m -> (IM.delete i m, ()))
    return (BS.concat (toList h), detach)
