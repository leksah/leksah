-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Running external tools and streaming their output.  A fresh, batch-only
-- replacement for the part of leksah-server's @IDE.Utils.Tool@ the web UI
-- still uses: the interactive-ghci machinery is gone with the in-process
-- debugger, so all that is needed is \"run a process, stream its tagged
-- output as a conduit, finish with the exit code\".
module IDE.Utils.Process
  ( ToolOutput(..)
  , toolline
  , runTool
    -- * Re-exports so callers need no direct System.Process import
  , ProcessHandle
  , getProcessExitCode
  , interruptProcessGroupOf
  , terminateProcess
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
       (atomically, newTChanIO, readTChan, writeTChan, TChan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (NFData(..))
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit (ConduitT)
import qualified Data.Conduit as C
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import System.IO
       (BufferMode(..), Handle, hSetBuffering, hSetEncoding, utf8)
import System.Process
       (CreateProcess(..), StdStream(..), ProcessHandle, createProcess
       , create_group, getProcessExitCode, interruptProcessGroupOf, proc
       , terminateProcess, waitForProcess)

-- | One tagged line of a tool run (plus the final exit).  The vocabulary the
-- log parsers and the Log pane are written against.
data ToolOutput
    = ToolInput Text    -- ^ a line we sent (echoed into the log)
    | ToolError Text    -- ^ a line of stderr
    | ToolOutput Text   -- ^ a line of stdout
    | ToolPrompt Text   -- ^ an interactive prompt (unused by batch runs)
    | ToolExit ExitCode
    deriving (Eq, Show, Generic)

instance NFData ToolOutput where
    rnf (ToolInput  t) = rnf t
    rnf (ToolError  t) = rnf t
    rnf (ToolOutput t) = rnf t
    rnf (ToolPrompt t) = rnf t
    rnf (ToolExit   c) = c `seq` ()

-- | The text of the line, whichever stream it came from.
toolline :: ToolOutput -> Text
toolline (ToolInput  t)  = t
toolline (ToolError  t)  = t
toolline (ToolOutput t)  = t
toolline (ToolPrompt t)  = t
toolline (ToolExit _)    = ""

-- | Start a process (in its own process group, so interrupt can signal the
-- whole tree) and return a conduit of its interleaved output — stdout as
-- 'ToolOutput', stderr as 'ToolError', arrival order, ending with 'ToolExit'
-- — together with the 'ProcessHandle'.
runTool :: MonadIO m
        => FilePath -> [Text] -> Maybe FilePath -> Maybe [(String, String)]
        -> IO (ConduitT () ToolOutput m (), ProcessHandle)
runTool executable args mbDir mbEnv = do
    (_, Just out, Just err, pid) <- createProcess (proc executable (map T.unpack args))
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , cwd     = mbDir
        , env     = mbEnv
        , create_group = True
        }
    chan <- newTChanIO
    outDone <- newEmptyMVar
    errDone <- newEmptyMVar
    _ <- forkIO $ pump ToolOutput out chan >> putMVar outDone ()
    _ <- forkIO $ pump ToolError err chan >> putMVar errDone ()
    _ <- forkIO $ do
        takeMVar outDone
        takeMVar errDone
        code <- waitForProcess pid
        atomically $ writeTChan chan (Just (ToolExit code))
    return (source chan, pid)
  where
    pump :: (Text -> ToolOutput) -> Handle -> TChan (Maybe ToolOutput) -> IO ()
    pump tag h chan = do
        hSetBuffering h LineBuffering
        void (try (hSetEncoding h utf8) :: IO (Either SomeException ()))
        let loop = try (T.hGetLine h) >>= \case
                Right l -> atomically (writeTChan chan (Just (tag l))) >> loop
                Left (_ :: SomeException) -> return ()
        loop
    source :: MonadIO m => TChan (Maybe ToolOutput) -> ConduitT () ToolOutput m ()
    source chan = do
        next <- liftIO . atomically $ readTChan chan
        case next of
            Just o@(ToolExit _) -> C.yield o
            Just o              -> C.yield o >> source chan
            Nothing             -> return ()
