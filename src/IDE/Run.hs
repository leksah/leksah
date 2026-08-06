-- SPDX-License-Identifier: Apache-2.0

-- | Run an external tool and stream what it prints.  This is the only way
-- the IDE runs build tools: raw interleaved stdout+stderr chunks go to a
-- callback (the build log, a problems-parser fold), and the exit code
-- arrives in its own callback when the streams drain.
--
-- Design points, each one a lesson:
--
-- * 'startRun' returns spawn failure in the type ('Left') — an uncaught
--   @createProcess@ exception from a UI thread once killed a whole
--   window's event network.
-- * Chunk callbacks are serialized (stdout and stderr readers share a
--   lock), so consumers need no locking of their own; they run on reader
--   threads and must not block on the UI.
-- * The child gets its own process group, so 'interruptRun' stops
--   @cabal@'s whole tree of compilers, not just cabal.
module IDE.Run
  ( RunSpec(..)
  , runSpec
  , RunHandle
  , startRun
  , interruptRun
  , waitRun
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar,
        tryReadMVar, withMVar)
import Control.Exception (IOException, try)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Exit (ExitCode)
import System.IO (hClose)
import System.Process
       (CreateProcess(..), ProcessHandle, StdStream(..), createProcess,
        interruptProcessGroupOf, proc, terminateProcess, waitForProcess)

data RunSpec = RunSpec
    { rsProgram :: FilePath
    , rsArgs    :: [String]
    , rsDir     :: Maybe FilePath
    , rsEnv     :: Maybe [(String, String)]  -- ^ 'Nothing' inherits
    , rsOnChunk :: ByteString -> IO ()       -- ^ interleaved stdout+stderr
    , rsOnDone  :: ExitCode -> IO ()         -- ^ after the streams drain
    }

-- | A spec with no callbacks, inheriting directory and environment.
runSpec :: FilePath -> [String] -> RunSpec
runSpec prog args = RunSpec
    { rsProgram = prog
    , rsArgs    = args
    , rsDir     = Nothing
    , rsEnv     = Nothing
    , rsOnChunk = \_ -> return ()
    , rsOnDone  = \_ -> return ()
    }

data RunHandle = RunHandle
    { rhProcess :: ProcessHandle
    , rhExit    :: MVar ExitCode
    }

-- | Spawn the tool.  'Left' is the spawn failure (program missing,
-- directory gone) — no callback has run and none will.
startRun :: RunSpec -> IO (Either IOException RunHandle)
startRun spec = do
    spawned <- try $ createProcess (proc (rsProgram spec) (rsArgs spec))
        { cwd = rsDir spec
        , env = rsEnv spec
        , std_in = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe
        , create_group = True
        }
    case spawned of
        Left e -> return (Left e)
        Right (_, mbOut, mbErr, ph) -> do
            emit <- newMVar ()  -- serializes rsOnChunk across the two readers
            outDone <- drain emit mbOut
            errDone <- drain emit mbErr
            exitVar <- newEmptyMVar
            _ <- forkIO $ do
                takeMVar outDone
                takeMVar errDone
                code <- waitForProcess ph
                putMVar exitVar code
                rsOnDone spec code
            return (Right (RunHandle ph exitVar))
  where
    drain emit mbH = do
        done <- newEmptyMVar
        case mbH of
            Nothing -> putMVar done ()
            Just h -> void . forkIO $ do
                loop h emit `finallyPut` done
        return done
    loop h emit = do
        chunk <- BS.hGetSome h 8192
        if BS.null chunk
            then hClose h
            else do
                withMVar emit $ \() -> rsOnChunk spec chunk
                loop h emit
    finallyPut act done = do
        _ <- try act :: IO (Either IOException ())
        putMVar done ()

-- | Interrupt the run: SIGINT to the process group, escalating to
-- SIGTERM if it is still running three seconds later.  Callbacks still
-- fire (with whatever output and exit code the interruption produced).
interruptRun :: RunHandle -> IO ()
interruptRun rh = do
    running <- tryReadMVar (rhExit rh)
    case running of
        Just _ -> return ()
        Nothing -> do
            interruptProcessGroupOf (rhProcess rh)
            void . forkIO $ do
                threadDelay 3000000
                still <- tryReadMVar (rhExit rh)
                when (still == Nothing) $ terminateProcess (rhProcess rh)

-- | Block until the tool exits (after 'rsOnDone' scheduling).
waitRun :: RunHandle -> IO ExitCode
waitRun = readMVar . rhExit
