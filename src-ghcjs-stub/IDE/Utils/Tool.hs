{-# LANGUAGE OverloadedStrings #-}
-- | Browser stub of leksah-server's IDE.Utils.Tool (GHC JavaScript backend
-- only; picked over @vendor/leksah-server/src@ by hs-source-dirs order).
--
-- The real module drives external tools and persistent ghci sessions through
-- conduits over pipes; its extra deps (conduit-extra → network) don't build
-- for the JS target, and there are no processes in a browser anyway.  This
-- carries the same types and signatures so the build/debug modules compile
-- unchanged; actually launching a tool fails with a plain IOError, which the
-- web demo surfaces instead of tool output.
module IDE.Utils.Tool (
    ToolOutput(..),
    toolline,
    isToolPrompt,
    ToolCommand(..),
    ToolState(..),
    toolProcess,
    newToolState,
    runTool,
    runTool',
    runInteractiveTool,
    newGhci,
    newGhci',
    executeCommand,
    executeGhciCommand,
    interruptTool,
    quoteArg,
    escapeQuotes,
    runCommand,
    waitForProcess,
    interruptProcessGroupOf,
    ProcessHandle,
    getProcessExitCode,
    runInteractiveProcess,
    runProcess,
    readProcessWithExitCode,
    terminateProcess
) where

import Control.Concurrent
       (MVar, Chan, newChan, newEmptyMVar, readMVar, tryPutMVar)
import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Conduit (ConduitT)
import Data.Map (Map)
import Data.Text (Text)
import Data.Void (Void)
import System.Exit (ExitCode)
import System.Process
       (ProcessHandle, waitForProcess, interruptProcessGroupOf,
        getProcessExitCode, runInteractiveProcess, runProcess,
        readProcessWithExitCode, terminateProcess, runCommand)

data ToolOutput = ToolInput Text
                | ToolError Text
                | ToolOutput Text
                | ToolPrompt Text
                | ToolExit ExitCode deriving(Eq, Show)

instance NFData ToolOutput where
    rnf (ToolInput t)  = rnf t
    rnf (ToolError t)  = rnf t
    rnf (ToolOutput t) = rnf t
    rnf (ToolPrompt t) = rnf t
    rnf (ToolExit c)   = rnf c

toolline :: ToolOutput -> Text
toolline (ToolInput l)  = l
toolline (ToolOutput l) = l
toolline (ToolError l)  = l
toolline (ToolPrompt l) = l
toolline (ToolExit _)   = ""

isToolPrompt :: ToolOutput -> Bool
isToolPrompt (ToolPrompt _) = True
isToolPrompt _              = False

data ToolCommand = ToolCommand Text Text (ConduitT ToolOutput Void IO ())
data ToolState = ToolState {
    toolProcessMVar :: MVar ProcessHandle,
    outputClosed :: MVar Bool,
    toolCommands :: Chan ToolCommand,
    toolCommandsRead :: Chan ToolCommand,
    currentToolCommand :: MVar Text,
    interruptToolMVar :: MVar ()}

toolProcess :: ToolState -> IO ProcessHandle
toolProcess = readMVar . toolProcessMVar

newToolState :: IO ToolState
newToolState = do
    toolProcessMVar' <- newEmptyMVar
    outputClosed' <- newEmptyMVar
    toolCommands' <- newChan
    currentToolCommand' <- newEmptyMVar
    interruptToolMVar' <- newEmptyMVar
    return (ToolState toolProcessMVar' outputClosed' toolCommands'
                      toolCommands' currentToolCommand' interruptToolMVar')

noTool :: String -> IO a
noTool op = ioError (userError ("IDE.Utils.Tool." <> op <> ": no processes in the browser"))

runTool' :: FilePath -> [Text] -> Maybe FilePath -> Maybe [(String,String)]
         -> IO ([ToolOutput], ProcessHandle)
runTool' _ _ _ _ = noTool "runTool'"

runTool :: MonadIO m => FilePath -> [Text] -> Maybe FilePath -> Maybe [(String,String)]
        -> IO (ConduitT () ToolOutput m (), ProcessHandle)
runTool _ _ _ _ = noTool "runTool"

runInteractiveTool :: ToolState -> a -> FilePath -> [Text] -> Maybe FilePath
                   -> Maybe [(String,String)] -> IO ()
runInteractiveTool _ _ _ _ _ _ = noTool "runInteractiveTool"

newGhci' :: [Text] -> ConduitT ToolOutput Void IO () -> ConduitT ToolOutput Void IO ()
         -> IO ToolState
newGhci' _ _ _ = noTool "newGhci'"

newGhci :: FilePath -> [Text] -> FilePath -> Maybe (Map String String) -> [Text]
        -> ConduitT ToolOutput Void IO () -> ConduitT ToolOutput Void IO ()
        -> IO ToolState
newGhci _ _ _ _ _ _ _ = noTool "newGhci"

executeCommand :: ToolState -> Text -> Text -> ConduitT ToolOutput Void IO () -> IO ()
executeCommand _ _ _ _ = noTool "executeCommand"

executeGhciCommand :: ToolState -> Text -> ConduitT ToolOutput Void IO () -> IO ()
executeGhciCommand _ _ _ = noTool "executeGhciCommand"

interruptTool :: ToolState -> IO ()
interruptTool ts = void' (tryPutMVar (interruptToolMVar ts) ())
  where void' a = a >> return ()

quoteArg :: String -> String
quoteArg s | ' ' `elem` s = "\"" <> escapeQuotes s <> "\""
quoteArg s                = s

escapeQuotes :: String -> String
escapeQuotes = concatMap (\c -> if c == '"' then "\\\"" else [c])
