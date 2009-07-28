{-# OPTIONS_GHC -XRecordWildCards #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Tool
-- Copyright   :  (c) Hamish Mackenzie, Juergen Nicklisch-Franken
-- License     :  GPL
--
-- Maintainer  :  <maintainer@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- | Support for running external tools.  Written mainly for GHCi but with
-- | support for others in mind.
--
-----------------------------------------------------------------------------

module IDE.Tool (
    ToolOutput(..),
    toolline,
    ToolCommand(..),
    ToolState(..),
    newToolState,
    runTool,
    runInteractiveTool,
    newGhci,
    executeCommand,
    executeGhciCommand)
where

import System.IO
import Control.Concurrent
    (takeMVar,
     putMVar,
     newEmptyMVar,
     forkIO,
     newChan,
     MVar(..),
     Chan(..),
     writeChan,
     getChanContents,
     dupChan)
import Control.Monad (when)
import Data.List (stripPrefix)
import Data.Maybe (isJust, catMaybes)
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import System.Posix (sigQUIT, sigINT, installHandler)
import System.Posix.Signals (Handler(..))
#endif
import System.Process
    (waitForProcess, ProcessHandle(..), runInteractiveProcess)

data ToolOutput = ToolInput String | ToolError String | ToolOutput String deriving(Eq, Show)
data ToolCommand = ToolCommand String ([ToolOutput] -> IO ())
data ToolState = ToolState {
    toolProcess :: MVar ProcessHandle,
    outputClosed :: MVar Bool,
    toolCommands :: Chan ToolCommand,
    toolCommandsRead :: Chan ToolCommand,
    currentToolCommand :: MVar ToolCommand}

data RawToolOutput = RawToolOutput ToolOutput
                   | ToolPrompt
                   | ToolOutClosed
                   | ToolErrClosed
                   | ToolClosed deriving(Eq, Show)

toolline :: ToolOutput -> String
toolline (ToolInput l) = l
toolline (ToolOutput l) = l
toolline (ToolError l) = l

runTool :: FilePath -> [String] -> IO ([ToolOutput], ProcessHandle)
runTool executable arguments = do
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    installHandler sigINT Ignore Nothing
    installHandler sigQUIT Ignore Nothing
#endif

    (inp,out,err,pid) <- runInteractiveProcess executable arguments Nothing Nothing
    output <- getOutputNoPrompt inp out err pid
    return (output, pid)

newToolState :: IO ToolState
newToolState = do
    toolProcess <- newEmptyMVar
    outputClosed <- newEmptyMVar
    toolCommands <- newChan
    toolCommandsRead <- dupChan toolCommands
    currentToolCommand <- newEmptyMVar
    return ToolState{..}

runInteractiveTool :: ToolState -> (Handle -> Handle -> Handle -> ProcessHandle -> IO [RawToolOutput]) -> FilePath -> [String] -> IO ()
runInteractiveTool tool getOutput executable arguments = do
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    installHandler sigINT Ignore Nothing
    installHandler sigQUIT Ignore Nothing
#endif

    (inp,out,err,pid) <- runInteractiveProcess executable arguments Nothing Nothing
    putMVar (toolProcess tool) pid
    output <- getOutput inp out err pid
    -- This is handy to show the processed output
    -- forkIO $ forM_ output (putStrLn.show)
    forkIO $ do
        commands <- getChanContents (toolCommandsRead tool)
        processCommand commands inp output
    return ()
    where
        processCommand [] _ _ = return ()
        processCommand ((command@(ToolCommand commandString handler)):remainingCommands) inp allOutput = do
            putMVar (currentToolCommand tool) command
            hPutStrLn inp commandString
            hFlush inp
            let (output, remainingOutputWithPrompt) = span (/= ToolPrompt) allOutput
            handler $ (map ToolInput (lines commandString)) ++ fromRawOutput output
            takeMVar (currentToolCommand tool)
            case remainingOutputWithPrompt of
                (ToolPrompt:remainingOutput) -> do
                    processCommand remainingCommands inp remainingOutput
                [] -> do
                    putMVar (outputClosed tool) True
                _ -> do
                    putStrLn "This should never happen in Tool.hs"

newInteractiveTool :: (Handle -> Handle -> Handle -> ProcessHandle -> IO [RawToolOutput]) -> FilePath -> [String] -> IO ToolState
newInteractiveTool getOutput executable arguments = do
    tool <- newToolState
    runInteractiveTool tool getOutput executable arguments
    return tool

ghciPrompt :: String
ghciPrompt = "3KM2KWR7LZZbHdXfHUOA5YBBsJVYoCQnKX"

data CommanLineReader = CommanLineReader {
    stripInitialPrompt :: String -> Maybe String,
    stripFollowingPrompt :: String -> Maybe String,
    errorSyncCommand :: Maybe String,
    stripExpectedError :: String -> Maybe String
    }

ghciStripInitialPrompt :: String -> Maybe String
ghciStripInitialPrompt output =
    case catMaybes [stripPrefix "Prelude" output, stripPrefix "*" output] of
        remaining:_ ->
            case dropWhile (/= '>') remaining of
                '>':' ':next -> Just next
                _ -> Nothing
        _ -> Nothing

ghciStripFollowingPrompt :: String -> Maybe String
ghciStripFollowingPrompt = stripPrefix ghciPrompt

ghciStripExpectedError :: String -> Maybe String
ghciStripExpectedError output = case stripPrefix "\n<interactive>:1:0" output of
                                    Just rest ->
                                        stripPrefix ": Not in scope: `kM2KWR7LZZbHdXfHUOA5YBBsJVYoC'\n"
                                            (maybe rest id (stripPrefix "-28" rest))
                                    Nothing -> Nothing

ghciCommandLineReader    = CommanLineReader {
    stripInitialPrompt   = ghciStripInitialPrompt,
    stripFollowingPrompt = ghciStripFollowingPrompt,
    errorSyncCommand     = Just "kM2KWR7LZZbHdXfHUOA5YBBsJVYoC",
    stripExpectedError   = ghciStripExpectedError
    }

noInputCommandLineReader = CommanLineReader {
    stripInitialPrompt = const Nothing,
    stripFollowingPrompt = const Nothing,
    errorSyncCommand = Nothing,
    stripExpectedError = const Nothing
    }
--waitTillEmpty :: Handle -> IO ()
--waitTillEmpty handle = do
--    ready <- hReady handle
--    when ready $ do
--        yield
--        threadDelay 100
--        yield
--        waitTillEmpty handle

getOutput :: CommanLineReader -> Handle -> Handle -> Handle -> ProcessHandle -> IO [RawToolOutput]
getOutput clr inp out err pid = do
    chan <- newChan
    -- hSetBuffering out NoBuffering
    -- hSetBuffering err NoBuffering
    foundExpectedError <- newEmptyMVar
    -- Use this and the too putStr threads bellow if you want to see the raw output
    -- hSetBuffering stdout NoBuffering
    forkIO $ do
        errors <- hGetContents err
        -- forkIO $ putStr errors
        readError chan (filter (/= '\r') errors) foundExpectedError
        writeChan chan ToolErrClosed
    forkIO $ do
        output <- hGetContents out
        -- forkIO $ putStr output
        readOutput chan (filter (/= '\r') output) True foundExpectedError False
        writeChan chan ToolOutClosed
    forkIO $ do
        testClosed <- dupChan chan
        output <- getChanContents testClosed
        when ((ToolOutClosed `elem` output) && (ToolErrClosed `elem` output)) $ do
            waitForProcess pid
            writeChan chan ToolClosed
    fmap (takeWhile ((/=) ToolClosed)) $ getChanContents chan
    where
        readError chan errors foundExpectedError = do
            case stripExpectedError clr errors of
                Just unexpectedErrors -> do
                    putMVar foundExpectedError True
                    readError chan unexpectedErrors foundExpectedError
                Nothing -> do
                    let (line, remaining) = break (== '\n') errors
                    case remaining of
                        [] -> return ()
                        _:remainingLines -> do
                            writeChan chan $ RawToolOutput $ ToolError line
                            readError chan remainingLines foundExpectedError

        readOutput chan output initial foundExpectedError synced = do
            let stripPrompt = (if initial then (stripInitialPrompt clr) else (stripFollowingPrompt clr))
            let line = getLine stripPrompt output
            let remaining = drop (length line) output
            case remaining of
                [] -> do
                        when (line /= "") $ writeChan chan $ RawToolOutput $ ToolOutput line
                '\n':remainingLines -> do
                        writeChan chan $ RawToolOutput $ ToolOutput line
                        readOutput chan remainingLines initial foundExpectedError synced
                _ -> do
                    when (line /= "") $ writeChan chan $ RawToolOutput $ ToolOutput line
                    case stripPrompt remaining of
                        Just afterPrompt -> do
                            case (initial, synced, errorSyncCommand clr) of
                                (True, _, _) -> do
                                    readOutput chan afterPrompt False foundExpectedError synced
                                (False, _, Nothing) -> do
                                    writeChan chan ToolPrompt
                                    readOutput chan afterPrompt False foundExpectedError synced
                                (False, False, Just syncCmd) -> do
                                    hPutStrLn inp syncCmd
                                    hFlush inp
                                    takeMVar foundExpectedError
                                    readOutput chan afterPrompt False foundExpectedError True
                                (False, True, Just _) -> do
                                    writeChan chan ToolPrompt
                                    readOutput chan afterPrompt False foundExpectedError False
                        _ -> return () -- Should never happen
        getLine _ [] = []
        getLine _ ('\n':xs) = []
        getLine stripPrompt output@(x:xs)
            | isJust (stripPrompt output) = []
            | otherwise                   = x : (getLine stripPrompt xs)

fromRawOutput :: [RawToolOutput] -> [ToolOutput]
fromRawOutput [] = []
fromRawOutput (RawToolOutput output:xs) = output : (fromRawOutput xs)
fromRawOutput (ToolClosed:xs) = []
fromRawOutput (_:xs) = fromRawOutput xs

getGhciOutput :: Handle -> Handle -> Handle -> ProcessHandle -> IO [RawToolOutput]
getGhciOutput = getOutput ghciCommandLineReader

getOutputNoPrompt :: Handle -> Handle -> Handle -> ProcessHandle -> IO [ToolOutput]
getOutputNoPrompt inp out err pid = fmap fromRawOutput $ getOutput noInputCommandLineReader inp out err pid

newGhci :: [String] -> [String] -> ([ToolOutput] -> IO ()) -> IO ToolState
newGhci buildFlags interactiveFlags startupOutputHandler = do
        tool <- newToolState
        writeChan (toolCommands tool) $
            ToolCommand (":set prompt " ++ ghciPrompt) startupOutputHandler
        putStrLn "Working out GHCi options"
        forkIO $ do
            (output, pid) <- runTool "runhaskell" (["Setup","build","--with-ghc=leksahecho"] ++ buildFlags)
            case catMaybes $ map (findMake . toolline) output of
                options:_ -> do
                        let newOptions = filterUnwanted options
                        putStrLn newOptions
                        putStrLn "Starting GHCi"
                        putStrLn $ unwords (words newOptions ++ ["-fforce-recomp"] ++ interactiveFlags)
                        runInteractiveTool tool getGhciOutput "ghci"
                            (words newOptions ++ ["-fforce-recomp"] ++ interactiveFlags)
                _ -> do
                    startupOutputHandler output
                    putMVar (outputClosed tool) True
        return tool
    where
        findMake [] = Nothing
        findMake line@(x:xs) =
                case stripPrefix " --make " line of
                    Nothing -> findMake xs
                    s -> s
        filterUnwanted [] = []
        filterUnwanted line@(x:xs) =
                case stripPrefix "-O " line of
                    Nothing -> x: filterUnwanted xs
                    Just s  -> filterUnwanted s


executeCommand :: ToolState -> String -> ([ToolOutput] -> IO ()) -> IO ()
executeCommand tool command handler = do
    writeChan (toolCommands tool) $ ToolCommand command handler

executeGhciCommand :: ToolState -> String -> ([ToolOutput] -> IO ()) -> IO ()
executeGhciCommand tool command handler = do
    if '\n' `elem` command
        then executeCommand tool safeCommand $ \output -> do
            handler $ fixInput $ fixOutput output
        else executeCommand tool command handler
    where
        filteredLines = (filter safeLine (lines command))
        promptCount = (length filteredLines)+1
        safeCommand = (unlines ([":{"] ++ filteredLines)) ++ ":}"
        safeLine ":{" = False
        safeLine ":}" = False
        safeLine _ = True
        fixOutput ((ToolOutput line):xs) = (ToolOutput (removePrompts line line promptCount)):xs
        fixOutput (x:xs) = x:(fixOutput xs)
        fixOutput [] = []
        fixInput = filter (\x -> (x /= ToolInput ":{") && (x /= ToolInput ":}"))
        removePrompts fullLine line 0 = line
        removePrompts fullLine line n = case dropWhile ((/=) '|') line of
            '|':' ':xs -> removePrompts fullLine xs (n-1)
            _ -> fullLine


