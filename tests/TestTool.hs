{-# LANGUAGE CPP, OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | Windows systems do not often have a real echo executable (so --with-ghc=echo fails)
--
-----------------------------------------------------------------------------

module Main (
main
) where

import System.Environment (getArgs)
import System.Exit (exitWith, exitSuccess, exitFailure, ExitCode(..))
import System.FilePath ((</>))
import Data.Monoid ((<>))
import IDE.Utils.Tool
       (toolProcess, executeGhciCommand, ToolOutput(..), runTool',
        newGhci')
import System.Process (interruptProcessGroupOf, getProcessExitCode)
import Test.HUnit
       ((@=?), (@?=), putTextToHandle, Counts(..), runTestTT, assertBool,
        runTestText, (~:), Testable(..), Test(..))
import Test.DocTest (doctest)
import System.IO (hPutStr, stdout, hPutStrLn, stderr, hFlush)
import qualified Data.Conduit.List as EL (consume)
import Control.Concurrent
       (threadDelay, forkIO, takeMVar, putMVar, newEmptyMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import System.Log.Logger
       (setLevel, rootLoggerName, updateGlobalLogger)
import System.Log (Priority(..))
import IDE.Utils.CabalProject (findProjectRoot)
import System.Directory (getCurrentDirectory)
import qualified Data.Text as T (pack)

-- stderr and stdout may not be in sync
check output expected = do
    checkFiltered notOut
    checkFiltered notErr
  where
    checkFiltered f = filter f output @?= filter f expected
    notErr (ToolError _) = False
    notErr _ = True
    notOut (ToolOutput _) = False
    notOut _ = True

runTests testMVar = loop
  where
    loop = do
        mbTest <- takeMVar testMVar
        case mbTest of
            Just test -> do
                test
                loop
            Nothing   -> return ()

sendTest testMVar test =
    liftIO $ putMVar testMVar $ Just test

doneTesting testMVar =
    liftIO $ putMVar testMVar Nothing

tests testTool =
    let runSelf' args = runTool' testTool args Nothing Nothing
    in test [
    "Exit Success" ~: do
        (output, _) <- runSelf' ["ExitSuccess"]
        output `check` [ToolInput $ T.pack testTool <> " ExitSuccess", ToolExit ExitSuccess],
    "Exit Failure" ~: do
        (output, _) <- runSelf' ["Exit42"]
        output `check` [ToolInput $ T.pack testTool <> " Exit42", ToolExit (ExitFailure 42)],
    "Single Blank Out Line" ~: do
        (output, _) <- runSelf' ["BlankLine", "StdOut"]
        output `check` [ToolInput $ T.pack testTool <> " BlankLine StdOut", ToolOutput "", ToolExit ExitSuccess],
    "Single Blank Err Line" ~: do
        (output, _) <- runSelf' ["BlankLine", "StdErr"]
        output `check` [ToolInput $ T.pack testTool <> " BlankLine StdErr", ToolError "", ToolExit ExitSuccess],
    "Hello Out" ~: do
        (output, _) <- runSelf' ["Hello", "StdOut"]
        output `check` [ToolInput $ T.pack testTool <> " Hello StdOut", ToolOutput "Hello World", ToolExit ExitSuccess],
    "Hello Err" ~: do
        (output, _) <- runSelf' ["Hello", "StdErr"]
        output `check` [ToolInput $ T.pack testTool <> " Hello StdErr", ToolError "Hello World", ToolExit ExitSuccess],
    "Both" ~: do
        (output, _) <- runSelf' ["ErrAndOut"]
        output `check` [ToolInput $ T.pack testTool <> " ErrAndOut", ToolError "Error", ToolOutput "Output", ToolExit ExitSuccess],
    "Unterminated Out" ~: do
        (output, _) <- runSelf' ["Unterminated", "StdOut"]
        output `check` [ToolInput $ T.pack testTool <> " Unterminated StdOut", ToolOutput "Unterminated", ToolExit ExitSuccess],
    "Unterminated Err" ~: do
        (output, _) <- runSelf' ["Unterminated", "StdErr"]
        output `check` [ToolInput $ T.pack testTool <> " Unterminated StdErr", ToolError "Unterminated", ToolExit ExitSuccess],
    "GHCi Failed Sart" ~: do
        t <- newEmptyMVar
        tool <- newGhci' ["MissingFile.hs"] $ do
            output <- EL.consume
            sendTest t $ last output @?= (ToolPrompt "")
        executeGhciCommand tool ":quit" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput ":quit",
                ToolOutput "Leaving GHCi.",
                ToolExit ExitSuccess],
    "GHCi" ~: do
        t <- newEmptyMVar
        tool <- newGhci' [] $ do
            output <- EL.consume
            sendTest t $ last output @?= (ToolPrompt "")
        executeGhciCommand tool ":m +System.IO" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput ":m +System.IO",
                ToolPrompt ""]
        executeGhciCommand tool "hPutStr stderr \"Test\"" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "hPutStr stderr \"Test\"",
                ToolError "Test",
                ToolPrompt ""]
        executeGhciCommand tool "1+1" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "1+1",
                ToolOutput "2",
                ToolPrompt ""]
        executeGhciCommand tool "jfkdfjdkl" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "jfkdfjdkl",
                ToolError "",
#if __GLASGOW_HASKELL__ >= 800
                ToolError "<interactive>:22:1: error: Variable not in scope: jfkdfjdkl",
#elif __GLASGOW_HASKELL__ > 706
                ToolError "<interactive>:23:1: Not in scope: ‘jfkdfjdkl’",
#elif __GLASGOW_HASKELL__ > 702
                ToolError "<interactive>:23:1: Not in scope: `jfkdfjdkl'",
#else
                ToolError "<interactive>:1:1: Not in scope: `jfkdfjdkl'",
#endif
                ToolPrompt ""]
        executeGhciCommand tool "\n1+1" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "",
                ToolInput "1+1",
                ToolOutput "2",
                ToolPrompt ""]
        executeGhciCommand tool ":m + Prelude" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput ":m + Prelude",
                ToolPrompt ""]
        executeGhciCommand tool "\njfkdfjdkl" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "",
                ToolInput "jfkdfjdkl",
                ToolError "",
#if __GLASGOW_HASKELL__ >= 800
                ToolError "<interactive>:35:1: error: Variable not in scope: jfkdfjdkl",
#elif __GLASGOW_HASKELL__ > 706
                ToolError "<interactive>:36:1: Not in scope: ‘jfkdfjdkl’",
#elif __GLASGOW_HASKELL__ > 702
                ToolError "<interactive>:38:1: Not in scope: `jfkdfjdkl'",
#else
                ToolError "<interactive>:1:1: Not in scope: `jfkdfjdkl'",
#endif
                ToolPrompt ""]
        executeGhciCommand tool "do\n putStrLn \"1\"\n putStrLn \"2\"\n putStrLn \"3\"\n putStrLn \"4\"\n putStrLn \"5\"\n" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "do",
                ToolInput " putStrLn \"1\"",
                ToolInput " putStrLn \"2\"",
                ToolInput " putStrLn \"3\"",
                ToolInput " putStrLn \"4\"",
                ToolInput " putStrLn \"5\"",
                ToolOutput "1",
                ToolOutput "2",
                ToolOutput "3",
                ToolOutput "4",
                ToolOutput "5",
                ToolPrompt ""]
        executeGhciCommand tool "do\n putStrLn \"| 1\"\n putStrLn \"| 2\"\n putStrLn \"| 3\"\n putStrLn \"| 4\"\n putStrLn \"| 5\"\n" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "do",
                ToolInput " putStrLn \"| 1\"",
                ToolInput " putStrLn \"| 2\"",
                ToolInput " putStrLn \"| 3\"",
                ToolInput " putStrLn \"| 4\"",
                ToolInput " putStrLn \"| 5\"",
                ToolOutput "| 1",
                ToolOutput "| 2",
                ToolOutput "| 3",
                ToolOutput "| 4",
                ToolOutput "| 5",
                ToolPrompt ""]
        executeGhciCommand tool "putStr \"ABC\"" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "putStr \"ABC\"",
                ToolPrompt "ABC"]
        executeGhciCommand tool ":m +Data.List" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput ":m +Data.List",
                ToolPrompt ""]
        executeGhciCommand tool ":quit" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput ":quit",
                ToolOutput "Leaving GHCi.",
                ToolExit ExitSuccess]
            doneTesting t
        runTests t]

main :: IO ()
main = do
    projectRoot <- findProjectRoot =<< getCurrentDirectory
    let testTool = projectRoot </> "dist-newstyle/build" </> ("leksah-" <> VERSION_leksah_server)
                    </> "build/test-tool/test-tool"
    args <- getArgs
    case args of
        [] -> do
            doctest ["-isrc", "src/IDE/Utils/CabalPlan.hs", "src/IDE/Utils/CabalProject.hs"]            -- updateGlobalLogger rootLoggerName (\ l -> setLevel DEBUG l)
            (Counts{failures=failures}, _) <- runTestText (putTextToHandle stderr False) (tests testTool)
            if failures == 0
                then exitSuccess
                else exitFailure
        ["ExitSuccess"]     -> exitSuccess
        ["Exit42"]          -> exitWith (ExitFailure 42)
        ["BlankLine", o]    -> hPutStrLn (h o) ""
        ["Hello", o]        -> hPutStrLn (h o) "Hello World"
        ["ErrAndOut"]       -> hPutStrLn stderr "Error" >> hPutStrLn stdout "Output"
        ["Unterminated", o] -> hPutStr (h o) "Unterminated" >> hFlush (h o)
        _  -> exitFailure
 where
    h "StdErr" = stderr
    h _ = stdout
