{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Sandbox
-- Copyright   :  2007-2014 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Sandbox (
  sandboxInit
, sandboxInitShared
, sandboxDelete
, sandboxAddSource
, sandboxDeleteSource
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import System.Exit (ExitCode(..))
import System.FilePath (dropFileName)
import qualified Data.Conduit as C (Sink, ZipSink(..), getZipSink)
import qualified Data.Conduit.List as CL (fold)
import IDE.Utils.Tool (ToolOutput(..))
import IDE.Utils.GUIUtils (showDialogOptions, __, chooseDir)
import IDE.Core.State
       (reflectIDE, reifyIDE, PackageAction, readIDE, prefs,
        ipdPackageDir, getMainWindow, Workspace, wsFile, liftIDE,
        IDEPackage, IDEM, runPackage, LogLaunch)
import IDE.Pane.Log (getDefaultLogLaunch)
import IDE.Utils.ExternalTool (runExternalTool')
import IDE.LogRef (logOutput)
import IDE.Pane.PackageEditor (choosePackageFile)
import IDE.Workspaces (workspaceTryQuiet)
import IDE.Package (refreshPackage)
import Data.Monoid ((<>))
import qualified Data.Text as T (pack)
import IDE.Core.Types (runWorkspace, IDE(..), PackageM)
import System.Directory (doesFileExist)
import System.FilePath.Windows ((</>))
import GI.Gtk.Enums (MessageType(..))
import GI.Gtk.Objects.Window (Window(..))

-- | Get the last item
sinkLast = CL.fold (\_ a -> Just a) Nothing

sandboxTry :: PackageAction -> PackageAction
sandboxTry action = do
    sandbox <- hasSandbox
    pkg <- ask
    if sandbox then action else do
        ideRef <- lift $ lift ask
        Just ws <- readIDE workspace
        let packageToIO = (`reflectIDE` ideRef) .
                          (`runWorkspace` ws) .
                          (`runPackage` pkg)
        liftIO $ showDialogOptions
            "This action requires a sandboxed package database. Would you like to initialize a sandbox for this package?"
            MessageTypeQuestion
            [ ("Initialize New Sandbox", packageToIO $ sandboxInit >> action)
            , ("Use Existing Sandbox", packageToIO $ sandboxInitShared >> action)
            , ("Cancel", return ())]
            (Just 0)

hasSandbox :: PackageM Bool
hasSandbox = do
    pkg <- ask
    liftIO $ doesFileExist (ipdPackageDir pkg </> "cabal.sandbox.config")

logSandbox :: IDEPackage -> LogLaunch -> C.Sink ToolOutput IDEM ()
logSandbox package logLaunch = do
    let log = logOutput logLaunch
    mbLastOutput <- C.getZipSink $ const <$> C.ZipSink sinkLast <*> C.ZipSink log
    when (mbLastOutput == Just (ToolExit ExitSuccess)) .
        lift $ workspaceTryQuiet (runPackage (void $ refreshPackage log) package)

sandboxInit :: PackageAction
sandboxInit = do
    package <- ask
    logLaunch <- getDefaultLogLaunch
    runExternalTool' (__ "Sandbox Init")
        "cabal" ["sandbox", "init"]
        (ipdPackageDir package) (logSandbox package logLaunch)

chooseSandboxDir :: Window -> Maybe FilePath -> IO (Maybe FilePath)
chooseSandboxDir window = chooseDir window (__ "Select sandbox folder")

sandboxInitShared :: PackageAction
sandboxInitShared = do
    package <- ask
    ws <- lift ask
    window <- liftIDE getMainWindow
    mbDir <- liftIO $ chooseSandboxDir window Nothing
    case mbDir of
        Nothing -> return ()
        Just dir -> do
            logLaunch <- getDefaultLogLaunch
            runExternalTool' (__ "Sandbox Init")
                "cabal" ["sandbox", "init", "--sandbox=" <> T.pack dir]
                (ipdPackageDir package) (logSandbox package logLaunch)

sandboxDelete :: PackageAction
sandboxDelete = sandboxTry $  do
    package <- ask
    logLaunch <- getDefaultLogLaunch
    runExternalTool' (__ "Sandbox Delete")
        "cabal" ["sandbox", "delete"]
        (ipdPackageDir package) (logSandbox package logLaunch)

chooseSandboxSourceDir :: Window -> Maybe FilePath -> IO (Maybe FilePath)
chooseSandboxSourceDir window = chooseDir window (__ "Select source folder")

sandboxAddSource :: Bool -> PackageAction
sandboxAddSource snapshot = sandboxTry $ do
    package <- ask
    ws <- lift ask
    let path = dropFileName (wsFile ws)
    window <- liftIDE getMainWindow
    mbFilePath <- liftIO $ chooseSandboxSourceDir window (Just path)
    case mbFilePath of
        Nothing -> return ()
        Just fp -> do
            logLaunch <- getDefaultLogLaunch
            runExternalTool' (__ "Sandbox Add Source")
                "cabal" (["sandbox", "add-source", T.pack fp] ++ ["--snapshot" | snapshot])
                (ipdPackageDir package) (logSandbox package logLaunch)

sandboxDeleteSource :: FilePath -> PackageAction
sandboxDeleteSource dir = sandboxTry $ do
    package <- ask
    logLaunch <- getDefaultLogLaunch
    runExternalTool' (__ "Sandbox Delete Source")
        "cabal" ["sandbox", "delete-source", T.pack dir]
        (ipdPackageDir package) (logSandbox package logLaunch)

