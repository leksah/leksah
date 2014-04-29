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
) where

import Graphics.UI.Gtk (Window)
import Control.Monad (when, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import System.Exit (ExitCode(..))
import System.FilePath (dropFileName)
import qualified Data.Conduit as C (Sink)
import qualified Data.Conduit.List as CL (fold)
import qualified Data.Conduit.Util as CU (zipSinks)
import IDE.Utils.Tool (ToolOutput(..))
import IDE.Utils.GUIUtils (__, chooseDir)
import IDE.Core.State (PackageAction, readIDE, prefs, ipdBuildDir, getMainWindow,
            Workspace, wsFile, liftIDE, IDEPackage, IDEM, runPackage, LogLaunch)
import IDE.Pane.Log (getDefaultLogLaunch)
import IDE.Utils.ExternalTool (runExternalTool')
import IDE.LogRef (logOutput)
import IDE.Pane.PackageEditor (choosePackageFile)
import IDE.Workspaces (workspaceTryQuiet)
import IDE.Package (refreshPackage)

-- | Get the last item
sinkLast = CL.fold (\_ a -> Just a) Nothing

logSandbox :: IDEPackage -> LogLaunch -> C.Sink ToolOutput IDEM ()
logSandbox package logLaunch = do
    let log = logOutput logLaunch
    (mbLastOutput, _) <- CU.zipSinks sinkLast log
    when (mbLastOutput == Just (ToolExit ExitSuccess)) .
        lift $ workspaceTryQuiet (runPackage (void $ refreshPackage log) package)

sandboxInit :: PackageAction
sandboxInit = do
    package <- ask
    logLaunch <- getDefaultLogLaunch
    runExternalTool' (__ "Sandbox Init")
        "cabal" ["sandbox", "init"]
        (ipdBuildDir package) (logSandbox package logLaunch)

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
                "cabal" ["sandbox", "init", "--sandbox=" ++ dir]
                (ipdBuildDir package) (logSandbox package logLaunch)

sandboxDelete :: PackageAction
sandboxDelete = do
    package <- ask
    logLaunch <- getDefaultLogLaunch
    runExternalTool' (__ "Sandbox Delete")
        "cabal" ["sandbox", "delete"]
        (ipdBuildDir package) (logSandbox package logLaunch)

chooseSandboxSourceDir :: Window -> Maybe FilePath -> IO (Maybe FilePath)
chooseSandboxSourceDir window = chooseDir window (__ "Select source folder")

sandboxAddSource :: Bool -> PackageAction
sandboxAddSource snapshot = do
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
                "cabal" (["sandbox", "add-source", fp] ++ ["--snapshot" | snapshot])
                (ipdBuildDir package) (logSandbox package logLaunch)

