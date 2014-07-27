-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.Tools
-- Copyright   :  2007-2013 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Utils.ExternalTool (
    runExternalTool'
  , runExternalTool
  , isRunning
  , interruptBuild
) where

import qualified Data.Conduit as C (Sink)
import IDE.Utils.Tool
       (interruptProcessGroupOf, getProcessExitCode, runTool,
        ProcessHandle, ToolOutput(..))
import IDE.Core.State
       (runningTool, modifyIDE_, reflectIDE, useVado, reifyIDE,
        triggerEventIDE, saveAllBeforeBuild, prefs, readIDE, IDEAction,
        IDEM, MonadIDE(..))
import Control.Monad (void, unless, when)
import IDE.Pane.SourceBuffer (belongsToWorkspace, fileSaveAll)
import IDE.Core.Types (StatusbarCompartment(..), IDEEvent(..))
import Control.Concurrent (forkIO)
import System.Process.Vado (vado, readSettings, getMountPoint)
import Data.Conduit (($$))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (isNothing)
import Control.Applicative ((<$>))
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack, null)

runExternalTool' :: MonadIDE m
                => Text
                -> FilePath
                -> [Text]
                -> FilePath
                -> C.Sink ToolOutput IDEM ()
                -> m ()
runExternalTool' description executable args dir handleOutput = do
        runExternalTool (do
                            run <- isRunning
                            return (not run))
                        (\_ -> return ())
                        description
                        executable
                        args
                        dir
                        handleOutput
        return()

runExternalTool :: MonadIDE m
                => m Bool
                -> (ProcessHandle -> IDEM ())
                -> Text
                -> FilePath
                -> [Text]
                -> FilePath
                -> C.Sink ToolOutput IDEM ()
                -> m ()
runExternalTool runGuard pidHandler description executable args dir handleOutput  = do
        prefs <- readIDE prefs
        run <- runGuard
        when run $ do
            when (saveAllBeforeBuild prefs) (do fileSaveAll belongsToWorkspace; return ())
            unless (T.null description) . void $
                triggerEventIDE (StatusbarChanged [CompartmentState description, CompartmentBuild True])
            reifyIDE $ \ideR -> forkIO $ do
                -- If vado is enabled then look up the mount point and transform
                -- the execuatble to "ssh" and the arguments
                mountPoint <- if useVado prefs then getMountPoint dir else return $ Right ""
                (executable', args') <- case mountPoint of
                                            Left mp -> do
                                                s <- readSettings
                                                a <- vado mp s dir [] executable (map T.unpack args)
                                                return ("ssh", map T.pack a)
                                            _ -> return (executable, args)
                -- Run the tool
                (output, pid) <- runTool executable' args' (Just dir)
                reflectIDE (do
                    pidHandler pid
                    modifyIDE_ (\ide -> ide{runningTool = Just pid})
                    output $$ handleOutput) ideR
            return ()

-- ---------------------------------------------------------------------
-- | Handling of Compiler errors
--
isRunning :: MonadIDE m => m Bool
isRunning = do
    maybeProcess <- readIDE runningTool
    liftIO $
        case maybeProcess of
            Just process ->
                isNothing <$> getProcessExitCode process
            Nothing -> return False

interruptBuild :: MonadIDE m => m ()
interruptBuild = do
    maybeProcess <- readIDE runningTool
    liftIO $ case maybeProcess of
        Just h -> interruptProcessGroupOf h
        _ -> return ()


