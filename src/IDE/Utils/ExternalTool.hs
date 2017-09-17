{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
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
       (postSyncIDE, runningTool, modifyIDE_, reflectIDE, useVado,
        reifyIDE, triggerEventIDE, saveAllBeforeBuild, prefs, readIDE,
        IDEAction, IDEM, MonadIDE(..))
import Control.Monad (void, unless, when)
import Control.Exception (catch, SomeException(..))
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
import System.Log.Logger (debugM)
import Data.Monoid ((<>))

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import System.Posix.Types (CPid(..))
import System.Exit (ExitCode)
import Control.Concurrent.MVar (withMVar, MVar)
import Unsafe.Coerce (unsafeCoerce)
import System.Posix.Process (getProcessGroupIDOf)
import System.Posix.Signals (inSignalSet, sigINT, getSignalMask)
#endif

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
showSignalMask = ("mask INT "<>) . show . (sigINT `inSignalSet`) <$> getSignalMask
#else
showSignalMask = return ""
#endif

runExternalTool' :: MonadIDE m
                => Text
                -> FilePath
                -> [Text]
                -> FilePath
                -> Maybe [(String,String)]
                -> C.Sink ToolOutput IDEM ()
                -> m ()
runExternalTool' description executable args dir mbEnv handleOutput = do
        runExternalTool (do
                            run <- isRunning
                            return (not run))
                        (\_ -> return ())
                        description
                        executable
                        args
                        dir
                        mbEnv
                        handleOutput
        return()

runExternalTool :: MonadIDE m
                => m Bool
                -> (ProcessHandle -> IDEM ())
                -> Text
                -> FilePath
                -> [Text]
                -> FilePath
                -> Maybe [(String,String)]
                -> C.Sink ToolOutput IDEM ()
                -> m ()
runExternalTool runGuard pidHandler description executable args dir mbEnv handleOutput  = do
        prefs <- readIDE prefs
        run <- runGuard
        when run $ do
            unless (T.null description) . void $
                triggerEventIDE (StatusbarChanged [CompartmentState description, CompartmentBuild True])
            -- If vado is enabled then look up the mount point and transform
            -- the execuatble to "ssh" and the arguments
            mountPoint <- if useVado prefs then liftIO $ getMountPoint dir else return $ Right ""
            (executable', args') <- case mountPoint of
                                        Left mp -> do
                                            s <- liftIO readSettings
                                            a <- liftIO $ vado mp s dir [] executable (map T.unpack args)
                                            return ("ssh", map T.pack a)
                                        _ -> return (executable, args)
            -- Run the tool
            (output, pid) <- liftIO $ runTool executable' args' (Just dir) mbEnv
            modifyIDE_ (\ide -> ide{runningTool = Just (pid, interruptProcessGroupOf pid)})
            reifyIDE $ \ideR -> forkIO $
                reflectIDE (do
                    pidHandler pid
                    output $$ handleOutput
                    modifyIDE_ $ \ide -> ide{runningTool = Nothing}) ideR
            return ()

-- ---------------------------------------------------------------------
-- | Handling of Compiler errors
--
isRunning :: MonadIDE m => m Bool
isRunning = do
    maybeProcess <- readIDE runningTool
    case maybeProcess of
       Just (process, _) ->
            liftIO $ isNothing <$> getProcessExitCode process
       Nothing -> return False

interruptBuild :: MonadIDE m => m ()
interruptBuild = do
    maybeProcess <- readIDE runningTool
    case maybeProcess of
        Just (h, interrupt) ->
            liftIO $ interrupt `catch` (\(_ :: SomeException) ->
                debugM "leksah" "interruptBuild Nothing")
--            modifyIDE_ $ \ide -> ide {runningTool = Just (h, return ())}
        _ -> liftIO $ debugM "leksah" "interruptBuild Nothing"


