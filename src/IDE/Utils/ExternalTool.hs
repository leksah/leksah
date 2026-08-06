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

import Prelude ()
import Prelude.Compat
import IDE.Utils.Process
       (interruptProcessGroupOf, getProcessExitCode, runTool,
        ProcessHandle, ToolOutput(..))
import IDE.Core.State
       (runningTool, modifyIDE_, reflectIDE,
        reifyIDE, readIDE,
        IDEM, MonadIDE(..), workspace, wsProjects, wsSettingsFor,
        ProjectSettings(..), pjKey, pjDir, Project)
import IDE.Utils.Files (isSubPath)
import IDE.Utils.RemoteExec
       (interruptRemoteRun, newRunNonce, remoteRunScript, remoteSshArgs)
import IDE.Utils.RemotePath (parseRemotePath)
import IDE.Web.RemoteRefresh (RefreshReason(..), requestRemoteRefresh)
import Control.Monad (void, unless, when)
import Control.Exception (catch, SomeException(..))
import Control.Lens ((?~), (^.))
import Data.List (find)
import Control.Concurrent (forkIO)
import Data.Conduit ((.|), runConduit, ConduitT)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack, null)
import System.Log.Logger (debugM)
import Data.Void (Void)

{-
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import System.Posix.Signals (inSignalSet, sigINT, getSignalMask)
#endif

showSignalMask :: IO String
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
showSignalMask = ("mask INT "<>) . show . (sigINT `inSignalSet`) <$> getSignalMask
#else
showSignalMask = return ""
#endif
-}

runExternalTool' :: MonadIDE m
                => Text
                -> FilePath
                -> [Text]
                -> FilePath
                -> Maybe [(String,String)]
                -> ConduitT ToolOutput Void IDEM ()
                -> m ()
runExternalTool' description executable args dir mbEnv handleOutput = do
        runExternalTool (not <$> isRunning)
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
                -> ConduitT ToolOutput Void IDEM ()
                -> m ()
runExternalTool runGuard pidHandler _description executable args dir mbEnv handleOutput  = do
    run <- runGuard
    when run $
      case parseRemotePath dir of
        -- ssh://host/dir: run the command ON the host, in dir, under a
        -- process group named by a nonce pidfile (see 'remoteRunScript').
        -- cwd/env MUST be Nothing here: createProcess would crash on an
        -- ssh:// cwd, and a locally-captured env (nix PATH,
        -- FFCABAL_TMUX_ARGS, …) must never apply to the local ssh client.
        -- The per-project command prefix (e.g. "nix develop -c") replaces
        -- the local nix wrapping that 'withToolCommand' skips for remote.
        Just (host, rdir) -> do
            mbWs <- readIDE workspace
            let prefix = do
                    ws <- mbWs
                    project <- find (\p -> pjDir (pjKey p) `isSubPath` dir)
                                    (ws ^. wsProjects)
                    psCmdPrefix (wsSettingsFor (pjKey project) ws)
            nonce <- liftIO newRunNonce
            let (executable', args') =
                    remoteSshArgs host (remoteRunScript prefix rdir nonce executable args)
            liftIO . debugM "leksah" $ "runExternalTool remote: ssh " <> show args'
            (output, pid) <- liftIO $ runTool executable' args' Nothing Nothing
            -- Interrupt = SIGINT the remote process group (one pooled ssh
            -- exec); if that ssh itself fails (host gone), fall back to
            -- killing the local ssh client so isRunning can't wedge.
            modifyIDE_ $ runningTool ?~
              (pid, interruptRemoteRun host nonce
                        `catch` \(SomeException _) ->
                            interruptProcessGroupOf pid
                                `catch` \(SomeException _) -> return ())
            reifyIDE $ \ideR -> void . forkIO $ do
                reflectIDE (pidHandler pid >> runConduit (output .| handleOutput)) ideR
                -- Remote panes (Changes, git decorations) refresh on
                -- events; a finished remote run is the main one.
                requestRemoteRefresh RefreshBuildDone
        Nothing -> do
          -- Run the tool
          (output, pid) <- liftIO $ runTool executable args (Just dir) mbEnv
          -- The stored interrupt action can race the tool exiting on its own:
          -- interruptProcessGroupOf (getProcessGroupIDOf inside it) then throws
          -- "does not exist" — benign (it's already gone), but uncaught it kills
          -- the calling thread (logged as "Uncaught exception").  Swallow it.
          modifyIDE_ $ runningTool ?~
            (pid, interruptProcessGroupOf pid `catch` \(SomeException _) -> return ())
          reifyIDE $ \ideR -> void . forkIO $
              reflectIDE (do
                  pidHandler pid
                  runConduit $ output .| handleOutput
                  -- We should not set runningTool = Nothing here becasuse the getProcessExitCode
                  -- in isRunning will let us know it is not in a running state and the next process
                  -- might already have started.
                  ) ideR
          return ()

-- ---------------------------------------------------------------------
-- | Handling of Compiler errors
--
isRunning :: MonadIDE m => m Bool
isRunning =
    readIDE runningTool >>= \case
       Just (process, _) ->
            liftIO $ isNothing <$> getProcessExitCode process
       Nothing -> return False

interruptBuild :: MonadIDE m => m ()
interruptBuild = do
    maybeProcess <- readIDE runningTool
    case maybeProcess of
        Just (_h, interrupt) ->
            liftIO $ interrupt `catch` (\(_ :: SomeException) ->
                debugM "leksah" "interruptBuild Nothing")
        _ -> liftIO $ debugM "leksah" "interruptBuild Nothing"


