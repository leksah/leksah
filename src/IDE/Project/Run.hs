-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Running project components interactively: component repls, run/test/bench
-- terminal windows, project terminals.  Carried from the old @IDE.Package@
-- (recent additions; see docs/relicensing.md), with explicit 'Project' /
-- 'Package' arguments in place of the old reader-monad tower.
module IDE.Project.Run
  ( packageOpenRepl
  , packageRunComponentTerm
  , projectSettings
  , projectOpenTerminal
  , addFFCabalTmuxEnv
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Conduit as C
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))

import IDE.Core.State
       (IDEAction, IDEM, MessageLevel(..), Package, Project(..),
        ProjectSettings(..), __, defaultProjectSettings, ideMessage,
        ipdPackageName, pjDir, pjKey, readIDE, workspace, wsSettingsFor)
import IDE.Core.Types (Log(..))
import IDE.Diagnostics (logOutputForBuild)
import IDE.Pane.Log ()
import IDE.Utils.ExternalTool (runExternalTool', sinkLast)
import IDE.Utils.Files (cabalBuildDir)
import IDE.Utils.Process (ToolOutput(..))
import IDE.Utils.RemotePath (parseRemotePath)
import IDE.Project.Nix (projectFileArguments, withToolCommand)
import IDE.Web.ReplTmux
       (ensureCommandWindow, ensureRemoteWindow, ffcabalTmuxEnv,
        findReplWindow, openTerminalInDir, selectTmuxWindowById)
import IDE.Web.RemoteTermRequest (requestLocalTerm, requestRemoteTerm)

-- | Prepend 'ffcabalTmuxEnv', replacing any existing entry — ffcabal's repls
-- must live on leksah's own tmux server so the workspace can reach them.
addFFCabalTmuxEnv :: [(String, String)] -> [(String, String)]
addFFCabalTmuxEnv env = ffcabalTmuxEnv : filter ((/= fst ffcabalTmuxEnv) . fst) env

-- | The per-project settings (command prefix etc.) for a project, read
-- from the open workspace.
projectSettings :: Project -> IDEM ProjectSettings
projectSettings project =
    maybe defaultProjectSettings (wsSettingsFor (pjKey project)) <$> readIDE workspace

-- | The workspace tree's component repl (>) button: bring the component's
-- ffcabal repl window up as a terminal tab.  Fast path: the window already
-- exists in the shared repl session — select it and ask for its tab.
-- Otherwise run @ffcabal repl <target>@ (which creates the window, loads the
-- component and leaves the repl for the user) and open the tab when it's
-- ready.  The run goes through 'withToolCommand' so it sees the same
-- environment as leksah's builds — a different PATH would make cabal treat
-- the project as reconfigured and rebuild the world.
packageOpenRepl :: Project -> Package -> Text -> IDEAction
packageOpenRepl project package component = do
    prefix <- psCmdPrefix <$> projectSettings project
    let target = ipdPackageName package <> ":" <> component
    case parseRemotePath (pjDir (pjKey project)) of
      -- Remote: no ffcabal window cache yet (follow-up) — a plain
      -- `cabal repl TARGET` window in the host's `leksah` tmux session,
      -- reused by name on later clicks.
      Just (host, rdir) -> do
        let shq t = "'" <> T.replace "'" "'\\''" t <> "'"
            rcmd = maybe "" (<> " ") prefix <> "cabal repl " <> shq target
        liftIO . void . forkIO $ do
            _ <- ensureRemoteWindow host rdir ("repl " <> target) (Just rcmd)
            requestRemoteTerm (host <> "#leksah")
      Nothing ->
        liftIO (findReplWindow target) >>= \case
        Just (sid, wid) -> liftIO $ do
            selectTmuxWindowById wid
            requestLocalTerm sid
        Nothing -> do
            let dir = pjDir $ pjKey project
            pjFileArgs <- projectFileArguments project dir
            liftIO (findExecutable "ffcabal") >>= \case
              Nothing -> ideMessage High $ __ "ffcabal was not found on $PATH (component repls need it)"
              Just _ -> withToolCommand project "ghc"
                  (Just ("ffcabal", ["repl", target]
                      <> pjFileArgs
                      <> ["--builddir=" <> T.pack (cabalBuildDir Nothing)])) $ \(cmd, args', nixEnv') -> do
                env <- addFFCabalTmuxEnv <$>
                    maybe (liftIO getEnvironment) return (M.toList <$> nixEnv')
                runExternalTool' (__ "Opening repl") (T.unpack cmd) args' dir (Just env) $ do
                    (mbLastOutput, _) <- C.getZipSink $ (,)
                        <$> C.ZipSink sinkLast
                        <*> C.ZipSink (logOutputForBuild project (LogProject dir) False False)
                    lift . when (mbLastOutput == Just (ToolExit ExitSuccess)) . liftIO $
                        findReplWindow target >>= mapM_ (\(sid, wid) -> do
                            selectTmuxWindowById wid
                            requestLocalTerm sid)

-- | The workspace tree's run (\x25b6) button on exe/test/bench components:
-- run the component in a repl-session terminal window (@cabal run@ \/ @test@
-- \/ @bench@), reusing the window on later clicks.  The window keeps a shell
-- when the command ends so its output stays readable.  It sources ffcabal's
-- captured environment when present, so cabal sees the same configuration
-- (PATH!) as leksah's builds instead of reconfiguring the world.
packageRunComponentTerm :: Project -> Package -> Text -> IDEAction
packageRunComponentTerm project package component = do
    prefix <- psCmdPrefix <$> projectSettings project
    let dir = pjDir $ pjKey project
        target = ipdPackageName package <> ":" <> component
        sub = case T.takeWhile (/= ':') component of
                "test"  -> "test"
                "bench" -> "bench"
                _       -> "run"
        envFile = dir </> cabalBuildDir Nothing </> "ffcabal" </> "env.sh"
        shq t = "'" <> T.replace "'" "'\\''" t <> "'"
        envQ = shq (T.pack envFile)
        cmd = "[ -f " <> envQ <> " ] && . " <> envQ <> " ; cabal " <> sub
              <> " --builddir=" <> shq (T.pack (cabalBuildDir Nothing))
              <> " " <> shq target
    case parseRemotePath dir of
        -- Remote: run in a window of the host's default tmux `leksah`
        -- session (the ssh://HOST tab), with the per-project prefix and no
        -- local ffcabal env.sh sourcing (that file is a local capture).
        Just (host, rdir) -> do
            let rcmd = maybe "" (<> " ") prefix <> "cabal " <> sub <> " " <> shq target
            liftIO . void . forkIO $ do
                _ <- ensureRemoteWindow host rdir (sub <> " " <> target) (Just rcmd)
                requestRemoteTerm (host <> "#leksah")
        Nothing ->
            liftIO . void . forkIO $
                ensureCommandWindow True (T.pack dir <> "#" <> sub <> " " <> target)
                                    dir (sub <> " " <> target) cmd
                    >>= mapM_ requestLocalTerm

-- | Project context menu: open a terminal in the project's directory.
projectOpenTerminal :: Project -> IDEAction
projectOpenTerminal = openTerminalInDir . pjDir . pjKey
