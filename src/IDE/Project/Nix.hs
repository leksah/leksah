-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The per-project nix environment cache, and the wrapper that runs a tool
-- command inside it.  Carried from the old @IDE.Package@ (these are recent
-- additions; see docs/relicensing.md).
--
-- A project's dev-shell environment (PATH etc.) is captured once — from
-- @shell.nix@\/@default.nix@ via @nix-shell@ (with gc-rooted shells so they
-- survive collection), from a flake via @nix develop@, or via @hix@ for a
-- Haskell project with neither — into @~\/.leksah-VERSION\/nix.cache@, and
-- every build\/run\/repl command is then re-issued through @bash -c@ with
-- that environment.
module IDE.Project.Nix
  ( projectRefreshNix
  , updateNixCache
  , projectFileArguments
  , withToolCommand
  ) where

import Control.Lens ((.~), to)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), makeRelative)
import System.Log.Logger (debugM)

import IDE.Core.State
       (IDEAction, IDEM, MonadIDE, Project(..), __, liftIDE, modifyIDE_,
        nixCache, nixEnv, pjDir, pjKey, prefs, readIDE, javaScript)
import IDE.Diagnostics (logOutputForBuild)
import IDE.Core.Types (Log(..))
import IDE.Pane.Log (showDefaultLogLaunch')
import IDE.Utils.ExternalTool (runExternalTool')
import IDE.Utils.Files
       (findCabalProjectRoot, getConfigDir, loadNixCache, nixShellFile,
        saveNixCache)
import IDE.Utils.Process (ToolOutput(..))
import IDE.Utils.Project
       (CabalProject(..), ProjectKey(..), StackProject(..))
import IDE.Utils.RemotePath (isRemotePath, remoteMakeRelative)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))

-- | Refresh the cached nix environment(s) for a project: @ghc@, plus
-- @ghcjs@ when the JavaScript build preference is on.  Stack projects
-- manage their own environment.
projectRefreshNix :: Project -> IDEAction
projectRefreshNix project = projectRefreshNix' project (return ())

projectRefreshNix' :: Project -> IDEAction -> IDEAction
projectRefreshNix' project continuation = do
    prefs' <- readIDE prefs
    case pjKey project of
        StackTool _ -> continuation
        _ -> updateNixCache project ("ghc":["ghcjs" | javaScript prefs']) continuation

updateNixCache :: MonadIDE m => Project -> [Text] -> IDEAction -> m ()
updateNixCache project compilers continuation = do
    liftIO $ debugM "leksah" "updateNixCache"
    loop compilers
  where
    loop :: MonadIDE m => [Text] -> m ()
    loop [] = liftIDE continuation
    loop (compiler:rest) = do
        showDefaultLogLaunch'

        let dir = pjDir $ pjKey project
        nixShellFile (pjKey project) >>= \case
            Just nixFile -> do
                configDir <- liftIO getConfigDir
                let gcRootsDir = configDir </> "nix-gc-roots" <> dir
                    shellFile = T.pack (gcRootsDir </> "shells.") <> compiler
                    shellDrvFile = shellFile <> ".drv"
                    shellOutFile = shellFile <> ".out"
                liftIO $ createDirectoryIfMissing True gcRootsDir
                let exp' = "let x = (let fn = import " <> T.pack nixFile
                              <> "; in if builtins.isFunction fn then fn {} else fn);"
                              <> "in ({ shells = { " <> compiler <> " = ({ env = x; } // x).env; }; } // x).shells." <> compiler
                    logOut = C.getZipSink $ const
                              <$> C.ZipSink CL.consume
                              <*> C.ZipSink (logOutputForBuild project (LogNix nixFile ("shells." <> compiler)) False False)
                    runNixShell =
                            lift $ runExternalTool' (__ "Nix")
                                             "nix-shell"
                                             ["-E", exp', "--run", "( set -o posix ; set )"]
                                             dir Nothing $ do
                                out <- logOut
                                when (take 1 (reverse out) == [ToolExit ExitSuccess]) $ do
                                    _ <- saveNixCache (pjKey project) compiler out
                                    newCache <- loadNixCache
                                    lift $ do
                                        modifyIDE_ $ nixCache .~ newCache
                                        loop rest
                runExternalTool' (__ "Nix")
                                 "nix-instantiate"
                                 [ "-E", exp'
                                 , "--indirect"
                                 , "--add-root", shellDrvFile]
                                 dir Nothing $ do
                    out <- logOut
                    if take 1 (reverse out) == [ToolExit ExitSuccess]
                        then lift $ runExternalTool' (__ "Nix")
                                             "nix-store"
                                             [ "--realize", shellDrvFile
                                             , "--indirect"
                                             , "--add-root", shellOutFile]
                                             dir Nothing $ do
                                _out <- logOut
                                runNixShell
                        else runNixShell
            _ -> do
                let logOut = C.getZipSink $ const
                              <$> C.ZipSink CL.consume
                              <*> C.ZipSink (logOutputForBuild project (LogProject (pjDir $ pjKey project)) False False)
                    -- Capture a dev shell's environment (PATH etc.) by running its
                    -- `develop` sub-command and dumping the resulting shell vars.
                    runDevEnv toolLabel toolExe =
                        runExternalTool' toolLabel toolExe
                                 ["develop", "--command", "bash", "-c", "( set -o posix ; set )"]
                                 dir Nothing $ do
                            out <- logOut
                            when (take 1 (reverse out) == [ToolExit ExitSuccess]) $ do
                                _ <- saveNixCache (pjKey project) compiler out
                                newCache <- loadNixCache
                                lift $ do
                                    modifyIDE_ $ nixCache .~ newCache
                                    loop rest
                -- Pick the dev-shell tool: a flake (any language) uses plain
                -- `nix develop`; otherwise `hix` is only for a Haskell (cabal)
                -- project with no flake.  A non-Haskell project with no nix files
                -- has no environment to load, so skip it — running `hix` on e.g. a
                -- Rust project is wrong, and a missing `hix` used to throw on the
                -- reflex frame thread and freeze the whole window.
                liftIO (doesFileExist (dir </> "flake.nix")) >>= \case
                    True  -> runDevEnv (__ "Nix") "nix"
                    False -> case pjKey project of
                        CabalTool {} -> liftIO (findExecutable "hix") >>= \case
                            Just _  -> runDevEnv (__ "Hix") "hix"
                            Nothing -> do
                                liftIO $ debugM "leksah"
                                    "hix not on PATH; skipping nix env load"
                                loop rest
                        _ -> loop rest

-- | The @--project-file@\/@--stack-yaml@ arguments a non-default project
-- file needs.
projectFileArguments :: MonadIO m => Project -> FilePath -> m [Text]
projectFileArguments project dir =
    case pjKey project of
        -- Remote: no local findCabalProjectRoot walk — same semantics, no IO.
        CabalTool (CabalProject file) | isRemotePath dir -> do
            let projectFile = remoteMakeRelative dir file
            return $ if projectFile /= "cabal.project"
                                then [ "--project-file", T.pack projectFile ]
                                else []
        CabalTool (CabalProject file) -> do
            let projectFile = T.pack $ makeRelative dir file
            defaultProjectRoot <- liftIO $ findCabalProjectRoot dir
            return $ if file /= defaultProjectRoot </> "cabal.project"
                                then [ "--project-file", projectFile ]
                                else []
        StackTool (StackProject file) -> do
            let projectFile = T.pack $ remoteMakeRelative dir file
            return $ if projectFile /= "stack.yaml"
                                then [ "--stack-yaml", projectFile ]
                                else []
        _ -> return []

-- | Run a tool command for a project: local commands are re-issued through
-- @bash -c@ with the project's cached nix environment (loading it first if
-- cold); remote commands pass through untouched — the remote wrapping
-- (@psCmdPrefix@) happens in 'IDE.Utils.ExternalTool'.  'Nothing' commands
-- (a project type that cannot run this operation) just report that.
withToolCommand
    :: Project
    -> Text  -- ^ compiler flavour key for the nix env (@\"ghc\"@ / @\"ghcjs\"@)
    -> Maybe (Text, [Text])
    -> ((Text, [Text], Maybe (M.Map String String)) -> IDEAction)
    -> IDEAction
withToolCommand project compiler mbCmd continuation =
    case mbCmd of
        Nothing -> ideNoCommand
        Just (cmd, args)
            | isRemotePath (pjDir (pjKey project)) ->
                continuation (cmd, args, Nothing)
            | otherwise ->
                readIDE (to (nixEnv (pjKey project) compiler)) >>= \case
                    Just env -> continuation (wrap cmd args env)
                    Nothing -> updateNixCache project [compiler] $
                        readIDE (to (nixEnv (pjKey project) compiler)) >>= \case
                            Just env -> continuation (wrap cmd args env)
                            -- No nix environment at all: run plainly.
                            Nothing  -> continuation (cmd, args, Nothing)
  where
    -- Re-issue through bash with the captured environment so PATH etc.
    -- come from the project's dev shell.
    wrap cmd args env =
        ( "bash"
        , [ "-c", T.unwords (map quote (cmd : args)) ]
        , Just env )
    quote t = "'" <> T.replace "'" "'\\''" t <> "'"
    ideNoCommand = liftIO $ debugM "leksah"
        "withToolCommand: no command for this project type"
