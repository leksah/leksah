{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Package
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | The packages methods of ide.
--
---------------------------------------------------------------------------------

module IDE.Package (
    projectRefreshNix
,   projectRefreshNix'
,   buildPackage
,   buildCustomProject
,   customBuildCommand

,   packageDoc
,   packageDoc'
,   packageClean
,   packageClean'
,   packageInstall
,   packageInstall'
,   packageRun'
,   packageRunJavaScript'
,   activatePackage
,   deactivatePackage

,   packageTest
,   packageTest'
,   packageBench
,   packageBench'
,   packageSdist
,   packageOpenDoc

,   getPackageDescriptionAndPath
,   getEmptyModuleTemplate
,   getModuleTemplate
,   ModuleLocation(..)
,   addModuleToPackageDescr
,   delModuleFromPackageDescr

,   idePackageFromPath'
,   ideProjectFromKey
,   writeGenericPackageDescription'

,   runPackage
,   packageOpenRepl
,   packageRunComponentTerm
,   projectOpenTerminal
,   getActiveComponent
,   projectFileArguments
,   exeToRun
,   printf
,   interruptSaveAndRun

) where

import Prelude ()
import Prelude.Compat

import Control.Arrow (Arrow(..))
import Control.Concurrent
       (readMVar, takeMVar, putMVar, newEmptyMVar, modifyMVar, tryPutMVar,
        newMVar, modifyMVar_, forkIO, threadDelay)
import Control.Monad.IO.Unlift (MonadUnliftIO(..))

import Data.Char (isSpace)
import Data.Function (on)
import Data.Maybe
       (listToMaybe, isNothing, mapMaybe, fromMaybe, isJust, fromJust,
        catMaybes)
import Data.Void (Void)

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
-- Cabal 3.17 (stable-haskell fork) added a `Normal` constructor to
-- Distribution.Verbosity, which collides with leksah's own message-level
-- `Normal` (IDE.Core.State) used in `ideMessage Normal`.  Hide Cabal's.
#if MIN_VERSION_Cabal(3,17,0)
import Distribution.Verbosity hiding (Normal)
#else
import Distribution.Verbosity
#endif
import Distribution.Utils.ShortText (fromShortText)

import System.FilePath
import System.Directory
       (createDirectoryIfMissing, removeDirectoryRecursive,
        canonicalizePath, setCurrentDirectory, doesFileExist,
        doesDirectoryExist, findExecutable)
import qualified Data.Set as S (fromList)
import Data.Either (isRight)
import Data.Map (Map)
import System.Exit (ExitCode(..))
import qualified Data.Conduit as C (ZipSink(..), getZipSink)
import qualified Data.Conduit.List as CL (fold, consume)
import Data.Conduit (ConduitT)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad (unless, void, when)
import Data.Traversable (forM)
import Data.Foldable (forM_)
import Debug.Trace (trace)
import Control.Exception (SomeException(..), IOException, catch)

import IDE.Web.RemoteTermRequest (requestLocalTerm, requestRemoteTerm)
import IDE.Web.ReplTmux
       (ffcabalTmuxEnv, findReplWindow, selectTmuxWindowById,
        ensureCommandWindow, ensureRemoteWindow, openTerminalInDir)
import qualified IDE.Core.State as State (runPackage)
import IDE.Core.State
       (pjPackages, changePackage,
        ipdPackageDir, PackageM, runProject, runWorkspace, debug,
        isError, runningTool, nixEnv, useVado,
        nixCache, modifyIDE_, pjDir, javaScript, ProjectAction,
        ipdPackageName, mkPackageMap, reflectIDEI,
        sysMessage, getDataDir,
        catchIDE, MessageLevel(..), ideMessage, activeComponent,
        activeProject, wsName, workspace, activePack, readIDE,
        Prefs, PackageAction, IDEM, IDEAction, IDEPackage(..), Project(..),
        MonadIDE, __, prefs, saveAllBeforeBuild, triggerBuild, native,
        packageIdentifierToString, leksahTemplateFileExtension,
        Log(..),
        MonadIDE(..),
        ProjectKey(..), autoURI, errorRefs, reflectIDE,
        StackProject(..), CabalProject(..), pjKey, pjIsCabal, pjIsStack,
        pjFileOrDir, CustomProject(..), ProjectSettings(..),
        defaultProjectSettings, wsSettingsFor)
import IDE.Gtk.State (postSyncIDE, postAsyncIDE, delayedBy)
import Distribution.Simple.Utils (writeUTF8File)
import Distribution.PackageDescription.PrettyPrint
       (showGenericPackageDescription)
import IDE.Pane.Log
       (addLogLaunchData, showLog, buildLogLaunchByName,
        showDefaultLogLaunch', getDefaultLogLaunch)
import IDE.Pane.SourceBuffer
       (removeTestLogRefs, fileSaveAll, belongsToWorkspace')
import IDE.Utils.Files
       (cabalProjectBuildDir, cabalBuildDir, loadNixCache, saveNixCache,
        getConfigDir, nixShellFile, getConfigFilePathForLoad)
import IDE.LogRef
       (logIdleOutput, logOutputForBuild, logOutputForCargoBuild,
        logOutputDefault, logOutput)
import Distribution.ModuleName (ModuleName)
import Data.List
       (intercalate, nub, nubBy, delete, dropWhileEnd)
import IDE.Utils.Process (ToolOutput(..), ProcessHandle)
import IDE.Pane.WebKit.Documentation
       (showDocumentationPane, loadDoc, reloadDoc)
import IDE.Pane.WebKit.Output
       (loadOutputUri, loadOutputHtmlFile, showOutputPane)
import System.Log.Logger (debugM)
#if !defined(ghcjs_HOST_OS)
-- vado pulls monad-logger→fast-logger, which doesn't build on the JS backend.
import System.Process.Vado (getMountPoint)
#endif
import qualified Data.Text as T
       (unlines, reverse, null, dropWhile, lines, isPrefixOf,
        stripPrefix, replace, unwords, takeWhile, pack, unpack)
import IDE.Utils.ExternalTool
       (runExternalTool', runExternalTool, isRunning,
        interruptBuild)
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)
import IDE.Utils.Files (findCabalProjectRoot)
import System.Environment (getEnvironment)
import Distribution.Simple.LocalBuildInfo
       (Component(..))
import Distribution.Compiler (CompilerFlavor(..))
import qualified Data.Map as M
       (toList, fromList, lookup)
import qualified Data.ByteString.Lazy as LBS (ByteString, fromStrict)
import Data.Text.Encoding (decodeUtf8)
import Control.Lens ((.~), (?~), (%~), _Just, to)
import System.Process (getProcessExitCode, showCommandForUser)
-- unix is a boot library even on the JS backend, but there the process
-- internals hold JSVal pids, so the POSIX group-kill doesn't typecheck —
-- fall back to plain terminateProcess there.
#if defined(MIN_VERSION_unix) && !defined(ghcjs_HOST_OS)
import System.Posix (sigKILL, signalProcessGroup, getProcessGroupIDOf)
import System.Process.Internals
       (withProcessHandle, ProcessHandle__(..))
#else
import IDE.Utils.Process (terminateProcess)
#endif
#if MIN_VERSION_Cabal(3,8,0)
import Distribution.Simple.PackageDescription
       (readGenericPackageDescription)
import Distribution.Utils.Path (getSymbolicPath)
#else
import Distribution.PackageDescription.Parsec
       (readGenericPackageDescription)
#endif
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path (makeSymbolicPath, SymbolicPathX)
#endif
import Data.ByteString (ByteString)
import Distribution.PackageDescription.Parsec
       (parseGenericPackageDescriptionMaybe)
import IDE.Web.FS (fsReadFile, fsDoesFileExist, fsListFilesRecursive)
import IDE.Utils.RemotePath
       (isRemotePath, parseRemotePath, remoteMakeRelative, renderRemotePath)
#if !defined(ghcjs_HOST_OS)
import IDE.Utils.RemoteExec (SnapshotEntry(..), remoteCabalSnapshot)
#endif
import Distribution.Pretty (prettyShow)
import qualified System.FilePath.Glob as Glob (globDir, compile)

printf :: PrintfType r => Text -> r
printf = S.printf . T.unpack

-- | Get the last item
sinkLast :: Monad m => ConduitT a o m (Maybe a)
sinkLast = CL.fold (\_ a -> Just a) Nothing

moduleInfo :: (a -> BuildInfo) -> (a -> [ModuleName]) -> a -> [(ModuleName, BuildInfo)]
moduleInfo bi mods a = map (, buildInfo) $ mods a
    where buildInfo = bi a

myLibModules, myExeModules, myTestModules, myBenchmarkModules
  :: PackageDescription -> [(ModuleName, BuildInfo)]
myLibModules pd = (case library pd of
                    Nothing -> []
                    Just l -> moduleInfo libBuildInfo explicitLibModules l)
                  ++ concatMap (moduleInfo libBuildInfo explicitLibModules) (subLibraries pd)
myExeModules pd = concatMap (moduleInfo buildInfo exeModules) (executables pd)
myTestModules pd = concatMap (moduleInfo testBuildInfo (otherModules . testBuildInfo)) (testSuites pd)
myBenchmarkModules pd = concatMap (moduleInfo benchmarkBuildInfo (otherModules . benchmarkBuildInfo)) (benchmarks pd)

activatePackage :: MonadIDE m => Maybe FilePath -> Maybe Project -> Maybe IDEPackage -> Maybe Text -> m ()
activatePackage mbPath mbProject mbPack mbComponent = do
    liftIO $ debugM "leksah" $ "activatePackage " <> show (mbPath, pjKey <$> mbProject, ipdCabalFile <$> mbPack, mbComponent)
    case mbPath of
        -- A remote package's directory doesn't exist locally; leave the
        -- process cwd alone (remote runs cd on the far side).
        Just p | not (isRemotePath p) -> liftIO $ setCurrentDirectory (dropFileName p)
        _ -> return ()

deactivatePackage :: IDEAction
deactivatePackage = activatePackage Nothing Nothing Nothing Nothing

interruptSaveAndRun :: (MonadUnliftIO m, MonadIDE m) => m () -> m ()
interruptSaveAndRun action = do
    alreadyRunning <- isRunning
    if alreadyRunning
        then do
            liftIO $ debugM "leksah" "interruptSaveAndRun"
            interruptBuild
            delayedBy 200000 $ interruptSaveAndRun action
            return ()
        else run
  where
    run = do
        prefs' <- readIDE prefs
        when (saveAllBeforeBuild prefs') . liftIDE . void $ fileSaveAll belongsToWorkspace'
        action

projectRefreshNix :: ProjectAction
projectRefreshNix =
    interruptSaveAndRun $ projectRefreshNix' (return ())

projectRefreshNix' :: IDEAction -> ProjectAction
projectRefreshNix' continuation = do
    project <- ask
    prefs'     <- readIDE prefs
    case pjKey project of
        StackTool _ ->
            liftIDE continuation
        _ ->
            updateNixCache project ("ghc":["ghcjs" | javaScript prefs']) continuation

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

getActiveComponent :: Project -> IDEPackage -> IDEM (Maybe Text)
getActiveComponent project package = do
    isActiveProject   <- maybe False (on (==) pjKey project) <$> readIDE activeProject
    isActivePackage   <- (isActiveProject &&) . maybe False (on (==) ipdCabalFile package) <$> readIDE activePack
    if isActivePackage
        then fmap ((ipdPackageName package <> ":")<>) <$> readIDE activeComponent
        else return Nothing

withToolCommand :: MonadIDE m => Project -> CompilerFlavor -> Maybe (FilePath, [Text]) -> ((FilePath, [Text], Maybe (Map String String)) -> IDEAction) -> m ()
withToolCommand project _compiler Nothing _continuation = ideMessage High $ "withToolCommand failed for " <> T.pack (show $ pjKey project)
withToolCommand project compiler (Just (cmd, args)) continuation
  | isRemotePath (pjDir (pjKey project)) =
    -- Remote project: the local nix machinery (nix cache env, nix-shell,
    -- vado mount points) does not apply — the per-project command prefix
    -- wraps the command on the remote side (see runExternalTool).
    liftIDE $ continuation (cmd, args, Nothing)
withToolCommand project compiler (Just (cmd, args)) continuation = do
    liftIO $ debugM "leksah" $ "withToolCommand " <> show (project, compiler, cmd, args)
    prefs' <- readIDE prefs
    -- Nix cache will not work over vado
#if defined(ghcjs_HOST_OS)
    let enableNixCache = True  -- no vado (or nix) in the browser
#else
    enableNixCache <- if useVado prefs' then liftIO $ isRight <$> getMountPoint (pjDir $ pjKey project) else return True
#endif
    nixShellFile (pjKey project) >>= \case
        _ | enableNixCache -> do
            let nixContinuation env = continuation ("bash", ["-c", T.pack . showCommandForUser cmd $ map T.unpack args], Just env)
            readIDE (to $ nixEnv (pjKey project) "ghc") >>= \case
                Just env -> liftIDE $ nixContinuation env
                Nothing -> updateNixCache project ["ghc"] $
                    readIDE (to $ nixEnv (pjKey project) "ghc") >>= mapM_ nixContinuation
        Just nixFile ->

            liftIDE $ continuation ("nix-shell", [ "-E"
                    , "let x = (let fn = import " <> T.pack nixFile <>
                                    "; in if builtins.isFunction fn then fn {} else fn); in ({ shells = { ghc = ({ env = x; } // x).env; }; } // x).shells.ghc"
                    , "--run", T.pack . showCommandForUser cmd $ map T.unpack args], Nothing)
        Nothing -> liftIDE $ continuation (cmd, args, Nothing)

#if MIN_VERSION_Cabal(3,17,0)
normalVerbosity = mkVerbosity defaultVerbosityHandles normal
#else
normalVerbosity = normal
#endif

-- | Parse a .cabal file from bytes that came through the FS seam (the
-- browser demo's mock tree, a remote host, or a project snapshot).
gpdFromBytes :: FilePath -> ByteString -> IO GenericPackageDescription
gpdFromBytes f bs = case parseGenericPackageDescriptionMaybe bs of
    Just gpd -> return gpd
    Nothing  -> ioError (userError ("Failed to parse " <> f))

-- Cabal 3.14 moved the cabal-file argument to a SymbolicPath and added a
-- working-directory argument; older Cabal takes a plain FilePath.
readGPD :: Verbosity -> FilePath -> IO GenericPackageDescription
#if defined(ghcjs_HOST_OS)
-- Browser demo: the .cabal file lives in the page-seeded mock tree
-- (IDE.Web.FS), so parse it from bytes instead of opening a real file.
readGPD _ f = gpdFromBytes f =<< fsReadFile f
#else
readGPD v f
    -- Remote .cabal files come through the FS seam as bytes.
    | isRemotePath f = gpdFromBytes f =<< fsReadFile f
    | otherwise =
#if MIN_VERSION_Cabal(3,14,0)
        readGenericPackageDescription v Nothing (makeSymbolicPath f)
#else
        readGenericPackageDescription v f
#endif
#endif
#if MIN_VERSION_Cabal(3,14,0)
-- main-module / exe paths became SymbolicPaths in Cabal 3.14.
mainPath :: SymbolicPathX allowAbsolute from to -> FilePath
mainPath p = getSymbolicPath p
#else
mainPath :: FilePath -> FilePath
mainPath p = p
#endif

readAndFlattenPackageDescription :: MonadIDE m => IDEPackage -> m PackageDescription
readAndFlattenPackageDescription package =
    liftIO $ flattenPackageDescription <$>
         readGPD normalVerbosity (ipdCabalFile package)

runCabalBuild :: CompilerFlavor -> Bool -> Bool -> Bool -> (Project, [IDEPackage]) -> (Bool -> IDEAction) -> IDEAction
runCabalBuild compiler backgroundBuild jumpToWarnings withoutLinking (project, packages) continuation = do
    let dir = pjDir $ pjKey project
    activeComponent' <- catMaybes <$> mapM (getActiveComponent project) packages
    pjFileArgs <- projectFileArguments project dir
    flagsForTestsAndBenchmarks <- fmap concat $ forM packages $ \package -> do
        -- Local packages re-read the .cabal so the flags see edits made
        -- since the project loaded; a remote re-read would cost one ssh
        -- round trip per package per build, so those use the names loaded
        -- with the project instead.
        (testNames, benchNames) <-
            if isRemotePath (ipdCabalFile package)
                then return (ipdTests package, ipdBenchmarks package)
                else do
                    pd <- readAndFlattenPackageDescription package
                    return ( map (T.pack . unUnqualComponentName . testName) (testSuites pd)
                           , map (T.pack . unUnqualComponentName . benchmarkName) (benchmarks pd) )
        let pkgName = ipdPackageName package
        return $
            [ pkgName <> ":lib:" <> pkgName | ipdHasLib package ]
            <> (if "--enable-tests" `elem` ipdConfigFlags package
                then case pjKey project of
                    StackTool {} -> ["--test", "--no-run-tests"] -- if we use stack, with tests enabled, we build the tests without running them
                    CabalTool {} -> map (\t -> pkgName <> ":test:" <> t) testNames
                    _ -> []
                else [])
            <> (if "--enable-benchmarks" `elem` ipdConfigFlags package
                then case pjKey project of
                    StackTool {} -> ["--bench", "--no-run-benchmarks"] -- if we use stack, with benchmarks enabled, we build the benchmarks without running them
                    CabalTool {} -> map (\t -> pkgName <> ":benchmark:" <> t) benchNames
                    _ -> []
                else [])
    -- Native GHC builds of cabal projects go through ffcabal (vendor/ffcabal)
    -- WHEN GHCI MODE IS ON: it type-checks each local component in a cached
    -- tmux repl first — the first error surfaces in seconds — then builds in
    -- parallel.  Background builds use its --repl-only (checks only),
    -- replacing the old --with-ld=false no-link trick.  With ghci mode off
    -- the user has opted out of repls, so ALL builds use regular cabal.
    -- Cross compilation (GHCJS) and stack always use plain cabal/stack, and
    -- we fall back to cabal when ffcabal isn't on PATH.
    ghciMode <- debug <$> readIDE prefs
    -- Remote projects always take the plain-cabal arm for now: ffcabal's
    -- cached repls live in the LOCAL tmux server (remote ffcabal is a
    -- planned follow-up via the remote-terminal machinery).
    mbFFCabal <- if ghciMode && not (isRemotePath dir)
                    then liftIO (findExecutable "ffcabal")
                    else return Nothing
    let nativeCabalCmd = case mbFFCabal of
            Just _ -> ("ffcabal", ["build"]
              <> pjFileArgs
              <> ["--builddir=" <> T.pack (cabalBuildDir Nothing)]
              <> ["--repl-only" | backgroundBuild && withoutLinking]
              <> activeComponent'
              <> flagsForTestsAndBenchmarks)
            Nothing -> ("cabal", ["new-build"]
              <> pjFileArgs
              <> ["--builddir=" <> T.pack (cabalBuildDir Nothing)]
              <> ["--with-ld=false" | backgroundBuild && withoutLinking]
              <> activeComponent'
              <> flagsForTestsAndBenchmarks)
    let mbCmdAndArgs = case pjKey project of
            StackTool {} -> Just ("stack",
                 ["build"]
              <> pjFileArgs
              <> activeComponent'
              <> flagsForTestsAndBenchmarks)
            CabalTool {} -> Just nativeCabalCmd
            CustomTool p -> pjCustomGhcBuild p
            NixTool _ -> Nothing
            -- A Makefile project builds with make (through the nix env when
            -- the project has one, like the other tools).
            MakeTool {} -> Just ("make", [])
        mbCmdAndArgs' = second (++ concatMap ipdBuildFlags packages) <$> mbCmdAndArgs

    withToolCommand project compiler mbCmdAndArgs' $ \(cmd, args', nixEnv') -> do
        let mbEnv = M.toList <$> nixEnv'
        -- ffcabal drives tmux repls: pin them to leksah's own tmux server so
        -- the workspace repl buttons / terminal tabs can reach the windows
        -- (see 'ffcabalTmuxEnv').  Materialize the environment when we'd
        -- otherwise inherit it.
        mbEnv' <- if isJust mbFFCabal
            then Just . addFFCabalTmuxEnv <$> maybe (liftIO getEnvironment) return mbEnv
            else return mbEnv
        runExternalTool' (__ "Building") cmd args' dir mbEnv' $ do
            (mbLastOutput, _) <- C.getZipSink $ (,)
                <$> C.ZipSink sinkLast
                <*> (C.ZipSink $ logOutputForBuild project (LogProject dir) backgroundBuild jumpToWarnings)
            lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess))
  `catchIDE`
      (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

-- | Prepend 'ffcabalTmuxEnv', replacing any existing entry.
addFFCabalTmuxEnv :: [(String, String)] -> [(String, String)]
addFFCabalTmuxEnv env = ffcabalTmuxEnv : filter ((/= fst ffcabalTmuxEnv) . fst) env

-- | The workspace tree's component repl (>) button: bring the component's
-- ffcabal repl window up as a terminal tab.  Fast path: the window already
-- exists in the shared repl session — select it and ask for its tab.
-- Otherwise run @ffcabal repl <target>@ (which creates the window, loads the
-- component and leaves the repl for the user) and open the tab when it's
-- ready.  The run goes through 'withToolCommand' so it sees the same
-- environment as leksah's builds — a different PATH would make cabal treat
-- the project as reconfigured and rebuild the world.
packageOpenRepl :: Text -> PackageAction
packageOpenRepl component = do
    project <- lift ask
    package <- ask
    prefix <- psCmdPrefix <$> liftIDE (projectSettings project)
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
        liftIDE $ liftIO (findReplWindow target) >>= \case
        Just (sid, wid) -> liftIO $ do
            selectTmuxWindowById wid
            requestLocalTerm sid
        Nothing -> do
            let dir = pjDir $ pjKey project
            pjFileArgs <- projectFileArguments project dir
            liftIO (findExecutable "ffcabal") >>= \case
              Nothing -> ideMessage High $ __ "ffcabal was not found on $PATH (component repls need it)"
              Just _ -> withToolCommand project GHC
                  (Just ("ffcabal", ["repl", target]
                      <> pjFileArgs
                      <> ["--builddir=" <> T.pack (cabalBuildDir Nothing)])) $ \(cmd, args', nixEnv') -> do
                env <- addFFCabalTmuxEnv <$>
                    maybe (liftIO getEnvironment) return (M.toList <$> nixEnv')
                runExternalTool' (__ "Opening repl") cmd args' dir (Just env) $ do
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
packageRunComponentTerm :: Text -> PackageAction
packageRunComponentTerm component = do
    project <- lift ask
    package <- ask
    prefix <- psCmdPrefix <$> liftIDE (projectSettings project)
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

-- | The per-project settings (command prefix etc.) for a project, read
-- from the open workspace.
projectSettings :: Project -> IDEM ProjectSettings
projectSettings project =
    maybe defaultProjectSettings (wsSettingsFor (pjKey project)) <$> readIDE workspace

-- | Project context menu: open a terminal in the project's directory.
-- Remote projects get a window in the host's default tmux @leksah@ session
-- surfaced as an @ssh:\/\/HOST@ tab; local ones a window in the shared
-- repl session.
projectOpenTerminal :: ProjectAction
projectOpenTerminal = openTerminalInDir . pjDir . pjKey =<< ask

--isConfigError :: Monad m => C.Sink ToolOutput m Bool
--isConfigError = CL.foldM (\a b -> return $ a || isCErr b) False
--    where
--    isCErr (ToolError str) = str1 `T.isInfixOf` str || str2 `T.isInfixOf` str || str3 `T.isInfixOf` str
--    isCErr _ = False
--    str1 = __ "Run the 'configure' command first"
--    str2 = __ "please re-configure"
--    str3 = __ "cannot satisfy -package-id"

data ReloadState = ReloadRunning | ReloadInterrupting | ReloadComplete deriving (Eq, Show)

buildPackage :: Bool -> Bool -> Bool -> (Project, [IDEPackage]) -> (Bool -> IDEAction) -> IDEAction
buildPackage backgroundBuild jumpToWarnings withoutLinking (project, packages) continuation = do
    liftIO $ debugM "leksah" "buildPackage"
    prefs' <- readIDE prefs
    alreadyRunning <- isRunning
    if alreadyRunning
        then do
            liftIO $ debugM "leksah" "buildPackage interruptBuild"
            interruptBuild
            _ <- delayedBy 100000 (
                    if backgroundBuild
                        then do
                            tb <- readIDE triggerBuild
                            void . liftIO $ tryPutMVar tb ()
                        else
                            buildPackage backgroundBuild jumpToWarnings withoutLinking
                                            (project, packages) continuation)
            return ()
        else do
            when (saveAllBeforeBuild prefs') . void $ fileSaveAll belongsToWorkspace'
            doBuild
  where
    doBuild = catchIDE compile'
        (\(e :: SomeException) -> sysMessage Normal (T.pack $ show e))
    compile' = do
        prefs' <- readIDE prefs
        compile [GHC | native prefs' || (javaScript prefs' && pjIsCabal (pjKey project))]
    compile :: [CompilerFlavor] -> IDEAction
    compile [] = continuation True
    compile (compiler:compilers) =
        runCabalBuild compiler backgroundBuild jumpToWarnings withoutLinking (project, packages) $ \f ->
            when f $ do
                mbURI <- readIDE autoURI
                case mbURI of
                    Just uri -> postSyncIDE . loadOutputUri $ T.unpack uri
                    Nothing  -> return ()
                compile compilers

-- | Guess a build command for a plain-directory ('CustomTool') project that
-- has no explicit build command configured, from marker files in its root.
-- A Rust crate (@Cargo.toml@) builds with @cargo build@.  'Nothing' means we
-- don't know how to build the directory.  An explicitly configured
-- 'pjCustomGhcBuild' always wins.
customBuildCommand :: Project -> IO (Maybe (FilePath, [Text]))
customBuildCommand project = case pjKey project of
    CustomTool p
        | Just cmd <- pjCustomGhcBuild p -> return (Just cmd)
        | otherwise -> do
            let dir = pjCustomDir p
            cargo <- doesFileExist (dir </> "Cargo.toml")
            return $ if cargo then Just ("cargo", ["build"]) else Nothing
    _ -> return Nothing

-- | Build a plain-directory ('CustomTool') project that carries no Haskell
-- packages — e.g. a Rust crate added via \"Open Folder\".  Uses the command
-- guessed by 'customBuildCommand', run through the project's nix env (so
-- @cargo@ comes from the flake dev shell) like the other tools; output goes to
-- the build log.  This is the fallback the toolbar/menu Build takes when there
-- is no active Haskell package.
buildCustomProject :: ProjectAction
buildCustomProject = interruptSaveAndRun $ do
    project <- ask
    liftIDE $ do
        let dir = pjDir $ pjKey project
        liftIO (customBuildCommand project) >>= \case
            Nothing -> ideMessage Normal $
                __ "Don't know how to build " <> T.pack dir
            Just (cmd, args) ->
                (`catchIDE` (\(e :: SomeException) -> ideMessage High . T.pack $ show e)) $ do
                    showDefaultLogLaunch'
                    -- cargo/rustc need their own diagnostic parser; the GHC one
                    -- mis-tags rustup/progress output and inflates the count.
                    let logParser =
                            if takeFileName cmd == "cargo"
                                then void $ logOutputForCargoBuild project (LogProject dir) False
                                else void $ logOutputForBuild project (LogProject dir) False False
                    withToolCommand project GHC (Just (cmd, args)) $ \(cmd', args', nixEnv') ->
                        runExternalTool' (__ "Building") cmd' args' dir (M.toList <$> nixEnv') $
                            void . C.getZipSink $ const
                                <$> C.ZipSink sinkLast
                                <*> C.ZipSink logParser

#if defined(MIN_VERSION_unix) && !defined(ghcjs_HOST_OS)
killProcess :: ProcessHandle -> IO ()
killProcess ph =
  -- The process (and hence its group) may have already exited between our
  -- decision to kill it and this call — getProcessGroupIDOf/signalProcessGroup
  -- then throw "does not exist (No such process)".  That's benign (it's already
  -- gone), but as an uncaught IOException it would tear down whatever thread ran
  -- us, so swallow it.
  (`catch` \(_ :: IOException) -> return ()) $
  withProcessHandle ph $ \case
      OpenHandle pid -> signalProcessGroup sigKILL =<< getProcessGroupIDOf pid
      _ -> return ()
#else
killProcess = terminateProcess
#endif

packageDoc :: PackageAction
packageDoc = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ liftIDE $ packageDoc' False True (project, [package]) (\ _ -> return ())

packageDoc' :: Bool -> Bool -> (Project, [IDEPackage]) -> (Bool -> IDEAction) -> IDEAction
packageDoc' backgroundBuild jumpToWarnings (project, packages) continuation =
    catchIDE (do
        let dir = pjDir $ pjKey project
            mbCmdAndArgs = case pjKey project of
                StackTool {} -> Just ("stack", ["haddock", "--no-haddock-deps"] <> map ipdPackageName packages <> concatMap ipdHaddockFlags packages)
                CabalTool {} -> Just ("cabal", ["new-haddock"] <> map ipdPackageName packages)
                _ -> Nothing
        withToolCommand project GHC mbCmdAndArgs $ \(cmd, args, nixEnv') ->
            runExternalTool' (__ "Documenting") cmd
                args dir (M.toList <$> nixEnv') $ do
                mbLastOutput <- C.getZipSink $ const <$> C.ZipSink sinkLast <*> (C.ZipSink $
                    logOutputForBuild project (LogProject dir) backgroundBuild jumpToWarnings)
                lift $ postAsyncIDE reloadDoc
                lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess)))
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

packageClean :: PackageAction
packageClean = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ liftIDE $ packageClean' (project, [package]) (\ _ -> return ())

packageClean' :: (Project, [IDEPackage]) -> (Bool -> IDEAction) -> IDEAction
packageClean' (project, packages) continuation = do
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'

    let dir = pjDir $ pjKey project
    case pjKey project of
        CabalTool _ -> do
            cleanCabal (cabalBuildDir Nothing)
            cleanCabal (cabalBuildDir (Just "js-unknown-ghcjs"))
            continuation True
        StackTool _ ->
            runExternalTool' (__ "Cleaning")
                            "stack"
                            ["clean"]
                            dir Nothing $ do
                mbLastOutput <- C.getZipSink $ const <$> C.ZipSink sinkLast <*> C.ZipSink (logOutput logLaunch)
                lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess))
        _ -> continuation True
  where
    cleanCabal buildDir = forM_ packages $ \package -> do
        (buildDir', _, _) <- liftIO $ cabalProjectBuildDir (pjDir $ pjKey project) buildDir
        let packageBuildDir = buildDir'
                        </> T.unpack (packageIdentifierToString $ ipdPackageId package)
        liftIO $ doesDirectoryExist packageBuildDir >>= \case
            True -> removeDirectoryRecursive packageBuildDir
            False -> return ()

packageInstall :: PackageAction
packageInstall = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ liftIDE $ packageInstall' (project, [package]) (\ _ -> return ())

packageInstall' :: (Project, [IDEPackage]) -> (Bool -> IDEAction) -> IDEAction
packageInstall' (project, packages) continuation = do
    prefs'     <- readIDE prefs
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'

    if native prefs' && pjIsStack (pjKey project)
        then catchIDE (do
            let dir = pjDir $ pjKey project
                (cmd, args) = case pjKey project of
                    StackTool p -> ("stack", "install" : "--stack-yaml" : T.pack (makeRelative dir (pjStackFile p)) : concatMap ipdInstallFlags packages)
                    _ -> ("echo", ["TODO run cabal new-install"])
            runExternalTool' (__ "Installing") cmd (args ++ concatMap ipdInstallFlags packages) dir Nothing $ do
                    mbLastOutput <- C.getZipSink $ (const <$> C.ZipSink sinkLast) <*> C.ZipSink (logOutput logLaunch)
                    lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess)))
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)
        else continuation True

packagesEnv :: MonadIO m => [IDEPackage] -> [(String, String)] -> m [(String, String)]
packagesEnv packages env =
    return $ map (\package -> (T.unpack (ipdPackageName package) <> "_datadir", ipdPackageDir package)) packages <> env

packageRun' :: Maybe (PackageM Bool) -> PackageAction
packageRun' removeGhcjsFlagIfPresent = do
    project <- lift ask
    package <- ask
    case removeGhcjsFlagIfPresent of
        Just promptUser | "--ghcjs" `elem` ipdConfigFlags package ->
            promptUser >>= \case
                True -> do
                    let packWithNewFlags = package { ipdConfigFlags = filter (/="--ghcjs") $ ipdConfigFlags package }
                    liftIDE $ changePackage packWithNewFlags
                    lift $ State.runPackage (packageRun' Nothing) packWithNewFlags
                False -> return ()
        _ -> liftIDE $ catchIDE (do
            pd <- readAndFlattenPackageDescription package
            mbComponent <- readIDE activeComponent
            let exe = exeToRun mbComponent $ executables pd
            let defaultLogName = ipdPackageName package
                logName' = fromMaybe defaultLogName . listToMaybe $ map (T.pack . unUnqualComponentName . exeName) exe
            (logLaunch,logName) <- buildLogLaunchByName logName'
            showLog
            do
                    let dir = ipdPackageDir package
                    case pjKey project of
                        StackTool {} -> IDE.Package.runPackage (addLogLaunchData logName logLaunch)
                                                   (T.pack $ printf (__ "Running %s") (T.unpack logName))
                                                   "stack"
                                                   (concat [["exec"]
                                                        , ipdBuildFlags package
                                                        , map (T.pack . unUnqualComponentName . exeName) exe
                                                        , ["--"]
                                                        , ipdExeFlags package])
                                                   dir
                                                   Nothing
                                                   (logOutput logLaunch)
                        CabalTool {} -> do
                            (buildDir, cDir, _) <- liftIO $ cabalProjectBuildDir (pjDir $ pjKey project) (cabalBuildDir Nothing)
                            env <- packagesEnv (pjPackages project) =<< liftIO getEnvironment
                            case exe ++ executables pd of
                                [] -> return ()
                                (Executable {exeName = name} : _) -> do
                                    let path' c = buildDir
                                                    </> T.unpack (packageIdentifierToString $ ipdPackageId package)
                                                    </> c </> unUnqualComponentName name </> unUnqualComponentName name
                                    path <- liftIO $ doesFileExist (path' "build") >>= \case
                                        True -> return $ path' "build"
                                        False -> return . path' $ cDir "x" (unUnqualComponentName name)
                                    IDE.Package.runPackage (addLogLaunchData logName logLaunch)
                                                           (T.pack $ printf (__ "Running %s") (T.unpack logName))
                                                           path
                                                           (ipdExeFlags package)
                                                           dir
                                                           (Just env)
                                                           (logOutput logLaunch)
                        CustomTool {} -> do
                            ideMessage High "Unable to run package in a custom project"
                            return ()
                        _ -> do
                            ideMessage High "Unable to run package in this project type"
                            return ())
            (\(e :: SomeException) -> ideMessage High (T.pack $ show e))

-- | Is the given executable the active one?
isActiveExe :: Text -> Executable -> Bool
isActiveExe selected Executable {exeName = name} = selected == "exe:" <> T.pack (unUnqualComponentName name)

-- | get executable to run
--   no exe activated, take first one
exeToRun :: Maybe Text -> [Executable] -> [Executable]
exeToRun Nothing (exe:_) = [exe]
exeToRun Nothing _ = []
exeToRun (Just selected) exes = take 1 $ filter (isActiveExe selected) exes

packageRunJavaScript' :: Maybe (PackageM Bool) -> PackageAction
packageRunJavaScript' addFlagIfMissing = do
    project <- lift ask
    package <- ask
    if pjIsStack (pjKey project)
        then ideMessage Normal (__ "Leksah does not know how to run stack.yaml projects built with GHCJS.  Please use a cabal.project file instead (or send a pull request to fix Leksah).")
        else do
            prefs' <- readIDE prefs
            case addFlagIfMissing of
                Just promptUser | not (javaScript prefs') ->
                    promptUser >>= \case
                        True -> liftIDE $
                            buildPackage False False False (project, [package]) $ \ ok -> when ok $
                                readIDE workspace >>= mapM_ (runWorkspace $ runProject (State.runPackage (packageRunJavaScript' Nothing) package) project)
                        False  -> return ()
                _ -> liftIDE $ buildPackage False False True (project, [package]) $ \ ok -> when ok $ liftIDE $ catchIDE (do
                        pd <- readAndFlattenPackageDescription package
                        mbComponent <- readIDE activeComponent
                        let exe = exeToRun mbComponent $ executables pd
--                            defaultLogName = ipdPackageName package
--                            logName = fromMaybe defaultLogName . listToMaybe $ map (T.pack . unUnqualComponentName . exeName) exe
--                            dir = ipdPackageDir package
--                            projectRoot = pjDir project
                        case exe ++ executables pd of
                            (Executable {exeName = name} : _) -> liftIDE $ do
                                (buildDir, cDir, _) <- liftIO $ cabalProjectBuildDir (pjDir $ pjKey project) (cabalBuildDir (Just "js-unknown-ghcjs"))
                                let path' c = buildDir
                                            </> T.unpack (packageIdentifierToString $ ipdPackageId package)
                                            </> c </> unUnqualComponentName name </> unUnqualComponentName name <.> "jsexe" </> "index.html"

                                path <- liftIO $ doesFileExist (path' "build") >>= \case
                                    True -> return $ path' "build"
                                    False -> return . path' $ cDir "x" (unUnqualComponentName name)

                                postAsyncIDE $ do
                                    loadOutputHtmlFile path
                                    showOutputPane
                              `catchIDE`
                                (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

                            _ -> return ())
                        (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

packageTest :: PackageAction
packageTest = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ liftIDE $ packageTest' False True (project, [package]) (\ _ -> return ())

packageTest' :: Bool -> Bool -> (Project, [IDEPackage]) -> (Bool -> IDEAction) -> IDEAction
packageTest' _ _ (_, []) continuation = continuation True
packageTest' backgroundBuild jumpToWarnings (project, package:rest) continuation =
    if "--enable-tests" `elem` ipdConfigFlags package
        then do
            removeTestLogRefs (LogCabal $ ipdCabalFile package)
            do
                    pd <- readAndFlattenPackageDescription package
                    runTests $ testSuites pd
          `catchIDE`
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)
        else packageTest' backgroundBuild jumpToWarnings (project, rest) continuation
  where
    runTests :: [TestSuite] -> IDEAction
    runTests [] = packageTest' backgroundBuild jumpToWarnings (project, rest) continuation
    runTests (test:tests) =
        packageRunComponent (CTest test) backgroundBuild jumpToWarnings (project, package) (\ok ->
            when ok $ runTests tests)

packageRunComponent :: Component -> Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
packageRunComponent (CLib _) _ _ _ _ = error "packageRunComponent"
packageRunComponent component backgroundBuild jumpToWarnings (project, package) continuation = do
    let (_cType, name, command) = case component of
                    CExe exe -> ("x" :: String, exeName exe, "run")
                    CTest test -> ("t", testName test, "test")
                    CBench bench -> ("b", benchmarkName bench, "bench")
                    CFLib flib -> ("f", foreignLibName flib, "flib") -- TODO check if "f" is correct
        dir = ipdPackageDir package
    showDefaultLogLaunch'
    catchIDE (do
--        let projectFile = pjFile project
--        ghcVersion <- liftIO getDefaultGhcVersion
--        packageDBs <- liftIO $ getPackageDBs' ghcVersion (Just projectFile)
--        let pkgId = packageIdentifierToString $ ipdPackageId package
        let pkgName = ipdPackageName package
        pjFileArgs <- projectFileArguments project dir
        let mbCmdAndArgs = case pjKey project of
                        StackTool {} -> Just ("stack", [command] <> pjFileArgs <> [pkgName <> ":" <> T.pack (unUnqualComponentName name)])
                        CabalTool {} -> Just ("cabal", ["new-" <> command] <> pjFileArgs <> [pkgName <> ":" <> T.pack (unUnqualComponentName name)])
                        CustomTool {} -> Nothing
                        NixTool {} -> Nothing
                        MakeTool {} -> Nothing
            mbCmdAndArgs' = second (<> ipdTestFlags package) <$> mbCmdAndArgs
        withToolCommand project GHC mbCmdAndArgs' $ \(cmd, args', nixEnv') ->
            runExternalTool' (__ "Run " <> T.pack (unUnqualComponentName name))
                    cmd args' dir (M.toList <$> nixEnv') $ do
                    (mbLastOutput, _) <- C.getZipSink $ (,)
                        <$> C.ZipSink sinkLast
                        <*> (C.ZipSink $ logOutputForBuild project (LogCabal $ ipdCabalFile package) backgroundBuild jumpToWarnings)
                    lift $
--                        errs <- readIDE errorRefs
                        when (mbLastOutput == Just (ToolExit ExitSuccess)) $ continuation True)
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

-- | Run benchmarks as foreground action for current package
packageBench :: PackageAction
packageBench = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ liftIDE $ packageBench' False True (project, [package]) (\ _ -> return ())

-- | Run benchmarks
packageBench' :: Bool -> Bool -> (Project, [IDEPackage]) -> (Bool -> IDEAction) -> IDEAction
packageBench' _ _ (_, []) continuation = continuation True
packageBench' backgroundBuild jumpToWarnings (project, package:rest) continuation =
    if "--enable-benchmarks" `elem` ipdConfigFlags package
        then do
            pd <- readAndFlattenPackageDescription package
            runBenchs $ benchmarks pd
          `catchIDE`
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)
        else packageBench' backgroundBuild jumpToWarnings (project, rest) continuation
  where
    runBenchs :: [Benchmark] -> IDEAction
    runBenchs [] = packageBench' backgroundBuild jumpToWarnings (project, rest) continuation
    runBenchs (bench:benches) =
        packageRunComponent (CBench bench) backgroundBuild jumpToWarnings (project, package) (\ok ->
            when ok $ runBenchs benches)

packageSdist :: PackageAction
packageSdist = do
    package <- ask
    interruptSaveAndRun $ do
        logLaunch <- getDefaultLogLaunch
        showDefaultLogLaunch'

        catchIDE (do
            let dir = ipdPackageDir package
            runExternalTool' (__ "Source Dist") "cabal" ("sdist" : ipdSdistFlags package) dir Nothing (logOutput logLaunch))
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)


-- | Open generated documentation for package
packageOpenDoc :: PackageAction
packageOpenDoc = do
    project <- lift ask
    package <- ask
    let dir = ipdPackageDir package
        pkgId = packageIdentifierToString $ ipdPackageId package
--        projectRoot = pjDir project
    mbDistDir <- case pjKey project of
        StackTool {} -> do
            --ask stack where its dist directory is
            mvar <- liftIO newEmptyMVar
            runExternalTool' "" "stack" ["path"] dir Nothing $ do
                output <- CL.consume
                liftIO . putMVar mvar $ listToMaybe $ mapMaybe getDistOutput output
            liftIO $ takeMVar mvar
        CabalTool {} -> do
            (buildDir, _, _) <- liftIO $ cabalProjectBuildDir (pjDir $ pjKey project) (cabalBuildDir Nothing)
            return . Just $ buildDir </> T.unpack pkgId
        _ -> return Nothing
    case mbDistDir of
      Nothing -> ideMessage High "Open documentation not supported for custom projects"
      Just distDir ->
        liftIDE $ do
            let path = dir </> distDir
                            </> "doc/html"
                            </> T.unpack (ipdPackageName package)
                            </> "index.html"
            loadDoc . T.pack $ "file://" ++ path
            showDocumentationPane
          `catchIDE`
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)
  where
    -- get dist directory from stack path output
    getDistOutput (ToolOutput o) | Just t<-T.stripPrefix "dist-dir:" o = Just $ dropWhile isSpace $ T.unpack t
    getDistOutput _ = Nothing

runPackage ::  (ProcessHandle -> IDEAction)
            -> Text
            -> FilePath
            -> [Text]
            -> FilePath
            -> Maybe [(String,String)]
            -> ConduitT ToolOutput Void IDEM ()
            -> IDEAction
runPackage = runExternalTool (return True) -- TODO here one could check if package to be run is building/configuring/etc atm

-- ---------------------------------------------------------------------
-- | * Utility functions/procedures, that have to do with packages
--

getPackageDescriptionAndPath :: IDEM (Maybe (PackageDescription,FilePath))
getPackageDescriptionAndPath = do
    active <- readIDE activePack
    case active of
        Nothing -> do
            ideMessage Normal (__ "No active package")
            return Nothing
        Just p  -> catchIDE (do
                pd <- liftIO $ readGPD normalVerbosity (ipdCabalFile p)
                return (Just (flattenPackageDescription pd,ipdCabalFile p)))
                    (\(e :: SomeException) -> do
                        ideMessage Normal (__ "Can't load package " <> T.pack (show e))
                        return Nothing)

getEmptyModuleTemplate :: PackageDescription -> Text -> IO Text
getEmptyModuleTemplate pd modName = getModuleTemplate "module" pd modName "" ""

getModuleTemplate :: FilePath -> PackageDescription -> Text -> Text -> Text -> IO Text
getModuleTemplate templateName pd modName exports body = catch (do
    dataDir  <- getDataDir
    filePath <- getConfigFilePathForLoad (templateName <> leksahTemplateFileExtension) Nothing dataDir
    template <- T.readFile filePath
    return (foldl' (\ a (from, to') -> T.replace from to' a) template
        [   ("@License@"      , (T.pack .
#if MIN_VERSION_Cabal(2,2,0)
                                          prettyShow
#else
                                          display
#endif
                                                  . license) pd)
        ,   ("@Maintainer@"   , T.pack . fromShortText $ maintainer pd)
        ,   ("@Stability@"    , T.pack . fromShortText $ stability pd)
        ,   ("@Portability@"  , "")
        ,   ("@Copyright@"    , T.pack . fromShortText $ copyright pd)
        ,   ("@ModuleName@"   , modName)
        ,   ("@ModuleExports@", exports)
        ,   ("@ModuleBody@"   , body)]))
                    (\ (e :: SomeException) -> do
                        sysMessage Normal . T.pack $ printf (__ "Couldn't read template file: %s") (show e)
                        return "")

data ModuleLocation = LibExposedMod | LibOtherMod | ExeOrTestMod Text

addModuleToPackageDescr :: ModuleName -> [ModuleLocation] -> PackageAction
addModuleToPackageDescr moduleName locations = do
    p    <- ask
    liftIDE $ catchIDE (liftIO $ do
        gpd <- readGPD normalVerbosity (ipdCabalFile p)
        let npd = trace (show gpd) foldr addModule gpd locations
        writeGenericPackageDescription' (ipdCabalFile p) npd)
           (\(e :: SomeException) -> do
            ideMessage Normal (__ "Can't update package " <> T.pack (show e))
            return ())
  where
    addModule LibExposedMod gpd@GenericPackageDescription{condLibrary = Just lib} =
        gpd {condLibrary = Just (addModToLib moduleName lib)}
    addModule LibOtherMod gpd@GenericPackageDescription{condLibrary = Just lib} =
        gpd {condLibrary = Just (addModToBuildInfoLib moduleName lib)}
    addModule (ExeOrTestMod name') gpd = let name = mkUnqualComponentName (T.unpack name') in gpd {
          condExecutables = map (addModToBuildInfoExe  name moduleName) (condExecutables gpd)
        , condTestSuites  = map (addModToBuildInfoTest name moduleName) (condTestSuites gpd)
        }
    addModule _ x = x

-- Cabal-syntax 3.17 (stable-haskell fork) dropped CondTree's middle
-- (aggregated-constraints) type parameter: CondTree v c a -> CondTree v a.
-- This synonym keeps the signatures below working on both APIs (the term-level
-- CondNode{condTreeData=..} access is unaffected).
#if MIN_VERSION_Cabal(3,17,0)
type CondTreeCV = CondTree ConfVar
#else
type CondTreeCV a = CondTree ConfVar [Dependency] a
#endif

addModToLib :: ModuleName -> CondTreeCV Library ->
    CondTreeCV Library
addModToLib modName ct@CondNode{condTreeData = lib} =
    ct{condTreeData = lib{exposedModules = modName `inOrderAdd` exposedModules lib}}

addModToBuildInfoLib :: ModuleName -> CondTreeCV Library ->
    CondTreeCV Library
addModToBuildInfoLib modName ct@CondNode{condTreeData = lib} =
    ct{condTreeData = lib{libBuildInfo = (libBuildInfo lib){otherModules = modName
        `inOrderAdd` otherModules (libBuildInfo lib)}}}

addModToBuildInfoExe :: UnqualComponentName -> ModuleName -> (UnqualComponentName, CondTreeCV Executable) ->
    (UnqualComponentName, CondTreeCV Executable)
addModToBuildInfoExe name modName (str,ct@CondNode{condTreeData = exe}) | str == name =
    (str, ct{condTreeData = exe{buildInfo = (buildInfo exe){otherModules = modName
        `inOrderAdd` otherModules (buildInfo exe)}}})
addModToBuildInfoExe _name _ x = x

addModToBuildInfoTest :: UnqualComponentName -> ModuleName -> (UnqualComponentName, CondTreeCV TestSuite) ->
    (UnqualComponentName, CondTreeCV TestSuite)
addModToBuildInfoTest name modName (str,ct@CondNode{condTreeData = test}) | str == name =
    (str, ct{condTreeData = test{testBuildInfo = (testBuildInfo test){otherModules = modName
        `inOrderAdd` otherModules (testBuildInfo test)}}})
addModToBuildInfoTest _ _ x = x

inOrderAdd :: Ord a => a -> [a] -> [a]
inOrderAdd a list = let (before, after) = span (< a) list in before ++ [a] ++ after

--------------------------------------------------------------------------
delModuleFromPackageDescr :: ModuleName -> PackageAction
delModuleFromPackageDescr moduleName = do
    p    <- ask
    liftIDE $ catchIDE (liftIO $ do
        gpd <- readGPD normalVerbosity (ipdCabalFile p)
        let isExposedAndJust = isExposedModule moduleName (condLibrary gpd)
        let npd = if isExposedAndJust
                then gpd{
                    condLibrary = Just (delModFromLib moduleName
                                                (fromJust (condLibrary gpd))),
                    condExecutables = map (delModFromBuildInfoExe moduleName)
                                            (condExecutables gpd)}
                else gpd{
                    condLibrary = case condLibrary gpd of
                                    Nothing -> Nothing
                                    Just _lib -> Just (delModFromBuildInfoLib moduleName
                                                       (fromJust (condLibrary gpd))),
                    condExecutables = map (delModFromBuildInfoExe moduleName)
                                                (condExecutables gpd)}
        writeGenericPackageDescription' (ipdCabalFile p) npd)
           (\(e :: SomeException) -> do
            ideMessage Normal (__ "Can't update package " <> T.pack (show e))
            return ())

delModFromLib :: ModuleName -> CondTreeCV Library ->
    CondTreeCV Library
delModFromLib modName ct@CondNode{condTreeData = lib} =
    ct{condTreeData = lib{exposedModules = delete modName (exposedModules lib)}}

delModFromBuildInfoLib :: ModuleName -> CondTreeCV Library ->
    CondTreeCV Library
delModFromBuildInfoLib modName ct@CondNode{condTreeData = lib} =
    ct{condTreeData = lib{libBuildInfo = (libBuildInfo lib){otherModules =
        delete modName (otherModules (libBuildInfo lib))}}}

delModFromBuildInfoExe :: ModuleName -> (UnqualComponentName, CondTreeCV Executable) ->
    (UnqualComponentName, CondTreeCV Executable)
delModFromBuildInfoExe modName (str,ct@CondNode{condTreeData = exe}) =
    (str, ct{condTreeData = exe{buildInfo = (buildInfo exe){otherModules =
        delete modName (otherModules (buildInfo exe))}}})

isExposedModule :: ModuleName -> Maybe (CondTreeCV Library)  -> Bool
isExposedModule _ Nothing                              = False
isExposedModule mn (Just CondNode{condTreeData = lib}) = mn `elem` exposedModules lib


-- Includes non buildable
allBuildInfo' :: PackageDescription -> [BuildInfo]
allBuildInfo' pkg_descr = [ libBuildInfo lib       | Just lib <- [library pkg_descr] ]
                       ++ [ libBuildInfo lib       | lib <- subLibraries pkg_descr ]
                       ++ [ buildInfo exe          | exe <- executables pkg_descr ]
                       ++ [ testBuildInfo tst      | tst <- testSuites pkg_descr ]
                       ++ [ benchmarkBuildInfo tst | tst <- benchmarks pkg_descr ]


--testMainPath :: TestSuiteInterface -> [FilePath]
--testMainPath (TestSuiteExeV10 _ f) = [f]
--testMainPath _ = []

idePackageFromPath' :: FilePath -> IDEM (Maybe IDEPackage)
idePackageFromPath' cabalFile = do
    mbGPD <- catchIDE (liftIO $ Just <$> readGPD normalVerbosity cabalFile)
            (\ (e :: SomeException) -> do
                ideMessage Normal (__ "Can't activate package " <> T.pack (show e))
                return Nothing)
    case mbGPD of
        Nothing  -> return Nothing
        Just gpd -> idePackageFromGPD cabalFile gpd

-- | Build the 'IDEPackage' from an already-parsed .cabal ('ideProjectFromKey'
-- parses remote packages from snapshot bytes — no per-package file reads).
idePackageFromGPD :: FilePath -> GenericPackageDescription -> IDEM (Maybe IDEPackage)
idePackageFromGPD ipdCabalFile gpd = do
        let packageD = flattenPackageDescription gpd
        do
            let ipdModules          = M.fromList $ myLibModules packageD ++ myExeModules packageD
                                        ++ myTestModules packageD ++ myBenchmarkModules packageD
                ipdMain             = [ (mainPath (modulePath exe), buildInfo exe, False) | exe <- executables packageD ]
                                        ++ [ (mainPath f, bi, True) | TestSuite {testInterface = TestSuiteExeV10 _ f, testBuildInfo = bi} <- testSuites packageD ]
                                        ++ [ (mainPath f, bi, True) | Benchmark {benchmarkInterface = BenchmarkExeV10 _ f, benchmarkBuildInfo = bi} <- benchmarks packageD ]
                ipdExtraSrcs        = S.fromList $ map mainPath $ extraSrcFiles packageD
                ipdSrcDirs          = case nub $ concatMap hsSourceDirs (allBuildInfo' packageD) of
                                            [] -> [".","src"]
                                            l -> map getSymbolicPath l
                ipdSubLibraries     = [ T.pack . unUnqualComponentName $ e | Just e <- libraryNameString . libName <$> subLibraries packageD ]
                ipdExes             = [ T.pack . unUnqualComponentName $ exeName e | e <- executables packageD ]
                ipdExtensions       = nub $ concatMap oldExtensions (allBuildInfo' packageD)
                ipdTests            = [ T.pack . unUnqualComponentName $ testName t | t <- testSuites packageD ]
                ipdBenchmarks       = [ T.pack . unUnqualComponentName $ benchmarkName b | b <- benchmarks packageD ]
                ipdPackageId        = package packageD
                ipdDepends          = allBuildDepends packageD
                ipdHasLib           = hasLibs packageD
                ipdConfigFlags      = ["--enable-tests"]
                ipdBuildFlags       = []
                ipdTestFlags        = []
                ipdBenchmarkFlags        = []
                ipdHaddockFlags     = []
                ipdExeFlags         = []
                ipdInstallFlags     = []
                ipdRegisterFlags    = []
                ipdUnregisterFlags  = []
                ipdSdistFlags       = []
                packp               = IDEPackage {..}
            return (Just packp)

-- | Write a GenericPackageDescription, stripping the trailing spaces
-- Cabal's pretty-printer leaves at line ends (noise in diffs).
writeGenericPackageDescription' :: FilePath -> GenericPackageDescription -> IO ()
writeGenericPackageDescription' fp =
    writeUTF8File fp
        . unlines . map (dropWhileEnd (== ' ')) . lines
        . showGenericPackageDescription

extractStackPackageList :: Text -> [String]
extractStackPackageList = (\x -> if null x then ["."] else x) .
                          map (stripQuotes . T.unpack . (\x -> fromMaybe x $ T.stripPrefix "location: " x)) .
                          filterSimple .
                          filter (not . T.null) .
                          map (T.reverse . T.dropWhile isSpace . T.reverse) .
                          drop 1 .
                          dropWhile (/= "packages:") .
                          map (T.pack . stripStackComments . T.unpack) .
                          T.lines
  where
    stripQuotes ('\'':rest) | take 1 (reverse rest) == "\'" = init rest
    stripQuotes x = x

    stripStackComments :: String -> String
    stripStackComments "" = ""
    stripStackComments ('#':_) = ""
    stripStackComments (x:xs) = x:stripStackComments xs

    filterSimple [] = []
    filterSimple (x:xs) = let indent = T.takeWhile (==' ') x in
                          mapMaybe (T.stripPrefix (indent <> "- ")) $
                          takeWhile (\l -> (indent <> "- ") `T.isPrefixOf` l || (indent <> " ") `T.isPrefixOf` l) (x:xs)

-- This started out really small...
-- TODO replace with code from cabal-install (currently not exposed)
extractCabalPackageList :: Text -> [String]
extractCabalPackageList =
    extractList "packages:" <> extractList "optional-packages:"
  where
    extractList :: Text -> Text -> [String]
    extractList listName =
        map dirOnly .
        filter (not . null) .
        (>>= words) .
        map (T.unpack . T.dropWhile (==' ')) .
        takeWhile (" " `T.isPrefixOf`) .
        dropListName .
        dropWhile (not . (listName `T.isPrefixOf`)) .
        filter (not . T.null) .
        map (T.pack . stripCabalComments . T.unpack) .
        T.lines
      where
        -- This function makes `packages: x` on one line work like
        -- packages:
        --   x
        -- Using fromJust here to get a better error if somehow listName is not a prefix (should never happen).
        dropListName [] = []
        dropListName (x:xs) = " " <> fromJust (T.stripPrefix listName x) : xs

    stripCabalComments :: String -> String
    stripCabalComments "" = ""
    stripCabalComments ('-':'-':_) = ""
    stripCabalComments (x:xs) = x:stripCabalComments xs
    dirOnly :: FilePath -> FilePath
    dirOnly f = if takeExtension f == ".cabal"
      then (if null (dropFileName f) then "./" else dropFileName f)
      else f

ideProjectFromKey :: ProjectKey -> IDEM (Maybe Project)
ideProjectFromKey key = do
--    case (case takeExtension filePath of
--                ".project" -> Just (CabalTool (CabalProject filePath), extractCabalPackageList)
--                ".yaml" -> Just (StackTool (StackProject filePath), extractStackPackageList)
--                _ -> Nothing) of
--        Just (key, extractPackageList) -> do
#if defined(ghcjs_HOST_OS)
            -- Browser demo: no Glob over a real file system — take every
            -- .cabal file under the project directory from the page-seeded
            -- mock tree (the demo tree is tiny, so the packages: globs are
            -- not interpreted).  Paths in the tree are already absolute.
            let dir = pjDir key
            cabalFiles <- liftIO $ filter ((== ".cabal") . takeExtension)
                              <$> fsListFilesRecursive dir
            packages <- fmap catMaybes . mapM idePackageFromPath' $ nub cabalFiles
#else
            packages <- case parseRemotePath (pjFileOrDir key) of
              Just (host, _) -> do
                -- Remote project: 2 round trips — read the project file,
                -- then one batched snapshot streaming back every matched
                -- .cabal (and sibling .lkshf) as bytes; the packages parse
                -- locally from those bytes with no further remote reads.
                let rdir = maybe (pjDir key) snd (parseRemotePath (pjDir key))
                pkgDirs <- case key of
                    CabalTool (CabalProject filePath) ->
                        extractCabalPackageList . decodeUtf8 <$> liftIO (fsReadFile filePath)
                    StackTool (StackProject filePath) ->
                        extractStackPackageList . decodeUtf8 <$> liftIO (fsReadFile filePath)
                    _ -> return []
                if null pkgDirs then return [] else do
                    entries <- nubBy ((==) `on` seLocalPath)
                        <$> liftIO (remoteCabalSnapshot host rdir pkgDirs)
                    let cabalEntries =
                            [ e | e <- entries
                            , takeExtension (seLocalPath e) == ".cabal" ]
                    fmap catMaybes . forM cabalEntries $ \e -> do
                        let cabalFile = renderRemotePath host (seLocalPath e)
                        mbGpd <- catchIDE (liftIO $ Just <$> gpdFromBytes cabalFile (seBytes e))
                            (\(e' :: SomeException) -> do
                                ideMessage Normal (__ "Can't activate package " <> T.pack (show e'))
                                return Nothing)
                        case mbGpd of
                            Nothing  -> return Nothing
                            Just gpd -> idePackageFromGPD cabalFile gpd
              Nothing -> do
                patterns <- liftIO $ map (Glob.compile . (</> "*.cabal")) <$>
                    case key of
                        CabalTool (CabalProject filePath) -> extractCabalPackageList <$> T.readFile filePath
                        StackTool (StackProject filePath) -> extractStackPackageList <$> T.readFile filePath
                        CustomTool _ -> return []
                        -- A flake project has no cabal packages; its tree shows the
                        -- flake outputs + files instead (see IDE.Web.Widget.Flake).
                        NixTool _ -> return []
                        -- A Makefile project builds with make, not cabal.
                        MakeTool _ -> return []
                let dir = pjDir key
                cabalFiles <- liftIO $ mapM canonicalizePath =<< map (dir </>) . concat <$>
                                  Glob.globDir patterns dir
                fmap catMaybes . mapM idePackageFromPath' $ nub cabalFiles
#endif
            return . Just $ Project { pjKey = key, pjPackageMap = mkPackageMap packages }
          `catchIDE`
             (\(e :: SomeException) -> do
                ideMessage Normal . T.pack $ show e
                return Nothing)
--        Nothing -> return Nothing

--refreshPackage :: C.Sink ToolOutput IDEM () -> PackageM (Maybe IDEPackage)
--refreshPackage log' = do
--    package <- ask
--    liftIDE $ do
--        mbUpdatedPack <- idePackageFromPath log' (ipdCabalFile package)
--        case mbUpdatedPack of
--            Just updatedPack -> do
--                changePackage updatedPack
--                return mbUpdatedPack
--            Nothing -> do
--                postAsyncIDE $ ideMessage Normal (__ "Can't read package file")
--                return Nothing

