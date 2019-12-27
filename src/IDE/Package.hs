{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
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

,   debugStart
,   printBindResultFlag
,   breakOnErrorFlag
,   breakOnExceptionFlag

,   printEvldWithShowFlag
,   tryDebug'
,   tryDebugQuiet
,   executeDebugCommand

,   idePackageFromPath'
,   ideProjectFromKey
,   writeGenericPackageDescription'

,   runPackage
,   getActiveComponent
,   projectFileArguments
,   exeToRun
,   printf
,   interactiveFlags
,   ghciFork
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
import Distribution.Verbosity

import System.FilePath
import System.Directory
       (createDirectoryIfMissing, removeDirectoryRecursive,
        canonicalizePath, setCurrentDirectory, doesFileExist,
        doesDirectoryExist)
import qualified Data.Set as S (fromList)
import Data.Either (isRight)
import Data.Map (Map)
import System.Exit (ExitCode(..))
import Control.Applicative ((<$>), (<*>))
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
import Control.Exception (SomeException(..), catch)

import qualified IDE.Core.State as State (runPackage)
import IDE.Core.State
       (packageDebugState, debugState, pjPackages, changePackage,
        ipdPackageDir, PackageM, runProject, runWorkspace, debug,
        autoCommand, isError, runningTool, nixEnv, useVado,
        nixCache, modifyIDE_, pjDir, javaScript, ProjectAction,
        ipdPackageName, triggerEventIDE_, mkPackageMap, reflectIDEI,
        runDebug, lookupDebugState, printBindResult, breakOnError,
        breakOnException, printEvldWithShow, sysMessage, getDataDir,
        catchIDE, MessageLevel(..), ideMessage, activeComponent,
        activeProject, wsName, workspace, activePack, readIDE, DebugAction,
        Prefs, PackageAction, IDEM, IDEAction, IDEPackage(..), Project(..),
        MonadIDE, __, prefs, saveAllBeforeBuild, triggerBuild, native,
        packageIdentifierToString, leksahTemplateFileExtension,
        leksahFlagFileExtension, Log(..), StatusbarCompartment(..),
        MonadIDE(..), IDEEvent(..), SensitivityMask(..), DebugState(..),
        ProjectKey(..), autoURI, pDBsPaths, errorRefs, reflectIDE,
        StackProject(..), CabalProject(..), pjKey, pjIsCabal, pjIsStack,
        CustomProject(..))
import IDE.Gtk.State (postSyncIDE, postAsyncIDE, delayedBy)
import IDE.Utils.CabalUtils (writeGenericPackageDescription')
import IDE.Pane.Log
       (addLogLaunchData, showLog, buildLogLaunchByName,
        showDefaultLogLaunch', getDefaultLogLaunch)
import IDE.Pane.SourceBuffer
       (removeTestLogRefs, fileSaveAll, belongsToWorkspace')
import IDE.PackageFlags (writeFlags, readFlags)
import IDE.Utils.FileUtils
       (getPackageDBs', cabalProjectBuildDir, loadNixCache, saveNixCache,
        getConfigDir, nixShellFile, getConfigFilePathForLoad)
import IDE.LogRef
       (logIdleOutput, logOutputForBuild, logOutputDefault, logOutput)
import Distribution.ModuleName (ModuleName(..))
import Data.List
       (intercalate, nub, foldl', delete)
import IDE.Utils.Tool
       (toolProcess, ToolOutput(..), newGhci, ToolState(..),
        ProcessHandle, executeGhciCommand, interruptTool,
        isToolPrompt)
import IDE.Pane.WebKit.Documentation
       (showDocumentationPane, loadDoc, reloadDoc)
import IDE.Pane.WebKit.Output
       (loadOutputUri, loadOutputHtmlFile, showOutputPane)
import System.Log.Logger (debugM)
import System.Process.Vado (getMountPoint)
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
import IDE.Metainfo.Provider (updateSystemInfo)
import IDE.Utils.VersionUtils (getDefaultGhcVersion)
import IDE.Utils.CabalProject
       (findProjectRoot)
import System.Environment (getEnvironment)
import Distribution.Simple.LocalBuildInfo
       (Component(..))
import Distribution.Compiler (CompilerFlavor(..))
import qualified Data.Map as M
       (toList, fromList)
import Control.Lens ((.~), (?~), (%~), _Just, to)
import System.Process (getProcessExitCode, showCommandForUser)
#ifdef MIN_VERSION_unix
import System.Posix (sigKILL, signalProcessGroup, getProcessGroupIDOf)
import System.Process.Internals
       (withProcessHandle, ProcessHandle__(..))
#else
import IDE.Utils.Tool (terminateProcess)
#endif
import Distribution.Types.ForeignLib (foreignLibName)
import Distribution.Types.UnqualComponentName
       (UnqualComponentName, mkUnqualComponentName,
        unUnqualComponentName)
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
       (readGenericPackageDescription)
import Distribution.Pretty (prettyShow)
#else
import Distribution.PackageDescription.Parse
       (readGenericPackageDescription)
import Distribution.Text (display)
#endif
import qualified System.FilePath.Glob as Glob (globDir, compile)
#if MIN_VERSION_Cabal(3,0,0)
import Distribution.Types.LibraryName (libraryNameString)
#endif

#if !MIN_VERSION_Cabal(3,0,0)
type LibraryName = Maybe UnqualComponentName
libraryNameString :: LibraryName -> Maybe UnqualComponentName
libraryNameString = id
#endif

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
    oldActivePack <- readIDE activePack
    case mbPath of
        Just p -> liftIO $ setCurrentDirectory (dropFileName p)
        Nothing -> return ()
    when (isJust mbPack || isJust oldActivePack) $
        triggerEventIDE_ (Sensitivity [(SensitivityProjectActive,isJust mbPack)])
    wsStr <- readIDE (workspace . _Just . wsName)
--    let wsStr = case mbWs of
--                    Nothing -> ""
--                    Just ws -> ws ^. wsName
    let txt = case (mbPath, mbPack) of
                    (_, Just pack) -> wsStr <> " > " <> packageIdentifierToString (ipdPackageId pack)
                    (Just path, _) -> wsStr <> " > " <> T.pack (takeFileName path)
                    _ -> wsStr <> ":"
    triggerEventIDE_ (StatusbarChanged [CompartmentPackage txt])

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
    loop [] = liftIDE (updateSystemInfo >> continuation)
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
                              <> "in ({ shells = { ghc = ({ env = x; } // x).env; }; } // x).shells." <> compiler
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
            _ -> loop rest

projectFileArguments :: MonadIO m => Project -> FilePath -> m [Text]
projectFileArguments project dir =
    case pjKey project of
        CabalTool (CabalProject file) -> do
            let projectFile = T.pack $ makeRelative dir file
            defaultProjectRoot <- liftIO $ findProjectRoot dir
            return $ if file /= defaultProjectRoot </> "cabal.project"
                                then [ "--project-file", projectFile ]
                                else []
        StackTool (StackProject file) -> do
            let projectFile = T.pack $ makeRelative dir file
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
withToolCommand project compiler Nothing continuation = ideMessage High $ "withToolCommand failed for " <> T.pack (show $ pjKey project)
withToolCommand project compiler (Just (cmd, args)) continuation = do
    liftIO $ debugM "leksah" $ "withToolCommand " <> show (project, compiler, cmd, args)
    prefs' <- readIDE prefs
    -- Nix cache will not work over vado
    enableNixCache <- if useVado prefs' then liftIO $ isRight <$> getMountPoint (pjDir $ pjKey project) else return True
    nixShellFile (pjKey project) >>= \case
        Just _ | enableNixCache -> do
            let nixCompilerName = if compiler == GHCJS then "ghcjs" else "ghc"
                nixContinuation env = continuation ("bash", ["-c", T.pack . showCommandForUser cmd $ map T.unpack args], Just env)
            readIDE (to $ nixEnv (pjKey project) nixCompilerName) >>= \case
                Just env -> liftIDE $ nixContinuation env
                Nothing -> updateNixCache project [nixCompilerName] $
                    readIDE (to $ nixEnv (pjKey project) nixCompilerName) >>= mapM_ nixContinuation
        Just nixFile ->

            liftIDE $ continuation ("nix-shell", [ "-E"
                    , "let x = (let fn = import " <> T.pack nixFile <>
                                    "; in if builtins.isFunction fn then fn {} else fn); in ({ shells = { ghc = ({ env = x; } // x).env; }; } // x).shells." <> if compiler == GHCJS then "ghcjs" else "ghc"
                    , "--run", T.pack . showCommandForUser cmd $ map T.unpack args], Nothing)
        Nothing -> liftIDE $ continuation (cmd, args, Nothing)

readAndFlattenPackageDescription :: MonadIDE m => IDEPackage -> m PackageDescription
readAndFlattenPackageDescription package =
    liftIO $ flattenPackageDescription <$>
         readGenericPackageDescription normal (ipdCabalFile package)

runCabalBuild :: CompilerFlavor -> Bool -> Bool -> Bool -> (Project, [IDEPackage]) -> (Bool -> IDEAction) -> IDEAction
runCabalBuild compiler backgroundBuild jumpToWarnings withoutLinking (project, packages) continuation = do
    let dir = pjDir $ pjKey project
    activeComponent' <- catMaybes <$> mapM (getActiveComponent project) packages
    pjFileArgs <- projectFileArguments project dir
    flagsForTestsAndBenchmarks <- fmap concat $ forM packages $ \package -> do
        pd <- readAndFlattenPackageDescription package
        let pkgName = ipdPackageName package
        return $
            [ pkgName <> ":lib:" <> pkgName | ipdHasLib package ]
            <> (if "--enable-tests" `elem` ipdConfigFlags package
                then case pjKey project of
                    StackTool {} -> ["--test", "--no-run-tests"] -- if we use stack, with tests enabled, we build the tests without running them
                    CabalTool {} -> map (\t -> pkgName <> ":test:" <> T.pack (unUnqualComponentName $ testName t)) $ testSuites pd
                    _ -> []
                else [])
            <> (if "--enable-benchmarks" `elem` ipdConfigFlags package
                then case pjKey project of
                    StackTool {} -> ["--bench", "--no-run-benchmarks"] -- if we use stack, with benchmarks enabled, we build the benchmarks without running them
                    CabalTool {} -> map (\t -> pkgName <> ":benchmark:" <> T.pack (unUnqualComponentName $ benchmarkName t)) $ benchmarks pd
                    _ -> []
                else [])
    let mbCmdAndArgs = case pjKey project of
            StackTool {} -> Just ("stack",
                 ["build"]
              <> pjFileArgs
              <> activeComponent'
              <> flagsForTestsAndBenchmarks)
            CabalTool {} -> Just ("cabal", ["new-build"]
              <> pjFileArgs
              <> (if compiler == GHCJS then ["--ghcjs", "--builddir=dist-ghcjs"] else [])
              <> ["--with-ld=false" | pjIsCabal (pjKey project) && backgroundBuild && withoutLinking]
              <> activeComponent'
              <> flagsForTestsAndBenchmarks)
            CustomTool p -> (if compiler == GHCJS then pjCustomGhcjsBuild else pjCustomGhcBuild) p
        mbCmdAndArgs' = second (++ concatMap ipdBuildFlags packages) <$> mbCmdAndArgs

    withToolCommand project compiler mbCmdAndArgs' $ \(cmd, args', nixEnv') -> do
        mbEnv <- if compiler == GHCJS
                    then do
                        env <- packagesEnv packages =<<
                            maybe (liftIO getEnvironment) return (M.toList <$> nixEnv')
                        dataDir <- getDataDir
                        emptyFile <- liftIO $ getConfigFilePathForLoad "empty-file" Nothing dataDir
                        return . Just $ ("GHC_ENVIRONMENT", emptyFile) : env
                    else return $ M.toList <$> nixEnv'
        runExternalTool' (__ "Building") cmd args' dir mbEnv $ do
            (mbLastOutput, _) <- C.getZipSink $ (,)
                <$> C.ZipSink sinkLast
                <*> (C.ZipSink $ logOutputForBuild project (LogProject dir) backgroundBuild jumpToWarnings)
            lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess))
  `catchIDE`
      (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

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
    doBuild = catchIDE (reloadDebug False packages)
        (\(e :: SomeException) -> sysMessage Normal (T.pack $ show e))
    reloadDebug _ [] = do
        prefs' <- readIDE prefs
        let compile' = compile ([GHC | native prefs'] ++ [GHCJS | javaScript prefs' && pjIsCabal (pjKey project)])
        compile'
    reloadDebug restart (package:rest) = do
        ideR  <- liftIDE ask
        prefs' <- readIDE prefs
        lookupDebugState (pjKey project, ipdCabalFile package) >>= \case
            Just debug@DebugState{..} | restart ->
                (`runDebug` debug) . executeDebugCommand ":quit" $ do
                    logOutputDefault
                    lift $ reloadDebug restart (package:rest)
            Just debug@DebugState{..} -> do
                proc <- liftIO $ toolProcess dsToolState
                reloadComplete <- liftIO $ newMVar ReloadRunning
                let interruptReload =
                        void . modifyMVar_ reloadComplete $ \case
                            ReloadComplete -> return ReloadComplete
                            ReloadInterrupting -> do
                                interruptTool dsToolState
                                return ReloadInterrupting
                            ReloadRunning -> do
                                interruptTool dsToolState
                                _ <- forkIO $ do
                                    threadDelay 5000000
                                    void . modifyMVar_ reloadComplete $ \case
                                        ReloadComplete -> return ReloadComplete
                                        _ -> (`reflectIDE` ideR) $ do
                                            stillRunning <- liftIO $ isNothing <$> getProcessExitCode proc
                                            when stillRunning $ do
                                                ideMessage High (__ "Interrupting :reload took too long. Terminating ghci.")
                                                liftIO $ killProcess proc
                                            return ReloadComplete
                                return ReloadInterrupting
                modifyIDE_ $ runningTool ?~ (proc, interruptReload)
                (`runDebug` debug) . executeDebugCommand ":reload" $ do
                    (lastOutput, errs) <- C.getZipSink $ (,)
                        <$> C.ZipSink sinkLast
                        <*> C.ZipSink (logOutputForBuild project (LogProject dsBasePath) backgroundBuild jumpToWarnings)
                    -- If the tool has exited we should not clear the runningTool.  The isRunning function will
                    -- already be returning False and the next process may have already srtarted.
                    case lastOutput of
                        Just (ToolExit _) -> return ()
                        _ -> lift $ modifyIDE_ $ runningTool .~ Nothing
                    lift . postAsyncIDE $ do
                        liftIO $ debugM "leksah" "Reload done"
                        wasInterrupted <- liftIO . modifyMVar reloadComplete $ \s ->
                            return (ReloadComplete, s /= ReloadRunning)
                        unless (any isError errs || wasInterrupted || not (maybe False isToolPrompt lastOutput)) $ do
                            readIDE autoCommand >>= mapM_ (\(autoPack, cmd) ->
                                when (autoPack == (pjKey project, ipdCabalFile package)) cmd)
                            reloadDebug True $ filter (\p -> ipdCabalFile p `notElem` map ipdCabalFile dsPackages) rest
            Nothing | debug prefs' ->
                    readIDE workspace >>= mapM_ (runWorkspace $ runProject (State.runPackage (debugStart
                        (liftIDE $ reloadDebug False (package:rest))) package) project)
            Nothing -> reloadDebug True rest
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

#ifdef MIN_VERSION_unix
killProcess :: ProcessHandle -> IO ()
killProcess ph =
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
            cleanCabal "dist-newstyle"
            cleanCabal "dist-ghcjs"
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
                    liftIO $ writeFlags (dropExtension (ipdCabalFile packWithNewFlags) ++ leksahFlagFileExtension) packWithNewFlags
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
            lookupDebugState (pjKey project, ipdCabalFile package) >>= \case
                Nothing -> do
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
                            (buildDir, cDir, _) <- liftIO $ cabalProjectBuildDir (pjDir $ pjKey project) "dist-newstyle"
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
                Just debug ->
                    -- TODO check debug package matches active package
                    runDebug (do
                        case exe of
                            [Executable {exeName = _name, modulePath = mainFilePath}] ->
                                executeDebugCommand (":module *" <> T.pack (map (\c -> if c == '/' then '.' else c) (takeWhile (/= '.') mainFilePath)))
                                                    (logOutput logLaunch)
                            _ -> return ()
                        executeDebugCommand (":main " <> T.unwords (ipdExeFlags package)) (logOutput logLaunch))
                        debug)
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
                                (buildDir, cDir, _) <- liftIO $ cabalProjectBuildDir (pjDir $ pjKey project) "dist-ghcjs"
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
            packageRunDocTests backgroundBuild jumpToWarnings (project, package) $ \ok ->
                when ok $ do
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

packageRunDocTests :: Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
packageRunDocTests backgroundBuild jumpToWarnings (project, package) continuation =
    case pjKey project of
        StackTool {} -> do
            ideMessage Normal "Skipping automatic doctests (not implemented for stack yet)."
            continuation True
        CabalTool {} -> do
            let dir = ipdPackageDir package
            showDefaultLogLaunch'
            catchIDE (do
                ghcVersion <- liftIO getDefaultGhcVersion
                packageDBs <- liftIO $ getPackageDBs' ghcVersion (Just $ pjKey project)
                let pkgId = packageIdentifierToString $ ipdPackageId package
                (buildDir, _, _cabalVer) <- liftIO $ cabalProjectBuildDir (pjDir $ pjKey project) "dist-newstyle"
                let args = [ "act-as-setup"
                           , "--"
                           , "doctest"
                           , T.pack ("--builddir=" <> (buildDir </> T.unpack pkgId))]
                withToolCommand project GHC (Just ("cabal", args ++ ipdTestFlags package)) $ \(cmd, args', nixEnv') ->
                    runExternalTool' (__ "Doctest")
                            cmd args' dir (Just $ [("GHC_PACKAGE_PATH", intercalate [searchPathSeparator] (pDBsPaths packageDBs))] <> maybe [] M.toList nixEnv') $ do
                            (mbLastOutput, _) <- C.getZipSink $ (,)
                                <$> C.ZipSink sinkLast
                                <*> (C.ZipSink $ logOutputForBuild project (LogCabal $ ipdCabalFile package) backgroundBuild jumpToWarnings)
                            lift $ do
                                _errs <- readIDE errorRefs
                                when (mbLastOutput == Just (ToolExit ExitSuccess)) $ continuation True)
                    (\(e :: SomeException) -> ideMessage High . T.pack $ show e)
        _ -> do
            ideMessage Normal "Skipping automatic doctests (not implemented for custom projects yet)."
            continuation True

packageRunComponent :: Component -> Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
packageRunComponent (CLib _) _ _ _ _ = error "packageRunComponent"
packageRunComponent component backgroundBuild jumpToWarnings (project, package) continuation = do
    let (_cType, name, command) = case component of
                    CLib _ -> error "packageRunComponent"
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
                liftIO . putMVar mvar $ head $ mapMaybe getDistOutput output
            liftIO $ Just <$> takeMVar mvar
        CabalTool {} -> do
            (buildDir, _, _) <- liftIO $ cabalProjectBuildDir (pjDir $ pjKey project) "dist-newstyle"
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
                pd <- liftIO $ readGenericPackageDescription normal (ipdCabalFile p)
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
        ,   ("@Maintainer@"   , T.pack $ maintainer pd)
        ,   ("@Stability@"    , T.pack $ stability pd)
        ,   ("@Portability@"  , "")
        ,   ("@Copyright@"    , T.pack $ copyright pd)
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
        gpd <- readGenericPackageDescription normal (ipdCabalFile p)
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

addModToLib :: ModuleName -> CondTree ConfVar [Dependency] Library ->
    CondTree ConfVar [Dependency] Library
addModToLib modName ct@CondNode{condTreeData = lib} =
    ct{condTreeData = lib{exposedModules = modName `inOrderAdd` exposedModules lib}}

addModToBuildInfoLib :: ModuleName -> CondTree ConfVar [Dependency] Library ->
    CondTree ConfVar [Dependency] Library
addModToBuildInfoLib modName ct@CondNode{condTreeData = lib} =
    ct{condTreeData = lib{libBuildInfo = (libBuildInfo lib){otherModules = modName
        `inOrderAdd` otherModules (libBuildInfo lib)}}}

addModToBuildInfoExe :: UnqualComponentName -> ModuleName -> (UnqualComponentName, CondTree ConfVar [Dependency] Executable) ->
    (UnqualComponentName, CondTree ConfVar [Dependency] Executable)
addModToBuildInfoExe name modName (str,ct@CondNode{condTreeData = exe}) | str == name =
    (str, ct{condTreeData = exe{buildInfo = (buildInfo exe){otherModules = modName
        `inOrderAdd` otherModules (buildInfo exe)}}})
addModToBuildInfoExe _name _ x = x

addModToBuildInfoTest :: UnqualComponentName -> ModuleName -> (UnqualComponentName, CondTree ConfVar [Dependency] TestSuite) ->
    (UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)
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
        gpd <- readGenericPackageDescription normal (ipdCabalFile p)
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

delModFromLib :: ModuleName -> CondTree ConfVar [Dependency] Library ->
    CondTree ConfVar [Dependency] Library
delModFromLib modName ct@CondNode{condTreeData = lib} =
    ct{condTreeData = lib{exposedModules = delete modName (exposedModules lib)}}

delModFromBuildInfoLib :: ModuleName -> CondTree ConfVar [Dependency] Library ->
    CondTree ConfVar [Dependency] Library
delModFromBuildInfoLib modName ct@CondNode{condTreeData = lib} =
    ct{condTreeData = lib{libBuildInfo = (libBuildInfo lib){otherModules =
        delete modName (otherModules (libBuildInfo lib))}}}

delModFromBuildInfoExe :: ModuleName -> (UnqualComponentName, CondTree ConfVar [Dependency] Executable) ->
    (UnqualComponentName, CondTree ConfVar [Dependency] Executable)
delModFromBuildInfoExe modName (str,ct@CondNode{condTreeData = exe}) =
    (str, ct{condTreeData = exe{buildInfo = (buildInfo exe){otherModules =
        delete modName (otherModules (buildInfo exe))}}})

isExposedModule :: ModuleName -> Maybe (CondTree ConfVar [Dependency] Library)  -> Bool
isExposedModule _ Nothing                              = False
isExposedModule mn (Just CondNode{condTreeData = lib}) = mn `elem` exposedModules lib

-- ---------------------------------------------------------------------
-- | * Debug code that needs to use the package
--

interactiveFlag :: Text -> Bool -> Text
interactiveFlag name f = (if f then "-f" else "-fno-") <> name

printEvldWithShowFlag :: Bool -> Text
printEvldWithShowFlag = interactiveFlag "print-evld-with-show"

breakOnExceptionFlag :: Bool -> Text
breakOnExceptionFlag = interactiveFlag "break-on-exception"

breakOnErrorFlag :: Bool -> Text
breakOnErrorFlag = interactiveFlag "break-on-error"

printBindResultFlag :: Bool -> Text
printBindResultFlag = interactiveFlag "print-bind-result"

interactiveFlags :: Prefs -> [Text]
interactiveFlags prefs' =
    printEvldWithShowFlag (printEvldWithShow prefs')
  : breakOnExceptionFlag (breakOnException prefs')
  : breakOnErrorFlag (breakOnError prefs')
  : [printBindResultFlag $ printBindResult prefs']

debugStart :: PackageAction -> PackageAction
debugStart continue = do
    liftIO $ debugM "leksah" "debugStart"
    project <- lift ask
    package <- ask
    let projectAndPackage = (pjKey project, ipdCabalFile package)
    liftIDE $ catchIDE (do
        ideRef     <- ask
        prefs'     <- readIDE prefs
        lookupDebugState projectAndPackage >>= \case
            Nothing -> do
                let obeliskPackageFiles = map (\s -> pjDir (pjKey project) </> s </> s <> ".cabal") ["common", "backend", "frontend"]
                    obeliskPackages = filter (\p -> ipdCabalFile p `elem` obeliskPackageFiles) $ pjPackages project
                isObelisk <- (&& (ipdCabalFile package `elem` obeliskPackageFiles)) <$>
                    liftIO (doesDirectoryExist (pjDir (pjKey project) </> ".obelisk"))
                let debugPackages = if isObelisk then obeliskPackages else [package]
                    basePath = if isObelisk then pjDir (pjKey project) else ipdPackageDir package
                mbActiveComponent <- getActiveComponent project package
                let dir  = ipdPackageDir  package
                    name = ipdPackageName package
                pjFileArgs <- projectFileArguments project dir
                liftIO $ debugM "leksah" "debugStart withToolCommand"
                withToolCommand project GHC (
                        case pjKey project of
                            _ | isObelisk -> Just ("ob", ["repl"])
                            CabalTool {} -> Just ("cabal", [ "new-repl" ]
                                                <> pjFileArgs
                                                <> [ fromMaybe (name <> ":lib:" <> name) mbActiveComponent | ipdHasLib package || isJust mbActiveComponent ]
                                                <> ipdBuildFlags package)
                            StackTool {} -> Just ("stack", [ "repl" ]
                                                <> pjFileArgs
                                                <> [ name <> maybe ":lib" (":" <>) mbActiveComponent ])
                            _ -> Nothing) $ \(tool, args, nixEnv') -> do
                    let logOut = reflectIDEI (void (logOutputForBuild project (LogProject basePath) True False)) ideRef
                        logIdle = reflectIDEI (C.getZipSink $ const <$> C.ZipSink (logIdleOutput project package) <*> C.ZipSink logOutputDefault) ideRef
                    ghci <- liftIO $ (if isObelisk then newGhci tool args (pjDir $ pjKey project) Nothing else newGhci tool args dir nixEnv') ("+c":"-ferror-spans":interactiveFlags prefs') logOut logIdle
                    liftIO $ do
                        executeGhciCommand ghci ghciFork logOut
                        executeGhciCommand ghci ":reload" logOut
                    modifyIDE_ $ debugState %~ (DebugState (pjKey project) debugPackages basePath ghci :)
                    triggerEventIDE_ (Sensitivity [(SensitivityInterpreting, True)])
                    triggerEventIDE_ (DebugStart projectAndPackage)
                    -- Fork a thread to wait for the output from the process to close
                    _ <- liftIO $ forkIO $ do
                        _ <- readMVar (outputClosed ghci)
                        (`reflectIDE` ideRef) . postSyncIDE $
                            forM_ debugPackages $ \_package -> do
                                modifyIDE_ $ debugState %~ filter ((/= toolProcessMVar ghci) . toolProcessMVar . dsToolState)
                                triggerEventIDE_ (Sensitivity [(SensitivityInterpreting, False)])
                                triggerEventIDE_ (DebugStop projectAndPackage)
                                return ()
                    readIDE workspace >>= mapM_ (runWorkspace $ runProject (State.runPackage continue package) project)
            _ -> do
                sysMessage Normal (__ "Debugger already running")
                readIDE workspace >>= mapM_ (runWorkspace $ runProject (State.runPackage continue package) project))
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

tryDebug' :: PackageM Bool -> DebugAction -> PackageAction
tryDebug' promptUser f = do
    prefs' <- readIDE prefs
    packageDebugState >>= \case
        Just d -> liftIDE $ runDebug f d
        _ | debug prefs' ->
                debugStart $
                    packageDebugState >>=
                        mapM_ (liftIDE . postAsyncIDE . runDebug f)
          | otherwise ->
            promptUser >>= \case
                True ->
                    debugStart $
                        packageDebugState >>=
                            mapM_ (liftIDE . postAsyncIDE . runDebug f)
                False  -> return ()

tryDebugQuiet :: DebugAction -> PackageAction
tryDebugQuiet f = do
    project <- lift ask
    package <- ask
    lookupDebugState (pjKey project, ipdCabalFile package) >>=
        mapM_ (liftIDE . runDebug f)

executeDebugCommand :: Text -> ConduitT ToolOutput Void IDEM () -> DebugAction
executeDebugCommand command handler = do
    DebugState{dsToolState = ghci} <- ask
    lift $ do
        ideR <- ask
        postAsyncIDE $
            triggerEventIDE_ (StatusbarChanged [CompartmentState command, CompartmentBuild True])
        liftIO . executeGhciCommand ghci command $
            reflectIDEI (do
                lift . postSyncIDE $
                   triggerEventIDE_ (StatusbarChanged [CompartmentState "", CompartmentBuild False])
                handler) ideR

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
idePackageFromPath' ipdCabalFile = do
    mbPackageD <- catchIDE (liftIO $
        Just . flattenPackageDescription <$> readGenericPackageDescription normal ipdCabalFile)
            (\ (e :: SomeException) -> do
                ideMessage Normal (__ "Can't activate package " <> T.pack (show e))
                return Nothing)
    case mbPackageD of
        Nothing       -> return Nothing
        Just packageD -> do

            let ipdModules          = M.fromList $ myLibModules packageD ++ myExeModules packageD
                                        ++ myTestModules packageD ++ myBenchmarkModules packageD
                ipdMain             = [ (modulePath exe, buildInfo exe, False) | exe <- executables packageD ]
                                        ++ [ (f, bi, True) | TestSuite {testInterface = TestSuiteExeV10 _ f, testBuildInfo = bi} <- testSuites packageD ]
                                        ++ [ (f, bi, True) | Benchmark {benchmarkInterface = BenchmarkExeV10 _ f, benchmarkBuildInfo = bi} <- benchmarks packageD ]
                ipdExtraSrcs        = S.fromList $ extraSrcFiles packageD
                ipdSrcDirs          = case nub $ concatMap hsSourceDirs (allBuildInfo' packageD) of
                                            [] -> [".","src"]
                                            l -> l
                ipdSubLibraries     = [ T.pack . unUnqualComponentName $ e | Just e <- libraryNameString . libName <$> subLibraries packageD ]
                ipdExes             = [ T.pack . unUnqualComponentName $ exeName e | e <- executables packageD ]
                ipdExtensions       = nub $ concatMap oldExtensions (allBuildInfo' packageD)
                ipdTests            = [ T.pack . unUnqualComponentName $ testName t | t <- testSuites packageD ]
                ipdBenchmarks       = [ T.pack . unUnqualComponentName $ benchmarkName b | b <- benchmarks packageD ]
                ipdPackageId        = package packageD
#if MIN_VERSION_Cabal(2,4,0)
                ipdDepends          = allBuildDepends packageD
#else
                ipdDepends          = buildDepends packageD
#endif
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
                pfile               = dropExtension ipdCabalFile
            pack <- do
                flagFileExists <- liftIO $ doesFileExist (pfile ++ leksahFlagFileExtension)
                if flagFileExists
                    then liftIO $ readFlags (pfile ++ leksahFlagFileExtension) packp
                    else return packp
            return (Just pack)

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
            patterns <- liftIO $ map (Glob.compile . (</> "*.cabal")) <$>
                case key of
                    CabalTool (CabalProject filePath) -> extractCabalPackageList <$> T.readFile filePath
                    StackTool (StackProject filePath) -> extractStackPackageList <$> T.readFile filePath
                    CustomTool p -> return []
            let dir = pjDir key
            cabalFiles <- liftIO $ mapM canonicalizePath =<< map (dir </>) . concat <$>
                              Glob.globDir patterns dir
            packages <- fmap catMaybes . mapM idePackageFromPath' $ nub cabalFiles
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
--                triggerEventIDE $ WorkspaceChanged False True
--                return mbUpdatedPack
--            Nothing -> do
--                postAsyncIDE $ ideMessage Normal (__ "Can't read package file")
--                return Nothing

ghciFork :: Text
ghciFork = T.unlines
  [ ":def! fork (\\s ->"
  , "  let (slot, code) = Data.List.span (\\c -> case c of"
  , "          '_' -> Data.Bool.True"
  , "          ' ' -> Data.Bool.False"
  , "          '\\n' -> Data.Bool.False"
  , "          _ -> if Data.Char.isAlphaNum c"
  , "                  then Data.Bool.True"
  , "                  else GHC.Base.error \" Slot name must contain alpha, numbers and '_' only. Usage :fork slotName putStrLn \\\" Hello World\\\"\") s"
  , "  in Control.Monad.return (Data.String.unlines"
  , "    [\" :{\" "
  , "    ,\" System.Environment.lookupEnv \\\" GHCI_FORK_\"  Data.Monoid.<> slot Data.Monoid.<> \" \\\"  Control.Monad.>>=\" "
  , "    ,\" (\\\\s ->\" "
  , "    ,\"   ( case s Control.Monad.>>= Text.Read.readMaybe of\" "
  , "    ,\"       Just n ->\" "
  , "    ,\"         let sPtr = Foreign.StablePtr.castPtrToStablePtr (Foreign.Ptr.wordPtrToPtr n)\" "
  , "    ,\"         in  Foreign.StablePtr.deRefStablePtr sPtr Control.Monad.>>=\" "
  , "    ,\"             (\\\\(t, running) -> Control.Concurrent.killThread t Control.Monad.>>\" "
  , "    ,\"             Foreign.StablePtr.freeStablePtr sPtr Control.Monad.>>\" "
  , "    ,\"             Control.Monad.return running)\" "
  , "    ,\"       Data.Maybe.Nothing -> Control.Concurrent.newEmptyMVar\" "
  , "    ,\"   ) Control.Monad.>>=\" "
  , "    ,\" (\\\\running -> Control.Concurrent.newEmptyMVar Control.Monad.>>=\" "
  , "    ,\" (\\\\sPtrSet -> Control.Concurrent.forkFinally\" "
  , "    ,\"   ( Control.Concurrent.takeMVar sPtrSet Control.Monad.>>\" "
  , "    ,\"     Control.Concurrent.putMVar running () Control.Monad.>>\" "
  , "    ,\"     (\" "
  , "    ,     Data.List.drop 1 code"
  , "    ,\"     )\" "
  , "    ,\"   ) (\\\\_ -> Control.Concurrent.takeMVar running) Control.Monad.>>=\" "
  , "    ,\" (\\\\t -> Foreign.StablePtr.newStablePtr (t, running) Control.Monad.>>=\" "
  , "    ,\" (\\\\sPtr -> System.Environment.setEnv \\\" GHCI_FORK_\"  Data.Monoid.<> slot Data.Monoid.<> \" \\\"  (GHC.Show.show\" "
  , "    ,\"   (Foreign.Ptr.ptrToWordPtr (Foreign.StablePtr.castStablePtrToPtr sPtr))) Control.Monad.>>\" "
  , "    ,\" Control.Concurrent.putMVar sPtrSet ())))))\" "
  , "    ,\" :}\" "
  , "    ]))"
  ]
