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
,   packageConfig
,   packageConfig'
,   buildPackage

,   packageDoc
,   packageDoc'
,   packageClean
,   packageClean'
,   packageCopy
,   packageInstall
,   packageInstall'
,   packageRun
,   packageRunJavaScript
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

,   backgroundBuildToggled
,   makeDocsToggled
,   runUnitTestsToggled
,   runBenchmarksToggled
,   nativeToggled
,   javaScriptToggled
,   makeModeToggled

,   debugStart
,   printBindResultFlag
,   breakOnErrorFlag
,   breakOnExceptionFlag

,   printEvldWithShowFlag
,   tryDebug
,   tryDebugQuiet
,   executeDebugCommand

,   choosePackageFile

,   idePackageFromPath'
,   ideProjectFromPath
,   writeGenericPackageDescription'

) where

import Distribution.Package hiding (depends,packageId)
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Verbosity
import System.FilePath
import Control.Concurrent
import System.Directory
       (removeDirectoryRecursive, canonicalizePath, setCurrentDirectory,
        doesFileExist, getDirectoryContents, doesDirectoryExist)
import Prelude hiding (catch)
import Data.Char (isSpace)
import Data.Maybe
       (maybeToList, mapMaybe, listToMaybe, fromMaybe, isNothing, isJust,
        fromJust, catMaybes)
import Data.Function (on)
import Control.Exception (SomeException(..), catch)

import qualified IDE.Core.State as State (runPackage)
import IDE.Core.State
       (ProjectAction, mkPackageMap, packageDebugState, reflectIDEI,
        printBindResult, breakOnError, breakOnException, printEvldWithShow,
        changePackage, pjToolCommand', postSyncIDE, autoURI, sysMessage,
        autoCommand, isError, postAsyncIDE, runDebug, modifyIDE_,
        runProject, runWorkspace, debug, triggerBuild, MessageLevel(..),
        catchIDE, errorRefs, getDataDir, ipdPackageName, pjDir, useVado,
        activeComponent, activeProject, ideMessage, ipdPackageDir,
        saveAllBeforeBuild, reflectIDE, wsName, workspace, triggerEventIDE,
        activePack, readIDE, IDEPackage(..), Project(..), pjPackageMap,
        pjFile, pjTool, DebugAction, debugState, Prefs, makeMode,
        javaScript, native, runBenchmarks, runUnitTests, makeDocs,
        backgroundBuild, prefs, ipdConfigFlags, runningTool, IDEM,
        PackageAction, IDEAction, MonadIDE, StatusbarCompartment(..),
        IDEEvent(..), SensitivityMask(..), MonadIDE(..), ProjectTool(..),
        packageIdentifierToString, getMainWindow, nixCache, modifyIDE,
        leksahTemplateFileExtension, leksahFlagFileExtension, displayPane,
        nixEnv, Log(..))
import IDE.Utils.GUIUtils
import IDE.Utils.CabalUtils (writeGenericPackageDescription')
import IDE.Pane.Log
import IDE.Pane.PackageEditor
import IDE.Pane.SourceBuffer
import IDE.Pane.PackageFlags (writeFlags, readFlags)
import Distribution.Text (display)
import IDE.Utils.FileUtils
       (loadNixCache, loadNixEnv, cabalProjectBuildDir, nixShellFile,
        getConfigFilePathForLoad, getPackageDBs', saveNixCache)
import IDE.LogRef
import Distribution.ModuleName (ModuleName(..))
import Data.List
       (isPrefixOf, intercalate, isInfixOf, nub, foldl', delete, find)
import IDE.Utils.Tool
       (toolProcess, ToolOutput(..), runTool, newGhci, ToolState(..),
        toolline, ProcessHandle, executeGhciCommand, interruptTool,
        terminateProcess, isToolPrompt)
import qualified Data.Set as  Set (fromList)
import Data.Either (isRight)
import Data.Map (Map)
import qualified Data.Map as  Map (empty, fromList)
import System.Exit (ExitCode(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.Conduit as C (Sink, ZipSink(..), getZipSink)
import qualified Data.Conduit.List as CL (foldM, fold, consume)
import Data.Conduit (($$))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad (void, when, unless, liftM, forM, forM_)
import Distribution.PackageDescription.PrettyPrint
       (showGenericPackageDescription)
import Debug.Trace (trace)
import IDE.Pane.WebKit.Documentation
       (getDocumentation, loadDoc, reloadDoc)
import IDE.Pane.WebKit.Output (loadOutputUri, loadOutputHtmlFile, getOutputPane)
import System.Log.Logger (errorM, debugM)
import System.Process.Vado (getMountPoint, vado, readSettings)
import qualified Data.Text as T
       (reverse, all, null, dropWhile, lines, isPrefixOf, stripPrefix,
        replace, unwords, takeWhile, pack, unpack, isInfixOf)
import IDE.Utils.ExternalTool
       (runExternalTool', runExternalTool, isRunning,
        interruptBuild)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text.IO as T (readFile)
import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)
import IDE.Metainfo.Provider (updateSystemInfo, updateSystemInfo)
import GI.GLib.Functions (timeoutAdd)
import GI.GLib.Constants (pattern PRIORITY_DEFAULT, pattern PRIORITY_LOW)
import GI.Gtk.Objects.MessageDialog
       (setMessageDialogText, constructMessageDialogButtons, setMessageDialogMessageType,
        MessageDialog(..))
import GI.Gtk.Objects.Dialog (constructDialogUseHeaderBar)
import GI.Gtk.Enums
       (WindowPosition(..), ResponseType(..), ButtonsType(..),
        MessageType(..))
import GI.Gtk.Objects.Window
       (setWindowWindowPosition, windowSetTransientFor)
import Graphics.UI.Editor.Parameters
       (dialogRun', dialogSetDefaultResponse', dialogAddButton')
import Data.GI.Base (set, new')
import GI.Gtk.Objects.Widget (widgetDestroy)
import IDE.Utils.VersionUtils (getDefaultGhcVersion)
import IDE.Utils.CabalProject
       (findProjectRoot, getCabalProjectPackages)
import System.Environment (getEnvironment)
import Distribution.Simple.LocalBuildInfo
       (Component(..), Component)
import Distribution.Simple.Utils (writeUTF8File)
import Distribution.Compiler (CompilerFlavor(..))
import qualified Data.Map as M
       (toList, fromList, member, delete, insert, lookup)
import Control.Lens ((<&>))
import System.Process (showCommandForUser)
#ifdef MIN_VERSION_unix
import System.Posix (sigKILL, signalProcess)
import System.Process.Internals
       (withProcessHandle, ProcessHandle__(..))
#endif
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.ForeignLib (foreignLibName)
import Distribution.Types.UnqualComponentName
       (UnqualComponentName(..), mkUnqualComponentName,
        unUnqualComponentName)
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
       (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse
       (readGenericPackageDescription)
#endif
#else
import Distribution.PackageDescription.Parse
       (readPackageDescription)
#endif
import qualified System.FilePath.Glob as Glob (globDir, compile)

#if !MIN_VERSION_Cabal(2,0,0)
type UnqualComponentName = String
mkUnqualComponentName :: String -> UnqualComponentName
mkUnqualComponentName = id
unUnqualComponentName :: UnqualComponentName -> String
unUnqualComponentName = id
#endif

printf :: PrintfType r => Text -> r
printf = S.printf . T.unpack

-- | Get the last item
sinkLast = CL.fold (\_ a -> Just a) Nothing

moduleInfo :: (a -> BuildInfo) -> (a -> [ModuleName]) -> a -> [(ModuleName, BuildInfo)]
moduleInfo bi mods a = map (\m -> (m, buildInfo)) $ mods a
    where buildInfo = bi a

myLibModules pd = case library pd of
                    Nothing -> []
#if MIN_VERSION_Cabal(2,0,0)
                    Just l -> moduleInfo libBuildInfo explicitLibModules l
#else
                    Just l -> moduleInfo libBuildInfo libModules l
#endif
myExeModules pd = concatMap (moduleInfo buildInfo exeModules) (executables pd)
myTestModules pd = concatMap (moduleInfo testBuildInfo (otherModules . testBuildInfo)) (testSuites pd)
myBenchmarkModules pd = concatMap (moduleInfo benchmarkBuildInfo (otherModules . benchmarkBuildInfo)) (benchmarks pd)

activatePackage :: MonadIDE m => Maybe FilePath -> Maybe Project -> Maybe IDEPackage -> Maybe Text -> m ()
activatePackage mbPath mbProject mbPack mbComponent = do
    liftIO $ debugM "leksah" $ "activatePackage " <> show (mbPath, pjFile <$> mbProject, ipdCabalFile <$> mbPack, mbComponent)
    oldActivePack <- readIDE activePack
    case mbPath of
        Just p -> liftIO $ setCurrentDirectory (dropFileName p)
        Nothing -> return ()
    when (isJust mbPack || isJust oldActivePack) $ do
        triggerEventIDE (Sensitivity [(SensitivityProjectActive,isJust mbPack)])
        return ()
    mbWs <- readIDE workspace
    let wsStr = case mbWs of
                    Nothing -> ""
                    Just ws -> wsName ws
        txt = case (mbPath, mbPack) of
                    (_, Just pack) -> wsStr <> " > " <> packageIdentifierToString (ipdPackageId pack)
                    (Just path, _) -> wsStr <> " > " <> T.pack (takeFileName path)
                    _ -> wsStr <> ":"
    triggerEventIDE (StatusbarChanged [CompartmentPackage txt])
    return ()

deactivatePackage :: IDEAction
deactivatePackage = activatePackage Nothing Nothing Nothing Nothing

interruptSaveAndRun :: MonadIDE m => IDEAction -> m ()
interruptSaveAndRun action = do
    ideR <- liftIDE ask
    alreadyRunning <- isRunning
    if alreadyRunning
        then do
            liftIO $ debugM "leksah" "interruptSaveAndRun"
            interruptBuild
            timeoutAdd PRIORITY_DEFAULT 200 (do
                reflectIDE (do
                    interruptSaveAndRun action
                    return False) ideR
                return False)
            return ()
        else liftIDE run
  where
    run = do
        prefs <- readIDE prefs
        when (saveAllBeforeBuild prefs) . liftIDE . void $ fileSaveAll belongsToWorkspace'
        action

projectRefreshNix :: ProjectAction
projectRefreshNix = do
    project <- ask
    interruptSaveAndRun $ projectRefreshNix' project (return ())

projectRefreshNix' :: Project -> IDEAction -> IDEAction
projectRefreshNix' project continuation = do
    prefs     <- readIDE prefs
    case pjTool project of
        StackTool ->
            continuation
        CabalTool ->
            updateNixCache project ("ghc":["ghcjs" | javaScript prefs]) continuation

updateNixCache :: MonadIDE m => Project -> [Text] -> IDEAction -> m ()
updateNixCache project compilers continuation = loop compilers
  where
    loop :: MonadIDE m => [Text] -> m ()
    loop [] = liftIDE updateSystemInfo
    loop (compiler:rest) = do
        logLaunch <- getDefaultLogLaunch
        showDefaultLogLaunch'

        let dir = pjDir project
        nixShellFile dir >>= \case
            Just nixFile ->
                runExternalTool' (__ "Nix")
                                 "nix-shell"
                                 ["-E", "let x = (let fn = import " <> T.pack nixFile <> "; in if builtins.isFunction fn then fn {} else fn); in ({ shells = { ghc = ({ env = x; } // x).env; }; } // x).shells." <> compiler, "--run", "( set -o posix ; set )"]
                                 dir Nothing $ do
                    out <- C.getZipSink $ const
                        <$> C.ZipSink CL.consume
                        <*> C.ZipSink (logOutputForBuild project (LogNix nixFile ("shells." <> compiler)) False False)
                    when (take 1 (reverse out) == [ToolExit ExitSuccess]) $ do
                        saveNixCache (pjFile project) compiler out
                        newCache <- loadNixCache
                        lift $ do
                            modifyIDE_ (\ide -> ide { nixCache = newCache})
                            loop rest
            _ -> loop rest

packageConfig :: PackageAction
packageConfig = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ packageConfig' (project, package) (\ _ -> return ())

packageConfig'  :: (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
packageConfig' (project, package) continuation = do
    prefs     <- readIDE prefs
    case pjTool project of
        StackTool -> do
            ideMessage Normal (__ "Stack projects do not require configuration.")
            liftIDE $ continuation True
        CabalTool -> do
            logLaunch <- getDefaultLogLaunch
            showDefaultLogLaunch'

            let dir = ipdPackageDir package
            runExternalTool'        (__ "Configuring")
                                    "cabal"
                                    ("new-configure" : ipdConfigFlags package)
                                    dir Nothing $ do
                mbLastOutput <- C.getZipSink $ const <$> C.ZipSink sinkLast <*> C.ZipSink (logOutput logLaunch)
                lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess))

projectFileArguments :: MonadIO m => Project -> FilePath -> m [Text]
projectFileArguments project dir = do
    let projectFile = T.pack . makeRelative dir $ pjFile project
    case pjTool project of
        CabalTool -> do
            defaultProjectRoot <- liftIO $ findProjectRoot dir
            return $ if pjFile project /= defaultProjectRoot </> "cabal.project"
                                then [ "--project-file", projectFile ]
                                else []
        StackTool ->
            return $ if projectFile /= "stack.yaml"
                                then [ "--stack-yaml", projectFile ]
                                else []

getActiveComponent :: Project -> IDEPackage -> IDEM (Maybe Text)
getActiveComponent project package = do
    isActiveProject   <- maybe False (on (==) pjFile project) <$> readIDE activeProject
    isActivePackage   <- (isActiveProject &&) . maybe False (on (==) ipdCabalFile package) <$> readIDE activePack
    if isActivePackage
        then readIDE activeComponent
        else return Nothing

withToolCommand :: MonadIDE m => Project -> CompilerFlavor -> [Text] -> ((FilePath, [Text], Maybe (Map String String)) -> IDEAction) -> m ()
withToolCommand project compiler args continuation = do
    prefs <- readIDE prefs
    -- Nix cache will not work over vado
    enableNixCache <- if useVado prefs then liftIO $ isRight <$> getMountPoint (pjDir project) else return True
    nixShellFile (pjDir project) >>= \case
        Just _ | enableNixCache -> do
            let nixCompilerName = if compiler == GHCJS then "ghcjs" else "ghc"
                nixContinuation env = continuation ("bash", ["-c", T.pack . showCommandForUser (pjToolCommand' project) $ map T.unpack args], Just env)
            readIDE (nixEnv (pjFile project) nixCompilerName) >>= \case
                Just env -> liftIDE $ nixContinuation env
                Nothing -> updateNixCache project [nixCompilerName] $
                    readIDE (nixEnv (pjFile project) nixCompilerName) >>= mapM_ nixContinuation
        Just nixFile -> liftIDE $ continuation ("nix-shell", [ "-E"
                    , "let x = (let fn = import " <> T.pack nixFile <>
                                    "; in if builtins.isFunction fn then fn {} else fn); in ({ shells = { ghc = ({ env = x; } // x).env; }; } // x).shells." <> if compiler == GHCJS then "ghcjs" else "ghc"
                    , "--run", T.pack . showCommandForUser (pjToolCommand' project) $ map T.unpack args], Nothing)
        Nothing -> liftIDE $ continuation (pjToolCommand' project, args, Nothing)

#if !MIN_VERSION_Cabal(2,0,0)
readGenericPackageDescription = readPackageDescription
#endif

readAndFlattenPackageDescription :: MonadIDE m => IDEPackage -> m PackageDescription
readAndFlattenPackageDescription package =
    liftIO $ flattenPackageDescription <$>
         readGenericPackageDescription normal (ipdCabalFile package)

runCabalBuild :: CompilerFlavor -> Bool -> Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
runCabalBuild compiler backgroundBuild jumpToWarnings withoutLinking (project, package) continuation = do
    prefs <- readIDE prefs
    pd <- readAndFlattenPackageDescription package
    let dir = ipdPackageDir package
        pkgName = ipdPackageName package

    mbActiveComponent <- getActiveComponent project package
    pjFileArgs <- projectFileArguments project dir
    let flagsForTests =
            if "--enable-tests" `elem` ipdConfigFlags package
                then case pjTool project of
                    StackTool -> ["--test", "--no-run-tests"] -- if we use stack, with tests enabled, we build the tests without running them
                    CabalTool -> map (\t -> pkgName <> ":test:" <> T.pack (unUnqualComponentName $ testName t)) $ testSuites pd
                else []
    let flagsForBenchmarks =
            if "--enable-benchmarks" `elem` ipdConfigFlags package
                then case pjTool project of
                    StackTool -> ["--bench", "--no-run-benchmarks"] -- if we use stack, with benchmarks enabled, we build the benchmarks without running them
                    CabalTool -> map (\t -> pkgName <> ":benchmark:" <> T.pack (unUnqualComponentName $ benchmarkName t)) $ benchmarks pd
                else []
    let args =  -- stack needs the package name to actually print the output info
                (case pjTool project of
                    StackTool -> ["build"] <> pjFileArgs <> [ipdPackageName package]
                    CabalTool -> "new-build" : pjFileArgs)
                ++ (if compiler == GHCJS then ["--ghcjs", "--builddir=dist-ghcjs"] else [])
                ++ ["--with-ld=false" | pjTool project == CabalTool && backgroundBuild && withoutLinking]
                ++ maybeToList mbActiveComponent
                ++ flagsForTests
                ++ flagsForBenchmarks
                ++ ipdBuildFlags package

    withToolCommand project compiler args $ \(cmd, args', nixEnv) -> do
        mbEnv <- if compiler == GHCJS
                    then do
                        env <- packageEnv package =<<
                            maybe (liftIO getEnvironment) return (M.toList <$> nixEnv)
                        dataDir <- getDataDir
                        emptyFile <- liftIO $ getConfigFilePathForLoad "empty-file" Nothing dataDir
                        return . Just $ ("GHC_ENVIRONMENT", emptyFile) : env
                    else return $ M.toList <$> nixEnv
        runExternalTool' (__ "Building") cmd args' dir mbEnv $ do
            (mbLastOutput, _) <- C.getZipSink $ (,)
                <$> C.ZipSink sinkLast
                <*> (C.ZipSink $ logOutputForBuild project (LogCabal $ ipdCabalFile package) backgroundBuild jumpToWarnings)
            lift $ do
                errs <- readIDE errorRefs
                continuation (mbLastOutput == Just (ToolExit ExitSuccess))
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

buildPackage :: Bool -> Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
buildPackage backgroundBuild jumpToWarnings withoutLinking (project, package) continuation = do
    liftIO $ debugM "leksah" "buildPackage"
    ideR  <- liftIDE ask
    prefs <- readIDE prefs
    alreadyRunning <- isRunning
    if alreadyRunning
        then do
            liftIO $ debugM "leksah" "buildPackage interruptBuild"
            interruptBuild
            timeoutAdd PRIORITY_DEFAULT 100 (do
                reflectIDE (do
                    if backgroundBuild
                        then do
                            tb <- readIDE triggerBuild
                            void . liftIO $ tryPutMVar tb ()
                        else
                            buildPackage backgroundBuild jumpToWarnings withoutLinking
                                            (project, package) continuation
                    return False) ideR
                return False)
            return ()
        else do
            when (saveAllBeforeBuild prefs) . void $ fileSaveAll belongsToWorkspace'
            M.member (pjFile project, ipdCabalFile package) <$> readIDE debugState >>= \case
                False | debug prefs -> do
                    readIDE workspace >>= mapM_ (runWorkspace $ runProject (State.runPackage debugStart package) project)
                    doBuild True
                _ -> doBuild False
  where
    doBuild debugStarting = catchIDE (do
        ideR  <- liftIDE ask
        prefs <- readIDE prefs
        let compile' = compile ([GHC | native prefs] ++ [GHCJS | javaScript prefs && pjTool project == CabalTool])
        M.lookup (pjFile project, ipdCabalFile package) <$> readIDE debugState >>= \case
            Just debug@(_, ghci) -> do
                proc <- liftIO $ toolProcess ghci
                reloadComplete <- liftIO $ newMVar ReloadRunning
                let interruptReload =
                        void . modifyMVar_ reloadComplete $ \case
                            ReloadComplete -> return ReloadComplete
                            ReloadInterrupting -> do
                                interruptTool ghci
                                return ReloadInterrupting
                            ReloadRunning -> do
                                interruptTool ghci
                                forkIO $ do
                                    threadDelay 5000000
                                    void . modifyMVar_ reloadComplete $ \case
                                        ReloadComplete -> return ReloadComplete
                                        _ -> (`reflectIDE` ideR) $ do
                                            ideMessage High (__ "Interrupting :reload took too long. Terminating ghci.")
                                            liftIO $ killProcess proc
                                            return ReloadComplete
                                return ReloadInterrupting
                modifyIDE_ $ \ide -> ide {runningTool = Just (proc, interruptReload)}
                (`runDebug` debug) . executeDebugCommand ":reload" $ do
                    (lastOutput, errs) <- C.getZipSink $ (,)
                        <$> C.ZipSink sinkLast
                        <*> C.ZipSink (logOutputForBuild project (LogCabal $ ipdCabalFile package) backgroundBuild jumpToWarnings)
                    lift . postAsyncIDE $ do
                        liftIO $ debugM "leksah" "Reload done"
                        modifyIDE_ (\ide -> ide {runningTool = Nothing})
                        wasInterrupted <- liftIO . modifyMVar reloadComplete $ \s ->
                            return (ReloadComplete, s /= ReloadRunning)
                        unless (any isError errs || wasInterrupted || not (maybe False isToolPrompt lastOutput)) $ do
                            (autoPack, cmd) <- readIDE autoCommand
                            when (autoPack == (pjFile project, ipdCabalFile package)) cmd
                            compile'
            Nothing -> compile'
        )
        (\(e :: SomeException) -> sysMessage Normal (T.pack $ show e))
    compile :: [CompilerFlavor] -> IDEAction
    compile [] = continuation True
    compile (compiler:compilers) =
        runCabalBuild compiler backgroundBuild jumpToWarnings withoutLinking (project, package) $ \f ->
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
      OpenHandle pid -> signalProcess sigKILL pid
      ClosedHandle _ -> return ()
#else
killProcess = terminateProcess
#endif

packageDoc :: PackageAction
packageDoc = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ packageDoc' False True (project, package) (\ _ -> return ())

packageDoc' :: Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
packageDoc' backgroundBuild jumpToWarnings (project, package) continuation = do
    prefs     <- readIDE prefs
    catchIDE (do
        let dir = ipdPackageDir package
        flags <- case pjTool project of
                        StackTool -> return ["haddock", "--no-haddock-deps"]
                        CabalTool -> do
                            (buildDir, _, cabalVer) <- liftIO $ cabalProjectBuildDir (pjDir project) "dist-newstyle"
                            case cabalVer of
                                Just v | "1.24." `isPrefixOf` v ->
                                    return
                                        [ "act-as-setup"
                                        , "--"
                                        , "haddock"
                                        , T.pack ("--builddir=" <> (buildDir </>
                                            T.unpack (packageIdentifierToString $ ipdPackageId package)))]
                                _ -> return ["new-haddock"]
        withToolCommand project GHC (flags <> ipdHaddockFlags package) $ \(cmd, args, nixEnv) ->
            runExternalTool' (__ "Documenting") cmd
                args dir (M.toList <$> nixEnv) $ do
                mbLastOutput <- C.getZipSink $ const <$> C.ZipSink sinkLast <*> (C.ZipSink $
                    logOutputForBuild project (LogCabal $ ipdCabalFile package) backgroundBuild jumpToWarnings)
                lift $ postAsyncIDE reloadDoc
                lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess)))
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

packageClean :: PackageAction
packageClean = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ packageClean' (project, package) (\ _ -> return ())

packageClean' :: (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
packageClean' (project, package) continuation = do
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'

    let dir = ipdPackageDir package
        projectRoot = pjDir project
    case pjTool project of
        CabalTool -> do
            cleanCabal "dist-newstyle"
            cleanCabal "dist-ghcjs"
        StackTool ->
            runExternalTool' (__ "Cleaning")
                            (pjToolCommand' project)
                            ["clean"]
                            dir Nothing $ do
                mbLastOutput <- C.getZipSink $ const <$> C.ZipSink sinkLast <*> C.ZipSink (logOutput logLaunch)
                lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess))
  where
    cleanCabal buildDir = do
        (buildDir, _, _) <- liftIO $ cabalProjectBuildDir (pjDir project) buildDir
        let packageBuildDir = buildDir
                        </> T.unpack (packageIdentifierToString $ ipdPackageId package)
        liftIO $ doesDirectoryExist packageBuildDir >>= \case
            True -> removeDirectoryRecursive packageBuildDir
            False -> return ()

packageCopy :: PackageAction
packageCopy = do
    package <- ask
    interruptSaveAndRun $ do
        logLaunch <- getDefaultLogLaunch
        showDefaultLogLaunch'

        catchIDE (do
            prefs       <- readIDE prefs
            window      <- getMainWindow
            mbDir       <- liftIO $ chooseDir window (__ "Select the target directory") Nothing
            case mbDir of
                Nothing -> return ()
                Just fp -> do
                    let dir = ipdPackageDir package
                    runExternalTool' (__ "Copying")
                                    "cabal"
                                    ["copy", "--destdir=" <> T.pack fp]
                                    dir Nothing
                                    (logOutput logLaunch))
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

packageInstall :: PackageAction
packageInstall = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ packageInstall' (project, package) (\ _ -> return ())

packageInstall' :: (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
packageInstall' (project, package) continuation = do
    prefs     <- readIDE prefs
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'

    if native prefs && pjTool project == StackTool
        then catchIDE (do
            let dir = ipdPackageDir package
            runExternalTool' (__ "Installing")
                             (case pjTool project of
                                StackTool -> "stack"
                                CabalTool -> "echo" {-cabalCommand prefs-})
                             ((case pjTool project of
                                StackTool -> "install" : "--stack-yaml" : T.pack (makeRelative dir $ pjFile project) : ipdBuildFlags package
                                CabalTool -> ["TODO run cabal new-install"]) ++ ipdInstallFlags package)
                             dir Nothing $ do
                    mbLastOutput <- C.getZipSink $ (const <$> C.ZipSink sinkLast) <*> C.ZipSink (logOutput logLaunch)
                    lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess)))
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)
        else continuation True

packageRun :: PackageAction
packageRun = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ packageRun' True (project, package)

packageEnv :: MonadIO m => IDEPackage -> [(String, String)] -> m [(String, String)]
packageEnv package env =
    return $ (T.unpack (ipdPackageName package) <> "_datadir", ipdPackageDir package) : env

packageRun' :: Bool -> (Project, IDEPackage) -> IDEAction
packageRun' removeGhcjsFlagIfPresent (project, package) =
    if removeGhcjsFlagIfPresent && "--ghcjs" `elem` ipdConfigFlags package
        then do
            window <- liftIDE getMainWindow
            md <- new' MessageDialog [
                    constructDialogUseHeaderBar 0,
                    constructMessageDialogButtons ButtonsTypeCancel]
            setMessageDialogMessageType md MessageTypeQuestion
            setMessageDialogText md $ __ "Package is configured to use GHCJS.  Would you like to remove --ghcjs from the configure flags and rebuild?"
            windowSetTransientFor md (Just window)
            dialogAddButton' md (__ "Use _GHC") (AnotherResponseType 1)
            dialogSetDefaultResponse' md (AnotherResponseType 1)
            setWindowWindowPosition md WindowPositionCenterOnParent
            resp <- dialogRun' md
            widgetDestroy md
            case resp of
                AnotherResponseType 1 -> do
                    let packWithNewFlags = package { ipdConfigFlags = filter (/="--ghcjs") $ ipdConfigFlags package }
                    changePackage packWithNewFlags
                    liftIO $ writeFlags (dropExtension (ipdCabalFile packWithNewFlags) ++ leksahFlagFileExtension) packWithNewFlags
                    packageConfig' (project, packWithNewFlags) $ \ ok -> when ok $
                        packageRun' False (project, packWithNewFlags)
                _  -> return ()
        else liftIDE $ catchIDE (do
            pd <- readAndFlattenPackageDescription package
            mbComponent <- readIDE activeComponent
            let exe = exeToRun mbComponent $ executables pd
            let defaultLogName = ipdPackageName package
                logName = fromMaybe defaultLogName . listToMaybe $ map (T.pack . unUnqualComponentName . exeName) exe
            (logLaunch,logName) <- buildLogLaunchByName logName
            showLog
            M.lookup (pjFile project, ipdCabalFile package) <$> readIDE debugState >>= \case
                Nothing -> do
                    prefs <- readIDE prefs
                    let dir = ipdPackageDir package
                    case pjTool project of
                        StackTool -> IDE.Package.runPackage (addLogLaunchData logName logLaunch)
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
                        CabalTool -> do
                            (buildDir, cDir, _) <- liftIO $ cabalProjectBuildDir (pjDir project) "dist-newstyle"
                            env <- packageEnv package =<< liftIO getEnvironment
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
                Just debug ->
                    -- TODO check debug package matches active package
                    runDebug (do
                        case exe of
                            [Executable {exeName = name, modulePath = mainFilePath}] ->
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

packageRunJavaScript :: PackageAction
packageRunJavaScript = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ packageRunJavaScript' True (project, package)

packageRunJavaScript' :: Bool -> (Project, IDEPackage) -> IDEAction
packageRunJavaScript' addFlagIfMissing (project, package) =
    if pjTool project == StackTool
        then ideMessage Normal (__ "Leksah does not know how to run stack.yaml projects built with GHCJS.  Please use a cabal.project file instead (or send a pull request to fix Leksah).")
        else do
            prefs' <- readIDE prefs
            if addFlagIfMissing && not (javaScript prefs')
                then do
                    window <- liftIDE getMainWindow
                    md <- new' MessageDialog [
                            constructDialogUseHeaderBar 0,
                            constructMessageDialogButtons ButtonsTypeCancel]
                    setMessageDialogMessageType md MessageTypeQuestion
                    setMessageDialogText md $ __ "Would you like to enable the GHCJS as a build target and rebuild?"
                    windowSetTransientFor md (Just window)
                    dialogAddButton' md (__ "Enable _GHCJS") (AnotherResponseType 1)
                    dialogSetDefaultResponse' md (AnotherResponseType 1)
                    setWindowWindowPosition md WindowPositionCenterOnParent
                    resp <- dialogRun' md
                    widgetDestroy md
                    case resp of
                        AnotherResponseType 1 -> do
                            setJavaScriptToggled True
                            buildPackage False False False (project, package) $ \ ok -> when ok $
                                packageRunJavaScript' False (project, package)
                        _  -> return ()
                else liftIDE $ buildPackage False False True (project, package) $ \ ok -> when ok $ liftIDE $ catchIDE (do
                        ideR        <- ask
                        maybeDebug   <- readIDE debugState
                        pd <- readAndFlattenPackageDescription package
                        mbComponent <- readIDE activeComponent
                        let exe = exeToRun mbComponent $ executables pd
                        let defaultLogName = ipdPackageName package
                            logName = fromMaybe defaultLogName . listToMaybe $ map (T.pack . unUnqualComponentName . exeName) exe
                        (logLaunch,logName) <- buildLogLaunchByName logName
                        let dir = ipdPackageDir package
                            projectRoot = pjDir project
                        case exe ++ executables pd of
                            (Executable {exeName = name} : _) -> liftIDE $ do
                                (buildDir, cDir, _) <- liftIO $ cabalProjectBuildDir (pjDir project) "dist-ghcjs"
                                let path' c = buildDir
                                            </> T.unpack (packageIdentifierToString $ ipdPackageId package)
                                            </> c </> unUnqualComponentName name </> unUnqualComponentName name <.> "jsexe" </> "index.html"

                                path <- liftIO $ doesFileExist (path' "build") >>= \case
                                    True -> return $ path' "build"
                                    False -> return . path' $ cDir "x" (unUnqualComponentName name)

                                postAsyncIDE $ do
                                    loadOutputHtmlFile path
                                    getOutputPane Nothing  >>= \ p -> displayPane p False
                              `catchIDE`
                                (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

                            _ -> return ())
                        (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

packageTest :: PackageAction
packageTest = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ packageTest' False True (project, package) (\ _ -> return ())

packageTest' :: Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
packageTest' backgroundBuild jumpToWarnings (project, package) continuation =
    if "--enable-tests" `elem` ipdConfigFlags package
        then do
            removeTestLogRefs (LogCabal $ ipdCabalFile package)
            packageRunDocTests backgroundBuild jumpToWarnings (project, package) $ \ok ->
                when ok $ do
                    pd <- readAndFlattenPackageDescription package
                    runTests $ testSuites pd
          `catchIDE`
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)
        else continuation True
  where
    runTests :: [TestSuite] -> IDEAction
    runTests [] = continuation True
    runTests (test:rest) =
        packageRunComponent (CTest test) backgroundBuild jumpToWarnings (project, package) (\ok ->
            when ok $ runTests rest)

packageRunDocTests :: Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
packageRunDocTests backgroundBuild jumpToWarnings (project, package) continuation =
    case pjTool project of
        StackTool -> do
            ideMessage Normal "Skipping automatic doctests (not implemented for stack yet)."
            continuation True
        CabalTool -> do
            let dir = ipdPackageDir package
            logLaunch <- getDefaultLogLaunch
            showDefaultLogLaunch'
            catchIDE (do
                prefs <- readIDE prefs
                let projectFile = pjFile project
                ghcVersion <- liftIO getDefaultGhcVersion
                packageDBs <- liftIO $ getPackageDBs' ghcVersion (Just projectFile)
                let pkgId = packageIdentifierToString $ ipdPackageId package
                (buildDir, _, cabalVer) <- liftIO $ cabalProjectBuildDir (pjDir project) "dist-newstyle"
                let args = [ "act-as-setup"
                           , "--"
                           , "doctest"
                           , T.pack ("--builddir=" <> (buildDir </> T.unpack pkgId))]
                withToolCommand project GHC (args ++ ipdTestFlags package) $ \(cmd, args', nixEnv) ->
                    runExternalTool' (__ "Doctest")
                            cmd args' dir (Just $ [("GHC_PACKAGE_PATH", intercalate  [searchPathSeparator] packageDBs)] <> maybe [] M.toList nixEnv) $ do
                            (mbLastOutput, _) <- C.getZipSink $ (,)
                                <$> C.ZipSink sinkLast
                                <*> (C.ZipSink $ logOutputForBuild project (LogCabal $ ipdCabalFile package) backgroundBuild jumpToWarnings)
                            lift $ do
                                errs <- readIDE errorRefs
                                when (mbLastOutput == Just (ToolExit ExitSuccess)) $ continuation True)
                    (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

packageRunComponent :: Component -> Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
packageRunComponent (CLib _) _ _ _ _ = error "packageRunComponent"
packageRunComponent component backgroundBuild jumpToWarnings (project, package) continuation = do
    let (cType, name, command) = case component of
                    CLib _ -> error "packageRunComponent"
                    CExe exe -> ("x", exeName exe, "run")
                    CTest test -> ("t", testName test, "test")
                    CBench bench -> ("b", benchmarkName bench, "bench")
#if MIN_VERSION_Cabal(2,0,0)
                    CFLib flib -> ("f", foreignLibName flib, "flib") -- TODO check if "f" is correct
#endif
        dir = ipdPackageDir package
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'
    catchIDE (do
        prefs <- readIDE prefs
        let projectFile = pjFile project
        ghcVersion <- liftIO getDefaultGhcVersion
        packageDBs <- liftIO $ getPackageDBs' ghcVersion (Just projectFile)
        let pkgId = packageIdentifierToString $ ipdPackageId package
            pkgName = ipdPackageName package
        pjFileArgs <- projectFileArguments project dir
        let args = case pjTool project of
                        StackTool -> [command] <> pjFileArgs <> [pkgName <> ":" <> T.pack (unUnqualComponentName name)]
                        CabalTool -> ["new-" <> command] <> pjFileArgs <> [pkgName <> ":" <> T.pack (unUnqualComponentName name)]
        withToolCommand project GHC (args ++ ipdTestFlags package) $ \(cmd, args', nixEnv) ->
            runExternalTool' (__ "Run " <> T.pack (unUnqualComponentName name))
                    cmd args' dir (M.toList <$> nixEnv) $ do
                    (mbLastOutput, _) <- C.getZipSink $ (,)
                        <$> C.ZipSink sinkLast
                        <*> (C.ZipSink $ logOutputForBuild project (LogCabal $ ipdCabalFile package) backgroundBuild jumpToWarnings)
                    lift $ do
                        errs <- readIDE errorRefs
                        when (mbLastOutput == Just (ToolExit ExitSuccess)) $ continuation True)
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

-- | Run benchmarks as foreground action for current package
packageBench :: PackageAction
packageBench = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ packageBench' False True (project, package) (\ _ -> return ())

-- | Run benchmarks
packageBench' :: Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
packageBench' backgroundBuild jumpToWarnings (project, package) continuation =
    if "--enable-benchmarks" `elem` ipdConfigFlags package
        then do
            pd <- readAndFlattenPackageDescription package
            runBenchs $ benchmarks pd
          `catchIDE`
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)
        else continuation True
  where
    runBenchs :: [Benchmark] -> IDEAction
    runBenchs [] = continuation True
    runBenchs (bench:rest) =
        packageRunComponent (CBench bench) backgroundBuild jumpToWarnings (project, package) (\ok ->
            when ok $ runBenchs rest)

packageSdist :: PackageAction
packageSdist = do
    package <- ask
    interruptSaveAndRun $ do
        logLaunch <- getDefaultLogLaunch
        showDefaultLogLaunch'

        catchIDE (do
            prefs <- readIDE prefs
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
        projectRoot = pjDir project
    distDir <- case pjTool project of
                        StackTool -> do
                            --ask stack where its dist directory is
                            mvar <- liftIO newEmptyMVar
                            runExternalTool' "" "stack" ["path"] dir Nothing $ do
                                output <- CL.consume
                                liftIO . putMVar mvar $ head $ mapMaybe getDistOutput output
                            liftIO $ takeMVar mvar
                        CabalTool -> do
                            (buildDir, _, _) <- liftIO $ cabalProjectBuildDir (pjDir project) "dist-newstyle"
                            return $ buildDir </> T.unpack pkgId
    liftIDE $ do
        prefs   <- readIDE prefs
        let path = dir </> distDir
                        </> "doc/html"
                        </> T.unpack (ipdPackageName package)
                        </> "index.html"
        loadDoc . T.pack $ "file://" ++ path
        getDocumentation Nothing  >>= \ p -> displayPane p False
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
            -> C.Sink ToolOutput IDEM ()
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
    return (foldl' (\ a (from, to) -> T.replace from to a) template
        [   ("@License@"      , (T.pack .
#if MIN_VERSION_Cabal(2,2,0)
                                          show
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
addModToBuildInfoExe name _ x = x

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
                                    Just lib -> Just (delModFromBuildInfoLib moduleName
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
isExposedModule mn Nothing                             = False
isExposedModule mn (Just CondNode{condTreeData = lib}) = mn `elem` exposedModules lib

backgroundBuildToggled :: IDEAction
backgroundBuildToggled = do
    toggled <- getBackgroundBuildToggled
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){backgroundBuild = toggled}})

makeDocsToggled :: IDEAction
makeDocsToggled = do
    toggled <- getMakeDocs
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){makeDocs = toggled}})

runUnitTestsToggled :: IDEAction
runUnitTestsToggled = do
    toggled <- getRunUnitTests
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){runUnitTests = toggled}})

runBenchmarksToggled :: IDEAction
runBenchmarksToggled = do
    toggled <- getRunBenchmarks
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){runBenchmarks = toggled}})

nativeToggled :: IDEAction
nativeToggled = do
    toggled <- getNativeToggled
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){native = toggled}})

javaScriptToggled :: IDEAction
javaScriptToggled = do
    toggled <- getJavaScriptToggled
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){javaScript = toggled}})

makeModeToggled :: IDEAction
makeModeToggled = do
    toggled <- getMakeModeToggled
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){makeMode = toggled}})

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
interactiveFlags prefs =
    printEvldWithShowFlag (printEvldWithShow prefs)
  : breakOnExceptionFlag (breakOnException prefs)
  : breakOnErrorFlag (breakOnError prefs)
  : [printBindResultFlag $ printBindResult prefs]

debugStart :: PackageAction
debugStart = do
    project <- lift ask
    package <- ask
    let projectAndPackage = (pjFile project, ipdCabalFile package)
    liftIDE $ catchIDE (do
        ideRef     <- ask
        prefs'     <- readIDE prefs
        M.lookup projectAndPackage <$> readIDE debugState >>= \case
            Nothing -> do
                mbActiveComponent <- getActiveComponent project package
                let dir  = ipdPackageDir  package
                    name = ipdPackageName package
                pjFileArgs <- projectFileArguments project dir
                withToolCommand project GHC (
                        case pjTool project of
                            CabalTool -> [ "new-repl" ]
                                                <> pjFileArgs
                                                <> [ name <> maybe (":lib:" <> name) (":" <>) mbActiveComponent | ipdHasLibs package || isJust mbActiveComponent ]
                            StackTool -> [ "repl" ]
                                                <> pjFileArgs
                                                <> [ name <> maybe ":lib" (":" <>) mbActiveComponent ]) $ \(tool, args, nixEnv) -> do
                    let logOut = reflectIDEI (void (logOutputForBuild project (LogCabal $ ipdCabalFile package) True False)) ideRef
                        logIdle = reflectIDEI (C.getZipSink $ const <$> C.ZipSink (logIdleOutput project package) <*> C.ZipSink logOutputDefault) ideRef
                    ghci <- liftIO $ newGhci tool args dir nixEnv ("+c":"-ferror-spans":interactiveFlags prefs') logOut logIdle
                    liftIO $ executeGhciCommand ghci ":reload" logOut
                    modifyIDE_ (\ide -> ide {debugState = M.insert projectAndPackage (package, ghci) (debugState ide)})
                    triggerEventIDE (Sensitivity [(SensitivityInterpreting, True)])
                    triggerEventIDE (DebugStart projectAndPackage)
                    -- Fork a thread to wait for the output from the process to close
                    liftIO $ forkIO $ do
                        readMVar (outputClosed ghci)
                        (`reflectIDE` ideRef) . postSyncIDE $ do
                            modifyIDE_ (\ide -> ide {debugState = M.delete projectAndPackage (debugState ide)})
                            triggerEventIDE (Sensitivity [(SensitivityInterpreting, False)])
                            triggerEventIDE (DebugStop projectAndPackage)
                            return ()
                    return ()
            _ -> do
                sysMessage Normal (__ "Debugger already running")
                return ())
            (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

tryDebug :: DebugAction -> PackageAction
tryDebug f = do
    prefs <- readIDE prefs
    packageDebugState >>= \case
        Just d -> liftIDE $ runDebug f d
        _ | debug prefs -> do
                debugStart
                packageDebugState >>=
                    mapM_ (liftIDE . postAsyncIDE . runDebug f)
          | otherwise -> do
            window <- liftIDE getMainWindow
            md <- new' MessageDialog [
                    constructDialogUseHeaderBar 0,
                    constructMessageDialogButtons ButtonsTypeCancel]
            setMessageDialogMessageType md MessageTypeQuestion
            setMessageDialogText md $ __ "GHCi debugger is not running."
            windowSetTransientFor md (Just window)
            dialogAddButton' md (__ "_Start GHCi") (AnotherResponseType 1)
            dialogSetDefaultResponse' md (AnotherResponseType 1)
            setWindowWindowPosition md WindowPositionCenterOnParent
            resp <- dialogRun' md
            widgetDestroy md
            case resp of
                AnotherResponseType 1 -> do
                    debugStart
                    liftIDE $ setDebugToggled True
                    packageDebugState >>=
                        mapM_ (liftIDE . postAsyncIDE . runDebug f)
                _  -> return ()

tryDebugQuiet :: DebugAction -> PackageAction
tryDebugQuiet f = do
    project <- lift ask
    package <- ask
    M.lookup (pjFile project, ipdCabalFile package) <$> readIDE debugState >>=
        mapM_ (liftIDE . runDebug f)

executeDebugCommand :: Text -> C.Sink ToolOutput IDEM () -> DebugAction
executeDebugCommand command handler = do
    (_, ghci) <- ask
    lift $ do
        ideR <- ask
        postAsyncIDE $ do
            triggerEventIDE (StatusbarChanged [CompartmentState command, CompartmentBuild True])
            return ()
        liftIO . executeGhciCommand ghci command $
            reflectIDEI (do
                lift . postSyncIDE $ do
                   triggerEventIDE (StatusbarChanged [CompartmentState "", CompartmentBuild False])
                   return ()
                handler) ideR

-- Includes non buildable
allBuildInfo' :: PackageDescription -> [BuildInfo]
allBuildInfo' pkg_descr = [ libBuildInfo lib       | Just lib <- [library pkg_descr] ]
                       ++ [ buildInfo exe          | exe <- executables pkg_descr ]
                       ++ [ testBuildInfo tst      | tst <- testSuites pkg_descr ]
                       ++ [ benchmarkBuildInfo tst | tst <- benchmarks pkg_descr ]
testMainPath (TestSuiteExeV10 _ f) = [f]
testMainPath _ = []

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

            let ipdModules          = Map.fromList $ myLibModules packageD ++ myExeModules packageD
                                        ++ myTestModules packageD ++ myBenchmarkModules packageD
                ipdMain             = [ (modulePath exe, buildInfo exe, False) | exe <- executables packageD ]
                                        ++ [ (f, bi, True) | TestSuite {testInterface = TestSuiteExeV10 _ f, testBuildInfo = bi} <- testSuites packageD ]
                                        ++ [ (f, bi, True) | Benchmark {benchmarkInterface = BenchmarkExeV10 _ f, benchmarkBuildInfo = bi} <- benchmarks packageD ]
                ipdExtraSrcs        = Set.fromList $ extraSrcFiles packageD
                ipdSrcDirs          = case nub $ concatMap hsSourceDirs (allBuildInfo' packageD) of
                                            [] -> [".","src"]
                                            l -> l
                ipdExes             = [ T.pack . unUnqualComponentName $ exeName e | e <- executables packageD ]
                ipdExtensions       = nub $ concatMap oldExtensions (allBuildInfo' packageD)
                ipdTests            = [ T.pack . unUnqualComponentName $ testName t | t <- testSuites packageD ]
                ipdBenchmarks       = [ T.pack . unUnqualComponentName $ benchmarkName b | b <- benchmarks packageD ]
                ipdPackageId        = package packageD
                ipdDepends          = buildDepends packageD
                ipdHasLibs          = hasLibs packageD
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
                ipdSandboxSources   = []
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


extractCabalPackageList :: Text -> [String]
extractCabalPackageList = extractList "packages:" <> extractList "optional-packages:"
  where
    extractList :: Text -> Text -> [String]
    extractList listName = map (dirOnly .T.unpack . T.dropWhile (==' ')) .
                          takeWhile (" " `T.isPrefixOf`) .
                          drop 1 .
                          dropWhile (/= listName) .
                          filter (not . T.null) .
                          map (T.pack . stripCabalComments . T.unpack) .
                          T.lines

    stripCabalComments :: String -> String
    stripCabalComments "" = ""
    stripCabalComments ('-':'-':_) = ""
    stripCabalComments (x:xs) = x:stripCabalComments xs
    dirOnly :: FilePath -> FilePath
    dirOnly f = if takeExtension f == "cabal" then dropFileName f else f

ideProjectFromPath :: FilePath -> IDEM (Maybe Project)
ideProjectFromPath filePath =
    case (case takeExtension filePath of
                ".project" -> Just (CabalTool, extractCabalPackageList)
                ".yaml" -> Just (StackTool, extractStackPackageList)
                _ -> Nothing) of
        Just (tool, extractPackageList) -> do
            let dir = takeDirectory filePath
            patterns <- liftIO $ map (Glob.compile . (</> "*.cabal")) . extractPackageList <$> T.readFile filePath
            cabalFiles <- liftIO $ mapM canonicalizePath =<< map (dir </>) . concat <$>
                              Glob.globDir patterns dir
            packages <- fmap catMaybes . mapM idePackageFromPath' $ nub cabalFiles
            return . Just $ Project { pjTool = tool, pjFile = filePath, pjPackageMap = mkPackageMap packages }
          `catchIDE`
             (\(e :: SomeException) -> do
                ideMessage Normal . T.pack $ show e
                return Nothing)
        Nothing -> return Nothing

--refreshPackage :: C.Sink ToolOutput IDEM () -> PackageM (Maybe IDEPackage)
--refreshPackage log = do
--    package <- ask
--    liftIDE $ do
--        mbUpdatedPack <- idePackageFromPath log (ipdCabalFile package)
--        case mbUpdatedPack of
--            Just updatedPack -> do
--                changePackage updatedPack
--                triggerEventIDE $ WorkspaceChanged False True
--                return mbUpdatedPack
--            Nothing -> do
--                postAsyncIDE $ ideMessage Normal (__ "Can't read package file")
--                return Nothing

