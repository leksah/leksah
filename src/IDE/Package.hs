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
    packageConfig
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
import Distribution.PackageDescription.Parse
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

import IDE.Core.State
import IDE.Utils.GUIUtils
import IDE.Utils.CabalUtils (writeGenericPackageDescription')
import IDE.Pane.Log
import IDE.Pane.PackageEditor
import IDE.Pane.SourceBuffer
import IDE.Pane.PackageFlags (writeFlags, readFlags)
import Distribution.Text (display)
import IDE.Utils.FileUtils
       (cabalProjectBuildDir, getConfigFilePathForLoad, getPackageDBs')
import IDE.LogRef
import Distribution.ModuleName (ModuleName(..))
import Data.List
       (intercalate, isInfixOf, nub, foldl', delete, find)
import IDE.Utils.Tool (ToolOutput(..), runTool, newGhci, ToolState(..), toolline, ProcessHandle, executeGhciCommand)
import qualified Data.Set as  Set (fromList)
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
import IDE.Pane.WebKit.Output (loadOutputUri, getOutputPane)
import System.Log.Logger (debugM)
import System.Process.Vado (getMountPoint, vado, readSettings)
import qualified Data.Text as T
       (reverse, all, null, dropWhile, lines, isPrefixOf, stripPrefix,
        replace, unwords, takeWhile, pack, unpack, isInfixOf)
import IDE.Utils.ExternalTool (runExternalTool', runExternalTool, isRunning, interruptBuild)
import Text.PrinterParser (writeFields)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text.IO as T (readFile)
import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)
import IDE.Metainfo.Provider (updateSystemInfo)
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
import IDE.Utils.CabalProject (getCabalProjectPackages)
import System.Environment (getEnvironment)
import Distribution.Simple.LocalBuildInfo
       (Component(..), Component)
import Distribution.Simple.Utils (writeUTF8File)
import Distribution.Compiler (CompilerFlavor(..))
import qualified Data.Map as M (member, delete, insert, lookup)
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.ForeignLib (foreignLibName)
import Distribution.Types.UnqualComponentName
       (UnqualComponentName(..), mkUnqualComponentName,
        unUnqualComponentName)
#endif

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

runCabalBuild :: CompilerFlavor -> Bool -> Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
runCabalBuild compiler backgroundBuild jumpToWarnings withoutLinking (project, package) continuation = do
    prefs <- readIDE prefs
    pd <- liftIO $ fmap flattenPackageDescription
                     (readPackageDescription normal (ipdCabalFile package))
    let dir = ipdPackageDir package
        pkgName = ipdPackageName package

    isActiveProject   <- maybe False (on (==) pjFile project) <$> readIDE activeProject
    isActivePackage   <- (isActiveProject &&) . maybe False (on (==) ipdCabalFile package) <$> readIDE activePack
    mbActiveComponent <- readIDE activeComponent
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
                    StackTool -> ["build", "--stack-yaml", T.pack (makeRelative dir $ pjFile project), ipdPackageName package]
                    CabalTool -> ["new-build", "--project-file", T.pack (makeRelative dir $ pjFile project)])
                ++ ["--ghcjs" | compiler == GHCJS]
                ++ ["--with-ld=false" | pjTool project == CabalTool && backgroundBuild && withoutLinking]
                ++ (if isActivePackage then maybeToList mbActiveComponent else [])
                ++ flagsForTests
                ++ flagsForBenchmarks
                ++ ipdBuildFlags package
    runExternalTool' (__ "Building") (pjToolCommand project) args dir Nothing $ do
        (mbLastOutput, _) <- C.getZipSink $ (,)
            <$> C.ZipSink sinkLast
            <*> (C.ZipSink $ logOutputForBuild project package backgroundBuild jumpToWarnings)
        lift $ do
            errs <- readIDE errorRefs
            continuation (mbLastOutput == Just (ToolExit ExitSuccess))

--isConfigError :: Monad m => C.Sink ToolOutput m Bool
--isConfigError = CL.foldM (\a b -> return $ a || isCErr b) False
--    where
--    isCErr (ToolError str) = str1 `T.isInfixOf` str || str2 `T.isInfixOf` str || str3 `T.isInfixOf` str
--    isCErr _ = False
--    str1 = __ "Run the 'configure' command first"
--    str2 = __ "please re-configure"
--    str3 = __ "cannot satisfy -package-id"

buildPackage :: Bool -> Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
buildPackage backgroundBuild jumpToWarnings withoutLinking (project, package) continuation = catchIDE (do
    liftIO $ debugM "leksah" "buildPackage"
    ideR      <- liftIDE ask
    prefs     <- readIDE prefs
    let compile' = compile ([GHC | native prefs || debug prefs] ++ [GHCJS | javaScript prefs && pjTool project == CabalTool])
    M.member (pjFile project, ipdCabalFile package) <$> readIDE debugState >>= \case
        False | debug prefs -> readIDE workspace >>= mapM_ (runWorkspace $ runProject (IDE.Core.State.runPackage debugStart package) project)
        _ -> return ()
    M.lookup (pjFile project, ipdCabalFile package) <$> readIDE debugState >>= \case
        Just debug@(_, ghci) -> do
            -- TODO check debug package matches active package
            ready <- liftIO $ isEmptyMVar (currentToolCommand ghci)
            if ready
                then do
                    let dir = ipdPackageDir package
                    when (saveAllBeforeBuild prefs) (do fileSaveAll belongsToWorkspace'; return ())
                    (`runDebug` debug) . executeDebugCommand ":reload" $ do
                        errs <- logOutputForBuild project package backgroundBuild jumpToWarnings
                        unless (any isError errs) . lift $ do
                            cmd <- readIDE autoCommand
                            postSyncIDE cmd
                            compile'
                else do
                    timeoutAdd PRIORITY_LOW 500 (do
                        reflectIDE (do
                            tb <- readIDE triggerBuild
                            void . liftIO $ tryPutMVar tb ()
                            return False) ideR
                        return False)
                    return ()
        Nothing -> compile'
    )
    (\(e :: SomeException) -> sysMessage Normal (T.pack $ show e))
  where
    compile :: [CompilerFlavor] -> IDEAction
    compile [] = continuation True
    compile (compiler:compilers) = do
        ideR      <- liftIDE ask
        prefs     <- readIDE prefs
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
                when (saveAllBeforeBuild prefs) . liftIDE . void $ fileSaveAll belongsToWorkspace'
                runCabalBuild compiler backgroundBuild jumpToWarnings withoutLinking (project, package) $ \f ->
                    when f $ do
                        mbURI <- readIDE autoURI
                        case mbURI of
                            Just uri -> postSyncIDE . loadOutputUri $ T.unpack uri
                            Nothing  -> return ()
                        compile compilers

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
                            (buildDir, _) <- liftIO $ cabalProjectBuildDir (pjDir project)
                            return
                                [ "act-as-setup"
                                , "--"
                                , "haddock"
                                , T.pack ("--builddir=" <> (buildDir </>
                                    T.unpack (packageIdentifierToString $ ipdPackageId package)))]
        runExternalTool' (__ "Documenting") (pjToolCommand project)
            (flags <> ipdHaddockFlags package) dir Nothing $ do
            mbLastOutput <- C.getZipSink $ const <$> C.ZipSink sinkLast <*> (C.ZipSink $
                logOutputForBuild project package backgroundBuild jumpToWarnings)
            lift $ postAsyncIDE reloadDoc
            lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess)))
        (\(e :: SomeException) -> print e)

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
            (buildDir, _) <- liftIO $ cabalProjectBuildDir (pjDir project)
            let packageBuildDir = buildDir
                            </> T.unpack (packageIdentifierToString $ ipdPackageId package)
            liftIO $ doesDirectoryExist packageBuildDir >>= \case
                True -> removeDirectoryRecursive packageBuildDir
                False -> return ()
        StackTool ->
            runExternalTool' (__ "Cleaning")
                            (pjToolCommand project)
                            ["clean"]
                            dir Nothing $ do
                mbLastOutput <- C.getZipSink $ const <$> C.ZipSink sinkLast <*> C.ZipSink (logOutput logLaunch)
                lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess))

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
            (\(e :: SomeException) -> print e)

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

    catchIDE (do
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
        (\(e :: SomeException) -> print e)

packageRun :: PackageAction
packageRun = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ packageRun' True (project, package)

packageEnv :: MonadIO m => IDEPackage -> m [(String, String)]
packageEnv package = do
    env <- liftIO getEnvironment
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
            ideR        <- ask
            pd <- liftIO $ fmap flattenPackageDescription
                             (readPackageDescription normal (ipdCabalFile package))
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
                            (buildDir, cDir) <- liftIO $ cabalProjectBuildDir (pjDir project)
                            env <- packageEnv package
                            case exe ++ executables pd of
                                [] -> return ()
                                (Executable {exeName = name} : _) -> do
                                    let exePath = buildDir
                                                    </> T.unpack (packageIdentifierToString $ ipdPackageId package)
                                                    </> cDir (unUnqualComponentName name) </> unUnqualComponentName name </> unUnqualComponentName name
                                    IDE.Package.runPackage (addLogLaunchData logName logLaunch)
                                                           (T.pack $ printf (__ "Running %s") (T.unpack logName))
                                                           exePath
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
            (\(e :: SomeException) -> print e)

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
    if addFlagIfMissing && ("--ghcjs" `notElem` ipdConfigFlags package)
        then do
            window <- liftIDE getMainWindow
            md <- new' MessageDialog [
                    constructDialogUseHeaderBar 0,
                    constructMessageDialogButtons ButtonsTypeCancel]
            setMessageDialogMessageType md MessageTypeQuestion
            setMessageDialogText md $ __ "Package is not configured to use GHCJS.  Would you like to add --ghcjs to the configure flags and rebuild?"
            windowSetTransientFor md (Just window)
            dialogAddButton' md (__ "Use _GHCJS") (AnotherResponseType 1)
            dialogSetDefaultResponse' md (AnotherResponseType 1)
            setWindowWindowPosition md WindowPositionCenterOnParent
            resp <- dialogRun' md
            widgetDestroy md
            case resp of
                AnotherResponseType 1 -> do
                    let packWithNewFlags = package { ipdConfigFlags = "--ghcjs" : ipdConfigFlags package }
                    changePackage packWithNewFlags
                    liftIO $ writeFlags (dropExtension (ipdCabalFile packWithNewFlags) ++ leksahFlagFileExtension) packWithNewFlags
                    packageConfig' (project, packWithNewFlags) $ \ ok -> when ok $
                        packageRunJavaScript' False (project, packWithNewFlags)
                _  -> return ()
        else liftIDE $ buildPackage False False True (project, package) $ \ ok -> when ok $ liftIDE $ catchIDE (do
                ideR        <- ask
                maybeDebug   <- readIDE debugState
                pd <- liftIO $ fmap flattenPackageDescription
                                 (readPackageDescription normal (ipdCabalFile package))
                mbComponent <- readIDE activeComponent
                let exe = exeToRun mbComponent $ executables pd
                let defaultLogName = ipdPackageName package
                    logName = fromMaybe defaultLogName . listToMaybe $ map (T.pack . unUnqualComponentName . exeName) exe
                (logLaunch,logName) <- buildLogLaunchByName logName
                let dir = ipdPackageDir package
                    projectRoot = pjDir project
                prefs <- readIDE prefs
                case exe ++ executables pd of
                    (Executable {exeName = name} : _) -> liftIDE $ do
                        (buildDir, cDir) <- liftIO $ cabalProjectBuildDir (pjDir project)
                        let path = buildDir
                                    </> T.unpack (packageIdentifierToString $ ipdPackageId package)
                                    </> cDir (unUnqualComponentName name) </> unUnqualComponentName name </> unUnqualComponentName name <.> "jsexe" </> "index.html"
                        postAsyncIDE $ do
                            loadOutputUri ("file:///" ++ path)
                            getOutputPane Nothing  >>= \ p -> displayPane p False
                      `catchIDE`
                        (\(e :: SomeException) -> print e)

                    _ -> return ())
                (\(e :: SomeException) -> print e)

packageTest :: PackageAction
packageTest = do
    project <- lift ask
    package <- ask
    interruptSaveAndRun $ packageTest' False True (project, package) (\ _ -> return ())

packageTest' :: Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
packageTest' backgroundBuild jumpToWarnings (project, package) continuation =
    if "--enable-tests" `elem` ipdConfigFlags package
        then do
            removeTestLogRefs (ipdPackageDir package)
            pd <- liftIO $ fmap flattenPackageDescription
                             (readPackageDescription normal (ipdCabalFile package))
            runTests $ testSuites pd
        else continuation True
  where
    runTests :: [TestSuite] -> IDEAction
    runTests [] = continuation True
    runTests (test:rest) =
        packageRunComponent (CTest test) backgroundBuild jumpToWarnings (project, package) (\ok ->
            when ok $ runTests rest)

packageRunComponent :: Component -> Bool -> Bool -> (Project, IDEPackage) -> (Bool -> IDEAction) -> IDEAction
packageRunComponent (CLib _) _ _ _ _ = error "packageRunComponent"
packageRunComponent component backgroundBuild jumpToWarnings (project, package) continuation = do
    let name = case component of
                    CLib _ -> error "packageRunComponent"
                    CExe exe -> exeName exe
                    CTest test -> testName test
                    CBench bench -> benchmarkName bench
#if MIN_VERSION_Cabal(2,0,0)
                    CFLib flib -> foreignLibName flib
#endif
        command = case component of
                    CLib _ -> error "packageRunComponent"
                    CExe exe -> "run"
                    CTest test -> "test"
                    CBench bench -> "bench"
#if MIN_VERSION_Cabal(2,0,0)
                    CFLib flib -> "flib"
#endif
        dir = ipdPackageDir package
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'
    catchIDE (do
        prefs <- readIDE prefs
        let projectRoot = pjDir project
        ghcVersion <- liftIO getDefaultGhcVersion
        packageDBs <- liftIO $ getPackageDBs' ghcVersion dir
        let pkgId = packageIdentifierToString $ ipdPackageId package
            pkgName = ipdPackageName package
            args = case pjTool project of
                        StackTool -> [command, pkgName <> ":" <> T.pack (unUnqualComponentName name)]
                        CabalTool -> []
        (cmd, mbEnv) <- case pjTool project of
            StackTool -> return ("stack", Nothing)
            CabalTool -> do
                (buildDir, cDir) <- liftIO $ cabalProjectBuildDir (pjDir project)
                env <- packageEnv package
                return
                    ( buildDir </> T.unpack pkgId </> cDir (unUnqualComponentName name)
                        </> unUnqualComponentName name </> unUnqualComponentName name
                    , Just $ ("GHC_PACKAGE_PATH", intercalate [searchPathSeparator] packageDBs) : env
                    )
        runExternalTool' (__ "Run " <> T.pack (unUnqualComponentName name)) cmd (args
            ++ ipdBuildFlags package ++ ipdTestFlags package) dir mbEnv $ do
                (mbLastOutput, _) <- C.getZipSink $ (,)
                    <$> C.ZipSink sinkLast
                    <*> (C.ZipSink $ logOutputForBuild project package backgroundBuild jumpToWarnings)
                lift $ do
                    errs <- readIDE errorRefs
                    when (mbLastOutput == Just (ToolExit ExitSuccess)) $ continuation True)
        (\(e :: SomeException) -> print e)

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
            pd <- liftIO $ fmap flattenPackageDescription
                             (readPackageDescription normal (ipdCabalFile package))
            runBenchs $ benchmarks pd
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
            (\(e :: SomeException) -> print e)


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
                            (buildDir, _) <- liftIO $ cabalProjectBuildDir (pjDir project)
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
        (\(e :: SomeException) -> print e)
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
        Just p  -> do
            ideR <- ask
            reifyIDE (\ideR -> catch (do
                pd <- readPackageDescription normal (ipdCabalFile p)
                return (Just (flattenPackageDescription pd,ipdCabalFile p)))
                    (\(e :: SomeException) -> do
                        reflectIDE (ideMessage Normal (__ "Can't load package " <> T.pack (show e))) ideR
                        return Nothing))

getEmptyModuleTemplate :: PackageDescription -> Text -> IO Text
getEmptyModuleTemplate pd modName = getModuleTemplate "module" pd modName "" ""

getModuleTemplate :: FilePath -> PackageDescription -> Text -> Text -> Text -> IO Text
getModuleTemplate templateName pd modName exports body = catch (do
    dataDir  <- getDataDir
    filePath <- getConfigFilePathForLoad (templateName <> leksahTemplateFileExtension) Nothing dataDir
    template <- T.readFile filePath
    return (foldl' (\ a (from, to) -> T.replace from to a) template
        [   ("@License@"      , (T.pack . display . license) pd)
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
    liftIDE $ reifyIDE (\ideR -> catch (do
        gpd <- readPackageDescription normal (ipdCabalFile p)
        let npd = trace (show gpd) foldr addModule gpd locations
        writeGenericPackageDescription' (ipdCabalFile p) npd)
           (\(e :: SomeException) -> do
            reflectIDE (ideMessage Normal (__ "Can't update package " <> T.pack (show e))) ideR
            return ()))
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
    liftIDE $ reifyIDE (\ideR -> catch (do
        gpd <- readPackageDescription normal (ipdCabalFile p)
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
            reflectIDE (ideMessage Normal (__ "Can't update package " <> T.pack (show e))) ideR
            return ()))

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
                mbTarget <- readIDE activeComponent
                let dir  = ipdPackageDir  package
                    name = ipdPackageName package
                    (tool, args) = case pjTool project of
                        CabalTool -> ("cabal", [ "new-repl"
                                               , "--project-file"
                                               , T.pack . makeRelative dir $ pjFile project
                                               , name <> maybe (":lib:" <> name) (":" <>) mbTarget
                                               ])
                        StackTool -> ("stack", [ "repl"
                                               , "--stack-yaml"
                                               , T.pack . makeRelative dir $ pjFile project
                                               , name <> maybe ":lib" (":" <>) mbTarget
                                               ])
                ghci <- reifyIDE $ \ideR -> newGhci tool args dir (interactiveFlags prefs')
                    $ reflectIDEI (void (logOutputForBuild project package True False)) ideR
                modifyIDE_ (\ide -> ide {debugState = M.insert projectAndPackage (package, ghci) (debugState ide)})
                triggerEventIDE (Sensitivity [(SensitivityInterpreting, True)])
                triggerEventIDE (DebugStart projectAndPackage)
                -- Fork a thread to wait for the output from the process to close
                liftIO $ forkIO $ do
                    readMVar (outputClosed ghci)
                    (`reflectIDE` ideRef) . postSyncIDE $ do
                        modifyIDE_ (\ide -> ide {debugState = M.delete projectAndPackage (debugState ide), autoCommand = return ()})
                        triggerEventIDE (Sensitivity [(SensitivityInterpreting, False)])
                        triggerEventIDE (DebugStop projectAndPackage)
                        return ()
                return ()
            _ -> do
                sysMessage Normal (__ "Debugger already running")
                return ())
            (\(e :: SomeException) -> print e)

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
    mbPackageD <- reifyIDE (\ideR -> catch (do
        pd <- readPackageDescription normal ipdCabalFile
        return (Just (flattenPackageDescription pd)))
            (\ (e  :: SomeException) -> do
                reflectIDE (ideMessage Normal (__ "Can't activate package " <> T.pack (show e))) ideR
                return Nothing))
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
extractCabalPackageList = map (dirOnly .T.unpack . T.dropWhile (==' ')) .
                          takeWhile (" " `T.isPrefixOf`) .
                          drop 1 .
                          dropWhile (/= "packages:") .
                          filter (not . T.null) .
                          map (T.pack . stripCabalComments . T.unpack) .
                          T.lines
  where
    stripCabalComments :: String -> String
    stripCabalComments "" = ""
    stripCabalComments ('-':'-':_) = ""
    stripCabalComments (x:xs) = x:stripCabalComments xs
    dirOnly :: FilePath -> FilePath
    dirOnly f = if takeExtension f == "cabal" then dropFileName f else f

ideProjectFromPath :: FilePath -> IDEM (Maybe Project)
ideProjectFromPath filePath = do
    let toolInfo = case takeExtension filePath of
                        ".project" -> Just (CabalTool, extractCabalPackageList)
                        ".yaml" -> Just (StackTool, extractStackPackageList)
                        _ -> Nothing
    case toolInfo of
        Just (tool, extractPackageList) -> do
            let dir = takeDirectory filePath
            paths <- liftIO $ map (dir </>) . extractPackageList <$> T.readFile filePath
            packages <- catMaybes <$> forM paths (\path -> do
                exists <- liftIO (doesDirectoryExist path)
                if exists
                    then do
                        cpath <- liftIO $ canonicalizePath path
                        contents <- liftIO $ getDirectoryContents cpath
                        let mbCabalFile = find ((== ".cabal") . takeExtension) contents
                        when (isNothing mbCabalFile) $
                            ideMessage Normal ("Could not find cabal file for " <> T.pack cpath)
                        return (fmap (cpath </>) mbCabalFile)
                    else do
                        ideMessage Normal ("Path does not exist: " <> T.pack path)
                        return Nothing)
            packages <- fmap catMaybes . mapM idePackageFromPath' $ nub packages
            return . Just $ Project { pjTool = tool, pjFile = filePath, pjPackageMap = mkPackageMap packages }
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

