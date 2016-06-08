{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
,   runUnitTestsToggled
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

,   idePackageFromPath
,   refreshPackage

) where

import Distribution.Package hiding (depends,packageId)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Verbosity
import System.FilePath
import Control.Concurrent
import System.Directory
       (canonicalizePath, setCurrentDirectory, doesFileExist,
        getDirectoryContents, doesDirectoryExist)
import Prelude hiding (catch)
import Data.Char (isSpace)
import Data.Maybe
       (mapMaybe, listToMaybe, fromMaybe, isNothing, isJust, fromJust,
        catMaybes)
import Control.Exception (SomeException(..), catch)

import IDE.Core.State
import IDE.Utils.GUIUtils
import IDE.Pane.Log
import IDE.Pane.PackageEditor
import IDE.Pane.SourceBuffer
import IDE.Pane.PackageFlags (writeFlags, readFlags)
import Distribution.Text (display)
import IDE.Utils.FileUtils(getConfigFilePathForLoad)
import IDE.LogRef
import Distribution.ModuleName (ModuleName(..))
import Data.List (isInfixOf, nub, foldl', delete, find)
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
       (writeGenericPackageDescription)
import Debug.Trace (trace)
import IDE.Pane.WebKit.Documentation
       (getDocumentation, loadDoc, reloadDoc)
import IDE.Pane.WebKit.Output (loadOutputUri, getOutputPane)
import System.Log.Logger (debugM)
import System.Process.Vado (getMountPoint, vado, readSettings)
import qualified Data.Text as T
       (lines, isPrefixOf, stripPrefix, replace, unwords, takeWhile, pack,
        unpack, isInfixOf)
import IDE.Utils.ExternalTool (runExternalTool', runExternalTool, isRunning, interruptBuild)
import Text.PrinterParser (writeFields)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text.IO as T (readFile)
import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)
import IDE.Metainfo.Provider (updateSystemInfo)
import GI.GLib.Functions (timeoutAdd)
import GI.GLib.Constants (pattern PRIORITY_DEFAULT)
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
import IDE.Utils.CabalUtils (findProjectRoot)

printf :: PrintfType r => Text -> r
printf = S.printf . T.unpack

-- | Get the last item
sinkLast = CL.fold (\_ a -> Just a) Nothing

moduleInfo :: (a -> BuildInfo) -> (a -> [ModuleName]) -> a -> [(ModuleName, BuildInfo)]
moduleInfo bi mods a = map (\m -> (m, buildInfo)) $ mods a
    where buildInfo = bi a

myLibModules pd = case library pd of
                    Nothing -> []
                    Just l -> moduleInfo libBuildInfo libModules l
myExeModules pd = concatMap (moduleInfo buildInfo exeModules) (executables pd)
myTestModules pd = concatMap (moduleInfo testBuildInfo (otherModules . testBuildInfo)) (testSuites pd)
myBenchmarkModules pd = concatMap (moduleInfo benchmarkBuildInfo (otherModules . benchmarkBuildInfo)) (benchmarks pd)

activatePackage :: Maybe FilePath -> Maybe IDEPackage -> Maybe Text -> IDEM ()
activatePackage mbPath mbPack mbExe = do
    liftIO $ debugM "leksah" "activatePackage"
    oldActivePack <- readIDE activePack
    modifyIDE_ (\ide -> ide{activePack = mbPack, activeExe = mbExe})
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
deactivatePackage = activatePackage Nothing Nothing Nothing

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
    package <- ask
    interruptSaveAndRun $ packageConfig' package (\ _ -> return ())

packageConfig'  :: IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageConfig' package continuation = do
    prefs     <- readIDE prefs
    let dir = ipdPackageDir package
    useStack <- liftIO . doesFileExist $ dir </> "stack.yaml"
    if useStack
        then do
            ideMessage Normal (__ "Leksah is not running \"cabal configure\" because a stack.yaml file was found.")
            continuation True
        else do
            logLaunch <- getDefaultLogLaunch
            showDefaultLogLaunch'

            runExternalTool'        (__ "Configuring")
                                    "cabal"
                                    ("new-configure" : ipdConfigFlags package)
                                    dir $ do
                mbLastOutput <- C.getZipSink $ const <$> C.ZipSink sinkLast <*> C.ZipSink (logOutput logLaunch)
                lift $ do
                    mbPack <- idePackageFromPath (logOutput logLaunch) (ipdCabalFile package)
                    case mbPack of
                        Just pack -> do
                            changePackage pack
                            triggerEventIDE $ WorkspaceChanged False True
                            continuation (mbLastOutput == Just (ToolExit ExitSuccess))
                            return ()
                        Nothing -> do
                            postAsyncIDE $ ideMessage Normal (__ "Can't read package file")
                            continuation False
                            return()

runCabalBuild :: Bool -> Bool -> Bool -> IDEPackage -> Bool -> (Bool -> IDEAction) -> IDEAction
runCabalBuild backgroundBuild jumpToWarnings withoutLinking package shallConfigure continuation = do
    prefs <- readIDE prefs
    let dir =  ipdPackageDir package
    useStack <- liftIO . doesFileExist $ dir </> "stack.yaml"
    -- if we use stack, with tests enabled, we build the tests without running them
    let stackFlagsForTests =
            if useStack
                then
                   if "--enable-tests" `elem` ipdConfigFlags package
                            then ["--test", "--no-run-tests"]
                            else []
                else []
    -- if we use stack, with benchmarks enabled, we build the benchmarks without running them
    let stackFlagsForBenchmarks =
            if useStack
                then
                   if "--enable-benchmarks" `elem` ipdConfigFlags package
                            then ["--bench", "--no-run-benchmarks"]
                            else []
                else []
    let stackFlags = stackFlagsForTests ++ stackFlagsForBenchmarks
    let args = ["new-build"]
                -- stack needs the package name to actually print the output info
                ++ (if useStack then [ipdPackageName package] else [])
                ++ ["--with-ld=false" | not useStack && backgroundBuild && withoutLinking]
                ++ stackFlags
                ++ ipdBuildFlags package
    runExternalTool' (__ "Building") (if useStack then "stack" else "cabal") args dir $ do
        (mbLastOutput, isConfigErr, _) <- C.getZipSink $ (,,)
            <$> C.ZipSink sinkLast
            <*> C.ZipSink isConfigError
            <*> (C.ZipSink $ logOutputForBuild package backgroundBuild jumpToWarnings)
        lift $ do
            errs <- readIDE errorRefs
            if shallConfigure && isConfigErr
                then
                    packageConfig' package (\ b ->
                        when b $ runCabalBuild backgroundBuild jumpToWarnings withoutLinking package False continuation)
                else do
                    continuation (mbLastOutput == Just (ToolExit ExitSuccess))
                    return ()

isConfigError :: Monad m => C.Sink ToolOutput m Bool
isConfigError = CL.foldM (\a b -> return $ a || isCErr b) False
    where
    isCErr (ToolError str) = str1 `T.isInfixOf` str || str2 `T.isInfixOf` str || str3 `T.isInfixOf` str
    isCErr _ = False
    str1 = __ "Run the 'configure' command first"
    str2 = __ "please re-configure"
    str3 = __ "cannot satisfy -package-id"

buildPackage :: Bool -> Bool -> Bool -> IDEPackage -> (Bool -> IDEAction) -> IDEAction
buildPackage backgroundBuild jumpToWarnings withoutLinking package continuation = catchIDE (do
    ideR      <- ask
    prefs     <- readIDE prefs
    maybeDebug <- readIDE debugState
    case maybeDebug of
        Nothing -> do
            alreadyRunning <- isRunning
            if alreadyRunning
                then do
                    liftIO $ debugM "leksah" "buildPackage interruptBuild"
                    interruptBuild
                    unless backgroundBuild $ do
                        timeoutAdd PRIORITY_DEFAULT 100 (do
                            reflectIDE (do
                                buildPackage backgroundBuild jumpToWarnings withoutLinking
                                                package continuation
                                return False) ideR
                            return False)
                        return ()
                else do
                    when (saveAllBeforeBuild prefs) . liftIDE . void $ fileSaveAll belongsToWorkspace'
                    runCabalBuild backgroundBuild jumpToWarnings withoutLinking package True $ \f -> do
                        when f $ do
                            mbURI <- readIDE autoURI
                            case mbURI of
                                Just uri -> postSyncIDE . loadOutputUri $ T.unpack uri
                                Nothing  -> return ()
                        continuation f
        Just debug@(_, ghci) -> do
            -- TODO check debug package matches active package
            ready <- liftIO $ isEmptyMVar (currentToolCommand ghci)
            when ready $ do
                let dir = ipdPackageDir package
                when (saveAllBeforeBuild prefs) (do fileSaveAll belongsToWorkspace'; return ())
                (`runDebug` debug) . executeDebugCommand ":reload" $ do
                    errs <- logOutputForBuild package backgroundBuild jumpToWarnings
                    unless (any isError errs) . lift $ do
                        cmd <- readIDE autoCommand
                        postSyncIDE cmd
                        continuation True
    )
    (\(e :: SomeException) -> sysMessage Normal (T.pack $ show e))

packageDoc :: PackageAction
packageDoc = do
    package <- ask
    interruptSaveAndRun $ packageDoc' False True package (\ _ -> return ())

packageDoc' :: Bool -> Bool -> IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageDoc' backgroundBuild jumpToWarnings package continuation = do
    prefs     <- readIDE prefs
    catchIDE (do
        let dir = ipdPackageDir package
        useStack <- liftIO . doesFileExist $ dir </> "stack.yaml"
        runExternalTool' (__ "Documenting") (if useStack then "stack" else "cabal") ("haddock" : ipdHaddockFlags package) dir $ do
            mbLastOutput <- C.getZipSink $ const <$> C.ZipSink sinkLast <*> (C.ZipSink $
                logOutputForBuild package backgroundBuild jumpToWarnings)
            lift $ postAsyncIDE reloadDoc
            lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess)))
        (\(e :: SomeException) -> print e)

packageClean :: PackageAction
packageClean = do
    package <- ask
    interruptSaveAndRun $ packageClean' package (\ _ -> return ())

packageClean' :: IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageClean' package continuation = do
    prefs     <- readIDE prefs
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'

    let dir = ipdPackageDir package
    useStack <- liftIO . doesFileExist $ dir </> "stack.yaml"
    runExternalTool' (__ "Cleaning")
                    (if useStack then "stack" else "cabal")
                    ["clean"]
                    dir $ do
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
                                    dir
                                    (logOutput logLaunch))
            (\(e :: SomeException) -> print e)

packageInstall :: PackageAction
packageInstall = do
    package <- ask
    interruptSaveAndRun $ packageInstall' package (\ _ -> return ())

packageInstall' :: IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageInstall' package continuation = do
    prefs     <- readIDE prefs
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'

    catchIDE (do
        let dir = ipdPackageDir package
        useStack <- liftIO . doesFileExist $ dir </> "stack.yaml"
        runExternalTool' (__ "Installing")
                         (if useStack then "stack" else "echo" {-cabalCommand prefs-})
                         ((if useStack then "install" : ipdBuildFlags package else ["TODO run cabal new-install"]) ++ ipdInstallFlags package)
                         dir $ do
                mbLastOutput <- C.getZipSink $ (const <$> C.ZipSink sinkLast) <*> C.ZipSink (logOutput logLaunch)
                lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess)))
        (\(e :: SomeException) -> print e)

packageRun :: PackageAction
packageRun = ask >>= (interruptSaveAndRun . packageRun' True)

packageRun' :: Bool -> IDEPackage -> IDEAction
packageRun' removeGhcjsFlagIfPresent package =
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
                    packageConfig' packWithNewFlags $ \ ok -> when ok $
                        packageRun' False packWithNewFlags
                _  -> return ()
        else liftIDE $ catchIDE (do
            ideR        <- ask
            maybeDebug   <- readIDE debugState
            pd <- liftIO $ fmap flattenPackageDescription
                             (readPackageDescription normal (ipdCabalFile package))
            mbExe <- readIDE activeExe
            let exe = exeToRun mbExe $ executables pd
            let defaultLogName = T.pack . display . pkgName $ ipdPackageId package
                logName = fromMaybe defaultLogName . listToMaybe $ map (T.pack . exeName) exe
            (logLaunch,logName) <- buildLogLaunchByName logName
            showLog
            case maybeDebug of
                Nothing -> do
                    prefs <- readIDE prefs
                    let dir = ipdPackageDir package
                    useStack <- liftIO . doesFileExist $ dir </> "stack.yaml"
                    if useStack
                        then
                            IDE.Package.runPackage (addLogLaunchData logName logLaunch)
                                                   (T.pack $ printf (__ "Running %s") (T.unpack logName))
                                                   "stack"
                                                   (concat [["exec"]
                                                        , ipdBuildFlags package
                                                        , map (T.pack . exeName) exe
                                                        , ["--"]
                                                        , ipdExeFlags package])
                                                   dir
                                                   (logOutput logLaunch)
                        else do
                            projectRoot <- liftIO $ findProjectRoot dir
                            case exe ++ executables pd of
                                [] -> return ()
                                (Executable name _ _ : _) -> do
                                    let exePath = projectRoot </> "dist-newstyle/build"
                                                    </> T.unpack (packageIdentifierToString $ ipdPackageId package)
                                                    </> "build" </> name </> name
                                    IDE.Package.runPackage (addLogLaunchData logName logLaunch)
                                                           (T.pack $ printf (__ "Running %s") (T.unpack logName))
                                                           exePath
                                                           (ipdExeFlags package)
                                                           dir
                                                           (logOutput logLaunch)
                Just debug ->
                    -- TODO check debug package matches active package
                    runDebug (do
                        case exe of
                            [Executable name mainFilePath _] ->
                                executeDebugCommand (":module *" <> T.pack (map (\c -> if c == '/' then '.' else c) (takeWhile (/= '.') mainFilePath)))
                                                    (logOutput logLaunch)
                            _ -> return ()
                        executeDebugCommand (":main " <> T.unwords (ipdExeFlags package)) (logOutput logLaunch))
                        debug)
            (\(e :: SomeException) -> print e)

-- | Is the given executable the active one?
isActiveExe :: Text -> Executable -> Bool
isActiveExe selected (Executable name _ _) = selected == T.pack name

-- | get executable to run
--   no exe activated, take first one
exeToRun :: Maybe Text -> [Executable] -> [Executable]
exeToRun Nothing (exe:_) = [exe]
exeToRun Nothing _ = []
exeToRun (Just selected) exes = take 1 $ filter (isActiveExe selected) exes

packageRunJavaScript :: PackageAction
packageRunJavaScript = ask >>= (interruptSaveAndRun . packageRunJavaScript' True)

packageRunJavaScript' :: Bool -> IDEPackage -> IDEAction
packageRunJavaScript' addFlagIfMissing package =
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
                    packageConfig' packWithNewFlags $ \ ok -> when ok $
                        packageRunJavaScript' False packWithNewFlags
                _  -> return ()
        else liftIDE $ buildPackage False False True package $ \ ok -> when ok $ liftIDE $ catchIDE (do
                ideR        <- ask
                maybeDebug   <- readIDE debugState
                pd <- liftIO $ fmap flattenPackageDescription
                                 (readPackageDescription normal (ipdCabalFile package))
                mbExe <- readIDE activeExe
                let exe = exeToRun mbExe $ executables pd
                let defaultLogName = T.pack . display . pkgName $ ipdPackageId package
                    logName = fromMaybe defaultLogName . listToMaybe $ map (T.pack . exeName) exe
                (logLaunch,logName) <- buildLogLaunchByName logName
                let dir = ipdPackageDir package
                prefs <- readIDE prefs
                case exe ++ executables pd of
                    (Executable name _ _ : _) -> liftIDE $ do
                        let path = "dist-newstyle/build" </> name </> name <.> "jsexe" </> "index.html"
                            dir = ipdPackageDir package
                        postAsyncIDE $ do
                            loadOutputUri ("file:///" ++ dir </> path)
                            getOutputPane Nothing  >>= \ p -> displayPane p False
                      `catchIDE`
                        (\(e :: SomeException) -> print e)

                    _ -> return ())
                (\(e :: SomeException) -> print e)

packageTest :: PackageAction
packageTest = do
    package <- ask
    interruptSaveAndRun $ packageTest' False True package True (\ _ -> return ())

packageTest' :: Bool -> Bool -> IDEPackage -> Bool -> (Bool -> IDEAction) -> IDEAction
packageTest' backgroundBuild jumpToWarnings package shallConfigure continuation = do
    let dir = ipdPackageDir package
    useStack <- liftIO . doesFileExist $ dir </> "stack.yaml"
    if "--enable-tests" `elem` ipdConfigFlags package
        then do
          logLaunch <- getDefaultLogLaunch
          showDefaultLogLaunch'
          catchIDE (do
            prefs <- readIDE prefs
            removeTestLogRefs dir
            let cmd=if useStack then "stack" else "cabal"
            let args=if useStack then ["test"] else ["test", "--with-ghc=leksahtrue"]
            runExternalTool' (__ "Testing") cmd (args
                ++ ipdBuildFlags package ++ ipdTestFlags package) dir $ do
                    (mbLastOutput, isConfigErr, _) <- C.getZipSink $ (,,)
                        <$> C.ZipSink sinkLast
                        <*> C.ZipSink isConfigError
                        <*> (C.ZipSink $ logOutputForBuild package backgroundBuild jumpToWarnings)
                    lift $ do
                        errs <- readIDE errorRefs
                        if shallConfigure && isConfigErr
                            then
                                packageConfig' package (\ b ->
                                    when b $ packageTest' backgroundBuild jumpToWarnings package shallConfigure continuation)
                            else do
                                continuation (mbLastOutput == Just (ToolExit ExitSuccess))
                                return ())
            (\(e :: SomeException) -> print e)
        else continuation True

-- | Run benchmarks as foreground action for current package
packageBench :: PackageAction
packageBench = do
    package <- ask
    interruptSaveAndRun $ packageBench' False True package True (\ _ -> return ())

-- | Run benchmarks
packageBench' :: Bool -> Bool -> IDEPackage -> Bool -> (Bool -> IDEAction) -> IDEAction
packageBench' backgroundBuild jumpToWarnings package shallConfigure continuation = do
    let dir = ipdPackageDir package
    useStack <- liftIO . doesFileExist $ dir </> "stack.yaml"
    if "--enable-benchmarks" `elem` ipdConfigFlags package
        then do
          logLaunch <- getDefaultLogLaunch
          showDefaultLogLaunch'
          catchIDE (do
            prefs <- readIDE prefs
            let cmd=if useStack then "stack" else "cabal"
            let args=if useStack then ["bench"] else ["bench", "--with-ghc=leksahtrue"]
            runExternalTool' (__ "Benchmarking") cmd (args
                ++ ipdBuildFlags package ++ ipdBenchmarkFlags package) dir $ do
                    (mbLastOutput, isConfigErr, _) <- C.getZipSink $ (,,)
                        <$> C.ZipSink sinkLast
                        <*> C.ZipSink isConfigError
                        <*> (C.ZipSink $ logOutputForBuild package backgroundBuild jumpToWarnings)
                    lift $ do
                        errs <- readIDE errorRefs
                        if shallConfigure && isConfigErr
                            then
                                packageConfig' package (\ b ->
                                    when b $ packageBench' backgroundBuild jumpToWarnings package shallConfigure continuation)
                            else do
                                continuation (mbLastOutput == Just (ToolExit ExitSuccess))
                                return ())
            (\(e :: SomeException) -> print e)
        else continuation True

packageSdist :: PackageAction
packageSdist = do
    package <- ask
    interruptSaveAndRun $ do
        logLaunch <- getDefaultLogLaunch
        showDefaultLogLaunch'

        catchIDE (do
            prefs <- readIDE prefs
            let dir = ipdPackageDir package
            runExternalTool' (__ "Source Dist") "cabal" ("sdist" : ipdSdistFlags package) dir (logOutput logLaunch))
            (\(e :: SomeException) -> print e)


-- | Open generated documentation for package
packageOpenDoc :: PackageAction
packageOpenDoc = do
    package <- ask
    let dir = ipdPackageDir package
    useStack <- liftIO . doesFileExist $ dir </> "stack.yaml"
    distDir <- if useStack
                        then do
                            --ask stack where its dist directory is
                            mvar <- liftIO newEmptyMVar
                            runExternalTool' "" "stack" ["path"] dir $ do
                                output <- CL.consume
                                liftIO . putMVar mvar $ head $ catMaybes $ map getDistOutput output
                            liftIO $ takeMVar mvar
                        else return "dist"
    liftIDE $ do
        prefs   <- readIDE prefs
        let path = dir </> distDir
                        </> "doc/html"
                        </> display (pkgName (ipdPackageId package))
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
        writeGenericPackageDescription (ipdCabalFile p) npd)
           (\(e :: SomeException) -> do
            reflectIDE (ideMessage Normal (__ "Can't update package " <> T.pack (show e))) ideR
            return ()))
  where
    addModule LibExposedMod gpd@GenericPackageDescription{condLibrary = Just lib} =
        gpd {condLibrary = Just (addModToLib moduleName lib)}
    addModule LibOtherMod gpd@GenericPackageDescription{condLibrary = Just lib} =
        gpd {condLibrary = Just (addModToBuildInfoLib moduleName lib)}
    addModule (ExeOrTestMod name') gpd = let name = T.unpack name' in gpd {
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

addModToBuildInfoExe :: String -> ModuleName -> (String, CondTree ConfVar [Dependency] Executable) ->
    (String, CondTree ConfVar [Dependency] Executable)
addModToBuildInfoExe name modName (str,ct@CondNode{condTreeData = exe}) | str == name =
    (str, ct{condTreeData = exe{buildInfo = (buildInfo exe){otherModules = modName
        `inOrderAdd` otherModules (buildInfo exe)}}})
addModToBuildInfoExe name _ x = x

addModToBuildInfoTest :: String -> ModuleName -> (String, CondTree ConfVar [Dependency] TestSuite) ->
    (String, CondTree ConfVar [Dependency] TestSuite)
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
        writeGenericPackageDescription (ipdCabalFile p) npd)
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

delModFromBuildInfoExe :: ModuleName -> (String, CondTree ConfVar [Dependency] Executable) ->
    (String, CondTree ConfVar [Dependency] Executable)
delModFromBuildInfoExe modName (str,ct@CondNode{condTreeData = exe}) =
    (str, ct{condTreeData = exe{buildInfo = (buildInfo exe){otherModules =
        delete modName (otherModules (buildInfo exe))}}})

isExposedModule :: ModuleName -> Maybe (CondTree ConfVar [Dependency] Library)  -> Bool
isExposedModule mn Nothing                             = False
isExposedModule mn (Just CondNode{condTreeData = lib}) = mn `elem` exposedModules lib

backgroundBuildToggled :: IDEAction
backgroundBuildToggled = do
    toggled <- getBackgroundBuildToggled
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){backgroundBuild= toggled}})

runUnitTestsToggled :: IDEAction
runUnitTestsToggled = do
    toggled <- getRunUnitTests
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){runUnitTests= toggled}})

makeModeToggled :: IDEAction
makeModeToggled = do
    toggled <- getMakeModeToggled
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){makeMode= toggled}})

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
    package   <- ask
    liftIDE $ catchIDE (do
        ideRef     <- ask
        prefs'     <- readIDE prefs
        maybeDebug <- readIDE debugState
        case maybeDebug of
            Nothing -> do
                let dir = ipdPackageDir package
                mbExe <- readIDE activeExe
                ghci <- reifyIDE $ \ideR -> newGhci dir mbExe (interactiveFlags prefs')
                    $ reflectIDEI (void (logOutputForBuild package True False)) ideR
                modifyIDE_ (\ide -> ide {debugState = Just (package, ghci)})
                triggerEventIDE (Sensitivity [(SensitivityInterpreting, True)])
                setDebugToggled True
                -- Fork a thread to wait for the output from the process to close
                liftIO $ forkIO $ do
                    readMVar (outputClosed ghci)
                    (`reflectIDE` ideRef) . postSyncIDE $ do
                        setDebugToggled False
                        modifyIDE_ (\ide -> ide {debugState = Nothing, autoCommand = return ()})
                        triggerEventIDE (Sensitivity [(SensitivityInterpreting, False)])
                        -- Kick of a build if one is not already due
                        modifiedPacks <- fileCheckAll belongsToPackages'
                        let modified = not (null modifiedPacks)
                        prefs <- readIDE prefs
                        when (not modified && backgroundBuild prefs) $ do
                            -- So although none of the pakages are modified,
                            -- they may have been modified in ghci mode.
                            -- Lets build to make sure the binaries are up to date
                            mbPackage   <- readIDE activePack
                            case mbPackage of
                                Just package -> runCabalBuild True False False package True (\ _ -> return ())
                                Nothing -> return ()
                return ()
            _ -> do
                sysMessage Normal (__ "Debugger already running")
                return ())
            (\(e :: SomeException) -> print e)

tryDebug :: DebugAction -> PackageAction
tryDebug f = do
    maybeDebug <- liftIDE $ readIDE debugState
    case maybeDebug of
        Just debug ->
            -- TODO check debug package matches active package
            liftIDE $ runDebug f debug
        _ -> do
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
                    maybeDebug <- liftIDE $ readIDE debugState
                    case maybeDebug of
                        Just debug -> liftIDE $ postAsyncIDE $ runDebug f debug
                        _ -> return ()
                _  -> return ()

tryDebugQuiet :: DebugAction -> PackageAction
tryDebugQuiet f = do
    maybeDebug <- liftIDE $ readIDE debugState
    case maybeDebug of
        Just debug ->
            -- TODO check debug package matches active package
            liftIDE $ runDebug f debug
        _ ->
            return ()

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
                                        ++ [ (f, bi, True) | TestSuite _ (TestSuiteExeV10 _ f) bi _ <- testSuites packageD ]
                                        ++ [ (f, bi, True) | Benchmark _ (BenchmarkExeV10 _ f) bi _ <- benchmarks packageD ]
                ipdExtraSrcs        = Set.fromList $ extraSrcFiles packageD
                ipdSrcDirs          = case nub $ concatMap hsSourceDirs (allBuildInfo' packageD) of
                                            [] -> [".","src"]
                                            l -> l
                ipdExes             = [ T.pack $ exeName e | e <- executables packageD
                                          , buildable (buildInfo e) ]
                ipdExtensions       = nub $ concatMap oldExtensions (allBuildInfo' packageD)
                ipdTests            = [ T.pack $ testName t | t <- testSuites packageD
                                          , buildable (testBuildInfo t) ]
                ipdBenchmarks       = [ T.pack $ benchmarkName b | b <- benchmarks packageD
                                          , buildable (benchmarkBuildInfo b) ]
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
extractStackPackageList = filter (/= ".") .
                          map (stripQuotes . T.unpack . (\x -> fromMaybe x $ T.stripPrefix "location: " x)) .
                          mapMaybe (T.stripPrefix "- ") .
                          takeWhile ("- " `T.isPrefixOf`) .
                          drop 1 .
                          dropWhile (/= "packages:") .
                          T.lines
  where
    stripQuotes ('\'':rest) | take 1 (reverse rest) == "\'" = init rest
    stripQuotes x = x


idePackageFromPath :: C.Sink ToolOutput IDEM () -> FilePath -> IDEM (Maybe IDEPackage)
idePackageFromPath log filePath = do
    mbRootPackage <- idePackageFromPath' filePath
    case mbRootPackage of
        Nothing -> return Nothing
        Just rootPackage -> do
            mvar <- liftIO newEmptyMVar
            let dir = takeDirectory filePath
            useStack <- liftIO . doesFileExist $ dir </> "stack.yaml"
            paths <-
                if useStack
                    then liftIO $ map (dir </>) . extractStackPackageList <$> T.readFile (dir </> "stack.yaml")
                    else do
                        runExternalTool' "" "cabal" ["sandbox", "list-sources"] dir $ do
                            output <- CL.consume
                            liftIO . putMVar mvar $ case take 1 $ reverse output of
                                [ToolExit ExitSuccess] ->
                                    map (T.unpack . toolline) . takeWhile (/= ToolOutput "") . drop 1 $ dropWhile (/= ToolOutput "") output
                                _ -> []
                        liftIO $ takeMVar mvar

            sandboxSources <- catMaybes <$> forM paths (\path -> do
                exists <- liftIO (doesDirectoryExist path)
                if exists
                    then do
                        cpath <- liftIO $ canonicalizePath path
                        contents <- liftIO $ getDirectoryContents cpath
                        let mbCabalFile = find ((== ".cabal") . takeExtension) contents
                        when (isNothing mbCabalFile) $
                            ideMessage Normal ("Could not find cabal file of the add-source dependency at " <> T.pack cpath)
                        return (fmap (path </>) mbCabalFile)
                    else do
                        ideMessage Normal ("Path of add-source dependency does not exist: " <> T.pack path)
                        return Nothing)
            s <- liftM catMaybes . mapM idePackageFromPath' $ nub sandboxSources
            return . Just $ rootPackage {ipdSandboxSources = s}

refreshPackage :: C.Sink ToolOutput IDEM () -> PackageM (Maybe IDEPackage)
refreshPackage log = do
    package <- ask
    liftIDE $ do
        mbUpdatedPack <- idePackageFromPath log (ipdCabalFile package)
        case mbUpdatedPack of
            Just updatedPack -> do
                changePackage updatedPack
                triggerEventIDE $ WorkspaceChanged False True
                return mbUpdatedPack
            Nothing -> do
                postAsyncIDE $ ideMessage Normal (__ "Can't read package file")
                return Nothing

