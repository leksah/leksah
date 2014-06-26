{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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
,   packageCopy'
,   packageRun
,   packageRunJavaScript
,   activatePackage
,   deactivatePackage

,   packageInstallDependencies
,   packageRegister
,   packageRegister'
,   packageTest
,   packageTest'
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

import Graphics.UI.Gtk
import Distribution.Package hiding (depends,packageId)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Verbosity
import System.FilePath
import Control.Concurrent
import System.Directory (setCurrentDirectory, doesFileExist, getDirectoryContents)
import Prelude hiding (catch)
import Data.Maybe
       (listToMaybe, fromMaybe, isNothing, isJust, fromJust, catMaybes)
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
import MyMissing (replace)
import Distribution.ModuleName (ModuleName(..))
import Data.List (isInfixOf, nub, foldl', delete)
import qualified System.IO.UTF8 as UTF8  (readFile)
import IDE.Utils.Tool (ToolOutput(..), runTool, newGhci, ToolState(..), toolline)
import qualified Data.Set as  Set (fromList)
import qualified Data.Map as  Map (empty, fromList)
import System.Exit (ExitCode(..))
import Control.Applicative ((<$>))
import IDE.Utils.Tool (executeGhciCommand, getProcessExitCode, interruptProcessGroupOf,
    ProcessHandle)
import qualified Data.Conduit as C (Sink)
import qualified Data.Conduit.List as CL (foldM, fold, consume)
import qualified Data.Conduit.Internal as CU (zipSinks)
import Data.Conduit (($$))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, unless, liftM, forM, forM_)
import Distribution.PackageDescription.PrettyPrint
       (writeGenericPackageDescription)
import Debug.Trace (trace)
import IDE.Pane.WebKit.Documentation
       (getDocumentation, loadDoc, reloadDoc)
import IDE.Pane.WebKit.Output (loadOutputUri, getOutputPane)
import Text.Printf (printf)
import System.Log.Logger (debugM)
import System.Process.Vado (getMountPoint, vado, readSettings)
import qualified Data.Text as T (pack, unpack, isInfixOf)
import IDE.Utils.ExternalTool (runExternalTool', runExternalTool, isRunning, interruptBuild)
import Text.PrinterParser (writeFields)

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

activatePackage :: Maybe FilePath -> Maybe IDEPackage -> Maybe String -> IDEM ()
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
                    (_, Just pack) -> wsStr ++ " > " ++ packageIdentifierToString (ipdPackageId pack)
                    (Just path, _) -> wsStr ++ " > " ++ takeFileName path
                    _ -> wsStr ++ ":"
    triggerEventIDE (StatusbarChanged [CompartmentPackage txt])
    return ()

deactivatePackage :: IDEAction
deactivatePackage = activatePackage Nothing Nothing Nothing

packageConfig :: PackageAction
packageConfig = do
    package <- ask
    liftIDE $ packageConfig' package (\ _ -> return ())

packageConfig'  :: IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageConfig' package continuation = do
    prefs     <- readIDE prefs
    let dir = ipdBuildDir package
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'

    runExternalTool'        (__ "Configuring")
                            (cabalCommand prefs)
                            (["configure"] ++ (ipdConfigFlags package))
                            dir $ do
        (mbLastOutput, _) <- CU.zipSinks sinkLast (logOutput logLaunch)
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

runCabalBuild :: Bool -> Bool -> Bool -> Bool -> IDEPackage -> Bool -> (Bool -> IDEAction) -> IDEAction
runCabalBuild backgroundBuild runTests jumpToWarnings withoutLinking package shallConfigure continuation = do
    prefs <- readIDE prefs
    let dir =  ipdBuildDir package
    let args = ([if runTests then "test" else "build"]
                ++ (if backgroundBuild && withoutLinking
                    then ["--with-ld=false"]
                    else [])
                ++ ipdBuildFlags package
                ++ (if runTests then ipdTestFlags package else []))
    runExternalTool' (__ "Building") (cabalCommand prefs) args dir $ do
        (mbLastOutput, (isConfigErr, _)) <- CU.zipSinks sinkLast $ CU.zipSinks isConfigError $
            logOutputForBuild package backgroundBuild jumpToWarnings
        lift $ do
            errs <- readIDE errorRefs
            if shallConfigure && isConfigErr
                then
                    packageConfig' package (\ b ->
                        when b $ runCabalBuild backgroundBuild runTests jumpToWarnings withoutLinking package False continuation)
                else do
                    continuation (mbLastOutput == Just (ToolExit ExitSuccess))
                    return ()

isConfigError :: Monad m => C.Sink ToolOutput m Bool
isConfigError = CL.foldM (\a b -> return $ a || isCErr b) False
    where
    isCErr (ToolError str) = str1 `T.isInfixOf` str || str2 `T.isInfixOf` str || str3 `T.isInfixOf` str
    isCErr _ = False
    str1 = T.pack (__ "Run the 'configure' command first")
    str2 = T.pack (__ "please re-configure")
    str3 = T.pack (__ "cannot satisfy -package-id")

buildPackage :: Bool -> Bool -> Bool -> Bool -> IDEPackage -> (Bool -> IDEAction) -> IDEAction
buildPackage backgroundBuild runTests jumpToWarnings withoutLinking package continuation = catchIDE (do
    ideR      <- ask
    prefs     <- readIDE prefs
    maybeDebug <- readIDE debugState
    case maybeDebug of
        Nothing -> do
            alreadyRunning <- isRunning
            if alreadyRunning
                then do
                    interruptBuild
                    when (not backgroundBuild) $ liftIO $ do
                        timeoutAddFull (do
                            reflectIDE (do
                                buildPackage backgroundBuild runTests jumpToWarnings withoutLinking
                                                package continuation
                                return False) ideR
                            return False) priorityDefaultIdle 1000
                        return ()
                else runCabalBuild backgroundBuild runTests jumpToWarnings withoutLinking package True $ \f -> do
                        when f $ do
                            mbURI <- readIDE autoURI
                            case mbURI of
                                Just uri -> postSyncIDE $ loadOutputUri uri
                                Nothing  -> return ()
                        continuation f
        Just debug@(_, ghci) -> do
            -- TODO check debug package matches active package
            ready <- liftIO $ isEmptyMVar (currentToolCommand ghci)
            when ready $ do
                let dir = ipdBuildDir package
                when (saveAllBeforeBuild prefs) (do fileSaveAll belongsToWorkspace; return ())
                (`runDebug` debug) . executeDebugCommand ":reload" $ do
                    errs <- logOutputForBuild package backgroundBuild jumpToWarnings
                    unless (any isError $ errs) $ do
                        cmd <- lift $ readIDE autoCommand
                        liftIO . postGUISync $ reflectIDE cmd ideR
                        lift $ continuation True
    )
    (\(e :: SomeException) -> sysMessage Normal (show e))

packageDoc :: PackageAction
packageDoc = do
    package <- ask
    liftIDE $ packageDoc' False True package (\ _ -> return ())

packageDoc' :: Bool -> Bool -> IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageDoc' backgroundBuild jumpToWarnings package continuation = do
    prefs     <- readIDE prefs
    catchIDE (do
        let dir = ipdBuildDir package
        runExternalTool' (__ "Documenting") (cabalCommand prefs) (["haddock"]
            ++ (ipdHaddockFlags package)) dir $ do
                (mbLastOutput, _) <- CU.zipSinks sinkLast $
                    logOutputForBuild package backgroundBuild jumpToWarnings
                lift $ reloadDoc
                lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess)))
        (\(e :: SomeException) -> putStrLn (show e))

packageClean :: PackageAction
packageClean = do
    package <- ask
    liftIDE $ packageClean' package (\ _ -> return ())

packageClean' :: IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageClean' package continuation = do
    prefs     <- readIDE prefs
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'

    let dir = ipdBuildDir package
    runExternalTool' (__ "Cleaning")
                    (cabalCommand prefs)
                    ["clean"]
                    dir $ do
        (mbLastOutput, _) <- CU.zipSinks sinkLast (logOutput logLaunch)
        lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess))

packageCopy :: PackageAction
packageCopy = do
    package <- ask
    liftIDE $ do
        logLaunch <- getDefaultLogLaunch
        showDefaultLogLaunch'

        catchIDE (do
            prefs       <- readIDE prefs
            window      <- getMainWindow
            mbDir       <- liftIO $ chooseDir window (__ "Select the target directory") Nothing
            case mbDir of
                Nothing -> return ()
                Just fp -> do
                    let dir = ipdBuildDir package
                    runExternalTool' (__ "Copying")
                                    (cabalCommand prefs)
                                    (["copy"] ++ ["--destdir=" ++ fp])
                                    dir
                                    (logOutput logLaunch))
            (\(e :: SomeException) -> putStrLn (show e))

packageInstallDependencies :: PackageAction
packageInstallDependencies = do
    package <- ask
    liftIDE $ do
        logLaunch <- getDefaultLogLaunch
        showDefaultLogLaunch'

        catchIDE (do
            prefs <- readIDE prefs
            let dir = ipdBuildDir package
            runExternalTool' (__ "Installing") (cabalCommand prefs) (
                   (if useCabalDev prefs
                        then ["install-deps"]
                        else ["install","--only-dependencies"])
                ++ (ipdConfigFlags package)
                ++ (ipdInstallFlags package)) dir (logOutput logLaunch))
            (\(e :: SomeException) -> putStrLn (show e))

packageCopy' :: IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageCopy' package continuation = do
    prefs     <- readIDE prefs
    logLaunch <- getDefaultLogLaunch
    showDefaultLogLaunch'

    catchIDE (do
        let dir = ipdBuildDir package
        runExternalTool' (__ "Copying") (cabalCommand prefs) (["copy"]
            ++ (ipdInstallFlags package)) dir $ do
                (mbLastOutput, _) <- CU.zipSinks sinkLast (logOutput logLaunch)
                lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess)))
        (\(e :: SomeException) -> putStrLn (show e))

packageRun :: PackageAction
packageRun = ask >>= (liftIDE . packageRun' True)

packageRun' :: Bool -> IDEPackage -> IDEAction
packageRun' addFlagIfMissing package = do
    if addFlagIfMissing && "--ghcjs" `elem` ipdConfigFlags package && not ("--ghcjs-option=--native-executables" `elem` ipdConfigFlags package)
        then do
            window <- liftIDE $ getMainWindow
            resp <- liftIO $ do
                md <- messageDialogNew (Just window) [] MessageQuestion ButtonsCancel
                        (__ "Package is configured to use GHCJS.  Would you like to add --ghcjs-option=--native-executables to the configure flags and rebuild?")
                dialogAddButton md (__ "Add _GHCJS Native Executables") (ResponseUser 1)
                dialogSetDefaultResponse md (ResponseUser 1)
                set md [ windowWindowPosition := WinPosCenterOnParent ]
                resp <- dialogRun md
                widgetDestroy md
                return resp
            case resp of
                ResponseUser 1 -> do
                    let packWithNewFlags = package { ipdConfigFlags = ["--ghcjs-option=--native-executables"] ++ ipdConfigFlags package }
                    changePackage packWithNewFlags
                    liftIO $ writeFlags (dropExtension (ipdCabalFile packWithNewFlags) ++ leksahFlagFileExtension) packWithNewFlags
                    packageConfig' packWithNewFlags $ \ ok -> when ok $ do
                        packageRun' False packWithNewFlags
                _  -> return ()
        else liftIDE $ catchIDE (do
            ideR        <- ask
            maybeDebug   <- readIDE debugState
            pd <- liftIO $ readPackageDescription normal (ipdCabalFile package) >>= return . flattenPackageDescription
            mbExe <- readIDE activeExe
            let exe = take 1 . filter (isActiveExe mbExe) $ executables pd
            let defaultLogName = display . pkgName $ ipdPackageId package
                logName = fromMaybe defaultLogName . listToMaybe $ map exeName exe
            (logLaunch,logName) <- buildLogLaunchByName logName
            case maybeDebug of
                Nothing -> do
                    let dir = ipdBuildDir package
                    IDE.Package.runPackage (addLogLaunchData logName logLaunch)
                                           (printf (__ "Running %s") logName)
                                           "cabal"
                                           (concat [["run"]
                                                , ipdBuildFlags package
                                                , map exeName exe
                                                , ["--"]
                                                , ipdExeFlags package])
                                           dir
                                           (logOutput logLaunch)
                Just debug -> do
                    -- TODO check debug package matches active package
                    runDebug (do
                        case exe of
                            [Executable name mainFilePath _] -> do
                                executeDebugCommand (":module *" ++ (map (\c -> if c == '/' then '.' else c) (takeWhile (/= '.') mainFilePath))) (logOutput logLaunch)
                            _ -> return ()
                        executeDebugCommand (":main " ++ (unwords (ipdExeFlags package))) (logOutput logLaunch))
                        debug)
            (\(e :: SomeException) -> putStrLn (show e))
  where
    isActiveExe selected (Executable name _ _) = selected == Just name

packageRunJavaScript :: PackageAction
packageRunJavaScript = ask >>= (liftIDE . packageRunJavaScript' True)

packageRunJavaScript' :: Bool -> IDEPackage -> IDEAction
packageRunJavaScript' addFlagIfMissing package = do
    if addFlagIfMissing && not ("--ghcjs" `elem` ipdConfigFlags package)
        then do
            window <- liftIDE $ getMainWindow
            resp <- liftIO $ do
                md <- messageDialogNew (Just window) [] MessageQuestion ButtonsCancel
                        (__ "Package is not configured to use GHCJS.  Would you like to add --ghcjs to the configure flags and rebuild?")
                dialogAddButton md (__ "Use _GHCJS") (ResponseUser 1)
                dialogSetDefaultResponse md (ResponseUser 1)
                set md [ windowWindowPosition := WinPosCenterOnParent ]
                resp <- dialogRun md
                widgetDestroy md
                return resp
            case resp of
                ResponseUser 1 -> do
                    let packWithNewFlags = package { ipdConfigFlags = ["--ghcjs"] ++ ipdConfigFlags package }
                    changePackage packWithNewFlags
                    liftIO $ writeFlags (dropExtension (ipdCabalFile packWithNewFlags) ++ leksahFlagFileExtension) packWithNewFlags
                    packageConfig' packWithNewFlags $ \ ok -> when ok $ do
                        packageRunJavaScript' False packWithNewFlags
                _  -> return ()
        else liftIDE $ buildPackage False False True False package $ \ ok -> when ok $ liftIDE $ catchIDE (do
                ideR        <- ask
                maybeDebug   <- readIDE debugState
                pd <- liftIO $ readPackageDescription normal (ipdCabalFile package) >>= return . flattenPackageDescription
                mbExe <- readIDE activeExe
                let exe = take 1 . filter (isActiveExe mbExe) $ executables pd
                let defaultLogName = display . pkgName $ ipdPackageId package
                    logName = fromMaybe defaultLogName . listToMaybe $ map exeName exe
                (logLaunch,logName) <- buildLogLaunchByName logName
                let dir = ipdBuildDir package
                prefs <- readIDE prefs
                case exe ++ executables pd of
                    (Executable name _ _ : _) -> liftIDE $ do
                        let path = "dist/build" </> name </> name <.> "jsexe" </> "index.html"
                            dir = ipdBuildDir package
#ifdef WEBKITGTK
                        loadOutputUri ("file:///" ++ dir </> path)
                        getOutputPane Nothing  >>= \ p -> displayPane p False
#else
                        openBrowser path
#endif
                      `catchIDE`
                        (\(e :: SomeException) -> putStrLn (show e))

                    _ -> return ())
                (\(e :: SomeException) -> putStrLn (show e))
  where
    isActiveExe selected (Executable name _ _) = selected == Just name

packageRegister :: PackageAction
packageRegister = do
    package <- ask
    liftIDE $ packageRegister' package (\ _ -> return ())

packageRegister' :: IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageRegister' package continuation =
    if ipdHasLibs package
        then do
          logLaunch <- getDefaultLogLaunch
          showDefaultLogLaunch'
          catchIDE (do
            prefs <- readIDE prefs
            let dir = ipdBuildDir package
            runExternalTool' (__ "Registering") (cabalCommand prefs) (["register"]
                ++ (ipdRegisterFlags package)) dir $ do
                    (mbLastOutput, _) <- CU.zipSinks sinkLast (logOutput logLaunch)
                    lift $ continuation (mbLastOutput == Just (ToolExit ExitSuccess)))
            (\(e :: SomeException) -> putStrLn (show e))
        else continuation True

packageTest :: PackageAction
packageTest = do
    package <- ask
    liftIDE $ packageTest' package True (\ _ -> return ())

packageTest' :: IDEPackage -> Bool -> (Bool -> IDEAction) -> IDEAction
packageTest' package shallConfigure continuation =
    if "--enable-tests" `elem` ipdConfigFlags package
        then do
          logLaunch <- getDefaultLogLaunch
          showDefaultLogLaunch'
          catchIDE (do
            prefs <- readIDE prefs
            let dir = ipdBuildDir package
            runExternalTool' (__ "Testing") (cabalCommand prefs) (["test"]
                ++ ipdBuildFlags package ++ ipdTestFlags package) dir $ do
                    (mbLastOutput, (isConfigErr, _)) <- CU.zipSinks sinkLast $ CU.zipSinks isConfigError $
                        logOutputForBuild package False True
                    lift $ do
                        errs <- readIDE errorRefs
                        if shallConfigure && isConfigErr
                            then
                                packageConfig' package (\ b ->
                                    when b $ packageTest' package shallConfigure continuation)
                            else do
                                continuation (mbLastOutput == Just (ToolExit ExitSuccess))
                                return ())
            (\(e :: SomeException) -> putStrLn (show e))
        else continuation True

packageSdist :: PackageAction
packageSdist = do
    package <- ask
    liftIDE $ do
        logLaunch <- getDefaultLogLaunch
        showDefaultLogLaunch'

        catchIDE (do
            prefs <- readIDE prefs
            let dir = ipdBuildDir package
            runExternalTool' (__ "Source Dist") (cabalCommand prefs) (["sdist"]
                            ++ (ipdSdistFlags package)) dir (logOutput logLaunch))
            (\(e :: SomeException) -> putStrLn (show e))


packageOpenDoc :: PackageAction
packageOpenDoc = do
    package <- ask

    liftIDE $ do
        prefs   <- readIDE prefs
        let path = ipdBuildDir package
                        </> "dist/doc/html"
                        </> display (pkgName (ipdPackageId package))
                        </> "index.html"
            dir = ipdBuildDir package
#ifdef WEBKITGTK
        loadDoc ("file:///" ++ dir </> path)
        getDocumentation Nothing  >>= \ p -> displayPane p False
#else
        openBrowser path
#endif
      `catchIDE`
        (\(e :: SomeException) -> putStrLn (show e))


runPackage ::  (ProcessHandle -> IDEAction)
            -> String
            -> FilePath
            -> [String]
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
                        reflectIDE (ideMessage Normal ((__ "Can't load package ") ++(show e))) ideR
                        return Nothing))

getEmptyModuleTemplate :: PackageDescription -> String -> IO String
getEmptyModuleTemplate pd modName = getModuleTemplate "module" pd modName "" ""

getModuleTemplate :: String -> PackageDescription -> String -> String -> String -> IO String
getModuleTemplate template pd modName exports body = catch (do
    dataDir  <- getDataDir
    filePath <- getConfigFilePathForLoad (template ++ leksahTemplateFileExtension) Nothing dataDir
    template <- UTF8.readFile filePath
    return (foldl' (\ a (from, to) -> replace from to a) template
        [   ("@License@"      , (display . license) pd)
        ,   ("@Maintainer@"   , maintainer pd)
        ,   ("@Stability@"    , stability pd)
        ,   ("@Portability@"  , "")
        ,   ("@Copyright@"    , copyright pd)
        ,   ("@ModuleName@"   , modName)
        ,   ("@ModuleExports@", exports)
        ,   ("@ModuleBody@"   , body)]))
                    (\ (e :: SomeException) -> sysMessage Normal (printf (__ "Couldn't read template file: %s") (show e)) >> return "")

data ModuleLocation = LibExposedMod | LibOtherMod | ExeOrTestMod String

addModuleToPackageDescr :: ModuleName -> [ModuleLocation] -> PackageAction
addModuleToPackageDescr moduleName locations = do
    p    <- ask
    liftIDE $ reifyIDE (\ideR -> catch (do
        gpd <- readPackageDescription normal (ipdCabalFile p)
        let npd = trace (show gpd) foldr addModule gpd locations
        writeGenericPackageDescription (ipdCabalFile p) npd)
           (\(e :: SomeException) -> do
            reflectIDE (ideMessage Normal ((__ "Can't update package ") ++ show e)) ideR
            return ()))
  where
    addModule LibExposedMod gpd@GenericPackageDescription{condLibrary = Just lib} =
        gpd {condLibrary = Just (addModToLib moduleName lib)}
    addModule LibOtherMod gpd@GenericPackageDescription{condLibrary = Just lib} =
        gpd {condLibrary = Just (addModToBuildInfoLib moduleName lib)}
    addModule (ExeOrTestMod name) gpd = gpd {
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
            reflectIDE (ideMessage Normal ((__ "Can't update package ") ++ show e)) ideR
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
isExposedModule mn (Just CondNode{condTreeData = lib}) = elem mn (exposedModules lib)

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

interactiveFlag :: String -> Bool -> String
interactiveFlag name f = (if f then "-f" else "-fno-") ++ name

printEvldWithShowFlag :: Bool -> String
printEvldWithShowFlag = interactiveFlag "print-evld-with-show"

breakOnExceptionFlag :: Bool -> String
breakOnExceptionFlag = interactiveFlag "break-on-exception"

breakOnErrorFlag :: Bool -> String
breakOnErrorFlag = interactiveFlag "break-on-error"

printBindResultFlag :: Bool -> String
printBindResultFlag = interactiveFlag "print-bind-result"

interactiveFlags :: Prefs -> [String]
interactiveFlags prefs =
    (printEvldWithShowFlag $ printEvldWithShow prefs)
    : (breakOnExceptionFlag $ breakOnException prefs)
    : (breakOnErrorFlag $ breakOnError prefs)
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
                let dir = ipdBuildDir package
                mbExe <- readIDE activeExe
                ghci <- reifyIDE $ \ideR -> newGhci dir mbExe (interactiveFlags prefs')
                    $ reflectIDEI (logOutputForBuild package True False >> return ()) ideR
                modifyIDE_ (\ide -> ide {debugState = Just (package, ghci)})
                triggerEventIDE (Sensitivity [(SensitivityInterpreting, True)])
                setDebugToggled True
                -- Fork a thread to wait for the output from the process to close
                liftIO $ forkIO $ do
                    readMVar (outputClosed ghci)
                    postGUISync . (`reflectIDE` ideRef) $ do
                        setDebugToggled False
                        modifyIDE_ (\ide -> ide {debugState = Nothing, autoCommand = return ()})
                        triggerEventIDE (Sensitivity [(SensitivityInterpreting, False)])
                        -- Kick of a build if one is not already due
                        modifiedPacks <- fileCheckAll belongsToPackages
                        let modified = not (null modifiedPacks)
                        prefs <- readIDE prefs
                        when ((not modified) && (backgroundBuild prefs)) $ do
                            -- So although none of the pakages are modified,
                            -- they may have been modified in ghci mode.
                            -- Lets build to make sure the binaries are up to date
                            mbPackage   <- readIDE activePack
                            case mbPackage of
                                Just package -> runCabalBuild True False False False package True (\ _ -> return ())
                                Nothing -> return ()
                return ()
            _ -> do
                sysMessage Normal (__ "Debugger already running")
                return ())
            (\(e :: SomeException) -> putStrLn (show e))

tryDebug :: DebugAction -> PackageAction
tryDebug f = do
    maybeDebug <- liftIDE $ readIDE debugState
    case maybeDebug of
        Just debug -> do
            -- TODO check debug package matches active package
            liftIDE $ runDebug f debug
        _ -> do
            window <- liftIDE $ getMainWindow
            resp <- liftIO $ do
                md <- messageDialogNew (Just window) [] MessageQuestion ButtonsCancel
                        (__ "GHCi debugger is not running.")
                dialogAddButton md (__ "_Start GHCi") (ResponseUser 1)
                dialogSetDefaultResponse md (ResponseUser 1)
                set md [ windowWindowPosition := WinPosCenterOnParent ]
                resp <- dialogRun md
                widgetDestroy md
                return resp
            case resp of
                ResponseUser 1 -> do
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
        Just debug -> do
            -- TODO check debug package matches active package
            liftIDE $ runDebug f debug
        _ -> do
            return ()

executeDebugCommand :: String -> (C.Sink ToolOutput IDEM ()) -> DebugAction
executeDebugCommand command handler = do
    (_, ghci) <- ask
    lift $ do
        reifyIDE $ \ideR -> do
            liftIO $ postGUIAsync $ reflectIDE (do
                triggerEventIDE (StatusbarChanged [CompartmentState command, CompartmentBuild True])
                return ()) ideR
            executeGhciCommand ghci command $ do
                reflectIDEI handler ideR
                liftIO $ postGUIAsync $ reflectIDE (do
                    triggerEventIDE (StatusbarChanged [CompartmentState "", CompartmentBuild False])
                    return ()) ideR
                return ()

allBuildInfo' :: PackageDescription -> [BuildInfo]
allBuildInfo' pkg_descr = [ libBuildInfo lib  | Just lib <- [library pkg_descr] ]
                       ++ [ buildInfo exe     | exe <- executables pkg_descr ]
                       ++ [ testBuildInfo tst | tst <- testSuites pkg_descr ]
testMainPath (TestSuiteExeV10 _ f) = [f]
testMainPath _ = []

idePackageFromPath' :: FilePath -> IDEM (Maybe IDEPackage)
idePackageFromPath' ipdCabalFile = do
    mbPackageD <- reifyIDE (\ideR -> catch (do
        pd <- readPackageDescription normal ipdCabalFile
        return (Just (flattenPackageDescription pd)))
            (\ (e  :: SomeException) -> do
                reflectIDE (ideMessage Normal ((__ "Can't activate package ") ++(show e))) ideR
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
                ipdSrcDirs          = case (nub $ concatMap hsSourceDirs (allBuildInfo' packageD)) of
                                            [] -> [".","src"]
                                            l -> l
                ipdExes             = [ exeName e | e <- executables packageD
                                          , buildable (buildInfo e) ]
                ipdExtensions       = nub $ concatMap oldExtensions (allBuildInfo' packageD)
                ipdTests            = [ testName t | t <- testSuites packageD
                                          , buildable (testBuildInfo t) ]
                ipdBenchmarks       = [ benchmarkName b | b <- benchmarks packageD
                                          , buildable (benchmarkBuildInfo b) ]
                ipdPackageId        = package packageD
                ipdDepends          = buildDepends packageD
                ipdHasLibs          = hasLibs packageD
                ipdConfigFlags      = ["--user", "--enable-tests"]
                ipdBuildFlags       = []
                ipdTestFlags        = []
                ipdHaddockFlags     = []
                ipdExeFlags         = []
                ipdInstallFlags     = []
                ipdRegisterFlags    = []
                ipdUnregisterFlags  = []
                ipdSdistFlags       = []
                ipdSandboxSources   = []
                packp               = IDEPackage {..}
                pfile               = dropExtension ipdCabalFile
            pack <- (do
                flagFileExists <- liftIO $ doesFileExist (pfile ++ leksahFlagFileExtension)
                if flagFileExists
                    then liftIO $ readFlags (pfile ++ leksahFlagFileExtension) packp
                    else return packp)
            return (Just pack)

idePackageFromPath :: C.Sink ToolOutput IDEM () -> FilePath -> IDEM (Maybe IDEPackage)
idePackageFromPath log filePath = do
    mbRootPackage <- idePackageFromPath' filePath
    case mbRootPackage of
        Nothing -> return Nothing
        Just rootPackage -> do
            mvar <- liftIO newEmptyMVar
            runExternalTool' "" "cabal" ["sandbox", "list-sources"] (takeDirectory filePath) $ do
                output <- CL.consume
                liftIO . putMVar mvar $ case take 1 $ reverse output of
                    [ToolExit ExitSuccess] ->
                        map (T.unpack . toolline) . takeWhile (/= ToolOutput "") . drop 1 $ dropWhile (/= ToolOutput "") output
                    _ -> []
            paths <- liftIO $ takeMVar mvar
            sandboxSources <- concat <$> (forM paths $ \ path -> do
                contents <- liftIO $ getDirectoryContents path
                return . take 1 . map (path </>) $ filter ((== ".cabal") . takeExtension) contents)
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

