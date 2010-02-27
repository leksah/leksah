{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}
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
    packageOpen
,   packageOpenThis
,   packageNew
,   packageConfig
,   buildPackage

,   packageDoc
,   packageClean
,   packageCopy
,   packageRun
,   activatePackage
,   deactivatePackage
,   getActivePackage

,   packageInstall
,   packageInstall'
,   packageRegister
,   packageUnregister
,   packageTest
,   packageSdist
,   packageOpenDoc

,   getPackageDescriptionAndPath
,   getModuleTemplate
,   addModuleToPackageDescr

,   backgroundBuildToggled
,   backgroundLinkToggled

,   debugStart
,   debugToggled
,   choosePackageFile

,   idePackageFromPath
) where

import Graphics.UI.Gtk
import Control.Monad.Reader
import Distribution.Package hiding (depends,packageId)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Verbosity
import System.FilePath
import Control.Concurrent
import System.Directory (setCurrentDirectory, doesFileExist)
import Prelude hiding (catch)
import Data.Maybe (isJust, fromJust)
import Control.Exception (SomeException(..), catch)
import Paths_leksah

import IDE.Core.State
import IDE.Utils.GUIUtils
import IDE.Pane.PackageEditor
import IDE.Pane.SourceBuffer
import IDE.Pane.PackageFlags (readFlags)
import Distribution.Text (display)
import IDE.Utils.FileUtils(getConfigFilePathForLoad)
import IDE.LogRef
import IDE.Debug
import MyMissing (replace)
import Distribution.ModuleName (ModuleName(..))
import Data.List (isInfixOf, nub, foldl')
import qualified System.IO.UTF8 as UTF8  (readFile)
import IDE.Utils.Tool (ToolOutput(..), runTool, newGhci, ToolState(..))
import Debug.Trace (trace)

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Data.Maybe (isNothing)
import System.Win32
    (DWORD(..))
import System.Process (getProcessExitCode)
#else
import System.Posix
    (getGroupProcessStatus,
     sigINT,
     installHandler,
     signalProcessGroup,
     getProcessGroupID)
import System.Posix.Signals (Handler(..))
import Foreign.C (Errno(..), getErrno)
#endif
-- Leave at least one import ofter this #endif
-- so the auto import tool does not add stuf insied the

import qualified Data.Set as  Set (fromList)
import qualified Data.Map as  Map (empty)
import System.Exit (ExitCode(..))


#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
foreign import stdcall unsafe "winbase.h GetCurrentProcessId"
    c_GetCurrentProcessId :: IO DWORD

foreign import stdcall unsafe "winbase.h GetProcessId"
    c_GetProcessId :: DWORD -> IO DWORD
#endif

packageNew :: IDEAction
packageNew = packageNew' (\fp -> do
    triggerEventIDE (WorkspaceAddPackage fp)
    mbPack <- idePackageFromPath fp
    activatePackage mbPack)

packageOpen :: IDEAction
packageOpen = packageOpenThis Nothing

packageOpenThis :: Maybe FilePath -> IDEAction
packageOpenThis mbFilePath = do
    active <- readIDE activePack
    case active of
        Just p -> deactivatePackage
        Nothing -> return ()
    selectActivePackage mbFilePath
    return ()

getActivePackage :: IDEM (Maybe IDEPackage)
getActivePackage = readIDE activePack

selectActivePackage :: Maybe FilePath -> IDEM (Maybe IDEPackage)
selectActivePackage mbFilePath' = do
    window     <- getMainWindow
    mbFilePath <- case mbFilePath' of
                    Nothing -> liftIO $ choosePackageFile window
                    Just fp -> return (Just fp)
    case mbFilePath of
        Nothing -> return Nothing
        Just filePath -> idePackageFromPath filePath >>= (\ p -> activatePackage p >> return p)

activatePackage :: Maybe IDEPackage -> IDEM ()
activatePackage mbPack@(Just pack) = do
        modifyIDE_ (\ide -> ide{activePack = mbPack})
        liftIO $ setCurrentDirectory (dropFileName (ipdCabalFile pack))
        triggerEventIDE (ActivePack mbPack)
        triggerEventIDE (Sensitivity [(SensitivityProjectActive,True)])
        mbWs <- readIDE workspace
        let wsStr = case mbWs of
                Nothing -> ""
                Just ws -> wsName ws
        let txt = wsStr ++ " > " ++ packageIdentifierToString (ipdPackageId pack)
        triggerEventIDE (StatusbarChanged [CompartmentPackage txt])
        return ()
activatePackage Nothing = return ()

deactivatePackage :: IDEAction
deactivatePackage = do
    oldActivePack <- readIDE activePack
    modifyIDE_ (\ide -> ide{activePack = Nothing})
    triggerEventIDE (ActivePack Nothing)
    when (isJust oldActivePack) $ do
        triggerEventIDE (Sensitivity [(SensitivityProjectActive,False)])
        return ()
    mbWs <- readIDE workspace
    let wsStr = case mbWs of
                    Nothing -> ""
                    Just ws -> wsName ws
    let txt = wsStr ++ ":"
    triggerEventIDE (StatusbarChanged [CompartmentPackage txt])
    return ()

packageConfig :: IDEAction
packageConfig = do
        mbPackage   <- getActivePackage
        case mbPackage of
            Nothing         -> return ()
            Just package    -> packageConfig' package (return ())

packageConfig'  :: IDEPackage -> IDEAction -> IDEAction
packageConfig' package continuation = catchIDE (do
    mbPackageD  <- reifyIDE (\ideR ->  catch (do
        let dir = dropFileName (ipdCabalFile package)
        reflectIDE (runExternalTool "Configuring" "runhaskell" (["Setup","configure"]
                                        ++ (ipdConfigFlags package)) (Just dir) logOutput) ideR
        pd  <- readPackageDescription normal (ipdCabalFile package)
        return (Just (flattenPackageDescription pd)))
        (\(e :: SomeException) -> do
                reflectIDE (ideMessage Normal (show e)) ideR
                return Nothing))
    case mbPackageD of
        Just packageD ->
            let modules    = Set.fromList $ libModules packageD ++ exeModules packageD
                files      = Set.fromList $ extraSrcFiles packageD ++ map modulePath (executables packageD)
                ipdSrcDirs = nub $ concatMap hsSourceDirs (allBuildInfo packageD)
                pack       = Just package{ipdDepends=buildDepends packageD, ipdModules = modules,
                             ipdExtraSrcs = files, ipdSrcDirs = ipdSrcDirs}
            in trace ("packageD :" ++ show packageD) $ do
                modifyIDE_ (\ide -> ide{activePack = pack, bufferProjCache = Map.empty})
                triggerEventIDE (ActivePack pack)
                continuation
                return ()
        Nothing -> return()) -- Does not continue if config fails?
        (\(e :: SomeException) -> putStrLn (show e))

runCabalBuild :: Bool -> IDEPackage -> Bool -> (Bool -> IDEAction) -> IDEAction
runCabalBuild backgroundBuild package shallConfigure continuation = do
    prefs   <- readIDE prefs
    let dir =  dropFileName (ipdCabalFile package)
    let args = (["Setup","build"] ++
                if ((not backgroundBuild) || (backgroundLink prefs))
                    then []
                    else ["--ghc-options=-c", "--with-ar=true", "--with-ld=true"]
                        ++ ipdBuildFlags package)
    runExternalTool "Building" "runhaskell" args (Just dir) $ \output -> do
        logOutputForBuild dir backgroundBuild output
        errs <- readIDE errorRefs
        if shallConfigure && isConfigError output
            then
                packageConfig' package (runCabalBuild backgroundBuild package False continuation)
            else do
                continuation (last output == ToolExit ExitSuccess && not (any isError errs))
                return ()

isConfigError :: [ToolOutput] -> Bool
isConfigError = or . (map isCErr)
    where
    isCErr (ToolError str) = str1 `isInfixOf` str || str2 `isInfixOf` str
    isCErr _ = False
    str1 = "Run the 'configure' command first"
    str2 = "please re-configure"


-- TODO Care for metadata

buildPackage :: Bool -> IDEPackage -> (Bool -> IDEAction) -> IDEAction
buildPackage backgroundBuild  package continuation = catchIDE (do
    ideR      <- ask
    prefs     <- readIDE prefs
    maybeGhci <- readIDE ghciState
    case maybeGhci of
        Nothing -> do
            alreadyRunning <- isRunning
            if alreadyRunning
                then do
                    interruptBuild
                    when (not backgroundBuild) $ liftIO $ do
                        timeoutAddFull (do
                            reflectIDE (do buildPackage backgroundBuild  package continuation; return False) ideR
                            return False) priorityDefaultIdle 1000
                        return ()
                else runCabalBuild backgroundBuild package True continuation
        Just ghci -> do
            ready <- liftIO $ isEmptyMVar (currentToolCommand ghci)
            when ready $ do
                let dir = dropFileName (ipdCabalFile package)
                when (saveAllBeforeBuild prefs) (do fileSaveAll belongsToWorkspace; return ())
                executeDebugCommand ":reload" $ logOutputForBuild dir backgroundBuild
    )
    (\(e :: SomeException) -> sysMessage Normal (show e))

--buildAndConfigPackage :: Bool -> IDEPackage -> (Bool -> IDEAction) -> IDEAction
--buildAndConfigPackage backgroundBuild package continuation = buildPackage backgroundBuild package continuation

packageDoc :: IDEAction
packageDoc = catchIDE (do
        mbPackage   <- getActivePackage
        case mbPackage of
            Nothing         -> return ()
            Just package    -> let dir = dropFileName (ipdCabalFile package)
                               in runExternalTool "Documenting" "runhaskell" (["Setup","haddock"]
                                                ++ (ipdHaddockFlags package)) (Just dir) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))

packageClean :: Maybe IDEPackage -> IDEAction
packageClean Nothing = do
    mbPackage   <- getActivePackage
    case mbPackage of
        Nothing         -> return ()
        Just package    -> packageClean' package
packageClean (Just package) = packageClean' package

packageClean' :: IDEPackage -> IDEAction
packageClean' package =
    let dir = dropFileName (ipdCabalFile package)
    in runExternalTool "Cleaning" "runhaskell" ["Setup","clean"] (Just dir) logOutput

packageCopy :: IDEAction
packageCopy = catchIDE (do
        mbPackage   <- getActivePackage
        window      <- getMainWindow
        mbDir       <- liftIO $ chooseDir window "Select the target directory" Nothing
        case mbDir of
            Nothing -> return ()
            Just fp ->
                case mbPackage of
                    Nothing         -> return ()
                    Just package    -> let dir = dropFileName (ipdCabalFile package)
                                       in runExternalTool "Copying" "runhaskell" (["Setup","copy"]
                                                ++ ["--destdir=" ++ fp]) (Just dir) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))

packageRun :: IDEAction
packageRun = catchIDE (do
        ideR        <- ask
        mbPackage   <- getActivePackage
        maybeGhci   <- readIDE ghciState
        case mbPackage of
            Nothing         -> return ()
            Just package    -> do
                pd <- liftIO $ readPackageDescription normal (ipdCabalFile package) >>= return . flattenPackageDescription
                case maybeGhci of
                    Nothing -> do
                        case executables pd of
                            (Executable name _ _):_ -> do
                                let path = "dist/build" </> name </> name
                                let dir = dropFileName (ipdCabalFile package)
                                runExternalTool ("Running " ++ name) path (ipdExeFlags package) (Just dir) logOutput
                            otherwise -> do
                                sysMessage Normal "no executable in selected package"
                                return ()
                    Just ghci -> do
                        case executables pd of
                            (Executable _ mainFilePath _):_ -> do
                                executeDebugCommand (":module *" ++ (map (\c -> if c == '/' then '.' else c) (takeWhile (/= '.') mainFilePath))) logOutput
                                executeDebugCommand (":main " ++ (unwords (ipdExeFlags package))) logOutput
                            otherwise -> do
                                sysMessage Normal "no executable in selected package"
                                return ())
        (\(e :: SomeException) -> putStrLn (show e))

packageInstall :: IDEAction
packageInstall = do
        mbPackage   <- getActivePackage
        case mbPackage of
            Nothing         -> return ()
            Just package    -> packageInstall' package (\ _ -> return ())

packageInstall' :: IDEPackage -> (Bool -> IDEAction) -> IDEAction
packageInstall' package continuation = catchIDE (do
   let dir = dropFileName (ipdCabalFile package)
   runExternalTool "Installing" "runhaskell" (["Setup","install"]
                    ++ (ipdInstallFlags package)) (Just dir) (\ to -> logOutput to >> continuation True)) --TODO
        (\(e :: SomeException) -> putStrLn (show e))

packageRegister :: IDEAction
packageRegister = catchIDE (do
        mbPackage   <- getActivePackage
        case mbPackage of
            Nothing         -> return ()
            Just package    -> let dir = dropFileName (ipdCabalFile package)
                               in runExternalTool "Registering" "runhaskell" (["Setup","register"]
                                                ++ (ipdRegisterFlags package)) (Just dir) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))

packageUnregister :: IDEAction
packageUnregister = catchIDE (do
    mbPackage   <- getActivePackage
    case mbPackage of
        Nothing         -> return ()
        Just package    -> let dir = dropFileName (ipdCabalFile package)
                           in runExternalTool "Unregistering" "runhaskell" (["Setup","unregister"]
                                            ++ (ipdUnregisterFlags package)) (Just dir) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))

packageTest :: IDEAction
packageTest = catchIDE (do
        mbPackage   <- getActivePackage
        case mbPackage of
            Nothing         -> return ()
            Just package    -> let dir = dropFileName (ipdCabalFile package)
                               in runExternalTool "Testing" "runhaskell" (["Setup","test"]) (Just dir) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))

packageSdist :: IDEAction
packageSdist = catchIDE (do
        mbPackage   <- getActivePackage
        case mbPackage of
            Nothing         -> return ()
            Just package    -> let dir = dropFileName (ipdCabalFile package)
                               in runExternalTool "Source Dist" "runhaskell" (["Setup","sdist"]
                                                ++ (ipdSdistFlags package)) (Just dir) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))


packageOpenDoc :: IDEAction
packageOpenDoc = catchIDE (do
        mbPackage   <- getActivePackage
        prefs       <- readIDE prefs
        case mbPackage of
            Nothing         -> return ()
            Just package    ->
                let path = dropFileName (ipdCabalFile package)
                                </> "dist/doc/html"
                                </> display (pkgName (ipdPackageId package))
                                </> display (pkgName (ipdPackageId package))
                                </> "index.html"
                    dir = dropFileName (ipdCabalFile package)
                in runExternalTool "Opening Documentation" (browser prefs) [path] (Just dir) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))

runExternalTool :: String -> FilePath -> [String] -> Maybe FilePath -> ([ToolOutput] -> IDEAction) -> IDEAction
runExternalTool description executable args mbDir handleOutput = do
        prefs          <- readIDE prefs
        alreadyRunning <- isRunning
        unless alreadyRunning $ do
            when (saveAllBeforeBuild prefs) (do fileSaveAll belongsToWorkspace; return ())
            triggerEventIDE (StatusbarChanged [CompartmentState description, CompartmentBuild True])
            reifyIDE (\ideR -> forkIO $ do
                (output, pid) <- runTool executable args mbDir
                reflectIDE (do
                    modifyIDE_ (\ide -> ide{runningTool = Just pid})
                    handleOutput output) ideR)
            return ()


-- ---------------------------------------------------------------------
-- | Handling of Compiler errors
--
isRunning :: IDEM Bool
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
isRunning = do
    maybeProcess <- readIDE runningTool
    liftIO $ do
        case maybeProcess of
            Just process -> do
                maybeExitCode <- getProcessExitCode process
                return $ isNothing maybeExitCode
            Nothing -> return False
--    Once we can interupt the build on windows, then something like this might be needed
--    alreadyRunning <- liftIO $ do
--        withTh32Snap tH32CS_SNAPPROCESS Nothing (\h -> do
--            all <- th32SnapEnumProcesses h
--            currentId <- c_GetCurrentProcessId
--            return $ not $ null $ filter (\(_, _, parentId, _, _) -> parentId == currentId) all)
#else
isRunning = do
    ideR <- ask
    liftIO $ (do
        group <- getProcessGroupID
        status <- getGroupProcessStatus False False group
        -- putStrLn $ "Status " ++ show status
        case status of
            Just _ -> reflectIDE isRunning ideR
            Nothing -> return True)
        `catch` (\(e :: IOError) -> do
            Errno errno <- liftIO $ getErrno
            -- putStrLn $ "Error " ++ show errno
            return $ errno /= 10)
#endif

interruptBuild :: IDEAction
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
interruptBuild = do
    -- I can't get this to work
    --maybeProcess <- readIDE runningTool
    --liftIO $ do
    --    processGroupId <- case maybeProcess of
    --        Just h -> do
    --            withProcessHandle h (\h2 -> do
    --                case h2 of
    --                    OpenHandle oh -> do
    --                        pid <- c_GetProcessId oh
    --                        return (h2, pid)
    --                    _ -> return (h2, 0))
    --        _ -> return 0
    --    old <- installHandler Ignore
    --    putStrLn $ show processGroupId
    --    generateConsoleCtrlEvent cTRL_BREAK_EVENT processGroupId
    --    installHandler old
    return ()
#else
interruptBuild = liftIO $ do
    group <- getProcessGroupID
    old_int <- installHandler sigINT Ignore Nothing
    signalProcessGroup sigINT group
    installHandler sigINT old_int Nothing
    return ()
#endif

-- ---------------------------------------------------------------------
-- | * Utility functions/procedures, that have to do with packages
--

getPackageDescriptionAndPath :: IDEM (Maybe (PackageDescription,FilePath))
getPackageDescriptionAndPath = do
    active <- readIDE activePack
    case active of
        Nothing -> do
            ideMessage Normal "No active packjage"
            return Nothing
        Just p  -> do
            ideR <- ask
            reifyIDE (\ideR -> catch (do
                pd <- readPackageDescription normal (ipdCabalFile p)
                return (Just (flattenPackageDescription pd,ipdCabalFile p)))
                    (\(e :: SomeException) -> do
                        reflectIDE (ideMessage Normal ("Can't load package " ++(show e))) ideR
                        return Nothing))

getModuleTemplate :: PackageDescription -> String -> IO String
getModuleTemplate pd modName = catch (do
    dataDir  <- getDataDir
    filePath <- getConfigFilePathForLoad standardModuleTemplateFilename Nothing dataDir
    template <- UTF8.readFile filePath
    return (foldl' (\ a (from, to) -> replace from to a) template
        [("@License@", (show . license) pd), ("@Maintainer@", maintainer pd),
            ("@Stability@",stability pd), ("@Portability@",""),
                ("@Copyright@", copyright pd),("@ModuleName@", modName)]))
                    (\ (e :: SomeException) -> sysMessage Normal ("Couldn't read template file: " ++ show e) >> return "")


addModuleToPackageDescr :: ModuleName -> Bool -> IDEM ()
addModuleToPackageDescr moduleName isExposed = do
    active <- readIDE activePack
    case active of
        Nothing -> do
            ideMessage Normal "No active packjage"
            return ()
        Just p  -> do
            ideR <- ask
            reifyIDE (\ideR -> catch (do
                gpd <- readPackageDescription normal (ipdCabalFile p)
                if hasConfigs gpd
                    then do
                        reflectIDE (ideMessage High
                            "Cabal file with configurations can't be automatically updated with the current version of Leksah") ideR
                    else
                        let pd = flattenPackageDescription gpd
                            npd = if isExposed && isJust (library pd)
                                    then pd{library = Just ((fromJust (library pd)){exposedModules =
                                                                    moduleName : exposedModules (fromJust $ library pd)})}
                                    else let npd1 = case library pd of
                                                       Nothing -> pd
                                                       Just lib -> pd{library = Just (lib{libBuildInfo =
                                                                addModToBuildInfo (libBuildInfo lib) moduleName})}
                                         in npd1{executables = map
                                                (\exe -> exe{buildInfo = addModToBuildInfo (buildInfo exe) moduleName})
                                                    (executables npd1)}
                        in writePackageDescription (ipdCabalFile p) npd)
                           (\(e :: SomeException) -> do
                            reflectIDE (ideMessage Normal ("Can't upade package " ++ show e)) ideR
                            return ()))
    where
    addModToBuildInfo :: BuildInfo -> ModuleName -> BuildInfo
    addModToBuildInfo bi mn = bi {otherModules = mn : otherModules bi}

backgroundBuildToggled :: IDEAction
backgroundBuildToggled = do
    toggled <- getBackgroundBuildToggled
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){backgroundBuild= toggled}})

backgroundLinkToggled :: IDEAction
backgroundLinkToggled = do
    toggled <- getBackgroundLinkToggled
    modifyIDE_ (\ide -> ide{prefs = (prefs ide){backgroundLink= toggled}})

-- ---------------------------------------------------------------------
-- | * Debug code that needs to use the package
--

debugStart :: IDEAction
debugStart = catchIDE (do
    ideRef <- ask
    mbPackage   <- getActivePackage
    prefs'      <- readIDE prefs
    case mbPackage of
        Nothing         -> return ()
        Just package    -> do
            maybeGhci <- readIDE ghciState
            case maybeGhci of
                Nothing -> do
                    let dir = dropFileName (ipdCabalFile package)
                    ghci <- reifyIDE $ \ideR -> newGhci (ipdBuildFlags package) (interactiveFlags prefs')
                        $ \output -> reflectIDE (logOutputForBuild dir True output) ideR
                    modifyIDE_ (\ide -> ide {ghciState = Just ghci})
                    triggerEventIDE (Sensitivity [(SensitivityInterpreting, True)])
                    -- Fork a thread to wait for the output from the process to close
                    liftIO $ forkIO $ do
                        readMVar (outputClosed ghci)
                        reflectIDE (do
                            modifyIDE_ (\ide -> ide {ghciState = Nothing})
                            triggerEventIDE (Sensitivity [(SensitivityInterpreting, False)])
                            -- Kick of a build if one is not already due
                            modifiedPacks <- fileCheckAll belongsToPackage
                            let modified = not (null modifiedPacks)
                            prefs <- readIDE prefs
                            when ((not modified) && (backgroundBuild prefs)) $ do
                                    -- TODO Check with Hamish what this means.
                                mbPackage   <- readIDE activePack
                                case mbPackage of
                                    Just package -> runCabalBuild True package True (\ _ -> return ())
                                    Nothing -> return ()) ideRef
                    return ()
                _ -> do
                    sysMessage Normal "Debugger already running"
                    return ())
        (\(e :: SomeException) -> putStrLn (show e))

debugToggled :: IDEAction
debugToggled = do
    toggled <- getDebugToggled
    if toggled
        then debugStart
        else debugQuit

idePackageFromPath :: FilePath -> IDEM (Maybe IDEPackage)
idePackageFromPath filePath = do
    mbPackageD <- reifyIDE (\ideR -> catch (do
        pd <- readPackageDescription normal filePath
        return (Just (flattenPackageDescription pd)))
            (\ (e  :: SomeException) -> do
                reflectIDE (ideMessage Normal ("Can't activate package " ++(show e))) ideR
                return Nothing))
    case mbPackageD of
        Nothing       -> return Nothing
        Just packageD -> do
            let modules    = Set.fromList $ libModules packageD ++ exeModules packageD --TODO Porting 6.12
            let mainFiles  = map modulePath (executables packageD)
            let files      = Set.fromList $ extraSrcFiles packageD ++ map modulePath (executables packageD)
            let ipdSrcDirs = nub $ concatMap hsSourceDirs (allBuildInfo packageD)
            let exts       = nub $ concatMap extensions (allBuildInfo packageD)
            let packp      = IDEPackage (package packageD) filePath (buildDepends packageD) modules

                             mainFiles files ipdSrcDirs exts ["--user"] [] [] [] [] [] [] []
            let pfile      = dropExtension filePath
            pack <- (do
                flagFileExists <- liftIO $ doesFileExist (pfile ++ leksahFlagFileExtension)
                if flagFileExists
                    then liftIO $ readFlags (pfile ++ leksahFlagFileExtension) packp
                    else return packp)
            return (Just pack)

