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
,   packageBuild
,   packageDoc
,   packageClean
,   packageCopy
,   packageRun
,   activatePackage
,   deactivatePackage
,   getActivePackage
,   belongsToActivePackage -- :: FilePath -> IDEM Bool

,   packageInstall
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
import System.Directory
    (doesFileExist, setCurrentDirectory)
import Prelude hiding (catch)
import Data.Maybe (isJust, fromJust)
import Control.Exception (SomeException(..), catch)

import Control.Event
import IDE.Core.State
import IDE.Pane.PackageEditor
import IDE.Pane.SourceBuffer
import IDE.Pane.PackageFlags
import Distribution.Text (display)
import IDE.FileUtils
    (moduleNameFromFilePath', isSubPath, getConfigFilePathForLoad)
import IDE.LogRef
import IDE.Debug
import MyMissing (replace)
import Distribution.ModuleName (ModuleName(..))
import Data.List (nub, foldl')
import qualified System.IO.UTF8 as UTF8  (readFile)
import IDE.Tool (ToolOutput(..), runTool, newGhci, ToolState(..))

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.Process (getProcessExitCode, ProcessHandle(..))
import Data.Maybe (isNothing)
import GHC.ConsoleHandler (Handler(..), installHandler)
import System.Win32
    (th32SnapEnumProcesses,
     DWORD(..),
     cTRL_BREAK_EVENT,
     generateConsoleCtrlEvent,
     tH32CS_SNAPPROCESS,
     withTh32Snap)
import System.Process.Internals
    (withProcessHandle, ProcessHandle__(..))
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
import IDE.Metainfo.Provider
    (rebuildActiveInfo)
import qualified Data.Set as  Set (member, fromList)
import qualified Data.Map as  Map (empty, insert, lookup)
import IDE.SourceCandy (getCandylessText)

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
foreign import stdcall unsafe "winbase.h GetCurrentProcessId"
    c_GetCurrentProcessId :: IO DWORD

foreign import stdcall unsafe "winbase.h GetProcessId"
    c_GetProcessId :: DWORD -> IO DWORD
#endif

packageNew :: IDEAction
packageNew = packageNew' (\fp -> activatePackage fp >> return ())

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
getActivePackage = do
    active <- readIDE activePack
    case active of
        Just p -> return (Just p)
        Nothing -> selectActivePackage Nothing

selectActivePackage :: Maybe FilePath -> IDEM (Maybe IDEPackage)
selectActivePackage mbFilePath' = do
    ideR       <- ask
    window     <- getMainWindow
    mbFilePath <- case mbFilePath' of
                    Nothing -> liftIO $ choosePackageFile window
                    Just fp -> return (Just fp)
    case mbFilePath of
        Nothing -> return Nothing
        Just filePath -> do
            let ppath = dropFileName filePath
            exists <- liftIO $ doesFileExist (ppath </> "IDE.session")
            wantToLoadSession <-
                if exists
                    then liftIO $ do
                        md  <- messageDialogNew Nothing [] MessageQuestion ButtonsYesNo
                                $ "Load the session settings stored with this project?"
                        rid <- dialogRun md
                        widgetDestroy md
                        case rid of
                            ResponseYes ->  return True
                            otherwise   ->  return False
                    else return False
            if wantToLoadSession
                then do
                    triggerEvent ideR (LoadSession (ppath </> "IDE.session"))
                    readIDE activePack
                else activatePackage filePath

activatePackage :: FilePath -> IDEM (Maybe IDEPackage)
activatePackage filePath = do
    ideR <- ask
    let ppath = dropFileName filePath
    mbPackageD <- reifyIDE (\ideR -> catch (do
        liftIO $ setCurrentDirectory ppath
        pd <- readPackageDescription normal filePath
        return (Just (flattenPackageDescription pd)))
            (\(e :: SomeException) -> do
                reflectIDE (ideMessage Normal ("Can't activate package " ++(show e))) ideR
                return Nothing))
    case mbPackageD of
        Nothing -> return (Nothing)
        Just packageD -> do
            let modules = Set.fromList $ libModules packageD ++ exeModules packageD
            let files = Set.fromList $ extraSrcFiles packageD ++ map modulePath (executables packageD)
            let srcDirs = nub $ concatMap hsSourceDirs (allBuildInfo packageD)
            let packp = IDEPackage (package packageD) filePath (buildDepends packageD) modules
                            files srcDirs [] [] [] [] [] [] [] []
            pack <- (do
                flagFileExists <- liftIO $ doesFileExist (ppath </> "IDE.flags")
                if flagFileExists
                    then liftIO $ readFlags (ppath </> "IDE.flags") packp
                    else return packp)
            modifyIDE_ (\ide -> return (ide{activePack = (Just pack), projFilesCache = Map.empty}))
            ide <- getIDE
            triggerEvent ideR ActivePack
            triggerEvent ideR (Sensitivity [(SensitivityProjectActive,True)])
            sb <- getSBActivePackage
            liftIO $ statusbarPop sb 1
            liftIO $ statusbarPush sb 1 (display $ packageId pack)
            removeRecentlyUsedPackage filePath
            return (Just pack)


belongsToActivePackage :: FilePath -> IDEBuffer -> IDEM Bool
belongsToActivePackage fp ideBuf = do
    projFilesCache' <-  readIDE projFilesCache
    activePack'     <-  readIDE activePack
    case Map.lookup fp projFilesCache' of
        Just b  -> return b
        Nothing -> case activePack' of
                        Nothing   -> return False
                        Just pack -> let basePath = dropFileName $ cabalFile pack in do
                                     r <- if isSubPath basePath fp
                                            then
                                                let srcPaths = map (\srcP -> basePath </> srcP) (srcDirs pack)
                                                    relPaths = map (\p -> makeRelative p fp) srcPaths in
                                                if or (map (\p -> Set.member p (extraSrcs pack)) relPaths)
                                                    then return True
                                                    else do
                                                    --        do
                                                        gtkbuf <- liftIO $ textViewGetBuffer (sourceView ideBuf)
                                                        candy  <- readIDE candy
                                                        text   <- liftIO $ getCandylessText candy gtkbuf
                                                        mbMn <- liftIO $ moduleNameFromFilePath' fp text
                                                        case mbMn of
                                                            Nothing -> return False
                                                            Just mn -> return (Set.member mn (modules pack))
                                            else return False
                                     modifyIDE_ (\ide -> return (ide{projFilesCache =
                                        Map.insert fp r projFilesCache'}))
                                     return r

deactivatePackage :: IDEAction
deactivatePackage = do
    ideR          <- ask
    oldActivePack <- readIDE activePack
    when (isJust oldActivePack) $ do
        triggerEvent ideR (SaveSession
            ((dropFileName . cabalFile . fromJust) oldActivePack </> "IDE.session"))
        addRecentlyUsedPackage ((cabalFile . fromJust) oldActivePack)
        return ()
    modifyIDE_ (\ide -> return (ide{activePack = Nothing, projFilesCache = Map.empty}))
    ideR          <- ask
    triggerEvent ideR ActivePack
    when (isJust oldActivePack) $ do
        triggerEvent ideR (Sensitivity [(SensitivityProjectActive,False)])
        return ()
    sb            <- getSBActivePackage
    liftIO $ statusbarPop sb 1
    liftIO $ statusbarPush sb 1 ""
    return ()

packageConfig :: IDEAction
packageConfig = catchIDE (do
        mbPackage   <- getActivePackage
        case mbPackage of
            Nothing         -> return ()
            Just package    -> do
                mbPackageD  <- reifyIDE (\ideR ->  catch (do
                    reflectIDE (runExternalTool "Configuring" "runhaskell" (["Setup","configure"]
                                                    ++ (configFlags package)) logOutput) ideR
                    pd  <- readPackageDescription normal (cabalFile package)
                    return (Just (flattenPackageDescription pd)))
                    (\(e :: SomeException) -> do
                            reflectIDE (ideMessage Normal (show e)) ideR
                            return Nothing))
                case mbPackageD of
                    Just packageD -> do
                        let modules = Set.fromList $ libModules packageD ++ exeModules packageD
                        let files = Set.fromList $ extraSrcFiles packageD ++ map modulePath (executables packageD)
                        let srcDirs = nub $ concatMap hsSourceDirs (allBuildInfo packageD)
                        modifyIDE_ (\ide -> return (ide{activePack =
                            Just package{depends=buildDepends packageD, modules = modules,
                                         extraSrcs = files, srcDirs = srcDirs}}))
                        ask >>= \ideR -> triggerEvent ideR ActivePack
                        return ()
                    Nothing -> return ())
        (\(e :: SomeException) -> putStrLn (show e))

runExternalTool :: String -> FilePath -> [String] -> ([ToolOutput] -> IDEAction) -> IDEAction
runExternalTool description executable args handleOutput = do
        prefs          <- readIDE prefs
        alreadyRunning <- isRunning
        unless alreadyRunning $ do
            when (saveAllBeforeBuild prefs) (do fileSaveAll belongsToActivePackage; return ())
            sb <- getSBErrors
            liftIO $statusbarPop sb 1
            liftIO $statusbarPush sb 1 description
            reifyIDE (\ideR -> forkIO $ do
                (output, pid) <- runTool executable args
                reflectIDE (do
                    modifyIDE_ (\ide -> return ide{runningTool = Just pid})
                    handleOutput output) ideR)
            return ()

runCabalBuild :: Bool -> IDEPackage -> IDEAction
runCabalBuild backgroundBuild package = do
        prefs       <- readIDE prefs
        let args = (["Setup","build"] ++
                    if ((not backgroundBuild) || (backgroundLink prefs))
                            then []
                            else ["--ghc-options=-c", "--with-ar=true", "--with-ld=true"]
                    ++ buildFlags package)
        runExternalTool "Building" "runhaskell" args $ \output -> do
            logOutputForBuild backgroundBuild output
            errs <- readIDE errorRefs
            when ((not backgroundBuild)
                && (collectAfterBuild prefs)
                && (not (any isError errs))) $ do
                    ideMessage Normal "Update meta info for active package"
                    rebuildActiveInfo

packageBuild :: Bool -> IDEAction
packageBuild backgroundBuild = catchIDE (do
        mbPackage   <- if backgroundBuild
                            then readIDE activePack
                            else getActivePackage
        ideR        <- ask
        prefs       <- readIDE prefs
        case mbPackage of
            Nothing         -> return ()
            Just package    -> do
                modified <- if saveAllBeforeBuild prefs
                                then fileCheckAll belongsToActivePackage
                                else return False
                when (not backgroundBuild || modified) $ do
                    maybeGhci <- readIDE ghciState
                    case maybeGhci of
                        Nothing -> do
                            alreadyRunning <- isRunning
                            if alreadyRunning
                                then do
                                    interruptBuild
                                    when (not backgroundBuild) $ liftIO $ do
                                        timeoutAddFull (do
                                            reflectIDE (do packageBuild False; return False) ideR
                                            return False) priorityDefaultIdle 100
                                        return ()
                                else runCabalBuild backgroundBuild package
                        Just ghci -> do
                            ready <- liftIO $ isEmptyMVar (currentToolCommand ghci)
                            when ready $ do
                                when (saveAllBeforeBuild prefs) (do fileSaveAll belongsToActivePackage; return ())
                                executeDebugCommand ":reload" $ logOutputForBuild backgroundBuild
        )
        (\(e :: SomeException) -> putStrLn (show e))

packageDoc :: IDEAction
packageDoc = catchIDE (do
        mbPackage   <- getActivePackage
        case mbPackage of
            Nothing         -> return ()
            Just package    -> runExternalTool "Documenting" "runhaskell" (["Setup","haddock"]
                                                ++ (haddockFlags package)) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))

packageClean :: IDEAction
packageClean = do
    mbPackage   <- getActivePackage
    case mbPackage of
        Nothing         -> return ()
        Just package    -> runExternalTool "Cleaning" "runhaskell" ["Setup","clean"] logOutput

packageCopy :: IDEAction
packageCopy = catchIDE (do
        mbPackage   <- getActivePackage
        mbDir       <- chooseDir "Select the target directory"
        case mbDir of
            Nothing -> return ()
            Just fp ->
                case mbPackage of
                    Nothing         -> return ()
                    Just package    -> runExternalTool "Copying" "runhaskell" (["Setup","copy"]
                                                ++ ["--destdir=" ++ fp]) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))

packageRun :: IDEAction
packageRun = catchIDE (do
        ideR        <- ask
        mbPackage   <- getActivePackage
        maybeGhci   <- readIDE ghciState
        case mbPackage of
            Nothing         -> return ()
            Just package    -> do
                pd <- liftIO $ readPackageDescription normal (cabalFile package) >>= return . flattenPackageDescription
                case maybeGhci of
                    Nothing -> do
                        case executables pd of
                            (Executable name _ _):_ -> do
                                let path = "dist/build" </> name </> name
                                runExternalTool ("Running "++name) path (exeFlags package) logOutput
                            otherwise -> do
                                sysMessage Normal "no executable in selected package"
                                return ()
                    Just ghci -> do
                        case executables pd of
                            (Executable _ mainFilePath _):_ -> do
                                executeDebugCommand (":module " ++ (map (\c -> if c == '/' then '.' else c) (takeWhile (/= '.') mainFilePath))) logOutput
                                executeDebugCommand (":main " ++ (unwords (exeFlags package))) logOutput
                            otherwise -> do
                                sysMessage Normal "no executable in selected package"
                                return ())
        (\(e :: SomeException) -> putStrLn (show e))

packageInstall :: IDEAction
packageInstall = catchIDE (do
        mbPackage   <- getActivePackage
        case mbPackage of
            Nothing         -> return ()
            Just package    -> runExternalTool "Installing" "runhaskell" (["Setup","install"]
                                                ++ (installFlags package)) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))

packageRegister :: IDEAction
packageRegister = catchIDE (do
        mbPackage   <- getActivePackage
        case mbPackage of
            Nothing         -> return ()
            Just package    -> runExternalTool "Registering" "runhaskell" (["Setup","register"]
                                                ++ (registerFlags package)) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))

packageUnregister :: IDEAction
packageUnregister = catchIDE (do
    mbPackage   <- getActivePackage
    case mbPackage of
        Nothing         -> return ()
        Just package    -> runExternalTool "Unregistering" "runhaskell" (["Setup","unregister"]
                                            ++ (unregisterFlags package)) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))

packageTest :: IDEAction
packageTest = catchIDE (do
        mbPackage   <- getActivePackage
        case mbPackage of
            Nothing         -> return ()
            Just package    -> runExternalTool "Testing" "runhaskell" (["Setup","test"]) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))

packageSdist :: IDEAction
packageSdist = catchIDE (do
        mbPackage   <- getActivePackage
        case mbPackage of
            Nothing         -> return ()
            Just package    -> runExternalTool "Source Dist" "runhaskell" (["Setup","sdist"]
                                                ++ (sdistFlags package)) logOutput)
        (\(e :: SomeException) -> putStrLn (show e))


packageOpenDoc :: IDEAction
packageOpenDoc = catchIDE (do
        mbPackage   <- getActivePackage
        prefs       <- readIDE prefs
        case mbPackage of
            Nothing         -> return ()
            Just package    ->
                let path = dropFileName (cabalFile package)
                                </> "dist/doc/html"
                                </> display (pkgName (packageId package))
                                </> display (pkgName (packageId package))
                                </> "index.html"
                in runExternalTool "Opening Documentation" (browser prefs) [path] logOutput)
        (\(e :: SomeException) -> putStrLn (show e))


chooseDir :: String -> IDEM (Maybe FilePath)
chooseDir str = do
    win <- getMainWindow
    liftIO $do
        dialog <- fileChooserDialogNew
                        (Just $ str)
                        (Just win)
                    FileChooserActionSelectFolder
                    [("gtk-cancel"
                    ,ResponseCancel)
                    ,("gtk-open"
                    ,ResponseAccept)]
        widgetShow dialog
        response <- dialogRun dialog
        case response of
            ResponseAccept -> do
                fn <- fileChooserGetFilename dialog
                widgetDestroy dialog
                return fn
            ResponseCancel -> do
                widgetDestroy dialog
                return Nothing
            ResponseDeleteEvent -> do
                widgetDestroy dialog
                return Nothing
            _ -> return Nothing


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
                pd <- readPackageDescription normal (cabalFile p)
                return (Just (flattenPackageDescription pd,cabalFile p)))
                    (\(e :: SomeException) -> do
                        reflectIDE (ideMessage Normal ("Can't load package " ++(show e))) ideR
                        return Nothing))

getModuleTemplate :: PackageDescription -> String -> IO String
getModuleTemplate pd modName = do
    filePath <- getConfigFilePathForLoad "Module.template"
    template <- UTF8.readFile filePath
    return (foldl' (\ a (from, to) -> replace from to a) template
        [("@License@", (show . license) pd), ("@Maintainer@", maintainer pd),
            ("@Stability@",stability pd), ("@Portability@",""),
                ("@Copyright@", copyright pd),("@ModuleName@", modName)])

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
                gpd <- readPackageDescription normal (cabalFile p)
                if hasConfigs gpd
                    then do
                        reflectIDE (ideMessage High
                            "Cabal File with configurations can't be automatically updated") ideR
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
                        in writePackageDescription (cabalFile p) npd)
                           (\(e :: SomeException) -> do
                            reflectIDE (ideMessage Normal ("Can't upade package " ++ show e)) ideR
                            return ()))
    where
    addModToBuildInfo :: BuildInfo -> ModuleName -> BuildInfo
    addModToBuildInfo bi mn = bi {otherModules = mn : otherModules bi}


addRecentlyUsedPackage :: FilePath -> IDEAction
addRecentlyUsedPackage fp = do
    state <- readIDE currentState
    when (not $ isStartingOrClosing state) $ do
        recentPackages' <- readIDE recentPackages
        unless (elem fp recentPackages') $
            modifyIDE_ (\ide -> return ide{recentPackages = take 12 (fp : recentPackages')})
        ask >>= \ideR -> triggerEvent ideR UpdateRecent
        return ()

removeRecentlyUsedPackage :: FilePath -> IDEAction
removeRecentlyUsedPackage fp = do
    state <- readIDE currentState
    when (not $ isStartingOrClosing state) $ do
        recentPackages' <- readIDE recentPackages
        when (elem fp recentPackages') $
            modifyIDE_ (\ide -> return ide{recentPackages = filter (\e -> e /= fp) recentPackages'})
        ask >>= \ideR -> triggerEvent ideR UpdateRecent
        return ()

backgroundBuildToggled :: IDEAction
backgroundBuildToggled = do
    toggled <- getBackgroundBuildToggled
    modifyIDE_ (\ide -> return (ide{prefs = (prefs ide){backgroundBuild= toggled}}))

backgroundLinkToggled :: IDEAction
backgroundLinkToggled = do
    toggled <- getBackgroundLinkToggled
    modifyIDE_ (\ide -> return (ide{prefs = (prefs ide){backgroundLink= toggled}}))

-- ---------------------------------------------------------------------
-- | * Debug code that needs to use the package
--

debugStart :: IDEAction
debugStart = catchIDE (do
        ideRef      <- ask
        mbPackage   <- getActivePackage
        prefs'      <- readIDE prefs
        case mbPackage of
            Nothing         -> return ()
            Just package    -> do
                maybeGhci <- readIDE ghciState
                case maybeGhci of
                    Nothing -> do
                        ghci <- reifyIDE $ \ideR -> newGhci (buildFlags package) (interactiveFlags prefs')
                            $ \output -> reflectIDE (logOutputForBuild True output) ideR
                        modifyIDE_ (\ide -> return ide {ghciState = Just ghci})
                        triggerEvent ideRef (Sensitivity [(SensitivityInterpreting, True)])
                        -- Fork a thread to wait for the output from the process to close
                        liftIO $ forkIO $ do
                            readMVar (outputClosed ghci)
                            reflectIDE (do
                                modifyIDE_ (\ide -> return ide {ghciState = Nothing})
                                triggerEvent ideRef (Sensitivity [(SensitivityInterpreting, False)])
                                -- Kick of a build if one is not already due
                                modified <- fileCheckAll belongsToActivePackage
                                prefs <- readIDE prefs
                                when ((not modified) && (backgroundBuild prefs)) $ do
                                    mbPackage   <- readIDE activePack
                                    case mbPackage of
                                        Just package -> runCabalBuild True package
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

