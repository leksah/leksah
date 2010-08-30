{-# LANGUAGE CPP, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Leksah
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
--  Main function of Leksah, an Haskell IDE written in Haskell
--
---------------------------------------------------------------------------------

module IDE.Leksah (
    leksah
) where

import Graphics.UI.Gtk
import Control.Monad.Reader
import Control.Concurrent
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map
import System.Console.GetOpt
import System.Environment
import Data.Version
import Prelude hiding(catch)

import qualified IDE.OSX as OSX
import qualified IDE.YiConfig as Yi

#ifdef LEKSAH_WITH_YI_DYRE
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>))
import Control.Applicative ((<$>))
import qualified Config.Dyre as Dyre
#endif

import Paths_leksah
import IDE.Session
import IDE.Core.State
import Control.Event
import IDE.SourceCandy
import IDE.Utils.FileUtils
import Graphics.UI.Editor.MakeEditor
import Graphics.UI.Editor.Parameters
import IDE.Command
import IDE.Pane.Preferences
import IDE.Keymap
import IDE.Pane.SourceBuffer
import IDE.Find
import Graphics.UI.Editor.Composite (filesEditor, maybeEditor)
import Graphics.UI.Editor.Simple
       (enumEditor, stringEditor)
import IDE.Metainfo.Provider (initInfo)
import IDE.Workspaces (backgroundMake)
import IDE.Utils.GUIUtils
import Network (withSocketsDo)
import Control.Exception
import System.Exit(exitFailure)
import qualified IDE.StrippedPrefs as SP
import IDE.Utils.Tool (runTool,toolline)
import IDE.System.Process(waitForProcess)
import System.Log
import System.Log.Logger(updateGlobalLogger,rootLoggerName,setLevel)
import Data.List (stripPrefix)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- --------------------------------------------------------------------
-- Command line options
--

data Flag =  VersionF | SessionN String | Help | Verbosity String
       deriving (Show,Eq)

options :: [OptDescr Flag]
options =   [Option ['v'] ["version"] (NoArg VersionF)
                "Show the version number of ide"
         ,   Option ['l'] ["loadSession"] (ReqArg SessionN "NAME")
                "Load session"
         ,   Option ['h'] ["help"] (NoArg Help)
                "Display command line options"
         ,   Option ['e'] ["verbosity"] (ReqArg Verbosity "Verbosity")
                "One of DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY"]

header = "Usage: leksah [OPTION...] files..."

ideOpts :: [String] -> IO ([Flag], [String])
ideOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options


-- ---------------------------------------------------------------------
-- | Main function
--

#ifdef LEKSAH_WITH_YI_DYRE
leksahDriver = Dyre.wrapMain Dyre.defaultParams
    { Dyre.projectName  = "yi"
    , Dyre.realMain     = \(config, mbError) -> do
        case mbError of
            Just error -> putStrLn $ "Error in yi configuration file : " ++ error
            Nothing    -> return ()
        realMain config
    , Dyre.showError    = \(config, _) error -> (config, Just error)
    , Dyre.configDir    = Just . getAppUserDataDirectory $ "yi"
    , Dyre.cacheDir     = Just $ ((</> "leksah") <$> (getAppUserDataDirectory "cache"))
    , Dyre.hidePackages = ["mtl"]
    , Dyre.ghcOpts = ["-DLEKSAH"]
    }

leksah yiConfig = leksahDriver (yiConfig, Nothing)
#else
leksah = realMain
#endif

realMain yiConfig = do
  withSocketsDo $ handleExceptions $ do
    args            <-  getArgs

    (o,_)           <-  ideOpts args
    isFirstStart    <-  liftM not $ hasSavedConfigFile standardPreferencesFilename
    let sessions        =   filter (\x -> case x of
                                        SessionN _ -> True
                                        _         -> False) o
    let sessionFilename =  if not (null sessions)
                                then  (head $ map (\ (SessionN x) -> x) sessions) ++ leksahSessionFileExtension
                                else  standardSessionFilename
    let verbosity'      =  catMaybes $
                                map (\x -> case x of
                                    Verbosity s -> Just s
                                    _           -> Nothing) o
    let verbosity       =  case verbosity' of
                               [] -> INFO
                               h:_ -> read h
    updateGlobalLogger rootLoggerName (\ l -> setLevel verbosity l)
    when (elem VersionF o)
        (sysMessage Normal $ "Leksah the Haskell IDE, version " ++ showVersion version)
    when (elem Help o)
        (sysMessage Normal $ "Leksah the Haskell IDE " ++ usageInfo header options)
    dataDir         <- getDataDir
    prefsPath       <- getConfigFilePathForLoad standardPreferencesFilename Nothing dataDir
    prefs           <- readPrefs prefsPath
    when (not (elem VersionF o) && not (elem Help o))
        (startGUI yiConfig sessionFilename prefs isFirstStart)

handleExceptions inner =
  catch inner (\(exception :: SomeException) -> do
    sysMessage Normal ("leksah: internal IDE error: " ++ show exception)
    exitFailure
  )

-- ---------------------------------------------------------------------
-- | Start the GUI

startGUI :: Yi.Config -> String -> Prefs -> Bool -> IO ()
startGUI yiConfig sessionFilename iprefs isFirstStart = do
  Yi.start yiConfig $ \yiControl -> do
    st          <-  unsafeInitGUIForThreadedRTS
    when rtsSupportsBoundThreads
        (sysMessage Normal "Linked with -threaded")
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100 -- maybe switch back to priorityHigh/???
    mapM_ (sysMessage Normal) st
    initGtkRc
    dataDir       <- getDataDir
    mbStartupPrefs <- if not isFirstStart
                                then return $ Just iprefs
                                else do
                                    firstStartOK <- firstStart iprefs
                                    if not firstStartOK
                                        then return Nothing
                                        else do
                                            prefsPath  <- getConfigFilePathForLoad standardPreferencesFilename Nothing dataDir
                                            prefs <- readPrefs prefsPath
                                            return $ Just prefs
    case mbStartupPrefs of
        Nothing           -> return ()
        Just startupPrefs -> startMainWindow yiControl sessionFilename startupPrefs isFirstStart

startMainWindow yiControl sessionFilename startupPrefs isFirstStart = do
    osxApp <- OSX.applicationNew
    uiManager   <-  uiManagerNew
    newIcons
    dataDir       <- getDataDir
    candyPath   <-  getConfigFilePathForLoad
                        (case sourceCandy startupPrefs of
                            Nothing     ->   standardCandyFilename
                            Just name   ->   name ++ leksahCandyFileExtension) Nothing dataDir
    candySt     <-  parseCandy candyPath
    -- keystrokes
    keysPath    <-  getConfigFilePathForLoad (keymapName startupPrefs ++ leksahKeymapFileExtension) Nothing dataDir
    keyMap      <-  parseKeymap keysPath
    let accelActions = setKeymap (keyMap :: KeymapI) mkActions
    specialKeys <-  buildSpecialKeys keyMap accelActions

    win         <-  windowNew
    widgetSetName win "Leksah Main Window"
    let fs = FrameState
            {   windows       =   [win]
            ,   uiManager     =   uiManager
            ,   panes         =   Map.empty
            ,   activePane    =   Nothing
            ,   paneMap       =   Map.empty
            ,   layout        =   (TerminalP Map.empty Nothing (-1) Nothing Nothing)
            ,   panePathFromNB =  Map.empty
            }

    let ide = IDE
          {   frameState        =   fs
          ,   recentPanes       =   []
          ,   specialKeys       =   specialKeys
          ,   specialKey        =   Nothing
          ,   candy             =   candySt
          ,   prefs             =   startupPrefs
          ,   workspace         =   Nothing
          ,   activePack        =   Nothing
          ,   bufferProjCache   =   Map.empty
          ,   allLogRefs        =   []
          ,   currentHist       =   0
          ,   currentEBC        =   (Nothing, Nothing, Nothing)
          ,   systemInfo        =   Nothing
          ,   packageInfo       =   Nothing
          ,   workspaceInfo     =   Nothing
          ,   workspInfoCache   =   Map.empty
          ,   handlers          =   Map.empty
          ,   currentState      =   IsStartingUp
          ,   guiHistory        =   (False,[],-1)
          ,   findbar           =   (False,Nothing)
          ,   toolbar           =   (True,Nothing)
          ,   recentFiles       =   []
          ,   recentWorkspaces  =   []
          ,   runningTool       =   Nothing
          ,   debugState        =   Nothing
          ,   completion        =   ((750,400),Nothing)
          ,   yiControl         =   yiControl
          ,   server            =   Nothing
    }
    ideR             <-  newIORef ide
    menuDescription' <- menuDescription
    reflectIDE (makeMenu uiManager accelActions menuDescription') ideR
    nb               <-  reflectIDE (newNotebook []) ideR
    afterSwitchPage nb (\i -> reflectIDE (handleNotebookSwitch nb i) ideR)
    widgetSetName nb $"root"
    win `onDelete` (\ _ -> do reflectIDE quit ideR; return True)
    reflectIDE (instrumentWindow win startupPrefs (castToWidget nb)) ideR
    reflectIDE (do
        setCandyState (isJust (sourceCandy startupPrefs))
        setBackgroundBuildToggled (backgroundBuild startupPrefs)
        setBackgroundLinkToggled (backgroundLink startupPrefs)) ideR
    let (x,y)   =   defaultSize startupPrefs
    windowSetDefaultSize win x y
    sessionPath <- getConfigFilePathForLoad sessionFilename Nothing dataDir
    (tbv,fbv)   <- reflectIDE (do
        registerEvents
        pair <- recoverSession sessionPath
        wins <- getWindows
        mapM_ instrumentSecWindow (tail wins)
        return pair
        ) ideR
    widgetShowAll win

    reflectIDE (do
        triggerEventIDE UpdateRecent
        if tbv
            then showToolbar
            else hideToolbar
        if fbv
            then showFindbar
            else hideFindbar
        OSX.updateMenu osxApp uiManager) ideR

    OSX.applicationReady osxApp

    when isFirstStart $ do
        welcomePath <- getConfigFilePathForLoad "welcome.txt" Nothing dataDir
        reflectIDE (fileOpenThis welcomePath) ideR
    reflectIDE (initInfo (modifyIDE_ (\ide -> ide{currentState = IsRunning}))) ideR
    timeoutAddFull (do
        reflectIDE (do
            currentPrefs <- readIDE prefs
            when (backgroundBuild currentPrefs) $ backgroundMake) ideR
        return True) priorityDefaultIdle 1000
    reflectIDE (triggerEvent ideR (Sensitivity [(SensitivityInterpreting, False)])) ideR
    mainGUI

fDescription :: FilePath -> FieldDescription Prefs
fDescription configPath = VFD emptyParams [
        mkField
            (paraName <<<- ParaName "Paths under which haskell sources for packages may be found"
                $ paraDirection  <<<- ParaDirection Vertical
                    $ emptyParams)
            sourceDirectories
            (\b a -> a{sourceDirectories = b})
            (filesEditor Nothing FileChooserActionSelectFolder "Select folders")
    ,   mkField
            (paraName <<<- ParaName "Unpack source for cabal packages to" $ emptyParams)
            unpackDirectory
            (\b a -> a{unpackDirectory = b})
            (maybeEditor (stringEditor (\ _ -> True),emptyParams) True "")
    ,   mkField
            (paraName <<<- ParaName "URL from which to download prebuilt metadata" $ emptyParams)
            retrieveURL
            (\b a -> a{retrieveURL = b})
            (stringEditor (\ _ -> True))
    ,   mkField
            (paraName <<<- ParaName "Strategy for downloading prebuilt metadata" $ emptyParams)
            retrieveStrategy
            (\b a -> a{retrieveStrategy = b})
            (enumEditor ["Try to download and then build locally if that fails","Try to build locally and then download if that fails","Never download (just try to build locally)"])]

--
-- | Called when leksah is first called (the .leksah-xx directory does not exist)
--
firstStart :: Prefs -> IO Bool
firstStart prefs = do
    dataDir     <- getDataDir
    prefsPath   <- getConfigFilePathForLoad standardPreferencesFilename Nothing dataDir
    prefs       <- readPrefs prefsPath
    configDir   <- getConfigDir
    dialog      <- dialogNew
    setLeksahIcon dialog
    set dialog [
        windowTitle := "Welcome to Leksah, the Haskell IDE",
        windowWindowPosition := WinPosCenter]
    dialogAddButton dialog "gtk-ok" ResponseOk
    dialogAddButton dialog "gtk-cancel" ResponseCancel
    vb          <- dialogGetUpper dialog
    label       <- labelNew (Just (
        "Before you start using Leksah it will collect and download metadata about your installed Haskell packages.\n" ++
        "You can add folders under which you have sources for Haskell packages not available from Hackage."))
    (widget, setInj, getExt,notifier) <- buildEditor (fDescription configDir) prefs
    boxPackStart vb label PackNatural 7
    sw <- scrolledWindowNew Nothing Nothing
    scrolledWindowAddWithViewport sw widget
    scrolledWindowSetPolicy sw PolicyNever PolicyAutomatic
    boxPackStart vb sw PackGrow 7
    windowSetDefaultSize dialog 800 630
    widgetShowAll dialog
    response <- dialogRun dialog
    widgetHide dialog
    case response of
        ResponseOk -> do
            mbNewPrefs <- extract prefs [getExt]
            widgetDestroy dialog
            case mbNewPrefs of
                Nothing -> do
                    sysMessage Normal "No dialog results"
                    return False
                Just newPrefs -> do
                    fp <- getConfigFilePathForSave standardPreferencesFilename
                    writePrefs fp newPrefs
                    fp2  <-  getConfigFilePathForSave strippedPreferencesFilename
                    SP.writeStrippedPrefs fp2
                            (SP.Prefs {SP.sourceDirectories = sourceDirectories newPrefs,
                                       SP.unpackDirectory   = unpackDirectory newPrefs,
                                       SP.retrieveURL       = retrieveURL newPrefs,
                                       SP.retrieveStrategy  = retrieveStrategy newPrefs,
                                       SP.serverPort        = serverPort newPrefs,
                                       SP.endWithLastConn   = endWithLastConn newPrefs})
                    firstBuild newPrefs
                    return True
        _ -> do
            widgetDestroy dialog
            return False

setLeksahIcon :: (WindowClass self) => self -> IO ()
setLeksahIcon window = do
    dataDir <- getDataDir
    let iconPath = dataDir </> "pics" </> "leksah.png"
    iconExists  <-  doesFileExist iconPath
    when iconExists $
        windowSetIconFromFile window iconPath

firstBuild newPrefs = do
    dialog      <- dialogNew
    setLeksahIcon dialog
    set dialog [
        windowTitle := "Leksah: Updating Metadata",
        windowWindowPosition := WinPosCenter]
    vb          <- dialogGetUpper dialog
    progressBar <- progressBarNew
    progressBarSetText progressBar "Please wait while Leksah collects information about Haskell packages on your system"
    progressBarSetFraction progressBar 0.0
    boxPackStart vb progressBar PackGrow 7
    forkIO $ do
            (output, pid) <- runTool "leksah-server" ["-sbo"] Nothing
            mapM_ (update progressBar) output
            waitForProcess pid
            postGUIAsync (dialogResponse dialog ResponseOk)
    widgetShowAll dialog
    dialogRun dialog
    widgetHide dialog
    widgetDestroy dialog
    return ()
    where
        update pb to = do
                when (isJust prog) $ postGUIAsync (progressBarSetFraction pb (fromJust prog))
            where
            str = toolline to
            prog = case stripPrefix "update_toolbar " str of
                        Just rest -> Just (read rest)
                        Nothing   -> Nothing




