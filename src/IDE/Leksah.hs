
{-# OPTIONS_GHC -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
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

#if defined(darwin_HOST_OS)
import Graphics.UI.Gtk.OSX (applicationNew, applicationReady)
import IDE.OSX
#endif

#ifdef YI
import qualified Yi.UI.Pango.Control as Yi
import IDE.YiConfig
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
       (enumEditor, intEditor, stringEditor, boolEditor)
import IDE.Metainfo.Provider (initInfo)
import IDE.Workspaces (backgroundMake)
import IDE.Utils.GUIUtils
import Network (withSocketsDo)
import Control.Exception
import System.Exit(exitFailure)
import qualified IDE.StrippedPrefs as SP
import IDE.Utils.Tool (runTool,toolline)
import IDE.System.Process(waitForProcess)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#else
import qualified System.Posix as P
#endif
import System.Log
import System.Log.Logger(updateGlobalLogger,rootLoggerName,setLevel)
import Data.List (stripPrefix)

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

main = withSocketsDo $ handleExceptions $ do
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#else
    P.getProcessID >>= P.createProcessGroup
#endif
    args            <-  getArgs

    (o,_)           <-  ideOpts args
    isFirstStart    <-  liftM not hasConfigDir
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
        (startGUI sessionFilename prefs isFirstStart)

handleExceptions inner =
  catch inner (\(exception :: SomeException) -> do
    sysMessage Normal ("leksah: internal IDE error: " ++ show exception)
    exitFailure
  )

-- ---------------------------------------------------------------------
-- | Start the GUI

startGUI :: String -> Prefs -> Bool -> IO ()
startGUI sessionFilename iprefs isFirstStart = do
#ifdef YI
  Yi.startControl yiVimConfig $ do
   yiControl <- Yi.getControl
   Yi.controlIO $ do
#endif
    st          <-  unsafeInitGUIForThreadedRTS
#if defined(darwin_HOST_OS)
    app <- applicationNew
#endif
    when rtsSupportsBoundThreads
        (sysMessage Normal "Linked with -threaded")
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100 -- maybe switch back to priorityHigh/???
    mapM_ (sysMessage Normal) st
    initGtkRc
    uiManager   <-  uiManagerNew
    newIcons
    dataDir       <- getDataDir
    startupPrefs  <-   if not isFirstStart
                                then return iprefs
                                else do
                                    firstStart iprefs
                                    prefsPath  <- getConfigFilePathForLoad standardPreferencesFilename Nothing dataDir
                                    prefs <- readPrefs prefsPath
                                    return prefs
    candyPath   <-  getConfigFilePathForLoad
                        (case sourceCandy startupPrefs of
                            Nothing     ->   standardCandyFilename
                            Just name   ->   name ++ leksahCandyFileExtension) Nothing dataDir
    candySt     <-  parseCandy candyPath
    -- keystrokes
    keysPath    <-  getConfigFilePathForLoad (keymapName iprefs ++ leksahKeymapFileExtension) Nothing dataDir
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
          ,   ghciState         =   Nothing
          ,   completion        =   Nothing
#ifdef YI
          ,   yiControl         =   yiControl
#endif
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

#if defined(darwin_HOST_OS)
    updateMenu app uiManager
    applicationReady app
#endif

    reflectIDE (do
        triggerEventIDE UpdateRecent
        if tbv
            then showToolbar
            else hideToolbar
        if fbv
            then showFindbar
            else hideFindbar) ideR

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
            (paraName <<<- ParaName "Maybe a directory for unpacking cabal packages" $ emptyParams)
            unpackDirectory
            (\b a -> a{unpackDirectory = b})
            (maybeEditor (stringEditor (\ _ -> True),emptyParams) True "")
    ,   mkField
            (paraName <<<- ParaName "An URL to load prebuild metadata" $ emptyParams)
            retrieveURL
            (\b a -> a{retrieveURL = b})
            (stringEditor (\ _ -> True))
    ,   mkField
            (paraName <<<- ParaName "A strategy for downloading prebuild metadata" $ emptyParams)
            retrieveStrategy
            (\b a -> a{retrieveStrategy = b})
            (enumEditor ["Retrieve then build","Build then retrieve","Never retrieve"])
    ,   mkField
            (paraName <<<- ParaName "Port number for server connection" $ emptyParams)
            serverPort
            (\b a -> a{serverPort = b})
            (intEditor (1.0, 65535.0, 1.0))
    ,   mkField
            (paraName <<<- ParaName "End the server with last connection" $ emptyParams)
            endWithLastConn
            (\b a -> a{endWithLastConn = b})
            boolEditor]

--
-- | Called when leksah ist first called (the .leksah-xx directory does not exist)
--
firstStart :: Prefs -> IO ()
firstStart prefs = do
    dataDir     <- getDataDir
    prefsPath   <- getConfigFilePathForLoad standardPreferencesFilename Nothing dataDir
    prefs       <- readPrefs prefsPath
    configDir   <- getConfigDir
    dialog      <- dialogNew
    dialogAddButton dialog "gtk-ok" ResponseOk
    dialogAddButton dialog "gtk-cancel" ResponseCancel
    vb          <- dialogGetUpper dialog
    label       <- labelNew (Just ("Welcome to Leksah, the Haskell IDE.\n" ++
        "At the first start, Leksah will collect and download metadata about your installed haskell packages.\n" ++
        "You can add folders under which you have sources for Haskell packages not available from Hackage.\n" ++
        "If you are not shure what to do, just keep the defaults \n" ++
        "This process may take a long time, so be patient."))
    (widget, setInj, getExt,notifier) <- buildEditor (fDescription configDir) prefs
    boxPackStart vb label PackGrow 7
    boxPackStart vb widget PackGrow 7
    widgetSetSizeRequest dialog 800 640
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
                    return ()
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
        _ ->     widgetDestroy dialog

firstBuild newPrefs = do
    dialog      <- dialogNew
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




