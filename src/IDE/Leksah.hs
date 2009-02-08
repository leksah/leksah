
{-# OPTIONS_GHC -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Leksah
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info at leksah.org>
-- Stability   :  experimental
-- Portability :  portable
--
--
--  Main function of Leksah, an Haskell IDE written in Haskell
--
---------------------------------------------------------------------------------

module IDE.Leksah (
    runMain
) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import qualified Graphics.UI.Gtk.Gdk.Events as GdkEvents
import Control.Monad.Reader
import Control.Concurrent
import Data.IORef
import Data.Maybe
import Data.List(sort)
import qualified Data.Map as Map
import System.Console.GetOpt
import System.Environment
import GHC
import Config
import Data.Version
import Prelude hiding(catch)
import System.FilePath
import System.Directory
import Debug.Trace

import Paths_leksah
import IDE.SaveSession
import IDE.Core.State
import Control.Event
import IDE.SourceCandy
import IDE.FileUtils
import Graphics.UI.Editor.MakeEditor
import Graphics.UI.Editor.Parameters
import IDE.Menu
import IDE.Pane.Preferences
import IDE.Keymap
import IDE.Pane.SourceBuffer
import IDE.SpecialEditors
import IDE.Metainfo.Provider
import IDE.Metainfo.SourceCollector
import IDE.Metainfo.InterfaceCollector
import IDE.Pane.Log
import IDE.Pane.Info
import IDE.Pane.Modules
import IDE.GUIHistory
import IDE.Pane.Search(setChoices,searchMetaGUI)
import IDE.Find
import Graphics.UI.Editor.Composite (maybeEditor)
import Graphics.UI.Editor.Simple (fileEditor)
--import Outputable (ppr,showSDoc)
import IDE.Metainfo.GHCUtils (inGhcIO)
import IDE.NotebookFlipper (flipUp,flipDown)

-- ---------------------------------------------------------------------
-- Command line options
--

data Flag =  UninstalledProject String | Collect | Rebuild | Sources | VersionF | DebugF
                | SessionN String | NoGUI
       deriving (Show,Eq)

options :: [OptDescr Flag]
options =   [Option ['r'] ["Rebuild"] (NoArg Rebuild)
                "Cleans all .pack files and rebuild everything"
         ,   Option ['c'] ["Collect"] (NoArg Collect)
                "Collects new information in .pack files"
         ,   Option ['u'] ["Uninstalled"] (ReqArg UninstalledProject "FILE")
                "Gather info about an uninstalled package"
         ,   Option ['s'] ["Sources"] (NoArg Sources)
                "Gather info about pathes to sources"
         ,   Option ['v'] ["Version"] (NoArg VersionF)
                "Show the version number of ide"
         ,   Option ['d'] ["Debug"] (NoArg DebugF)
                "Write ascii pack files"
         ,   Option ['l'] ["LoadSession"] (ReqArg SessionN "NAME")
                "Load session"
         ,   Option ['n'] ["NoGUI"] (NoArg NoGUI)
                "Don't start the leksah GUI"]


ideOpts :: [String] -> IO ([Flag], [String])
ideOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $userError $concat errs ++ usageInfo header options
    where header = "Usage: ide [OPTION...] files..."


-- ---------------------------------------------------------------------
-- | Main function
--

runMain = handleTopExceptions $ do
    args            <-  getArgs
    (o,_)           <-  ideOpts args
    let uninstalled     =   filter (\x -> case x of
                                        UninstalledProject _ -> True
                                        _                    -> False) o
    let sessions        =   filter (\x -> case x of
                                        SessionN _ -> True
                                        _         -> False) o
    let sessionFilename =  if not (null sessions)
                                then  (head $ map (\ (SessionN x) -> x) sessions) ++ ".session"
                                else  "Current.session"
    when (elem VersionF o)
        (sysMessage Normal $ "Leksah an IDE for Haskell, version " ++ showVersion version)
    when (elem Sources o) (do
        buildSourceForPackageDB
        sysMessage Normal "rebuild SourceForPackageDB")
    when (elem Rebuild o || elem Collect o || not (null uninstalled)) $ do
        inGhcIO $ do
            flags <- getSessionDynFlags
            let version     =   cProjectVersion
            let uninstalled =   filter (\x -> case x of
                                                UninstalledProject _ -> True
                                                _                    -> False) o
            let writeAscii  = elem DebugF o
            if length uninstalled > 0
                then mapM_ (collectUninstalled writeAscii version)
                    $ map (\ (UninstalledProject x) -> x) uninstalled
                else do
                    collectInstalled' writeAscii version (elem Rebuild o)
    when (not (elem NoGUI o) && not (elem VersionF o)) (startGUI sessionFilename)

-- ---------------------------------------------------------------------
-- | Start the GUI

startGUI :: String -> IO ()
startGUI sessionFilename = do
    trace "start gui called" $ return ()
    st          <-  initGUI
    when rtsSupportsBoundThreads
        (sysMessage Normal "Linked with -threaded")
    timeoutAddFull (yield >> return True) priorityHigh 5
    mapM_ (sysMessage Normal) st
    uiManager   <-  uiManagerNew
    newIcons
    hasConfigDir' <- hasConfigDir
    when (not hasConfigDir') firstStart
    prefsPath   <-  getConfigFilePathForLoad "Default.prefs"
    prefs       <-  readPrefs prefsPath
    keysPath    <-  getConfigFilePathForLoad $ keymapName prefs ++ ".keymap"
    keyMap      <-  parseKeymap keysPath
    let accelActions = setKeymap (keyMap :: KeymapI) actions
    specialKeys <-  buildSpecialKeys keyMap accelActions
    candyPath   <-  getConfigFilePathForLoad
                        (case sourceCandy prefs of
                                    Nothing     ->   "Default.candy"
                                    Just name   ->   name ++ ".candy")
    candySt     <-  parseCandy candyPath
    win         <-  windowNew
    dataDir     <-  getDataDir
    let iconPath = dataDir </> "data" </> "leksah.png"
    iconExists  <-  doesFileExist iconPath
    when iconExists $
        windowSetIconFromFile win iconPath
    findBar <- liftIO $ toolbarNew
    toolbarSetStyle findBar ToolbarIcons
    let ide = IDE
          {   window        =   win
          ,   uiManager     =   uiManager
          ,   panes         =   Map.empty
          ,   activePane    =   Nothing
          ,   recentPanes   =   []
          ,   paneMap       =   Map.empty
          ,   layout        =   (TerminalP Nothing (-1))
          ,   specialKeys   =   specialKeys
          ,   specialKey    =   Nothing
          ,   candy         =   candySt
          ,   prefs         =   prefs
          ,   activePack    =   Nothing
          ,   errors        =   []
          ,   currentErr    =   Nothing
          ,   accessibleInfo     =   Nothing
          ,   currentInfo   =   Nothing
          ,   handlers      =   Map.empty
          ,   currentState  =   IsStartingUp
          ,   guiHistory    =   (False,[],-1)
          ,   findbar       =   findBar
          ,   toolbar       =   Nothing
          ,   findbarVisible  = True
          ,   toolbarVisible  = True
    }
    ideR        <-  newIORef ide
    menuDescription' <- menuDescription
    (acc,menus) <-  reflectIDE (makeMenu uiManager accelActions menuDescription') ideR
    when (length menus /= 2) $ throwIDE ("Failed to build menu" ++ show (length menus))
    let toolbar = castToToolbar (menus !! 1)
    reflectIDE (do
        constructFindReplace findBar
        modifyIDE_ (\ide -> return ide{toolbar = Just toolbar})
        initInfo :: IDEAction) ideR
    sep0 <- separatorToolItemNew
    separatorToolItemSetDraw sep0 False
    toolItemSetExpand sep0 True
    toolbarInsert toolbar sep0 (-1)
    closeButton <- toolButtonNewFromStock "gtk-close"
    toolbarInsert toolbar closeButton (-1)
    closeButton `onToolButtonClicked` do
        reflectIDE hideToolbar ideR

    windowAddAccelGroup win acc
    nb          <-  newNotebook
    afterSwitchPage nb (\i -> reflectIDE (handleNotebookSwitch nb i) ideR)
    widgetSetName nb $"root"
    statusBar   <-  buildStatusbar ideR
    vb          <-  vBoxNew False 1  -- Top-level vbox
    widgetSetName vb "topBox"
    toolbarSetStyle (castToToolbar (menus !! 1)) ToolbarIcons
    boxPackStart vb (menus !! 0) PackNatural 0
    boxPackStart vb (menus !! 1) PackNatural 0
    boxPackStart vb nb PackGrow 0
    boxPackStart vb findBar PackNatural 0
    boxPackEnd vb statusBar PackNatural 0
    win `onDelete` (\ _ -> do reflectIDE quit ideR; return True)
    win `onKeyPress` (\ e -> reflectIDE (handleSpecialKeystrokes e) ideR)
    containerAdd win vb
    reflectIDE (setCandyState (isJust (sourceCandy prefs))) ideR
    let (x,y)   =   defaultSize prefs
    windowSetDefaultSize win x y
    sessionPath <- getConfigFilePathForLoad sessionFilename
    (tbv,fbv) <- reflectIDE (do
        registerEvents menus
        recoverSession sessionPath
        ) ideR
    reflectIDE (do
        if tbv
            then showToolbar
            else hideToolbar
        if fbv
            then showFindbar
            else hideFindbar) ideR
    widgetShowAll win
-- patch for windows
    buffers <- reflectIDE allBuffers ideR
    fdesc <- fontDescriptionFromString (case textviewFont prefs of Just str -> str; Nothing -> "")
    fds <- fontDescriptionGetSize fdesc
    when (isJust fds) $ do
        fontDescriptionSetSize fdesc (fromJust fds + 0.01)
        mapM_ (\buf -> widgetModifyFont (castToWidget $sourceView buf) (Just fdesc)) buffers
-- end patch
        reflectIDE (modifyIDE_ (\ide -> return ide{currentState = IsRunning})) ideR
        mainGUI

--
-- | Callback function for onKeyPress of the main window, so 'preprocess' any key
--
handleSpecialKeystrokes :: GdkEvents.Event -> IDEM Bool
handleSpecialKeystrokes (Key { eventKeyName = name,  eventModifier = mods,
                                eventKeyVal = keyVal, eventKeyChar = mbChar}) = do
    sb <- getSBSpecialKeys
    prefs' <- readIDE prefs
    case (name, mods) of
		(tab, [Control]) | (tab == "Tab" || tab == "ISO_Left_Tab")
		                        && useCtrlTabFlipping prefs'      -> do
		    flipDown
		    return True
		(tab, [Shift, Control]) | (tab == "Tab" || tab == "ISO_Left_Tab")
		                        && useCtrlTabFlipping prefs'      -> do
		    flipUp
		    return True
		_                                                            -> do
                bs <- getCandyState
                when bs (editKeystrokeCandy mbChar)
                sk  <- readIDE specialKey
                sks <- readIDE specialKeys
                return True
                case sk of
                    Nothing ->
                        case Map.lookup (keyVal,sort mods) sks of
                            Nothing -> do
                                liftIO $statusbarPop sb 1
                                liftIO $statusbarPush sb 1 ""
                                return False
                            Just map -> do
                                let sym = printMods mods ++ name
                                liftIO $statusbarPop sb 1
                                liftIO $statusbarPush sb 1 sym
                                modifyIDE_ (\ide -> return (ide{specialKey = Just (map,sym)}))
                                return True
                    Just (map,sym) -> do
                        case Map.lookup (keyVal,sort mods) map of
                            Nothing -> do
                                liftIO $statusbarPop sb 1
                                liftIO $statusbarPush sb 1 $ sym ++ printMods mods ++ name ++ "?"
                                return ()
                            Just (AD actname _ _ _ ideAction _ _) -> do
                                liftIO $statusbarPop sb 1
                                liftIO $statusbarPush sb 1
                                    $ sym ++ " " ++ printMods mods ++ name ++ "=" ++ actname
                                ideAction
                        modifyIDE_ (\ide -> return (ide{specialKey = Nothing}))
                        return True
    where
    printMods :: [Modifier] -> String
    printMods []    = ""
    printMods (m:r) = show m ++ printMods r
handleSpecialKeystrokes _ = return True

--
-- | Register handlers for IDE events
--
registerEvents :: [Widget] -> IDEAction
registerEvents tbl =    do
    stRef   <-  ask
    registerEvent stRef "LogMessage" (Left logHandler)
    registerEvent stRef "SelectInfo" (Left siHandler)
    registerEvent stRef "SelectIdent" (Left sidHandler)
    registerEvent stRef "CurrentInfo" (Left ciuHandler)
    registerEvent stRef "ActivePack" (Left apHandler)
    registerEvent stRef "RecordHistory" (Left rhHandler)
    registerEvent stRef "Sensitivity" (Left sHandler)
    registerEvent stRef "DescrChoice" (Left dcHandler)
    registerEvent stRef "SearchMeta" (Left smHandler)
    registerEvent stRef "LoadSession" (Left lsHandler)
    registerEvent stRef "SaveSession" (Left ssHandler)


    return ()
    where
        logHandler e@(LogMessage s t) =   do
            (log :: IDELog)          <-  getLog
            liftIO $ appendLog log s t
            return e
        logHandler _ =   throwIDE "Leksah>>registerEvents: Impossible event"

        siHandler e@(SelectInfo str) =   do
            setSymbol str
            return e
        siHandler _ =   throwIDE "Leksah>>registerEvents: Impossible event"

        sidHandler e@(SelectIdent id) =   do
            selectIdentifier id
            return e
        sidHandler _ =   throwIDE "Leksah>>registerEvents: Impossible event"

        ciuHandler CurrentInfo =   do
            reloadKeepSelection
            return CurrentInfo
        ciuHandler _ =   throwIDE "Leksah>>registerEvents: Impossible event"

        apHandler ActivePack =   do
            infoForActivePackage :: IDEAction
            return ActivePack
        apHandler _ =   throwIDE "Leksah>>registerEvents: Impossible event"

        rhHandler rh@(RecordHistory h) =   do
            recordHistory h
            return rh
        rhHandler _ =   throwIDE "Leksah>>registerEvents: Impossible event"

        sHandler s@(Sensitivity h) =   do
            setSensitivity h
            return s
        sHandler _ =   throwIDE "Leksah>>registerEvents: Impossible event"

        dcHandler e@(DescrChoice descrs) =   do
            setChoices descrs
            return e
        dcHandler _ =   throwIDE "Leksah>>registerEvents: Impossible event"

        smHandler e@(SearchMeta string) =  searchMetaGUI string >> return e
        smHandler _ =   throwIDE "Leksah>>registerEvents: Impossible event"

        lsHandler e@(LoadSession fp) =  loadSession fp >> return e
        lsHandler _ =   throwIDE "Leksah>>registerEvents: Impossible event"

        ssHandler e@(SaveSession fp) =  saveSessionAs fp >> return e
        ssHandler _ =   throwIDE "Leksah>>registerEvents: Impossible event"

fDescription :: FieldDescription Prefs
fDescription = VFD emptyParams [
        mkField
            (paraName <<<- ParaName "Paths under which haskell sources may be found"
                $ paraDirection  <<<- ParaDirection Vertical
                    $ emptyParams)
            sourceDirectories
            (\b a -> a{sourceDirectories = b})
            (filesEditor Nothing FileChooserActionSelectFolder "Select folders")
    ,   mkField
            (paraName <<<- ParaName "Extract packages from cabal-install" $ emptyParams)
            autoExtractTars
            (\b a -> a{autoExtractTars = b})
            (maybeEditor ((fileEditor (Just "~/.cabal/packages/") FileChooserActionSelectFolder
                "Select folder"), emptyParams) True "Yes")]

--
-- | Called when leksah ist first called (the .leksah directory does not exist)
--
firstStart :: IO ()
firstStart = do
    prefsPath   <-  getConfigFilePathForLoad "Default.prefs"
    prefs       <-  readPrefs prefsPath
    dialog      <- windowNew
    vb          <- vBoxNew False 0
    bb          <- hButtonBoxNew
    ok          <- buttonNewFromStock "gtk-ok"
    cancel      <- buttonNewFromStock "gtk-cancel"
    boxPackStart bb ok PackNatural 0
    boxPackStart bb cancel PackNatural 0
    label       <- labelNew (Just ("Welcome to Leksah, an IDE for Haskell.\n" ++
        "At the first start, Leksah will collect metadata about your installed haskell packages.\n" ++
        "Select folders under which you have installed Haskell packages with sources below and click add.\n" ++
        "It may take some time before Leksah starts up."))
    (widget, setInj, getExt,notifier)
                <- buildEditor fDescription prefs
    ok `onClicked` (do
        mbNewPrefs <- extract prefs [getExt]
        case mbNewPrefs of
            Nothing -> return ()
            Just newPrefs -> do
                fp <- getConfigFilePathForSave "Default.prefs"
                writePrefs fp newPrefs
                widgetDestroy dialog
                mainQuit
                firstBuild)
    cancel `onClicked` (do
        widgetDestroy dialog
        mainQuit)
    dialog `onDelete` (\_ -> do
        widgetDestroy dialog
        mainQuit
        return True)
    boxPackStart vb label PackGrow 7
    boxPackStart vb widget PackGrow 7
    boxPackEnd vb bb PackNatural 7
    containerAdd dialog vb
    widgetSetSizeRequest dialog 700 400
    widgetShowAll dialog
    mainGUI
    return ()

firstBuild :: IO ()
firstBuild = let version = cProjectVersion in do
    buildSourceForPackageDB
    sources             <-  getSourcesMap
    libDir          <-  getSysLibDir
    runGhc (Just libDir) $ collectInstalled' False version True


