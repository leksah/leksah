
{-# OPTIONS_GHC -XScopedTypeVariables #-}
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
    runMain
) where

import Graphics.UI.Gtk
import Control.Monad.Reader
import Control.Concurrent
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map
import System.Console.GetOpt
import System.Environment
import GHC
import Config
import Data.Version
import Prelude hiding(catch)
import System.Directory

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
import IDE.Metainfo.SourceCollector
import IDE.Metainfo.InterfaceCollector
import IDE.Find
import Graphics.UI.Editor.Composite (filesEditor, maybeEditor)
import Graphics.UI.Editor.Simple (fileEditor)
--import Outputable (ppr,showSDoc)
import IDE.Metainfo.GHCUtils (inGhcIO)
import IDE.Package (packageBuild)
import IDE.Metainfo.Provider (initInfo)

-- --------------------------------------------------------------------
-- Command line options
--

data Flag =  UninstalledProject String | Collect | Rebuild | Sources | VersionF | DebugF
                | SessionN String | NoGUI | ExtractTars (Maybe String) | Help
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
                "Don't start the leksah GUI"
         ,   Option ['x'] ["Extract"] (OptArg ExtractTars "FILE")
                "Extract tars from cabal install directory"
         ,   Option ['h'] ["Help"] (NoArg Help)
                "Display command line options"]

header = "Usage: ide [OPTION...] files..."

ideOpts :: [String] -> IO ([Flag], [String])
ideOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options


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
    when (elem Help o)
        (sysMessage Normal $ "Leksah an IDE for Haskell, version " ++ usageInfo header options)
    prefsPath       <-  getConfigFilePathForLoad "Default.prefs"
    prefs           <-  readPrefs prefsPath
    let extract     =   filter (\x -> case x of
                                        ExtractTars _ -> True
                                        _             -> False) o
    when (not (null extract)) $ case head extract of
                                ExtractTars (Just path) -> do
                                            dir <- getCurrentDirectory
                                            autoExtractTarFiles path
                                            setCurrentDirectory dir
                                _                       -> do
                                    case autoExtractTars prefs of
                                        Nothing         -> return ()
                                        Just path       -> do
                                            dir <- getCurrentDirectory
                                            autoExtractTarFiles path
                                            setCurrentDirectory dir
    when (elem Sources o) (do
        buildSourceForPackageDB prefs
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
                    collectInstalled' prefs writeAscii version (elem Rebuild o)
    when (not (elem NoGUI o) && not (elem VersionF o) && not (elem Help o))
        (startGUI sessionFilename prefs)

-- ---------------------------------------------------------------------
-- | Start the GUI

startGUI :: String -> Prefs -> IO ()
startGUI sessionFilename iprefs = do
    st          <-  unsafeInitGUIForThreadedRTS
    when rtsSupportsBoundThreads
        (sysMessage Normal "Linked with -threaded")
    timeoutAddFull (yield >> return True) priorityHigh 25
    mapM_ (sysMessage Normal) st
    uiManager   <-  uiManagerNew
    newIcons
    hasConfigDir' <- hasConfigDir
    (startupPrefs,isFirstStart) <-   if hasConfigDir'
                                then return (iprefs,False)
                                else do
                                    firstStart iprefs
                                    prefsPath  <- getConfigFilePathForLoad "Default.prefs"
                                    prefs <- readPrefs prefsPath
                                    return (prefs,True)
    candyPath   <-  getConfigFilePathForLoad
                        (case sourceCandy startupPrefs of
                            Nothing     ->   "Default.candy"
                            Just name   ->   name ++ ".candy")
    candySt     <-  parseCandy candyPath
    -- keystrokes
    keysPath    <-  getConfigFilePathForLoad $ keymapName iprefs ++ ".keymap"
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
          {   frameState    =   fs
          ,   recentPanes   =   []
          ,   specialKeys   =   specialKeys
          ,   specialKey    =   Nothing
          ,   candy         =   candySt
          ,   prefs         =   startupPrefs
          ,   activePack    =   Nothing
          ,   allLogRefs    =   []
          ,   currentEBC    =   (Nothing, Nothing, Nothing)
          ,   accessibleInfo     =   Nothing
          ,   currentInfo   =   Nothing
          ,   handlers      =   Map.empty
          ,   currentState  =   IsStartingUp
          ,   guiHistory    =   (False,[],-1)
          ,   findbar       =   (False,Nothing)
          ,   toolbar       =   (True,Nothing)
          ,   recentFiles     =   []
          ,   recentPackages  =   []
          ,   runningTool     =   Nothing
          ,   ghciState       =   Nothing
    }
    ideR        <-  newIORef ide
    reflectIDE (initInfo :: IDEAction) ideR
    menuDescription' <- menuDescription
    reflectIDE (makeMenu uiManager accelActions menuDescription') ideR
    nb          <-  reflectIDE (newNotebook []) ideR
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
    sessionPath <- getConfigFilePathForLoad sessionFilename
    (tbv,fbv)   <- reflectIDE (do
        registerEvents
        pair <- recoverSession sessionPath
        wins <- getWindows
        mapM_ instrumentSecWindow (tail wins)
        return pair
        ) ideR
    widgetShowAll win
    reflectIDE (do
        ask >>= \ideR -> triggerEvent ideR UpdateRecent
        if tbv
            then showToolbar
            else hideToolbar
        if fbv
            then showFindbar
            else hideFindbar) ideR

    when isFirstStart $ do
        welcomePath <- getConfigFilePathForLoad $ "welcome.txt"
        reflectIDE (fileOpenThis welcomePath) ideR
    reflectIDE (modifyIDE_ (\ide -> return ide{currentState = IsRunning})) ideR

    timeoutAddFull (do
        reflectIDE (do
            currentPrefs <- readIDE prefs
            when (backgroundBuild currentPrefs) $ packageBuild True) ideR
        return True) priorityDefaultIdle 1000
    reflectIDE (triggerEvent ideR (Sensitivity [(SensitivityInterpreting, False)])) ideR
    mainGUI

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
firstStart :: Prefs -> IO ()
firstStart prefs = do
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
            Nothing -> do
                sysMessage Normal "No dialog results"
                return ()
            Just newPrefs -> do
                fp <- getConfigFilePathForSave "Default.prefs"
                writePrefs fp newPrefs
                widgetDestroy dialog
                mainQuit
                firstBuild newPrefs)
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

firstBuild :: Prefs -> IO ()
firstBuild prefs = let version = cProjectVersion in do
    buildSourceForPackageDB prefs
    sources             <-  getSourcesMap prefs
    libDir              <-  getSysLibDir
    runGhc (Just libDir) $ collectInstalled' prefs False version True


