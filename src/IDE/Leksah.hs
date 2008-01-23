{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Leksah
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
--
--  Main function of Genuine Haskell Face, an Haskell IDE written in Haskell
--
---------------------------------------------------------------------------------

module IDE.Leksah (
    runMain
) where

import Graphics.UI.Gtk
import Control.Monad.Reader
import System.FilePath
import System.Directory(doesFileExist)
import Control.Concurrent
import Data.IORef
import Data.Maybe
import Data.List(sort)
import qualified Data.Map as Map
import System.Console.GetOpt
import System.Environment
import System.IO
import GHC
import DynFlags hiding(Option)
import Bag
import ErrUtils
import Config
import Data.Version
import Control.Exception
import System.Exit
import Prelude hiding(catch)

import Paths_leksah
import IDE.SaveSession
import IDE.Core.State
import IDE.SourceCandy
import IDE.Utils.File
import IDE.Framework.ViewFrame
import IDE.Menu
import IDE.Preferences
import IDE.Keymap
import IDE.SourceEditor
import IDE.Metainfo.Info
import IDE.Metainfo.SourceCollector
import IDE.Metainfo.InterfaceCollector
import IDE.Log(getLog,appendLog)

-- ---------------------------------------------------------------------
-- Command line options
--

data Flag =  UninstalledProject String | Collect | Rebuild | Sources | VersionF | DebugF
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
                "Write ascii pack files"]

ideOpts :: [String] -> IO ([Flag], [String])
ideOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $userError $concat errs ++ usageInfo header options
    where header = "Usage: ide [OPTION...] files..."


-- ---------------------------------------------------------------------
-- | Main function
--

runMain = handleTopExceptions $do
    args            <-  getArgs
    (o,_)          <-  ideOpts args
    let uninstalled =   filter (\x -> case x of
                                        UninstalledProject _ -> True
                                        _                    -> False) o
    if elem VersionF o
        then do sysMessage Normal $ "Leksah an IDE for Haskell, version " ++ showVersion version
        else
            if elem Sources o
                then do
                    buildSourceForPackageDB
                    sysMessage Normal "rebuild SourceForPackageDB"
                else if elem Rebuild o
                        || elem Collect o
                        || not (null uninstalled)
                    then do
                        libDir          <-  getSysLibDir
#if __GHC__ > 670
                        session     <-  newSession (Just libDir)
#else
                        session     <-  newSession JustTypecheck (Just libDir)
#endif
                        dflags0         <-  getSessionDynFlags session
                        setSessionDynFlags session dflags0
                        let version     =   cProjectVersion
                        let uninstalled =   filter (\x -> case x of UninstalledProject _ -> True
                                                                    _                    -> False) o
                        let writeAscii = elem DebugF o
                        if length uninstalled > 0
                            then mapM_ (collectUninstalled writeAscii session version)
                                    $ map (\ (UninstalledProject x) -> x) uninstalled
                            else collectInstalled writeAscii session version (elem Rebuild o)
                    else startGUI

-- ---------------------------------------------------------------------
-- | Start the GUI

startGUI :: IO ()
startGUI = do
    sysMessage Normal "Now starting GUI"
    st          <-  initGUI
    when rtsSupportsBoundThreads
        (sysMessage Normal "Linked with -threaded")
    timeoutAddFull (yield >> return True) priorityHigh 25
    mapM_ (sysMessage Normal) st
    uiManager   <-  uiManagerNew
    prefsPath   <-  getConfigFilePathForLoad "Default.prefs"
    prefs       <-  readPrefs prefsPath
    keysPath    <-  getConfigFilePathForLoad $keymapName prefs ++ ".keymap"
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
    let iconPath = dataDir </> "data" </> "leksah.gif"
    iconExists  <-  doesFileExist iconPath
    when iconExists $
        windowSetIconFromFile win iconPath
    libDir      <-  getSysLibDir
#if __GHC__ > 670
    session     <-  newSession (Just libDir)
#else
    session     <-  newSession JustTypecheck (Just libDir)
#endif
    dflags0     <-  getSessionDynFlags session
    setSessionDynFlags session dflags0
    let ide = IDE
          {   window        =   win
          ,   uiManager     =   uiManager
          ,   panes         =   Map.empty
          ,   activePane    =   Nothing
          ,   paneMap       =   Map.empty
          ,   layout        =   (TerminalP Nothing (-1))
          ,   specialKeys   =   specialKeys
          ,   specialKey    =   Nothing
          ,   candy         =   candySt
          ,   prefs         =   prefs
--          ,   packages      =   []
          ,   activePack    =   Nothing
          ,   errors        =   []
          ,   currentErr    =   Nothing
          ,   accessibleInfo     =   Nothing
          ,   currentInfo   =   Nothing
          ,   session       =   session
          ,   handlers      =   Map.empty}
    ideR        <-  newIORef ide
    runReaderT (initInfo :: IDEAction) ideR
    (acc,menus) <-  runReaderT (makeMenu uiManager accelActions menuDescription) ideR
    let mb      =   case menus !! 0 of
                        Just m  ->  m
                        Nothing ->  throwIDE "Failed to build menu"
    windowAddAccelGroup win acc
    nb          <-  newNotebook
    widgetSetName nb $"root"
    statusBar   <-  buildStatusbar ideR
    vb          <-  vBoxNew False 1  -- Top-level vbox
    widgetSetName vb "topBox"
    boxPackStart vb mb PackNatural 0
    boxPackStart vb nb PackGrow 0
    boxPackEnd vb statusBar PackNatural 0
    win `onDelete` (\ _ -> do runReaderT quit ideR; return True)
    win `onKeyPress` (\ e -> runReaderT (handleSpecialKeystrokes e) ideR)
    containerAdd win vb
    runReaderT (setCandyState (isJust (sourceCandy prefs))) ideR
    let (x,y)   =   defaultSize prefs
    windowSetDefaultSize win x y
    runReaderT (do
        registerEvents (menus !! 1)
        recoverSession :: IDEAction) ideR
    widgetShowAll win
    mainGUI

--
-- | Callback function for onKeyPress of the main window, so preprocess any key
--
handleSpecialKeystrokes :: Event -> IDEM Bool
handleSpecialKeystrokes (Key _ _ _ mods _ _ _ keyVal name mbChar) = do
    bs <- getCandyState
    when bs $ editKeystrokeCandy mbChar
    sk  <- readIDE specialKey
    sks <- readIDE specialKeys
    sb <- getSBSpecialKeys
    case sk of
        Nothing -> do
            case Map.lookup (keyVal,sort mods) sks of
                Nothing -> do
                    lift $statusbarPop sb 1
                    lift $statusbarPush sb 1 ""
                    return False
                Just map -> do
                    sb <- getSBSpecialKeys
                    let sym = printMods mods ++ name
                    lift $statusbarPop sb 1
                    lift $statusbarPush sb 1 sym
                    modifyIDE_ (\ide -> return (ide{specialKey = Just (map,sym)}))
                    return True
        Just (map,sym) -> do
            case Map.lookup (keyVal,sort mods) map of
                Nothing -> do
                    sb <- getSBSpecialKeys
                    lift $statusbarPop sb 1
                    lift $statusbarPush sb 1 $sym ++ printMods mods ++ name ++ "?"
                    return ()
                Just (AD actname _ _ _ ideAction _ _) -> do
                    sb <- getSBSpecialKeys
                    lift $statusbarPop sb 1
                    lift $statusbarPush sb 1
                        $sym ++ " " ++ printMods mods ++ name ++ "=" ++ actname
                    ideAction
            modifyIDE_ (\ide -> return (ide{specialKey = Nothing}))
            return True
    where
    printMods :: [Modifier] -> String
    printMods []    = ""
    printMods (m:r) = show m ++ printMods r
handleSpecialKeystrokes _ = return True

registerEvents :: Maybe Widget -> IDEAction
registerEvents mbTb =    do
    stRef   <-  ask
    st      <-  lift $ readIORef stRef
    registerEvent st LogMessageS (Left logHandler)
    registerEvent st GetToolbarS (Left tbHandler)
    return ()
    where
        logHandler e@(LogMessage s t) =   do
            (log :: IDELog)          <-  getLog
            lift $ appendLog log s t
            return e
        logHandler _ =   throwIDE "Leksah>>registerEvents: Impossible event"

        tbHandler (GetToolbar Nothing) =   return (GetToolbar mbTb)
        tbHandler _ =   throwIDE "Leksah>>registerEvents: Impossible event"


-- ---------------------------------------------------------------------
-- Exception handling
--

handleTopExceptions =
  handleNormalExceptions . handleIDEExceptions . handleGhcExceptions

handleNormalExceptions inner =
  catch inner (\exception -> do
    hFlush stdout
    case exception of
      AsyncException StackOverflow -> do
        sysMessage Normal "stack overflow: use -g +RTS -K<size> to increase it"
        exitFailure
      ExitException code -> exitWith code
      _other -> do
        sysMessage Normal ("ide: internal IDE error: " ++ show exception)
        exitFailure
  )


handleIDEExceptions inner =
  catchDyn inner (\(e::IDEException) -> do
    sysMessage Normal $ "ide: " ++ (show e)
    exitFailure
  )


handleGhcExceptions inner =
  -- throwIDE messages propagated as exceptions
  let inner2 = catchDyn inner (\dyn -> do
        hFlush stdout
        case dyn of
          PhaseFailed _ code -> exitWith code
          Interrupted -> exitFailure
          _ -> do
            print (dyn :: GhcException)
            exitFailure)
  in
  -- compilation errors: messages with locations attached
  catchDyn inner2 (\dyn -> do
    sysMessage Normal "ide: Compilation error(s):"
    printBagOfErrors defaultDynFlags (unitBag dyn)
    exitFailure
  )


