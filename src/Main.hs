{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :
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

import Paths_ghf
import Graphics.UI.Gtk
import Control.Monad.Reader
import System.FilePath
import Control.Concurrent
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map
import System.Console.GetOpt
import System.Environment
import System.IO
import GHC
import DynFlags hiding(Option)
import Util (handleDyn)
import Bag
import ErrUtils
import Config
import Data.Version
import Control.Exception
import System.Exit
import Prelude hiding(catch)

import Paths_ghf
import Ghf.SaveSession
import Ghf.Core.State
import Ghf.SourceCandy
import Ghf.File
import Ghf.ViewFrame
import Ghf.Menu
import Ghf.Preferences
import Ghf.Keymap
import Ghf.Info
import Ghf.SourceCollector
import Ghf.Collector

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
                "Show the version number of ghf"
         ,   Option ['d'] ["Debug"] (NoArg DebugF)
                "Write ascii pack files"]

ghfOpts :: [String] -> IO ([Flag], [String])
ghfOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $userError $concat errs ++ usageInfo header options
    where header = "Usage: ghf [OPTION...] files..."


-- ---------------------------------------------------------------------
-- | Main function
--

main = handleTopExceptions $do
    args            <-  getArgs
    (o,fl)          <-  ghfOpts args
    let uninstalled =   filter (\x -> case x of
                                        UninstalledProject _ -> True
                                        otherwise -> False) o
    if elem VersionF o
        then do putStrLn $ "Ghf an IDE for Haskell, version " ++ showVersion version
        else
            if elem Sources o
                then do
                    buildSourceForPackageDB
                    putStrLn "rebuild SourceForPackageDB"
                else if elem Rebuild o
                        || elem Collect o
                        || not (null uninstalled)
                    then do
                        libDir          <-  getSysLibDir
                    --    putStrLn $"libdir '" ++ normalise libDir ++ "'"
#if __GHC__ > 670
                        session     <-  newSession (Just libDir)
#else
                        session     <-  newSession JustTypecheck (Just libDir)
#endif
                        dflags0         <-  getSessionDynFlags session
                        setSessionDynFlags session dflags0
                        let version     =   cProjectVersion
                        let uninstalled =   filter (\x -> case x of UninstalledProject _ -> True
                                                                    otherwise -> False) o
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
    args        <-  getArgs
    (o,fl)      <-  ghfOpts args
    st          <-  unsafeInitGUIForThreadedRTS
    when rtsSupportsBoundThreads
        (putStrLn "Linked with -threaded (Will Gtk work?)")
    timeoutAddFull (yield >> return True) priorityHigh 50
    mapM_ putStrLn st
    uiManager   <-  uiManagerNew
    prefsPath   <-  getConfigFilePathForLoad "Default.prefs"
    prefs       <-  readPrefs prefsPath
    keysPath    <-  getConfigFilePathForLoad $keymapName prefs ++ ".keymap"
    keyMap      <-  parseKeymap keysPath
    let accelActions = setKeymap actions keyMap
    specialKeys <-  buildSpecialKeys keyMap accelActions
    candyPath   <-  getConfigFilePathForLoad
                        (case sourceCandy prefs of
                                    Nothing     ->   "Default.candy"
                                    Just name   ->   name ++ ".candy")
    candySt     <-  parseCandy candyPath
    win         <-  windowNew
    dataDir     <-  getDataDir
    windowSetIconFromFile win $dataDir </> "data" </> "ghf.gif"
    libDir      <-  getSysLibDir
#if __GHC__ > 670
    session     <-  newSession (Just libDir)
#else
    session     <-  newSession JustTypecheck (Just libDir)
#endif
    dflags0     <-  getSessionDynFlags session
    setSessionDynFlags session dflags0
    let ghf = Ghf
          {   window        =   win
          ,   uiManager     =   uiManager
          ,   panes         =   Map.empty
          ,   activePane    =   Nothing
          ,   paneMap       =   Map.empty
          ,   layout        =   (TerminalP Nothing)
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
          ,   session       =   session}
    ghfR <- newIORef ghf
    runReaderT initInfo ghfR
    (acc,menus) <- runReaderT (makeMenu uiManager accelActions menuDescription) ghfR
    let mb = case menus !! 0 of
                Just m -> m
                Nothing -> error "Failed to build menu"
    let tb = case menus !! 1 of
                Just m -> m
                Nothing -> error "Failed to build toolbar"
    windowAddAccelGroup win acc
    nb <- newNotebook
    widgetSetName nb $"root"
    hb <- buildStatusbar ghfR
    vb <- vBoxNew False 1  -- Top-level vbox
    widgetSetName vb "topBox"
    boxPackStart vb mb PackNatural 0
    boxPackStart vb tb PackNatural 0
    boxPackStart vb nb PackGrow 0
    boxPackStart vb hb PackNatural 0
    win `onDelete` (\ _ -> do runReaderT quit ghfR; return True)
    win `onKeyPress` (\ e -> runReaderT (handleSpecialKeystrokes e) ghfR)
    containerAdd win vb
    runReaderT (setCandyState (isJust (sourceCandy prefs))) ghfR
    let (x,y) = defaultSize prefs
    windowSetDefaultSize win x y
    runReaderT recoverSession ghfR
    widgetShowAll win
    mainGUI


-- ---------------------------------------------------------------------
-- Exception handling
--

handleTopExceptions =
  handleNormalExceptions . handleGhfExceptions . handleGhcExceptions

handleNormalExceptions inner =
  catch inner (\exception -> do
    hFlush stdout
    case exception of
      AsyncException StackOverflow -> do
        putStrLn "stack overflow: use -g +RTS -K<size> to increase it"
        exitFailure
      ExitException code -> exitWith code
      _other -> do
        putStrLn ("ghf: internal Ghf error: " ++ show exception)
        exitFailure
  )


handleGhfExceptions inner =
  catchDyn inner (\(e::GhfException) -> do
    putStrLn $ "ghf: " ++ (show e)
    exitFailure
  )


handleGhcExceptions inner =
  -- error messages propagated as exceptions
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
    putStrLn "ghf: Compilation error(s):"
    printBagOfErrors defaultDynFlags (unitBag dyn)
    exitFailure
  )


