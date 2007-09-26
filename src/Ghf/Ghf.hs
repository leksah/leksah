-----------------------------------------------------------------------------
--
-- Module      :  Ghf
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
--
-- | Main module of Genuine Haskell Face, an Haskell IDE written in Haskell
--
---------------------------------------------------------------------------------

import Paths_ghf
import Graphics.UI.Gtk
import Control.Monad.Reader
import System.FilePath
import Control.Concurrent
import Data.IORef
import Data.Maybe
import Data.Map(Map)
import qualified Data.Map as Map
import System.Console.GetOpt
import System.Environment
import System.IO

import Ghf.SaveSession
import Ghf.Core
import Ghf.SourceCandy
import Ghf.File
import Ghf.ViewFrame
import Ghf.Menu
import Ghf.PreferencesEditor
import Ghf.Keymap

data Flag =  OpenFile
       deriving Show

options :: [OptDescr Flag]
options = [ ]

ghfOpts :: [String] -> IO ([Flag], [String])
ghfOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $userError $concat errs ++ usageInfo header options
    where header = "Usage: ghf [OPTION...] files..."

-- |Build the main window
main = do
    args        <-  getArgs
    (o,fl)      <-  ghfOpts args
    st          <-  initGUI
--    if rtsSupportsBoundThreads
--        then error "Don't link with -theaded, Gtk won't work"
--        else
    timeoutAddFull (yield >> return True) priorityHigh 50
    mapM_ putStrLn st
    uiManager   <-  uiManagerNew
    prefsPath   <-  getConfigFilePathForLoad "Default.prefs"
    prefs       <-  readPrefs prefsPath
    keysPath    <-  getConfigFilePathForLoad "Default.keymap"
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
          ,   packages      =   []
          ,   activePack    =   Nothing
          ,   errors        =   []
          ,   currentErr    =   Nothing}
    ghfR <- newIORef ghf
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


