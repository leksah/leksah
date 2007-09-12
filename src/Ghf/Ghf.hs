--
-- Copyright (c) 2007 Jürgen Nicklisch - Jutaro
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Types

import Control.Monad.Reader
import Data.IORef
import System.FilePath
import System.Directory
import System.Console.GetOpt
import System.Environment
import Data.Maybe ( fromMaybe, isJust)
import qualified Data.Map as Map
import Data.Map(Map)
import System.IO
import Control.Concurrent

import Ghf.Core
import Ghf.Editor.SourceEditor
import Ghf.GUI.ViewFrame
import Ghf.GUI.Keymap
import Ghf.GUI.SourceCandy
import Ghf.Editor.PreferencesEditor
import Ghf.GUI.Menu
import Ghf.GUI.Statusbar
import Ghf.GUI.Log

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
    args <- getArgs
    (o,fl) <- ghfOpts args
    st <- initGUI
    if rtsSupportsBoundThreads
        then error "Don't link with -theaded, Gtk won't work"
        else timeoutAddFull (yield >> return True) priorityHigh 50
    mapM_ putStrLn st

    prefs <- readPrefs "config/Default.prefs"
    keyMap <- parseKeymap "config/Default.keymap"
    let accelActions = setKeymap actions keyMap
    specialKeys <- buildSpecialKeys keyMap accelActions
    candySt     <- parseCandy (case sourceCandy prefs of
                                    Nothing   -> "config/Default.candy"
                                    Just name -> "config/" ++ name ++ ".candy")
    win <- windowNew
    windowSetIconFromFile win "bin/ghf.gif"
    uiManager <- uiManagerNew
    let ghf = Ghf
          {   window        = win
          ,   uiManager     = uiManager
          ,   panes         = Map.empty
          ,   activePane    = Nothing
          ,   paneMap       = Map.empty
          ,   layout        = TerminalP
          ,   specialKeys   = specialKeys
          ,   specialKey    = Nothing
          ,   candy         = candySt
          ,   prefs         = prefs
          ,   packages      = []
          ,   activePack    = Nothing}
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
    flip runReaderT ghfR $ case fl of
        [] -> newTextBuffer "Unnamed" Nothing
        otherwise  -> mapM_ (\ fn -> (newTextBuffer (takeFileName fn) (Just fn))) fl
    runReaderT initLog ghfR
    widgetShowAll win
    hbf <- runReaderT getFindBar ghfR
    widgetHide hbf
    spinL <- runReaderT getGotoLineSpin ghfR
    widgetHide spinL
    mainGUI
