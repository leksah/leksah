--
-- Copyright (c) 2007 Jürgen Nicklisch - 
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
import Data.Maybe ( fromMaybe, isJust, fromJust )
import qualified Data.Map as Map
import Data.Map(Map)

import Ghf.Core
import Ghf.Editor
import Ghf.Dialogs
import Ghf.View
import Ghf.Statusbar
import Ghf.Menu

version = "0.1"

data Flag =  OpenFile
       deriving Show

options :: [OptDescr Flag]
options = [ ]

ghfOpts :: [String] -> IO ([Flag], [String])
ghfOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ghf [OPTION...] files..."


-- |Build the main window
main = do
    args <- getArgs
    (o,fl) <- ghfOpts args
    st <- initGUI
    mapM_ putStrLn st
    
    win <- windowNew
    windowSetIconFromFile win "ghf.gif"
    uiManager <- uiManagerNew
    let ghf = Ghf
                  {   window = win
                  ,   uiManager = uiManager
                  ,   buffers = Map.empty
                  ,   mbActiveBuf = Nothing
                  ,   paneMap = Map.empty}
    ghfR <- newIORef ghf
    (acc,menus) <- runReaderT (makeMenu uiManager actions menuDescription) ghfR
    let mb = fromJust $menus !! 0
    let tb = fromJust $menus !! 1
    windowAddAccelGroup win acc

    nb <- notebookNew
    widgetSetName nb $"notebookTop"
    hb <- buildStatusbar
    vb <- vBoxNew False 1  -- Top-level vbox
    widgetSetName vb "topBox"
    boxPackStart vb mb PackNatural 0
    boxPackStart vb tb PackNatural 0
    boxPackStart vb hpane PackGrow 0
    boxPackStart vb hb PackNatural 0

    win `onDelete` (\_ -> do runReaderT quit ghfR; return True)
    containerAdd win vb

    windowSetDefaultSize win 700 1000
    flip runReaderT ghfR $ case fl of
        [] -> newTextBuffer "Unnamed" Nothing
        otherwise  -> mapM_ (\fn -> (newTextBuffer (takeFileName fn) (Just fn))) fl
    widgetShowAll win
    mainGUI
    widgetHide hbf
    widgetHide spinL
     
quit :: GhfAction
quit = do
    bufs    <- readGhf buffers
    if Map.null bufs 
        then    lift mainQuit
        else    do  r <- fileClose
                    if r then quit else return ()


                    
aboutDialog :: GhfAction
aboutDialog = lift $ do
    d <- aboutDialogNew
    aboutDialogSetName d "Genuine Haskell Face"
    aboutDialogSetVersion d version
    aboutDialogSetCopyright d "Copyright 2007 Juergen Nicklisch-Franken aka Jutaro"
    aboutDialogSetComments d $ "An integrated development environement (IDE) for the " ++
                               "programming language haskell and the Glasgow Haskell compiler"
    license <- readFile "gpl.txt"     
    aboutDialogSetLicense d $ Just license
    aboutDialogSetWebsite d "www.haskell.org/ghf"
    aboutDialogSetAuthors d ["Juergen Nicklisch-Franken aka Jutaro"]
    dialogRun d
    widgetDestroy d
    return ()
 