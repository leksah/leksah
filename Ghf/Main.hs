module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import Control.Monad.Reader
import Data.IORef
import System.FilePath
import System.Directory
import System.Console.GetOpt
import System.Environment
import Data.Maybe ( fromMaybe, isJust, fromJust )

import Ghf.Core
import Ghf.Editor

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

main = do
    args <- getArgs
    (o,fl) <- ghfOpts args
    initGUI
    win <- windowNew
    nb <- notebookNew
    let ghf = Ghf win nb []
    ghfR <- newIORef ghf
    mb <- runReaderT (makeMenuBar [fileMenu]) ghfR
    vb <- vBoxNew False 1  -- Top-level vbox

    boxPackStart vb mb PackNatural 0
    boxPackStart vb nb PackGrow 0

    win `onDestroy` mainQuit
    win `containerAdd` vb

      -- show the widget and run the main loop
    windowSetDefaultSize win 400 500
    flip runReaderT ghfR $ case fl of
        [] -> newTextBuffer "Unnamed" Nothing
        otherwise  -> mapM_ (\fn -> (newTextBuffer (takeFileName fn) (Just fn))) fl 
    widgetShowAll win
    mainGUI

    

type ItemDesc   =   (Maybe String, Maybe String, GhfAction)
type SubDesc    =   (String, [ItemDesc])
type MenuDesc   =   [SubDesc]

makeMenuBar :: MenuDesc -> GhfM MenuBar
makeMenuBar menuDesc = do
    mb <- lift menuBarNew
    mapM_ (buildSubmenu mb) menuDesc
    return mb
    where
    buildSubmenu :: MenuBar -> SubDesc -> GhfAction
    buildSubmenu mb (title,items) = do
        mu <- lift menuNew
        mapM_ (buildItems mu) items
        lift $ do
            item <- imageMenuItemNewWithMnemonic title
            menuItemSetSubmenu item mu
            menuShellAppend mb item
    buildItems :: Menu -> ItemDesc -> GhfAction
    buildItems menu (mbItemName,mbStock,func) = do
        ghfR <- ask
        lift $ case mbItemName of
                Just itemName -> do
                    item <- case mbStock of
                        Nothing -> imageMenuItemNewWithMnemonic itemName
                        Just stock -> imageMenuItemNewFromStock stock
                    menuShellAppend menu item
                    onActivateLeaf item (runReaderT func ghfR)
                    return ()
                Nothing -> do
                    item <- separatorMenuItemNew
                    menuShellAppend menu item

fileMenu :: SubDesc
fileMenu =  ("_File",[  (Just "_New",Just "gtk-new",fileNew)
                     ,  (Just "_Open",Just "gtk-open",fileOpen)
                     ,  (Nothing,Nothing,return ())
                     ,  (Just "_Save",Just "gtk-save",fileSave False)
                     ,  (Just "Save_As",Just "gtk-save-as",fileSave True)
                     ,  (Nothing,Nothing,return ())
                     ,  (Just "_Close",Just "gtk-close",fileClose)
                     ,  (Just "_Quit",Just "gtk-quit",quit)])

