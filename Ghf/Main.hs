module Main where

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
    windowSetIconFromFile win "ghf.gif"
    nb <- notebookNew
    sb <- statusbarNew
    statusbarSetHasResizeGrip sb False
    sblc <- statusbarNew
    statusbarSetHasResizeGrip sblc False
    widgetSetSizeRequest sblc 140 (-1)
    sbio <- statusbarNew
    statusbarSetHasResizeGrip sbio False
    widgetSetSizeRequest sbio 40 (-1)

    let ghf = Ghf win nb [] [sblc,sbio]
    ghfR <- newIORef ghf
    (acc,menus) <- runReaderT (makeMenu actions menuDescription) ghfR
    let mb = fromJust $head menus 
    windowAddAccelGroup win acc
    vb <- vBoxNew False 1  -- Top-level vbox
    hb <- hBoxNew False 1

    boxPackStart hb sb PackGrow 0
    boxPackStart hb sblc PackNatural 0
    boxPackStart hb sbio PackNatural 0

    boxPackStart vb mb PackNatural 0
    boxPackStart vb nb PackGrow 0
    boxPackStart vb hb PackNatural 0


    win `onDelete` (\_ -> do runReaderT quit ghfR; return True)
    win `containerAdd` vb

      -- show the widget and run the main loop
    windowSetDefaultSize win 700 1000
    flip runReaderT ghfR $ case fl of
        [] -> newTextBuffer "Unnamed" Nothing
        otherwise  -> mapM_ (\fn -> (newTextBuffer (takeFileName fn) (Just fn))) fl 
    widgetShowAll win
    mainGUI

quit :: GhfAction
quit = do
    bufs    <- readGhf buffers
    case bufs of
        []          ->  lift mainQuit
        otherwise   ->  do  r <- fileClose
                            if r then quit else return ()

type ActionDescr = (String, String, Maybe String, Maybe String, GhfAction,Maybe String)

actions :: [ActionDescr]
actions =  [("File", "_File", Nothing, Nothing,return (),Nothing)
           ,("FileNew", "_New", Nothing, Just "gtk-new",fileNew,Just "<control>N")
           ,("FileOpen","_Open",Nothing, Just "gtk-open",fileOpen,Just "<control>O")    
           ,("FileSave","_Save",Nothing, Just "gtk-save",fileSave False,Just "<control>S")
           ,("FileSaveAs","Save_As",Nothing, Just "gtk-save_as",fileSave True,Just "") 
           ,("FileClose","_Close",Nothing, Just "gtk-close",do fileClose; return (),Just "<control>W")
           ,("Quit","_Quit",Nothing,Just "gtk-quit",quit,Just "<control>Q")]  

menuDescription :: String
menuDescription = "\n\
 \<ui>\n\
   \<menubar>\n\
     \<menu name=\"_File\" action=\"File\">\n\
       \<menuitem name=\"_New\" action=\"FileNew\" />\n\
       \<menuitem name=\"_Open\" action=\"FileOpen\" />\n\
       \<separator/>\n\
       \<menuitem name=\"_Save\" action=\"FileSave\" />\n\
       \<menuitem name=\"Save_As\" action=\"FileSaveAs\" />\n\
       \<separator/>\n\
       \<menuitem name=\"_Close\" action=\"FileClose\" />\n\
       \<menuitem name=\"_Quit\" action=\"Quit\" />\n\
     \</menu>\n\
   \</menubar>\n\
 \</ui>"

makeMenu :: [ActionDescr] -> String -> GhfM (AccelGroup, [Maybe Widget])
makeMenu actions menuDescription = do
    ghfR <- ask 
    lift $ do
        actionGroupGlobal <- actionGroupNew "global"
        mapM_ (actm ghfR actionGroupGlobal) actions
        uiManager <- uiManagerNew
        uiManagerInsertActionGroup uiManager actionGroupGlobal 1
        uiManagerAddUiFromString uiManager menuDescription 
        accGroup <- uiManagerGetAccelGroup uiManager
        widgets <- mapM (uiManagerGetWidget uiManager) ["ui/menubar"]
        return (accGroup,widgets)
    where
        actm ghfR ag (name,label,tooltip,stockId,ghfAction,acc) = do
            act <- actionNew name label tooltip stockId
            onActionActivate act (runReaderT ghfAction ghfR) 
            actionGroupAddActionWithAccel ag act acc
                    
