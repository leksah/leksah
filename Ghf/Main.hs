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

main = do
    args <- getArgs
    (o,fl) <- ghfOpts args
    st <- initGUI
    --mapM_ putStrLn st
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
    --searching
    entry <- entryNew
    hbf <- hBoxNew False 1
    
    let ghf = Ghf win nb [] [sblc,sbio,sb] entry 
    ghfR <- newIORef ghf
    (acc,menus) <- runReaderT (makeMenu actions menuDescription) ghfR
    let mb = fromJust $menus !! 0
    let tb = fromJust $menus !! 1
    windowAddAccelGroup win acc
    vb <- vBoxNew False 1  -- Top-level vbox
    hb <- hBoxNew False 1

    boxPackStart hbf entry PackGrow 0
    
    boxPackStart hb hbf PackGrow 0
    boxPackStart hb sblc PackNatural 0
    boxPackStart hb sbio PackNatural 0

    boxPackStart vb mb PackNatural 0
    boxPackStart vb tb PackNatural 0
    boxPackStart vb nb PackGrow 0
    boxPackStart vb hb PackNatural 0


    win `onDelete` (\_ -> do runReaderT quit ghfR; return True)
    win `containerAdd` vb
    entry `afterInsertText` (\_ _ -> do runReaderT editFindInc ghfR)
    entry `afterDeleteText` (\_ _ -> do runReaderT editFindInc ghfR; return ())
    entry `afterKeyPress`  (\e -> do runReaderT (editFindKey e) ghfR; return True)
    entry `onEntryActivate` runReaderT editFindNext ghfR

      -- show the widget and run the main loop
    windowSetDefaultSize win 700 1000
    flip runReaderT ghfR $ case fl of
        [] -> newTextBuffer "Unnamed" Nothing
        otherwise  -> mapM_ (\fn -> (newTextBuffer (takeFileName fn) (Just fn))) fl 
    widgetShowAll win
    widgetHide entry
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
           ,("Quit","_Quit",Nothing,Just "gtk-quit",quit,Just "<control>Q")

           ,("Edit", "_Edit", Nothing, Nothing,return (),Nothing)
           ,("EditUndo","_Undo", Nothing, Just "gtk-undo",editUndo,Just "<control>Z") 
           ,("EditRedo","_Redo", Nothing, Just "gtk-redo",editRedo,Just "<shift><control>Z")
           ,("EditCut","Cu_t", Nothing, Nothing{--Just "gtk-cut"--},editCut,Nothing {--Just "<control>X"--})
           ,("EditCopy","_Copy", Nothing, Nothing{--Just "gtk-copy"--},editCopy,Nothing {--Just "<control>C"--})
           ,("EditPaste","_Paste", Nothing, Nothing{--Just "gtk-paste"--},editPaste,Nothing {--Just "<control>V"--})
           ,("EditDelete","_Delete", Nothing, Just "gtk-delete",editDelete,Nothing)
           ,("EditSelectAll","Select_All", Nothing, Just "gtk-select-all",editSelectAll,Just "<control>A")
           ,("EditFind","_Find", Nothing, Just "gtk-find",editFind,Just "<control>F")
           ,("EditFindNext","Find _Next", Nothing, Just "gtk-find-next",editFindNext,Just "<control>G")
           ,("EditFindPrevious","Find _Previous", Nothing, Just "gtk-find-previous",editFindPrevious,Just "<shift><control>G")
           ,("EditReplace","_Replace", Nothing, Just "gtk-replace",editReplace,Just "<control>R")


           ,("Help", "_Help", Nothing, Nothing,return (),Nothing)
           ,("HelpAbout","About", Nothing, Just "gtk-about",aboutDialog,Nothing)
           ]  

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
     \<menu name=\"_Edit\" action=\"Edit\">\n\
       \<menuitem name=\"_Undo\" action=\"EditUndo\" />\n\
       \<menuitem name=\"_Redo\" action=\"EditRedo\" />\n\
       \<separator/>\n\
       \<menuitem name=\"Cu_t\" action=\"EditCut\" />\n\
       \<menuitem name=\"_Copy\" action=\"EditCopy\" />\n\
       \<menuitem name=\"_Paste\" action=\"EditPaste\" />\n\
       \<menuitem name=\"_Delete\" action=\"EditDelete\" />\n\
       \<separator/>\n\
       \<menuitem name=\"Select _All\" action=\"EditSelectAll\" />\n\
       \<separator/>\n\
       \<menuitem name=\"_Find\" action=\"EditFind\" />\n\
       \<menuitem name=\"Find_Next\" action=\"EditFindNext\" />\n\
       \<menuitem name=\"Find_Previous\" action=\"EditFindPrevious\" />\n\
       \<menuitem name=\"Replace\" action=\"EditReplace\" />\n\
     \</menu>\n\
    \<menu name=\"_Help\" action=\"Help\">\n\
       \<menuitem name=\"_About\" action=\"HelpAbout\" />\n\
     \</menu>\n\
   \</menubar>\n\
    \<toolbar>\n\
     \<placeholder name=\"FileToolItems\">\n\
       \<separator/>\n\
       \<toolitem name=\"New\" action=\"FileNew\"/>\n\
       \<toolitem name=\"Open\" action=\"FileOpen\"/>\n\
       \<toolitem name=\"Save\" action=\"FileSave\"/>\n\
       \<toolitem name=\"Close\" action=\"FileClose\"/>\n\
       \<separator/>\n\
     \</placeholder>\n\
     \<placeholder name=\"FileEditItems\">\n\
       \<separator/>\n\
       \<toolitem name=\"Undo\" action=\"EditUndo\"/>\n\
       \<toolitem name=\"Redo\" action=\"EditRedo\"/>\n\
       \<separator/>\n\
     \</placeholder>\n\
   \</toolbar>\n\
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
        widgets <- mapM (uiManagerGetWidget uiManager) ["ui/menubar","ui/toolbar"]
        return (accGroup,widgets)
    where
        actm ghfR ag (name,label,tooltip,stockId,ghfAction,acc) = do
            act <- actionNew name label tooltip stockId
            onActionActivate act (runReaderT ghfAction ghfR) 
            actionGroupAddActionWithAccel ag act acc
                    
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
 