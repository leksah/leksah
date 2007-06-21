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
                  ,   panes = Map.empty
                  ,   activePane = Nothing
                  ,   paneMap = Map.empty
                  ,   layout = TerminalP}
    ghfR <- newIORef ghf
    (acc,menus) <- runReaderT (makeMenu uiManager actions menuDescription) ghfR
    let mb = fromJust $menus !! 0
    let tb = fromJust $menus !! 1
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

    win `onDelete` (\_ -> do runReaderT quit ghfR; return True)
    containerAdd win vb

    windowSetDefaultSize win 700 1000
    flip runReaderT ghfR $ case fl of
        [] -> newTextBuffer "Unnamed" Nothing
        otherwise  -> mapM_ (\fn -> (newTextBuffer (takeFileName fn) (Just fn))) fl
    widgetShowAll win
    mainGUI
    hbf <- runReaderT getFindBar ghfR
    widgetHide hbf
    spinL <- runReaderT getGotoLineSpin ghfR
    widgetHide spinL
     
quit :: GhfAction
quit = do
    bufs    <- readGhf panes
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


data ActionDescr = AD {
                name :: String
            ,   label :: String
            ,   tooltip ::Maybe String
            ,   stockID :: Maybe String
            ,   action :: GhfAction
            ,   accelerator :: Maybe String
            ,   isToggle :: Bool}

actions :: [ActionDescr]
actions =   
    [(AD "File" "_File" Nothing Nothing (return ()) Nothing False)
    ,(AD "FileNew" "_New" (Just "Opens a new empty buffer") (Just "gtk-new") 
        fileNew (Just "<control>N") False)
    ,AD "FileOpen" "_Open" (Just "Opens an existing file") (Just "gtk-open") 
        fileOpen (Just "<control>O") False    
    ,AD "FileSave" "_Save" (Just "Saves the current buffer") (Just "gtk-save") 
        (fileSave False) (Just "<control>S") False
    ,AD "FileSaveAs" "Save_As" (Just "Saves the current buffer as a new file") (Just "gtk-save_as") 
        (fileSave True) (Just "") False 
    ,AD "FileClose" "_Close" (Just "Closes the current buffer") (Just "gtk-close") 
        (do fileClose; return ()) (Just "<control>W") False
    ,AD "Quit" "_Quit" (Just "Quits this program") (Just "gtk-quit") 
        quit (Just "<control>Q") False

    ,AD "Edit" "_Edit" Nothing Nothing (return ()) Nothing False
    ,AD "EditUndo" "_Undo" (Just "Undos the last user action") (Just "gtk-undo")
        editUndo (Just "<control>Z") False 
    ,AD "EditRedo" "_Redo" (Just "Undos the last user action") (Just "gtk-redo")
        editRedo (Just "<shift><control>Z") False
    ,AD "EditCut" "Cu_t" Nothing Nothing{--Just "gtk-cut"--}
        editCut Nothing {--Just "<control>X"--} False
    ,AD "EditCopy" "_Copy"  Nothing  Nothing{--Just "gtk-copy"--}
        editCopy Nothing {--Just "<control>C"--} False
    ,AD "EditPaste" "_Paste" Nothing Nothing{--Just "gtk-paste"--}
        editPaste Nothing {--Just "<control>V"--} False
    ,AD "EditDelete" "_Delete" Nothing (Just "gtk-delete")
        editDelete Nothing False
    ,AD "EditSelectAll" "Select_All" (Just "Select the whole text in the current buffer") (Just "gtk-select-all")
        editSelectAll (Just "<control>A") False
    ,AD "EditFind" "_Find" (Just "Search for a text string") (Just "gtk-find") 
        editFindShow (Just "<control>F") False
    ,AD "EditFindNext" "Find _Next" (Just "Find the next occurence of the text string") (Just "gtk-find-next")
        (editFindInc Forward) (Just "F3") False
    ,AD "EditFindPrevious" "Find _Previous" (Just "Find the previous occurence of the text string") (Just "gtk-find-previous")
        (editFindInc Backward) (Just "<shift>F3") False
    ,AD "EditReplace" "_Replace" Nothing (Just "gtk-replace") 
        replaceDialog (Just "<control>R") False
    ,AD "EditGotoLine" "_Goto Line" (Just "Go to line with a known index") (Just "gtk-jump") 
        editGotoLine (Just "<control>G") False

    ,AD "EditComment" "_Comment" (Just "Add a line style comment to the selected lies") Nothing 
        editComment (Just "<alt><shift>Right") False
    ,AD "EditUncomment" "_Uncomment" (Just "Remove a line style comment") Nothing 
        editUncomment (Just "<alt><shift>Left") False
    ,AD "EditShiftRight" "Shift _Right" (Just "Shift right") Nothing 
        editShiftRight (Just "<alt>Right") False
    ,AD "EditShiftLeft" "Shift _Left" (Just "Shift Left") Nothing 
        editShiftLeft (Just "<alt>Left") False

    ,AD "View" "_View" Nothing Nothing (return ()) Nothing False
    ,AD "MoveLeft" "Move _Left" (Just "Move the current pane left") Nothing
        (viewMove LeftP) (Just "<Ctrl><alt><shift>Left") False
    ,AD "MoveRight" "Move _Right" (Just "Move the current pane right") Nothing
        (viewMove RightP) (Just "<Ctrl><alt><shift>Right") False
    ,AD "MoveUp" "Move _Up" (Just "Move the current pane up") Nothing
        (viewMove TopP) (Just "<Ctrl><alt><shift>Up") False
    ,AD "MoveDown" "Move _Down" (Just "Move the current pane down") Nothing
        (viewMove BottomP) (Just "<Ctrl><alt><shift>Down") False
    ,AD "SplitHorizontal" "Split H_orizontal" (Just "Split the current pane in horizontal direction") Nothing
        viewSplitHorizontal (Just "<Ctrl>2") False
    ,AD "SplitVertical" "Split _Vertical" (Just "Split the current pane in vertical direction") Nothing
        viewSplitVertical (Just "<Ctrl>3") False
    ,AD "Collapse" "_Collapse" (Just "Collapse the panes around the currentla selected pane into one") Nothing
        viewCollapse (Just "<Ctrl>1") False

    ,AD "TabsLeft" "Tabs Left" (Just "Shows the tabs of the current notebook on the left") Nothing
        (viewTabsPos PosLeft) Nothing False
    ,AD "TabsRight" "Tabs Right" (Just "Shows the tabs of the current notebook on the right") Nothing
        (viewTabsPos PosRight) Nothing False
    ,AD "TabsUp" "Tabs Up" (Just "Shows the tabs of the current notebook on the top") Nothing
        (viewTabsPos PosTop) Nothing False
    ,AD "TabsDown" "Tabs Down" (Just "Shows the tabs of the current notebook on the bottom") Nothing
        (viewTabsPos PosBottom) Nothing False
    ,AD "SwitchTabs" "Tabs On/Off" (Just "Switches if tabs for the current notebook are visible") Nothing
        viewSwitchTabs Nothing False


    ,AD "Help" "_Help" Nothing Nothing (return ()) Nothing False
    ,AD "HelpDebug" "Debug" (Just "<Ctrl>d") Nothing helpDebug Nothing False
    ,AD "HelpAbout" "About" Nothing (Just "gtk-about") aboutDialog Nothing False]
 

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
       \<menuitem name=\"_Goto Line\" action=\"EditGotoLine\" />\n\
       \<separator/>\n\
       \<menuitem name=\"Replace\" action=\"EditReplace\" />\n\
       \<separator/>\n\
       \<menuitem name=\"Comment\" action=\"EditComment\" />\n\
       \<menuitem name=\"Uncomment\" action=\"EditUncomment\" />\n\
       \<menuitem name=\"Shift Left\" action=\"EditShiftLeft\" />\n\
       \<menuitem name=\"Shift Right\" action=\"EditShiftRight\" />\n\
     \</menu>\n\
    \<menu name=\"_View\" action=\"View\">\n\
       \<menuitem name=\"Move _Left\" action=\"MoveLeft\" />\n\
       \<menuitem name=\"Move _Right\" action=\"MoveRight\" />\n\
       \<menuitem name=\"Move _Up\" action=\"MoveUp\" />\n\
       \<menuitem name=\"Move _Down\" action=\"MoveDown\" />\n\
       \<separator/>\n\
       \<menuitem name=\"Split H_orizontal\" action=\"SplitHorizontal\" />\n\
       \<menuitem name=\"Split V_ertical\" action=\"SplitVertical\" />\n\
       \<menuitem name=\"_Collapse\" action=\"Collapse\" />\n\
       \<separator/>\n\
       \<menuitem name=\"Tabs _Left\" action=\"TabsLeft\" />\n\
       \<menuitem name=\"Tabs _Right\" action=\"TabsRight\" />\n\
       \<menuitem name=\"Tabs _Up\" action=\"TabsUp\" />\n\
       \<menuitem name=\"Tabs _Down\" action=\"TabsDown\" />\n\
       \<menuitem name=\"Switch Tabs\" action=\"SwitchTabs\" />\n\
     \</menu>\n\
    \<menu name=\"_Help\" action=\"Help\">\n\
       \<menuitem name=\"_Debug\" action=\"HelpDebug\" />\n\
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

makeMenu :: UIManager -> [ActionDescr] -> String -> GhfM (AccelGroup, [Maybe Widget])
makeMenu uiManager actions menuDescription = do
    ghfR <- ask 
    lift $ do
        actionGroupGlobal <- actionGroupNew "global"
        mapM_ (actm ghfR actionGroupGlobal) actions
        uiManagerInsertActionGroup uiManager actionGroupGlobal 1
        uiManagerAddUiFromString uiManager menuDescription 
        accGroup <- uiManagerGetAccelGroup uiManager
        widgets <- mapM (uiManagerGetWidget uiManager) ["ui/menubar","ui/toolbar"]
        return (accGroup,widgets)
    where
        actm ghfR ag (AD name label tooltip stockId ghfAction acc isToggle) = 
            if isToggle 
                then do
                    act <- toggleActionNew name label tooltip stockId
                    onToggleActionToggled act (runReaderT ghfAction ghfR) 
                    actionGroupAddActionWithAccel ag act acc
                else do
                    act <- actionNew name label tooltip stockId
                    onActionActivate act (runReaderT ghfAction ghfR) 
                    actionGroupAddActionWithAccel ag act acc

buildStatusbar :: GhfRef -> IO (HBox)
buildStatusbar ghfR = do
    sb <- statusbarNew
    statusbarSetHasResizeGrip sb False

    sblc <- statusbarNew
    widgetSetName sblc $"statusBarLineColumn" 
    statusbarSetHasResizeGrip sblc False
    widgetSetSizeRequest sblc 140 (-1)

    sbio <- statusbarNew
    widgetSetName sbio $"statusBarInsertOverwrite" 
    statusbarSetHasResizeGrip sbio False
    widgetSetSizeRequest sbio 40 (-1)

    entry <- entryNew
    widgetSetName entry $"searchEntry"

    caseSensitiveButton <- checkButtonNewWithLabel "Case sensitive"
    widgetSetName caseSensitiveButton $"caseSensitiveButton" 

    entireWordButton <- checkButtonNewWithLabel "Entire word"
    widgetSetName entireWordButton $"entireWordButton" 

    wrapAroundButton <- checkButtonNewWithLabel "Warp around"
    widgetSetName wrapAroundButton $"wrapAroundButton" 

    dummy <- hBoxNew False 1
    widgetSetName dummy $"dummyBox" 

    spinL <- spinButtonNewWithRange 1.0 100.0 10.0
    widgetSetName spinL $"gotoLineEntry"

    hbf <- hBoxNew False 1
    widgetSetName hbf $"searchBox" 
    boxPackStart hbf entry PackGrow 0
    boxPackStart hbf caseSensitiveButton PackNatural 0
    boxPackStart hbf entireWordButton PackNatural 0
    boxPackStart hbf wrapAroundButton PackNatural 0

    hb <- hBoxNew False 1
    widgetSetName hb $ "statusBox"
    boxPackStart hb dummy PackGrow 0
    boxPackStart hb spinL PackGrow 0
    boxPackStart hb hbf PackGrow 0
    boxPackStart hb sblc PackNatural 0
    boxPackStart hb sbio PackNatural 0

    entry `afterInsertText` (\_ _ -> do runReaderT (editFindInc Insert) ghfR; 
                                        t <- entryGetText entry
                                        return (length t))
    entry `afterDeleteText` (\_ _ -> do runReaderT (editFindInc Delete) ghfR; return ())
    entry `afterKeyPress`  (\e -> do runReaderT (editFindKey e) ghfR; return True)
    entry `onEntryActivate` runReaderT (editFindHide) ghfR

    spinL `afterKeyPress`  (\e -> do runReaderT (editGotoLineKey e) ghfR; return True)
    spinL `afterEntryActivate` runReaderT editGotoLineEnd ghfR
    spinL `afterFocusOut` (\_ -> do runReaderT editGotoLineEnd ghfR; return False)
    return hb
