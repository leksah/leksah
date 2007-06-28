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
import Data.Maybe ( fromMaybe, isJust, fromJust )
import qualified Data.Map as Map
import Data.Map(Map)

import Ghf.Core
import Ghf.Editor
import Ghf.Dialogs
import Ghf.View
import Ghf.Keymap
import Ghf.SourceCandy
import Ghf.Preferences

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
    
    prefs <- readPrefs "config/Default2.prefs"
    putStrLn $show prefs
    writePrefs "config/Default.prefs" prefs

    keyMap <- parseKeymap "config/Default.keymap"
--    putStrLn $show keyMap
    let accelActions = setKeymap actions keyMap
    specialKeys <- buildSpecialKeys keyMap accelActions

    candy <- parseCandy "config/Default.candy"
--    putStrLn $show candy
    
    win <- windowNew
    windowSetIconFromFile win "bin/ghf.gif"
    uiManager <- uiManagerNew
    let ghf = Ghf
          {   window = win
          ,   uiManager = uiManager
          ,   panes = Map.empty
          ,   activePane = Nothing
          ,   paneMap = Map.empty
          ,   layout = TerminalP
          ,   specialKeys = specialKeys
          ,   specialKey = Nothing
          ,   candy = candy
          ,   prefs = prefs
          }
    ghfR <- newIORef ghf


    (acc,menus) <- runReaderT (makeMenu uiManager accelActions menuDescription) ghfR
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

    win `onDelete` (\ _ -> do runReaderT quit ghfR; return True)

    win `onKeyPress` (\ e -> runReaderT (handleSpecialKeystrokes e) ghfR)
    
    containerAdd win vb

    windowSetDefaultSize win 700 1000
    flip runReaderT ghfR $ case fl of
        [] -> newTextBuffer "Unnamed" Nothing
        otherwise  -> mapM_ (\ fn -> (newTextBuffer (takeFileName fn) (Just fn))) fl
    widgetShowAll win
    hbf <- runReaderT getFindBar ghfR
    widgetHide hbf
    spinL <- runReaderT getGotoLineSpin ghfR
    widgetHide spinL
    mainGUI
     
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

actions :: [ActionDescr]
actions =   
    [(AD "File" "_File" Nothing Nothing (return ()) [] False)
    ,(AD "FileNew" "_New" Nothing (Just "gtk-new") 
        fileNew [] False)
    ,AD "FileOpen" "_Open" Nothing (Just "gtk-open") 
        fileOpen [] False    
    ,AD "FileSave" "_Save" Nothing (Just "gtk-save") 
        (fileSave False) [] False
    ,AD "FileSaveAs" "Save_As" Nothing (Just "gtk-save_as") 
        (fileSave True) [] False 
    ,AD "FileClose" "_Close" Nothing (Just "gtk-close") 
        (do fileClose; return ()) [] False
    ,AD "Quit" "_Quit" Nothing (Just "gtk-quit") 
        quit [] False

    ,AD "Edit" "_Edit" Nothing Nothing (return ()) [] False
    ,AD "EditUndo" "_Undo" Nothing (Just "gtk-undo")
        editUndo [] False 
    ,AD "EditRedo" "_Redo" Nothing (Just "gtk-redo")
        editRedo [] False
    ,AD "EditCut" "Cu_t" Nothing Nothing{--Just "gtk-cut"--}
        editCut [] {--Just "<control>X"--} False
    ,AD "EditCopy" "_Copy"  Nothing  Nothing{--Just "gtk-copy"--}
        editCopy [] {--Just "<control>C"--} False
    ,AD "EditPaste" "_Paste" Nothing Nothing{--Just "gtk-paste"--}
        editPaste [] {--Just "<control>V"--} False
    ,AD "EditDelete" "_Delete" Nothing (Just "gtk-delete")
        editDelete [] False
    ,AD "EditSelectAll" "Select_All" Nothing (Just "gtk-select-all")
        editSelectAll [] False
    ,AD "EditFind" "_Find" Nothing (Just "gtk-find") 
        editFindShow [] False
    ,AD "EditFindNext" "Find _Next" Nothing (Just "gtk-find-next")
        (editFindInc Forward) [] False
    ,AD "EditFindPrevious" "Find _Previous" Nothing (Just "gtk-find-previous")
        (editFindInc Backward) [] False
    ,AD "EditReplace" "_Replace" Nothing (Just "gtk-replace") 
        replaceDialog [] False
    ,AD "EditGotoLine" "_Goto Line" Nothing (Just "gtk-jump") 
        editGotoLine [] False

    ,AD "EditComment" "_Comment" Nothing Nothing 
        editComment [] False
    ,AD "EditUncomment" "_Uncomment" Nothing Nothing 
        editUncomment [] False
    ,AD "EditShiftRight" "Shift _Right" Nothing Nothing 
        editShiftRight [] False
    ,AD "EditShiftLeft" "Shift _Left" Nothing Nothing 
        editShiftLeft [] False

    ,AD "EditCandy" "_To Candy" Nothing Nothing 
        editCandy [] True

    ,AD "View" "_View" Nothing Nothing (return ()) [] False
    ,AD "ViewMoveLeft" "Move _Left" Nothing Nothing
        (viewMove LeftP) [] False
    ,AD "ViewMoveRight" "Move _Right" Nothing Nothing
        (viewMove RightP) [] False
    ,AD "ViewMoveUp" "Move _Up" Nothing Nothing
        (viewMove TopP) [] False
    ,AD "ViewMoveDown" "Move _Down" Nothing Nothing
        (viewMove BottomP) [] False
    ,AD "ViewSplitHorizontal" "Split H_orizontal" Nothing Nothing
        viewSplitHorizontal [] False
    ,AD "ViewSplitVertical" "Split _Vertical" Nothing Nothing
        viewSplitVertical [] False
    ,AD "ViewCollapse" "_Collapse" Nothing Nothing
        viewCollapse [] False

    ,AD "ViewTabsLeft" "Tabs Left" Nothing Nothing
        (viewTabsPos PosLeft) [] False
    ,AD "ViewTabsRight" "Tabs Right" Nothing Nothing
        (viewTabsPos PosRight) [] False
    ,AD "ViewTabsUp" "Tabs Up" Nothing Nothing
        (viewTabsPos PosTop) [] False
    ,AD "ViewTabsDown" "Tabs Down" Nothing Nothing
        (viewTabsPos PosBottom) [] False
    ,AD "ViewSwitchTabs" "Tabs On/Off" Nothing Nothing
        viewSwitchTabs [] False

    ,AD "Preferences" "_Preferences" Nothing Nothing (return ()) [] False
    ,AD "PrefsEdit" "_Edit Prefs" Nothing Nothing
        editPrefs [] False


    ,AD "Help" "_Help" Nothing Nothing (return ()) [] False
    ,AD "HelpDebug" "Debug" (Just "<Ctrl>d") Nothing helpDebug [] False
    ,AD "HelpAbout" "About" Nothing (Just "gtk-about") aboutDialog [] False]
 

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
       \<separator/>\n\
       \<menuitem name=\"Source Candy\" action=\"EditCandy\" />\n\
     \</menu>\n\
    \<menu name=\"_View\" action=\"View\">\n\
       \<menuitem name=\"Move _Left\" action=\"ViewMoveLeft\" />\n\
       \<menuitem name=\"Move _Right\" action=\"ViewMoveRight\" />\n\
       \<menuitem name=\"Move _Up\" action=\"ViewMoveUp\" />\n\
       \<menuitem name=\"Move _Down\" action=\"ViewMoveDown\" />\n\
       \<separator/>\n\
       \<menuitem name=\"Split H_orizontal\" action=\"ViewSplitHorizontal\" />\n\
       \<menuitem name=\"Split V_ertical\" action=\"ViewSplitVertical\" />\n\
       \<menuitem name=\"_Collapse\" action=\"ViewCollapse\" />\n\
       \<separator/>\n\
       \<menuitem name=\"Tabs _Left\" action=\"ViewTabsLeft\" />\n\
       \<menuitem name=\"Tabs _Right\" action=\"ViewTabsRight\" />\n\
       \<menuitem name=\"Tabs _Up\" action=\"ViewTabsUp\" />\n\
       \<menuitem name=\"Tabs _Down\" action=\"ViewTabsDown\" />\n\
       \<menuitem name=\"Switch Tabs\" action=\"ViewSwitchTabs\" />\n\
     \</menu>\n\
    \<menu name=\"_Preferences\" action=\"Preferences\">\n\
       \<menuitem name=\"Edit Preferences\" action=\"PrefsEdit\" />\n\
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
        actm ghfR ag (AD name label tooltip stockId ghfAction accs isToggle) = do
            let (acc,accString) = if null accs 
                                    then (Nothing,"=" ++ name) 
                                    else (Just (head accs),(head accs) ++ "=" ++ name)
            if isToggle 
                then do      
                    act <- toggleActionNew name label tooltip stockId
                    onToggleActionToggled act (doAction ghfAction ghfR accString)  
                    actionGroupAddActionWithAccel ag act acc
                else do
                    act <- actionNew name label tooltip stockId
                    onActionActivate act (doAction ghfAction ghfR accString) 
                    actionGroupAddActionWithAccel ag act acc      
        doAction ghfAction ghfR accStr = 
            runReaderT (do
                ghfAction
                sb <- getSpecialKeys
                lift $statusbarPop sb 1
                lift $statusbarPush sb 1 $accStr
                return ()) ghfR

buildStatusbar :: GhfRef -> IO (HBox)
buildStatusbar ghfR = do
    sb <- statusbarNew
    statusbarSetHasResizeGrip sb False

    sblk <- statusbarNew
    widgetSetName sblk $"statusBarSpecialKeys" 
    statusbarSetHasResizeGrip sblk False
    widgetSetSizeRequest sblk 210 (-1)

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
    boxPackStart hb sblk PackNatural 0
    boxPackStart hb dummy PackGrow 0
    boxPackStart hb spinL PackGrow 0
    boxPackStart hb hbf PackGrow 0
    boxPackStart hb sblc PackNatural 0
    boxPackStart hb sbio PackNatural 0

    entry `afterInsertText` (\ _ _ -> do 
        runReaderT (editFindInc Insert) ghfR
        t <- entryGetText entry
        return (length t))
    entry `afterDeleteText` (\ _ _ -> do runReaderT (editFindInc Delete) ghfR; return ())
    entry `afterKeyPress`  (\ e -> do runReaderT (editFindKey e) ghfR; return True)
    entry `onEntryActivate` runReaderT (editFindHide) ghfR

    spinL `afterKeyPress`  (\ e -> do runReaderT (editGotoLineKey e) ghfR; return True)
    spinL `afterEntryActivate` runReaderT editGotoLineEnd ghfR
    spinL `afterFocusOut` (\ _ -> do runReaderT editGotoLineEnd ghfR; return False)
    return hb


