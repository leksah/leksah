module Ghf.Menu (
    makeMenu

)where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Types

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
    ,AD "Move" "Move" (Just "Move the current pane") Nothing
        viewMove Nothing False
    ,AD "MoveUp" "Move _Up" (Just "Move the current pane up in the hierarchy") Nothing
        viewMoveUp Nothing False
    ,AD "SplitHorizontal" "Split H-orizontal" (Just "Split the current pane in horizontal direction") Nothing
        viewSplitHorizontal (Just "<Ctrl>2") False
    ,AD "SplitVertical" "Split _Vertical" (Just "Split the current pane in vertical direction") Nothing
        viewSplitVertical (Just "<Ctrl>3") False
    ,AD "Collapse" "_Collapse" (Just "Collapse the panes around the currentla selected pane into one") Nothing
        viewCollapse (Just "<Ctrl>1") False

    ,AD "Help" "_Help" Nothing Nothing (return ()) Nothing False
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
       \<menuitem name=\"Move _Horizontal\" action=\"MoveHorizontal\" />\n\
       \<menuitem name=\"Move _Vertical\" action=\"MoveVertical\" />\n\
       \<menuitem name=\"Move _Up\" action=\"MoveUp\" />\n\
       \<menuitem name=\"Split H_orizontal\" action=\"SplitHorizontal\" />\n\
       \<menuitem name=\"Split V_ertical\" action=\"SplitVertical\" />\n\
       \<menuitem name=\"_Collapse\" action=\"Collapse\" />\n\
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