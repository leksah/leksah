{-# OPTIONS_GHC -XTypeSynonymInstances -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module       :  IDE.Menu
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | Module for actions, menus and toolbars and the rest ...
--
-------------------------------------------------------------------------------


module IDE.Command (
    mkActions
,   menuDescription
,   makeMenu
,   quit
,   aboutDialog
,   buildStatusbar
,   newIcons
,   setSensitivity
,   updateRecentEntries
,   handleSpecialKeystrokes
,   registerEvents
,   instrumentWindow
,   instrumentSecWindow
) where

import Graphics.UI.Gtk
       (containerAdd, windowAddAccelGroup, onKeyPress, boxPackEnd,
        boxPackStart, widgetSetName, vBoxNew, windowSetIconFromFile,
        Widget, Window, actionGroupGetAction, uiManagerGetActionGroups,
        Action, actionSetSensitive, iconFactoryAdd, iconSetNewFromPixbuf,
        pixbufNewFromFile, iconFactoryAddDefault, iconFactoryNew,
        dialogRun, aboutDialogSetAuthors, aboutDialogSetWebsite,
        aboutDialogSetLicense, aboutDialogSetComments,
        aboutDialogSetCopyright, aboutDialogSetVersion, aboutDialogSetName,
        aboutDialogNew, mainQuit, widgetHide, widgetShow,
        widgetSetSensitive, castToWidget, separatorMenuItemNew,
        containerGetChildren, Menu, widgetSetSizeRequest, toolbarSetStyle,
        toolbarSetIconSize, castToToolbar, castToMenuBar,
        uiManagerGetWidget, uiManagerGetAccelGroup, onActionActivate,
        actionNew, actionGroupAddActionWithAccel, actionToggled,
        toggleActionNew, uiManagerAddUiFromString,
        uiManagerInsertActionGroup, actionGroupNew, UIManager,
        widgetShowAll, menuItemSetSubmenu, widgetDestroy, widgetHideAll,
        menuItemGetSubmenu, menuShellAppend, onActivateLeaf,
        menuItemNewWithLabel, menuNew, Packing(..), ToolbarStyle(..),
        PositionType(..), on, IconSize(..))
import Control.Monad.Reader
import System.FilePath
import Data.Version
import Prelude hiding (catch)
import Control.Exception
import Data.Maybe


import IDE.Core.State
import IDE.Pane.SourceBuffer
import IDE.Pane.Preferences
import IDE.Pane.PackageFlags
import IDE.Pane.PackageEditor
import IDE.Pane.Errors
import IDE.Package
import IDE.Pane.Log
import IDE.Session
import IDE.Pane.Modules
import IDE.Find
import IDE.Utils.FileUtils
import IDE.Utils.GUIUtils
import IDE.Pane.References
import Paths_leksah
import IDE.GUIHistory
import IDE.Metainfo.Provider
       (getWorkspaceInfo, getIdentifierDescr, updateWorkspaceInfo,
        updateSystemInfo, rebuildSystemInfo, rebuildWorkspaceInfo)
import IDE.NotebookFlipper
import IDE.ImportTool (addAllImports)
import IDE.LogRef
import IDE.Debug
import System.Directory (doesFileExist)
import qualified Graphics.UI.Gtk.Gdk.Events as GdkEvents
import Graphics.UI.Gtk.Gdk.Events
    (Modifier(..),
     Event(..))
import qualified Data.Map as  Map (lookup)
import Data.List (sort)
import Control.Event (registerEvent)
import IDE.Pane.Breakpoints
    (fillBreakpointList, selectBreak)
import IDE.Workspaces
import IDE.Statusbar
import IDE.Pane.Workspace
import IDE.Pane.Variables (fillVariablesList)
import IDE.Pane.Trace (fillTraceList)
import IDE.PaneGroups
import IDE.Pane.Search (getSearch, setChoices, searchMetaGUI)
import IDE.Pane.Grep (getGrep)

--
-- | The Actions known to the system (they can be activated by keystrokes or menus)
--
mkActions :: [ActionDescr IDERef]
mkActions =
    [AD "File" "_File" Nothing Nothing (return ()) [] False
    ,AD "FileNew" "_New" Nothing (Just "gtk-new")
        fileNew [] False
    ,AD "FileOpen" "_Open" Nothing (Just "gtk-open")
        fileOpen [] False
    ,AD "RecentFiles" "_Recent Files" Nothing Nothing (return ()) [] False
    ,AD "FileRevert" "_Revert" Nothing Nothing
        fileRevert [] False
    ,AD "FileSave" "_Save" Nothing (Just "gtk-save")
        (do fileSave False; return ()) [] False
    ,AD "FileSaveAs" "Save _As" Nothing (Just "gtk-save-as")
        (do fileSave True; return ()) [] False
    ,AD "FileSaveAll" "Save A_ll" Nothing Nothing
        (do fileSaveAll (\ b -> return (bufferName b /= "_Eval.hs")); return ()) [] False
    ,AD "FileClose" "_Close" Nothing (Just "gtk-close")
        (do fileClose; return ()) [] False
    ,AD "FileCloseAll" "Close All" Nothing Nothing
        (do fileCloseAll (\ b -> return (bufferName b /= "_Eval.hs")); return ()) [] False
    ,AD "FileCloseAllButPackage" "Close All But Package" Nothing Nothing
        (do fileCloseAllButPackage; return ()) [] False
    ,AD "FileCloseAllButWorkspace" "Close All But Workspace" Nothing Nothing
        (do fileCloseAllButWorkspace; return ()) [] False
    ,AD "Quit" "_Quit" Nothing (Just "gtk-quit")
        quit [] False

    ,AD "Edit" "_Edit" Nothing Nothing (return ()) [] False
    ,AD "EditUndo" "_Undo" Nothing (Just "gtk-undo")
        editUndo [] False
    ,AD "EditRedo" "_Redo" Nothing (Just "gtk-redo")
        editRedo [] False
    ,AD "EditCut" "Cu_t" Nothing (Just "gtk-cut")
        editCut [] {--Just "<control>X"--} False
    ,AD "EditCopy" "_Copy"  Nothing  (Just "gtk-copy")
        editCopy [] {--Just "<control>C"--} False
    ,AD "EditPaste" "_Paste" Nothing (Just "gtk-paste")
        editPaste [] {--Just "<control>V"--} False
    ,AD "EditDelete" "_Delete" Nothing (Just "gtk-delete")
        editDelete [] False
    ,AD "EditSelectAll" "Select_All" Nothing (Just "gtk-select-all")
        editSelectAll [] False
    ,AD "EditFind" "Find" Nothing (Just "gtk-find")
        (editFindInc Initial) [] False
    ,AD "EditFindNext" "Find _Next" Nothing (Just "gtk-find-next")
        (editFindInc Forward) [] False
    ,AD "EditFindPrevious" "Find _Previous" Nothing (Just "gtk-find-previous")
        (editFindInc Backward) [] False
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

    --,AD "Align" "_Align" Nothing Nothing (return ()) [] False
    ,AD "EditAlignEqual" "Align _=" Nothing Nothing
        (align '=') [] False
    ,AD "EditAlignRightArrow" "Align -_>" Nothing Nothing
        (align '>') [] False
    ,AD "EditAlignLeftArrow" "Align _<-" Nothing Nothing
        (align '<') [] False
    ,AD "EditAlignTypeSig" "Align _::" Nothing Nothing
        (align ':') [] False

    ,AD "Workspace" "_Workspace" Nothing Nothing (return ()) [] False
    ,AD "NewWorkspace" "_New Workspace" Nothing Nothing
        workspaceNew [] False
    ,AD "OpenWorkspace" "_Open Workspace" Nothing Nothing
        workspaceOpen [] False
    ,AD "RecentWorkspaces" "_Recent Workspaces" Nothing Nothing (return ()) [] False
    ,AD "CloseWorkspace" "_Close Workspace" Nothing Nothing
        workspaceClose [] False

    ,AD "CleanWorkspace" "Cl_ean Workspace" (Just "Cleans all packages") (Just "ide_clean")
        workspaceClean [] False
    ,AD "MakeWorkspace" "_Make Workspace" (Just "Makes all of this workspace") (Just "ide_configure")
        workspaceMake [] False
    ,AD "NextError" "_Next Error" (Just "Go to the next error") (Just "ide_error_next")
        nextError [] False
    ,AD "PreviousError" "_Previous Error" (Just "Go to the previous error") (Just "ide_error_prev")
        previousError [] False

    ,AD "Package" "_Package" Nothing Nothing (return ()) [] False
    ,AD "NewPackage" "_New Package" Nothing Nothing
        workspacePackageNew [] False
--    ,AD "RecentPackages" "_Recent Packages" Nothing Nothing (return ()) [] False
    ,AD "EditPackage" "_Edit Package" Nothing Nothing
        packageEdit [] False
--    ,AD "RemovePackage" "_Close Package" Nothing Nothing
--        removePackage [] False

    ,AD "PackageFlags" "Edit Flags" (Just "Edit the package flags") Nothing
        (getFlags Nothing >>= \ p -> displayPane p False) [] False
    ,AD "CleanPackage" "Cl_ean Package" (Just "Cleans the package") (Just "ide_clean")
        (packageClean Nothing) [] False
    ,AD "ConfigPackage" "_Configure Package" (Just "Configures the package") (Just "ide_configure")
        packageConfig [] False
    ,AD "BuildPackage" "_Build Package" (Just "Builds the package") (Just "ide_make")
        makePackage [] False
    ,AD "DocPackage" "_Build Documentation" (Just "Builds the documentation") Nothing
        packageDoc [] False
    ,AD "CopyPackage" "_Copy Package" (Just "Copies the package") Nothing
        packageCopy [] False
    ,AD "RunPackage" "_Run" (Just "Runs the package") (Just "ide_run")
        packageRun [] False
    ,AD "AddAllImports" "_Add All Imports" (Just "Resolve 'Not in scope' errors by adding the necessary imports") Nothing
        addAllImports [] False

    ,AD "InstallPackage" "_Install Package" Nothing Nothing
        packageInstall [] False
    ,AD "RegisterPackage" "_Register Package" Nothing Nothing
        packageRegister [] False
    ,AD "UnregisterPackage" "_Unregister" Nothing Nothing
        packageUnregister [] False
    ,AD "TestPackage" "Test Package" Nothing Nothing
        packageTest [] False
    ,AD "SdistPackage" "Source Dist" Nothing Nothing
        packageSdist [] False
    ,AD "OpenDocPackage" "_Open Doc" Nothing Nothing
        packageOpenDoc [] False

    ,AD "Debug" "_Debug" Nothing Nothing (return ()) [] False
    ,AD "StartDebugger" "_Start Debugger" (Just "Starts using the GHCi debugger for build and run") Nothing
        debugStart [] False
    ,AD "QuitDebugger" "_Quit Debugger" (Just "Quit the GHCi debugger if it is running") Nothing
        debugQuit [] False
    ,AD "ExecuteSelection" "_Execute Selection" (Just "Sends the selected text to the debugger") Nothing
        debugExecuteSelection [] False

    ,AD "DebugSetBreakpoint" "Set Breakpoint" (Just "Set a breakpoint on the selected name or current line") Nothing
        debugSetBreakpoint [] False
    ,AD "ShowNextBreakpoint" "Show Next Breakpoint" (Just "Show the next breakpoint") Nothing
        nextBreakpoint [] False
    ,AD "ShowPreviousBreakpoint" "Show Previous Breakpoint" (Just "Show the previous breakpoint") Nothing
        previousBreakpoint [] False
    ,AD "DebugDeleteAllBreakpoints" "Delete All Breakpoints" (Just "") Nothing
        debugDeleteAllBreakpoints [] False

    ,AD "DebugContinue" "Continue" (Just "Resume after a breakpoint") (Just "ide_continue")
        debugContinue [] False
    ,AD "DebugAbandon" "Abandon" (Just "At a breakpoint, abandon current computation") Nothing
        debugAbandon [] False
    ,AD "DebugStop" "Stop" (Just "Interrupt the running operation.") Nothing
        debugStop [] False

    ,AD "DebugStep" "Step" (Just "Single-step after stopping at a breakpoint") (Just "ide_step")
        debugStep [] False
    ,AD "DebugStepExpression" "Step Expression" (Just "Single-step into selected expression") Nothing
        debugStepExpression [] False
    ,AD "DebugStepLocal" "Step Local" (Just "Single-step within the current top-level binding") (Just "ide_local")
        debugStepLocal [] False
    ,AD "DebugStepModule" "Step Module" (Just "Single-step restricted to the current module") (Just "ide_module")
        debugStepModule [] False

    ,AD "DebugTrace" "Trace" (Just "Trace after stopping at a breakpoint") Nothing
        debugTrace [] False
    ,AD "DebugTraceExpression" "Trace Expression" (Just "Evaluate the selected expression with tracing on") Nothing
        debugTraceExpression [] False
    ,AD "DebugHistory" "History" (Just "After 'Trace', show the execution history") Nothing
        debugHistory [] False
    ,AD "DebugBack" "Back" (Just "Go back in the history (after 'Trace')") Nothing
        debugBack [] False
    ,AD "DebugForward" "Forward" (Just "Go forward in the history (after 'Back')") Nothing
        debugForward [] False

    ,AD "DebugForce" "Force" (Just "Print the selected expression, forcing unevaluated parts") Nothing
        debugForce [] False
    ,AD "DebugPrint" "Print" (Just "Prints a value without forcing its computation") Nothing
        debugPrint [] False
    ,AD "DebugSimplePrint" "SimplePrint" (Just "Simplifed version of Print") Nothing
        debugSimplePrint [] False

    ,AD "ShowBindings" "Show Bin_dings" (Just "Show the current bindings") Nothing
        debugShowBindings [] False
    ,AD "ShowBreakpoints" "Show _Breakpoints" (Just "Show the active breakpoints") Nothing
        debugShowBreakpoints [] False
    ,AD "ShowContext" "Show _Context" (Just "Show the breakpoint context") Nothing
        debugShowContext [] False
    ,AD "ShowLoadedModules" "Show Loaded _Modules" (Just "Show the currently loaded modules") Nothing
        debugShowModules [] False
    ,AD "ShowPackages" "Show _Packages" (Just "Show the currently active packages") Nothing
        debugShowPackages [] False
    ,AD "ShowLanguages" "Show _Languages" (Just "Show the currently active language") Nothing
        debugShowLanguages [] False

    ,AD "DebugInformation" "Information" (Just "Display information about the selected name(s)") Nothing
        debugInformation [] False
    ,AD "DebugKind" "Kind" (Just "Show the kind of the selected type") Nothing
        debugKind [] False
    ,AD "DebugType" "Type" (Just "Show the type of the selected expression") Nothing
        debugType [] False

    ,AD "Metadata" "_Metadata" Nothing Nothing (return ()) [] False
    ,AD "UpdateMetadataCurrent" "_Update workspace data" (Just "Updates data for the current workspace")
            (Just "ide_rebuild_meta") updateWorkspaceInfo [] False
    ,AD "RebuildMetadataCurrent" "_Rebuild workspace data" (Just "Rebuilds data for the current workspace")
            Nothing rebuildWorkspaceInfo [] False
    ,AD "UpdateMetadataLib" "U_pdate system data" Nothing Nothing
        updateSystemInfo [] False
    ,AD "RebuildMetadataLib" "R_ebuild system data" Nothing Nothing
        rebuildSystemInfo [] False

    ,AD "Session" "_Session" Nothing Nothing (return ()) [] False
    ,AD "SaveSession" "_Save Session" Nothing Nothing
        saveSessionAsPrompt [] False
    ,AD "LoadSession" "_Load Session" Nothing Nothing
        loadSessionPrompt [] False
    ,AD "ForgetSession" "_Forget Session" Nothing Nothing
        (return ()) [] True

    ,AD "Panes" "_Panes" Nothing Nothing (return ()) [] False
    ,AD "ShowBrowser" "Browser" Nothing Nothing
        showBrowser [] False
    ,AD "ShowDebugger" "Debugger" Nothing Nothing
        showDebugger [] False
    ,AD "ShowSearch" "Search" Nothing Nothing
        (getSearch Nothing  >>= \ p -> displayPane p False) [] False
    ,AD "ShowGrep" "Grep" Nothing Nothing
        (getGrep Nothing  >>= \ p -> displayPane p False) [] False
    ,AD "ShowErrors" "Errors" Nothing Nothing
        (getErrors Nothing  >>= \ p -> displayPane p False) [] False
    ,AD "ShowLog" "Log" Nothing Nothing
        showLog [] False
    ,AD "ShowReferences" "References" Nothing Nothing
        (getReferences Nothing >>= \ p -> displayPane p False) [] False
    ,AD "ShowWorkspace" "Workspace" Nothing Nothing
        (getWorkspace Nothing >>= \ p -> displayPane p False) [] False

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
    ,AD "ViewNest" "_Group" Nothing Nothing
        (viewNewGroup) [] False
    ,AD "ViewDetach" "_Detach" Nothing Nothing
        viewDetachInstrumented [] False

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

    ,AD "ViewClosePane" "Close pane" Nothing (Just "gtk-close")
        sessionClosePane [] False

    ,AD "FlipDown" "Flip down" Nothing Nothing
        flipDown [] False
    ,AD "FlipUp" "Flip up" Nothing Nothing
        flipUp [] False
    ,AD "StartComplete" "StartComplete" Nothing Nothing
        startComplete [] False

    ,AD "ViewHistoryBack" "Back" Nothing (Just "gtk-go-back")
        historyBack [] False
    ,AD "ViewHistoryForth" "Forward" Nothing (Just "gtk-go-forward")
        historyForward [] False


    ,AD "ClearLog" "_Clear Log" Nothing Nothing
        clearLog [] False
    ,AD "ToggleToolbar" "Toggle Toolbar" Nothing Nothing
        toggleToolbar [] False

    ,AD "Configuration" "_Configuration" Nothing Nothing (return ()) [] False
    ,AD "EditCandy" "_To Candy" Nothing Nothing
        editCandy [] True
    ,AD "PrefsEdit" "_Edit Prefs" Nothing Nothing
        (getPrefs Nothing >>= \ p -> displayPane p False) [] False

    ,AD "Help" "_Help" Nothing Nothing (return ()) [] False
    ,AD "HelpDebug" "Debug" Nothing Nothing (do
            pack <- readIDE activePack
            ideMessage Normal (show pack)) [] False
--    ,AD "HelpDebug2" "Debug2" (Just "<Ctrl>d") Nothing dbgInstalledPackageInfo [] False
    ,AD "HelpManual" "Manual" Nothing Nothing (openBrowser "http://leksah.org/leksah_manual.pdf") [] False
    ,AD "HelpHomepage" "Homepage" Nothing Nothing (openBrowser "http://leksah.org") [] False
    ,AD "HelpAbout" "About" Nothing (Just "gtk-about") (liftIO aboutDialog) [] False

    ,AD "BackgroundBuildToggled" "_BackgroundBuild" (Just "Build in the background and report errors") (Just "ide_build")
        backgroundBuildToggled [] True
    ,AD "BackgroundLinkToggled" "_BackgroundLink" (Just "Link in the background") (Just "ide_link")
        backgroundLinkToggled [] True
    ,AD "DebugToggled" "_Debug" (Just "Use GHCi debugger to build and run") (Just "ide_debug")
        debugToggled [] True]

--
-- | The menu description in XML Syntax as defined by GTK
--
menuDescription :: IO String
menuDescription = do
    dataDir     <- getDataDir
    prefsPath   <- getConfigFilePathForLoad "leksah.menu" Nothing dataDir
    res         <- readFile prefsPath
    return res

updateRecentEntries :: IDEAction
updateRecentEntries = do
    recentFiles'       <-  readIDE recentFiles
    recentWorkspaces'  <-  readIDE recentWorkspaces
    recentFilesItem    <-  getRecentFiles
    recentWorkspacesItem <-  getRecentWorkspaces
    reifyIDE (\ ideR -> do
        recentFilesMenu    <-  menuNew
        mapM_ (\s -> do
            fe <- doesFileExist s
            when fe $ do
                mi <- menuItemNewWithLabel s
                mi `onActivateLeaf` (reflectIDE (fileOpenThis s) ideR)
                menuShellAppend recentFilesMenu mi) recentFiles'
        oldSubmenu <- menuItemGetSubmenu recentFilesItem
        when (isJust oldSubmenu) $ do
            widgetHideAll (fromJust oldSubmenu)
            widgetDestroy (fromJust oldSubmenu)
        menuItemSetSubmenu recentFilesItem recentFilesMenu
        widgetShowAll recentFilesMenu
        recentWorkspacesMenu    <-  menuNew
        mapM_ (\s -> do
            fe <- doesFileExist s
            when fe $ do
                mi <- menuItemNewWithLabel s
                mi `onActivateLeaf` (reflectIDE (workspaceOpenThis True (Just s) >> return ()) ideR)
                menuShellAppend recentWorkspacesMenu mi) recentWorkspaces'
        oldSubmenu <- menuItemGetSubmenu recentWorkspacesItem
        when (isJust oldSubmenu) $ do
            widgetHideAll (fromJust oldSubmenu)
            widgetDestroy (fromJust oldSubmenu)
        menuItemSetSubmenu recentWorkspacesItem recentWorkspacesMenu
        widgetShowAll recentWorkspacesMenu)


--
-- | Building the Menu
--
makeMenu :: UIManager -> [ActionDescr IDERef] -> String -> IDEAction
makeMenu uiManager actions menuDescription = reifyIDE (\ideR -> do
    actionGroupGlobal <- actionGroupNew "global"
    mapM_ (actm ideR actionGroupGlobal) actions
    uiManagerInsertActionGroup uiManager actionGroupGlobal 1
    uiManagerAddUiFromString uiManager menuDescription
    return ())
    where
        actm ideR ag (AD name label tooltip stockId ideAction accs isToggle) = do
            let (acc,accString) = if null accs
                                    then (Just "","=" ++ name)
                                    else (Just (head accs),(head accs) ++ "=" ++ name)
            if isToggle
                then do
                    act <- toggleActionNew name label tooltip stockId
                    on act actionToggled (doAction ideAction ideR accString)
                    actionGroupAddActionWithAccel ag act acc
                else do
                    act <- actionNew name label tooltip stockId
                    onActionActivate act (doAction ideAction ideR  accString)
                    actionGroupAddActionWithAccel ag act acc
        doAction ideAction ideR accStr =
            (reflectIDE (do
                ideAction
                triggerEventIDE (StatusbarChanged [CompartmentCommand accStr])
                return ()) ideR)

-- getMenuAndToolbars :: UIManager -> IO (AccelGroup, MenuBar, Toolbar)
getMenuAndToolbars uiManager = do
    accGroup <- uiManagerGetAccelGroup uiManager
    mbMenu   <- uiManagerGetWidget uiManager "/ui/menubar"
    let menu = case mbMenu of
                    Just it -> castToMenuBar it
                    Nothing -> throwIDE "Menu>>makeMenu: failed to create menubar"
    mbToolbar <- uiManagerGetWidget uiManager "/ui/toolbar"
    let toolbar = case mbToolbar of
                    Just it -> castToToolbar it
                    Nothing -> throwIDE "Menu>>makeMenu: failed to create toolbar"
    toolbarSetIconSize toolbar IconSizeSmallToolbar
    toolbarSetStyle toolbar ToolbarIcons
    widgetSetSizeRequest toolbar 700 (-1)
    return (accGroup,menu,toolbar)

textPopupMenu :: IDERef -> Menu -> IO ()
textPopupMenu ideR menu = do
    items <- containerGetChildren menu
    mi1 <- menuItemNewWithLabel "Eval"
    mi1 `onActivateLeaf` (reflectIDE debugExecuteSelection ideR)
    menuShellAppend menu mi1
    mi11 <- menuItemNewWithLabel "Eval & Insert"
    mi11 `onActivateLeaf` (reflectIDE debugExecuteAndShowSelection ideR)
    menuShellAppend menu mi11
    mi12 <- menuItemNewWithLabel "Step"
    mi12 `onActivateLeaf` (reflectIDE debugStepExpression ideR)
    menuShellAppend menu mi12
    mi13 <- menuItemNewWithLabel "Trace"
    mi13 `onActivateLeaf` (reflectIDE debugTraceExpression ideR)
    menuShellAppend menu mi13
    mi16 <- menuItemNewWithLabel "Set Breakpoint"
    mi16 `onActivateLeaf` (reflectIDE debugSetBreakpoint ideR)
    menuShellAppend menu mi16
    sep1 <- separatorMenuItemNew
    menuShellAppend menu sep1
    mi14 <- menuItemNewWithLabel "Type"
    mi14 `onActivateLeaf` (reflectIDE debugType ideR)
    menuShellAppend menu mi14
    mi141 <- menuItemNewWithLabel "Info"
    mi141 `onActivateLeaf` (reflectIDE debugInformation ideR)
    menuShellAppend menu mi141
    mi15 <- menuItemNewWithLabel "Kind"
    mi15 `onActivateLeaf` (reflectIDE debugKind ideR)
    menuShellAppend menu mi15
    sep2 <- separatorMenuItemNew
    menuShellAppend menu sep2
    mi2 <- menuItemNewWithLabel "Find (text)"
    mi2 `onActivateLeaf` (reflectIDE (editFindInc Initial) ideR)
    menuShellAppend menu mi2
    mi3 <- menuItemNewWithLabel "Search (metadata)"
    mi3 `onActivateLeaf` (reflectIDE (do
        mbtext <- selectedText
        case mbtext of
            Just t  -> searchMetaGUI t
            Nothing -> ideMessage  Normal "Select a text first") ideR)
    menuShellAppend menu mi3
    let interpretingEntries = [castToWidget mi16]
    let interpretingSelEntries = [castToWidget mi1, castToWidget mi11, castToWidget mi12,
                                castToWidget mi13, castToWidget mi14, castToWidget mi141,
                                castToWidget mi15]
    let otherEntries = [castToWidget mi2, castToWidget mi3]
    isInterpreting' <- (reflectIDE isInterpreting ideR)
    selected <- (reflectIDE selectedText ideR)
    unless isInterpreting'
        $ mapM_ (\w -> widgetSetSensitive w False) (interpretingEntries ++ interpretingSelEntries)
--    unless (isJust selected)
--        $ mapM_ (\w -> widgetSetSensitive w False) (otherEntries ++ interpretingSelEntries)
    mapM_ widgetShow interpretingEntries
    mapM_ widgetShow interpretingSelEntries
    mapM_ widgetShow (castToWidget sep1 : castToWidget sep2 : otherEntries)
    mapM_ widgetHide $ take 2 (reverse items)

-- | Quit ide
quit :: IDEAction
quit = do
    modifyIDE_ (\ide -> ide{currentState = IsShuttingDown})
    saveSession :: IDEAction
    b <- fileCloseAll (\_ -> return True)
    if b
        then liftIO mainQuit
        else modifyIDE_ (\ide -> ide{currentState = IsRunning})

--
-- | Show the about dialog
--
aboutDialog :: IO ()
aboutDialog = do
    d <- aboutDialogNew
    aboutDialogSetName d "Leksah"
    aboutDialogSetVersion d (showVersion version)
    aboutDialogSetCopyright d "Copyright 2007-2010 Jürgen Nicklisch-Franken, Hamish Mackenzie"
    aboutDialogSetComments d $ "An integrated development environement (IDE) for the " ++
                               "programming language Haskell and the Glasgow Haskell Compiler"
    dd <- getDataDir
    license <- catch (readFile $ dd </> "LICENSE") (\ (_ :: SomeException) -> return "")
    aboutDialogSetLicense d $ Just license
    aboutDialogSetWebsite d "http://leksah.org/"
    aboutDialogSetAuthors d ["Jürgen Nicklisch-Franken","Hamish Mackenzie"]
    dialogRun d
    widgetDestroy d
    return ()


newIcons :: IO ()
newIcons = catch (do
        iconFactory <- iconFactoryNew
        dataDir <- getDataDir
        mapM_ (loadIcon dataDir iconFactory) ["ide_class","ide_configure","ide_data","ide_error_next",
            "ide_error_prev","ide_function","ide_instance", "ide_konstructor","ide_make",
            "ide_method","ide_newtype","ide_other","ide_rule","ide_run","ide_slot",
            "ide_source","ide_type","leksah", "ide_reexported", "ide_clean", "ide_link", "ide_build",
            "ide_debug", "ide_step", "ide_local", "ide_module", "ide_continue", "ide_rebuild_meta",
            "ide_empty","ide_source_local"]
        iconFactoryAddDefault iconFactory)
    (\(e :: SomeException) -> getDataDir >>= \dataDir -> throwIDE ("Can't load icons from " ++ dataDir ++ " " ++ show e))
    where
    loadIcon dataDir iconFactory name = do
        pb      <-  pixbufNewFromFile $ dataDir </> "pics" </> (name ++ ".png")
        icon    <-  iconSetNewFromPixbuf pb
        iconFactoryAdd iconFactory name icon

setSensitivity :: [(SensitivityMask, Bool)] -> IDEAction
setSensitivity l = mapM_ setSensitivitySingle l
    where   setSensitivitySingle (sens,bool) = do
                actions <- getActionsFor sens
                liftIO $ mapM_ (\a -> actionSetSensitive a bool) actions
                let additionalActions = getAdditionalActionsFor sens
                mapM_ (\a -> a bool) additionalActions

getActionsFor :: SensitivityMask -> IDEM [Action]
getActionsFor SensitivityForwardHist = getActionsFor' ["ViewHistoryForth"]
getActionsFor SensitivityBackwardHist = getActionsFor' ["ViewHistoryBack"]
getActionsFor SensitivityProjectActive = getActionsFor'
    ["EditPackage", "PackageFlags", "ConfigPackage", "BuildPackage"
    ,"DocPackage", "CleanPackage", "CopyPackage", "RunPackage","InstallPackage"
    ,"RegisterPackage", "UnregisterPackage","TestPackage","SdistPackage"
    ,"OpenDocPackage","FileCloseAll"]
getActionsFor SensitivityError = getActionsFor' ["NextError", "PreviousError"]
getActionsFor SensitivityEditor = getActionsFor' ["EditUndo", "EditRedo", "EditGotoLine"
    ,"EditComment", "EditUncomment", "EditShiftLeft", "EditShiftRight"]
getActionsFor SensitivityInterpreting = getActionsFor' ["QuitDebugger" , "ExecuteSelection",
 "DebugSetBreakpoint", "ShowNextBreakpoint", "ShowPreviousBreakpoint", "DebugDeleteAllBreakpoints",
 "DebugContinue", "DebugAbandon", "DebugStop", "DebugStep", "DebugStepExpression", "DebugStepLocal",
 "DebugStepModule", "DebugTrace", "DebugTraceExpression", "DebugHistory", "DebugBack", "DebugForward",
 "DebugForce", "DebugPrint", "DebugSimplePrint", "ShowBindings", "ShowBreakpoints", "ShowContext",
 "ShowLoadedModules", "ShowPackages", "ShowLanguages", "DebugInformation", "DebugKind", "DebugType"]
getActionsFor SensitivityWorkspaceOpen = return [] --TODO add here

getActionsFor' :: [String] -> IDEM[Action]
getActionsFor' l = do
    r <- mapM getActionFor l
    return (catMaybes r)
    where
        getActionFor string = do
            uiManager' <- getUiManager
            actionGroups <- liftIO $ uiManagerGetActionGroups uiManager'
            res <- liftIO $ actionGroupGetAction (head actionGroups) string
            when (isNothing res) $ ideMessage Normal $ "Can't find UI Action " ++ string
            return res

getAdditionalActionsFor :: SensitivityMask -> [Bool -> IDEAction]
getAdditionalActionsFor SensitivityInterpreting = [setSensitivityDebugger]
getAdditionalActionsFor _ = []

viewDetachInstrumented :: IDEAction
viewDetachInstrumented = do
    mbPair <- viewDetach
    case mbPair of
        Nothing     -> return ()
        Just (win,wid) -> do
            instrumentSecWindow win
            liftIO $ widgetShowAll win

instrumentWindow :: Window -> Prefs -> Widget -> IDEAction
instrumentWindow win prefs topWidget = do
    -- sets the icon
    ideR <- ask
    uiManager' <- getUiManager
    liftIO $ do
        dataDir <- getDataDir
        let iconPath = dataDir </> "pics" </> "leksah.png"
        iconExists  <-  doesFileExist iconPath
        when iconExists $
            windowSetIconFromFile win iconPath
        vb <- vBoxNew False 1  -- Top-level vbox
        widgetSetName vb "topBox"
        (acc,menu,toolbar) <-  getMenuAndToolbars uiManager'
        boxPackStart vb menu PackNatural 0
        boxPackStart vb toolbar PackNatural 0
        boxPackStart vb topWidget PackGrow 0
        findbar <- reflectIDE (do
            modifyIDE_ (\ide -> ide{toolbar = (True,Just toolbar)})
            constructFindReplace ) ideR
        boxPackStart vb findbar PackNatural 0
        statusBar   <-  buildStatusbar
        boxPackEnd vb statusBar PackNatural 0
        win `onKeyPress` (\ e -> reflectIDE (handleSpecialKeystrokes e) ideR)
        windowAddAccelGroup win acc
        containerAdd win vb
        reflectIDE (do
            setCandyState (isJust (sourceCandy prefs))
            setBackgroundBuildToggled (backgroundBuild prefs)
            setBackgroundLinkToggled (backgroundLink prefs)) ideR

instrumentSecWindow :: Window -> IDEAction
instrumentSecWindow win = do
    ideR <- ask
    uiManager' <- getUiManager
    liftIO $ do
        dataDir <- getDataDir
        let iconPath = dataDir </> "data" </> "leksah.png"
        iconExists  <-  doesFileExist iconPath
        when iconExists $
            windowSetIconFromFile win iconPath

        (acc,_,_) <-  getMenuAndToolbars uiManager'
        windowAddAccelGroup win acc
        win `onKeyPress` (\ e -> reflectIDE (handleSpecialKeystrokes e) ideR)
        return ()

--
-- | Callback function for onKeyPress of the main window, so 'preprocess' any key
--
handleSpecialKeystrokes :: GdkEvents.Event -> IDEM Bool
handleSpecialKeystrokes (Key { eventKeyName = name,  eventModifier = mods,
                                eventKeyVal = keyVal, eventKeyChar = mbChar}) = do
    prefs' <- readIDE prefs
    case (name, mods) of
        (tab, [Control]) | (tab == "Tab" || tab == "ISO_Left_Tab")
                                && useCtrlTabFlipping prefs'      -> do
            flipDown
            return True
        (tab, [Shift, Control]) | (tab == "Tab" || tab == "ISO_Left_Tab")
                                && useCtrlTabFlipping prefs'      -> do
            flipUp
            return True
        _                                                            -> do
                bs <- getCandyState
                when bs (editKeystrokeCandy mbChar)
                sk  <- readIDE specialKey
                sks <- readIDE specialKeys
                return True
                case sk of
                    Nothing ->
                        case Map.lookup (keyVal,sort mods) sks of
                            Nothing -> do
                                triggerEventIDE (StatusbarChanged [CompartmentCommand ""])
                                return False
                            Just map -> do
                                let sym = printMods mods ++ name
                                triggerEventIDE (StatusbarChanged [CompartmentCommand sym])
                                modifyIDE_ (\ide -> ide{specialKey = Just (map,sym)})
                                return True
                    Just (map,sym) -> do
                        case Map.lookup (keyVal,sort mods) map of
                            Nothing -> do
                                triggerEventIDE (StatusbarChanged [CompartmentCommand
                                    (sym ++ printMods mods ++ name ++ "?")])
                                return ()
                            Just (AD actname _ _ _ ideAction _ _) -> do
                                triggerEventIDE (StatusbarChanged [CompartmentCommand
                                    (sym ++ " " ++ printMods mods ++ name ++ "=" ++ actname)])
                                ideAction
                        modifyIDE_ (\ide -> ide{specialKey = Nothing})
                        return True
    where
    printMods :: [Modifier] -> String
    printMods []    = ""
    printMods (m:r) = show m ++ printMods r
handleSpecialKeystrokes _ = return True

setSymbol :: String -> IDEAction
setSymbol symbol = do
    currentInfo' <- getWorkspaceInfo
    case currentInfo' of
        Nothing -> return ()
        Just ((GenScopeC (PackScope _ symbolTable1)),(GenScopeC (PackScope _ symbolTable2))) ->
            case filter (not . isReexported) (getIdentifierDescr symbol symbolTable1 symbolTable2) of
                []     -> return ()
                a:[]   -> selectIdentifier a
                l      -> setChoices l

--
-- | Register handlers for IDE events
--
registerEvents :: IDEAction
registerEvents =    do
    stRef   <-  ask
    registerEvent stRef "LogMessage"
        (Left (\e@(LogMessage s t)      -> getLog >>= \(log :: IDELog) -> liftIO $ appendLog log s t
                                                >> return e))
    registerEvent stRef "SelectInfo"
        (Left (\ e@(SelectInfo str)     -> setSymbol str >> return e))
    registerEvent stRef "SelectIdent"
        (Left (\ e@(SelectIdent id)     -> selectIdentifier id >> return e))
    registerEvent stRef "InfoChanged"
        (Left (\ e@(InfoChanged b)      -> reloadKeepSelection b >> return e))
    registerEvent stRef "UpdateWorkspaceInfo"
        (Left (\ e@UpdateWorkspaceInfo  -> updateWorkspaceInfo >> return e))
    registerEvent stRef "WorkspaceChanged"
        (Left (\ e@(WorkspaceChanged showPane updateFileCache)
                                        -> updateWorkspace showPane updateFileCache >> return e))
    registerEvent stRef "RecordHistory"
        (Left (\ rh@(RecordHistory h)   -> recordHistory h >> return rh))
    registerEvent stRef "Sensitivity"
        (Left (\ s@(Sensitivity h)      -> setSensitivity h >> return s))
    registerEvent stRef "SearchMeta"
        (Left (\ e@(SearchMeta string)  -> searchMetaGUI string >> return e))
    registerEvent stRef "LoadSession"
        (Left (\ e@(LoadSession fp)     -> loadSession fp >> return e))
    registerEvent stRef "SaveSession"
        (Left (\ e@(SaveSession fp)     -> saveSessionAs fp Nothing >> return e))
    registerEvent stRef "UpdateRecent"
        (Left (\ e@UpdateRecent         -> updateRecentEntries >> return e))
    registerEvent stRef "VariablesChanged"
        (Left (\ e@VariablesChanged     -> fillVariablesList >> return e))
    registerEvent stRef "ErrorChanged"
        (Left (\ e@ErrorChanged         -> postAsyncIDE fillErrorList >> return e))
    registerEvent stRef "CurrentErrorChanged"
        (Left (\ e@(CurrentErrorChanged mbLogRef) -> postAsyncIDE (do
            selectRef mbLogRef
            selectError mbLogRef)  >> return e))
    registerEvent stRef "BreakpointChanged"
        (Left (\ e@BreakpointChanged    -> postAsyncIDE fillBreakpointList >> return e))
    registerEvent stRef "CurrentBreakChanged"
        (Left (\ e@(CurrentBreakChanged mbLogRef) -> postAsyncIDE (do
            selectRef mbLogRef
            selectBreak mbLogRef) >> return e))
    registerEvent stRef "TraceChanged"
        (Left (\ e@TraceChanged         -> fillTraceList >> return e))
    registerEvent stRef "GetTextPopup"
        (Left (\ e@(GetTextPopup _)     -> return (GetTextPopup (Just textPopupMenu))))
    registerEvent stRef "StatusbarChanged"
        (Left (\ e@(StatusbarChanged args)
                                        -> changeStatusbar args >> return e))
    return ()


