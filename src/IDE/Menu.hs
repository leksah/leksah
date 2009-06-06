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


module IDE.Menu (
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
import Graphics.UI.Gtk.Types
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
import IDE.SaveSession
import IDE.Pane.Modules
import IDE.Find
import IDE.FileUtils
import IDE.Pane.ClassHierarchy
import IDE.Pane.Search
import IDE.Pane.References
import Paths_leksah
import IDE.GUIHistory
import IDE.Metainfo.Provider
    (infoForActivePackage, rebuildLibInfo, rebuildActiveInfo)
import IDE.Pane.Info (setSymbol, showInfo)
import IDE.NotebookFlipper
import IDE.ImportTool (addAllImports)
import IDE.LogRef
import IDE.Debug
import System.Directory (doesFileExist)
import qualified Graphics.UI.Gtk.Gdk.Events as GdkEvents
import Graphics.UI.Gtk.Gdk.Events
    (Modifier(..),
     Event(..))
import Graphics.UI.Gtk.Gdk.Enums (Modifier(..))
import qualified Data.Map as  Map (lookup)
import Data.List (sort)
import Control.Event (registerEvent)
import Paths_leksah
import IDE.Pane.Breakpoints (showBreakpointList, fillBreakpointList, selectBreak)
import Debug.Trace (trace)

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
        (do fileSaveAll; return ()) [] False
    ,AD "FileClose" "_Close" Nothing (Just "gtk-close")
        (do fileClose; return ()) [] False
    ,AD "FileCloseAll" "Close All" Nothing Nothing
        (do fileCloseAll; return ()) [] False
    ,AD "FileCloseAllButPackage" "Close All But Package" Nothing Nothing
        (do fileCloseAllButPackage; return ()) [] False
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

    ,AD "EditCandy" "_To Candy" Nothing Nothing
        editCandy [] True

    ,AD "Package" "_Package" Nothing Nothing (return ()) [] False
    ,AD "NewPackage" "_New Package" Nothing Nothing
        packageNew [] False
    ,AD "OpenPackage" "_Open Package" Nothing Nothing
        packageOpen [] False
    ,AD "RecentPackages" "_Recent Packages" Nothing Nothing (return ()) [] False
    ,AD "EditPackage" "_Edit Package" Nothing Nothing
        packageEdit [] False
    ,AD "ClosePackage" "_Close Package" Nothing Nothing
        deactivatePackage [] False

    ,AD "PackageFlags" "Edit Flags" (Just "Edit the package flags") Nothing
        (do getFlags; return ()) [] False
    ,AD "ConfigPackage" "_Configure Package" (Just "Configures the package") (Just "ide_configure")
        packageConfig [] False
    ,AD "BuildPackage" "_Build Package" (Just "Builds the package") (Just "ide_make")
        (packageBuild False) [] False
    ,AD "DocPackage" "_Build Documentation" (Just "Builds the documentation") Nothing
        packageDoc [] False
    ,AD "CleanPackage" "Cl_ean Package" (Just "Cleans the package") (Just "ide_clean")
        packageClean [] False
    ,AD "CopyPackage" "_Copy Package" (Just "Copies the package") Nothing
        packageCopy [] False
    ,AD "RunPackage" "_Run" (Just "Runs the package") (Just "ide_run")
        packageRun [] False
    ,AD "NextError" "_Next Error" (Just "Go to the next error") (Just "ide_error_next")
        nextError [] False
    ,AD "PreviousError" "_Previous Error" (Just "Go to the previous error") (Just "ide_error_prev")
        previousError [] False
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
    ,AD "ShowErrorsList" "Show Errors" Nothing Nothing
        showErrors [] False
    ,AD "ShowBreakpointsList" "Show Breakpoints List" Nothing Nothing
        (trace "1" showBreakpointList) [] False
    ,AD "ShowVariablesList" "Show Variables List" Nothing Nothing
        {--showVariables--} undefined [] False
    ,AD "ShowTracesList" "Show Traces List" Nothing Nothing
        {--showTraces--} undefined [] False
    ,AD "ShowEval" "Show Eval" Nothing Nothing
        {--showTrace--} undefined [] False
    ,AD "ShowDebugger" "Show Debugger" Nothing Nothing
        {--showTrace--} undefined [] False

    ,AD "DebugSetBreakpoint" "Set Breakpoint" (Just "Set a breakpoint on the selected name or current line") Nothing
        debugSetBreakpoint [] False
    ,AD "ShowNextBreakpoint" "Show Next Breakpoint" (Just "Show the next breakpoint") Nothing
        nextBreakpoint [] False
    ,AD "ShowPreviousBreakpoint" "Show Previous Breakpoint" (Just "Show the previous breakpoint") Nothing
        previousBreakpoint [] False
    ,AD "DebugDeleteAllBreakpoints" "Delete All Breakpoints" (Just "") Nothing
        debugDeleteAllBreakpoints [] False

    ,AD "DebugContinue" "Continue" (Just "Resume after a breakpoint") Nothing
        debugContinue [] False
    ,AD "DebugAbandon" "Abandon" (Just "At a breakpoint, abandon current computation") Nothing
        debugAbandon [] False
    ,AD "DebugStop" "Stop" (Just "Interrupt the running operation.") Nothing
        debugStop [] False

    ,AD "DebugStep" "Step" (Just "Single-step after stopping at a breakpoint") Nothing
        debugStep [] False
    ,AD "DebugStepExpression" "Step Expression" (Just "Single-step into selected expression") Nothing
        debugStepExpression [] False
    ,AD "DebugStepLocal" "Step Local" (Just "Single-step within the current top-level binding") Nothing
        debugStepLocal [] False
    ,AD "DebugStepModule" "Step Module" (Just "Single-step restricted to the current module") Nothing
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
    ,AD "UpdateMetadataCurrent" "_Update Project" Nothing Nothing
        rebuildActiveInfo [] False
    ,AD "UpdateMetadataLib" "_Update Lib" Nothing Nothing
        rebuildLibInfo [] False
    ,AD "ShowModules" "Show Modules" Nothing Nothing
        showModules [] False
    ,AD "ShowReferences" "Show References" Nothing Nothing
        showReferences [] False
    ,AD "ShowClasses" "Show Classes" Nothing Nothing
        showClasses [] False
    ,AD "ShowSearch" "Show Search" Nothing Nothing
        showSearch [] False
    ,AD "ShowInfo" "Show Info" Nothing Nothing
        showInfo [] False

    ,AD "Session" "_Session" Nothing Nothing (return ()) [] False
    ,AD "SaveSession" "_Save Session" Nothing Nothing
        saveSessionAsPrompt [] False
    ,AD "LoadSession" "_Load Session" Nothing Nothing
        loadSessionPrompt [] False
    ,AD "ForgetSession" "_Forget Session" Nothing Nothing
        (return ()) [] True

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

    ,AD "Help" "_Help" Nothing Nothing (return ()) [] False
    ,AD "PrefsEdit" "_Edit Prefs" Nothing Nothing
        editPrefs [] False
    ,AD "HelpDebug" "Debug" Nothing Nothing (do
            pack <- readIDE activePack
            ideMessage Normal (show pack)) [] False
--    ,AD "HelpDebug2" "Debug2" (Just "<Ctrl>d") Nothing dbgInstalledPackageInfo [] False
    ,AD "HelpAbout" "About" Nothing (Just "gtk-about") aboutDialog [] False

    ,AD "BackgroundBuildToggled" "_BackgroundBuild" (Just "Build in the background and report errors") (Just "ide_build")
        backgroundBuildToggled [] True
    ,AD "BackgroundLinkToggled" "_BackgroundLink" (Just "Link in the background") (Just "ide_link")
        backgroundLinkToggled [] True
    ,AD "DebugToggled" "_Debug" (Just "Use GHCi debuggr to build and run") (Just "ide_build")
        debugToggled [] True]

--
-- | The menu description in XML Syntax as defined by GTK
--
menuDescription :: IO String
menuDescription = do
    prefsPath   <-  getConfigFilePathForLoad "Default.menu"
    res         <-  readFile prefsPath
    return res

updateRecentEntries :: IDEAction
updateRecentEntries = do
    recentFiles'       <-  readIDE recentFiles
    recentPackages'    <-  readIDE recentPackages
    recentFilesItem    <-  getRecentFiles
    recentPackagesItem <-  getRecentPackages
    reifyIDE (\ ideR -> do
        recentFilesMenu    <-  menuNew
        mapM_ (\s -> do
            mi <- menuItemNewWithLabel s
            mi `onActivateLeaf` (reflectIDE (fileOpenThis s) ideR)
            menuShellAppend recentFilesMenu mi) recentFiles'
        oldSubmenu <- menuItemGetSubmenu recentFilesItem
        when (isJust oldSubmenu) $ do
            widgetHideAll (fromJust oldSubmenu)
            widgetDestroy (fromJust oldSubmenu)
        menuItemSetSubmenu recentFilesItem recentFilesMenu
        widgetShowAll recentFilesMenu
        recentPackagesMenu    <-  menuNew
        mapM_ (\s -> do
            mi <- menuItemNewWithLabel s
            mi `onActivateLeaf` (reflectIDE (packageOpenThis (Just s) >> return ()) ideR)
            menuShellAppend recentPackagesMenu mi) recentPackages'
        oldSubmenu <- menuItemGetSubmenu recentPackagesItem
        when (isJust oldSubmenu) $ do
            widgetHideAll (fromJust oldSubmenu)
            widgetDestroy (fromJust oldSubmenu)
        menuItemSetSubmenu recentPackagesItem recentPackagesMenu
        widgetShowAll recentPackagesMenu)


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
            handleTopExceptions (reflectIDE (do
                ideAction
                sb <- getSBSpecialKeys
                liftIO $statusbarPop sb 1
                liftIO $statusbarPush sb 1 $accStr
                return ()) ideR)

getMenu :: UIManager -> IO (AccelGroup, [Widget])
getMenu uiManager = do
    accGroup <- uiManagerGetAccelGroup uiManager
    mbWidgets <- mapM (uiManagerGetWidget uiManager) ["ui/menubar","ui/toolbar"]
    let widgets = map (\mb -> case mb of
					Just it -> it
					Nothing -> throwIDE "Menu>>makeMenu: failed to build menu") mbWidgets
    return (accGroup,widgets)

-- | Quit ide
--  ### make reasonable
--
quit :: IDEAction
quit = do
    modifyIDE_ (\ide -> return (ide{currentState = IsShuttingDown}))
    saveSession :: IDEAction
    b <- fileCloseAll
    if b
        then liftIO mainQuit
        else modifyIDE_ (\ide -> return (ide{currentState = IsRunning}))

--
-- | Show the about dialog
--
aboutDialog :: IDEAction
aboutDialog = liftIO $ do
    d <- aboutDialogNew
    aboutDialogSetName d "Leksah"
    aboutDialogSetVersion d (showVersion version)
    aboutDialogSetCopyright d "Copyright 2007-2009 Jürgen Nicklisch-Franken, Hamish Mackenzie"
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

buildStatusbar ideR = do
    sb <- statusbarNew
    statusbarSetHasResizeGrip sb False

    sblk <- statusbarNew
    widgetSetName sblk "statusBarSpecialKeys"
    statusbarSetHasResizeGrip sblk False
    widgetSetSizeRequest sblk 150 (-1)

    sbap <- statusbarNew
    widgetSetName sbap "statusBarActivePane"
    statusbarSetHasResizeGrip sbap False
    widgetSetSizeRequest sbap 150 (-1)

    sbapr <- statusbarNew
    widgetSetName sbapr "statusBarActiveProject"
    statusbarSetHasResizeGrip sbapr False
    widgetSetSizeRequest sbapr 150 (-1)

    sbe <- statusbarNew
    widgetSetName sbe "statusBarErrors"
    statusbarSetHasResizeGrip sbe False
    widgetSetSizeRequest sbe 150 (-1)

    sblc <- statusbarNew
    widgetSetName sblc "statusBarLineColumn"
    statusbarSetHasResizeGrip sblc True
    widgetSetSizeRequest sblc 150 (-1)

    sbio <- statusbarNew
    widgetSetName sbio "statusBarInsertOverwrite"
    statusbarSetHasResizeGrip sbio False
    widgetSetSizeRequest sbio 60 (-1)

    dummy <- hBoxNew False 1
    widgetSetName dummy "dummyBox"


    hb <- hBoxNew False 1
    widgetSetName hb "statusBox"
    boxPackStart hb sblk PackGrow 0
    boxPackStart hb sbap PackGrow 0
    boxPackStart hb sbapr PackGrow 0
    --boxPackStart hb dummy PackGrow 0
    boxPackEnd hb sblc PackNatural 0
    boxPackEnd hb sbio PackNatural 0
    boxPackEnd hb sbe PackNatural 0

    return hb

newIcons :: IO ()
newIcons =
    catch (do
        iconFactory <- iconFactoryNew
        dataDir <- getDataDir
        mapM_ (loadIcon dataDir iconFactory) ["ide_class","ide_configure","ide_data","ide_error_next",
            "ide_error_prev","ide_field","ide_function","ide_instance", "ide_konstructor","ide_make",
            "ide_method","ide_newtype","ide_other","ide_rule","ide_run","ide_slot",
            "ide_source","ide_type","leksah", "ide_reexported", "ide_clean", "ide_link", "ide_build"
            ]
        iconFactoryAddDefault iconFactory)
    (\(e :: SomeException) -> getDataDir >>= \dataDir -> throwIDE ("Can't load icons from " ++ dataDir))
    where
    loadIcon dataDir iconFactory name = do
        pb      <-  pixbufNewFromFile $ dataDir </> "data" </> (name ++ ".png")
        icon    <-  iconSetNewFromPixbuf pb
        iconFactoryAdd iconFactory name icon

setSensitivity :: [(SensitivityMask, Bool)] -> IDEAction
setSensitivity l = mapM_ setSensitivitySingle l
    where   setSensitivitySingle (sens,bool) = do
                actions <- getActionsFor sens
                liftIO $ mapM_ (\a -> actionSetSensitive a bool) actions

getActionsFor :: SensitivityMask -> IDEM [Action]
getActionsFor SensitivityForwardHist = getActionsFor' ["ViewHistoryForth"]
getActionsFor SensitivityBackwardHist = getActionsFor' ["ViewHistoryBack"]
getActionsFor SensitivityProjectActive = getActionsFor'
    ["EditPackage", "ClosePackage", "PackageFlags", "ConfigPackage", "BuildPackage"
    ,"DocPackage", "CleanPackage", "CopyPackage", "RunPackage","InstallPackage"
    ,"RegisterPackage", "UnregisterPackage","TestPackage","SdistPackage"
    ,"OpenDocPackage","FileCloseAll"]
getActionsFor SensitivityError = getActionsFor' ["NextError", "PreviousError"]
getActionsFor SensitivityEditor = getActionsFor' ["EditUndo", "EditRedo", "EditGotoLine"
    ,"EditComment", "EditUncomment", "EditShiftLeft", "EditShiftRight"]


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
        let iconPath = dataDir </> "data" </> "leksah.png"
        iconExists  <-  doesFileExist iconPath
        when iconExists $
            windowSetIconFromFile win iconPath

        (acc,menus) <-  getMenu uiManager'
        when (length menus /= 2) $ throwIDE ("Failed to build menu" ++ show (length menus))
        let toolbar = castToToolbar (menus !! 1)
        findbar <- reflectIDE (do
            modifyIDE_ (\ide -> return (ide{toolbar = (True,Just toolbar)}))
            constructFindReplace ) ideR
        sep0 <- separatorToolItemNew
        separatorToolItemSetDraw sep0 False
        toolItemSetExpand sep0 True
        toolbarInsert toolbar sep0 (-1)
        closeButton <- toolButtonNewFromStock "gtk-close"
        toolbarInsert toolbar closeButton (-1)
        closeButton `onToolButtonClicked` do
            reflectIDE hideToolbar ideR
        windowAddAccelGroup win acc
        statusBar   <-  buildStatusbar ideR
        vb          <-  vBoxNew False 1  -- Top-level vbox
        widgetSetName vb "topBox"
        toolbarSetStyle (castToToolbar (menus !! 1)) ToolbarIcons
        widgetSetSizeRequest (menus !! 1)  500 (-1)
        boxPackStart vb (menus !! 0) PackNatural 0
        boxPackStart vb (menus !! 1) PackNatural 0
        boxPackStart vb topWidget PackGrow 0
        boxPackStart vb findbar PackNatural 0
        boxPackEnd vb statusBar PackNatural 0
        win `onKeyPress` (\ e -> reflectIDE (handleSpecialKeystrokes e) ideR)
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

        (acc,_) <-  getMenu uiManager'
        windowAddAccelGroup win acc
        win `onKeyPress` (\ e -> reflectIDE (handleSpecialKeystrokes e) ideR)
        return ()

--
-- | Callback function for onKeyPress of the main window, so 'preprocess' any key
--
handleSpecialKeystrokes :: GdkEvents.Event -> IDEM Bool
handleSpecialKeystrokes (Key { eventKeyName = name,  eventModifier = mods,
                                eventKeyVal = keyVal, eventKeyChar = mbChar}) = do
    sb <- getSBSpecialKeys
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
                                liftIO $statusbarPop sb 1
                                liftIO $statusbarPush sb 1 ""
                                return False
                            Just map -> do
                                let sym = printMods mods ++ name
                                liftIO $statusbarPop sb 1
                                liftIO $statusbarPush sb 1 sym
                                modifyIDE_ (\ide -> return (ide{specialKey = Just (map,sym)}))
                                return True
                    Just (map,sym) -> do
                        case Map.lookup (keyVal,sort mods) map of
                            Nothing -> do
                                liftIO $statusbarPop sb 1
                                liftIO $statusbarPush sb 1 $ sym ++ printMods mods ++ name ++ "?"
                                return ()
                            Just (AD actname _ _ _ ideAction _ _) -> do
                                liftIO $statusbarPop sb 1
                                liftIO $statusbarPush sb 1
                                    $ sym ++ " " ++ printMods mods ++ name ++ "=" ++ actname
                                ideAction
                        modifyIDE_ (\ide -> return (ide{specialKey = Nothing}))
                        return True
    where
    printMods :: [Modifier] -> String
    printMods []    = ""
    printMods (m:r) = show m ++ printMods r
handleSpecialKeystrokes _ = return True

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
    registerEvent stRef "CurrentInfo"
        (Left (\ CurrentInfo            -> reloadKeepSelection >> return CurrentInfo))
    registerEvent stRef "ActivePack"
        (Left (\ ActivePack             -> (infoForActivePackage :: IDEAction) >> return ActivePack))
    registerEvent stRef "RecordHistory"
        (Left (\ rh@(RecordHistory h)   -> recordHistory h >> return rh))
    registerEvent stRef "Sensitivity"
        (Left (\ s@(Sensitivity h)      -> setSensitivity h >> return s))
    registerEvent stRef "DescrChoice"
        (Left (\ e@(DescrChoice descrs) -> setChoices descrs >> return e))
    registerEvent stRef "SearchMeta"
        (Left (\ e@(SearchMeta string)  -> searchMetaGUI string >> return e))
    registerEvent stRef "LoadSession"
        (Left (\ e@(LoadSession fp)     -> loadSession fp >> return e))
    registerEvent stRef "SaveSession"
        (Left (\ e@(SaveSession fp)     -> saveSessionAs fp >> return e))
    registerEvent stRef "UpdateRecent"
        (Left (\ e@UpdateRecent         -> updateRecentEntries >> return e))
--    registerEvent stRef "DebuggerChanged"
--        (Left (\ e@DebuggerChanged      -> updateDebugger >> return e))
    registerEvent stRef "ErrorChanged"
        (Left (\ e@ErrorChanged         -> reifyIDE (\ideR ->
            postGUIAsync (reflectIDE fillErrorList ideR)) >> return e))
    registerEvent stRef "CurrentErrorChanged"
        (Left (\ e@(CurrentErrorChanged mbLogRef) -> reifyIDE (\ideR ->
            postGUIAsync (reflectIDE (selectRef mbLogRef) ideR) >>
            postGUIAsync (reflectIDE (selectError mbLogRef) ideR)) >> return e))
    registerEvent stRef "BreakpointChanged"
        (Left (\ e@BreakpointChanged    -> reifyIDE (\ideR ->
            postGUIAsync (reflectIDE fillBreakpointList ideR)) >> return e))
    registerEvent stRef "CurrentBreakChanged"
        (Left (\ e@(CurrentBreakChanged mbLogRef) -> reifyIDE (\ideR ->
            postGUIAsync (reflectIDE (selectRef mbLogRef) ideR) >>
            postGUIAsync (reflectIDE (selectBreak mbLogRef) ideR)) >> return e))
    return ()


