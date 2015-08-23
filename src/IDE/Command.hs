{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
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
,   canQuit
,   quit
,   aboutDialog
,   buildStatusbar
,   newIcons
,   setSensitivity
,   updateRecentEntries
,   handleSpecialKeystrokes
,   registerLeksahEvents
,   instrumentWindow
,   instrumentSecWindow
) where

import Graphics.UI.Gtk
       (toToolbar, ToolbarClass, Toolbar(..), keyToChar, eventKeyVal,
        eventModifier, eventKeyName, EKey, containerAdd,
        windowAddAccelGroup, keyPressEvent, boxPackEnd, boxPackStart,
        widgetSetName, vBoxNew, windowSetIconFromFile, Widget, Window,
        actionGroupGetAction, uiManagerGetActionGroups, Action,
        actionSetSensitive, iconFactoryAdd, iconSetNewFromPixbuf,
        pixbufNewFromFile, iconFactoryAddDefault, iconFactoryNew,
        dialogRun, aboutDialogAuthors, aboutDialogWebsite,
        aboutDialogLicense, aboutDialogComments, aboutDialogCopyright,
        aboutDialogVersion, aboutDialogName, aboutDialogNew, mainQuit,
        widgetHide, widgetShow, castToWidget, separatorMenuItemNew,
        containerGetChildren, Menu, widgetSetSizeRequest, toolbarSetStyle,
        set, AttrOp(..), castToToolbar, castToMenuBar, uiManagerGetWidget,
        uiManagerGetAccelGroup, actionActivated, actionNew,
        actionGroupAddActionWithAccel, actionToggled, toggleActionNew,
        uiManagerAddUiFromString, uiManagerInsertActionGroup,
        actionGroupNew, UIManager, widgetShowAll, menuItemSetSubmenu,
        widgetDestroy, menuItemGetSubmenu, menuShellAppend,
        menuItemActivate, menuItemNewWithLabel, menuNew, Packing(..),
        ToolbarStyle(..), PositionType(..), on, IconSize(..), Modifier(..))
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
import IDE.HLint
import IDE.Sandbox
import IDE.Pane.Log
import IDE.Pane.Modules
import IDE.Find
import IDE.Pane.Info
import IDE.Utils.FileUtils
import IDE.Utils.GUIUtils
import IDE.GUIHistory
import IDE.Metainfo.Provider
       (getWorkspaceInfo, getIdentifierDescr, updateWorkspaceInfo,
        updateSystemInfo, rebuildSystemInfo, rebuildWorkspaceInfo)
import IDE.NotebookFlipper
import IDE.ImportTool (resolveErrors)
import IDE.LogRef
import IDE.Debug
import System.Directory (doesFileExist)
import Graphics.UI.Gtk.Gdk.EventM (EventM)
import qualified Data.Map as  Map (lookup)
import Data.List (sort)
import Control.Event (registerEvent)
import IDE.Pane.Breakpoints
       (showBreakpoints, fillBreakpointList, selectBreak)
import IDE.Workspaces
import IDE.Statusbar
import IDE.Pane.Workspace
import IDE.Pane.Variables (showVariables, fillVariablesListQuiet)
import IDE.Pane.Trace (showTrace, fillTraceList)
import IDE.PaneGroups
import IDE.Pane.Search (getSearch, IDESearch(..))
import IDE.Pane.Grep (getGrep)
import IDE.Pane.WebKit.Documentation (getDocumentation)
import IDE.Pane.WebKit.Output (getOutputPane)
import IDE.Pane.WebKit.Inspect (getInspectPane)
import IDE.Pane.Files (refreshFiles, getFiles)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (unless, when)
import Control.Monad.Trans.Reader (ask)
import System.Log.Logger (debugM)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr(..))
import Foreign.ForeignPtr (withForeignPtr)
import Graphics.UI.GtkInternals (unToolbar)
import IDE.Session
       (saveSessionAs, loadSession, saveSession, sessionClosePane,
        loadSessionPrompt, saveSessionAsPrompt, viewFullScreen, viewDark)
import qualified Data.Text as T (unpack, pack)
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
import Data.Monoid (Monoid(..), (<>))
import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)

printf :: PrintfType r => Text -> r
printf = S.printf . T.unpack

foreign import ccall safe "gtk_toolbar_set_icon_size"
  gtk_toolbar_set_icon_size :: Ptr Toolbar -> CInt -> IO ()

toolbarSetIconSize :: ToolbarClass self => self -> IconSize -> IO ()
toolbarSetIconSize self iconSize =
  withForeignPtr (unToolbar $ toToolbar self) $
    \selfPtr ->gtk_toolbar_set_icon_size selfPtr (fromIntegral $ fromEnum iconSize)

--
-- | The Actions known to the system (they can be activated by keystrokes or menus)
--
mkActions :: [ActionDescr IDERef]
mkActions =
    [
    AD "vcs" (__ "Version Con_trol") Nothing Nothing (return ()) [] False
    ,AD "FilePrint" (__ "_Print File") Nothing Nothing filePrint [] False
    ,AD "File" (__ "_File") Nothing Nothing (return ()) [] False
    ,AD "FileNew" (__ "_New Module...") Nothing (Just "gtk-new")
        (packageTry $ addModule []) [] False
    ,AD "FileNewTextFile" (__ "_New Text File") Nothing Nothing
        fileNew [] False
    ,AD "FileOpen" (__ "_Open...") Nothing (Just "gtk-open")
        fileOpen [] False
    ,AD "RecentFiles" (__ "Open _Recent") Nothing Nothing (return ()) [] False
    ,AD "FileRevert" (__ "_Revert") Nothing Nothing
        fileRevert [] False
    ,AD "FileSave" (__ "_Save") Nothing (Just "gtk-save")
        (do fileSave False; return ()) [] False
    ,AD "FileSaveAs" (__ "Save _As...") Nothing (Just "gtk-save-as")
        (do fileSave True; return ()) [] False
    ,AD "FileSaveAll" (__ "Save A_ll") Nothing Nothing
        (do fileSaveAll (\ b -> return (bufferName b /= "_Eval.hs")); return ()) [] False
    ,AD "FileClose" (__ "_Close") Nothing (Just "gtk-close")
        (do fileClose; return ()) [] False
    ,AD "FileCloseAll" (__ "Close All") Nothing Nothing
        (do fileCloseAll (\ b -> return (bufferName b /= "_Eval.hs")); return ()) [] False
    ,AD "FileCloseAllButPackage" (__ "Close All But Package") Nothing Nothing
        (do fileCloseAllButPackage; return ()) [] False
    ,AD "FileCloseAllButWorkspace" (__ "Close All But Workspace") Nothing Nothing
        (do fileCloseAllButWorkspace; return ()) [] False
    ,AD "Quit" (__ "_Quit") Nothing (Just "gtk-quit")
        quit [] False

    ,AD "Edit" (__ "_Edit") Nothing Nothing (return ()) [] False
    ,AD "EditUndo" (__ "_Undo") Nothing (Just "gtk-undo")
        editUndo [] False
    ,AD "EditRedo" (__ "_Redo") Nothing (Just "gtk-redo")
        editRedo [] False
    ,AD "EditCut" (__ "Cu_t") Nothing (Just "gtk-cut")
        editCut [] {--Just "<control>X"--} False
    ,AD "EditCopy" (__ "_Copy")  Nothing  (Just "gtk-copy")
        editCopy [] {--Just "<control>C"--} False
    ,AD "EditPaste" (__ "_Paste") Nothing (Just "gtk-paste")
        editPaste [] {--Just "<control>V"--} False
    ,AD "EditDelete" (__ "_Delete") Nothing (Just "gtk-delete")
        editDelete [] False
    ,AD "EditSelectAll" (__ "Select_All") Nothing (Just "gtk-select-all")
        editSelectAll [] False
    ,AD "EditFind" (__ "Find") Nothing (Just "gtk-find")
        (editFindInc Initial) [] False
    ,AD "EditFindNext" (__ "Find _Next") Nothing (Just "gtk-find-next")
        (editFindInc Forward) [] False
    ,AD "EditFindPrevious" (__ "Find _Previous") Nothing (Just "gtk-find-previous")
        (editFindInc Backward) [] False
    ,AD "EditGotoLine" (__ "_Goto Line") Nothing (Just "gtk-jump")
        editGotoLine [] False

    ,AD "EditComment" (__ "_Comment") Nothing Nothing
        editComment [] False
    ,AD "EditUncomment" (__ "_Uncomment") Nothing Nothing
        editUncomment [] False
    ,AD "EditShiftRight" (__ "Shift _Right") Nothing Nothing
        editShiftRight [] False
    ,AD "EditShiftLeft" (__ "Shift _Left") Nothing Nothing
        editShiftLeft [] False

    --,AD "Align" "_Align" Nothing Nothing (return ()) [] False
    ,AD "EditAlignEqual" (__ "Align _=") Nothing Nothing
        (align '=') [] False
    ,AD "EditAlignRightArrow" (__ "Align -_>") Nothing Nothing
        (align '>') [] False
    ,AD "EditAlignLeftArrow" (__ "Align _<-") Nothing Nothing
        (align '<') [] False
    ,AD "EditAlignTypeSig" (__ "Align _::") Nothing Nothing
        (align ':') [] False

    ,AD "Workspace" (__ "_Workspace") Nothing Nothing (return ()) [] False
    ,AD "NewWorkspace" (__ "_New...") Nothing Nothing
        (workspaceNew >> showWorkspace) [] False
    ,AD "OpenWorkspace" (__ "_Open...") Nothing Nothing
        (workspaceOpen >> showWorkspace) [] False
    ,AD "RecentWorkspaces" (__ "Open _Recent") Nothing Nothing (return ()) [] False
    ,AD "CloseWorkspace" (__ "_Close") Nothing Nothing
        workspaceClose [] False

    ,AD "CleanWorkspace" (__ "Cl_ean") (Just (__ "Cleans all packages")) (Just "ide_clean")
        (workspaceTry workspaceClean) [] False
    ,AD "MakeWorkspace" (__ "_Make") (Just (__ "Makes all of this workspace")) (Just "ide_configure")
        (workspaceTry workspaceMake) [] False
    ,AD "NextError" (__ "_Next Error") (Just (__ "Go to the next error")) (Just "ide_error_next")
        nextError [] False
    ,AD "PreviousError" (__ "_Previous Error") (Just (__ "Go to the previous error")) (Just "ide_error_prev")
        previousError [] False

    ,AD "Package" (__ "_Package") Nothing Nothing (return ()) [] False
    ,AD "NewPackage" (__ "_New...") Nothing Nothing
        (showWorkspace >> workspaceTry workspacePackageNew) [] False
    ,AD "AddPackage" (__ "_Add...") Nothing Nothing
        (showWorkspace >> workspaceTry workspaceAddPackage) [] False
    ,AD "ClonePackage" (__ "Add From Source _Repository...") Nothing Nothing
        (showWorkspace >> workspaceTry workspacePackageClone) [] False
--    ,AD "RecentPackages" "_Recent Packages" Nothing Nothing (return ()) [] False
    ,AD "PackageEdit" (__ "_Edit") Nothing Nothing (return ()) [] False
    ,AD "EditPackage" (__ "With _Package Editor") Nothing Nothing
        (packageTry packageEdit) [] False
    ,AD "EditPackageText" (__ "With _Text Editor") Nothing Nothing
        (packageTry packageEditText) [] False
--    ,AD "RemovePackage" "_Close Package" Nothing Nothing
--        removePackage [] False
    ,AD "PackageFlags" (__ "Package Flags") (Just (__ "Edit the package flags used")) Nothing
        (getFlags Nothing >>= \ p -> displayPane p False) [] False

    ,AD "PackageSandbox" (__ "_Sandbox") Nothing Nothing (return ()) [] False
    ,AD "SandboxInit" (__ "_Init") (Just (__ "Initialise a cabal sandbox for the package")) Nothing
        (packageTry sandboxInit) [] False
    ,AD "SandboxInitShared" (__ "Init _Shared...") (Just (__ "Initialise or use a cabal sandbox in a selected directory")) Nothing
        (packageTry sandboxInitShared) [] False
    ,AD "SandboxDelete" (__ "_Delete") (Just (__ "Delete the cabal sandbox")) Nothing
        (packageTry sandboxDelete) [] False
    ,AD "SandboxAddSource" (__ "_Add Source...") (Just (__ "Add a source package into the sandbox")) Nothing
        (packageTry (sandboxAddSource False)) [] False
    ,AD "SandboxAddSourceSnapshot" (__ "Add Source S_napshot...") (Just (__ "Add a snapshot of a source package into the sandbox")) Nothing
        (packageTry (sandboxAddSource True)) [] False

    ,AD "CleanPackage" (__ "Cl_ean") (Just (__ "Cleans the package")) (Just "ide_clean")
        (packageTry packageClean) [] False
    ,AD "ConfigPackage" (__ "_Configure") (Just (__ "Configures the package")) (Just "ide_configure")
        (packageTry packageConfig) [] False
    ,AD "BuildPackage" (__ "_Build") (Just (__ "Builds the package")) (Just "ide_make")
        (packageTry makePackage) [] False
    ,AD "DocPackage" (__ "_Build Documentation") (Just (__ "Builds the documentation")) Nothing
        (packageTry packageDoc) [] False
    ,AD "CopyPackage" (__ "_Copy") (Just (__ "Copies the package")) Nothing
        (packageTry packageCopy) [] False
    ,AD "RunPackage" (__ "_Run") (Just (__ "Runs the package")) (Just "ide_run")
        (packageTry packageRun) [] False
    ,AD "RunJavaScript" (__ "_Run JavaScript") (Just (__ "Run jsexe created by GHCJS")) (Just "ide_js")
        (packageTry packageRunJavaScript) [] False
    ,AD "ResolveErrors" (__ "Resol_ve Errors") (Just (__ "Resolve 'Hidden package' and 'Not in scope' errors by adding the necessary dependancies or imports")) Nothing
        resolveErrors [] False

    ,AD "InstallDependenciesPackage" (__ "_Install Dependencies") (Just (__ "Install the package's dependencies from the hackage server")) Nothing
        (packageTry packageInstallDependencies) [] False
    ,AD "RegisterPackage" (__ "_Register") Nothing Nothing
        (packageTry packageRegister) [] False
    ,AD "HLintPackage" (__ "_HLint") Nothing Nothing
        (packageTry packageHLint) [] False
    ,AD "TestPackage" (__ "Test") Nothing Nothing
        (packageTry packageTest) [] False
    ,AD "SdistPackage" (__ "Source Dist") Nothing Nothing
        (packageTry packageSdist) [] False
    ,AD "OpenDocPackage" (__ "_Open Doc") Nothing Nothing
        (packageTry packageOpenDoc) [] False

    ,AD "Debug" (__ "_Debug") Nothing Nothing (return ()) [] False
    ,AD "StartDebugger" (__ "_Start Debugger") (Just (__ "Starts using the GHCi debugger for build and run")) Nothing
        (packageTry debugStart) [] False
    ,AD "QuitDebugger" (__ "_Quit Debugger") (Just (__ "Quit the GHCi debugger if it is running")) Nothing
        debugQuit [] False
    ,AD "ExecuteSelection" (__ "_Execute Selection") (Just (__ "Sends the selected text to the debugger")) Nothing
        debugExecuteSelection [] False

    ,AD "DebugSetBreakpoint" (__ "Set Breakpoint") (Just (__ "Set a breakpoint on the selected name or current line")) Nothing
        debugSetBreakpoint [] False
    ,AD "ShowNextBreakpoint" (__ "Show Next Breakpoint") (Just (__ "Show the next breakpoint")) Nothing
        nextBreakpoint [] False
    ,AD "ShowPreviousBreakpoint" (__ "Show Previous Breakpoint") (Just (__ "Show the previous breakpoint")) Nothing
        previousBreakpoint [] False
    ,AD "DebugDeleteAllBreakpoints" (__ "Delete All Breakpoints") (Just "") Nothing
        debugDeleteAllBreakpoints [] False

    ,AD "DebugContinue" (__ "Continue") (Just (__ "Resume after a breakpoint")) (Just "ide_continue")
        debugContinue [] False
    ,AD "DebugAbandon" (__ "Abandon") (Just (__ "At a breakpoint, abandon current computation")) Nothing
        debugAbandon [] False
    ,AD "DebugStop" (__ "Stop") (Just (__ "Interrupt the running operation.")) Nothing
        debugStop [] False

    ,AD "DebugStep" (__ "Step") (Just (__ "Single-step after stopping at a breakpoint")) (Just "ide_step")
        debugStep [] False
    ,AD "DebugStepExpression" (__ "Step Expression") (Just (__ "Single-step into selected expression")) Nothing
        debugStepExpression [] False
    ,AD "DebugStepLocal" (__ "Step Local") (Just (__ "Single-step within the current top-level binding")) (Just "ide_local")
        debugStepLocal [] False
    ,AD "DebugStepModule" (__ "Step Module") (Just (__ "Single-step restricted to the current module")) (Just "ide_module")
        debugStepModule [] False

    ,AD "DebugTrace" (__ "Trace") (Just (__ "Trace after stopping at a breakpoint")) Nothing
        debugTrace [] False
    ,AD "DebugTraceExpression" (__ "Trace Expression") (Just (__ "Evaluate the selected expression with tracing on")) Nothing
        debugTraceExpression [] False
    ,AD "DebugHistory" (__ "History") (Just (__ "After 'Trace', show the execution history")) Nothing
        debugHistory [] False
    ,AD "DebugBack" (__ "Back") (Just (__ "Go back in the history (after 'Trace')")) Nothing
        debugBack [] False
    ,AD "DebugForward" (__ "Forward") (Just (__ "Go forward in the history (after 'Back')")) Nothing
        debugForward [] False

    ,AD "DebugForce" (__ "Force") (Just (__ "Print the selected expression, forcing unevaluated parts")) Nothing
        debugForce [] False
    ,AD "DebugPrint" (__ "Print") (Just (__ "Prints a value without forcing its computation")) Nothing
        debugPrint [] False
    ,AD "DebugSimplePrint" (__ "SimplePrint") (Just (__ "Simplifed version of Print")) Nothing
        debugSimplePrint [] False

    ,AD "ShowBindings" (__ "Show Bin_dings") (Just (__ "Show the current bindings")) Nothing
        debugShowBindings [] False
    ,AD "ShowBreakpoints" (__ "Show _Breakpoints") (Just (__ "Show the active breakpoints")) Nothing
        debugShowBreakpoints [] False
    ,AD "ShowContext" (__ "Show _Context") (Just (__ "Show the breakpoint context")) Nothing
        debugShowContext [] False
    ,AD "ShowLoadedModules" (__ "Show Loaded _Modules") (Just (__ "Show the currently loaded modules")) Nothing
        debugShowModules [] False
    ,AD "ShowPackages" (__ "Show _Packages") (Just (__ "Show the currently active packages")) Nothing
        debugShowPackages [] False
    ,AD "ShowLanguages" (__ "Show _Languages") (Just (__ "Show the currently active language")) Nothing
        debugShowLanguages [] False

    ,AD "DebugInformation" (__ "Information") (Just (__ "Display information about the selected name(s)")) Nothing
        debugInformation [] False
    ,AD "DebugKind" (__ "Kind") (Just (__ "Show the kind of the selected type")) Nothing
        debugKind [] False
    ,AD "DebugType" (__ "Type") (Just (__ "Show the type of the selected expression")) Nothing
        debugType [] False

    ,AD "Metadata" (__ "_Metadata") Nothing Nothing (return ()) [] False
    ,AD "UpdateMetadataCurrent" (__ "_Update workspace data") (Just (__ "Updates data for the current workspace"))
            (Just "ide_rebuild_meta") updateWorkspaceInfo [] False
    ,AD "RebuildMetadataCurrent" (__ "_Rebuild workspace data") (Just (__ "Rebuilds data for the current workspace"))
            Nothing rebuildWorkspaceInfo [] False
    ,AD "UpdateMetadataLib" (__ "U_pdate system data") Nothing Nothing
        updateSystemInfo [] False
    ,AD "RebuildMetadataLib" (__ "R_ebuild system data") Nothing Nothing
        rebuildSystemInfo [] False

    ,AD "Session" (__ "_Session") Nothing Nothing (return ()) [] False
    ,AD "SaveSession" (__ "_Save Session") Nothing Nothing
        saveSessionAsPrompt [] False
    ,AD "LoadSession" (__ "_Load Session") Nothing Nothing
        loadSessionPrompt [] False
    ,AD "ForgetSession" (__ "_Forget Session") Nothing Nothing
        (return ()) [] True

    ,AD "Panes" (__ "_Panes") Nothing Nothing (return ()) [] False
    ,AD "ShowBrowser" (__ "Browser") Nothing Nothing
        showBrowser [] False
    ,AD "ShowModules" (__ "Modules") Nothing Nothing
        showModules [] False
    ,AD "ShowInfo" (__ "Info") Nothing Nothing
        showInfo [] False
    ,AD "ShowDebugger" (__ "Debugger") Nothing Nothing
        showDebugger [] False
    ,AD "ShowBreakpointsPane" (__ "Breakpoints") Nothing Nothing
        showBreakpoints [] False
    ,AD "ShowTrace" (__ "Trace") Nothing Nothing
        showTrace [] False
    ,AD "ShowVariables" (__ "Variables") Nothing Nothing
        showVariables [] False
    ,AD "ShowSearch" (__ "Search") Nothing Nothing
        (getSearch Nothing  >>= \ p -> displayPane p False) [] False
    ,AD "ShowFiles" (__ "Files") Nothing Nothing
        (getFiles Nothing  >>= \ p -> displayPane p False >> refreshFiles) [] False
    ,AD "ShowGrep" (__ "Grep") Nothing Nothing
        (getGrep Nothing  >>= \ p -> displayPane p False) [] False
    ,AD "ShowDocumentation" (__ "Documentation") Nothing Nothing
        (getDocumentation Nothing  >>= \ p -> displayPane p False) [] False
    ,AD "ShowOutput" (__ "Output") Nothing Nothing
        (getOutputPane Nothing  >>= \ p -> displayPane p False) [] False
    ,AD "ShowInspect" (__ "Web Inspect") Nothing Nothing
        (getInspectPane Nothing  >>= \ p -> displayPane p False) [] False
    ,AD "ShowErrors" (__ "Errors") Nothing Nothing
        (getErrors Nothing  >>= \ p -> displayPane p False) [] False
    ,AD "ShowLog" (__ "Log") Nothing Nothing
        showLog [] False
    ,AD "ShowWorkspace" (__ "Workspace") Nothing Nothing
        showWorkspace [] False

    ,AD "View" (__ "_View") Nothing Nothing (return ()) [] False
    ,AD "ViewMoveLeft" (__ "Move _Left") Nothing Nothing
        (viewMove LeftP) [] False
    ,AD "ViewMoveRight" (__ "Move _Right") Nothing Nothing
        (viewMove RightP) [] False
    ,AD "ViewMoveUp" (__ "Move _Up") Nothing Nothing
        (viewMove TopP) [] False
    ,AD "ViewMoveDown" (__ "Move _Down") Nothing Nothing
        (viewMove BottomP) [] False
    ,AD "ViewSplitHorizontal" (__ "Split H_orizontal") Nothing Nothing
        viewSplitHorizontal [] False
    ,AD "ViewSplitVertical" (__ "Split _Vertical") Nothing Nothing
        viewSplitVertical [] False
    ,AD "ViewCollapse" (__ "_Collapse") Nothing Nothing
        viewCollapse [] False
    ,AD "ViewNest" (__ "_Group") Nothing Nothing
        viewNewGroup [] False
    ,AD "ViewDetach" (__ "_Detach") Nothing Nothing
        viewDetachInstrumented [] False
    ,AD "ViewFullScreen" (__ "_Full Screen") Nothing Nothing
        viewFullScreen [] True
    ,AD "ViewDark" (__ "Dark") Nothing Nothing
        viewDark [] True

    ,AD "ViewTabsLeft" (__ "Tabs Left") Nothing Nothing
        (viewTabsPos PosLeft) [] False
    ,AD "ViewTabsRight" (__ "Tabs Right") Nothing Nothing
        (viewTabsPos PosRight) [] False
    ,AD "ViewTabsUp" (__ "Tabs Up") Nothing Nothing
        (viewTabsPos PosTop) [] False
    ,AD "ViewTabsDown" (__ "Tabs Down") Nothing Nothing
        (viewTabsPos PosBottom) [] False
    ,AD "ViewSwitchTabs" (__ "Tabs On/Off") Nothing Nothing
        viewSwitchTabs [] False

    ,AD "ViewClosePane" (__ "Close pane") Nothing (Just "gtk-close")
        sessionClosePane [] False

    ,AD "FlipDown" (__ "Flip down") Nothing Nothing
        flipDown [] False
    ,AD "FlipUp" (__ "Flip up") Nothing Nothing
        flipUp [] False
    ,AD "StartComplete" (__ "StartComplete") Nothing Nothing
        startComplete [] False

    ,AD "ViewHistoryBack" (__ "Back") Nothing (Just "gtk-go-back")
        historyBack [] False
    ,AD "ViewHistoryForth" (__ "Forward") Nothing (Just "gtk-go-forward")
        historyForward [] False


    ,AD "ClearLog" (__ "_Clear Log") Nothing Nothing
        clearLog [] False
    ,AD "ToggleToolbar" (__ "Toggle Toolbar") Nothing Nothing
        toggleToolbar [] False

    ,AD "Configuration" (__ "_Configuration") Nothing Nothing (return ()) [] False
    ,AD "EditCandy" (__ "_To Candy") Nothing Nothing
        editCandy [] True
    ,AD "PrefsEdit" (__ "_Edit Prefs") Nothing Nothing
        (getPrefs Nothing >>= \ p -> displayPane p False) [] False

    ,AD "Help" (__ "_Help") Nothing Nothing (return ()) [] False
    ,AD "HelpDebug" (__ "Debug") Nothing Nothing (do
            pack <- readIDE activePack
            ideMessage Normal (T.pack $ show pack)) [] False
--    ,AD "HelpDebug2" "Debug2" (Just "<Ctrl>d") Nothing dbgInstalledPackageInfo [] False
    ,AD "HelpManual" (__ "Manual") Nothing Nothing (openBrowser "http://leksah.org/leksah_manual.pdf") [] False
    ,AD "HelpHomepage" (__ "Homepage") Nothing Nothing (openBrowser "http://leksah.org") [] False
    ,AD "HelpAbout" (__ "About") Nothing (Just "gtk-about") (liftIO aboutDialog) [] False

    ,AD "BackgroundBuildToggled" (__ "_BackgroundBuild") (Just (__ "Build in the background and report errors")) (Just "ide_build")
        backgroundBuildToggled [] True
    ,AD "RunUnitTestsToggled" (__ "_RunUnitTests") (Just (__ "Run unit tests when building")) (Just "gtk-apply")
        runUnitTestsToggled [] True
    ,AD "MakeModeToggled" (__ "_MakeMode") (Just (__ "Make dependent packages")) (Just "ide_make")
        makeModeToggled [] True
    ,AD "DebugToggled" "_GHCi" (Just (__ "Use GHCi debugger to build and run")) (Just "ide_debug")
        debugToggled [] True
    ,AD "OpenDocu" (__ "_OpenDocu") (Just (__ "Opens a browser for a search of the selected data")) Nothing
        openDocu [] True
        ]

--
-- | The menu description in XML Syntax as defined by GTK
--
menuDescription :: IO Text
menuDescription = do
    dataDir     <- getDataDir
    prefsPath   <- getConfigFilePathForLoad "leksah.menu" Nothing dataDir
    T.readFile prefsPath

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
                mi <- menuItemNewWithLabel $ T.pack s
                mi `on` menuItemActivate $ reflectIDE (fileOpen' s) ideR
                menuShellAppend recentFilesMenu mi) recentFiles'
        oldSubmenu <- menuItemGetSubmenu recentFilesItem
        when (isJust oldSubmenu) $ do
            widgetHide (fromJust oldSubmenu)
            widgetDestroy (fromJust oldSubmenu)
        menuItemSetSubmenu recentFilesItem recentFilesMenu
        widgetShowAll recentFilesMenu
        recentWorkspacesMenu    <-  menuNew
        mapM_ (\s -> do
            fe <- doesFileExist s
            when fe $ do
                mi <- menuItemNewWithLabel $ T.pack s
                mi `on` menuItemActivate $ reflectIDE (workspaceOpenThis True (Just s) >> showWorkspace) ideR
                menuShellAppend recentWorkspacesMenu mi) recentWorkspaces'
        oldSubmenu <- menuItemGetSubmenu recentWorkspacesItem
        when (isJust oldSubmenu) $ do
            widgetHide (fromJust oldSubmenu)
            widgetDestroy (fromJust oldSubmenu)
        menuItemSetSubmenu recentWorkspacesItem recentWorkspacesMenu
        widgetShowAll recentWorkspacesMenu)


--
-- | Building the Menu
--
makeMenu :: UIManager -> [ActionDescr IDERef] -> Text -> IDEAction
makeMenu uiManager actions menuDescription = reifyIDE (\ideR -> do
    actionGroupGlobal <- actionGroupNew ("global" :: Text)
    mapM_ (actm ideR actionGroupGlobal) actions
    uiManagerInsertActionGroup uiManager actionGroupGlobal 1
    uiManagerAddUiFromString uiManager menuDescription
    return ())
    where
        actm ideR ag (AD name label tooltip stockId ideAction accs isToggle) = do
            let (acc,accString) = if null accs
                                    then (Just "", "=" <> name)
                                    else (Just (head accs), head accs <> "=" <> name)
            if isToggle
                then do
                    act <- toggleActionNew name label tooltip stockId
                    on act actionToggled $ doAction ideAction ideR accString
                    actionGroupAddActionWithAccel ag act acc
                else do
                    act <- actionNew name label tooltip stockId
                    on act actionActivated $ doAction ideAction ideR accString
                    actionGroupAddActionWithAccel ag act acc
        doAction ideAction ideR accStr =
            reflectIDE (do
                ideAction
                triggerEventIDE (StatusbarChanged [CompartmentCommand accStr])
                return ()) ideR

-- getMenuAndToolbars :: UIManager -> IO (AccelGroup, MenuBar, Toolbar)
getMenuAndToolbars uiManager = do
    accGroup <- uiManagerGetAccelGroup uiManager
    mbMenu   <- uiManagerGetWidget uiManager ("/ui/menubar" :: Text)
    let menu = case mbMenu of
                    Just it -> castToMenuBar it
                    Nothing -> throwIDE (__ "Menu>>makeMenu: failed to create menubar")
    mbToolbar <- uiManagerGetWidget uiManager ("/ui/toolbar" :: Text)
    let toolbar = case mbToolbar of
                    Just it -> castToToolbar it
                    Nothing -> throwIDE (__ "Menu>>makeMenu: failed to create toolbar")
    toolbarSetStyle toolbar ToolbarIcons
    toolbarSetIconSize toolbar IconSizeSmallToolbar
    widgetSetSizeRequest toolbar 700 (-1)
    return (accGroup,menu,toolbar)

textPopupMenu :: IDERef -> Menu -> IO ()
textPopupMenu ideR menu = do
    let reflectIDE_ x = reflectIDE x ideR
    items <- containerGetChildren menu
    mi1 <- menuItemNewWithLabel (__ "Eval")
    mi1 `on` menuItemActivate $ reflectIDE_ debugExecuteSelection
    menuShellAppend menu mi1
    mi11 <- menuItemNewWithLabel (__ "Eval & Insert")
    mi11 `on` menuItemActivate $
      reflectIDE_ debugExecuteAndShowSelection
    menuShellAppend menu mi11
    mi12 <- menuItemNewWithLabel (__ "Step")
    mi12 `on` menuItemActivate $ reflectIDE_ debugStepExpression
    menuShellAppend menu mi12
    mi13 <- menuItemNewWithLabel (__ "Trace")
    mi13 `on` menuItemActivate $ reflectIDE_ debugTraceExpression
    menuShellAppend menu mi13
    mi16 <- menuItemNewWithLabel (__ "Set Breakpoint")
    mi16 `on` menuItemActivate $ reflectIDE_ debugSetBreakpoint
    menuShellAppend menu mi16
    sep1 <- separatorMenuItemNew
    menuShellAppend menu sep1
    mi14 <- menuItemNewWithLabel (__ "Type")
    mi14 `on` menuItemActivate $ reflectIDE_ debugType
    menuShellAppend menu mi14
    mi141 <- menuItemNewWithLabel (__ "Info")
    mi141 `on` menuItemActivate $ reflectIDE_ debugInformation
    menuShellAppend menu mi141
    mi15 <- menuItemNewWithLabel (__ "Kind")
    mi15 `on` menuItemActivate $ reflectIDE_ debugKind
    menuShellAppend menu mi15
    sep2 <- separatorMenuItemNew
    menuShellAppend menu sep2
    mi2 <- menuItemNewWithLabel (__ "Find (text)")
    mi2 `on` menuItemActivate $ reflectIDE_ (editFindInc Initial)
    menuShellAppend menu mi2
    mi3 <- menuItemNewWithLabel (__ "Search (metadata)")
    mi3 `on` menuItemActivate $
      reflectIDE_ $
         getSearch Nothing >>=
           (\ search ->
              do mbtext <- selectedText
                 case mbtext of
                     Just t -> searchMetaGUI search t
                     Nothing -> ideMessage Normal (__ "Select a text first"))
    menuShellAppend menu mi3
    let interpretingEntries = [castToWidget mi16]
    let interpretingSelEntries
          = [castToWidget mi1, castToWidget mi11, castToWidget mi12,
             castToWidget mi13, castToWidget mi14, castToWidget mi141,
             castToWidget mi15]
    let otherEntries = [castToWidget mi2, castToWidget mi3]
    -- isInterpreting' <- (reflectIDE isInterpreting ideR)
    selected <- reflectIDE selectedText ideR
--    unless isInterpreting'
--        $ mapM_ (\w -> widgetSetSensitive w False) (interpretingEntries ++ interpretingSelEntries)
--    unless (isJust selected)
--        $ mapM_ (\w -> widgetSetSensitive w False) (otherEntries ++ interpretingSelEntries)
    mapM_ widgetShow interpretingEntries
    mapM_ widgetShow interpretingSelEntries
    mapM_ widgetShow
      (castToWidget sep1 : castToWidget sep2 : otherEntries)
    mapM_ widgetHide $ take 2 (reverse items)

canQuit :: IDEM Bool
canQuit = do
    modifyIDE_ (\ide -> ide{currentState = IsShuttingDown})
    saveSession :: IDEAction
    can <- fileCloseAll (\_ -> return True)
    unless can $ modifyIDE_ (\ide -> ide{currentState = IsRunning})
    return can

-- | Quit ide
quit :: IDEAction
quit = do
    can <- canQuit
    when can $ liftIO mainQuit

--
-- | Show the about dialog
--
aboutDialog :: IO ()
aboutDialog = do
    d <- aboutDialogNew
    dd <- getDataDir
    license <- catch (T.readFile $ dd </> T.unpack (__ "LICENSE")) (\ (_ :: SomeException) -> return "")
    set d [ aboutDialogName := ("Leksah" :: Text)
          , aboutDialogVersion := T.pack $ showVersion version
          , aboutDialogCopyright := __ "Copyright 2007-2011 Jürgen Nicklisch-Franken, Hamish Mackenzie"
          , aboutDialogComments := __ "An integrated development environement (IDE) for the " <>
                               __ "programming language Haskell and the Glasgow Haskell Compiler"
          , aboutDialogLicense := Just license
          , aboutDialogWebsite := ("http://leksah.org/" :: Text)
          , aboutDialogAuthors := ["Jürgen Nicklisch-Franken","Hamish Mackenzie" :: Text] ]
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
            "ide_empty","ide_source_local", "ide_js"]
        iconFactoryAddDefault iconFactory)
    (\(e :: SomeException) -> getDataDir >>= \dataDir -> throwIDE (T.pack $ printf (__ "Can't load icons from %s %s") dataDir (show e)))
    where
    loadIcon dataDir iconFactory name = do
        pb      <-  pixbufNewFromFile $ dataDir </> "pics" </> (name <> ".png")
        icon    <-  iconSetNewFromPixbuf pb
        iconFactoryAdd iconFactory (T.pack name) icon

setSensitivity :: [(SensitivityMask, Bool)] -> IDEAction
setSensitivity = mapM_ setSensitivitySingle
    where   setSensitivitySingle (sens,bool) = do
                actions <- getActionsFor sens
                liftIO $ mapM_ (`actionSetSensitive` bool) actions
                let additionalActions = getAdditionalActionsFor sens
                mapM_ (\a -> a bool) additionalActions

getActionsFor :: SensitivityMask -> IDEM [Action]
getActionsFor SensitivityForwardHist = getActionsFor' ["ViewHistoryForth"]
getActionsFor SensitivityBackwardHist = getActionsFor' ["ViewHistoryBack"]
getActionsFor SensitivityProjectActive = getActionsFor'
    ["EditPackage", "PackageFlags", "ConfigPackage", "BuildPackage"
    ,"DocPackage", "CleanPackage", "CopyPackage", "RunPackage","InstallDependenciesPackage"
    ,"RegisterPackage", "TestPackage","SdistPackage"
    ,"OpenDocPackage","FileCloseAll"]
getActionsFor SensitivityError = getActionsFor' ["NextError", "PreviousError"]
getActionsFor SensitivityEditor = getActionsFor' ["EditUndo", "EditRedo",
        "EditGotoLine","EditComment", "EditUncomment",
        "EditShiftLeft", "EditShiftRight","FileClose",
        "OpenDocu"
        ]
getActionsFor SensitivityInterpreting = getActionsFor' ["QuitDebugger"]
getActionsFor SensitivityWorkspaceOpen = return [] --TODO add here

getActionsFor' :: [Text] -> IDEM[Action]
getActionsFor' l = do
    r <- mapM getActionFor l
    return (catMaybes r)
    where
        getActionFor string = do
            uiManager' <- getUiManager
            actionGroups <- liftIO $ uiManagerGetActionGroups uiManager'
            res <- liftIO $ actionGroupGetAction (head actionGroups) string
            when (isNothing res) $ ideMessage Normal $ T.pack $ printf (__ "Can't find UI Action %s") (T.unpack string)
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
        widgetSetName vb ("topBox" :: Text)
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
        win `on` keyPressEvent $ handleSpecialKeystrokes ideR
        windowAddAccelGroup win acc
        containerAdd win vb
        reflectIDE (do
            setCandyState (fst (sourceCandy prefs))
            setBackgroundBuildToggled (backgroundBuild prefs)
            setRunUnitTests (runUnitTests prefs)
            setMakeModeToggled (makeMode prefs)) ideR

instrumentSecWindow :: Window -> IDEAction
instrumentSecWindow win = do
    liftIO $ debugM "leksah" "instrumentSecWindow"
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
        win `on` keyPressEvent $ handleSpecialKeystrokes ideR
        return ()

--
-- | Callback function for onKeyPress of the main window, so 'preprocess' any key
--
handleSpecialKeystrokes :: IDERef -> EventM EKey Bool
handleSpecialKeystrokes ideR = do
  name <- eventKeyName
  mods <- eventModifier
  keyVal <- eventKeyVal
  let mbChar = keyToChar keyVal
  liftIO $ (`reflectIDE` ideR) $ do
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
                case sk of
                    Nothing ->
                        case Map.lookup (keyVal,sort mods) sks of
                            Nothing -> do
                                triggerEventIDE (StatusbarChanged [CompartmentCommand ""])
                                return False
                            Just map -> do
                                let sym = printMods mods <> name
                                triggerEventIDE (StatusbarChanged [CompartmentCommand sym])
                                modifyIDE_ (\ide -> ide{specialKey = Just (map,sym)})
                                return True
                    Just (map,sym) -> do
                        case Map.lookup (keyVal,sort mods) map of
                            Nothing -> do
                                triggerEventIDE (StatusbarChanged [CompartmentCommand
                                    (sym <> printMods mods <> name <> "?")])
                                return ()
                            Just (AD actname _ _ _ ideAction _ _) -> do
                                triggerEventIDE (StatusbarChanged [CompartmentCommand
                                    (sym <> " " <> printMods mods <> name <> "=" <> actname)])
                                ideAction
                        modifyIDE_ (\ide -> ide{specialKey = Nothing})
                        return True
    where
    printMods :: [Modifier] -> Text
    printMods = mconcat . map (T.pack . show)

setSymbol :: Text -> Bool -> IDEAction
setSymbol symbol openSource = do
    currentInfo' <- getWorkspaceInfo
    search <- getSearch Nothing
    case currentInfo' of
        Nothing -> return ()
        Just (GenScopeC (PackScope _ symbolTable1),
               GenScopeC (PackScope _ symbolTable2)) ->
            case filter (not . isReexported) (getIdentifierDescr symbol symbolTable1 symbolTable2) of
                []     -> return ()
                [a]   -> selectIdentifier a openSource
                [a, b] -> if isJust (dscMbModu a) && dscMbModu a == dscMbModu b &&
                            isNear (dscMbLocation a) (dscMbLocation b)
                                then selectIdentifier a openSource
                                else setChoices search [a,b]
                l      -> setChoices search l
  where
    isNear (Just a) (Just b) = abs (locationSLine a - locationSLine b) <= 3
    isNear _ _               = False

--
-- | Register handlers for IDE events
--
registerLeksahEvents :: IDEAction
registerLeksahEvents =    do
    stRef   <-  ask
    liftIO $ debugM "leksah" "registerLeksahEvents"
    defaultLogLaunch <- getDefaultLogLaunch
    registerEvent stRef "LogMessage"
        (\e@(LogMessage s t)      -> do
            postAsyncIDE $ do
                log <- getLog
                defaultLogLaunch <- getDefaultLogLaunch
                liftIO $ appendLog log defaultLogLaunch s t
                return ()
            return e)
    registerEvent stRef "SelectInfo"
        (\ e@(SelectInfo str gotoSource)
                                  -> setSymbol str gotoSource >> return e)
    registerEvent stRef "SelectIdent"
        (\ e@(SelectIdent id)     -> selectIdentifier id False >> return e)
    registerEvent stRef "InfoChanged"
        (\ e@(InfoChanged b)      -> reloadKeepSelection b >> return e)
    registerEvent stRef "UpdateWorkspaceInfo"
        (\ e@UpdateWorkspaceInfo  -> updateWorkspaceInfo >> return e)
    registerEvent stRef "WorkspaceChanged"
        (\ e@(WorkspaceChanged showPane updateFileCache)
                                  -> postAsyncIDE (updateWorkspace showPane updateFileCache) >> return e)
    registerEvent stRef "RecordHistory"
        (\ rh@(RecordHistory h)   -> recordHistory h >> return rh)
    registerEvent stRef "Sensitivity"
        (\ s@(Sensitivity h)      -> setSensitivity h >> return s)
    registerEvent stRef "SearchMeta"
        (\ e@(SearchMeta string)  -> getSearch Nothing >>= flip searchMetaGUI string >> return e)
    registerEvent stRef "StartFindInitial"
        (\ e@StartFindInitial     -> editFindInc Initial >> return e)
    registerEvent stRef "LoadSession"
        (\ e@(LoadSession fp)     -> loadSession fp >> return e)
    registerEvent stRef "SaveSession"
        (\ e@(SaveSession fp)     -> saveSessionAs fp Nothing >> return e)
    registerEvent stRef "UpdateRecent"
        (\ e@UpdateRecent         -> updateRecentEntries >> return e)
    registerEvent stRef "VariablesChanged"
        (\ e@VariablesChanged     -> fillVariablesListQuiet >> return e)
    registerEvent stRef "ErrorChanged"
        (\ e@(ErrorChanged show') -> postAsyncIDE (fillErrorList show') >> return e)
    registerEvent stRef "ErrorAdded"
        (\ e@(ErrorAdded show' i ref) -> postAsyncIDE (addErrorToList show' i ref) >> return e)
    registerEvent stRef "CurrentErrorChanged"
        (\ e@(CurrentErrorChanged mbLogRef) -> postAsyncIDE (do
            selectRef mbLogRef
            selectError mbLogRef)  >> return e)
    registerEvent stRef "BreakpointChanged"
        (\ e@BreakpointChanged    -> postAsyncIDE fillBreakpointList >> return e)
    registerEvent stRef "CurrentBreakChanged"
        (\ e@(CurrentBreakChanged mbLogRef) -> postAsyncIDE (do
            selectRef mbLogRef
            selectBreak mbLogRef) >> return e)
    registerEvent stRef "TraceChanged"
        (\ e@TraceChanged         -> fillTraceList >> return e)
    registerEvent stRef "GotoDefinition"
        (\ e@(GotoDefinition descr)         -> goToDefinition descr >> return e)
    registerEvent stRef "GetTextPopup"
        (\ e@(GetTextPopup _)     -> return (GetTextPopup (Just textPopupMenu)))
    registerEvent stRef "StatusbarChanged"
        (\ e@(StatusbarChanged args) -> changeStatusbar args >> return e)
    registerEvent stRef "SelectSrcSpan"
        (\ e@(SelectSrcSpan mbSpan) -> selectMatchingErrors mbSpan >> return e)
    registerEvent stRef "SavedFile"
        (\ e@(SavedFile file) -> scheduleHLint (Right file) >> return e)
    return ()



