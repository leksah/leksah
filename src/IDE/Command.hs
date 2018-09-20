{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall -fno-warn-deprecations #-}
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

import Prelude ()
import Prelude.Compat hiding (log)
import System.FilePath
import Data.Version
import Control.Exception
import Data.Maybe

import IDE.Core.State
       (runPackage, runProject, postAsyncIDE, ipdPackageDir, pjTool,
        specialKeys, candyState, useCtrlTabFlipping, makeMode,
        runBenchmarks, runUnitTests, makeDocs, debug, javaScript, native,
        backgroundBuild, throwIDE, version, application,
        saveSessionOnClose, modifyIDE_, triggerEventIDE, reflectIDE,
        reifyIDE, recentWorkspaces, recentFiles, getDataDir,
        MessageLevel(..), ideMessage, activePack, readIDE, bufferProjCache,
        location, SymbolEvent(..), specialKey, toolbar, Prefs, prefs,
        darkUserInterface, SensitivityMask, currentState, IDEM, IDEAction,
        IDERef, ActionDescr, SearchHint(..), SensitivityMask(..),
        ActionDescr(..), IDEEvent(..), ProjectTool(..), PackScope(..),
        GenScope(..), Descr, Descr(..), SrcSpan(..), IDEState(..),
        StatusbarCompartment(..), PackModule(..), dscMbModu', displayPane,
        viewMove, PaneDirection(..), viewSplitHorizontal, viewSplitVertical,
        viewCollapse, viewNewGroup, viewTabsPos, viewSwitchTabs, getUiManager,
        viewDetach, Location(..), postSyncIDE, isReexported, dscMbModu,
        dscMbLocation, hlintOnSave, exitCode)
import IDE.Pane.SourceBuffer
import IDE.Pane.PackageFlags
import IDE.Pane.PackageEditor
import IDE.Pane.Errors
import IDE.Package
import IDE.Preferences (runPreferencesDialog, applyInterfaceTheme)
import IDE.HLint
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
import qualified Data.Map as  Map (lookup, empty)
import Data.List (intersperse, sort)
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
import IDE.TypeTip (setTypeTip)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad
       (void, unless, when, forM_, filterM)
import Control.Monad.Trans.Reader (ask)
import System.Log.Logger (debugM)
import IDE.Session
       (saveSessionAs, loadSession, saveSession, sessionClosePane,
        loadSessionPrompt, saveSessionAsPrompt, viewFullScreen)
import qualified Data.Text as T
       (uncons, concat, takeWhile, splitOn, unpack, pack)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)
import Data.Monoid (Monoid(..))
import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)
import GI.Gtk.Enums
       (IconSize(..), ToolbarStyle(..), PositionType(..), Orientation(..))
import GI.Gtk.Objects.Menu (Menu(..), menuNew)
import GI.Gtk.Objects.MenuItem
       (toMenuItem, menuItemSetSubmenu, menuItemGetSubmenu,
        onMenuItemActivate, menuItemNewWithLabel)
import GI.Gtk.Objects.Widget
       (IsWidget, onWidgetKeyPressEvent, widgetSetName,
        widgetShow, widgetSetSizeRequest, widgetShowAll,
        widgetDestroy, widgetHide, widgetSetSensitive)
import GI.Gtk.Objects.MenuShell (menuShellAppend)
import GI.Gtk.Objects.UIManager
       (uIManagerGetActionGroups, uIManagerGetWidget,
        uIManagerGetAccelGroup, uIManagerAddUiFromString,
        uIManagerInsertActionGroup, UIManager(..))
import GI.Gtk.Objects.ActionGroup
       (actionGroupGetAction, actionGroupAddActionWithAccel,
        actionGroupNew)
import GI.Gtk.Objects.ToggleAction
       (onToggleActionToggled, toggleActionNew)
import GI.Gtk.Objects.Action
       (onActionActivate, Action(..), actionSetSensitive, actionNew)
import Data.GI.Base (unsafeCastTo)
import GI.Gtk.Objects.MenuBar (MenuBar(..))
import GI.Gtk.Objects.Toolbar
       (toolbarSetIconSize, toolbarSetStyle, Toolbar(..))
import GI.Gtk.Objects.Container
       (containerAdd, containerGetChildren)
import GI.Gtk.Objects.SeparatorMenuItem (separatorMenuItemNew)
import GI.Gtk.Objects.AboutDialog
       (aboutDialogSetAuthors, setAboutDialogProgramName, setAboutDialogWebsite,
        setAboutDialogLicense, setAboutDialogComments, setAboutDialogCopyright,
        setAboutDialogVersion, aboutDialogNew)
import GI.Gtk.Objects.Dialog (dialogRun)
import GI.Gtk.Objects.IconFactory
       (iconFactoryAdd, iconFactoryAddDefault, iconFactoryNew)
import GI.GdkPixbuf.Objects.Pixbuf (pixbufNewFromFile)
import GI.Gtk.Structs.IconSet (iconSetNewFromPixbuf)
import GI.Gtk.Objects.IconTheme (iconThemeAddBuiltinIcon)
import GI.Gtk.Objects.Window
       (windowAddAccelGroup, windowSetIconFromFile, Window(..))
import GI.Gtk.Objects.Box (boxNew)
import Graphics.UI.Editor.Parameters
       (boxPackEnd', Packing(..), boxPackStart')
import GI.Gdk.Structs.EventKey
       (getEventKeyState, getEventKeyKeyval, EventKey(..))
import GI.Gdk.Functions (keyvalToUnicode, keyvalName)
import GI.Gdk.Flags (ModifierType, ModifierType(..))
import GI.Gtk.Objects.AccelGroup (AccelGroup(..))

import IDE.LPaste
import GI.Gio.Objects.Application (applicationQuit)
import IDE.Utils.Tool (ToolOutput(..))
import Data.IORef (writeIORef)
import GHC.IO.Exception (ExitCode(..))
import Control.Concurrent.MVar
       (tryTakeMVar, MVar, takeMVar, putMVar, newEmptyMVar)
import Distribution.Text (simpleParse)
import Control.Concurrent (tryPutMVar, threadDelay, forkIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Char (isUpper)
import Data.Attoparsec.Text (parseOnly)
import IDE.HaRe
       (deleteDef, rmOneParameter, addOneParameter, rename, liftOneLevel,
        liftToTopLevel, duplicateDef, ifToCase, demote)

printf :: PrintfType r => Text -> r
printf = S.printf . T.unpack

--
-- | The Actions known to the system (they can be activated by keystrokes or menus)
--
mkActions :: [ActionDescr IDERef]
mkActions =
    [
    AD "vcs" (__ "Version Con_trol") Nothing Nothing (return ()) [] False
    ,AD "FilePrint" (__ "_Print File") Nothing Nothing filePrint [] False
    ,AD "File" (__ "_File") Nothing Nothing (return ()) [] False
    ,AD "FileNew" (__ "_New") Nothing Nothing (return ()) [] False
    ,AD "FileNewWorkspace" (__ "_Workspace...") Nothing Nothing
        (workspaceNew >> showWorkspacePane) [] False
    ,AD "FileNewProject" (__ "Pro_ject...") Nothing Nothing
        (showWorkspacePane >> workspaceTry projectNew) [] False
    ,AD "FileNewPackage" (__ "_Package...") Nothing Nothing
        (showWorkspacePane >> workspaceTry workspacePackageNew) [] False
    ,AD "FileNewModule" (__ "_Module...") Nothing (Just "gtk-new")
        (packageTry $ addModule []) [] False
    ,AD "FileNewTextFile" (__ "_Text File...") Nothing Nothing
        fileNew [] False
    ,AD "FileOpen" (__ "_Open") Nothing Nothing (return ()) [] False
    ,AD "FileOpenWorkspace" (__ "_Workspace...") Nothing Nothing
        (workspaceOpen >> showWorkspacePane) [] False
    ,AD "FileOpenProject" (__ "Pro_ject...") Nothing Nothing
        (showWorkspacePane >> workspaceTry projectOpen) [] False
    ,AD "FileOpenPackage" (__ "_Package...") Nothing Nothing
        (showWorkspacePane >> projectTry projectAddPackage) [] False
    ,AD "FileOpenFile" (__ "_File...") Nothing (Just "gtk-open")
        fileOpen [] False
    ,AD "FileRecentFiles" (__ "Recent Files") Nothing Nothing (return ()) [] False
    ,AD "FileRecentWorkspaces" (__ "Recent Workspaces") Nothing Nothing (return ()) [] False
    ,AD "FileSave" (__ "_Save") Nothing (Just "gtk-save")
        (void $ fileSave False) [] False
    ,AD "FileSaveAs" (__ "Save _As...") Nothing (Just "gtk-save-as")
        (void $ fileSave True) [] False
    ,AD "FileSaveAll" (__ "Save A_ll") Nothing Nothing
        (void . fileSaveAll $ \ b -> return (bufferName b /= "_Eval.hs")) [] False
    ,AD "FileClose" (__ "_Close") Nothing (Just "gtk-close")
        (void fileClose) [] False
    ,AD "FileCloseWorkspace" (__ "Close Workspace") Nothing Nothing
        workspaceClose [] False
    ,AD "FileCloseAll" (__ "Close All") Nothing Nothing
        (void . fileCloseAll $ \ b -> return (bufferName b /= "_Eval.hs")) [] False
    ,AD "FileCloseAllButPackage" (__ "Close All But Package") Nothing Nothing
        (void fileCloseAllButPackage) [] False
    ,AD "FileCloseAllButWorkspace" (__ "Close All But Workspace") Nothing Nothing
        (void fileCloseAllButWorkspace) [] False
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

    ,AD "EditReformat" (__ "Re_format") Nothing Nothing
        editReformat [] False
    ,AD "EditRefactor" (__ "Ref_actor") Nothing Nothing (return ()) [] False
    ,AD "EditRefactorIfToCase" (__ "_If To Case") Nothing Nothing
        ifToCase [] False
    ,AD "EditRefactorDuplicateDef" (__ "_Duplicate Definition") Nothing Nothing
        duplicateDef [] False
    ,AD "EditRefactorLiftToTopLevel" (__ "_Lift To Top Level") Nothing Nothing
        liftToTopLevel [] False
    ,AD "EditRefactorLiftOneLevel" (__ "Lift _One Level") Nothing Nothing
        liftOneLevel [] False
    ,AD "EditRefactorDemote" (__ "De_mote") Nothing Nothing
        demote [] False
    ,AD "EditRefactorRename" (__ "_Rename") Nothing Nothing
        rename [] False
    ,AD "EditRefactorAddOneParameter" (__ "Add One _Parameter") Nothing Nothing
        addOneParameter [] False
    ,AD "EditRefactorRmOneParameter" (__ "Remove One Parameter") Nothing Nothing
        rmOneParameter [] False
    ,AD "EditRefactorDeleteDef" (__ "Delete Definition") Nothing Nothing
        deleteDef [] False
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
        (align "=") [] False
    ,AD "EditAlignRightArrow" (__ "Align -_>") Nothing Nothing
        (align "->") [] False
    ,AD "EditAlignLeftArrow" (__ "Align _<-") Nothing Nothing
        (align "<-") [] False
    ,AD "EditAlignTypeSig" (__ "Align _::") Nothing Nothing
        (align "::") [] False

    ,AD "Workspace" (__ "_Workspace") Nothing Nothing (return ()) [] False
    ,AD "WorkspaceAddPackage" (__ "_Add Package...") Nothing Nothing
        (showWorkspacePane >> projectTry projectAddPackage) [] False
    ,AD "WorkspaceAddPackageCopy" (__ "_Add Copy Of Installed Package...") Nothing Nothing
        (showWorkspacePane >> projectTry projectPackageClone) [] False
    ,AD "CleanWorkspace" (__ "Cl_ean All packages") (Just (__ "Cleans all packages in the workspace")) (Just "ide_clean")
        (workspaceTry workspaceClean) [] False
    ,AD "MakeWorkspace" (__ "_Build All packages") (Just (__ "Builds all of the packages in the workspace")) (Just "ide_configure")
        (workspaceTry workspaceMake) [] False
    ,AD "NextError" (__ "_Next Error") (Just (__ "Go to the next error")) (Just "ide_error_next")
        nextError [] False
    ,AD "PreviousError" (__ "_Previous Error") (Just (__ "Go to the previous error")) (Just "ide_error_prev")
        previousError [] False

    ,AD "RefreshNixProject" (__ "Refresh Ni_x Project") (Just (__ "Refresh Leksah's cached nix environment variables for the active project")) (Just "ide_nix")
        (projectTry projectRefreshNix) [] False

    ,AD "Package" (__ "_Package") Nothing Nothing (return ()) [] False
--    ,AD "RecentPackages" "_Recent Packages" Nothing Nothing (return ()) [] False
    ,AD "PackageEdit" (__ "_Edit") Nothing Nothing (return ()) [] False
    ,AD "EditPackage" (__ "Edit _Package") Nothing Nothing
        (packageTry packageEdit) [] False
    ,AD "EditPackageText" (__ "Edit Package as _Text") Nothing Nothing
        (packageTry packageEditText) [] False
--    ,AD "RemovePackage" "_Close Package" Nothing Nothing
--        removePackage [] False
    ,AD "PackageFlags" (__ "_Flags") (Just (__ "Edit the cabal and stack command line flags used")) Nothing
        (getFlags Nothing >>= \ p -> displayPane p False) [] False

    ,AD "CleanPackage" (__ "Cl_ean") (Just (__ "Cleans the package")) (Just "ide_clean")
        (packageTry packageClean) [] False
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

    ,AD "InstallPackage" (__ "_Install") Nothing Nothing
        (packageTry packageInstall) [] False
    ,AD "HLintPackage" (__ "_HLint") Nothing Nothing
        (packageTry packageHLint) [] False
    ,AD "TestPackage" (__ "Test") Nothing Nothing
        (packageTry packageTest) [] False
    ,AD "BenchPackage" (__ "Benchmark") Nothing Nothing
        (packageTry packageBench) [] False
    ,AD "SdistPackage" (__ "Source Dist") Nothing Nothing
        (packageTry packageSdist) [] False
    ,AD "OpenDocPackage" (__ "_Open Documentation") Nothing Nothing
        (packageTry packageOpenDoc) [] False

    ,AD "Debug" (__ "_Debug") Nothing Nothing (return ()) [] False
    ,AD "StartDebugger" (__ "_Start Debugger") (Just (__ "Starts using the GHCi debugger for build and run")) Nothing
        (packageTry debugStart) [] False
    ,AD "QuitDebugger" (__ "_Quit Debugger") (Just (__ "Quit the GHCi debugger if it is running")) Nothing
        (packageTry debugQuit) [] False
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
        (packageTry debugStop) [] False

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
    ,AD "UpdateMetadataCurrent" (__ "_Update Workspace Data") (Just (__ "Updates data for the current workspace"))
            (Just "ide_rebuild_meta") updateWorkspaceInfo [] False
    ,AD "RebuildMetadataCurrent" (__ "_Rebuild Workspace Data") (Just (__ "Rebuilds data for the current workspace"))
            Nothing rebuildWorkspaceInfo [] False
    ,AD "UpdateMetadataLib" (__ "U_pdate System Data") Nothing Nothing
        updateSystemInfo [] False
    ,AD "RebuildMetadataLib" (__ "R_ebuild System Data") Nothing Nothing
        rebuildSystemInfo [] False

    ,AD "Session" (__ "_Session") Nothing Nothing (return ()) [] False
    ,AD "SaveSession" (__ "_Save Session As...") Nothing Nothing
        saveSessionAsPrompt [] False
    ,AD "LoadSession" (__ "_Load Session") Nothing Nothing
        loadSessionPrompt [] False

    ,AD "Panes" (__ "_Panes") Nothing Nothing (return ()) [] False
    ,AD "ShowBrowser" (__ "Browser") Nothing Nothing
        showBrowser [] False
    ,AD "ShowModules" (__ "Modules") Nothing Nothing
        showModules [] False
    ,AD "ShowInfo" (__ "Symbol _Info") Nothing Nothing
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
        showWorkspacePane [] False

    ,AD "View" (__ "_View") Nothing Nothing (return ()) [] False
    ,AD "ViewMoveLeft" (__ "Move _Left") Nothing Nothing
        (viewMove LeftP) [] False
    ,AD "ViewMoveRight" (__ "Move _Right") Nothing Nothing
        (viewMove RightP) [] False
    ,AD "ViewMoveUp" (__ "Move _Up") Nothing Nothing
        (viewMove TopP) [] False
    ,AD "ViewMoveDown" (__ "Move _Down") Nothing Nothing
        (viewMove BottomP) [] False
    ,AD "ViewSplitHorizontal" (__ "Split T_op/Bottom") Nothing Nothing
        viewSplitHorizontal [] False
    ,AD "ViewSplitVertical" (__ "Split L_eft/Right") Nothing Nothing
        viewSplitVertical [] False
    ,AD "ViewCollapse" (__ "_Collapse Active Split") Nothing Nothing
        viewCollapse [] False
    ,AD "ViewNest" (__ "_Group") Nothing Nothing
        viewNewGroup [] False
    ,AD "ViewDetach" (__ "_Detach") Nothing Nothing
        viewDetachInstrumented [] False
    ,AD "ViewFullScreen" (__ "_Full Screen") Nothing Nothing
        viewFullScreen [] True
    ,AD "UseDarkInterface" (__ "_Use Dark Interface") Nothing Nothing
        viewUseDarkInterface [] True
    ,AD "ViewTabsLeft" (__ "Tabs Left") Nothing Nothing
        (viewTabsPos PositionTypeLeft) [] False
    ,AD "ViewTabsRight" (__ "Tabs Right") Nothing Nothing
        (viewTabsPos PositionTypeRight) [] False
    ,AD "ViewTabsUp" (__ "Tabs Up") Nothing Nothing
        (viewTabsPos PositionTypeTop) [] False
    ,AD "ViewTabsDown" (__ "Tabs Down") Nothing Nothing
        (viewTabsPos PositionTypeBottom) [] False
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

    ,AD "Tools" (__ "_Tools") Nothing Nothing (return ()) [] False
    ,AD "PrefsEdit" (__ "_Preferences") Nothing Nothing
        runPreferencesDialog [] False

    ,AD "Help" (__ "_Help") Nothing Nothing (return ()) [] False
    ,AD "HelpDebug" (__ "Debug") Nothing Nothing (do
            p <- readIDE activePack
            ideMessage Normal (T.pack $ show p)) [] False
--    ,AD "HelpDebug2" "Debug2" (Just "<Ctrl>d") Nothing dbgInstalledPackageInfo [] False
    ,AD "HelpManual" (__ "Manual") Nothing Nothing (openBrowser "http://leksah.org/leksah_manual.pdf") [] False
    ,AD "HelpHomepage" (__ "Homepage") Nothing Nothing (openBrowser "http://leksah.org") [] False
    ,AD "HelpAbout" (__ "About") Nothing (Just "gtk-about") (liftIO aboutDialog) [] False

    ,AD "BackgroundBuildToggled" (__ "_Build in the background") (Just (__ "Build in the background and report errors")) (Just "ide_build")
        backgroundBuildToggled [] True
    ,AD "MakeDocsToggled" (__ "Build with _documentation") (Just (__ "Make documentation when building")) (Just "ide_doc_build")
        makeDocsToggled [] True
    ,AD "RunUnitTestsToggled" (__ "Run _unit tests when building") (Just (__ "Run unit tests when building")) (Just "ide_test_build")
        runUnitTestsToggled [] True
    ,AD "RunBenchmarksToggled" (__ "_Run benchmarks when building") (Just (__ "Run benchmarks when building")) (Just "ide_bench_build")
        runBenchmarksToggled [] True
    ,AD "MakeModeToggled" (__ "_MakeMode") (Just (__ "Make dependent packages")) (Just "ide_make")
        makeModeToggled [] True
    ,AD "NativeToggled" "_Enable GHC" (Just (__ "Use GHC to compile")) (Just "ide_target_binary")
        nativeToggled [] True
    ,AD "JavaScriptToggled" "_Enable GHCJS" (Just (__ "Use GHCJS to compile")) (Just "ide_target_js")
        javaScriptToggled [] True
    ,AD "DebugToggled" "_Enable GHCi" (Just (__ "Use GHCi debugger to build and run")) (Just "ide_debug")
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
    recentWorkspacesItem <- getRecentWorkspaces
    reifyIDE (\ ideR -> do
        recentFilesMenu    <-  menuNew
        existingRecentFiles <- filterM doesFileExist recentFiles'
        if null existingRecentFiles
            then do
                mi <- menuItemNewWithLabel (T.pack "No recently opened files")
                widgetSetSensitive mi False
                menuShellAppend recentFilesMenu mi
            else
                forM_ recentFiles' $ \s -> do
                    mi <- menuItemNewWithLabel $ T.pack s
                    void $ onMenuItemActivate mi $ reflectIDE (fileOpen' s) ideR
                    menuShellAppend recentFilesMenu mi
        menuItemGetSubmenu recentFilesItem >>= \case
            Just oldSubmenu -> do
                widgetHide oldSubmenu
                widgetDestroy oldSubmenu
            Nothing -> return ()
        menuItemSetSubmenu recentFilesItem (Just recentFilesMenu)
        widgetShowAll recentFilesMenu

        recentWorkspacesMenu    <-  menuNew
        existingRecentWorkspaces <- filterM doesFileExist recentWorkspaces'
        if null existingRecentWorkspaces
            then do
                mi <- menuItemNewWithLabel (T.pack "No recently opened workspaces")
                widgetSetSensitive mi False
                menuShellAppend recentWorkspacesMenu mi
            else
                forM_ existingRecentWorkspaces $ \s -> do
                    mi <- menuItemNewWithLabel $ T.pack s
                    void $ onMenuItemActivate mi $ reflectIDE (workspaceOpenThis True s >> showWorkspacePane) ideR
                    menuShellAppend recentWorkspacesMenu mi

        menuItemGetSubmenu recentWorkspacesItem >>= \case
            Just oldSubmenu -> do
                widgetHide oldSubmenu
                widgetDestroy oldSubmenu
            Nothing -> return ()
        menuItemSetSubmenu recentWorkspacesItem (Just recentWorkspacesMenu)
        widgetShowAll recentWorkspacesMenu)


--
-- | Building the Menu
--
makeMenu :: UIManager -> [ActionDescr IDERef] -> Text -> IDEAction
makeMenu uiMgr actions description = reifyIDE $ \ideR -> do
    actionGroupGlobal <- actionGroupNew "global"
    mapM_ (actm ideR actionGroupGlobal) actions
    uIManagerInsertActionGroup uiMgr actionGroupGlobal 1
    void $ uIManagerAddUiFromString uiMgr description (-1)
  where
    actm ideR ag AD{..} = do
        let (acc,accString) = case accelerator of
                                [] -> (Just "", "=" <> name)
                                a:_ -> (Just a, a <> "=" <> name)
        if isToggle
            then do
                act <- toggleActionNew name (Just label) tooltip stockID
                void . onToggleActionToggled act $ doAction action ideR accString
                actionGroupAddActionWithAccel ag act acc
            else do
                act <- actionNew name (Just label) tooltip stockID
                void . onActionActivate act $ doAction action ideR accString
                actionGroupAddActionWithAccel ag act acc
    doAction ideAction ideR accStr =
        reflectIDE (do
            void ideAction
            void $ triggerEventIDE (StatusbarChanged [CompartmentCommand accStr])) ideR

getMenuAndToolbars :: MonadIO m => UIManager -> m (AccelGroup, MenuBar, Toolbar)
getMenuAndToolbars uiMgr = do
    accGroup <- uIManagerGetAccelGroup uiMgr
    menu     <- uIManagerGetWidget uiMgr "/ui/menubar" >>= liftIO . unsafeCastTo MenuBar . fromMaybe (error "Failed to get /ui/menubar!")
    toolbar' <- uIManagerGetWidget uiMgr "/ui/toolbar" >>= liftIO . unsafeCastTo Toolbar . fromMaybe (error "Failed to get /ui/toolbar!")
    toolbarSetStyle toolbar' ToolbarStyleIcons
    toolbarSetIconSize toolbar' IconSizeLargeToolbar
    widgetSetSizeRequest toolbar' 700 (-1)
    return (accGroup,menu,toolbar')

textPopupMenu :: IDERef -> Menu -> IO ()
textPopupMenu ideR menu = do
    let reflectIDE_ x = reflectIDE x ideR
    items <- containerGetChildren menu
    mi1 <- menuItemNewWithLabel (__ "Eval")
    void . onMenuItemActivate mi1 $ reflectIDE_ debugExecuteSelection
    menuShellAppend menu mi1
    mi11 <- menuItemNewWithLabel (__ "Eval & Insert")
    void . onMenuItemActivate mi11 $
      reflectIDE_ debugExecuteAndShowSelection
    menuShellAppend menu mi11
    mi12 <- menuItemNewWithLabel (__ "Step")
    void . onMenuItemActivate mi12 $ reflectIDE_ debugStepExpression
    menuShellAppend menu mi12
    mi13 <- menuItemNewWithLabel (__ "Trace")
    void . onMenuItemActivate mi13 $ reflectIDE_ debugTraceExpression
    menuShellAppend menu mi13
    mi16 <- menuItemNewWithLabel (__ "Set Breakpoint")
    void . onMenuItemActivate mi16 $ reflectIDE_ debugSetBreakpoint
    menuShellAppend menu mi16
    sep1 <- separatorMenuItemNew >>= liftIO . toMenuItem
    menuShellAppend menu sep1
    mi14 <- menuItemNewWithLabel (__ "Type")
    void . onMenuItemActivate mi14 $ reflectIDE_ debugType
    menuShellAppend menu mi14
    mi141 <- menuItemNewWithLabel (__ "Info")
    void . onMenuItemActivate mi141 $ reflectIDE_ debugInformation
    menuShellAppend menu mi141
    mi15 <- menuItemNewWithLabel (__ "Kind")
    void . onMenuItemActivate mi15 $ reflectIDE_ debugKind
    menuShellAppend menu mi15
    sep2 <- separatorMenuItemNew >>= liftIO . toMenuItem
    menuShellAppend menu sep2
    mi2 <- menuItemNewWithLabel (__ "Find (text)")
    void . onMenuItemActivate mi2 $ reflectIDE_ (editFindInc Initial)
    menuShellAppend menu mi2
    mi3 <- menuItemNewWithLabel (__ "Search (metadata)")
    void . onMenuItemActivate mi3 $
      reflectIDE_ $ do
         mbtext <- snd <$> selectedTextOrCurrentIdentifier -- if no text selected, search for current identifier
         searchPane <- getSearch Nothing
         case mbtext of
              Just t  -> searchMetaGUI searchPane t
              Nothing -> ideMessage Normal (__ "No identifier selected")
    menuShellAppend menu mi3
    mi4 <- menuItemNewWithLabel (__ "Upload to lpaste.net")
    void . onMenuItemActivate mi4 $ reflectIDE_ uploadToLpaste
    menuShellAppend menu mi4
    let interpretingEntries = [mi16]
    let interpretingSelEntries
          = [mi1, mi11, mi12,
             mi13, mi14, mi141,
             mi15]
    let otherEntries = [mi2, mi3, mi4]
    -- isInterpreting' <- (reflectIDE isInterpreting ideR)
    _selected <- reflectIDE selectedText ideR
--    unless isInterpreting'
--        $ mapM_ (\w -> widgetSetSensitive w False) (interpretingEntries ++ interpretingSelEntries)
--    unless (isJust selected)
--        $ mapM_ (\w -> widgetSetSensitive w False) (otherEntries ++ interpretingSelEntries)
    mapM_ widgetShow interpretingEntries
    mapM_ widgetShow interpretingSelEntries
    mapM_ widgetShow
      (sep1 : sep2 : otherEntries)
    mapM_ widgetHide $ take 2 (reverse items)

canQuit :: IDEM Bool
canQuit = do
    modifyIDE_ (\ide -> ide{currentState = IsShuttingDown})
    p <- readIDE prefs
    when (saveSessionOnClose p) saveSession
    can <- fileCloseAll (\_ -> return True)
    unless can $ modifyIDE_ (\ide -> ide{currentState = IsRunning})
    return can

-- | Quit ide
quit :: IDEAction
quit = do
    can <- canQuit
    app <- readIDE application
    when can $ applicationQuit app

--
-- | Show the about dialog
--
aboutDialog :: IO ()
aboutDialog = do
    liftIO $ debugM "leksah" "aboutDialog"
    d <- aboutDialogNew
    dd <- getDataDir
    (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
    license <- catch (T.readFile $ dd </> T.unpack (__ "LICENSE")) (\ (_ :: SomeException) -> return "")
    setAboutDialogProgramName d "Leksah"
    setAboutDialogVersion d . T.pack $ showVersion version
    setAboutDialogCopyright d $ __ "Copyright 2007-" <> T.pack (show year) <> " Jürgen Nicklisch-Franken, Hamish Mackenzie,\nJacco Krijnen, JP Moresmau"
    setAboutDialogComments d $ __ "An integrated development environement (IDE) for the " <>
                               __ "programming language Haskell and the Glasgow Haskell Compiler"
    setAboutDialogLicense d license
    setAboutDialogWebsite d "http://leksah.org/"
    aboutDialogSetAuthors d ["Jürgen Nicklisch-Franken","Hamish Mackenzie","Jacco Krijnen","JP Moresmau"]
    void $ dialogRun d
    widgetDestroy d
    return ()


newIcons :: IO ()
newIcons = catch (do
        iconFactory <- iconFactoryNew
        dataDir <- getDataDir

        mapM_ (loadIcon dataDir iconFactory)
            ["ide_class","ide_configure","ide_data","ide_error_next",
            "ide_error_prev","ide_function","ide_instance", "ide_konstructor","ide_make",
            "ide_method","ide_newtype","ide_other","ide_rule","ide_run","ide_slot",
            "ide_source","ide_type","leksah", "ide_reexported", "ide_clean", "ide_link", "ide_build",
            "ide_debug", "ide_step", "ide_local", "ide_module", "ide_continue", "ide_rebuild_meta",
            "ide_empty","ide_source_local", "ide_js", "ide_folder", "ide_source_folder",
            "ide_cabal_file", "ide_package", "ide_component", "ide_source_dependency", "ide_error",
            "ide_warning", "ide_suggestion", "ide_git", "ide_test_build", "ide_bench_build", "ide_doc_build",
            "ide_target_binary", "ide_target_js", "ide_nix" ]
        iconFactoryAddDefault iconFactory)
    (\(e :: SomeException) -> getDataDir >>= \dataDir -> throwIDE (T.pack $ printf (__ "Can't load icons from %s %s") dataDir (show e)))
    where
    loadIcon dataDir iconFactory name = do
        pb      <-  pixbufNewFromFile $ dataDir </> "pics" </> (name <> ".png")
        icon    <-  iconSetNewFromPixbuf pb
        iconFactoryAdd iconFactory (T.pack name) icon
        iconThemeAddBuiltinIcon (T.pack name) 16 pb

--newIcons :: IO ()
--newIcons = catch (do
--        dataDir <- getDataDir
--        iconTheme <- iconThemeGetDefault
--        iconThemeAddResourcePath iconTheme . T.pack $ dataDir </> "pics")
--    (\(e :: SomeException) -> getDataDir >>= \dataDir -> throwIDE (T.pack $ printf (__ "Can't load icons from %s %s") dataDir (show e)))

setSensitivity :: [(SensitivityMask, Bool)] -> IDEAction
setSensitivity = mapM_ setSensitivitySingle
    where   setSensitivitySingle (sens,bool) = do
                actions <- getActionsFor sens
                mapM_ (`actionSetSensitive` bool) actions
                let additionalActions = getAdditionalActionsFor sens
                mapM_ (\a -> a bool) additionalActions

getActionsFor :: SensitivityMask -> IDEM [Action]
getActionsFor SensitivityForwardHist = getActionsFor' ["ViewHistoryForth"]
getActionsFor SensitivityBackwardHist = getActionsFor' ["ViewHistoryBack"]
getActionsFor SensitivityProjectActive = getActionsFor'
    ["EditPackage", "PackageFlags", "BuildPackage"
    ,"DocPackage", "CleanPackage", "CopyPackage", "RunPackage"
    ,"InstallPackage", "TestPackage","SdistPackage"
    ,"OpenDocPackage","FileCloseAll"]
getActionsFor SensitivityError = getActionsFor' ["NextError", "PreviousError"]
getActionsFor SensitivityEditor = getActionsFor' ["EditUndo", "EditRedo",
        "EditGotoLine","EditComment", "EditUncomment", "EditSelectAll",
        "EditShiftLeft", "EditShiftRight","FileClose",
        "OpenDocu"
        ]
getActionsFor SensitivityInterpreting = getActionsFor' ["QuitDebugger"]
getActionsFor SensitivityWorkspaceOpen = return [] --TODO add here

getActionsFor' :: [Text] -> IDEM[Action]
getActionsFor' = fmap catMaybes . mapM getActionFor
    where
        getActionFor string = do
            uiMgr <- getUiManager
            uIManagerGetActionGroups uiMgr >>= \case
                actionGroup:_ -> do
                    res <- actionGroupGetAction actionGroup string
                    when (isNothing res) $ ideMessage Normal $ T.pack $ printf (__ "Can't find UI Action %s") (T.unpack string)
                    return res
                _ -> return Nothing

getAdditionalActionsFor :: SensitivityMask -> [Bool -> IDEAction]
getAdditionalActionsFor SensitivityInterpreting = [setSensitivityDebugger]
getAdditionalActionsFor _ = []

viewDetachInstrumented :: IDEAction
viewDetachInstrumented = do
    mbPair <- viewDetach
    case mbPair of
        Nothing     -> return ()
        Just (win, _wid) -> do
            instrumentSecWindow win
            widgetShowAll win

viewUseDarkInterface :: IDEAction
viewUseDarkInterface = do
    useDark <- getDarkState
    prefs <- readIDE prefs
    when (useDark /= darkUserInterface prefs) $ do
        let prefs' = prefs {darkUserInterface = useDark}
        modifyIDE_ (\ide -> ide {prefs = prefs'})
        applyInterfaceTheme


instrumentWindow :: IsWidget topWidget => Window -> Prefs -> topWidget -> IDEAction
instrumentWindow win prefs topWidget = do
    -- sets the icon
    ideR <- ask
    uiMgr <- getUiManager
    dataDir <- getDataDir
    let iconPath = dataDir </> "pics" </> "leksah.png"
    iconExists  <- liftIO $ doesFileExist iconPath
    when iconExists $
        windowSetIconFromFile win iconPath
    vb <- boxNew OrientationVertical 1  -- Top-level vbox
    widgetSetName vb "topBox"
    (acc,menu,toolbar) <- getMenuAndToolbars uiMgr
    boxPackStart' vb menu PackNatural 0
    boxPackStart' vb toolbar PackNatural 0
    boxPackStart' vb topWidget PackGrow 0
    modifyIDE_ (\ide -> ide{toolbar = (True,Just toolbar)})
    findbar <- constructFindReplace
    boxPackStart' vb findbar PackNatural 0
    statusBar <- buildStatusbar
    boxPackEnd' vb statusBar PackNatural 0
    void . onWidgetKeyPressEvent win $ handleSpecialKeystrokes ideR
    windowAddAccelGroup win acc
    containerAdd win vb
    setBackgroundBuildToggled (backgroundBuild prefs)
    setNativeToggled          (native          prefs)
    setJavaScriptToggled      (javaScript      prefs)
    setDebugToggled           (debug           prefs)
    setMakeDocs               (makeDocs        prefs)
    setRunUnitTests           (runUnitTests    prefs)
    setRunBenchmarks          (runBenchmarks   prefs)
    setMakeModeToggled        (makeMode        prefs)

instrumentSecWindow :: Window -> IDEAction
instrumentSecWindow win = do
    liftIO $ debugM "leksah" "instrumentSecWindow"
    ideR <- ask
    uiMgr <- getUiManager
    liftIO $ do
        dataDir <- getDataDir
        let iconPath = dataDir </> "data" </> "leksah.png"
        iconExists  <-  doesFileExist iconPath
        when iconExists $
            windowSetIconFromFile win iconPath

        (acc,_,_) <-  getMenuAndToolbars uiMgr
        windowAddAccelGroup win acc
        void . onWidgetKeyPressEvent win $ handleSpecialKeystrokes ideR

--
-- | Callback function for onKeyPress of the main window, so 'preprocess' any key
--
handleSpecialKeystrokes :: IDERef -> EventKey -> IO Bool
handleSpecialKeystrokes ideR e = do
  keyVal <- getEventKeyKeyval e
  name <- fromMaybe "" <$> keyvalName keyVal
  mods <- getEventKeyState e
  char <- toEnum . fromIntegral <$> keyvalToUnicode keyVal
  liftIO $ (`reflectIDE` ideR) $ do
    prefs' <- readIDE prefs
    case (name, mods) of
        (tab, [ModifierTypeControlMask]) | (tab == "Tab" || tab == "ISO_Left_Tab")
                                && useCtrlTabFlipping prefs'  -> do
            flipDown
            return True
        (tab, [ModifierTypeShiftMask, ModifierTypeControlMask]) | (tab == "Tab" || tab == "ISO_Left_Tab")
                                && useCtrlTabFlipping prefs'  -> do
            flipUp
            return True
        _                                                     -> do
                when (candyState prefs') (editKeystrokeCandy char)
                sk  <- readIDE specialKey
                sks <- readIDE specialKeys
                case sk of
                    Nothing ->
                        case Map.lookup (keyVal,sort mods) sks of
                            Nothing -> do
                                void $ triggerEventIDE (StatusbarChanged [CompartmentCommand ""])
                                return False
                            Just m -> do
                                let sym = printMods mods <> name
                                void $ triggerEventIDE (StatusbarChanged [CompartmentCommand sym])
                                modifyIDE_ (\ide -> ide{specialKey = Just (m,sym)})
                                return True
                    Just (m,sym) -> do
                        case Map.lookup (keyVal,sort mods) m of
                            Nothing ->
                                void $ triggerEventIDE (StatusbarChanged [CompartmentCommand
                                    (sym <> printMods mods <> name <> "?")])
                            Just (AD actname _ _ _ ideAction _ _) -> do
                                void $ triggerEventIDE (StatusbarChanged [CompartmentCommand
                                    (sym <> " " <> printMods mods <> name <> "=" <> actname)])
                                ideAction
                        modifyIDE_ (\ide -> ide{specialKey = Nothing})
                        return True
    where
    printMods :: [ModifierType] -> Text
    printMods = mconcat . map (T.pack . show)

setSymbolThread :: MVar SymbolEvent -> IDEAction
setSymbolThread mvar = do
    ideR <- ask
    void . liftIO . forkIO . (`reflectIDE` ideR) $
        loop (SymbolEvent "" Nothing False False (0, 0), return ())
  where
    loop :: (SymbolEvent, IDEAction) -> IDEAction
    loop (lastEvent, reshow) = do
        s <- liftIO $ takeMVar mvar
        if s == lastEvent
            then do
                reshow
                loop (lastEvent, reshow)
            else do
                r <- setSymbol s
                loop (s, r)
    setSymbol :: SymbolEvent -> IDEM IDEAction
    setSymbol s@(SymbolEvent symbol (Just (file, (sLine, sCol), (eLine, eCol))) activatePanes openDefinition typeTipLocation) = do
        let fallback = void $ setSymbol s{location = Nothing}
        prefs <- readIDE prefs
        if debug prefs
            then
                belongsToPackages file >>= \case
                    [] -> fallback >> return (return ())
                    (project, package):_ -> do
                        setTTMVar :: MVar IDEAction <- liftIO newEmptyMVar
                        lookupMVar :: MVar (Maybe IDEAction) <- liftIO newEmptyMVar
                        result :: MVar Bool <- liftIO newEmptyMVar
                        let f = case pjTool project of
                                    CabalTool -> makeRelative (ipdPackageDir package) file
                                    StackTool -> file
                        postAsyncIDE . workspaceTry . (`runProject` project) . (`runPackage` package) . tryDebug $ do
                            defaultLogLaunch <- lift getDefaultLogLaunch
                            debugCommand (":type-at "
                                            <> T.pack (f <> " " <> show (succ sLine) <> " " <> show (succ sCol) <> " " <> show (succ eLine) <> " " <> show (succ eCol))
                                            <> " " <> symbol) $ do
                                lines' <- foldOutputLines defaultLogLaunch (\log logLaunch t output ->
                                    case output of
                                        ToolInput  line -> do
                                            liftIO . void $ appendLog log logLaunch (line <> "\n") InputTag
                                            return t
                                        ToolOutput line -> do
                                            liftIO . void $ appendLog log logLaunch (line <> "\n") InfoTag
                                            return $ t <> [line]
                                        ToolError  line -> do
                                            liftIO . void $ appendLog log logLaunch (line <> "\n") ErrorTag
                                            return t
                                        ToolPrompt _    -> do
                                            liftIO . void $ defaultLineLogger' log logLaunch output
                                            return t
                                        ToolExit _      -> do
                                            liftIO . void $ appendLog log logLaunch "X--X--X ghci process exited unexpectedly X--X--X" FrameTag
                                            return t) []
                                liftIO . void . tryPutMVar setTTMVar $
                                    if null lines'
                                        then return ()
                                        else postAsyncIDE . setTypeTip typeTipLocation $ T.concat $ intersperse "\n" lines'
                            debugCommand (":loc-at "
                                            <> T.pack (f <> " " <> show (succ sLine) <> " " <> show (succ sCol) <> " " <> show (succ eLine) <> " " <> show (succ eCol))
                                            <> " " <> symbol) $ do
                                worked <- foldOutputLines defaultLogLaunch (\log logLaunch worked output ->
                                    case output of
                                        ToolInput  line -> do
                                            void . liftIO $ appendLog log logLaunch (line <> "\n") InputTag
                                            return worked
                                        ToolOutput line -> do
                                            void . liftIO $ appendLog log logLaunch (line <> "\n") InfoTag
                                            case parseOnly srcSpanParser line of
                                                Right (SrcSpan f' sl sc el ec) -> do
                                                    void . liftIO . tryPutMVar lookupMVar . Just . when openDefinition . postAsyncIDE . void $ goToSourceDefinition (ipdPackageDir package) (Location f' sl (succ sc) el ec)
                                                    return True
                                                Left _err -> return worked
                                        ToolError  line -> do
                                            void . liftIO $ appendLog log logLaunch (line <> "\n") InfoTag
                                            case T.splitOn ":" line of
                                                [p, _, m] ->
                                                    case (simpleParse . T.unpack $ T.takeWhile (/='@') p, simpleParse $ T.unpack m) of
                                                        (Just pid, Just mName) -> do
                                                            descrs <- getSymbols symbol
                                                            case filter (\case
                                                                    Real rd -> dscMbModu' rd == Just (PM pid mName)
                                                                    _ -> False) descrs of
                                                                [a] -> do
                                                                    void . liftIO . tryPutMVar lookupMVar . Just . postAsyncIDE $ selectIdentifier a activatePanes openDefinition
                                                                    return True
                                                                _   -> return worked
                                                        _ -> return worked
                                                _ -> return worked
                                        ToolPrompt _    -> do
                                            void . liftIO $ defaultLineLogger' log logLaunch output
                                            return worked
                                        ToolExit _      -> do
                                            void . liftIO $ appendLog log logLaunch "X--X--X ghci process exited unexpectedly X--X--X" FrameTag
                                            return worked) False
                                liftIO $ putMVar result worked
                        void . liftIO . forkIO $ do
                            threadDelay 2000000
                            void . tryPutMVar setTTMVar $ return ()
                            void $ tryPutMVar lookupMVar Nothing
                        setTT <- liftIO $ takeMVar setTTMVar
                        setTT
                        liftIO (takeMVar lookupMVar) >>= \case
                            Nothing -> do
                                ideMessage Normal "Timed out looking up symbol with ghci"
                                fallback
                                void . liftIO $ takeMVar result
                            Just l -> do
                                l
                                liftIO (takeMVar result) >>= (`unless` fallback)
                        return setTT
            else fallback >> return (return ())
    setSymbol (SymbolEvent symbol Nothing activatePanes openDefinition _typeTipLocation) = do
        postSyncIDE $ do
            search <- getSearch Nothing
            let unqual = T.concat . intersperse "." . dropQual $ T.splitOn "." symbol
            descrs <- getSymbols unqual
            case filter (not . isReexported) descrs of
                []     -> return ()
                [a]   -> selectIdentifier a activatePanes openDefinition
                [a, b] -> if isJust (dscMbModu a) && dscMbModu a == dscMbModu b &&
                            isNear (dscMbLocation a) (dscMbLocation b)
                                then selectIdentifier a activatePanes openDefinition
                                else when activatePanes $ setChoices search [a,b]
                l      -> when activatePanes $ setChoices search l
        return (return ())
    isNear (Just a) (Just b) = abs (locationSLine a - locationSLine b) <= 3
    isNear _ _               = False
    dropQual [] = []
    dropQual [a] = [a]
    dropQual (q:rest) = case T.uncons q of
                            Just (l, _) | isUpper l -> dropQual rest
                            _ -> q:rest

getSymbols :: Text -> IDEM [Descr]
getSymbols name =
    getWorkspaceInfo >>= \case
        Nothing -> return []
        Just (GenScopeC (PackScope _ symbolTable1),
              GenScopeC (PackScope _ symbolTable2)) -> return $
                    getIdentifierDescr name symbolTable1 symbolTable2

--
-- | Register handlers for IDE events
--
registerLeksahEvents :: IDEAction
registerLeksahEvents =    do
    stRef   <-  ask
    liftIO $ debugM "leksah" "registerLeksahEvents"
    void . registerEvent stRef "LogMessage" $
        \e@(LogMessage s t) -> do
            postAsyncIDE $ do
                log <- getLog
                defaultLogLaunch <- getDefaultLogLaunch
                void . liftIO $ appendLog log defaultLogLaunch s t
            return e
    setSymbolMVar <- liftIO newEmptyMVar
    setSymbolThread setSymbolMVar
    void . registerEvent stRef "SelectInfo" $
        \e@(SelectInfo se)      -> liftIO $ tryTakeMVar setSymbolMVar >> putMVar setSymbolMVar se >> return e
    void . registerEvent stRef "SelectIdent" $
        \e@(SelectIdent id')     -> selectIdentifier id' True False >> return e
    void . registerEvent stRef "InfoChanged" $
        \e@(InfoChanged b)      -> reloadKeepSelection b >> return e
    void . registerEvent stRef "UpdateWorkspaceInfo" $
        \e@(UpdateWorkspaceInfo sys) -> (if sys then updateSystemInfo else updateWorkspaceInfo) >> return e
    void . registerEvent stRef "WorkspaceChanged" $
        \e@(WorkspaceChanged showPane updateFileCache) -> do
                                     postAsyncIDE $ do
                                         refreshWorkspacePane
                                         when showPane
                                            showWorkspacePane
                                         when updateFileCache $
                                            modifyIDE_ (\ide -> ide{bufferProjCache = Map.empty})
                                     return e
    void . registerEvent stRef "RecordHistory" $
        \rh@(RecordHistory h)   -> recordHistory h >> return rh
    void . registerEvent stRef "Sensitivity" $
        \s@(Sensitivity h)      -> setSensitivity h >> return s
    void . registerEvent stRef "SearchMeta" $
        \e@(SearchMeta string)  -> getSearch Nothing >>= flip searchMetaGUI string >> return e
    void . registerEvent stRef "StartFindInitial" $
        \e@StartFindInitial     -> editFindInc Initial >> return e
    void . registerEvent stRef "LoadSession" $
        \e@(LoadSession fp)     -> loadSession fp >> return e
    void . registerEvent stRef "SaveSession" $
        \e@(SaveSession fp)     -> saveSessionAs fp Nothing >> return e
    void . registerEvent stRef "UpdateRecent" $
        \e@UpdateRecent         -> updateRecentEntries >> return e
    void . registerEvent stRef "VariablesChanged" $
        \e@VariablesChanged     -> fillVariablesListQuiet >> return e
    void . registerEvent stRef "ErrorChanged" $
        \e@(ErrorChanged show') -> postAsyncIDE (fillErrorList show') >> return e
    void . registerEvent stRef "ErrorAdded" $
        \e@(ErrorAdded show' i ref) -> postAsyncIDE (addErrorToList show' i ref) >> return e
    void . registerEvent stRef "ErrorsRemoved" $
        \e@(ErrorsRemoved show' toRemove) -> postAsyncIDE (removeErrorsFromList show' toRemove) >> return e
    void . registerEvent stRef "CurrentErrorChanged" $
        \e@(CurrentErrorChanged mbLogRef) -> postAsyncIDE (selectRef mbLogRef)  >> return e
    void . registerEvent stRef "BreakpointChanged" $
        \e@BreakpointChanged    -> postAsyncIDE fillBreakpointList >> return e
    void . registerEvent stRef "CurrentBreakChanged" $
        \e@(CurrentBreakChanged mbLogRef) -> postAsyncIDE (do
            selectRef mbLogRef
            selectBreak mbLogRef) >> return e
    void . registerEvent stRef "TraceChanged" $
        \e@TraceChanged         -> fillTraceList >> return e
    void . registerEvent stRef "GotoDefinition" $
        \e@(GotoDefinition descr) -> goToDefinition descr >> return e
    void . registerEvent stRef "GetTextPopup" $
        \(GetTextPopup _)     -> return (GetTextPopup (Just textPopupMenu))
    void . registerEvent stRef "StatusbarChanged" $
        \e@(StatusbarChanged args) -> changeStatusbar args >> return e
    void . registerEvent stRef "SelectSrcSpan" $
        \e@(SelectSrcSpan mbSpan) -> selectMatchingErrors mbSpan >> return e
    void . registerEvent stRef "SavedFile" $
        \e@(SavedFile file) -> do
              prefs <- readIDE prefs
              when (hlintOnSave prefs && takeExtension file `elem` [".hs", ".lhs"]) $
                  scheduleHLint (Right file)
              return e
    void . registerEvent stRef "DebugStart" $
        \e@(DebugStart _projectAndPackage) -> redrawWorkspacePane >> return e
    void . registerEvent stRef "DebugStop" $
        \e@(DebugStop _projectAndPackage) -> redrawWorkspacePane >> return e
    void . registerEvent stRef "QuitToRestart" $
        \e@QuitToRestart -> do
            can <- canQuit
            app <- readIDE application
            when can $ do
                readIDE exitCode >>= liftIO . (`writeIORef` ExitFailure 2) -- Exit code to trigger leksah.sh to restart
                applicationQuit app
            return e



