{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Web.Command where

import Control.Lens
       (Getter, to, makePrisms, view)

import Data.Text (Text)

import IDE.Core.State
       (readIDE, Prefs(..), prefs, PackageAction, ProjectAction,
        WorkspaceAction, IDEAction, __, IDE)
import IDE.Debug
       (debugContinue, debugStepModule, debugStepLocal, debugStep,
        debugToggled)
import IDE.Gtk.Package
       (makeModeToggled, runBenchmarksToggled, runUnitTestsToggled,
        makeDocsToggled, javaScriptToggled, nativeToggled,
        backgroundBuildToggled, packageRunJavaScript, packageRun)
import IDE.Metainfo.Provider (updateWorkspaceInfo)
import IDE.Package (packageClean, projectRefreshNix)
import IDE.Gtk.Workspaces
       (projectTry, packageTry, workspaceTry, makePackage)

data Command =
    CommandIDEAction Text Text IDEAction
  | CommandIDEToggleAction Text Text IDEAction (IDE -> Bool)
  | CommandWorkspaceAction Text Text WorkspaceAction
  | CommandProjectAction Text Text ProjectAction
  | CommandPackageAction Text Text PackageAction
  | CommandDebugAction Text Text IDEAction
  | CommandFileOpen
  | CommandFileSave
  | CommandFind
  | CommandUndo
  | CommandRedo
  | CommandNextError
  | CommandPreviousError
  | CommandFlipDown
  | CommandFlipUp
  | CommandFlipDone

makePrisms ''Command

commandAction :: Getter Command (Maybe IDEAction)
commandAction = to $ \case
  (CommandIDEAction       _ _ a)   -> Just a
  (CommandIDEToggleAction _ _ a _) -> Just a
  (CommandWorkspaceAction _ _ a)   -> Just (workspaceTry a)
  (CommandProjectAction   _ _ a)   -> Just (projectTry a)
  (CommandPackageAction   _ _ a)   -> Just (packageTry a)
  (CommandDebugAction     _ _ a)   -> Just a
  _ -> Nothing

commandImageAndTip :: Command -> (Text, Text)
commandImageAndTip (CommandIDEAction img tip _) = (img, tip)
commandImageAndTip (CommandIDEToggleAction img tip _ _) = (img, tip)
commandImageAndTip (CommandWorkspaceAction img tip _) = (img, tip)
commandImageAndTip (CommandProjectAction img tip _) = (img, tip)
commandImageAndTip (CommandPackageAction img tip _) = (img, tip)
commandImageAndTip (CommandDebugAction img tip _) = (img, tip)
commandImageAndTip CommandFileOpen = ("/pics/tango/actions/document-open.svg", __ "Opens an existing file")
commandImageAndTip CommandFileSave = ("/pics/tango/actions/document-save.svg", __ "Saves the current buffer")
commandImageAndTip CommandFind = ("/pics/tango/actions/edit-find.svg", __ "Opens an existing file")
commandImageAndTip CommandUndo = ("/pics/tango/actions/edit-undo.svg", __ "Opens an existing file")
commandImageAndTip CommandRedo = ("/pics/tango/actions/edit-redo.svg", __ "Opens an existing file")
commandImageAndTip CommandNextError = ("/pics/ide_error_next.png", __ "Go to the next error")
commandImageAndTip CommandPreviousError = ("/pics/ide_error_prev.png", __ "Go to the previous error")
commandImageAndTip _ = ("", "")

commandGetToggleState :: Command -> Maybe (IDE -> Bool)
commandGetToggleState (CommandIDEToggleAction _ _ _ f) = Just f
commandGetToggleState _ = Nothing

commandAddModule, commandRefreshNix, commandPackageClean
  , commandPackageBuild, commandPackageRun, commandPackageRunJavascript
  , commandToggleBackgroundBuild, commandToggleNative, commandToggleJavaScript
  , commandToggleDebug, commandToggleMakeDocs, commandToggleTest
  , commandToggleRunBenchmarks, commandToggleMakeDependents
  , commandUpdateWorkspaceInfo, commandDebugStep, commandDebugStepLocal
  , commandDebugStepModule, commandDebugContinue :: Command
commandAddModule = CommandPackageAction
  "/pics/tango/actions/document-new.svg"
  (__ "Creates a new Haskell module")
  (return ())

commandRefreshNix = CommandProjectAction
  "/pics/ide_nix.svg"
  (__ "Refresh Leksah's cached nix environment variables for the active project")
  projectRefreshNix

commandPackageClean = CommandPackageAction
  "/pics/ide_clean.png"
  (__ "Cleans the package")
  packageClean

commandPackageBuild = CommandPackageAction
  "/pics/ide_make.png"
  (__ "Builds the package")
  makePackage

commandPackageRun = CommandPackageAction
  "/pics/ide_run.png"
  (__ "Runs the package")
  packageRun

commandPackageRunJavascript = CommandPackageAction
  "/pics/ide_js.png"
  (__ "Run jsexe created by GHCJS")
  packageRunJavaScript

commandToggleBackgroundBuild = CommandIDEToggleAction
  "/pics/ide_build.png"
  (__ "Build in the background and report errors")
  backgroundBuildToggled
  (view $ prefs . to backgroundBuild)

commandToggleNative = CommandIDEToggleAction
  "/pics/ide_target_binary.svg"
  (__ "Use GHC to compile")
  nativeToggled
  (view $ prefs . to native)

commandToggleJavaScript = CommandIDEToggleAction
  "/pics/ide_target_js.svg"
  (__ "Use GHCJS to compile")
  javaScriptToggled
  (view $ prefs . to javaScript)

commandToggleDebug = CommandIDEToggleAction
  "/pics/ide_debug.png"
  (__ "Use GHCi debugger to build and run")
  (readIDE prefs >>= debugToggled . not . debug)
  (view $ prefs . to debug)

commandToggleMakeDocs = CommandIDEToggleAction
  "/pics/ide_doc_build.png"
  (__ "Make documentation when building")
  makeDocsToggled
  (view $ prefs . to makeDocs)

commandToggleTest = CommandIDEToggleAction
  "/pics/ide_test_build.png"
  (__ "Run unit tests when building")
  runUnitTestsToggled
  (view $ prefs . to runUnitTests)

commandToggleRunBenchmarks = CommandIDEToggleAction
  "/pics/ide_bench_build.png"
  (__ "Run benchmarks when building")
  runBenchmarksToggled
  (view $ prefs . to runBenchmarks)

commandToggleMakeDependents = CommandIDEToggleAction
  "/pics/ide_make.png"
  (__ "Make dependent packages")
  makeModeToggled
  (view $ prefs . to makeMode)

commandUpdateWorkspaceInfo = CommandIDEAction
  "/pics/ide_rebuild_meta.png"
  (__ "Updates data for the current workspace")
  updateWorkspaceInfo

commandDebugStep = CommandIDEAction
  "/pics/ide_step.png"
  (__ "Single-step after stopping at a breakpoint")
  debugStep

commandDebugStepLocal = CommandIDEAction
  "/pics/ide_local.png"
  (__ "Single-step within the current top-level binding")
  debugStepLocal

commandDebugStepModule = CommandIDEAction
  "/pics/ide_module.png"
  (__ "Single-step restricted to the current module")
  debugStepModule

commandDebugContinue = CommandIDEAction
  "/pics/ide_continue.png"
  (__ "Resume after a breakpoint")
  debugContinue
