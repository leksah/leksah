{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Web.Command where

import Control.Lens
       (Getter, to, makePrisms, view, (%~))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (cons)
import Data.Text (Text)

import IDE.Web.CloseRequest (requestCloseActivePane)
import IDE.Web.TerminalInput
       (sendToActiveTerminal, tmuxCommandActiveTerminal)
import IDE.Web.TransparencyRequest (requestToggleTransparency)
import IDE.Web.SnapRequest (requestSnapWindow)

import IDE.Core.State
       (readIDE, modifyIDE_, Prefs(..), prefs, PackageAction, ProjectAction,
        WorkspaceAction, IDEAction, __, IDE, TallVisibility(..))
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
  | CommandProjectOpen
  | CommandFileSave
  | CommandFind
  | CommandShowPreferences
  | CommandNextError
  | CommandPreviousError
  | CommandFlipDown
  | CommandFlipUp
  | CommandFlipDone
  -- Jump to the next terminal window flagged for attention (bell first, then
  -- activity) — e.g. a teammate that rang the bell wanting input.
  | CommandFocusAlert
  -- Numbered navigation (1-based; see IDE.Web.Main): Cmd+N the Nth split of
  -- the active terminal, Opt+Cmd+N the Nth side-bar pane, Ctrl+Cmd+N the Nth
  -- bottom-bar pane.
  | CommandSelectSplit Int
  | CommandSelectSidePane Int
  | CommandSelectBottomPane Int

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
commandImageAndTip CommandFileOpen = ("/pics/file-open.svg", __ "Opens an existing file")
commandImageAndTip CommandFileSave = ("/pics/file-save.svg", __ "Saves the current buffer")
commandImageAndTip CommandFind = ("/pics/find.svg", __ "Show or hide the find bar")
commandImageAndTip CommandNextError = ("/pics/error-next.svg", __ "Go to the next error")
commandImageAndTip CommandPreviousError = ("/pics/error-prev.svg", __ "Go to the previous error")
commandImageAndTip _ = ("", "")

commandGetToggleState :: Command -> Maybe (IDE -> Bool)
commandGetToggleState (CommandIDEToggleAction _ _ _ f) = Just f
commandGetToggleState _ = Nothing

commandAddModule, commandRefreshNix, commandPackageClean
  , commandPackageBuild, commandPackageRun, commandPackageRunJavascript
  , commandToggleBackgroundBuild, commandToggleNative, commandToggleJavaScript
  , commandToggleDebug, commandToggleMakeDocs, commandToggleTest
  , commandToggleRunBenchmarks, commandToggleMakeDependents
  , commandToggleShowIgnored, commandToggleShowHidden, commandToggleTallPane
  , commandToggleWide1Pane
  , commandUpdateWorkspaceInfo, commandDebugStep, commandDebugStepLocal
  , commandDebugStepModule, commandDebugContinue, commandFileClose :: Command
commandAddModule = CommandPackageAction
  "/pics/new-module.svg"
  (__ "Creates a new Haskell module")
  (return ())

commandRefreshNix = CommandProjectAction
  "/pics/nix.svg"
  (__ "Refresh Leksah's cached nix environment variables for the active project")
  projectRefreshNix

commandPackageClean = CommandPackageAction
  "/pics/clean.svg"
  (__ "Cleans the package")
  packageClean

commandPackageBuild = CommandPackageAction
  "/pics/build.svg"
  (__ "Builds the package")
  makePackage

commandPackageRun = CommandPackageAction
  "/pics/run.svg"
  (__ "Runs the package")
  packageRun

commandPackageRunJavascript = CommandPackageAction
  "/pics/run-js.svg"
  (__ "Run jsexe created by GHCJS")
  packageRunJavaScript

commandToggleBackgroundBuild = CommandIDEToggleAction
  "/pics/background-build.svg"
  (__ "Build in the background and report errors")
  backgroundBuildToggled
  (view $ prefs . to backgroundBuild)

commandToggleNative = CommandIDEToggleAction
  "/pics/target-native.svg"
  (__ "Use GHC to compile")
  nativeToggled
  (view $ prefs . to native)

commandToggleJavaScript = CommandIDEToggleAction
  "/pics/target-js.svg"
  (__ "Use GHCJS to compile")
  javaScriptToggled
  (view $ prefs . to javaScript)

commandToggleDebug = CommandIDEToggleAction
  "/pics/debug.svg"
  (__ "Use GHCi debugger to build and run")
  (readIDE prefs >>= debugToggled . not . debug)
  (view $ prefs . to debug)

commandToggleMakeDocs = CommandIDEToggleAction
  "/pics/docs.svg"
  (__ "Make documentation when building")
  makeDocsToggled
  (view $ prefs . to makeDocs)

commandToggleTest = CommandIDEToggleAction
  "/pics/test.svg"
  (__ "Run unit tests when building")
  runUnitTestsToggled
  (view $ prefs . to runUnitTests)

commandToggleRunBenchmarks = CommandIDEToggleAction
  "/pics/bench.svg"
  (__ "Run benchmarks when building")
  runBenchmarksToggled
  (view $ prefs . to runBenchmarks)

commandToggleMakeDependents = CommandIDEToggleAction
  "/pics/dependents.svg"
  (__ "Make dependent packages")
  makeModeToggled
  (view $ prefs . to makeMode)

commandToggleShowIgnored = CommandIDEToggleAction
  "/pics/show-ignored.svg"
  (__ "Show files ignored by git in the workspace file trees")
  (modifyIDE_ (prefs %~ \p -> p { showIgnoredFiles = not (showIgnoredFiles p) }))
  (view $ prefs . to showIgnoredFiles)

commandToggleShowHidden = CommandIDEToggleAction
  "/pics/show-hidden.svg"
  (__ "Show hidden (dot-) files in the workspace file trees")
  (modifyIDE_ (prefs %~ \p -> p { showHiddenFiles = not (showHiddenFiles p) }))
  (view $ prefs . to showHiddenFiles)

-- | Cycle the side ("tall") pane: show -> auto-hide -> hide -> show.  Rendered
-- by a dedicated toolbar button that shows the current state (see Toolbar).
commandToggleTallPane = CommandIDEAction
  "/pics/sidebar.svg"
  (__ "Side pane: show / auto-hide / hide")
  (modifyIDE_ (prefs %~ \p -> p { tallVisibility = cycleTall (tallVisibility p) }))

-- | Cycle the bottom pane (the errors/log/grep/changes area, grid area wide1):
-- show -> auto-hide -> hide -> show.  Like 'commandToggleTallPane' but for the
-- bottom row instead of the side column.
commandToggleWide1Pane = CommandIDEAction
  "/pics/bottombar.svg"
  (__ "Bottom pane: show / auto-hide / hide")
  (modifyIDE_ (prefs %~ \p -> p { wide1Visibility = cycleTall (wide1Visibility p) }))

-- | Next side-pane visibility in the cycle.
cycleTall :: TallVisibility -> TallVisibility
cycleTall v = if v == maxBound then minBound else succ v

commandUpdateWorkspaceInfo = CommandIDEAction
  "/pics/refresh-meta.svg"
  (__ "Updates data for the current workspace")
  updateWorkspaceInfo

-- | Close the active editor or terminal tab.  The actual close happens in the
-- reflex network (it's tab state), so this just signals a request that
-- 'IDE.Web.Main' picks up; closing a terminal this way detaches from tmux
-- rather than killing the session.
commandFileClose = CommandIDEAction
  "/pics/tango/actions/window-close.svg"
  (__ "Close the active source file or terminal")
  (liftIO requestCloseActivePane)

commandDebugStep = CommandIDEAction
  "/pics/debug-step.svg"
  (__ "Single-step after stopping at a breakpoint")
  debugStep

commandDebugStepLocal = CommandIDEAction
  "/pics/debug-step-local.svg"
  (__ "Single-step within the current top-level binding")
  debugStepLocal

commandDebugStepModule = CommandIDEAction
  "/pics/debug-step-module.svg"
  (__ "Single-step restricted to the current module")
  debugStepModule

commandDebugContinue = CommandIDEAction
  "/pics/debug-continue.svg"
  (__ "Resume after a breakpoint")
  debugContinue

-- | A menu command that sends the tmux prefix (@C-b@, byte 0x02) followed by
-- @keys@ to the active terminal — exactly as if the shortcut had been typed
-- there.  @keys@ are the raw bytes that follow the prefix: a printable key like
-- @\"c\"@, or an escape sequence such as @\"\\ESC[A\"@ (Up) / @\"\\ESC1\"@
-- (M-1).  A no-op when no terminal is on screen (see 'IDE.Web.TerminalInput').
tmuxKey :: ByteString -> Command
tmuxKey keys = CommandIDEAction
  ""  -- menu-only: no toolbar icon
  (__ "Send this tmux C-b shortcut to the active terminal")
  (liftIO (sendToActiveTerminal (BS.cons 2 keys)))

-- | A pane command for the Terminal menu that works with EITHER kind of
-- terminal tab: on a control-mode (CC) tab it runs @ccCmd@ verbatim over the
-- control channel (a chord can't work there — keystrokes are @send-keys@'d
-- straight into the pane, bypassing tmux's prefix handling); on a classic PTY
-- tab it falls back to typing the @C-b chord@ (which works even for remote
-- @ssh -t@ sessions, where we can't run commands directly).
paneCmd :: Text -> ByteString -> Command
paneCmd ccCmd chord = CommandIDEAction
  ""  -- menu-only: no toolbar icon
  (__ "Terminal split/pane command (control channel or C-b chord)")
  (liftIO $ do
      done <- tmuxCommandActiveTerminal ccCmd
      unless done $ sendToActiveTerminal (BS.cons 2 chord))

-- | A menu command that toggles whether the active terminal's active tmux pane
-- is shown as a see-through, click-through hole in the window (macOS).  The work
-- happens in the reflex layer; this just signals it (see
-- 'IDE.Web.TransparencyRequest').
toggleTransparencyCmd :: Command
toggleTransparencyCmd = CommandIDEAction
  ""  -- menu-only: no toolbar icon
  (__ "Make the active tmux pane transparent (a click-through hole)")
  (liftIO requestToggleTransparency)

-- | A menu command that snaps another app's window over the active tmux pane
-- (which is made transparent so the window shows through), tracking the pane;
-- toggling it again unsnaps.  Needs macOS Accessibility permission (it asks).
-- See 'IDE.Web.SnapRequest' and the native side in @main/leksah-mac-menu.m@.
snapWindowCmd :: Command
snapWindowCmd = CommandIDEAction
  ""  -- menu-only: no toolbar icon
  (__ "Snap another app's window over the active tmux pane (macOS)")
  (liftIO requestSnapWindow)
