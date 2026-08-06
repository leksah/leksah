{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Web.Command where

import Control.Lens
       (Getter, to, makePrisms, view, (%~), (^.), (&), ix)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (cons)
import Data.Text (Text)

import IDE.Web.AIContextRequest
       (AIAction(..), requestAIAction)
import IDE.Web.AddServerRequest (requestAddServer)
import IDE.Web.CloseRequest (requestCloseActivePane)
import IDE.Web.NewWindowRequest (requestNewWindow)
import IDE.Web.NewLwRequest (requestFontConvert, requestConsolidate)
import IDE.Web.ReplTmux (paneCountOfWindow)
import IDE.Web.RegionGrabRequest (requestRegionGrab)
import IDE.Web.TerminalInput
       (sendToActiveTerminal, tmuxCommandActiveTerminal, splitActiveTerminal)
import IDE.Web.TransparencyRequest (requestToggleTransparency)
import IDE.Web.SnapRequest (requestSnapWindow)

import Data.Map (Map)
import qualified Data.Map as M (adjust, lookup)
import Data.Maybe (fromMaybe)

import IDE.Core.State
       (readIDE, modifyIDE_, Prefs(..), prefs, PackageAction, ProjectAction,
        WorkspaceAction, IDEAction, __, IDE, TallVisibility(..),
        webWindows, activeWindow, wwTall, wwWide1, wwActive, activeProject,
        activePack, pjDir, pjKey, TabKey(..), leksahWindows,
        LeksahWindow(..), PaneContent(..), PaneKind(..))
import IDE.Web.Claude (runClaudeCmd, ClaudeCmd(..))
import IDE.Gtk.Package
       (makeModeToggled, runBenchmarksToggled, runUnitTestsToggled,
        makeDocsToggled, javaScriptToggled, nativeToggled,
        backgroundBuildToggled, packageRunJavaScript, packageRun)
import IDE.Package (packageClean, projectRefreshNix, buildCustomProject)
import IDE.Gtk.Workspaces
       (projectTry, packageTry, workspaceTry, makePackage)

data Command =
    CommandIDEAction Text Text IDEAction
  | CommandIDEToggleAction Text Text IDEAction (IDE -> Bool)
  | CommandWorkspaceAction Text Text WorkspaceAction
  | CommandProjectAction Text Text ProjectAction
  | CommandPackageAction Text Text PackageAction
  | CommandFileOpen
  | CommandProjectOpen
  | CommandProjectOpenFolder
  | CommandProjectAddRemote
  | CommandFileSave
  | CommandFind
  | CommandShowPreferences
  | CommandShowShortcuts
  | CommandOpenBrowser
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
  _ -> Nothing

commandImageAndTip :: Command -> (Text, Text)
commandImageAndTip (CommandIDEAction img tip _) = (img, tip)
commandImageAndTip (CommandIDEToggleAction img tip _ _) = (img, tip)
commandImageAndTip (CommandWorkspaceAction img tip _) = (img, tip)
commandImageAndTip (CommandProjectAction img tip _) = (img, tip)
commandImageAndTip (CommandPackageAction img tip _) = (img, tip)
commandImageAndTip CommandFileOpen = ("/pics/file-open.svg", __ "Opens an existing file")
commandImageAndTip CommandFileSave = ("/pics/file-save.svg", __ "Saves the current buffer")
commandImageAndTip CommandFind = ("/pics/find.svg", __ "Show or hide the find bar")
commandImageAndTip CommandNextError = ("/pics/error-next.svg", __ "Go to the next error")
commandImageAndTip CommandPreviousError = ("/pics/error-prev.svg", __ "Go to the previous error")
commandImageAndTip CommandShowShortcuts = ("/pics/shortcuts.svg", __ "Show the keyboard shortcut cheat sheet")
commandImageAndTip CommandOpenBrowser = ("/pics/browser.svg", __ "Open a new web browser pane")
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
  , commandToggleWide1Pane, commandToggleTmuxIntercept
  , commandFileClose :: Command
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

commandPackageBuild = CommandIDEAction
  "/pics/build.svg"
  (__ "Builds the package")
  buildActiveTarget

-- | Build the active target: the active Haskell package if there is one,
-- otherwise fall back to a package-less project's custom build (e.g. a Rust
-- crate added via \"Open Folder\" builds with @cargo build@).  Routing on
-- 'activePack' keeps Haskell projects on the usual 'makePackage' path — they
-- always have an active package once opened.
buildActiveTarget :: IDEAction
buildActiveTarget = readIDE activePack >>= \case
    Just _  -> packageTry makePackage
    Nothing -> projectTry buildCustomProject

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
  (__ "Build and run in GHCi (ffcabal repls)")
  (modifyIDE_ $ prefs %~ (\p -> p { debug = not (debug p) }))
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

-- | Toggle intercepting the tmux @C-b@ prefix in terminals: @C-b w@ activates
-- the Terminals pane and other prefix keys run the equivalent tmux command
-- (so they work in control-mode tabs, where a raw @C-b@ chord otherwise just
-- types @^B@).  The interception state machine lives in JS
-- (@window.LeksahTmux@); this flips the pref that each window mirrors into it.
commandToggleTmuxIntercept = CommandIDEToggleAction
  ""  -- menu-only: no toolbar icon
  (__ "Intercept the tmux Ctrl+B prefix in terminals (C-b w shows the Tmux pane)")
  (modifyIDE_ (prefs %~ \p -> p { tmuxInterceptPrefix = not (tmuxInterceptPrefix p) }))
  (view $ prefs . to tmuxInterceptPrefix)

-- | Cycle the side ("tall") pane: show -> auto-hide -> hide -> show.  Rendered
-- by a dedicated toolbar button that shows the current state (see Toolbar).
commandToggleTallPane = CommandIDEAction
  "/pics/sidebar.svg"
  (__ "Side pane: show / auto-hide / hide")
  -- Per-window: cycle the visibility of the frontmost OS window's side pane.
  (modifyIDE_ $ \i -> case i ^. activeWindow of
     Just aw -> i & webWindows . ix aw . wwTall %~ cycleTall
     Nothing -> i)

-- | Cycle the bottom pane (the errors/log/grep/changes area, grid area wide1):
-- show -> auto-hide -> hide -> show.  Like 'commandToggleTallPane' but for the
-- bottom row instead of the side column.
commandToggleWide1Pane = CommandIDEAction
  "/pics/bottombar.svg"
  (__ "Bottom pane: show / auto-hide / hide")
  -- Per-window: cycle the visibility of the frontmost OS window's bottom pane.
  (modifyIDE_ $ \i -> case i ^. activeWindow of
     Just aw -> i & webWindows . ix aw . wwWide1 %~ cycleTall
     Nothing -> i)

-- | Next side-pane visibility in the cycle.
cycleTall :: TallVisibility -> TallVisibility
cycleTall v = if v == maxBound then minBound else succ v

-- | Close the active editor or terminal tab.  The actual close happens in the
-- reflex network (it's tab state), so this just signals a request that
-- 'IDE.Web.Main' picks up; closing a terminal this way detaches from tmux
-- rather than killing the session.
commandFileClose = CommandIDEAction
  "/pics/tango/actions/window-close.svg"
  (__ "Close the active source file or terminal")
  (liftIO requestCloseActivePane)

-- | File ▸ New Window (⌘N): open a fresh, empty OS window.  The library can't
-- create a native window, so the action just drops a request; the wkwebview
-- front end ('IDE.Web.MacMenu') registers the handler that mints a 'WindowId'
-- and asks the ObjC glue to create the NSWindow + WKWebView.  A no-op on
-- warp/webkitgtk (one browser tab is the whole UI).
commandNewWindow :: Command
commandNewWindow = CommandIDEAction
  ""
  (__ "Open a new window")
  (liftIO requestNewWindow)

-- | File ▸ Add Server… (and the Terminals-tree row): register an ssh host in
-- the 'remoteHosts' preference.  A plain 'CommandIDEAction', so both the web
-- menubar and the native menus dispatch it generically; the action drops a
-- token on the "IDE.Web.AddServerRequest" bridge and the reflex modal in
-- 'IDE.Web.Main' does the actual work.
commandAddServer :: Command
commandAddServer = CommandIDEAction
  ""
  (__ "Add an ssh server to the Terminals tree")
  (liftIO requestAddServer)

-- | AI ▸ Grab Region: select a screen rectangle and drop its PNG path into the
-- terminal named by the 'regionCaptureTarget' preference.  The orchestration
-- (permission probe → crosshair or in-leksah overlay+snapshot) lives in
-- 'IDE.Web.Main'; this just drops a request.
commandGrabRegion :: Command
commandGrabRegion = CommandIDEAction
  ""
  (__ "Grab a screen region and send its image to the terminal")
  (liftIO (requestRegionGrab Nothing))

-- The AI ▸ Send… commands drop a token; the front end ('IDE.Web.Main') reads the
-- active editor / current error and types the reference into the AI terminal.
commandSendSelection :: Command
commandSendSelection = CommandIDEAction
  ""
  (__ "Send the selected lines (@file#Lx-Ly) to the AI terminal")
  (liftIO (requestAIAction SendSelection))

commandSendFileRef :: Command
commandSendFileRef = CommandIDEAction
  ""
  (__ "Send the current file (@file) to the AI terminal")
  (liftIO (requestAIAction SendFileRef))

commandSendError :: Command
commandSendError = CommandIDEAction
  ""
  (__ "Send the current error (location + message) to the AI terminal")
  (liftIO (requestAIAction SendError))

commandFocusAITerminal :: Command
commandFocusAITerminal = CommandIDEAction
  ""
  (__ "Focus the AI terminal pane")
  (liftIO (requestAIAction FocusAITerminal))

-- | Start a new Claude Code session in the active project's directory (toolbar
-- + AI menu).  A no-op when no project is active or @claude@ isn't on PATH
-- (runClaudeCmd just opens nothing).
commandClaudeNew :: Command
commandClaudeNew = CommandIDEAction
  "/pics/tree-claude.svg"
  (__ "Start a Claude Code session in the active project")
  (readIDE activeProject >>= mapM_ (liftIO . runClaudeCmd . ClaudeNew . pjDir . pjKey))

-- | Continue the most recent Claude Code session in the active project.
commandClaudeContinue :: Command
commandClaudeContinue = CommandIDEAction
  ""
  (__ "Continue the most recent Claude Code session in the active project")
  (readIDE activeProject >>= mapM_ (liftIO . runClaudeCmd . ClaudeContinue . pjDir . pjKey))

-- | View ▸ Bigger/Smaller/Reset Font (⌘+/⌘−/⌘0): adjust the FOCUSED pane's
-- font size in the active leksah window.  Per-pane fonts are the point of
-- the native split system: a pane shows a whole tmux window, so two tmux
-- panes with different font sizes can never share a window.  A no-op when
-- the active tab isn't a leksah window (or nothing is focused).
commandFontBigger, commandFontSmaller, commandFontReset :: Command
commandFontBigger = CommandIDEAction
  ""
  (__ "Increase the focused split's font size")
  (leafFontAdjust (\eff -> Just (eff + 1)))

commandFontSmaller = CommandIDEAction
  ""
  (__ "Decrease the focused split's font size")
  (leafFontAdjust (\eff -> Just (eff - 1)))

commandFontReset = CommandIDEAction
  ""
  (__ "Reset the focused split's font size to the preference")
  (leafFontAdjust (const Nothing))

-- | Apply a font-size edit to the focused pane of the active OS window's
-- active leksah window.  @f@ maps the current EFFECTIVE size (override, else
-- the global monospace pref) to the new override; 'Nothing' = follow the
-- pref.  A tmux pane in a MULTI-pane window is isolated first (per-pane
-- fonts can't share a tmux window) via the font-convert queue — Main's
-- driver runs the minimal-path conversion and then applies @f@.
leafFontAdjust :: (Int -> Maybe Int) -> IDEAction
leafFontAdjust f = do
  aw  <- readIDE activeWindow
  wws <- readIDE webWindows
  lws <- readIDE leksahWindows
  let mbTarget = do
        a  <- aw
        ww <- M.lookup a wws
        k  <- ww ^. wwActive
        n  <- case k of LeksahWinKey n' -> Just n'; _ -> Nothing
        lw <- M.lookup n lws
        l  <- lwFocused lw
        pc <- M.lookup l (lwPanes lw)
        return (n, l, pc)
  case mbTarget of
    Nothing -> return ()
    Just (n, l, PaneContent kind cur) -> do
      multi <- case kind of
        PaneTmux w -> (> 1) <$> liftIO (paneCountOfWindow w)
        _          -> return False
      case kind of
        PaneTmux w | multi -> liftIO (requestFontConvert (n, w, f))
        _ -> do
          modifyIDE_ $ \i ->
            let eff = fromMaybe (monospaceFontSize (i ^. prefs)) cur
                new = fmap (max 6 . min 72) (f eff)
                setFont lw = lw { lwPanes =
                    M.adjust (\pc -> pc { pcFontSize = new }) l (lwPanes lw) }
            in i & leksahWindows %~ M.adjust setFont n
          -- The new size may match a neighbouring tmux window's — merge
          -- them (Main's consolidateLw drains this).
          liftIO (requestConsolidate n)

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

-- | The Terminal menu's split commands.  Unlike 'paneCmd' these build a
-- @split-window@ that reproduces a directory window's environment — the new
-- pane re-enters the project's command prefix (a repl just inherits the
-- directory); see 'IDE.Web.TerminalInput.splitActiveTerminal'.
splitCmd :: Bool -> ByteString -> Command
splitCmd horizontal chord = CommandIDEAction
  ""  -- menu-only: no toolbar icon
  (__ "Split the terminal pane (re-entering a directory window's environment)")
  (liftIO (splitActiveTerminal horizontal chord))

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
