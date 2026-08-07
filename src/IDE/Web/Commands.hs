-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

-- | The command registry: every bindable\/menu action under a stable id
-- (@\"package.build\"@, @\"nav.split\"@, …), the single vocabulary
-- @keybindings.json@ rules and menu entries name commands by.  Id namespaces
-- mirror the menus: @workspace.*@, @edit.*@, @package.*@, @build.*@,
-- @errors.*@, @view.*@, @terminal.*@, @ai.*@, @nav.*@.
--
-- The old 27 generated navigation chords collapse here to three commands
-- taking an integer argument (@nav.split@ + @args: 3@); everything else is
-- nullary.
module IDE.Web.Commands
  ( allCommands
  , lookupCommand
  , duplicateCommandIds
  , commandReloadKeybindings
  ) where

import Data.List (find, group, sort)
import Data.Text (Text)

import IDE.App (appNote)
import IDE.Web.Command
import IDE.Web.Keybindings
       (CommandSpec(..), When(..), loadKeybindings, nullarySpec)

-- | Every command a key can bind or a menu can reference.  Titles are the
-- menu labels.  'duplicateCommandIds' guards the ids at boot.
allCommands :: [CommandSpec]
allCommands =
    [ always "workspace.newWindow"        "New Window"            commandNewWindow
    , always "workspace.openFile"         "Open File…"            CommandFileOpen
    , always "workspace.openProject"      "Open Project…"         CommandProjectOpen
    , always "workspace.openFolder"       "Open Folder…"          CommandProjectOpenFolder
    , always "workspace.addRemoteProject" "Add Remote Project…"   CommandProjectAddRemote
    , always "workspace.addServer"        "Add Server…"           commandAddServer
    , always "workspace.saveFile"         "Save File"             CommandFileSave
    , always "workspace.closeFile"        "Close File"            commandFileClose
    , always "workspace.refreshNix"       "Refresh Nix Environment" commandRefreshNix

    , always "edit.find"                  "Find"                  CommandFind
    , always "edit.showShortcuts"         "Keyboard Shortcuts…"   CommandShowShortcuts
    , always "edit.showPreferences"       "Preferences…"          CommandShowPreferences
    , always "edit.reloadKeybindings"     "Reload Keybindings"    commandReloadKeybindings

    , always "package.addModule"          "Add Module"            commandAddModule
    , always "package.clean"              "Clean"                 commandPackageClean
    , always "package.build"              "Build"                 commandPackageBuild
    , always "package.run"                "Run"                   commandPackageRun
    , always "package.runJavaScript"      "Run JavaScript"        commandPackageRunJavascript

    , always "build.toggleBackground"     "Background Build"      commandToggleBackgroundBuild
    , always "build.toggleNative"         "Native"                commandToggleNative
    , always "build.toggleJavaScript"     "JavaScript"            commandToggleJavaScript
    , always "build.toggleGhci"           "Interpreted (ghci) Mode" commandToggleDebug
    , always "build.toggleDocs"           "Make Docs"             commandToggleMakeDocs
    , always "build.toggleTests"          "Run Tests"             commandToggleTest
    , always "build.toggleBenchmarks"     "Run Benchmarks"        commandToggleRunBenchmarks
    , always "build.toggleDependents"     "Make Dependents"       commandToggleMakeDependents

    , always "errors.next"                "Next Error"            CommandNextError
    , always "errors.previous"            "Previous Error"        CommandPreviousError

    , always "view.nextTab"               "Next Tab"              CommandFlipDown
    , always "view.previousTab"           "Previous Tab"          CommandFlipUp
    , always "view.fontBigger"            "Bigger Font"           commandFontBigger
    , always "view.fontSmaller"           "Smaller Font"          commandFontSmaller
    , always "view.fontReset"             "Reset Font"            commandFontReset
    , always "view.newBrowserPane"        "New Browser Pane"      CommandOpenBrowser
    , always "view.toggleSidebar"         "Toggle Side Pane"      commandToggleTallPane
    , always "view.toggleBottomBar"       "Toggle Bottom Pane"    commandToggleWide1Pane
    , always "view.showHiddenFiles"       "Show Hidden Files"     commandToggleShowHidden
    , always "view.showIgnoredFiles"      "Show Ignored Files"    commandToggleShowIgnored

    , terminal "terminal.newWindow"       "New Window"            (paneCmd "new-window" "c")
    , terminal "terminal.previousWindow"  "Previous Window"       (paneCmd "previous-window" "p")
    , terminal "terminal.nextWindow"      "Next Window"           (paneCmd "next-window" "n")
    , splittable "terminal.splitRight"    "Split Right"           (splitCmd True  "%")
    , splittable "terminal.splitDown"     "Split Down"            (splitCmd False "\"")
    , terminal "terminal.selectSplitAbove" "Select Split Above"   (paneCmd "select-pane -U" "\ESC[A")
    , terminal "terminal.selectSplitBelow" "Select Split Below"   (paneCmd "select-pane -D" "\ESC[B")
    , terminal "terminal.selectSplitLeft"  "Select Split Left"    (paneCmd "select-pane -L" "\ESC[D")
    , terminal "terminal.selectSplitRight" "Select Split Right"   (paneCmd "select-pane -R" "\ESC[C")
    , terminal "terminal.selectPreviousSplit" "Select Previous Split" (paneCmd "select-pane -t :.-" ";")
    , terminal "terminal.selectNextSplit" "Select Next Split"     (paneCmd "select-pane -t :.+" "o")
    , terminal "terminal.equalizeSplits"  "Equalize Splits"       (paneCmd "select-layout -E" "E")
    , terminal "terminal.moveDividerUp"   "Move Divider Up"       (paneCmd "resize-pane -U 5" "K")
    , terminal "terminal.moveDividerDown" "Move Divider Down"     (paneCmd "resize-pane -D 5" "J")
    , terminal "terminal.moveDividerLeft" "Move Divider Left"     (paneCmd "resize-pane -L 5" "H")
    , terminal "terminal.moveDividerRight" "Move Divider Right"   (paneCmd "resize-pane -R 5" "L")
    , terminal "terminal.zoomSplit"       "Zoom Split"            (paneCmd "resize-pane -Z" "z")
    , terminal "terminal.closeSplit"      "Close Split"           (paneCmd "kill-pane" "x")
    , always "terminal.togglePaneTransparency" "Toggle Pane Transparency" toggleTransparencyCmd
    , always "terminal.snapWindowToPane"  "Snap Window to Pane"   snapWindowCmd
    , always "terminal.focusAlerting"     "Focus Alerting Terminal" CommandFocusAlert
    , always "terminal.toggleTmuxIntercept" "Intercept Ctrl+B"    commandToggleTmuxIntercept

    , always "ai.sendSelection"           "Send Selection"        commandSendSelection
    , always "ai.sendFileRef"             "Send File Reference"   commandSendFileRef
    , always "ai.sendError"               "Send Error"            commandSendError
    , always "ai.focusTerminal"           "Focus AI Terminal"     commandFocusAITerminal
    , always "ai.grabRegion"              "Grab Region"           commandGrabRegion
    , always "ai.newClaudeSession"        "New Claude Session"    commandClaudeNew
    , always "ai.continueClaudeSession"   "Continue Claude Session" commandClaudeContinue

    , withArg "nav.split"      "Select the Nth split of the active terminal" CommandSelectSplit
    , withArg "nav.sidePane"   "Select the Nth side-bar pane"                CommandSelectSidePane
    , withArg "nav.bottomPane" "Select the Nth bottom-bar pane"              CommandSelectBottomPane
    ]
  where
    always i t     = nullarySpec i t WhenAlways
    terminal i t   = nullarySpec i t WhenTerminal
    splittable i t = nullarySpec i t WhenTerminalOrConvertible
    withArg i t mk = CommandSpec i t WhenAlways (fmap mk)

lookupCommand :: Text -> Maybe CommandSpec
lookupCommand cid = find ((== cid) . csId) allCommands

-- | Ids appearing more than once — checked (and logged) at boot.
duplicateCommandIds :: [Text]
duplicateCommandIds =
    [ g0 | (g0:_:_) <- group (sort (map csId allCommands)) ]

-- | @edit.reloadKeybindings@: re-read @keybindings.json@ and apply it to the
-- DOM keymap, the menus and the Shortcuts pane; problems land in the build-log
-- pane.
commandReloadKeybindings :: Command
commandReloadKeybindings = CommandIDEAction
  ""
  "Reload keybindings.json"
  (\app -> loadKeybindings allCommands >>= mapM_ (appNote app) . snd)
