{-# LANGUAGE OverloadedStrings #-}
-- | The menu model: top-level menus, each a tree of items.  An item is either a
-- labelled 'Command' or a labelled 'Submenu' of further items (arbitrary depth).
-- This is the single source of truth shared by the web menubar
-- (`IDE.Web.Widget.Menubar`) and the native macOS menu (`IDE.Web.MacMenu`), so
-- they stay in sync.
module IDE.Web.MenuModel
  ( MenuItem(..)
  , menus
  , prettyKeySpec
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import IDE.Web.Command
       (Command(..), commandAddModule, commandRefreshNix, commandPackageClean,
        commandPackageBuild, commandPackageRun, commandPackageRunJavascript,
        commandToggleBackgroundBuild, commandToggleNative, commandToggleJavaScript,
        commandToggleDebug, commandToggleMakeDocs, commandToggleTest,
        commandToggleRunBenchmarks, commandToggleMakeDependents,
        commandUpdateWorkspaceInfo, commandDebugStep, commandDebugStepLocal,
        commandDebugStepModule, commandDebugContinue, commandFileClose,
        commandNewWindow, commandToggleTmuxIntercept, tmuxKey,
        paneCmd, toggleTransparencyCmd, snapWindowCmd, commandGrabRegion,
        commandSendSelection, commandSendFileRef, commandSendError,
        commandFocusAITerminal)

-- | One entry in a menu: a clickable command (optionally with a shortcut hint
-- shown the macOS way — right-aligned and greyed), or a nested submenu.
data MenuItem
  = MenuItem Text Command            -- ^ label, command
  | MenuShortcut Text Text Command   -- ^ label, shortcut hint, command
  | MenuKey Text Text Command        -- ^ label, REAL key equivalent (a spec
                                     --   like @\"cmd+shift+d\"@ or
                                     --   @\"cmd+alt+Up\"@ — parsed natively
                                     --   into an NSMenuItem key equivalent,
                                     --   enabled only while a terminal is
                                     --   active), command
  | MenuGlobalKey Text Text Command  -- ^ like 'MenuKey' but NOT gated to a
                                     --   terminal — an always-available real
                                     --   key equivalent (label, spec, command)
  | MenuSep                          -- ^ a separator line
  | Submenu  Text [MenuItem]         -- ^ a labelled nested menu

-- | Convenience: a plain @(label, command)@ leaf.
item :: Text -> Command -> MenuItem
item = MenuItem

-- | A leaf whose shortcut is displayed in the native key-equivalent column.
-- The shortcut is display-only — tmux chords (@C-b X@) aren't single-chord macOS
-- key equivalents, so they can't be real 'NSMenuItem' key equivalents.
key :: Text -> Text -> Command -> MenuItem
key = MenuShortcut

-- | Render a 'MenuKey' spec (@\"cmd+shift+d\"@) the way macOS displays
-- shortcuts (@⇧⌘D@) — for the web menubar, which shows the hint as text.
prettyKeySpec :: Text -> Text
prettyKeySpec spec =
    let parts = T.splitOn "+" spec
        -- "cmd+ctrl+=": a trailing empty part means the key itself is '+';
        -- treat empties as literal "+" if last, drop otherwise.
        (mods, keys) = span (`elem` ["cmd", "super", "shift", "alt", "opt", "ctrl"]) parts
        modSym m = case m of
            "ctrl"  -> "⌃"
            "alt"   -> "⌥"
            "opt"   -> "⌥"
            "shift" -> "⇧"
            _       -> "⌘"   -- cmd/super
        -- macOS convention orders modifiers ⌃⌥⇧⌘.
        order = ["ctrl", "alt", "opt", "shift", "cmd", "super"]
        sortedMods = [ modSym o | o <- order, o `elem` mods ]
        keySym k = case k of
            "Up"    -> "↑"
            "Down"  -> "↓"
            "Left"  -> "←"
            "Right" -> "→"
            "Enter" -> "⏎"
            _       -> T.toUpper k
    in T.concat (sortedMods <> map keySym (filter (not . T.null) keys))

menus :: [(Text, [MenuItem])]
menus =
  [ ("File",
      [ MenuGlobalKey "New Window" "cmd+n" commandNewWindow
      , MenuSep
      , item "Open…"         CommandFileOpen
      , item "Open Project…" CommandProjectOpen
      , item "Save"          CommandFileSave
      , item "Close"         commandFileClose
      ])
  , ("Edit",
      [ item "Find" CommandFind
      , item "Preferences…" CommandShowPreferences
      ])
  , ("Workspace",
      [ item "Refresh Nix Environment" commandRefreshNix
      , item "Update Workspace Info"   commandUpdateWorkspaceInfo
      ])
  , ("Package",
      [ item "Add Module"     commandAddModule
      , item "Clean"          commandPackageClean
      , item "Build"          commandPackageBuild
      , item "Run"            commandPackageRun
      , item "Run JavaScript" commandPackageRunJavascript
      ])
  , ("Debug",
      [ item "Step"        commandDebugStep
      , item "Step Local"  commandDebugStepLocal
      , item "Step Module" commandDebugStepModule
      , item "Continue"    commandDebugContinue
      ])
  , ("Build",
      [ item "Background Build" commandToggleBackgroundBuild
      , item "Native"           commandToggleNative
      , item "JavaScript"       commandToggleJavaScript
      , item "Debug"            commandToggleDebug
      , item "Make Docs"        commandToggleMakeDocs
      , item "Run Tests"        commandToggleTest
      , item "Run Benchmarks"   commandToggleRunBenchmarks
      , item "Make Dependents"  commandToggleMakeDependents
      ])
  , ("Errors",
      [ item "Next Error"     CommandNextError
      , item "Previous Error" CommandPreviousError
      ])
  , ("Terminal", terminalMenu)
  , ("AI",
      [ MenuGlobalKey "Send Selection"      "cmd+ctrl+s" commandSendSelection
      , MenuGlobalKey "Send File Reference" "cmd+ctrl+r" commandSendFileRef
      , MenuGlobalKey "Send Error"          "cmd+ctrl+e" commandSendError
      , MenuGlobalKey "Focus AI Terminal"   "cmd+ctrl+j" commandFocusAITerminal
      , MenuGlobalKey "Grab Region"         "cmd+ctrl+g" commandGrabRegion
      ])
  ]

-- | The Terminal menu: iTerm2's \"Shell\" grouping (everything terminal-ish in
-- one menu) with Ghostty's item names and default shortcuts.  The split/pane
-- items are 'paneCmd's, so they work on BOTH kinds of terminal tab — real
-- commands over the control channel for CC (⊞) tabs, the @C-b@ chord typed
-- into the PTY for classic (▭) tabs.  Their ⌘ key equivalents are real but
-- native-menu-gated: enabled only while a terminal tab is on screen, so ⌘D,
-- ⌘[/⌘], ⌘⌥arrows etc. still reach the editor otherwise.
terminalMenu :: [MenuItem]
terminalMenu =
  [ MenuKey "New Window"      "cmd+shift+t" (paneCmd "new-window" "c")
  , MenuKey "Previous Window" "cmd+shift+[" (paneCmd "previous-window" "p")
  , MenuKey "Next Window"     "cmd+shift+]" (paneCmd "next-window" "n")
  , MenuSep
  , MenuKey "Split Right" "cmd+d"       (paneCmd "split-window -h" "%")
  , MenuKey "Split Down"  "cmd+shift+d" (paneCmd "split-window -v" "\"")
  , MenuSep
  , Submenu "Select Split"
      [ MenuKey "Select Split Above" "cmd+alt+Up"    (paneCmd "select-pane -U" "\ESC[A")
      , MenuKey "Select Split Below" "cmd+alt+Down"  (paneCmd "select-pane -D" "\ESC[B")
      , MenuKey "Select Split Left"  "cmd+alt+Left"  (paneCmd "select-pane -L" "\ESC[D")
      , MenuKey "Select Split Right" "cmd+alt+Right" (paneCmd "select-pane -R" "\ESC[C")
      , MenuSep
      , MenuKey "Select Previous Split" "cmd+[" (paneCmd "select-pane -t :.-" ";")
      , MenuKey "Select Next Split"     "cmd+]" (paneCmd "select-pane -t :.+" "o")
      ]
  , Submenu "Resize Split"
      [ MenuKey "Equalize Splits"    "cmd+ctrl+="     (paneCmd "select-layout -E" "E")
      , MenuSep
      , MenuKey "Move Divider Up"    "cmd+ctrl+Up"    (paneCmd "resize-pane -U 5" "K")
      , MenuKey "Move Divider Down"  "cmd+ctrl+Down"  (paneCmd "resize-pane -D 5" "J")
      , MenuKey "Move Divider Left"  "cmd+ctrl+Left"  (paneCmd "resize-pane -L 5" "H")
      , MenuKey "Move Divider Right" "cmd+ctrl+Right" (paneCmd "resize-pane -R 5" "L")
      ]
  , MenuKey "Zoom Split" "cmd+shift+Enter" (paneCmd "resize-pane -Z" "z")
  , item "Close Split" (paneCmd "kill-pane" "x")
  , MenuSep
  , Submenu "Underlay"
      -- ⌘⌥Y / ⌘⌥U are shown as hints; the leksah keymap actually handles them.
      [ key "Toggle Pane Transparency" "⌘⌥Y" toggleTransparencyCmd
      , key "Snap Window to Pane"      "⌘⌥U" snapWindowCmd
      -- Populated natively from the currently-snapped windows (see leksah-mac-menu.m).
      , Submenu "Unsnap" []
      ]
  , MenuSep
  , item "Intercept Ctrl+B" commandToggleTmuxIntercept
  , Submenu "Tmux" tmuxMenu
  ]

-- | Every default @tmux@ prefix (@C-b@) key binding, grouped into submenus, as
-- items that send the shortcut to the active terminal (see 'tmuxKey').  Each
-- shows its chord in the native shortcut column (@⌃B@ = the C-b prefix).  The
-- bytes following the prefix are printable keys as-is, arrows/meta as escape
-- sequences (@\\ESC[A@ = Up, @\\ESC1@ = M-1).  Chords only work on classic
-- (▭, attached-client) tabs; the Terminal menu's 'paneCmd' items above cover
-- the common pane operations on CC tabs too.
tmuxMenu :: [MenuItem]
tmuxMenu =
  [ Submenu "Sessions"
      [ key "Detach client"    "⌃B d" (tmuxKey "d")
      , key "Choose session"   "⌃B s" (tmuxKey "s")
      , key "Rename session"   "⌃B $" (tmuxKey "$")
      , key "Previous session" "⌃B (" (tmuxKey "(")
      , key "Next session"     "⌃B )" (tmuxKey ")")
      -- ⌃B L is rebound to resize-pane-right (vim HJKL), so last-session moves to ⇧Tab.
      , key "Last session"     "⌃B ⇧Tab" (tmuxKey "\ESC[Z")
      ]
  , Submenu "Windows"
      [ key "New window"       "⌃B c" (tmuxKey "c")
      , key "Rename window"    "⌃B ," (tmuxKey ",")
      , key "Kill window"      "⌃B &" (tmuxKey "&")
      , key "Next window"      "⌃B n" (tmuxKey "n")
      , key "Previous window"  "⌃B p" (tmuxKey "p")
      -- ⌃B l is rebound to select-pane-right (vim hjkl), so last-window moves to Tab.
      , key "Last window"      "⌃B Tab" (tmuxKey "\t")
      , key "Choose window"    "⌃B w" (tmuxKey "w")
      , key "Find window"      "⌃B f" (tmuxKey "f")
      , key "Select by index"  "⌃B '" (tmuxKey "'")
      , key "Move window"      "⌃B ." (tmuxKey ".")
      , Submenu "Select window"
          [ key "Window 0" "⌃B 0" (tmuxKey "0")
          , key "Window 1" "⌃B 1" (tmuxKey "1")
          , key "Window 2" "⌃B 2" (tmuxKey "2")
          , key "Window 3" "⌃B 3" (tmuxKey "3")
          , key "Window 4" "⌃B 4" (tmuxKey "4")
          , key "Window 5" "⌃B 5" (tmuxKey "5")
          , key "Window 6" "⌃B 6" (tmuxKey "6")
          , key "Window 7" "⌃B 7" (tmuxKey "7")
          , key "Window 8" "⌃B 8" (tmuxKey "8")
          , key "Window 9" "⌃B 9" (tmuxKey "9")
          ]
      ]
  , Submenu "Panes"
      [ key "Split left/right"     "⌃B %"  (tmuxKey "%")
      , key "Split top/bottom"     "⌃B \"" (tmuxKey "\"")
      , key "Next pane"            "⌃B o"  (tmuxKey "o")
      , key "Last pane"            "⌃B ;"  (tmuxKey ";")
      , key "Swap pane up"         "⌃B {"  (tmuxKey "{")
      , key "Swap pane down"       "⌃B }"  (tmuxKey "}")
      , key "Rotate panes"         "⌃B ⌃O" (tmuxKey "\SI")
      , key "Kill pane"            "⌃B x"  (tmuxKey "x")
      , key "Zoom/unzoom pane"     "⌃B z"  (tmuxKey "z")
      , key "Break pane to window" "⌃B !"  (tmuxKey "!")
      , key "Show pane numbers"    "⌃B q"  (tmuxKey "q")
      , key "Mark pane"            "⌃B m"  (tmuxKey "m")
      , key "Clear marked pane"    "⌃B M"  (tmuxKey "M")
      , Submenu "Select pane"
          [ key "Above" "⌃B ↑ / k" (tmuxKey "\ESC[A")
          , key "Below" "⌃B ↓ / j" (tmuxKey "\ESC[B")
          , key "Right" "⌃B → / l" (tmuxKey "\ESC[C")
          , key "Left"  "⌃B ← / h" (tmuxKey "\ESC[D")
          ]
      , Submenu "Resize pane"
          [ key "Up (5)"    "⌃B K"  (tmuxKey "K")
          , key "Down (5)"  "⌃B J"  (tmuxKey "J")
          , key "Right (5)" "⌃B L"  (tmuxKey "L")
          , key "Left (5)"  "⌃B H"  (tmuxKey "H")
          , key "Up (1)"    "⌃B ⌃k" (tmuxKey "\v")
          , key "Down (1)"  "⌃B ⌃j" (tmuxKey "\n")
          , key "Right (1)" "⌃B ⌃l" (tmuxKey "\f")
          , key "Left (1)"  "⌃B ⌃h" (tmuxKey "\b")
          ]
      ]
  , Submenu "Layout"
      [ key "Next layout"     "⌃B Space" (tmuxKey " ")
      , key "Even horizontal" "⌃B ⌥1"    (tmuxKey "\ESC1")
      , key "Even vertical"   "⌃B ⌥2"    (tmuxKey "\ESC2")
      , key "Main horizontal" "⌃B ⌥3"    (tmuxKey "\ESC3")
      , key "Main vertical"   "⌃B ⌥4"    (tmuxKey "\ESC4")
      , key "Tiled"           "⌃B ⌥5"    (tmuxKey "\ESC5")
      ]
  , Submenu "Copy & Buffers"
      [ key "Copy (scroll) mode" "⌃B [" (tmuxKey "[")
      , key "Paste buffer"       "⌃B ]" (tmuxKey "]")
      , key "Choose buffer"      "⌃B =" (tmuxKey "=")
      , key "List paste buffers" "⌃B #" (tmuxKey "#")
      ]
  , Submenu "Misc"
      [ key "Command prompt"   "⌃B :" (tmuxKey ":")
      , key "List key bindings" "⌃B ?" (tmuxKey "?")
      , key "Clock"            "⌃B t" (tmuxKey "t")
      , key "Refresh client"   "⌃B r" (tmuxKey "r")
      , key "Show messages"    "⌃B ~" (tmuxKey "~")
      ]
  ]
