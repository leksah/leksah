{-# LANGUAGE OverloadedStrings #-}
-- | The menu model: top-level menus, each a tree of items.  An item is either a
-- labelled 'Command' or a labelled 'Submenu' of further items (arbitrary depth).
-- This is the single source of truth shared by the web menubar
-- (`IDE.Web.Widget.Menubar`) and the native macOS menu (`IDE.Web.MacMenu`), so
-- they stay in sync.
module IDE.Web.MenuModel
  ( MenuItem(..)
  , menus
  ) where

import Data.Text (Text)

import IDE.Web.Command
       (Command(..), commandAddModule, commandRefreshNix, commandPackageClean,
        commandPackageBuild, commandPackageRun, commandPackageRunJavascript,
        commandToggleBackgroundBuild, commandToggleNative, commandToggleJavaScript,
        commandToggleDebug, commandToggleMakeDocs, commandToggleTest,
        commandToggleRunBenchmarks, commandToggleMakeDependents,
        commandUpdateWorkspaceInfo, commandDebugStep, commandDebugStepLocal,
        commandDebugStepModule, commandDebugContinue, commandFileClose, tmuxKey)

-- | One entry in a menu: a clickable command (optionally with a shortcut hint
-- shown the macOS way — right-aligned and greyed), or a nested submenu.
data MenuItem
  = MenuItem Text Command            -- ^ label, command
  | MenuShortcut Text Text Command   -- ^ label, shortcut hint, command
  | Submenu  Text [MenuItem]         -- ^ a labelled nested menu

-- | Convenience: a plain @(label, command)@ leaf.
item :: Text -> Command -> MenuItem
item = MenuItem

-- | A leaf whose shortcut is displayed in the native key-equivalent column.
-- The shortcut is display-only — tmux chords (@C-b X@) aren't single-chord macOS
-- key equivalents, so they can't be real 'NSMenuItem' key equivalents.
key :: Text -> Text -> Command -> MenuItem
key = MenuShortcut

menus :: [(Text, [MenuItem])]
menus =
  [ ("File",
      [ item "Open…"         CommandFileOpen
      , item "Open Project…" CommandProjectOpen
      , item "Save"          CommandFileSave
      , item "Close"         commandFileClose
      ])
  , ("Edit",
      [ item "Find" CommandFind
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
  , ("Tmux", tmuxMenu)
  ]

-- | Every default @tmux@ prefix (@C-b@) key binding, grouped into submenus, as
-- items that send the shortcut to the active terminal (see 'tmuxKey').  Each
-- shows its chord in the native shortcut column (@⌃B@ = the C-b prefix).  The
-- bytes following the prefix are printable keys as-is, arrows/meta as escape
-- sequences (@\\ESC[A@ = Up, @\\ESC1@ = M-1).
tmuxMenu :: [MenuItem]
tmuxMenu =
  [ Submenu "Sessions"
      [ key "Detach client"    "⌃B d" (tmuxKey "d")
      , key "Choose session"   "⌃B s" (tmuxKey "s")
      , key "Rename session"   "⌃B $" (tmuxKey "$")
      , key "Previous session" "⌃B (" (tmuxKey "(")
      , key "Next session"     "⌃B )" (tmuxKey ")")
      , key "Last session"     "⌃B L" (tmuxKey "L")
      ]
  , Submenu "Windows"
      [ key "New window"       "⌃B c" (tmuxKey "c")
      , key "Rename window"    "⌃B ," (tmuxKey ",")
      , key "Kill window"      "⌃B &" (tmuxKey "&")
      , key "Next window"      "⌃B n" (tmuxKey "n")
      , key "Previous window"  "⌃B p" (tmuxKey "p")
      , key "Last window"      "⌃B l" (tmuxKey "l")
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
          [ key "Above" "⌃B ↑" (tmuxKey "\ESC[A")
          , key "Below" "⌃B ↓" (tmuxKey "\ESC[B")
          , key "Right" "⌃B →" (tmuxKey "\ESC[C")
          , key "Left"  "⌃B ←" (tmuxKey "\ESC[D")
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
