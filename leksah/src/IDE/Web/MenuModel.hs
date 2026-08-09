{-# LANGUAGE OverloadedStrings #-}
-- | The menu model: top-level menus, each a tree of items.  The SOURCE model
-- ('menuModel') names commands by their registry id (\"package.build\"), so a
-- menu item's key equivalent comes from the live keybindings table — a user
-- rebind in @keybindings.json@ shows up in the native menus and the web
-- menubar alike.  'renderedMenus' resolves the source model against a
-- 'Keymap' into the 'MenuItem' shape the consumers eat (the web menubar
-- `IDE.Web.Widget.Menubar`, the native macOS/Gtk/Win32 menus, and the
-- Shortcuts pane), so they stay in sync by construction.
--
-- The tmux submenu's @⌃B x@ chords are DISPLAY hints, not key equivalents
-- (a two-chord sequence can't be an @NSMenuItem@ equivalent): those items
-- keep an explicit label + hint + 'Command' and bypass the registry.
module IDE.Web.MenuModel
  ( MenuItem(..)
  , MenuEntry(..)
  , menuModel
  , renderedMenus
  , prettyKeySpec
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)

import IDE.Web.Chord (parseChord, toGlyphs, toNativeSpec)
import IDE.Web.Command
       (Command(..), paneCmd, tmuxKey)
import IDE.Web.Commands (lookupCommand)
import IDE.Web.Keybindings
       (Binding(..), CommandSpec(..), Keymap, When(..), bindingFor)

-- | One entry of the source model.
data MenuEntry
  = MCmd Text                 -- ^ a registry command id; label = its title,
                              --   key equivalent = its (re)binding
  | MHint Text Text Command   -- ^ label, display-only shortcut hint, command
                              --   (the tmux ⌃B chords)
  | MSepE
  | MSubE Text [MenuEntry]

-- | One RENDERED entry (unchanged shape — the native menu builders and the
-- web menubar consume this).
data MenuItem
  = MenuItem Text Command            -- ^ label, command (no shortcut)
  | MenuShortcut Text Text Command   -- ^ label, display hint, command
  | MenuKey Text Text Command        -- ^ label, real key equivalent spec,
                                     --   enabled only while a terminal is
                                     --   active, command
  | MenuGlobalKey Text Text Command  -- ^ like 'MenuKey' but always enabled
  | MenuSplitKey Text Text Command   -- ^ like 'MenuKey' but enabled while a
                                     --   terminal OR a convertible tab is
                                     --   active (the Split items)
  | MenuSep
  | Submenu  Text [MenuItem]

-- | Render a chord spec (@\"cmd+shift+d\"@) the way macOS displays shortcuts
-- (@⇧⌘D@) — for the web menubar, which shows the hint as text.
prettyKeySpec :: Text -> Text
prettyKeySpec spec = maybe spec toGlyphs (parseChord spec)

-- | Resolve the source model against the keybindings table.
renderedMenus :: Keymap -> [(Text, [MenuItem])]
renderedMenus km = [ (title, mapMaybe render items) | (title, items) <- menuModel ]
  where
    render (MHint l h c) = Just (MenuShortcut l h c)
    render MSepE         = Just MenuSep
    render (MSubE t es)  = Just (Submenu t (mapMaybe render es))
    render (MCmd cid)    = do
        spec <- lookupCommand cid
        cmd  <- csMake spec Nothing
        return $ case bindingFor km cid of
            Nothing -> MenuItem (csTitle spec) cmd
            Just b  ->
                let ks = toNativeSpec (bChord b)
                in case csWhen spec of
                    WhenAlways                -> MenuGlobalKey (csTitle spec) ks cmd
                    WhenTerminal              -> MenuKey       (csTitle spec) ks cmd
                    WhenTerminalOrConvertible -> MenuSplitKey  (csTitle spec) ks cmd

-- | A display-hint leaf (the tmux chords).
key :: Text -> Text -> Command -> MenuEntry
key = MHint

menuModel :: [(Text, [MenuEntry])]
menuModel =
  -- "Workspace" is the app's File menu (first after the app menu): the
  -- file/project items plus what used to be a separate Workspace menu.
  -- NB the native front ends hang OS furniture off this menu BY TITLE
  -- (Open Recent in leksah-mac-menu.m leksah_ensure_recent_menu, Open
  -- Recent + Quit in GtkMenu.hs addTop) — keep the title in sync there.
  [ ("Workspace",
      [ MCmd "workspace.newWindow"
      , MSepE
      , MCmd "workspace.openFile"
      , MCmd "workspace.openProject"
      , MCmd "workspace.openFolder"
      , MCmd "workspace.addRemoteProject"
      , MCmd "workspace.addServer"
      , MCmd "workspace.saveFile"
      , MCmd "workspace.closeFile"
      , MSepE
      , MCmd "workspace.refreshNix"
      ])
  , ("Edit",
      [ MCmd "edit.find"
      , MCmd "edit.showShortcuts"
      , MCmd "edit.reloadKeybindings"
      , MCmd "edit.showPreferences"
      ])
  , ("Package",
      [ MCmd "package.addModule"
      , MCmd "package.clean"
      , MCmd "package.build"
      , MCmd "package.run"
      , MCmd "package.runJavaScript"
      ])
  , ("Build",
      [ MCmd "build.toggleBackground"
      , MCmd "build.toggleNative"
      , MCmd "build.toggleJavaScript"
      , MCmd "build.toggleGhci"
      , MCmd "build.toggleDocs"
      , MCmd "build.toggleTests"
      , MCmd "build.toggleBenchmarks"
      , MCmd "build.toggleDependents"
      ])
  , ("Errors",
      [ MCmd "errors.next"
      , MCmd "errors.previous"
      ])
  , ("View",
      -- The tab flipper.  mod+` steps it; the flip commits when the modifier
      -- is released (the keymap's flip-done).  The menu equivalents matter
      -- when focus is inside a cross-origin iframe (the DOM keymap can't see
      -- keys there); Main.hs pulls focus out of the iframe on the first step
      -- so the commit keyup reaches the page.
      [ MCmd "view.nextTab"
      , MCmd "view.previousTab"
      , MSepE
      -- Whole-window page zoom, then one pane's font: the coarse control
      -- first, since it is the one on the unmodified chords.
      , MCmd "view.zoomIn"
      , MCmd "view.zoomOut"
      , MCmd "view.zoomReset"
      , MSepE
      , MCmd "view.fontBigger"
      , MCmd "view.fontSmaller"
      , MCmd "view.fontReset"
      , MSepE
      -- An embedded web page with minimal chrome (address bar, back/forward);
      -- devtools for it via right-click ▸ Inspect Element inside the page.
      , MCmd "view.newBrowserPane"
      ])
  , ("Terminal", terminalMenu)
  , ("AI",
      [ MCmd "ai.sendSelection"
      , MCmd "ai.sendFileRef"
      , MCmd "ai.sendError"
      , MCmd "ai.focusTerminal"
      , MCmd "ai.grabRegion"
      , MSepE
      , MCmd "ai.newClaudeSession"
      , MCmd "ai.continueClaudeSession"
      ])
  ]

-- | The Terminal menu: iTerm2's \"Shell\" grouping (everything terminal-ish in
-- one menu) with Ghostty's item names and default shortcuts.  The split/pane
-- commands work on BOTH kinds of terminal tab — real commands over the
-- control channel for CC (⊞) tabs, the @C-b@ chord typed into the PTY for
-- classic (▭) tabs.  Their ⌘ key equivalents are real but native-menu-gated
-- (the commands' when-context), so ⌘D, ⌘[/⌘], ⌘⌥arrows etc. still reach the
-- editor otherwise.
terminalMenu :: [MenuEntry]
terminalMenu =
  [ MCmd "terminal.newWindow"
  , MCmd "terminal.previousWindow"
  , MCmd "terminal.nextWindow"
  , MSepE
  , MCmd "terminal.splitRight"
  , MCmd "terminal.splitDown"
  , MSepE
  , MSubE "Select Split"
      [ MCmd "terminal.selectSplitAbove"
      , MCmd "terminal.selectSplitBelow"
      , MCmd "terminal.selectSplitLeft"
      , MCmd "terminal.selectSplitRight"
      , MSepE
      , MCmd "terminal.selectPreviousSplit"
      , MCmd "terminal.selectNextSplit"
      ]
  , MSubE "Resize Split"
      [ MCmd "terminal.equalizeSplits"
      , MSepE
      , MCmd "terminal.moveDividerUp"
      , MCmd "terminal.moveDividerDown"
      , MCmd "terminal.moveDividerLeft"
      , MCmd "terminal.moveDividerRight"
      ]
  , MCmd "terminal.zoomSplit"
  , MCmd "terminal.closeSplit"
  , MSepE
  , MSubE "Underlay"
      [ MCmd "terminal.togglePaneTransparency"
      , MCmd "terminal.snapWindowToPane"
      -- Populated natively from the currently-snapped windows (see leksah-mac-menu.m).
      , MSubE "Unsnap" []
      ]
  , MSepE
  , MCmd "terminal.focusAlerting"
  , MCmd "terminal.toggleTmuxIntercept"
  , MSubE "Tmux" tmuxMenu
  ]

-- | Every default @tmux@ prefix (@C-b@) key binding, grouped into submenus.
-- Each shows its chord in the native shortcut column (@⌃B@ = the C-b prefix).
--
-- Items that map to a concrete tmux command use 'paneCmd', so they work on
-- control-mode (⊞ / CC) tabs — the common case — by running that command over
-- the control channel, falling back to typing the @C-b@ chord on a classic
-- (▭, attached-client) PTY tab.  The remaining items are genuinely
-- interactive (choose-tree, a command-prompt dialog, find/rename, display-panes,
-- list-keys, the clock, …): tmux only renders those in an attached client, so
-- they stay 'tmuxKey' (chord-only, i.e. PTY tabs).  For 'paneCmd' the second
-- argument is the same chord byte the 'tmuxKey' used, so the PTY behaviour is
-- unchanged; only CC tabs gain the action.
tmuxMenu :: [MenuEntry]
tmuxMenu =
  [ MSubE "Sessions"
      -- Session-level bindings steer the *client's* session and are either
      -- interactive or would repoint/detach leksah's per-tab control client, so
      -- they stay chord-only (PTY tabs).
      [ key "Detach client"    "⌃B d" (tmuxKey "d")
      , key "Choose session"   "⌃B s" (tmuxKey "s")
      , key "Rename session"   "⌃B $" (tmuxKey "$")
      , key "Previous session" "⌃B (" (tmuxKey "(")
      , key "Next session"     "⌃B )" (tmuxKey ")")
      -- ⌃B L is rebound to resize-pane-right (vim HJKL), so last-session moves to ⇧Tab.
      , key "Last session"     "⌃B ⇧Tab" (tmuxKey "\ESC[Z")
      ]
  , MSubE "Windows"
      [ key "New window"       "⌃B c" (paneCmd "new-window" "c")
      , key "Rename window"    "⌃B ," (tmuxKey ",")           -- prompt
      , key "Kill window"      "⌃B &" (paneCmd "kill-window" "&")
      , key "Next window"      "⌃B n" (paneCmd "next-window" "n")
      , key "Previous window"  "⌃B p" (paneCmd "previous-window" "p")
      -- ⌃B l is rebound to select-pane-right (vim hjkl), so last-window moves to Tab.
      , key "Last window"      "⌃B Tab" (paneCmd "last-window" "\t")
      , key "Choose window"    "⌃B w" (tmuxKey "w")           -- chooser overlay
      , key "Find window"      "⌃B f" (tmuxKey "f")           -- prompt
      , key "Select by index"  "⌃B '" (tmuxKey "'")           -- prompt
      , key "Move window"      "⌃B ." (tmuxKey ".")           -- prompt
      , MSubE "Select window"
          [ key "Window 0" "⌃B 0" (paneCmd "select-window -t :0" "0")
          , key "Window 1" "⌃B 1" (paneCmd "select-window -t :1" "1")
          , key "Window 2" "⌃B 2" (paneCmd "select-window -t :2" "2")
          , key "Window 3" "⌃B 3" (paneCmd "select-window -t :3" "3")
          , key "Window 4" "⌃B 4" (paneCmd "select-window -t :4" "4")
          , key "Window 5" "⌃B 5" (paneCmd "select-window -t :5" "5")
          , key "Window 6" "⌃B 6" (paneCmd "select-window -t :6" "6")
          , key "Window 7" "⌃B 7" (paneCmd "select-window -t :7" "7")
          , key "Window 8" "⌃B 8" (paneCmd "select-window -t :8" "8")
          , key "Window 9" "⌃B 9" (paneCmd "select-window -t :9" "9")
          ]
      ]
  , MSubE "Panes"
      [ key "Split left/right"     "⌃B %"  (paneCmd "split-window -h" "%")
      , key "Split top/bottom"     "⌃B \"" (paneCmd "split-window -v" "\"")
      , key "Next pane"            "⌃B o"  (paneCmd "select-pane -t :.+" "o")
      , key "Last pane"            "⌃B ;"  (paneCmd "last-pane" ";")
      , key "Swap pane up"         "⌃B {"  (paneCmd "swap-pane -U" "{")
      , key "Swap pane down"       "⌃B }"  (paneCmd "swap-pane -D" "}")
      , key "Rotate panes"         "⌃B ⌃O" (paneCmd "rotate-window" "\SI")
      , key "Kill pane"            "⌃B x"  (paneCmd "kill-pane" "x")
      , key "Zoom/unzoom pane"     "⌃B z"  (paneCmd "resize-pane -Z" "z")
      , key "Break pane to window" "⌃B !"  (paneCmd "break-pane" "!")
      , key "Show pane numbers"    "⌃B q"  (tmuxKey "q")      -- transient overlay
      , key "Mark pane"            "⌃B m"  (paneCmd "select-pane -m" "m")
      , key "Clear marked pane"    "⌃B M"  (paneCmd "select-pane -M" "M")
      , MSubE "Select pane"
          [ key "Above" "⌃B ↑ / k" (paneCmd "select-pane -U" "\ESC[A")
          , key "Below" "⌃B ↓ / j" (paneCmd "select-pane -D" "\ESC[B")
          , key "Right" "⌃B → / l" (paneCmd "select-pane -R" "\ESC[C")
          , key "Left"  "⌃B ← / h" (paneCmd "select-pane -L" "\ESC[D")
          ]
      , MSubE "Resize pane"
          [ key "Up (5)"    "⌃B K"  (paneCmd "resize-pane -U 5" "K")
          , key "Down (5)"  "⌃B J"  (paneCmd "resize-pane -D 5" "J")
          , key "Right (5)" "⌃B L"  (paneCmd "resize-pane -R 5" "L")
          , key "Left (5)"  "⌃B H"  (paneCmd "resize-pane -L 5" "H")
          , key "Up (1)"    "⌃B ⌃k" (paneCmd "resize-pane -U 1" "\v")
          , key "Down (1)"  "⌃B ⌃j" (paneCmd "resize-pane -D 1" "\n")
          , key "Right (1)" "⌃B ⌃l" (paneCmd "resize-pane -R 1" "\f")
          , key "Left (1)"  "⌃B ⌃h" (paneCmd "resize-pane -L 1" "\b")
          ]
      ]
  , MSubE "Layout"
      [ key "Next layout"     "⌃B Space" (paneCmd "next-layout" " ")
      , key "Even horizontal" "⌃B ⌥1"    (paneCmd "select-layout even-horizontal" "\ESC1")
      , key "Even vertical"   "⌃B ⌥2"    (paneCmd "select-layout even-vertical" "\ESC2")
      , key "Main horizontal" "⌃B ⌥3"    (paneCmd "select-layout main-horizontal" "\ESC3")
      , key "Main vertical"   "⌃B ⌥4"    (paneCmd "select-layout main-vertical" "\ESC4")
      , key "Tiled"           "⌃B ⌥5"    (paneCmd "select-layout tiled" "\ESC5")
      ]
  , MSubE "Copy & Buffers"
      [ key "Copy (scroll) mode" "⌃B [" (paneCmd "copy-mode" "[")
      , key "Paste buffer"       "⌃B ]" (paneCmd "paste-buffer" "]")
      , key "Choose buffer"      "⌃B =" (tmuxKey "=")         -- chooser overlay
      , key "List paste buffers" "⌃B #" (tmuxKey "#")         -- list overlay
      ]
  , MSubE "Misc"
      [ key "Command prompt"    "⌃B :" (tmuxKey ":")          -- prompt
      , key "List key bindings" "⌃B ?" (tmuxKey "?")          -- list overlay
      , key "Clock"             "⌃B t" (tmuxKey "t")          -- clock mode
      , key "Refresh client"    "⌃B r" (paneCmd "refresh-client" "r")
      , key "Show messages"     "⌃B ~" (tmuxKey "~")          -- message log overlay
      ]
  ]
