{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The native Win32 menu bar for leksah-webview2.
--
-- Builds an HMENU (via the C glue in @main/leksah-win-menu.c@) from the
-- shared 'IDE.Web.MenuModel.menus' — the same model the web menubar, the
-- macOS menu and the GTK menu use — and installs it on the window
-- jsaddle-webview2 created ('webView2Hwnd').  When a menu item is chosen the
-- subclassed window procedure calls back into 'leksah_menu_action', which
-- runs that command's 'IDEAction'.
--
-- Shortcut texts are display-only (shown in the menu's accelerator column):
-- the real key handling stays in the web page's keymap, since the
-- jsaddle-webview2 message loop has no TranslateAccelerator.
module IDE.Web.Win32Menu
  ( installWin32Menu
  ) where

import Control.Lens ((^.))
import Control.Monad (void)

import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)

import Language.Javascript.JSaddle.WebView2 (WebView2, webView2Hwnd)

import IDE.Core.State (reflectIDE)
import IDE.Gtk.Workspaces (workspaceTry)
import IDE.Workspaces (projectOpenPath)
import IDE.Web.Command (Command(..), commandAction)
import IDE.Web.IDERefStore (getGlobalIDERef)
import IDE.Web.MenuModel (menus, MenuItem(..))
import IDE.Web.OpenFileRequest (deliverOpenedFile)
import IDE.Web.OpenPanel
       (setOpenFilePanelHandler, setOpenProjectPanelHandler,
        setOpenFolderPanelHandler)
import IDE.Web.SaveRequest (requestSaveActiveFile)
import IDE.Web.FindRequest (requestToggleFindbar)
import IDE.Web.PreferencesRequest (requestShowPreferences)
import IDE.Web.ShortcutsRequest (requestShowShortcuts)
import IDE.Web.BrowserRequest (requestOpenBrowser)
import IDE.Web.KeymapRequest (requestKeymapCommand)
import IDE.Web.NewWindowRequest (setNewWindowHandler)
import IDE.Web.RecentFiles (setRecentFilesHandler)
import IDE.Web.TerminalInput
       (setActiveTerminalNotifier, setSplitActiveNotifier)

-- NOTE: multiple OS windows are not yet available on Windows.  The shared
-- multi-window machinery is complete and platform-neutral: the shared state
-- ('IDE.Core.Types.webWindows') already holds one 'WebWindow' per OS window,
-- each window would run its own reflex network ('IDE.Web.Main.jsMain') in its
-- own jsaddle context, coordinating through the shared state + resync
-- ('IDE.Web.WindowBridge'); the close-merge ('IDE.Web.WindowBridge.closeWindowMerge')
-- and the flipper's cross-window raise are ready to use.  What is MISSING is a
-- jsaddle-webview2 primitive to create a SECOND WebView2 window: the current C
-- shim (@jsaddle-webview2/cbits/WebView2Shim.c@, entry @runJsaddleWebView2@)
-- creates exactly ONE HWND + controller and owns the message loop, with no way
-- to spawn another window/context on that UI thread.
--
-- To finish Windows multi-window (all in jsaddle-webview2, then a few lines here):
--   1. Add a C entry @wv2CreateWindow(app, widthTag)@ that, ON THE UI THREAD
--      (PostMessage a WM_APP to the existing loop), creates another HWND +
--      ICoreWebView2Controller in the SAME environment and, once its controller
--      completes, calls back a Haskell export @leksah_attach_wv2 :: CInt ->
--      WebView2 -> IO ()@ — mirroring wkwebview's @c_newWindow@ /
--      @leksah_attach_window@.
--   2. That Haskell callback runs @jsaddleMainURL url (jsMain False False
--      (Just wid) ideR) newWebView@ (from 'IDE.Web.Main' + jsaddle-webview2), so
--      the new window gets its own context sharing the global 'IDERef'.
--   3. Subclass the new window's wndProc so WM_CLOSE calls
--      'closeWindowMerge' (…) wid (quit = PostQuitMessage on the last window),
--      and WM_ACTIVATE records the active window (the 'leksah_window_activated'
--      analog).
--   4. Register the handlers here exactly as 'IDE.Web.GtkApp' / 'IDE.Web.MacMenu'
--      do (setNewWindowHandler / setOpenWindowHandler / setRaiseWindowHandler).
-- Until then New Window reports that it is unavailable (see 'installWin32Menu').

foreign import ccall "leksah_win_menu_begin"    c_menuBegin   :: IO ()
foreign import ccall "leksah_win_menu_add_menu" c_menuAddMenu :: CString -> IO ()
-- label, accelerator display text ("" = none), tag, terminal-gated (1 = only
-- enabled while a terminal tab is active).
foreign import ccall "leksah_win_menu_add_item" c_menuAddItem
  :: CString -> CString -> CInt -> CInt -> IO ()
foreign import ccall "leksah_win_menu_add_separator" c_menuAddSeparator :: IO ()
foreign import ccall "leksah_win_menu_push_submenu" c_menuPushSubmenu :: CString -> IO ()
foreign import ccall "leksah_win_menu_pop_submenu"  c_menuPopSubmenu  :: IO ()
-- Install the built menu bar on the given HWND (also subclasses its wndProc
-- for WM_COMMAND dispatch and appends the Open Recent/Exit File items).
foreign import ccall "leksah_win_menu_install"  c_menuInstall :: Ptr () -> IO ()
-- Whether a terminal tab is on screen: enables/greys the gated items.
foreign import ccall "leksah_win_set_terminal_active" c_setTerminalActive :: CInt -> IO ()
-- Whether the active tab can convert to a tmux pane (an editor/git-log with a
-- backing pane): enables the Split items so ⌘D converts-and-splits.
foreign import ccall "leksah_win_set_split_active" c_setSplitActive :: CInt -> IO ()
-- Show the native "Open File"/"Open Project" dialog (marshalled to the UI
-- thread); it calls back leksah_open_file/leksah_open_project.
foreign import ccall "leksah_win_show_open_panel" c_showOpenPanel :: IO ()
foreign import ccall "leksah_win_show_open_project_panel" c_showOpenProjectPanel :: IO ()
-- Show the native "Open Folder" dialog; it also calls back leksah_open_project
-- (the handler adds a directory as a plain-directory project).
foreign import ccall "leksah_win_show_open_folder_panel" c_showOpenFolderPanel :: IO ()
-- Populate the "Open Recent" submenu (newline-separated paths).
foreign import ccall "leksah_win_set_recent_files" c_setRecentFiles :: CString -> IO ()

-- | Called from C with the path chosen in the native open dialog (or an Open
-- Recent item).
foreign export ccall "leksah_open_file" leksah_open_file :: CString -> IO ()

leksah_open_file :: CString -> IO ()
leksah_open_file cstr = peekCString cstr >>= deliverOpenedFile

-- | Called from C with the path chosen in the open-project OR open-folder
-- dialog; add it to the workspace.  'projectOpenPath' handles both: a directory
-- becomes a plain-directory project, a file is a project file.
foreign export ccall "leksah_open_project" leksah_open_project :: CString -> IO ()

leksah_open_project :: CString -> IO ()
leksah_open_project cstr = do
  fp <- peekCString cstr
  getGlobalIDERef >>= \case
    Just ideR -> void $ reflectIDE (workspaceTry (projectOpenPath fp)) ideR
    Nothing   -> return ()

-- | The Underlay submenu (pane transparency, window snapping) is macOS-only
-- window trickery; drop it.  Preferences stays in Edit — the Windows
-- convention keeps settings in the regular menus (no app menu to move it to).
stripMacOnly :: [(Text, [MenuItem])] -> [(Text, [MenuItem])]
stripMacOnly = map (\(t, items) -> (t, go items))
  where
    go = concatMap keep
    keep (Submenu "Underlay" _) = []
    keep (Submenu l subs)       = [Submenu l (go subs)]
    keep x                      = [x]

-- Commands flattened in menu order; a menu item's tag indexes into this.
{-# NOINLINE commandsRef #-}
commandsRef :: IORef [Command]
commandsRef = unsafePerformIO (newIORef [])

-- Leaf commands of a menu item tree, in depth-first (pre-)order — the same
-- order the native menu is built, so tags line up.
flattenCmds :: [MenuItem] -> [Command]
flattenCmds = concatMap $ \case
  MenuItem _ cmd        -> [cmd]
  MenuShortcut _ _ cmd  -> [cmd]
  MenuKey _ _ cmd       -> [cmd]
  MenuGlobalKey _ _ cmd -> [cmd]
  MenuSplitKey _ _ cmd  -> [cmd]
  MenuSep               -> []
  Submenu _ subs        -> flattenCmds subs

-- | Called from C when a menu item is chosen.
foreign export ccall "leksah_menu_action" leksah_menu_action :: CInt -> IO ()

leksah_menu_action :: CInt -> IO ()
leksah_menu_action tag = do
  cmds <- readIORef commandsRef
  case drop (fromIntegral tag) cmds of
    -- File ▸ Open / Open Project are handled natively (GetOpenFileName).
    (CommandFileOpen:_)        -> c_showOpenPanel
    (CommandProjectOpen:_)     -> c_showOpenProjectPanel
    (CommandProjectOpenFolder:_) -> c_showOpenFolderPanel
    -- These act on reflex state; signal via the bridges.
    (CommandFileSave:_)        -> requestSaveActiveFile
    (CommandFind:_)            -> requestToggleFindbar
    (CommandShowPreferences:_) -> requestShowPreferences
    (CommandShowShortcuts:_)   -> requestShowShortcuts
    (CommandOpenBrowser:_)     -> requestOpenBrowser
    (cmd:_) -> case cmd ^. commandAction of
      -- No IDEAction: handled inside the reflex network by matching the keymap
      -- event stream (flipper, next/previous error, …) — inject via the bridge.
      Nothing  -> requestKeymapCommand cmd
      Just act -> getGlobalIDERef >>= \case
        Just ideR -> void $ reflectIDE act ideR
        Nothing   -> return ()
    [] -> return ()

-- | Render a key spec (@\"cmd+shift+d\"@) or a mac-symbol hint (@\"⌃B d\"@)
-- the way Windows menus display shortcuts (@Ctrl+Shift+D@).  Display-only.
prettyWinKeySpec :: Text -> Text
prettyWinKeySpec spec
  | "+" `T.isInfixOf` spec && not (" " `T.isInfixOf` spec) =
      let parts = T.splitOn "+" spec
          (mods, keys) = span (`elem` ["cmd", "super", "shift", "alt", "opt", "ctrl"]) parts
          modTxt m = case m of
            "alt"   -> "Alt"
            "opt"   -> "Alt"
            "shift" -> "Shift"
            _       -> "Ctrl"   -- cmd/super/ctrl
          keyTxt k = case k of
            "" -> "+"           -- a trailing empty part means the key was '+'
            _  -> T.toUpper (T.take 1 k) <> T.drop 1 k
      in T.intercalate "+" (map modTxt mods <> map keyTxt keys)
  | otherwise =
      -- Mac-symbol hint strings (tmux chords like "⌃B d", "⌘⌥Y").
      T.replace "⌃" "Ctrl+" . T.replace "⌘" "Ctrl+"
        . T.replace "⌥" "Alt+" . T.replace "⇧" "Shift+" $ spec

-- | Build and install the native menu bar on the WebView2's window.  Must run
-- on the UI thread — i.e. inside jsaddle-webview2's @run'@ callback.
installWin32Menu :: WebView2 -> IO ()
installWin32Menu wv = do
  -- Recent files are shown in the native "Open Recent" submenu.
  setRecentFilesHandler $ \fps -> withCString (intercalate "\n" fps) c_setRecentFiles
  -- Keep the gated (terminal-pane) items told whether a terminal tab is on
  -- screen.  Local PTY terminals are disabled on Windows, but control-mode
  -- (ssh tmux) tabs still count.
  setActiveTerminalNotifier $ \on -> c_setTerminalActive (if on then 1 else 0)
  -- …and whether the active tab, though not a terminal, can convert to a tmux
  -- pane — enables the Split items so ⌘D converts-and-splits (macOS parity).
  setSplitActiveNotifier $ \on -> c_setSplitActive (if on then 1 else 0)
  -- The toolbar/menubar Open commands show the native open dialogs.
  setOpenFilePanelHandler c_showOpenPanel
  setOpenProjectPanelHandler c_showOpenProjectPanel
  setOpenFolderPanelHandler c_showOpenFolderPanel
  -- File ▸ New Window: multi-window needs a jsaddle-webview2 primitive that does
  -- not exist yet (see the module NOTE).  Give the user feedback instead of a
  -- silent no-op, so the command's absence is explicable rather than a bug.
  setNewWindowHandler $ hPutStrLn stderr
    "leksah: New Window is not yet supported on Windows (needs jsaddle-webview2 \
    \multi-window support; see IDE.Web.Win32Menu)."
  let winMenus = stripMacOnly menus
  -- Tags index this list; it must be the leaf commands in the same
  -- depth-first order that 'addItems' emits them.
  writeIORef commandsRef (concatMap (flattenCmds . snd) winMenus)
  c_menuBegin
  let addItem label accel tag gated =
        withCString (T.unpack label) $ \l ->
          withCString (T.unpack (prettyWinKeySpec accel)) $ \a ->
            c_menuAddItem l a (fromIntegral tag) gated
      loop _ [] = return ()
      loop tag ((title, items):rest) = do
        withCString (T.unpack title) c_menuAddMenu
        addItems tag items >>= \tag' -> loop tag' rest
      addItems tag [] = return (tag :: Int)
      addItems tag (MenuItem label _ : rs) = do
        withCString (T.unpack label) $ \l ->
          withCString "" $ \a -> c_menuAddItem l a (fromIntegral tag) 0
        addItems (tag + 1) rs
      addItems tag (MenuShortcut label sc _ : rs) = do
        addItem label sc tag 0
        addItems (tag + 1) rs
      addItems tag (MenuKey label spec _ : rs) = do
        addItem label spec tag 1
        addItems (tag + 1) rs
      addItems tag (MenuGlobalKey label spec _ : rs) = do
        addItem label spec tag 0
        addItems (tag + 1) rs
      -- Split items: gate mode 2 = enabled on a terminal OR a convertible
      -- editor/git-log tab (⌘D converts-and-splits), matching macOS.
      addItems tag (MenuSplitKey label spec _ : rs) = do
        addItem label spec tag 2
        addItems (tag + 1) rs
      addItems tag (MenuSep : rs) = do
        c_menuAddSeparator
        addItems tag rs
      addItems tag (Submenu label subs : rs) = do
        withCString (T.unpack label) c_menuPushSubmenu
        tag' <- addItems tag subs
        c_menuPopSubmenu
        addItems tag' rs
  loop 0 winMenus
  hwnd <- webView2Hwnd wv
  c_menuInstall hwnd
