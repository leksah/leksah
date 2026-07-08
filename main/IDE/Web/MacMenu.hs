{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The native macOS menu bar for leksah-wkwebview.
--
-- Builds an AppKit @NSMenu@ (via the Objective-C glue in
-- @main/leksah-mac-menu.m@) from the shared 'IDE.Web.MenuModel.menus', so the
-- macOS menu, the in-page web menubar, and the GTK app all run the same
-- 'Command's.  When a menu item is chosen, Objective-C calls back into
-- 'leksah_menu_action', which runs that command's 'IDEAction' in the IDE.
module IDE.Web.MacMenu
  ( installMacMenu
  , setupMacTitlebar
  ) where

import Control.Applicative ((<|>))
import Control.Lens ((^.), (.~), (?~), (&), (%~))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import System.Exit (ExitCode(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (exitImmediately)

import Language.Javascript.JSaddle.WKWebView (WKWebView(..), jsaddleMainHTMLWithBaseURL)

import IDE.Core.State (reflectIDE, readIDE, modifyIDE_)
import IDE.Core.Types
       (filePathToProjectKey, WindowId(..), webWindows, activeWindow,
        wwWide0, wwActive)
import IDE.Gtk.Workspaces (workspaceTry)
import IDE.Workspaces (projectOpenThis)
import IDE.Web.Command (Command(..), commandAction)
import IDE.Web.IDERefStore (getGlobalIDERef)
import IDE.Web.Main (jsMain, indexHtml, mintWindowId)
import IDE.Web.MenuModel (menus, MenuItem(..))
import IDE.Web.NewWindowRequest
       (setNewWindowHandler, setOpenWindowHandler, setRaiseWindowHandler)
import IDE.Web.OpenFileRequest (deliverOpenedFile)
import IDE.Web.OpenPanel (setOpenFilePanelHandler, setOpenProjectPanelHandler)
import IDE.Web.SaveRequest (requestSaveActiveFile)
import IDE.Web.SnapRequest (requestUnsnapPane)
import IDE.Web.FindRequest (requestToggleFindbar)
import IDE.Web.PreferencesRequest (requestShowPreferences)
import IDE.Web.WindowBridge (unregisterWindowBridge, unregisterResync)
import IDE.Web.ScreenshotRequest
       (registerScreenshotHandler, registerScreenshotRegionHandler)
import IDE.Web.ColorPick (setColorPickImpl, colorPicked)
import IDE.Web.RecentFiles (setRecentFilesHandler)
import IDE.Web.TerminalInput (setActiveTerminalNotifier)

foreign import ccall "leksah_menu_begin"    c_menuBegin   :: IO ()
foreign import ccall "leksah_menu_add_menu" c_menuAddMenu :: CString -> IO ()
foreign import ccall "leksah_menu_add_item" c_menuAddItem :: CString -> CInt -> IO ()
foreign import ccall "leksah_menu_add_item_kv" c_menuAddItemKV :: CString -> CString -> CInt -> IO ()
-- An item with a REAL key equivalent (spec like "cmd+shift+d"), enabled only
-- while a terminal tab is active (see leksah_set_terminal_active).
foreign import ccall "leksah_menu_add_item_key" c_menuAddItemKey :: CString -> CString -> CInt -> IO ()
-- Like add_item_key but NOT gated to a terminal — an always-available real key
-- equivalent (e.g. the AI menu's Grab Region).
foreign import ccall "leksah_menu_add_item_key_global" c_menuAddItemKeyGlobal :: CString -> CString -> CInt -> IO ()
foreign import ccall "leksah_menu_add_separator" c_menuAddSeparator :: IO ()
foreign import ccall "leksah_menu_push_submenu" c_menuPushSubmenu :: CString -> IO ()
foreign import ccall "leksah_menu_pop_submenu"  c_menuPopSubmenu  :: IO ()
foreign import ccall "leksah_menu_install"  c_menuInstall :: IO ()
-- Whether a terminal tab is on screen: gates the Terminal menu's key
-- equivalents so ⌘D etc. pass through to the editor otherwise.
foreign import ccall "leksah_set_terminal_active" c_setTerminalActive :: CInt -> IO ()
foreign import ccall "leksah_titlebar_setup" c_titlebarSetup :: IO ()
-- Create a native NSWindow + WKWebView for a freshly-minted 'WindowId'; the ObjC
-- glue calls back 'leksah_attach_window' once the webview exists so Haskell can
-- attach a jsaddle context (a second reflex network) to it.
foreign import ccall "leksah_new_window" c_newWindow :: CInt -> IO ()
-- Bring a specific window to the front (the global flipper's cross-window raise).
foreign import ccall "leksah_raise_window" c_raiseWindow :: CInt -> IO ()
-- Show the native "Open File" panel (NSOpenPanel); it calls back leksah_open_file.
foreign import ccall "leksah_show_open_panel" c_showOpenPanel :: IO ()
-- Show the native "Open Project" panel; it calls back leksah_open_project.
foreign import ccall "leksah_show_open_project_panel" c_showOpenProjectPanel :: IO ()
-- Populate the native "Open Recent" submenu (newline-separated paths).
foreign import ccall "leksah_set_recent_files" c_setRecentFiles :: CString -> IO ()
-- Snapshot the WKWebView content to a PNG at the given path; returns 1 on success.
foreign import ccall "leksah_screenshot" c_screenshot :: CString -> IO CInt
-- Snapshot just a rectangle (x,y,w,h in CSS px) of the WKWebView content.
foreign import ccall "leksah_snapshot_rect" c_snapshotRect
  :: CString -> CInt -> CInt -> CInt -> CInt -> IO CInt

-- | Called from Objective-C with the path chosen in the native open dialog.
foreign export ccall "leksah_open_file" leksah_open_file :: CString -> IO ()

leksah_open_file :: CString -> IO ()
leksah_open_file cstr = peekCString cstr >>= deliverOpenedFile

-- | Called from Objective-C with the project file chosen in the open-project
-- dialog; add it to the workspace, like the GTK projectOpen.
foreign export ccall "leksah_open_project" leksah_open_project :: CString -> IO ()

leksah_open_project :: CString -> IO ()
leksah_open_project cstr = do
  fp <- peekCString cstr
  case filePathToProjectKey fp of
    Nothing -> return ()
    Just pk -> getGlobalIDERef >>= \case
      Just ideR -> void $ reflectIDE (workspaceTry (projectOpenThis pk)) ideR
      Nothing   -> return ()

-- | Called from Objective-C when an Underlay ▸ Unsnap item is chosen; signals the
-- reflex layer (via the snap bridge) to drop that pane's window binding.
foreign export ccall "leksah_unsnap" leksah_unsnap :: CString -> IO ()

leksah_unsnap :: CString -> IO ()
leksah_unsnap cstr = peekCString cstr >>= requestUnsnapPane . T.pack

-- | Called from Objective-C when the app menu's "Settings…" item is chosen;
-- asks the reflex layer to show the Preferences pane (see 'showPrefsE').
foreign export ccall "leksah_open_settings" leksah_open_settings :: IO ()

leksah_open_settings :: IO ()
leksah_open_settings = requestShowPreferences

-- | Called from Objective-C once 'leksah_new_window' (or the restore path) has
-- created an NSWindow + WKWebView for 'wid': attach a fresh jsaddle context so a
-- new reflex network ('jsMain') renders leksah's UI into that webview.  The
-- 'WebWindow' for 'wid' was already seeded by 'mintWindowId'; 'jsMain' adopts it.
foreign export ccall "leksah_attach_window" leksah_attach_window :: CInt -> Ptr () -> IO ()

leksah_attach_window :: CInt -> Ptr () -> IO ()
leksah_attach_window widInt pWebView = getGlobalIDERef >>= \case
  Nothing   -> return ()
  Just ideR ->
    -- Flags match the first window's (main/WKWebView.hs: newIDE False True):
    -- hide the web menubar, use the native title bar.
    jsaddleMainHTMLWithBaseURL indexHtml baseURL
      (jsMain False True (Just (WindowId (fromIntegral widInt))) ideR)
      (WKWebView (castPtr pWebView))
  where baseURL = "http://127.0.0.1:3367"

-- | Called from Objective-C when a window becomes key (frontmost): record it as
-- the active window, so the process-wide bridges (close/save/find/…) and the
-- flipper's in-place actions target it.
foreign export ccall "leksah_window_activated" leksah_window_activated :: CInt -> IO ()

leksah_window_activated :: CInt -> IO ()
leksah_window_activated widInt = getGlobalIDERef >>= \case
  Nothing   -> return ()
  Just ideR -> reflectIDE (modifyIDE_ (activeWindow ?~ WindowId (fromIntegral widInt))) ideR

-- | Called from Objective-C when a window is closing: its wide0 tabs merge into
-- the frontmost remaining window (its 'activeWindow', else the lowest-id one);
-- closing the last window quits the app.  Also drops the window's bridge.
foreign export ccall "leksah_window_closing" leksah_window_closing :: CInt -> IO ()

leksah_window_closing :: CInt -> IO ()
leksah_window_closing widInt = getGlobalIDERef >>= \case
  Nothing   -> return ()
  Just ideR -> do
    let wid = WindowId (fromIntegral widInt)
    unregisterWindowBridge wid
    unregisterResync wid
    reflectIDE (do
      wins <- readIDE webWindows
      act  <- readIDE activeWindow
      case M.lookup wid wins of
        Nothing      -> return ()   -- already merged/gone
        Just closing -> do
          let others = M.delete wid wins
          case M.keys others of
            [] -> liftIO (exitImmediately ExitSuccess)   -- last window → quit
            _  -> do
              let target = case act of
                             Just a | a /= wid, M.member a others -> a
                             _ -> fst (M.findMin others)
                  merge tw = tw & wwWide0  %~ (++ closing ^. wwWide0)
                                & wwActive %~ (<|> closing ^. wwActive)
              modifyIDE_ $ \i -> i & webWindows  .~ M.adjust merge target others
                                   & activeWindow .~ Just target
      ) ideR

-- | The macOS app menu holds Settings… natively (see leksah-mac-menu.m), so
-- strip the Preferences command from the shared menu model when building the
-- Mac menu bar — filtering the same list that feeds both the item build and the
-- command-index table ('commandsRef') keeps their tags aligned.
stripPreferences :: [(Text, [MenuItem])] -> [(Text, [MenuItem])]
stripPreferences = map (\(t, items) -> (t, go items))
  where
    go = concatMap keep
    keep (MenuItem _ CommandShowPreferences) = []
    keep (Submenu l subs)                    = [Submenu l (go subs)]
    keep x                                   = [x]

-- Commands flattened in menu order; a menu item's tag indexes into this.
{-# NOINLINE commandsRef #-}
commandsRef :: IORef [Command]
commandsRef = unsafePerformIO (newIORef [])

-- Leaf commands of a menu item tree, in depth-first (pre-)order — the same order
-- the native menu is built, so tags line up.
flattenCmds :: [MenuItem] -> [Command]
flattenCmds = concatMap $ \case
  MenuItem _ cmd        -> [cmd]
  MenuShortcut _ _ cmd  -> [cmd]
  MenuKey _ _ cmd       -> [cmd]
  MenuGlobalKey _ _ cmd -> [cmd]
  MenuSep               -> []
  Submenu _ subs        -> flattenCmds subs

-- | Called from Objective-C when a menu item is chosen.
foreign export ccall "leksah_menu_action" leksah_menu_action :: CInt -> IO ()

foreign import ccall "leksah_pick_color" c_pickColor :: CString -> IO ()

-- | NSColorPanel reports each colour change here (as "#rrggbb").
leksah_color_picked :: CString -> IO ()
leksah_color_picked cs = peekCString cs >>= colorPicked . T.pack
foreign export ccall "leksah_color_picked" leksah_color_picked :: CString -> IO ()

leksah_menu_action :: CInt -> IO ()
leksah_menu_action tag = do
  cmds <- readIORef commandsRef
  case drop (fromIntegral tag) cmds of
    -- File ▸ Open / Open Project are handled natively (NSOpenPanel).
    (CommandFileOpen:_)    -> c_showOpenPanel
    (CommandProjectOpen:_) -> c_showOpenProjectPanel
    -- File ▸ Save acts on the active editor (reflex state); signal via the bridge.
    (CommandFileSave:_)    -> requestSaveActiveFile
    -- Edit ▸ Find toggles the find bar (reflex state); signal via the bridge.
    (CommandFind:_)        -> requestToggleFindbar
    (cmd:_) -> getGlobalIDERef >>= \case
      Just ideR -> case cmd ^. commandAction of
        Just act -> void $ reflectIDE act ideR
        Nothing  -> return ()  -- special commands (Save/…) have no IDEAction
      Nothing -> return ()
    [] -> return ()

-- | Build and install the native menu bar.  Safe to call before the app's run
-- loop starts; the actual menu-bar install is scheduled onto the main thread.
installMacMenu :: IO ()
installMacMenu = do
  -- Recent files are shown in the native "Open Recent" submenu.
  setRecentFilesHandler $ \fps -> withCString (intercalate "\n" fps) c_setRecentFiles
  -- Keep the native menu told whether a terminal tab is on screen, so the
  -- Terminal menu's key equivalents only fire then.
  setActiveTerminalNotifier $ \on -> c_setTerminalActive (if on then 1 else 0)
  -- The toolbar/menubar Open commands show the native open panels.
  setOpenFilePanelHandler c_showOpenPanel
  setOpenProjectPanelHandler c_showOpenProjectPanel
  -- File ▸ New Window: mint a WindowId (seeds an empty WebWindow), then ask the
  -- ObjC glue to create an NSWindow + WKWebView; it calls back leksah_attach_window.
  setNewWindowHandler $ getGlobalIDERef >>= \case
    Nothing   -> return ()
    Just ideR -> do
      WindowId n <- mintWindowId ideR
      c_newWindow (fromIntegral n)
  -- Restore: create a native window for an already-seeded window id (no mint).
  setOpenWindowHandler (c_newWindow . fromIntegral)
  -- Global flipper: bring another window to the front on cross-window select.
  setRaiseWindowHandler (c_raiseWindow . fromIntegral)
  -- The Preferences colour swatches open the native NSColorPanel (the web
  -- colour input's popover mis-anchors in our transparent-titlebar window).
  setColorPickImpl $ \hex -> withCString (T.unpack hex) c_pickColor
  -- `leksah-cmd screenshot FILE`: snapshot the WKWebView content to a PNG.
  registerScreenshotHandler $ \path ->
    (/= 0) <$> withCString (T.unpack path) c_screenshot
  -- Region grab's permission-free path: snapshot just the selected rectangle.
  registerScreenshotRegionHandler $ \path (x, y, w, h) ->
    withCString (T.unpack path) $ \p -> (/= 0) <$>
      c_snapshotRect p (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
  -- Settings… is added natively to the app menu, so drop it from the shared
  -- model here (both the tag table and the item build use this filtered list).
  let macMenus = stripPreferences menus
  -- Tags index this list; it must be the leaf commands in the same depth-first
  -- order that 'addItems' emits them (so a chosen item's tag finds its command).
  writeIORef commandsRef (concatMap (flattenCmds . snd) macMenus)
  c_menuBegin
  let loop _ [] = return ()
      loop tag ((title, items):rest) = do
        withCString (T.unpack title) c_menuAddMenu
        addItems tag items >>= \tag' -> loop tag' rest
      -- Emit items into the current (sub)menu, threading the running tag so leaf
      -- tags match 'flattenCmds'.  Submenus recurse between push/pop.
      addItems tag [] = return tag
      addItems tag (MenuItem label _ : rs) = do
        withCString (T.unpack label) $ \s -> c_menuAddItem s (fromIntegral tag)
        addItems (tag + 1) rs
      addItems tag (MenuShortcut label sc _ : rs) = do
        withCString (T.unpack label) $ \l ->
          withCString (T.unpack sc) $ \s -> c_menuAddItemKV l s (fromIntegral tag)
        addItems (tag + 1) rs
      addItems tag (MenuKey label spec _ : rs) = do
        withCString (T.unpack label) $ \l ->
          withCString (T.unpack spec) $ \s -> c_menuAddItemKey l s (fromIntegral tag)
        addItems (tag + 1) rs
      addItems tag (MenuGlobalKey label spec _ : rs) = do
        withCString (T.unpack label) $ \l ->
          withCString (T.unpack spec) $ \s -> c_menuAddItemKeyGlobal l s (fromIntegral tag)
        addItems (tag + 1) rs
      addItems tag (MenuSep : rs) = do
        c_menuAddSeparator
        addItems tag rs
      addItems tag (Submenu label subs : rs) = do
        withCString (T.unpack label) c_menuPushSubmenu
        tag' <- addItems tag subs
        c_menuPopSubmenu
        addItems tag' rs
  loop (0 :: Int) macMenus
  c_menuInstall

-- | Configure the native window so the web toolbar can occupy the title bar
-- (transparent title bar + full-size content view).  Safe to call before the
-- run loop starts; it retries on the main thread until the window exists.
setupMacTitlebar :: IO ()
setupMacTitlebar = c_titlebarSetup
