{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The native macOS menu bar for leksah-wkwebview.
--
-- Builds an AppKit @NSMenu@ (via the Objective-C glue in
-- @main/leksah-mac-menu.m@) from the shared 'IDE.Web.MenuModel.menus', so the
-- macOS menu, the in-page web menubar, and the GTK app all run the same
-- 'Command's.  When a menu item is chosen, Objective-C calls back into
-- Haskell, which runs that command's 'IDEAction' in the IDE.
--
-- The @foreign import@/@foreign export@ layer lives in 'IDE.Web.MacGlue' (the
-- compiled @leksah-mac-glue@ sublibrary — @foreign export@ is illegal in
-- interpreted code, and this module is bytecode under @leksah.sh --ghci@).
-- This module supplies the behaviour: 'installMacMenu' registers the
-- 'MacCallbacks' the glue's exports dispatch to.
module IDE.Web.MacMenu
  ( installMacMenu
  , setupMacTitlebar
  ) where

import Control.Lens ((^.), (?~), to)
import Control.Monad (void, when)

import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import Data.Text.Encoding (encodeUtf8)

import Foreign.C.String (withCString)
import Foreign.Ptr (Ptr, castPtr)
import System.Exit (ExitCode(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (exitImmediately)

import Language.Javascript.JSaddle.WKWebView (WKWebView(..), jsaddleMainHTMLWithBaseURL)

import IDE.Core.State (reflectIDE, modifyIDE_, readIDE)
import IDE.Core.Types (WindowId(..), activeWindow)
import IDE.Gtk.Workspaces (workspaceTry)
import IDE.Workspaces (projectOpenPath)
import IDE.Web.Command (Command(..), commandAction, commandGetToggleState)
import IDE.Web.GhciMode
       (ghciMode, registerGhciCleanup, setGhciStop, stopForGhci)
import IDE.Web.IDERefStore (getGlobalIDERef)
import IDE.Web.Instance (leksahPort)
import IDE.Web.MacGlue
import IDE.Web.Main (jsMain, indexHtml, mintWindowId)
import IDE.Web.MenuModel (menus, MenuItem(..))
import IDE.Web.NewWindowRequest
       (setNewWindowHandler, setOpenWindowHandler, setRaiseWindowHandler)
import IDE.Web.OpenFileRequest (deliverOpenedFile)
import IDE.Web.OpenPanel
       (setOpenFilePanelHandler, setOpenProjectPanelHandler,
        setOpenFolderPanelHandler)
import IDE.Web.PreferencesRequest (requestShowPreferences)
import IDE.Web.SaveRequest (requestSaveActiveFile)
import IDE.Web.SnapRequest (requestUnsnapPane)
import IDE.Web.FindRequest (requestToggleFindbar)
import IDE.Web.ShortcutsRequest (requestShowShortcuts)
import IDE.Web.AddRemoteRequest (requestAddRemoteProject)
import IDE.Web.WindowBridge (closeWindowMerge)
import IDE.Web.ScreenshotRequest
       (registerScreenshotHandler, registerScreenshotRegionHandler)
import IDE.Web.ColorPick (setColorPickImpl, colorPicked)
import IDE.Web.RecentFiles (setRecentFilesHandler)
import IDE.Web.TerminalInput (setActiveTerminalNotifier, setSplitActiveNotifier)

-- | Called from Objective-C (via the glue) with the path chosen in the
-- open-project OR open-folder dialog; add it to the workspace, like the GTK
-- projectOpen.  'projectOpenPath' handles both: a directory becomes a
-- plain-directory project, a file is a project file (cabal.project / …).
macOpenProject :: FilePath -> IO ()
macOpenProject fp = getGlobalIDERef >>= \case
  Just ideR -> void $ reflectIDE (workspaceTry (projectOpenPath fp)) ideR
  Nothing   -> return ()

-- | Called (via the glue) once 'c_newWindow' (or the restore path) has
-- created an NSWindow + WKWebView for 'wid': attach a fresh jsaddle context so a
-- new reflex network ('jsMain') renders leksah's UI into that webview.  The
-- 'WebWindow' for 'wid' was already seeded by 'mintWindowId'; 'jsMain' adopts it.
macAttachWindow :: Int -> Ptr () -> IO ()
macAttachWindow widInt pWebView = getGlobalIDERef >>= \case
  Nothing   -> return ()
  Just ideR ->
    -- Flags match the first window's (main/WKWebView.hs: newIDE False True):
    -- hide the web menubar, use the native title bar.
    jsaddleMainHTMLWithBaseURL indexHtml baseURL
      (jsMain False True (Just (WindowId widInt)) ideR)
      (WKWebView (castPtr pWebView))
  -- Same port the first window's jsaddle server bound (see 'IDE.Web.Instance');
  -- a second instance on a different LEKSAH_PORT points its webviews at its own.
  where baseURL = encodeUtf8 (T.pack ("http://127.0.0.1:" <> show leksahPort))

-- | A window became key (frontmost): record it as the active window, so the
-- process-wide bridges (close/save/find/…) and the flipper's in-place actions
-- target it.
macWindowActivated :: Int -> IO ()
macWindowActivated widInt = getGlobalIDERef >>= \case
  Nothing   -> return ()
  Just ideR -> reflectIDE (modifyIDE_ (activeWindow ?~ WindowId widInt)) ideR

-- | A window is closing: its wide0 tabs merge into the frontmost remaining
-- window (its 'activeWindow', else the lowest-id one); closing the last window
-- quits the app.  Also drops the window's bridge.  The merge itself lives in
-- the shared 'closeWindowMerge' (identical across platforms); only the
-- last-window quit is macOS-specific (hard exit — or, in ghci mode, a stop
-- back to the prompt so the ghci session survives).
macWindowClosing :: Int -> IO ()
macWindowClosing widInt =
  closeWindowMerge lastWindowQuit (WindowId widInt)
  where lastWindowQuit | ghciMode  = stopForGhci
                       | otherwise = exitImmediately ExitSuccess

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
  MenuSplitKey _ _ cmd  -> [cmd]
  MenuSep               -> []
  Submenu _ subs        -> flattenCmds subs

-- | Called (via the glue) when a menu item is chosen.
macMenuAction :: Int -> IO ()
macMenuAction tag = do
  cmds <- readIORef commandsRef
  case drop tag cmds of
    -- File ▸ Open / Open Project are handled natively (NSOpenPanel).
    (CommandFileOpen:_)    -> c_showOpenPanel
    (CommandProjectOpen:_) -> c_showOpenProjectPanel
    (CommandProjectOpenFolder:_) -> c_showOpenFolderPanel
    -- Add Remote Project… opens a reflex modal (host/path/prefix); signal it
    -- via the bridge, like Find/Save.
    (CommandProjectAddRemote:_) -> requestAddRemoteProject
    -- File ▸ Save acts on the active editor (reflex state); signal via the bridge.
    (CommandFileSave:_)    -> requestSaveActiveFile
    -- Edit ▸ Find toggles the find bar (reflex state); signal via the bridge.
    (CommandFind:_)        -> requestToggleFindbar
    -- Edit ▸ Keyboard Shortcuts opens the reflex cheat-sheet pane; bridge it.
    (CommandShowShortcuts:_) -> requestShowShortcuts
    (cmd:_) -> getGlobalIDERef >>= \case
      Just ideR -> case cmd ^. commandAction of
        Just act -> void $ reflectIDE act ideR
        Nothing  -> return ()  -- special commands (Save/…) have no IDEAction
      Nothing -> return ()
    [] -> return ()

-- | Report a menu item's live toggle state to the native validateMenuItem (so
-- a toggle command shows a checkmark): -1 if the item's command isn't a toggle,
-- else 0/1 from its state getter read off the current IDE.  @tag@ indexes
-- 'commandsRef' exactly as 'macMenuAction'.
macToggleState :: Int -> IO Int
macToggleState tag = do
  cmds <- readIORef commandsRef
  case drop tag cmds of
    (c:_) | Just f <- commandGetToggleState c ->
      getGlobalIDERef >>= \case
        Just ideR -> (\on -> if on then 1 else 0) <$> reflectIDE (readIDE (to f)) ideR
        Nothing   -> return (-1)
    _ -> return (-1)

-- | Build and install the native menu bar.  Safe to call before the app's run
-- loop starts; the actual menu-bar install is scheduled onto the main thread.
installMacMenu :: IO ()
installMacMenu = do
  -- Everything Objective-C calls back into: registered before anything can
  -- trigger a native callback (we run at the top of main).
  setMacCallbacks MacCallbacks
    { cbMenuAction      = macMenuAction
    , cbOpenFile        = deliverOpenedFile
    , cbOpenProject     = macOpenProject
    , cbUnsnap          = requestUnsnapPane . T.pack
    , cbOpenSettings    = requestShowPreferences
    , cbAttachWindow    = macAttachWindow
    , cbWindowActivated = macWindowActivated
    , cbWindowClosing   = macWindowClosing
    , cbColorPicked     = colorPicked . T.pack
    , cbToggleState     = macToggleState
    }
  when ghciMode $ do
    -- Returning to the ghci prompt = stopping the Cocoa run loop; and Cocoa
    -- must not terminate the process (the ghci session!) on last-window-close.
    setGhciStop stopApp
    disableAutoTerminate
    -- Full teardown (stopForGhci) closes the windows so the old reflex
    -- networks die before :reload'd code starts a fresh :main.  Registered
    -- first = run last (LIFO), after the listeners are gone.
    registerGhciCleanup closeAllWindows
  -- Recent files are shown in the native "Open Recent" submenu.
  setRecentFilesHandler $ \fps -> withCString (intercalate "\n" fps) c_setRecentFiles
  -- Keep the native menu told whether a terminal tab is on screen, so the
  -- Terminal menu's key equivalents only fire then.
  setActiveTerminalNotifier $ \on -> c_setTerminalActive (if on then 1 else 0)
  -- …and whether the active tab, though not a terminal, can convert to a tmux
  -- pane (editor/git-log with a backing pane) — enables the Split items so
  -- ⌘D converts-and-splits.
  setSplitActiveNotifier $ \on -> c_setSplitActive (if on then 1 else 0)
  -- The toolbar/menubar Open commands show the native open panels.
  setOpenFilePanelHandler c_showOpenPanel
  setOpenProjectPanelHandler c_showOpenProjectPanel
  setOpenFolderPanelHandler c_showOpenFolderPanel
  -- File ▸ New Window: mint a WindowId (seeds an empty WebWindow), then ask the
  -- ObjC glue to create an NSWindow + WKWebView; it calls back macAttachWindow.
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
      addItems tag (MenuSplitKey label spec _ : rs) = do
        withCString (T.unpack label) $ \l ->
          withCString (T.unpack spec) $ \s -> c_menuAddItemKeySplittable l s (fromIntegral tag)
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
