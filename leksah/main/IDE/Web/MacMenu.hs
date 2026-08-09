{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The native macOS menu bar for leksah-wkwebview.
--
-- Builds an AppKit @NSMenu@ (via the Objective-C glue in
-- @main/leksah-mac-menu.m@) from the shared 'IDE.Web.MenuModel.menus', so the
-- macOS menu, the in-page web menubar, and the GTK app all run the same
-- 'Command's.  When a menu item is chosen, Objective-C calls back into
-- Haskell, which runs that command's 'AppAction' against the booted 'App'.
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

import Control.Concurrent (forkIO)
import Control.Lens ((^.), (?~))
import Control.Monad (void, when)

import Data.IORef (IORef, newIORef, writeIORef, readIORef, atomicModifyIORef')
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Foreign.C.String (withCString)
import Foreign.Ptr (Ptr, castPtr)
import System.Exit (ExitCode(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (exitImmediately)

import Language.Javascript.JSaddle.WKWebView
       (WKWebView(..), jsaddleMainHTMLWithBaseURL, jsaddleWebViewInvalidate)

import IDE.App
       (appConfig, appUi, appWorkspace, getGlobalApp, withApp)
import IDE.Config (currentConfig)
import IDE.Reactive (modifyCell)
import IDE.Web.Model (WindowId(..), activeWindow)
import IDE.Workspace (projectOpenPath)
import IDE.Web.Claude (showLiveSession)
import IDE.Web.ClaudeStatus
       (ClaudeStatus(..), ClaudeStatusRow(..), registerClaudeStatusPush)
import IDE.Web.Command (Command(..), commandAction, commandGetToggleState)
import IDE.Web.GhciMode
       (ghciMode, registerGhciCleanupNamed, registerGhciQuiesceNamed, setGhciStop,
        stopForGhci, phaseLog)
import IDE.Web.Instance (leksahPort)
import IDE.Web.MacGlue
import IDE.Web.Main (jsMain, indexHtml, mintWindowId)
import IDE.Web.Keybindings (registerKeymapListener)
import IDE.Web.MenuModel (renderedMenus, MenuItem(..))
import IDE.Web.NativeBrowser (NativeBrowserOps(..), setNativeBrowserOps)
import IDE.Web.NewWindowRequest
       (setNewWindowHandler, setOpenWindowHandler, setRaiseWindowHandler,
        setOrderWindowFrontHandler, setCloseWindowHandler)
import IDE.Web.OpenFileRequest (deliverOpenedFile)
import IDE.Web.OpenPanel
       (setOpenFilePanelHandler, setOpenProjectPanelHandler,
        setOpenFolderPanelHandler)
import IDE.Web.PreferencesRequest (requestShowPreferences)
import IDE.Web.SaveRequest (requestSaveActiveFile)
import IDE.Web.SnapRequest (requestUnsnapPane)
import IDE.Web.FindRequest (requestToggleFindbar)
import IDE.Web.ShortcutsRequest (requestShowShortcuts)
import IDE.Web.BrowserRequest (requestOpenBrowser)
import IDE.Web.KeymapRequest (requestKeymapCommand)
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
macOpenProject fp = withApp $ \app -> projectOpenPath (appWorkspace app) fp

-- | Called (via the glue) once 'c_newWindow' (or the restore path) has
-- created an NSWindow + WKWebView for 'wid': attach a fresh jsaddle context so a
-- new reflex network ('jsMain') renders leksah's UI into that webview.  The
-- 'WebWindow' for 'wid' was already seeded by 'mintWindowId'; 'jsMain' adopts it.
macAttachWindow :: Int -> Ptr () -> IO ()
macAttachWindow widInt pWebView = do
  phaseLog $ "boot: macAttachWindow wid=" <> show widInt
             <> " webview=" <> show pWebView
  getGlobalApp >>= \case
    Nothing  -> phaseLog "boot: macAttachWindow: no global App!"
    Just app -> do
      -- Remember the webview so the ghci teardown can invalidate its jsaddle
      -- context before the native side releases it (see 'installMacMenu').
      atomicModifyIORef' attachedWebViews (\ws -> (castPtr pWebView : ws, ()))
      -- Flags match the first window's (main/WKWebView.hs: newIDE False True):
      -- hide the web menubar, use the native title bar.
      jsaddleMainHTMLWithBaseURL indexHtml baseURL
        (jsMain False True (Just (WindowId widInt)) app)
        (WKWebView (castPtr pWebView))
      phaseLog $ "boot: macAttachWindow wid=" <> show widInt
                 <> " attach call returned"
  where
    -- Same port the first window's jsaddle server bound (see 'IDE.Web.Instance');
    -- a second instance on a different LEKSAH_PORT points its webviews at its own.
    baseURL = encodeUtf8 (T.pack ("http://127.0.0.1:" <> show leksahPort))

-- | Raw webview pointers handed to 'macAttachWindow' by this instance —
-- exactly the ones carrying leksah_new_window's extra retain.  Drained by
-- 'invalidateAttachedWebViews' at ghci teardown.  (Interpreted-module CAF:
-- a :reload gives the next instance a fresh, empty list — which is right,
-- each instance invalidates only its own webviews.)
{-# NOINLINE attachedWebViews #-}
attachedWebViews :: IORef [Ptr ()]
attachedWebViews = unsafePerformIO (newIORef [])

-- | Invalidate the jsaddle context of every webview this instance attached:
-- after this no jsaddle thread touches those pointers, so the native teardown
-- (leksah_close_all_windows) can safely release them — which lets each
-- webview's WebContent XPC renderer exit instead of leaking one per restart.
-- MUST run before 'closeAllWindows' dispatches its teardown block.
invalidateAttachedWebViews :: IO ()
invalidateAttachedWebViews = do
    ws <- atomicModifyIORef' attachedWebViews (\ws' -> ([], ws'))
    phaseLog $ "boot: invalidating " <> show (length ws)
               <> " jsaddle webview context(s) before teardown"
    mapM_ (jsaddleWebViewInvalidate . WKWebView . castPtr) ws

-- | A window became key (frontmost): record it as the active window, so the
-- process-wide bridges (close/save/find/…) and the flipper's in-place actions
-- target it.
macWindowActivated :: Int -> IO ()
macWindowActivated widInt = withApp $ \app ->
  modifyCell (appUi app) (activeWindow ?~ WindowId widInt)

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
    -- View ▸ New Browser Pane opens a reflex browser pane; bridge it too.
    (CommandOpenBrowser:_)   -> requestOpenBrowser
    (cmd:_) -> case cmd ^. commandAction of
      -- No AppAction: the command is handled inside the reflex network by
      -- matching the keymap event stream (flipper, next/previous error,
      -- focus-alert, …) — inject it there via the bridge.
      Nothing  -> requestKeymapCommand cmd
      Just act -> withApp act
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
      getGlobalApp >>= \case
        Just app -> (\on -> if on then 1 else 0) . f
                        <$> currentConfig (appConfig app)
        Nothing   -> return (-1)
    _ -> return (-1)

-- | Push a 'ClaudeStatus' into the menu-bar status item: the aggregate state
-- (which shape\/colour the icon draws), how many sessions are in that state (drawn
-- beside the icon when it isn't green), the summary line for its tooltip, and one
-- menu row per session as @state \\t title \\t tooltip \\t session id@ — tooltip
-- newlines escaped as @\\n@, since a raw one would end the row.
--
-- Called only when the status CHANGES (see 'registerClaudeStatusPush'), so the
-- native menu is rebuilt on real state changes rather than 20 times a minute.
pushClaudeStatusToMenuBar :: ClaudeStatus -> IO ()
pushClaudeStatusToMenuBar st =
    withCString (T.unpack (csState st)) $ \s ->
      withCString (T.unpack (csSummary st)) $ \t ->
        withCString (T.unpack (T.unlines (map row (csRows st))))
                    (c_setClaudeStatus s (fromIntegral (csCount st)) t)
  where
    -- The directory tells two sessions of one project apart (worktrees are named
    -- for their task), so it rides along in the row's title.
    row r = T.intercalate "\t"
      [ csrState r
      , ellipsize 60 (csrTitle r)
          <> (if T.null (csrDir r) then "" else "  ·  " <> baseName (csrDir r))
      , T.intercalate "\\n" [ csrDir r, csrDetail r, csrSession r ]
      , csrSession r
      ]
    ellipsize k t | T.length t > k = T.take (k - 1) t <> "…"
                  | otherwise      = t
    baseName = T.takeWhileEnd (/= '/') . T.dropWhileEnd (== '/')

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
      -- A session chosen from the menu-bar status item: show its terminal.  Off
      -- the main thread — it runs tmux/ps, and Cocoa is waiting for the menu
      -- action to return.
    , cbClaudeActivate  = \sid -> void . forkIO . void $ showLiveSession (T.pack sid)
    }
  when ghciMode $ do
    -- Returning to the ghci prompt = stopping the Cocoa run loop; and Cocoa
    -- must not terminate the process (the ghci session!) on last-window-close.
    setGhciStop stopApp
    disableAutoTerminate
    -- Full teardown (stopForGhci) closes the windows so the old reflex
    -- networks die before :reload'd code starts a fresh :main.  Registered
    -- first = run last (LIFO), after the listeners are gone.  Invalidate the
    -- attached webviews' jsaddle contexts FIRST (synchronously, before
    -- closeAllWindows dispatches its async teardown block): only then may the
    -- native side balance leksah_new_window's extra retain and let each old
    -- webview — and its WebContent renderer process — actually die.
    -- Invalidating the jsaddle contexts is a QUIESCE, not a cleanup: it must
    -- happen before any thread is reaped, or the still-live pages keep
    -- delivering results and jsaddle keeps forking threads for them (each of
    -- which then escapes the reap — see 'IDE.Web.GhciMode.quiescesRef').
    registerGhciQuiesceNamed "jsaddle-webview-contexts" invalidateAttachedWebViews
    registerGhciCleanupNamed "native-windows" closeAllWindows
  -- Browser panes get REAL per-pane native WKWebViews on this front end (an
  -- iframe can't show X-Frame-Options sites); the widget drives them through
  -- these ops, and the JS rect reporter (browserNativeReporterJs) does the
  -- geometry/lifecycle directly against the native glue.
  setNativeBrowserOps NativeBrowserOps
    { nbLoad    = \bid url -> withCString (T.unpack url) (c_browserLoad (fromIntegral bid))
    , nbBack    = c_browserBack . fromIntegral
    , nbForward = c_browserForward . fromIntegral
    , nbReload  = c_browserReload . fromIntegral
    }
  -- Recent files are shown in the native "Open Recent" submenu.
  setRecentFilesHandler $ \fps -> withCString (intercalate "\n" fps) c_setRecentFiles
  -- The menu-bar status item follows the live Claude sessions (the shared poll in
  -- IDE.Web.ClaudeStatus, which the in-page traffic light reads too — one poll,
  -- and the two surfaces can't disagree).  The handler fires once on registration
  -- with the current status, so the item starts in sync.
  registerClaudeStatusPush pushClaudeStatusToMenuBar
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
  setNewWindowHandler . withApp $ \app -> do
      WindowId n <- mintWindowId app
      c_newWindow (fromIntegral n)
  -- Restore: create a native window for an already-seeded window id (no mint).
  setOpenWindowHandler (c_newWindow . fromIntegral)
  -- Global flipper: bring another window to the front on cross-window select.
  setRaiseWindowHandler (c_raiseWindow . fromIntegral)
  -- Global flipper, live preview: walk the highlighted entry's window to the
  -- top as you step, WITHOUT moving the keyboard off the flipping window.
  setOrderWindowFrontHandler (c_orderWindowFront . fromIntegral)
  -- The never-empty-window rule: a window whose last pane just closed has
  -- nothing to show, so leksah closes it for you (unless it is the only one
  -- left, which gets a Welcome pane instead).
  setCloseWindowHandler (c_closeWindow . fromIntegral)
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
  -- Build the menu bar from the LIVE keybindings table, and rebuild it when
  -- the table reloads (keybindings.json edit / edit.reloadKeybindings) so a
  -- user rebind shows up as the item's key equivalent.  The listener fires
  -- immediately with the current table — this is the initial build.
  registerKeymapListener $ \km -> buildMacMenuBar (renderedMenus km)

-- | (Re)build and install the menu bar from a rendered menu model.
buildMacMenuBar :: [(Text, [MenuItem])] -> IO ()
buildMacMenuBar rendered = do
  -- Settings… is added natively to the app menu, so drop it from the shared
  -- model here (both the tag table and the item build use this filtered list).
  let macMenus = stripPreferences rendered
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
