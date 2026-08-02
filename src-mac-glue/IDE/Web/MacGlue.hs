{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | The Haskell face of the macOS native glue (@main/leksah-mac-menu.m@).
--
-- The Objective-C calls back into Haskell through function pointers
-- registered with @leksah_set_haskell_callbacks@ (from 'setMacCallbacks',
-- run at startup by @IDE.Web.MacMenu.installMacMenu@), NOT through foreign
-- exports: under @leksah.sh --ghci@ the ObjC must be loaded by dyld (GHCi's
-- RTS linker can load ObjC objects but never registers their classes with
-- the ObjC runtime), and a dylib cannot name RTS-linker-loaded exports —
-- while FunPtrs made with @foreign import ccall "wrapper"@ are callable from
-- both worlds.  In the ordinary compiled app the very same registration
-- happens against the statically linked copy of the ObjC.
--
-- In ghci mode the @objc-in-library@ cabal flag is switched off, so this
-- sublibrary's archive carries no ObjC and the @foreign import@s below
-- resolve against @libleksah-mac-menu.dylib@ (compiled and preloaded by
-- @leksah.sh --ghci@).
--
-- Off macOS everything compiles to no-op stubs (same pattern as
-- @IDE.Web.ThreadPriority@) so the sublibrary builds on every platform.
module IDE.Web.MacGlue
  ( -- * Callbacks from Objective-C into leksah
    MacCallbacks(..)
  , setMacCallbacks
    -- * ghci-mode app lifecycle
  , stopApp
  , resumeApp
  , closeAllWindows
  , disableAutoTerminate
  , takeFirstLaunch
    -- * The native API of leksah-mac-menu.m
  , c_menuBegin
  , c_menuAddMenu
  , c_menuAddItem
  , c_menuAddItemKV
  , c_menuAddItemKey
  , c_menuAddItemKeyGlobal
  , c_menuAddItemKeySplittable
  , c_menuAddSeparator
  , c_menuPushSubmenu
  , c_menuPopSubmenu
  , c_menuInstall
  , c_setTerminalActive
  , c_setSplitActive
  , c_titlebarSetup
  , c_newWindow
  , c_raiseWindow
  , c_showOpenPanel
  , c_showOpenProjectPanel
  , c_showOpenFolderPanel
  , c_setRecentFiles
  , c_setClaudeStatus
  , c_screenshot
  , c_snapshotRect
  , c_pickColor
    -- * Native browser panes (real per-pane WKWebViews)
  , c_browserLoad
  , c_browserBack
  , c_browserForward
  , c_browserReload
  ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
#ifdef darwin_HOST_OS
import Foreign.C.String (peekCString)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (FunPtr, castPtrToFunPtr, freeHaskellFunPtr)
#else
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO)
#endif

-- | 'True' the first time it is called in a process, 'False' thereafter, so
-- exe:leksah's @main@ can tell a first launch from a ghci reload re-run — and
-- route window 0 through the app-launch path once, but the runtime new-window
-- path on every reload (the app-launch path's jsaddle bridge is one-shot and
-- dead on a 2nd @:main@).
--
-- On macOS the flag is a C static in the PRELOADED leksah-mac-menu dylib
-- (@leksah_take_first_launch@).  That dylib is loaded once and never reloaded,
-- so the flag genuinely survives @:reload@.  A Haskell CAF here does NOT: this
-- sublibrary can't be interpreted (it has foreign exports), but it is still a
-- home-package OBJECT module, so @:reload@ hands it a fresh module instance and
-- resets the CAF to 'True' — which wrongly routed every reload back through the
-- first-launch path (its dead bridge → run loop alive but no working window).
-- Off macOS there is no reload story, so a plain IORef is fine.
takeFirstLaunch :: IO Bool
#ifdef darwin_HOST_OS
takeFirstLaunch = (/= 0) <$> c_takeFirstLaunch
#else
takeFirstLaunch = atomicModifyIORef' firstLaunchRef (\f -> (False, f))

{-# NOINLINE firstLaunchRef #-}
firstLaunchRef :: IORef Bool
firstLaunchRef = unsafePerformIO (newIORef True)
#endif

-- | Everything Objective-C calls back into leksah with.  Registered once at
-- startup with 'setMacCallbacks'; the native side null-guards every pointer,
-- so a callback that races registration is dropped rather than crashing.
data MacCallbacks = MacCallbacks
  { cbMenuAction      :: Int -> IO ()          -- ^ menu item chosen (tag)
  , cbOpenFile        :: FilePath -> IO ()     -- ^ NSOpenPanel file chosen
  , cbOpenProject     :: FilePath -> IO ()     -- ^ open-project panel chosen
  , cbUnsnap          :: String -> IO ()       -- ^ Underlay ▸ Unsnap <key>
  , cbOpenSettings    :: IO ()                 -- ^ app menu ▸ Settings…
  , cbAttachWindow    :: Int -> Ptr () -> IO () -- ^ new NSWindow + WKWebView ready
  , cbWindowActivated :: Int -> IO ()          -- ^ window became key
  , cbWindowClosing   :: Int -> IO ()          -- ^ window closing
  , cbColorPicked     :: String -> IO ()       -- ^ NSColorPanel change ("#rrggbb")
  , cbToggleState     :: Int -> IO Int         -- ^ a menu item's toggle state by
                                               --   tag: -1 not a toggle, 0 off, 1 on
  , cbClaudeActivate  :: String -> IO ()       -- ^ a live Claude session chosen
                                               --   from the menu-bar status
                                               --   item's menu (session id)
  }

#ifdef darwin_HOST_OS

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
-- A Split item: enabled while a terminal OR a convertible (editor/git-log)
-- tab is active — see leksah_set_split_active.
foreign import ccall "leksah_menu_add_item_key_splittable" c_menuAddItemKeySplittable :: CString -> CString -> CInt -> IO ()
foreign import ccall "leksah_menu_add_separator" c_menuAddSeparator :: IO ()
foreign import ccall "leksah_menu_push_submenu" c_menuPushSubmenu :: CString -> IO ()
foreign import ccall "leksah_menu_pop_submenu"  c_menuPopSubmenu  :: IO ()
foreign import ccall "leksah_menu_install"  c_menuInstall :: IO ()
-- Whether a terminal tab is on screen: gates the Terminal menu's key
-- equivalents so ⌘D etc. pass through to the editor otherwise.
foreign import ccall "leksah_set_terminal_active" c_setTerminalActive :: CInt -> IO ()
-- Whether the active tab, though not a terminal, can convert to a tmux pane
-- (an editor/git-log tab with a backing pane): enables the Split items.
foreign import ccall "leksah_set_split_active" c_setSplitActive :: CInt -> IO ()
foreign import ccall "leksah_titlebar_setup" c_titlebarSetup :: IO ()
-- Create a native NSWindow + WKWebView for a freshly-minted 'WindowId'; the ObjC
-- glue calls back 'cbAttachWindow' once the webview exists so Haskell can
-- attach a jsaddle context (a second reflex network) to it.
foreign import ccall "leksah_new_window" c_newWindow :: CInt -> IO ()
-- Bring a specific window to the front (the global flipper's cross-window raise).
foreign import ccall "leksah_raise_window" c_raiseWindow :: CInt -> IO ()
-- Show the native "Open File" panel (NSOpenPanel); it calls back 'cbOpenFile'.
foreign import ccall "leksah_show_open_panel" c_showOpenPanel :: IO ()
-- Show the native "Open Project" panel; it calls back 'cbOpenProject'.
foreign import ccall "leksah_show_open_project_panel" c_showOpenProjectPanel :: IO ()
-- Show the native "Open Folder" panel; it also calls back 'cbOpenProject' (the
-- handler adds a directory as a plain-directory project).
foreign import ccall "leksah_show_open_folder_panel" c_showOpenFolderPanel :: IO ()
-- Populate the native "Open Recent" submenu (newline-separated paths).
foreign import ccall "leksah_set_recent_files" c_setRecentFiles :: CString -> IO ()
-- The menu-bar status item: the live Claude sessions' aggregate state
-- ("waiting"/"busy"/"idle"/"none"), a one-line tooltip summary, and one row per
-- session (@state \\t title \\t tooltip \\t session id@, newline-separated).
foreign import ccall "leksah_set_claude_status" c_setClaudeStatus
  :: CString -> CString -> CString -> IO ()
-- Snapshot the WKWebView content to a PNG at the given path; returns 1 on success.
foreign import ccall "leksah_screenshot" c_screenshot :: CString -> IO CInt
-- Snapshot just a rectangle (x,y,w,h in CSS px) of the WKWebView content.
foreign import ccall "leksah_snapshot_rect" c_snapshotRect
  :: CString -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "leksah_pick_color" c_pickColor :: CString -> IO ()
-- Native browser panes: drive pane <bid>'s overlaid WKWebView (created by the
-- JS rect reporter's first snapshot — a load before creation is kept pending).
foreign import ccall "leksah_browser_load"    c_browserLoad    :: CInt -> CString -> IO ()
foreign import ccall "leksah_browser_back"    c_browserBack    :: CInt -> IO ()
foreign import ccall "leksah_browser_forward" c_browserForward :: CInt -> IO ()
foreign import ccall "leksah_browser_reload"  c_browserReload  :: CInt -> IO ()
-- ghci-mode lifecycle (see the matching definitions in leksah-mac-menu.m).
foreign import ccall "leksah_take_first_launch" c_takeFirstLaunch :: IO CInt
foreign import ccall "leksah_stop_app" c_stopApp :: IO ()
foreign import ccall "leksah_run_app" c_runApp :: IO ()
foreign import ccall "leksah_close_all_windows" c_closeAllWindows :: IO ()
foreign import ccall "leksah_disable_auto_terminate" c_disableAutoTerminate :: IO ()

-- FunPtr factories for the callback shapes the ObjC dispatches through, and
-- the registration entry point in leksah-mac-menu.m.
foreign import ccall "wrapper" mkIntCb
  :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))
foreign import ccall "wrapper" mkStringCb
  :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))
foreign import ccall "wrapper" mkUnitCb
  :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "wrapper" mkIntPtrCb
  :: (CInt -> Ptr () -> IO ()) -> IO (FunPtr (CInt -> Ptr () -> IO ()))
foreign import ccall "wrapper" mkIntRetCb
  :: (CInt -> IO CInt) -> IO (FunPtr (CInt -> IO CInt))
-- Registered separately from the main callbacks (additive, so the 9-arg
-- leksah_set_haskell_callbacks keeps its ABI): reports a menu item's live
-- toggle state to validateMenuItem.
foreign import ccall "leksah_set_toggle_state_callback" c_setToggleStateCallback
  :: FunPtr (CInt -> IO CInt) -> IO ()
-- Also additive: shows the live Claude session chosen from the menu-bar status
-- item's menu.
foreign import ccall "leksah_set_claude_activate_callback" c_setClaudeActivateCallback
  :: FunPtr (CString -> IO ()) -> IO ()
foreign import ccall "leksah_set_haskell_callbacks" c_setHaskellCallbacks
  :: FunPtr (CInt -> IO ())           -- menu_action
  -> FunPtr (CString -> IO ())        -- open_file
  -> FunPtr (CString -> IO ())        -- open_project
  -> FunPtr (CString -> IO ())        -- unsnap
  -> FunPtr (IO ())                   -- open_settings
  -> FunPtr (CInt -> Ptr () -> IO ()) -- attach_window
  -> FunPtr (CInt -> IO ())           -- window_activated
  -> FunPtr (CInt -> IO ())           -- window_closing
  -> FunPtr (CString -> IO ())        -- color_picked
  -> IO ()
-- Takes (and clears) the snapshot leksah_set_haskell_callbacks made of the
-- previously installed FunPtrs, for 'setMacCallbacks' to free.  Returns how
-- many pointers were written into the buffer.
foreign import ccall "leksah_take_previous_callbacks" c_takePreviousCallbacks
  :: Ptr (Ptr ()) -> CInt -> IO CInt

-- | Size of the FunPtr set 'setMacCallbacks' installs — the buffer bound for
-- 'c_takePreviousCallbacks' (LEKSAH_N_CALLBACKS in leksah-mac-menu.m).
nMacCallbacks :: Int
nMacCallbacks = 11

-- | Register the callbacks with the native side.  Call before anything can
-- trigger a native callback (in practice: at the top of @installMacMenu@,
-- from @main@).  Marshalling to Haskell types happens here so the handlers in
-- MacMenu.hs stay plain Haskell.
--
-- Each @:main@ builds a fresh set of FunPtrs, and every FunPtr PINS its
-- closure — which captures this instance's @IDERef@, so the whole IDE state
-- and reflex network with it.  Left unfreed (as they were), a ghci session grew
-- by an entire instance per reload.  So the PREVIOUS set is freed here, from
-- the snapshot the native side takes before the swap
-- (@leksah_take_previous_callbacks@).  Safe at this point: @:main@ sets up with
-- the run loop stopped, so no callback can be executing.
setMacCallbacks :: MacCallbacks -> IO ()
setMacCallbacks cb = do
  menuAction   <- mkIntCb $ \tag -> cbMenuAction cb (fromIntegral tag)
  openFile     <- mkStringCb $ \cs -> peekCString cs >>= cbOpenFile cb
  openProject  <- mkStringCb $ \cs -> peekCString cs >>= cbOpenProject cb
  unsnap       <- mkStringCb $ \cs -> peekCString cs >>= cbUnsnap cb
  openSettings <- mkUnitCb $ cbOpenSettings cb
  attachWindow <- mkIntPtrCb $ \wid p -> cbAttachWindow cb (fromIntegral wid) p
  activated    <- mkIntCb $ \wid -> cbWindowActivated cb (fromIntegral wid)
  closing      <- mkIntCb $ \wid -> cbWindowClosing cb (fromIntegral wid)
  colorPicked  <- mkStringCb $ \cs -> peekCString cs >>= cbColorPicked cb
  toggleState  <- mkIntRetCb $ \tag -> fromIntegral <$> cbToggleState cb (fromIntegral tag)
  claudeAct    <- mkStringCb $ \cs -> peekCString cs >>= cbClaudeActivate cb
  c_setHaskellCallbacks menuAction openFile openProject unsnap openSettings
                        attachWindow activated closing colorPicked
  c_setToggleStateCallback toggleState
  c_setClaudeActivateCallback claudeAct
  -- All three setters have run, so the snapshot the first of them took is the
  -- complete previous set and nothing native points at it any more.
  allocaArray nMacCallbacks $ \buf -> do
    n <- c_takePreviousCallbacks buf (fromIntegral nMacCallbacks)
    stale <- peekArray (fromIntegral n) buf
    mapM_ (\p -> freeHaskellFunPtr (castPtrToFunPtr p :: FunPtr ())) stale

-- | Make @[NSApp run]@ return (posts a stop + wake event on the main queue).
-- Windows and app state survive; 'resumeApp' re-enters the run loop.
stopApp :: IO ()
stopApp = c_stopApp

-- | Re-enter @[NSApp run]@.  Must be called on the process main thread (under
-- ghci that is the prompt's thread with @-fno-ghci-sandbox@); blocks until the
-- next 'stopApp'.
resumeApp :: IO ()
resumeApp = c_runApp

-- | Close every leksah window without running the per-window closing
-- callbacks (ghci-mode teardown before @:reload@ + a fresh @:main@).
closeAllWindows :: IO ()
closeAllWindows = c_closeAllWindows

-- | Stop Cocoa terminating the process when the last window closes (jsaddle's
-- AppDelegate answers YES to that) — in ghci mode terminating would kill the
-- ghci session itself; leksah manages quitting explicitly.
disableAutoTerminate :: IO ()
disableAutoTerminate = c_disableAutoTerminate

#else

-- Not macOS: no-op stubs with the same signatures.

setMacCallbacks :: MacCallbacks -> IO ()
setMacCallbacks _ = return ()

stopApp, resumeApp, closeAllWindows, disableAutoTerminate :: IO ()
stopApp = return ()
resumeApp = return ()
closeAllWindows = return ()
disableAutoTerminate = return ()

c_menuBegin :: IO ()
c_menuBegin = return ()
c_menuAddMenu :: CString -> IO ()
c_menuAddMenu _ = return ()
c_menuAddItem :: CString -> CInt -> IO ()
c_menuAddItem _ _ = return ()
c_menuAddItemKV :: CString -> CString -> CInt -> IO ()
c_menuAddItemKV _ _ _ = return ()
c_menuAddItemKey :: CString -> CString -> CInt -> IO ()
c_menuAddItemKey _ _ _ = return ()
c_menuAddItemKeyGlobal :: CString -> CString -> CInt -> IO ()
c_menuAddItemKeyGlobal _ _ _ = return ()
c_menuAddItemKeySplittable :: CString -> CString -> CInt -> IO ()
c_menuAddItemKeySplittable _ _ _ = return ()
c_menuAddSeparator :: IO ()
c_menuAddSeparator = return ()
c_menuPushSubmenu :: CString -> IO ()
c_menuPushSubmenu _ = return ()
c_menuPopSubmenu :: IO ()
c_menuPopSubmenu = return ()
c_menuInstall :: IO ()
c_menuInstall = return ()
c_setTerminalActive :: CInt -> IO ()
c_setTerminalActive _ = return ()
c_setSplitActive :: CInt -> IO ()
c_setSplitActive _ = return ()
c_titlebarSetup :: IO ()
c_titlebarSetup = return ()
c_newWindow :: CInt -> IO ()
c_newWindow _ = return ()
c_raiseWindow :: CInt -> IO ()
c_raiseWindow _ = return ()
c_showOpenPanel :: IO ()
c_showOpenPanel = return ()
c_showOpenProjectPanel :: IO ()
c_showOpenProjectPanel = return ()
c_showOpenFolderPanel :: IO ()
c_showOpenFolderPanel = return ()
c_setRecentFiles :: CString -> IO ()
c_setRecentFiles _ = return ()
c_setClaudeStatus :: CString -> CString -> CString -> IO ()
c_setClaudeStatus _ _ _ = return ()
c_screenshot :: CString -> IO CInt
c_screenshot _ = return 0
c_snapshotRect :: CString -> CInt -> CInt -> CInt -> CInt -> IO CInt
c_snapshotRect _ _ _ _ _ = return 0
c_pickColor :: CString -> IO ()
c_pickColor _ = return ()
c_browserLoad :: CInt -> CString -> IO ()
c_browserLoad _ _ = return ()
c_browserBack, c_browserForward, c_browserReload :: CInt -> IO ()
c_browserBack _ = return ()
c_browserForward _ = return ()
c_browserReload _ = return ()

#endif
