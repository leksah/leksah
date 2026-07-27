{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The native GTK4 menubar for leksah-webkitgtk.
--
-- Builds a GMenu from the shared 'IDE.Web.MenuModel.menus' (the same model the
-- web menubar and the macOS menu use) and installs it with
-- @gtk_application_set_menubar@, so it renders in the window (or the desktop
-- shell, where the environment does that).  Instead of one named GAction per
-- command, three parameterized actions (@app.cmd@ / @app.termcmd@ /
-- @app.globalcmd@) carry an Int32 tag indexing the flattened command list —
-- the same tag scheme 'IDE.Web.MacMenu' uses.
module IDE.Web.GtkMenu
  ( installGtkMenu
  , postGUIAsync
  ) where

import Control.Exception (SomeException, try)
import Control.Lens ((^.))
import Control.Monad (void, when, forM_, foldM)

import Data.GI.Base.GVariant (fromGVariant, toGVariant)
import Data.Int (Int32)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeFileName)
import System.IO.Unsafe (unsafePerformIO)

import qualified GI.GLib as GLib
       (idleAdd, variantTypeNew, pattern PRIORITY_DEFAULT)
import qualified GI.Gio as Gio
       (Cancellable, Menu, SimpleAction, actionMapAddAction, applicationQuit,
        fileGetPath, menuAppendItem, menuAppendSection, menuAppendSubmenu,
        menuItemNew, menuItemSetActionAndTargetValue, menuNew, menuRemoveAll,
        onSimpleActionActivate, simpleActionNew, simpleActionSetEnabled)
import qualified GI.Gtk as Gtk
       (Application, ApplicationWindow, applicationSetAccelsForAction,
        applicationSetMenubar, applicationWindowSetShowMenubar, fileDialogNew,
        fileDialogOpen, fileDialogOpenFinish,
        fileDialogSelectFolder, fileDialogSelectFolderFinish)

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
import IDE.Web.AddRemoteRequest (requestAddRemoteProject)
import IDE.Web.PreferencesRequest (requestShowPreferences)
import IDE.Web.ShortcutsRequest (requestShowShortcuts)
import IDE.Web.RecentFiles (setRecentFilesHandler)
import IDE.Web.TerminalInput
       (setActiveTerminalNotifier, setSplitActiveNotifier)

-- | Run an action on the GTK main loop.  Bridge handlers are invoked from
-- reflex\/jsaddle\/CmdServer threads, which must not touch GTK directly.
postGUIAsync :: IO () -> IO ()
postGUIAsync action =
  void . GLib.idleAdd GLib.PRIORITY_DEFAULT $ action >> return False

-- | The Underlay submenu (pane transparency, window snapping) is macOS-only
-- window trickery; drop it from the GTK menu.  Filtering the same list that
-- feeds both the item build and 'commandsRef' keeps their tags aligned.
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
-- order the GMenu is built, so tags line up.
flattenCmds :: [MenuItem] -> [Command]
flattenCmds = concatMap $ \case
  MenuItem _ cmd        -> [cmd]
  MenuShortcut _ _ cmd  -> [cmd]
  MenuKey _ _ cmd       -> [cmd]
  MenuGlobalKey _ _ cmd -> [cmd]
  MenuSplitKey _ _ cmd  -> [cmd]
  MenuSep               -> []
  Submenu _ subs        -> flattenCmds subs

-- | Run the command a menu tag points at (same dispatch as MacMenu).
dispatchTag :: Gtk.ApplicationWindow -> Int -> IO ()
dispatchTag win tag = do
  cmds <- readIORef commandsRef
  case drop tag cmds of
    -- File ▸ Open / Open Project use the native GtkFileDialog.
    (CommandFileOpen:_)        -> openFilePanel win
    (CommandProjectOpen:_)     -> openProjectPanel win
    (CommandProjectOpenFolder:_) -> openFolderPanel win
    -- These act on reflex state; signal via the bridges.
    (CommandFileSave:_)        -> requestSaveActiveFile
    (CommandFind:_)            -> requestToggleFindbar
    (CommandProjectAddRemote:_) -> requestAddRemoteProject
    (CommandShowPreferences:_) -> requestShowPreferences
    (CommandShowShortcuts:_)   -> requestShowShortcuts
    (cmd:_) -> getGlobalIDERef >>= \case
      Just ideR -> case cmd ^. commandAction of
        Just act -> void $ reflectIDE act ideR
        Nothing  -> return ()
      Nothing -> return ()
    [] -> return ()

-- | Native GTK4 open-file dialog; the chosen path goes through the same
-- bridge the macOS NSOpenPanel uses.
openFilePanel :: Gtk.ApplicationWindow -> IO ()
openFilePanel win = postGUIAsync $ do
  d <- Gtk.fileDialogNew
  Gtk.fileDialogOpen d (Just win) (Nothing :: Maybe Gio.Cancellable) . Just $
    \_ res ->
      try (Gtk.fileDialogOpenFinish d res) >>= \case
        Right file -> Gio.fileGetPath file >>= mapM_ deliverOpenedFile
        Left (_ :: SomeException) -> return ()  -- dismissed

-- | Native open-project dialog; adds the chosen project to the workspace,
-- like MacMenu's leksah_open_project.
openProjectPanel :: Gtk.ApplicationWindow -> IO ()
openProjectPanel win = postGUIAsync $ do
  d <- Gtk.fileDialogNew
  Gtk.fileDialogOpen d (Just win) (Nothing :: Maybe Gio.Cancellable) . Just $
    \_ res ->
      try (Gtk.fileDialogOpenFinish d res) >>= \case
        Right file -> Gio.fileGetPath file >>= mapM_ addToWorkspace
        Left (_ :: SomeException) -> return ()

-- | Native open-folder dialog; adds the chosen directory to the workspace as a
-- plain-directory project ('projectOpenPath' turns a directory into a
-- package-less 'CustomTool').  The macOS sibling is leksah_show_open_folder_panel.
openFolderPanel :: Gtk.ApplicationWindow -> IO ()
openFolderPanel win = postGUIAsync $ do
  d <- Gtk.fileDialogNew
  Gtk.fileDialogSelectFolder d (Just win) (Nothing :: Maybe Gio.Cancellable) . Just $
    \_ res ->
      try (Gtk.fileDialogSelectFolderFinish d res) >>= \case
        Right file -> Gio.fileGetPath file >>= mapM_ addToWorkspace
        Left (_ :: SomeException) -> return ()

-- | Add a path (project file or directory) to the workspace; 'projectOpenPath'
-- decides which.  Shared by the open-project and open-folder dialogs.
addToWorkspace :: FilePath -> IO ()
addToWorkspace fp = getGlobalIDERef >>= \case
  Just ideR -> void $ reflectIDE (workspaceTry (projectOpenPath fp)) ideR
  Nothing   -> return ()

-- | Translate a key spec like @\"cmd+ctrl+s\"@ to a GTK accelerator string.
-- cmd is the primary modifier → Control on Linux; the specs' extra ctrl
-- becomes Super so combinations stay distinct.
keySpecToGtkAccel :: Text -> Text
keySpecToGtkAccel spec =
  let parts = T.splitOn "+" spec
      (mods, keys) = span (`elem` ["cmd", "super", "shift", "alt", "opt", "ctrl"]) parts
      modStr m = case m of
        "ctrl"  -> "<Super>"
        "alt"   -> "<Alt>"
        "opt"   -> "<Alt>"
        "shift" -> "<Shift>"
        _       -> "<Control>"   -- cmd/super
      keyStr k = case k of
        "Enter" -> "Return"
        "["     -> "bracketleft"
        "]"     -> "bracketright"
        "="     -> "equal"
        -- A trailing empty part means the key itself was '+'.
        ""      -> "plus"
        _       -> k
  in T.concat (map modStr mods <> map keyStr keys)

-- | Build the GMenu from the shared model and register the bridge handlers.
-- Called on the GTK main thread (from the application activate handler).
installGtkMenu :: Gtk.Application -> Gtk.ApplicationWindow -> IO ()
installGtkMenu app win = do
  let gtkMenus = stripMacOnly menus
  -- Tags index this list; it must be the leaf commands in the same
  -- depth-first order that the menu build emits them.
  writeIORef commandsRef (concatMap (flattenCmds . snd) gtkMenus)

  -- Four parameterized actions carry the tag (avoids one named action per
  -- command).  termcmd's enabled state mirrors the macOS terminal gating:
  -- terminal-pane commands grey out unless a terminal tab is on screen.
  -- splitcmd mirrors the macOS 'splittable' gate: the Split items are enabled
  -- on EITHER a terminal (to split) OR a convertible editor/git-log tab (⌘D
  -- converts it to a backing tmux pane, then splits).
  it <- GLib.variantTypeNew "i"
  let mkTagAction name = do
        act <- Gio.simpleActionNew name (Just it)
        _ <- Gio.onSimpleActionActivate act $ \mv -> forM_ mv $ \gv ->
          (fromGVariant gv :: IO (Maybe Int32)) >>=
            mapM_ (dispatchTag win . fromIntegral)
        Gio.actionMapAddAction app act
        return act
  _        <- mkTagAction "cmd"
  termAct  <- mkTagAction "termcmd"
  splitAct <- mkTagAction "splitcmd"
  _        <- mkTagAction "globalcmd"
  Gio.simpleActionSetEnabled termAct False
  Gio.simpleActionSetEnabled splitAct False
  -- The split gate is the OR of two independently-notified booleans, so track
  -- both and recompute on either change.
  termActiveRef  <- newIORef False
  splitActiveRef <- newIORef False
  let refreshSplit =
        (||) <$> readIORef termActiveRef <*> readIORef splitActiveRef
          >>= Gio.simpleActionSetEnabled splitAct
  setActiveTerminalNotifier $ \on -> postGUIAsync $ do
    writeIORef termActiveRef on
    Gio.simpleActionSetEnabled termAct on
    refreshSplit
  setSplitActiveNotifier $ \on -> postGUIAsync $ do
    writeIORef splitActiveRef on
    refreshSplit

  -- Open Recent (a section of app.recent items, rebuilt on every change).
  recentMenu <- Gio.menuNew
  recentAct <- Gio.simpleActionNew "recent" . Just =<< GLib.variantTypeNew "s"
  _ <- Gio.onSimpleActionActivate recentAct $ \mv -> forM_ mv $ \gv ->
    (fromGVariant gv :: IO (Maybe Text)) >>=
      mapM_ (deliverOpenedFile . T.unpack)
  Gio.actionMapAddAction app recentAct
  setRecentFilesHandler $ \fps -> postGUIAsync $ do
    Gio.menuRemoveAll recentMenu
    forM_ fps $ \fp -> do
      mi <- Gio.menuItemNew (Just . T.pack $ takeFileName fp) Nothing
      gv <- toGVariant (T.pack fp)
      Gio.menuItemSetActionAndTargetValue mi (Just "app.recent") (Just gv)
      Gio.menuAppendItem recentMenu mi

  -- Quit (GTK convention: File ▸ Quit, Ctrl+Q).
  quitAct <- Gio.simpleActionNew "quit" Nothing
  _ <- Gio.onSimpleActionActivate quitAct $ \_ -> Gio.applicationQuit app
  Gio.actionMapAddAction app quitAct
  Gtk.applicationSetAccelsForAction app "app.quit" ["<Control>q"]

  -- The toolbar/menubar Open commands show the native dialogs.
  setOpenFilePanelHandler (openFilePanel win)
  setOpenProjectPanelHandler (openProjectPanel win)
  setOpenFolderPanelHandler (openFolderPanel win)

  -- Build the menubar.  MenuSep splits a level into GMenu sections (GMenu has
  -- no separator primitive; section boundaries render as separators).
  menubar <- Gio.menuNew
  let -- Add one leaf item with the given action and tag.
      addLeaf section label action tag = do
        mi <- Gio.menuItemNew (Just label) Nothing
        gv <- toGVariant (fromIntegral tag :: Int32)
        Gio.menuItemSetActionAndTargetValue mi (Just action) (Just gv)
        Gio.menuAppendItem section mi
      -- Fill a menu with items, splitting at MenuSep into sections; returns
      -- the tag after the last leaf.  Submenus recurse.
      fillMenu :: Gio.Menu -> Int -> [MenuItem] -> IO Int
      fillMenu parent tag0 items0 = do
        section0 <- Gio.menuNew
        let flush section = Gio.menuAppendSection parent Nothing section
            go section tag [] = flush section >> return tag
            go section tag (MenuSep : rs) = do
              flush section
              section' <- Gio.menuNew
              go section' tag rs
            go section tag (MenuItem label _ : rs) = do
              addLeaf section label "app.cmd" tag
              go section (tag + 1) rs
            -- The shortcut hints (tmux chords) have no GMenu column; the
            -- label alone is the fallback.
            go section tag (MenuShortcut label _ _ : rs) = do
              addLeaf section label "app.cmd" tag
              go section (tag + 1) rs
            -- Deliberately no accelerator: the macOS specs are ⌘-based, and
            -- their Ctrl translations (Ctrl+D, Ctrl+[/]) would steal keys
            -- from the terminal these commands act on.  Menu clicks only.
            go section tag (MenuKey label _ _ : rs) = do
              addLeaf section label "app.termcmd" tag
              go section (tag + 1) rs
            -- Split items: enabled on a terminal OR a convertible editor/
            -- git-log tab (app.splitcmd's dual gate, above).  No accelerator
            -- for the same reason as MenuKey — Ctrl+D would steal the key from
            -- the terminal; menu clicks only.
            go section tag (MenuSplitKey label _ _ : rs) = do
              addLeaf section label "app.splitcmd" tag
              go section (tag + 1) rs
            go section tag (MenuGlobalKey label spec _ : rs) = do
              addLeaf section label "app.globalcmd" tag
              Gtk.applicationSetAccelsForAction app
                ("app.globalcmd(" <> T.pack (show tag) <> ")")
                [keySpecToGtkAccel spec]
              go section (tag + 1) rs
            go section tag (Submenu label subs : rs) = do
              sub <- Gio.menuNew
              tag' <- fillMenu sub tag subs
              Gio.menuAppendSubmenu section (Just label) sub
              go section tag' rs
        go section0 tag0 items0
      addTop tag (title, items) = do
        top <- Gio.menuNew
        tag' <- fillMenu top tag items
        -- Workspace (the app's File menu — title kept in sync with
        -- MenuModel.hs) gets the native Open Recent and Quit sections.
        when (title == ("Workspace" :: Text)) $ do
          recentSection <- Gio.menuNew
          Gio.menuAppendSubmenu recentSection (Just "Open Recent") recentMenu
          Gio.menuAppendSection top Nothing recentSection
          quitSection <- Gio.menuNew
          quitItem <- Gio.menuItemNew (Just "Quit") (Just "app.quit")
          Gio.menuAppendItem quitSection quitItem
          Gio.menuAppendSection top Nothing quitSection
        Gio.menuAppendSubmenu menubar (Just title) top
        return tag'
  _ <- foldM addTop (0 :: Int) gtkMenus
  Gtk.applicationSetMenubar app (Just menubar)
  Gtk.applicationWindowSetShowMenubar win True
