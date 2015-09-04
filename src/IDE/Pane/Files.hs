{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--f
-- Module      :  IDE.Pane.Files
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability  :  portable
--
-- | The pane of ide that shows a list of all the files in the workspace
--
-------------------------------------------------------------------------------

module IDE.Pane.Files (
    IDEFiles(..)
,   FilesState(..)
) where

import Prelude hiding (catch)
import Graphics.UI.Gtk
import Data.Maybe (fromMaybe, maybeToList, listToMaybe, isJust)
import Control.Monad (forM, void, forM_, when)
import Data.Typeable (Typeable)
import IDE.Core.State
       (catchIDE, window, getIDE, MessageLevel(..), ipdPackageId,
        wsPackages, workspace, readIDE, IDEAction, ideMessage, reflectIDE,
        reifyIDE, IDEM, IDEPackage, ipdSandboxSources)
import IDE.Pane.SourceBuffer (fileNew, goToSourceDefinition')
import IDE.Sandbox
import Control.Applicative ((<$>))
import System.FilePath ((</>), takeFileName, dropFileName)
import Distribution.Package (PackageIdentifier(..))
import System.Directory
       (removeDirectoryRecursive, removeDirectory, createDirectory,
        doesFileExist, removeFile, doesDirectoryExist,
        getDirectoryContents, getPermissions, readable)
import IDE.Core.CTypes
       (Location(..), packageIdentifierToString)
import Graphics.UI.Frame.Panes
       (RecoverablePane(..), PanePath, RecoverablePane, Pane(..))
import Graphics.UI.Frame.ViewFrame (getMainWindow, getNotebook)
import Graphics.UI.Editor.Basics (Connection(..))
import Graphics.UI.Gtk.General.Enums
       (ShadowType(..), PolicyType(..), SelectionMode(..),
        TreeViewColumnSizing(..))
import System.Glib.Attributes (set, AttrOp(..))
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils
       (showErrorDialog, showInputDialog, treeViewContextMenu', __,
        showDialogOptions)
import Control.Exception (SomeException(..), catch)
import Data.Text (Text)
import qualified Data.Text as T
       (isPrefixOf, words, isSuffixOf, unpack, pack)
import Data.Monoid ((<>))
import IDE.Core.Types
       (ipdLib, WorkspaceAction, Workspace(..), wsAllPackages, WorkspaceM,
        runPackage, runWorkspace, PackageAction, PackageM, IDEPackage(..),
        IDE(..), Prefs(..), MonadIDE(..), ipdPackageDir)
import System.Glib.Properties (newAttrFromMaybeStringProperty)
import System.FilePath
       (addTrailingPathSeparator, takeDirectory, takeExtension,
       makeRelative, splitDirectories)
import Control.Monad.Reader.Class (MonadReader(..))
import IDE.Workspaces
       (makePackage, workspaceAddPackage', workspaceRemovePackage,
        workspaceActivatePackage, workspaceTry, workspaceTryQuiet,
        packageTry)
import Data.List
       (isSuffixOf, find, stripPrefix, isPrefixOf, sortBy, sort)
import Data.Ord (comparing)
import Data.Char (toUpper, toLower)
import System.Log.Logger (debugM)
import Data.Tree (Forest, Tree(..))
import Graphics.UI.Gtk.MenuComboToolbar.MenuItem
       (menuItemActivate, menuItemNewWithLabel)
import IDE.Pane.Modules (addModule)
import Graphics.UI.Gtk.Windows.MessageDialog
       (ButtonsType(..), MessageType(..), messageDialogNew)
import Graphics.UI.Gtk.ModelView.CellRenderer
       (CellRendererMode(..), cellMode)
import IDE.Pane.PackageEditor (packageEditText)
import IDE.Utils.GtkBindings (treeViewSetActiveOnSingleClick)
import IDE.Package (packageTest, packageRun, packageClean)


-- * The Files pane

-- | The representation of the Files pane
data IDEFiles        =   IDEFiles {
    deprecatedLabel :: Label
} deriving Typeable


-- | The additional state used when recovering the pane
--   (none, the package directories come from the IDE state)
data FilesState      =   FilesState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEFiles IDEM where
    primPaneName _  =   __ "Files"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . deprecatedLabel
    paneId b        =   "*Files"

instance RecoverablePane IDEFiles FilesState IDEM where
    saveState p     =   return (Just FilesState)
    recoverState pp FilesState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder pp nb windows = reifyIDE $ \ ideR -> do
        deprecatedLabel <- labelNew $ Just ("The Files pane is deprecated and has been combined with the Workspace pane"::Text)
        return (Just IDEFiles {..}, [])
