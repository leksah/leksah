-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.GtkBindings
-- Copyright   :  2007-2014 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Utils.GtkBindings (
    treeViewSetActiveOnSingleClick
) where

import Foreign.Ptr (Ptr)
import Graphics.UI.Gtk (toToolbar, TreeViewClass, TreeView)
import Foreign.ForeignPtr (withForeignPtr)
import Graphics.UI.GtkInternals (toTreeView, unTreeView)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Utils (fromBool)

foreign import ccall safe "gtk_tree_view_set_activate_on_single_click"
    gtk_tree_view_set_activate_on_single_click :: Ptr TreeView -> CInt -> IO ()

treeViewSetActiveOnSingleClick :: TreeViewClass self => self -> Bool -> IO ()
treeViewSetActiveOnSingleClick self singleClick =
  withForeignPtr (unTreeView $ toTreeView self) $
      \selfPtr -> gtk_tree_view_set_activate_on_single_click selfPtr (fromIntegral $ fromBool singleClick)
