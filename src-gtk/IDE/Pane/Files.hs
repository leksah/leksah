{-# LANGUAGE DeriveGeneric #-}
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

import Prelude ()
import Prelude.Compat
import Data.Typeable (Typeable)
import IDE.Core.State (IDEM)
import Graphics.UI.Frame.Panes
       (RecoverablePane(..), RecoverablePane, Pane(..))
import Graphics.UI.Frame.ViewFrame (getNotebook)
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils (__)
import GI.Gtk.Objects.Label (labelNew, Label(..))
import GI.Gtk.Objects.Widget (toWidget)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- * The Files pane

-- | The representation of the Files pane
newtype IDEFiles = IDEFiles {
    deprecatedLabel :: Label
} deriving Typeable


-- | The additional state used when recovering the pane
--   (none, the package directories come from the IDE state)
data FilesState      =   FilesState
    deriving(Eq,Ord,Read,Show,Typeable,Generic)

instance ToJSON FilesState
instance FromJSON FilesState

instance Pane IDEFiles IDEM where
    primPaneName _  =   __ "Files"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . deprecatedLabel
    paneId _        =   "*Files"

instance RecoverablePane IDEFiles FilesState IDEM where
    saveState _     =   return (Just FilesState)
    recoverState pp FilesState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder _pp _nb _windows = liftIO $ do
        deprecatedLabel <- labelNew $ Just "The Files pane is deprecated and has been combined with the Workspace pane"
        return (Just IDEFiles {..}, [])
