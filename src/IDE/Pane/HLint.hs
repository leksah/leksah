{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.HLint
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability  :  portable
--
-- | Deprecated.  Kept in to make old session (.lkshs) files work.
--
-----------------------------------------------------------------------------

module IDE.Pane.HLint (
   HLintState(..)
) where

import Graphics.UI.Frame.Panes
       (RecoverablePane(..), RecoverablePane, Pane(..))
import Graphics.UI.Gtk.Display.Label (labelNew, Label)
import Data.Typeable (Typeable)
import IDE.Core.Types (IDEM)
import Graphics.UI.Gtk.Abstract.Widget (castToWidget)
import Graphics.UI.Frame.ViewFrame (getNotebook)
import IDE.Core.State (reifyIDE)
import Data.Text (Text)

data IDEHLint       =   IDEHLint {
    deprecatedLabel :: Label
} deriving Typeable

data HLintState      =   HLintState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEHLint IDEM
    where
    primPaneName _  =   "HLint"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . deprecatedLabel
    paneId b        =   "*HLint"

instance RecoverablePane IDEHLint HLintState IDEM where
    saveState p     =   return (Just HLintState)
    recoverState pp HLintState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder pp nb windows = reifyIDE $ \ ideR -> do
        deprecatedLabel <- labelNew $ Just ("HLint has moved to the Errors pane and Package -> HLint menu item"::Text)
        return (Just IDEHLint {..}, [])
