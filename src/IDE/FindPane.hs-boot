{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.FindPane
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The pane of ide for searching in a text buffer
--
-------------------------------------------------------------------------------

module IDE.FindPane (
    IDEFind(..)
,   FindAction(..)
,   FindState(..)
,   FindView(..)
) where

import Graphics.UI.Gtk
import IDE.Core.State

-------------------------------------------------------------------------------
--
-- * Interface
--

class IDEPaneC alpha => FindView alpha where
    getFind             ::   IDEM alpha
    getFindEntry        ::   alpha -> Entry
    getCaseSensitive    ::   alpha -> ToggleButton
    getWrapAround       ::   alpha -> ToggleButton
    getEntireWord       ::   alpha -> ToggleButton
    getGotoLineSpin     ::   alpha -> SpinButton

class FindAction alpha where
    doFind              ::   alpha

instance FindAction IDEAction
instance IDEObject IDEFind
instance IDEPaneC IDEFind

instance FindView IDEFind
instance CastablePane IDEFind
instance Recoverable FindState
instance Pane IDEFind
instance RecoverablePane IDEFind FindState
