{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Framework.EditorBasics
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Module for the basiscs of composing GUIs from editors
--
-----------------------------------------------------------------------------------

module IDE.Framework.EditorBasics (
-- * Types
    Getter
,   Setter
,   Injector
,   Extractor
,   Editor
--,   Applicator
,   Handler
,   Notifier
,   EventSelector(..)
) where

import Graphics.UI.Gtk --hiding (Event)
import Data.Unique

import Graphics.UI.Gtk --hiding (Event)

import IDE.Framework.Parameters

-- ---------------------------------------------------------------------
-- * Basic Types
--

--
-- | A type for getting a field of a record
--
type Getter alpha beta     =   alpha -> beta
--
-- | A type for setting the field of a record
--
type Setter alpha beta     =   beta -> alpha -> alpha

--
-- | A type for injecting a value into an editor
--
type Injector beta     =   beta -> IO()
--
-- | A type for extracting a value from an editor
--
type Extractor beta    =   IO(Maybe (beta))

--type Applicator beta =   beta -> IDEAction ()

--class EventSource alpha where

--class Event alpha where
--    event :: a -> Event

--
-- | A type for handling an IO event
-- Returning True: The event has been handles
-- Returning False: Handling should proceed
type Handler        =   Event  -> IO Bool

data EventSelector  =   Clicked
                    |   FocusOut
                    |   FocusIn
                    |   SelectionChanged
                    |   ButtonRelease
    deriving (Eq,Ord,Show)

--
-- | A type to register or unregister a handler
-- If the second argument is Left Handler the handler gets registered
-- If the second argument is Right Unique the handler will be removed
-- The returned unique value must be used for unregistering an event
type Notifier       =   EventSelector -> Either Handler Unique -> IO (Unique)

--
-- | A type to describe an editor.
-- alpha is the type of the individual field of the record
type Editor alpha       =   Parameters -> IO(Widget, Injector alpha , Extractor alpha , Notifier)


