{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Ghf.Core.Panes
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The core state of ghf. This module is imported from every other module,
-- | and all data structures of the state are declared here, to avoid circular
-- | module dependencies.
--
-------------------------------------------------------------------------------

module Ghf.Core.Panes (
-- * Panes and pane layout
    Pane(..)
,   Castable(..)
,   Casting(..)
,   GhfPane(..)
,   PaneDirection(..)
,   PanePath
,   PaneLayout(..)
,   PaneName
,   Connections(..)

-- * The pane types
,   GhfBuffer(..)
,   GhfLog(..)
,   GhfInfo(..)
,   GhfModules(..)

) where

import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.ModelView as New
import System.Glib.Signals
import Data.Maybe
import System.Time

import Ghf.Core.Data

-- ---------------------------------------------------------------------
-- Panes and pane layout
--

--
-- | A path to a pane
--
type PanePath       =   [PaneDirection]

--
-- | The relative direction to a pane from the parent
--
data PaneDirection  =   TopP | BottomP | LeftP | RightP
    deriving (Eq,Show,Read)
  	
--
-- | Description of a window layout
--
data PaneLayout =       HorizontalP PaneLayout PaneLayout Int
                    |   VerticalP PaneLayout PaneLayout Int
                    |   TerminalP (Maybe PaneDirection)
    deriving (Eq,Show,Read)



--
-- | Signal handlers for the different pane types
--
data Connections =  BufConnections
                        [ConnectId SourceView]
                        [ConnectId TextBuffer]
                        [ConnectId TextView]
--                        [ConnectId New.TreeView]


type PaneName = String

--
-- | Description of the different pane types
--

class Pane alpha  where
    paneName        ::   alpha -> PaneName
    paneName b      =   if getAddedIndex b == 0
                            then primPaneName b
                            else primPaneName b ++ "(" ++ show (getAddedIndex b) ++ ")"
    primPaneName    ::   alpha -> String
    getAddedIndex   ::   alpha -> Int
    getAddedIndex _ =   0
    getTopWidget    ::   alpha -> Widget
    paneId          ::   alpha -> String

data GhfPane        =   forall alpha . (Pane alpha, Castable alpha) => PaneC alpha

data Casting alpha  where
    LogCasting      ::   Casting GhfLog
    InfoCasting     ::   Casting GhfInfo
    BufferCasting   ::   Casting GhfBuffer
    ModulesCasting  ::   Casting GhfModules

class Castable alpha where
    casting         ::   alpha -> Casting alpha
    downCast        ::   Casting alpha -> GhfPane -> Maybe alpha
    isIt            ::   Casting alpha -> GhfPane -> Bool
    isIt t i        =   isJust (downCast t i)

instance Pane GhfPane where
    paneName (PaneC a)      =   paneName a
    primPaneName (PaneC a)  =   primPaneName a
    getAddedIndex (PaneC a) =   getAddedIndex a
    getTopWidget (PaneC a)  =   getTopWidget a
    paneId (PaneC a)        =   paneId a

-- ---------------------------------------------------------------------
-- Panes - The data structures for the panes
--

--
-- | A text editor pane description
--
data GhfBuffer  =   GhfBuffer {
    fileName    ::  Maybe FilePath
,   bufferName  ::  String
,   addedIndex  ::  Int
,   sourceView  ::  SourceView
,   scrolledWindow :: ScrolledWindow
,   modTime     ::  Maybe (ClockTime)
}

--
-- | A log view pane description
--
data GhfLog  =   GhfLog {
    textView  ::  TextView
,   scrolledWindowL :: ScrolledWindow
}

--
-- | An info pane description
--
data GhfInfo  =   GhfInfo {
    box             ::   HBox
,   injectors       ::   [IdentifierDescr -> IO() ]
}

-- | A modules pane description
--

data GhfModules     =   GhfModules {
    boxM            ::   HPaned
,   treeStore       ::   New.TreeStore (String, [(ModuleDescr,PackageDescr)])
,   facetStore      ::   New.ListStore (String, IdentifierDescr)
}
