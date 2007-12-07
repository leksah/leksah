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
,   CastablePane(..)
,   Casting(..)
,   GhfPane(..)
,   ModelPane(..)
,   PaneDirection(..)
,   PanePath
,   PaneLayout(..)
,   PaneName
,   Connections(..)

-- * The pane types
,   GhfBuffer(..)
,   BufferState(..)
,   GhfLog(..)
,   LogState(..)
,   GhfInfo(..)
,   InfoState(..)
,   GhfModules(..)
,   ModulesState(..)
,   GhfCallers(..)
,   CallersState(..)

,   GhfState(..)
,   PaneState(..)
,   Model(..)
,   CastableModel(..)
) where

import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.ModelView as New
import System.Glib.Signals
import Data.Maybe
import System.Time
import Control.Monad.Reader
import GHC.IOBase hiding (BufferState)
import Text.ParserCombinators.ReadP

import Ghf.Core.Types
import {-# SOURCE #-} Ghf.Core.State
import GUI.Ghf.EditorBasics
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
    makeActive      ::   alpha -> GhfAction
    close           ::   alpha -> GhfAction

class CastablePane alpha where
    casting         ::   alpha -> Casting alpha
    downCast        ::   Casting alpha -> GhfPane -> Maybe alpha
    isIt            ::   Casting alpha -> GhfPane -> Bool
    isIt t i        =   isJust (downCast t i)

class (Pane alpha, Model beta) => ModelPane alpha beta | beta -> alpha, alpha -> beta  where
    saveState               ::   alpha -> GhfM (Maybe GhfState)
    recoverState            ::   PanePath -> beta -> GhfAction

data GhfPane        =   forall alpha beta . (CastablePane alpha, ModelPane alpha beta) => PaneC alpha

instance Pane GhfPane where
    paneName (PaneC a)      =   paneName a
    primPaneName (PaneC a)  =   primPaneName a
    getAddedIndex (PaneC a) =   getAddedIndex a
    getTopWidget (PaneC a)  =   getTopWidget a
    paneId (PaneC a)        =   paneId a
    makeActive (PaneC a)    =   makeActive a
    close (PaneC a)         =   close a

class Model alpha where
    toPaneState      ::   alpha -> PaneState

class Model alpha =>   CastableModel alpha where
    castingS         ::   alpha -> CastingS alpha
    downCastS        ::   CastingS alpha -> GhfState -> Maybe alpha
    isItS            ::   CastingS alpha -> GhfState -> Bool
    isItS t i        =   isJust (downCastS t i)

data GhfState       =   forall alpha beta . (ModelPane alpha beta, CastableModel beta) => StateC beta

instance Model GhfState where
    toPaneState (StateC a)  =   toPaneState a

instance ModelPane GhfPane GhfState where
    saveState (PaneC p)             =   saveState p
    recoverState pp (StateC s)      =   recoverState pp s

-- ---------------------------------------------------------------------
-- Special Panes - The data structures for the panes
--

data Casting alpha  where
    LogCasting      ::   Casting GhfLog
    InfoCasting     ::   Casting GhfInfo
    BufferCasting   ::   Casting GhfBuffer
    ModulesCasting  ::   Casting GhfModules
    CallersCasting  ::   Casting GhfCallers

data CastingS alpha  where
    LogCastingS      ::   CastingS LogState
    InfoCastingS     ::   CastingS InfoState
    BufferCastingS   ::   CastingS BufferState
    ModulesCastingS  ::   CastingS ModulesState
    CallersCastingS  ::   CastingS CallersState

--
-- | A text editor pane description
--
data GhfBuffer      =   GhfBuffer {
    fileName        ::  Maybe FilePath
,   bufferName      ::  String
,   addedIndex      ::  Int
,   sourceView      ::  SourceView
,   scrolledWindow  ::  ScrolledWindow
,   modTime         ::  Maybe (ClockTime)
}

instance CastablePane GhfBuffer where
    casting _       =   BufferCasting
    downCast _ (PaneC a)
                    =   case casting a of
                            BufferCasting   -> Just a
                            _               -> Nothing

data BufferState            =   BufferState FilePath Int
    deriving(Eq,Ord,Read,Show)

instance Model BufferState where
    toPaneState a           =   BufferSt a

instance CastableModel BufferState where
    castingS _              =   BufferCastingS
    downCastS _ (StateC a)  =   case castingS a of
                                    BufferCastingS -> Just a
                                    _          -> Nothing

--
-- | A log view pane description
--
data GhfLog         =   GhfLog {
    textView        ::   TextView
,   scrolledWindowL ::   ScrolledWindow
}

instance CastablePane GhfLog where
    casting _               =   LogCasting
    downCast _ (PaneC a)    =   case casting a of
                                    LogCasting -> Just a
                                    _          -> Nothing

data LogState               =   LogState
    deriving(Eq,Ord,Read,Show)

instance Model LogState where
    toPaneState a           =   LogSt a

instance CastableModel LogState where
    castingS _               =   LogCastingS
    downCastS _ (StateC a)    =   case castingS a of
                                    LogCastingS -> Just a
                                    _          -> Nothing
--
-- | An info pane description
--
data GhfInfo        =   GhfInfo {
    sw              ::   ScrolledWindow
,   injectors       ::   [IdentifierDescr -> IO()]
,   extractors      ::   [IdentifierDescr -> Extractor IdentifierDescr]
}

instance CastablePane GhfInfo where
    casting _               =   InfoCasting
    downCast _ (PaneC a)    =   case casting a of
                                    InfoCasting -> Just a
                                    _           -> Nothing

data InfoState              =   InfoState IdentifierDescr
    deriving(Eq,Ord,Read,Show)

instance Model InfoState where
    toPaneState a           =   InfoSt a

instance CastableModel InfoState where
    castingS _               =   InfoCastingS
    downCastS _ (StateC a)    =   case castingS a of
                                    InfoCastingS -> Just a
                                    _           -> Nothing

-- | A modules pane description
--

data GhfModules     =   GhfModules {
    paned           ::   HPaned
,   treeView        ::   New.TreeView
,   treeStore       ::   New.TreeStore (String, [(ModuleDescr,PackageDescr)])
,   facetStore      ::   New.ListStore (String, IdentifierDescr)
}

instance CastablePane GhfModules where
    casting _               =   ModulesCasting
    downCast _ (PaneC a)    =   case casting a of
                                    ModulesCasting  -> Just a
                                    _               -> Nothing

data ModulesState           =   ModulesState Int
    deriving(Eq,Ord,Read,Show)

instance Model ModulesState where
    toPaneState a           =   ModulesSt a

instance CastableModel ModulesState where
    castingS _               =   ModulesCastingS
    downCastS _ (StateC a)    =   case castingS a of
                                    ModulesCastingS -> Just a
                                    _               -> Nothing

-- | A callers pane description
--

data GhfCallers     =   GhfCallers {
    scrolledView    ::   ScrolledWindow
,   treeViewC       ::   New.TreeView
,   callersStore    ::   New.ListStore (ModuleDescr,Symbol)
}

instance CastablePane GhfCallers where
    casting _               =   CallersCasting
    downCast _ (PaneC a)    =   case casting a of
                                    CallersCasting  -> Just a
                                    _               -> Nothing

data CallersState           =   CallersState
    deriving(Eq,Ord,Read,Show)

instance Model CallersState where
    toPaneState a           =   CallersSt a

instance CastableModel CallersState where
    castingS _               =   CallersCastingS
    downCastS _ (StateC a)    =   case castingS a of
                                    CallersCastingS -> Just a
                                    _               -> Nothing
data PaneState      =   BufferSt BufferState
                    |   LogSt LogState
                    |   InfoSt InfoState
                    |   ModulesSt ModulesState
                    |   CallersSt CallersState
    deriving(Eq,Ord,Read,Show)

