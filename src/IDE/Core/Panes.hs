{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.Panes
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The core state of ide. This module is imported from every other module,
-- | and all data structures of the state are declared here, to avoid circular
-- | module dependencies.
--
-------------------------------------------------------------------------------

module IDE.Core.Panes (
-- * Panes and pane layout
    Pane(..)
,   CastablePane(..)
,   Casting(..)
,   IDEPane(..)
,   ModelPane(..)
,   PaneDirection(..)
,   PanePath
,   PaneLayout(..)
,   PaneName
,   Connections(..)

,   IDEState(..)
,   PaneState(..)
,   Model(..)
,   CastableModel(..)

-- * The pane types
,   IDEBuffer(..)
,   BufferState(..)
,   IDELog(..)
,   LogState(..)
,   IDEInfo(..)
,   InfoState(..)
,   FacetWrapper(..)
,   IDEModules(..)
,   ModulesState(..)
,   IDECallers(..)
,   CallersState(..)
,   IDEToolbar(..)
,   ToolbarState(..)
,   IDEFind(..)
,   FindState(..)

) where

import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.ModelView as New
import System.Glib.Signals
import Data.Maybe
import System.Time
import GHC.IOBase hiding (BufferState)

import IDE.Core.Types
import {-# SOURCE #-} IDE.Core.State
import IDE.Framework.EditorBasics
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
    makeActive      ::   alpha -> IDEAction
    close           ::   alpha -> IDEAction

class CastablePane alpha where
    casting         ::   alpha -> Casting alpha
    downCast        ::   Casting alpha -> IDEPane -> Maybe alpha
    isIt            ::   Casting alpha -> IDEPane -> Bool
    isIt t i        =   isJust (downCast t i)

class (Pane alpha, Model beta) => ModelPane alpha beta | beta -> alpha, alpha -> beta  where
    saveState               ::   alpha -> IDEM (Maybe IDEState)
    recoverState            ::   PanePath -> beta -> IDEAction

data IDEPane        =   forall alpha beta . (CastablePane alpha, ModelPane alpha beta) => PaneC alpha

instance Pane IDEPane where
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
    downCastS        ::   CastingS alpha -> IDEState -> Maybe alpha
    isItS            ::   CastingS alpha -> IDEState -> Bool
    isItS t i        =   isJust (downCastS t i)

data IDEState       =   forall alpha beta . (ModelPane alpha beta, CastableModel beta) => StateC beta

instance Model IDEState where
    toPaneState (StateC a)  =   toPaneState a

instance ModelPane IDEPane IDEState where
    saveState (PaneC p)             =   saveState p
    recoverState pp (StateC s)      =   recoverState pp s

-- ---------------------------------------------------------------------
-- All pane types must be in here !
--
data Casting alpha  where
    LogCasting      ::   Casting IDELog
    InfoCasting     ::   Casting IDEInfo
    BufferCasting   ::   Casting IDEBuffer
    ModulesCasting  ::   Casting IDEModules
    CallersCasting  ::   Casting IDECallers
    ToolbarCasting  ::   Casting IDEToolbar
    FindCasting     ::   Casting IDEFind

data CastingS alpha  where
    LogCastingS      ::   CastingS LogState
    InfoCastingS     ::   CastingS InfoState
    BufferCastingS   ::   CastingS BufferState
    ModulesCastingS  ::   CastingS ModulesState
    CallersCastingS  ::   CastingS CallersState
    ToolbarCastingS  ::   CastingS ToolbarState
    FindCastingS     ::   CastingS FindState


data PaneState      =   BufferSt BufferState
                    |   LogSt LogState
                    |   InfoSt InfoState
                    |   ModulesSt ModulesState
                    |   CallersSt CallersState
                    |   ToolbarSt ToolbarState
                    |   FindSt FindState
    deriving(Eq,Ord,Read,Show)

-- ---------------------------------------------------------------------
-- Special Panes - The data structures for the panes
--

--
-- | A text editor pane description
--
data IDEBuffer      =   IDEBuffer {
    fileName        ::  Maybe FilePath
,   bufferName      ::  String
,   addedIndex      ::  Int
,   sourceView      ::  SourceView
,   scrolledWindow  ::  ScrolledWindow
,   modTime         ::  Maybe (ClockTime)
}


instance CastablePane IDEBuffer where
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
data IDELog         =   IDELog {
    textView        ::   TextView
,   scrolledWindowL ::   ScrolledWindow
}

instance CastablePane IDELog where
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
data IDEInfo        =   IDEInfo {
    sw              ::   ScrolledWindow
,   currentIDs      ::   IORef [IdentifierDescr]
,   currentInd      ::   IORef Int
,   injectors       ::   [IdentifierDescr -> IO()]
,   extractors      ::   [IdentifierDescr -> Extractor IdentifierDescr]
,   nextB           ::   Button
,   prevB           ::   Button
,   numLabel        ::   Label
}

instance CastablePane IDEInfo where
    casting _               =   InfoCasting
    downCast _ (PaneC a)    =   case casting a of
                                    InfoCasting -> Just a
                                    _           -> Nothing

data InfoState              =   InfoState [IdentifierDescr] Int
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

data IDEModules     =   IDEModules {
    outer           ::   VBox
,   paned           ::   HPaned
,   treeView        ::   New.TreeView
,   treeStore       ::   New.TreeStore (String, [(ModuleDescr,PackageDescr)])
,   facetView       ::   New.TreeView
,   facetStore      ::   New.TreeStore FacetWrapper
}

instance CastablePane IDEModules where
    casting _               =   ModulesCasting
    downCast _ (PaneC a)    =   case casting a of
                                    ModulesCasting  -> Just a
                                    _               -> Nothing

data FacetWrapper =
        Itself IdentifierDescr
    |   ConstructorW Symbol IdentifierDescr
    |   FieldW Symbol IdentifierDescr
    |   ClassOpsW Symbol IdentifierDescr
    |   OrphanedData IdentifierDescr

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

data IDECallers     =   IDECallers {
    scrolledView    ::   ScrolledWindow
,   treeViewC       ::   New.TreeView
,   callersStore    ::   New.ListStore (ModuleDescr,Symbol)
}

instance CastablePane IDECallers where
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

-- | A Toolbar pane description
--
data IDEToolbar     =   IDEToolbar {
    toolbar         ::   Toolbar
}

instance CastablePane IDEToolbar where
    casting _               =   ToolbarCasting
    downCast _ (PaneC a)    =   case casting a of
                                    ToolbarCasting  -> Just a
                                    _               -> Nothing

data ToolbarState           =   ToolbarState
    deriving(Eq,Ord,Read,Show)

instance Model ToolbarState where
    toPaneState a           =   ToolbarSt a

instance CastableModel ToolbarState where
    castingS _              =   ToolbarCastingS
    downCastS _ (StateC a)  =   case castingS a of
                                    ToolbarCastingS -> Just a
                                    _               -> Nothing

-- | A Find pane description
--
data IDEFind                =   IDEFind {
    findBox                 ::   HBox
,   caseSensitive           ::   ToggleButton
,   wrapAround              ::   ToggleButton
,   entireWord              ::   ToggleButton
,   gotoLine                ::   SpinButton
,   findEntry               ::   Entry
}

instance CastablePane IDEFind where
    casting _               =   FindCasting
    downCast _ (PaneC a)    =   case casting a of
                                    FindCasting  -> Just a
                                    _               -> Nothing

data FindState              =   FindState
    deriving(Eq,Ord,Read,Show)

instance Model FindState where
    toPaneState a           =   FindSt a

instance CastableModel FindState where
    castingS _              =   FindCastingS
    downCastS _ (StateC a)  =   case castingS a of
                                    FindCastingS -> Just a
                                    _               -> Nothing
