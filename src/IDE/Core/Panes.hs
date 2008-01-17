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
--    Casting(..)
--,   CastingS(..)

-- * Panes and pane layout
    Pane(..)
,   CastablePane(..)
,   Casting(..)
,   IDEPane(..)
,   RecoverablePane(..)
,   PaneDirection(..)
,   PanePath
,   PaneLayout(..)
,   PaneName
,   Connections(..)

,   IDEState(..)
,   PaneState(..)
,   Recoverable(..)

-- * The pane types
,   IDEBuffer(..)
,   BufferState(..)
,   IDEInfo(..)
,   InfoState(..)
,   FacetWrapper(..)
,   IDEModules(..)
,   ModulesState(..)
,   IDECallers(..)
,   CallersState(..)
,   IDEToolbar(..)
,   ToolbarState(..)

,   IDEReplace(..)
,   ReplaceState(..)

,   IDELog(..)
,   LogState(..)
,   LogTag(..)

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
                    |   TerminalP (Maybe PaneDirection) Int
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

class (Pane alpha, Recoverable beta) => RecoverablePane alpha beta | beta -> alpha, alpha -> beta  where
    saveState               ::   alpha -> IDEM (Maybe IDEState)
    recoverState            ::   PanePath -> beta -> IDEAction

class CastablePane alpha where
    casting         ::   alpha -> Casting alpha
    downCast        ::   Casting alpha -> IDEPane -> Maybe alpha
    isIt            ::   Casting alpha -> IDEPane -> Bool
    isIt t i        =   isJust (downCast t i)

data Casting alpha  where
    LogCasting      ::   Casting IDELog
    InfoCasting     ::   Casting IDEInfo
    BufferCasting   ::   Casting IDEBuffer
    ModulesCasting  ::   Casting IDEModules
    CallersCasting  ::   Casting IDECallers
    ToolbarCasting  ::   Casting IDEToolbar
    FindCasting     ::   Casting IDEFind
    ReplaceCasting  ::   Casting IDEReplace

data IDEPane        =   forall alpha beta . (CastablePane alpha, RecoverablePane alpha beta) => PaneC alpha

instance Pane IDEPane where
    paneName (PaneC a)      =   paneName a
    primPaneName (PaneC a)  =   primPaneName a
    getAddedIndex (PaneC a) =   getAddedIndex a
    getTopWidget (PaneC a)  =   getTopWidget a
    paneId (PaneC a)        =   paneId a
    makeActive (PaneC a)    =   makeActive a
    close (PaneC a)         =   close a

class Recoverable alpha where
    toPaneState      ::   alpha -> PaneState

data IDEState       =   forall alpha beta . (RecoverablePane alpha beta, Recoverable beta) => StateC beta

instance Recoverable IDEState where
    toPaneState (StateC a)  =   toPaneState a

instance RecoverablePane IDEPane IDEState where
    saveState (PaneC p)             =   saveState p
    recoverState pp (StateC s)      =   recoverState pp s

-- ---------------------------------------------------------------------
-- All pane types must be in here !
--

data PaneState      =   BufferSt BufferState
                    |   LogSt LogState
                    |   InfoSt InfoState
                    |   ModulesSt ModulesState
                    |   CallersSt CallersState
                    |   ToolbarSt ToolbarState
                    |   FindSt FindState
                    |   ReplaceSt ReplaceState
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

instance Recoverable BufferState where
    toPaneState a           =   BufferSt a


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

instance Recoverable InfoState where
    toPaneState a           =   InfoSt a


-- | A modules pane description
--

data IDEModules     =   IDEModules {
    outer           ::   VBox
,   paned           ::   HPaned
,   treeView        ::   New.TreeView
,   treeStore       ::   New.TreeStore (String, [(ModuleDescr,PackageDescr)])
,   facetView       ::   New.TreeView
,   facetStore      ::   New.TreeStore FacetWrapper
,   localScopeB     ::   RadioButton
,   packageScopeB   ::   RadioButton
,   worldScopeB     ::   RadioButton
,   blacklistB      ::   CheckButton
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

data ModulesState           =   ModulesState Int (Scope,Bool)
                                    (Maybe String, Maybe String)
    deriving(Eq,Ord,Read,Show)

instance Recoverable ModulesState where
    toPaneState a           =   ModulesSt a


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

instance Recoverable CallersState where
    toPaneState a           =   CallersSt a

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

instance Recoverable ToolbarState where
    toPaneState a           =   ToolbarSt a

-- | A Control pane for simple text replace
--

data IDEReplace             =   IDEReplace {
    replaceBox              ::   VBox
--,   replaceExtractor        ::   Extractor ReplaceState
}

data ReplaceState = ReplaceState{
    searchFor       ::   String
,   replaceWith     ::   String
,   matchCase       ::   Bool
,   matchEntire     ::   Bool
,   searchBackwards ::   Bool}
    deriving(Eq,Ord,Read,Show)

--
-- | The Log Viev
--

data IDELog         =   IDELog {
    textView        ::   TextView
,   scrolledWindowL ::   ScrolledWindow}

data LogState               =   LogState
    deriving(Eq,Ord,Read,Show)

data LogTag = LogTag | ErrorTag | FrameTag


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

data FindState              =   FindState
    deriving(Eq,Ord,Read,Show)





