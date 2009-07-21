{-# OPTIONS_GHC
    -XExistentialQuantification
    -XMultiParamTypeClasses
    -XFunctionalDependencies
    -XNoMonomorphismRestriction  #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.Panes
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional21
-- Portability :  portable
--
-- | The basic definitions for all panes
--
-------------------------------------------------------------------------------

module Graphics.UI.Frame.Panes (

-- * Panes and pane layout
    PaneMonad(..)
,   Pane(..)
,   IDEPane(..)
,   RecoverablePane(..)
,   PaneDirection(..)
,   PanePathElement(..)
,   PanePath
,   PaneLayout(..)
,   PaneName
,   Connection(..)
,   Connections
,   StandardPath
,   FrameState(..)
,   signalDisconnectAll
) where

import Graphics.UI.Gtk hiding (get)
import System.Glib.GObject
import System.Glib.Signals
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Control.Monad.Trans

-- ---------------------------------------------------------------------
-- Panes and pane layout
--

--
-- | A path to a pane
--
type PanePath       =   [PanePathElement]

--
-- | An element of a path to a pane
--
data PanePathElement = SplitP PaneDirection | GroupP String
    deriving (Eq,Show,Read)

--
-- | The relative direction to a pane from the parent
--
data PaneDirection  =   TopP | BottomP | LeftP | RightP
    deriving (Eq,Show,Read)
  	
--
-- | Description of a window layout
-- Horizontal: top bottom Vertical: left right
--
data PaneLayout =       HorizontalP PaneLayout PaneLayout Int
                    |   VerticalP PaneLayout PaneLayout Int
                    |   TerminalP {
                                paneGroups   :: Map String PaneLayout
                            ,   paneTabs     :: Maybe PaneDirection
                            ,   currentPage  :: Int
                            ,   detachedId   :: Maybe String
                            ,   detachedSize :: Maybe (Int, Int) }
    deriving (Eq,Show,Read)

--
-- | All kinds of panes are instances of pane
--

class (Typeable alpha, PaneMonad beta) => Pane alpha beta | alpha -> beta where
    paneName        ::   alpha -> PaneName
    paneName b      =   if getAddedIndex b == 0
                            then primPaneName b
                            else primPaneName b ++ "(" ++ show (getAddedIndex b) ++ ")"
    primPaneName    ::   alpha -> String
    getAddedIndex   ::   alpha -> Int
    getAddedIndex _ =   0
    getTopWidget    ::   alpha -> Widget
    paneId          ::   alpha -> String
    makeActive      ::   alpha -> beta ()
    close           ::   alpha -> beta Bool

class (Pane alpha delta, Read beta, Show beta, Typeable beta, PaneMonad delta)
                    => RecoverablePane alpha beta delta | beta -> alpha, alpha -> beta where
    saveState       ::   alpha -> delta  (Maybe beta)
    recoverState    ::   PanePath -> beta -> delta ()

type PaneName = String

data IDEPane delta       =   forall alpha beta. (RecoverablePane alpha beta delta) => PaneC alpha

instance Eq (IDEPane delta) where
    (==) (PaneC x) (PaneC y) = paneName x == paneName y

instance Ord (IDEPane delta) where
    (<=) (PaneC x) (PaneC y) = paneName x <=  paneName y

instance Show (IDEPane delta) where
    show (PaneC x)    = "Pane " ++ paneName x

type StandardPath = PanePath

data FrameState delta = FrameState {
    windows         ::  [Window]
,   uiManager       ::  UIManager
,   panes           ::  Map PaneName (IDEPane delta)
,   paneMap         ::  (Map PaneName (PanePath, Connections))
,   activePane      ::  Maybe (PaneName, Connections)
,   panePathFromNB  ::  Map Notebook PanePath
,   layout          ::  PaneLayout}

class MonadIO delta =>  PaneMonad delta where
    setFrameState   ::  FrameState delta -> delta ()
    getFrameState   ::  delta (FrameState delta)
    runInIO         ::  forall alpha beta. (beta -> delta alpha) -> delta (beta -> IO alpha)
    panePathForGroup::  String -> delta PanePath

--
-- | Signal handlers for the different pane types
--
data Connection =  forall alpha . GObjectClass alpha => ConnectC (ConnectId alpha)

type Connections = [Connection]

signalDisconnectAll :: Connections -> IO ()
signalDisconnectAll = mapM_ (\ (ConnectC s) -> signalDisconnect s)


-- Necessary with pre 10.1 verion of gtk2hs

#ifdef MIN_VERSION_gtk
#if MIN_VERSION_gtk(0,10,1)
#else
instance Eq Notebook
    where (==) a b = let (GObject pa, GObject pb) = (toGObject a, toGObject b)
                    in pa == pb
instance Ord Notebook
    where (<=) a b = let (GObject pa, GObject pb) = (toGObject a, toGObject b)
                    in pa <= pb
instance Eq Window
    where (==) a b = let (GObject pa, GObject pb) = (toGObject a, toGObject b)
                    in pa == pb
#endif
#endif
