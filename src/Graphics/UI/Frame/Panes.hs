{-# OPTIONS_GHC
    -XExistentialQuantification
    -XMultiParamTypeClasses
    -XFunctionalDependencies #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.Panes
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info at leksah.org>
-- Stability   :  experimental
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
,   PanePath
,   PaneLayout(..)
,   PaneName
,   Connection(..)
,   Connections
,   StandardPath
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
    makeActive _    =    return ()
    close           ::   alpha -> beta ()

class (Pane alpha delta, Read beta, Show beta, Typeable beta, PaneMonad delta)
                    => RecoverablePane alpha beta delta | beta -> alpha, alpha -> beta where
    saveState       ::   alpha -> delta  (Maybe beta)
    recoverState    ::   PanePath -> beta -> delta ()

type PaneName = String

data IDEPane delta       =   forall alpha beta. (RecoverablePane alpha beta delta) => PaneC alpha

instance Eq (IDEPane delta) where
    (== ) (PaneC x) (PaneC y) = paneName x == paneName y

instance Ord (IDEPane delta) where
    (<=) (PaneC x) (PaneC y) = paneName x <=  paneName y

instance Show (IDEPane delta) where
    show (PaneC x)    = "Pane " ++ paneName x

type StandardPath = PanePath


class MonadIO delta =>  PaneMonad delta where
    getWindowSt     ::   delta Window
    getUIManagerSt  ::   delta UIManager

    getPanesSt      ::   delta (Map PaneName (IDEPane delta))
    setPanesSt      ::   Map PaneName (IDEPane delta) -> delta ()

    getPaneMapSt    ::   delta (Map PaneName (PanePath, Connections))
    setPaneMapSt    ::   Map PaneName (PanePath, Connections) -> delta ()

    getActivePaneSt ::   delta (Maybe (PaneName, Connections))
    setActivePaneSt ::   Maybe (PaneName, Connections) -> delta ()

    getLayoutSt     ::   delta PaneLayout
    setLayoutSt     ::   PaneLayout -> delta ()

    runInIO         ::   forall beta. (beta -> delta()) -> delta (beta -> IO ())


--
-- | Signal handlers for the different pane types
--
data Connection =  forall alpha . GObjectClass alpha => ConnectC (ConnectId alpha)

type Connections = [Connection]

signalDisconnectAll :: Connections -> IO ()
signalDisconnectAll = mapM_ (\ (ConnectC s) -> signalDisconnect s)



