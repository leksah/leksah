-----------------------------------------------------------------------------
--
-- Module      :  Ghf.Core.State
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

module Ghf.Core.State (
-- * IDE State
    Ghf(..)
,   GhfRef
,   GhfM
,   GhfAction

-- * Convenience methods for accesing the IDE State
,   readGhf
,   modifyGhf
,   modifyGhf_
,   withGhf

,   removePaneAdmin
,   addPaneAdmin
-- * debugging
--,   helpDebug
,   message
,   trace
,   module Ghf.Core.Data
,   module Ghf.Core.Panes

) where

import Debug.Trace
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.ModelView as New
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.IORef
import Control.Monad.Reader
import GHC (Session)

import Ghf.Core.Data
import Ghf.Core.Panes


message m = trace m (return ())

-- ---------------------------------------------------------------------
-- IDE State
--

--
-- | The IDE state
--
data Ghf            =  Ghf {
    window          ::  Window                  -- ^ the gtk window
,   uiManager       ::  UIManager               -- ^ the gtk uiManager
,   panes           ::  Map PaneName GhfPane    -- ^ a map with all panes (subwindows)
,   activePane      ::  Maybe (PaneName,Connections)
,   paneMap         ::  Map PaneName (PanePath, Connections)
                    -- ^ a map from the pane to its gui path and signal connections
,   layout          ::  PaneLayout              -- ^ a description of the general gui layout
,   specialKeys     ::  SpecialKeyTable GhfRef  -- ^ a structure for emacs like keystrokes
,   specialKey      ::  SpecialKeyCons GhfRef   -- ^ the first of a double keystroke
,   candy           ::  CandyTables             -- ^ table for source candy
,   prefs           ::  Prefs                   -- ^ configuration preferences
--,   packages        ::  [GhfPackage]            -- ^ the packages known to ghf
,   activePack      ::  Maybe GhfPackage
,   errors          ::  [ErrorSpec]
,   currentErr      ::  Maybe Int
,   accessibleInfo  ::  Maybe (PackageScope)
,   currentInfo     ::  Maybe (PackageScope,PackageScope)
,   session         ::  Session                 -- ^ the bridge to ghc
} --deriving Show

--
-- | A mutable reference to the IDE state
--
type GhfRef = IORef Ghf

--
-- | A reader monad for a mutable reference to the IDE state
--
type GhfM = ReaderT (GhfRef) IO

--
-- | A shorthand for a reader monad for a mutable reference to the IDE state
-- | which does not return a value
--
type GhfAction = GhfM ()

-- ---------------------------------------------------------------------
-- Convenience methods for accesing the IDE State
--

-- | Read an attribute of the contents
readGhf :: (Ghf -> beta) -> GhfM beta
readGhf f = do
    e <- ask
    lift $ liftM f (readIORef e)

-- | Modify the contents, using an IO action.
modifyGhf_ :: (Ghf -> IO Ghf) -> GhfM ()
modifyGhf_ f = do
    e <- ask
    e' <- lift $ (f =<< readIORef e)
    lift $ writeIORef e e'

-- | Variation on modifyGhf_ that lets you return a value
modifyGhf :: (Ghf -> IO (Ghf,beta)) -> GhfM beta
modifyGhf f = do
    e <- ask
    (e',result) <- lift (f =<< readIORef e)
    lift $ writeIORef e e'
    return result

withGhf :: (Ghf -> IO alpha) -> GhfM alpha
withGhf f = do
    e <- ask
    lift $ f =<< readIORef e

removePaneAdmin :: (CastablePane alpha,ModelPane alpha beta) => alpha -> GhfAction
removePaneAdmin pane = do
    panes'          <-  readGhf panes
    paneMap'        <-  readGhf paneMap
    let newPanes    =   Map.delete (paneName pane) panes'
    let newPaneMap  =   Map.delete (paneName pane) paneMap'
    modifyGhf_ (\ghf -> return (ghf{panes = newPanes, paneMap = newPaneMap}))

addPaneAdmin :: (CastablePane alpha,ModelPane alpha beta) => alpha -> Connections -> PanePath ->  GhfAction
addPaneAdmin pane conn pp = do
    panes'          <-  readGhf panes
    paneMap'        <-  readGhf paneMap
    let newPaneMap  =   Map.insert (paneName pane) (pp, conn) paneMap'
    let newPanes    =   Map.insert (paneName pane) (PaneC pane) panes'
    modifyGhf_ (\ghf -> return (ghf{panes = newPanes,
                                    paneMap = newPaneMap}))

