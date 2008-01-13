{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.State
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

module IDE.Core.State (
    IDEObject
,   IDEPaneC
,   IDEEditor
-- * IDE State
,   IDE(..)
,   IDERef
,   IDEM
,   IDEAction

-- * Convenience methods for accesing the IDE State
,   readIDE
,   modifyIDE
,   modifyIDE_
,   withIDE

,   removePaneAdmin
,   addPaneAdmin
-- * debugging
--,   helpDebug
,   message
,   trace
,   module IDE.Core.Types
,   module IDE.Core.Panes
,   module IDE.Core.Exception

) where

import Debug.Trace
import Graphics.UI.Gtk hiding (get)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Control.Monad.Reader
import GHC (Session)

import IDE.Core.Types
import IDE.Core.Panes
import IDE.Core.Exception
import IDE.SourceCandy
import IDE.Keymap


message m = trace m (return ())


class IDEObject o
class IDEObject o => IDEPaneC o
class IDEObject o => IDEEditor o


-- ---------------------------------------------------------------------
-- IDE State
--

--
-- | The IDE state
--
data IDE            =  IDE {
    window          ::  Window                  -- ^ the gtk window
,   uiManager       ::  UIManager               -- ^ the gtk uiManager
,   panes           ::  Map PaneName IDEPane    -- ^ a map with all panes (subwindows)
,   activePane      ::  Maybe (PaneName,Connections)
,   paneMap         ::  Map PaneName (PanePath, Connections)
                    -- ^ a map from the pane name to its gui path and signal connections
,   layout          ::  PaneLayout              -- ^ a description of the general gui layout
,   specialKeys     ::  SpecialKeyTable IDERef  -- ^ a structure for emacs like keystrokes
,   specialKey      ::  SpecialKeyCons IDERef   -- ^ the first of a double keystroke
,   candy           ::  CandyTable              -- ^ table for source candy
,   prefs           ::  Prefs                   -- ^ configuration preferences
,   activePack      ::  Maybe IDEPackage
,   errors          ::  [ErrorSpec]
,   currentErr      ::  Maybe Int
,   accessibleInfo  ::  (Maybe (PackageScope))     -- ^  the world scope
,   currentInfo     ::  (Maybe (PackageScope,PackageScope))
                                                -- ^ the first is for the current package,
                                                --the second is the scope in the current package
,   session         ::  Session                  -- ^ a ghc session object, side effects
                                                -- reusing with sessions?
} --deriving Show

--
-- | A mutable reference to the IDE state
--
type IDERef = IORef IDE

--
-- | A reader monad for a mutable reference to the IDE state
--
type IDEM = ReaderT (IDERef) IO

--
-- | A shorthand for a reader monad for a mutable reference to the IDE state
--   which does not return a value
--
type IDEAction = IDEM ()

-- ---------------------------------------------------------------------
-- Convenience methods for accesing the IDE State
--

-- | Read an attribute of the contents
readIDE :: (IDE -> beta) -> IDEM beta
readIDE f = do
    e <- ask
    lift $ liftM f (readIORef e)

-- | Modify the contents, using an IO action.
modifyIDE_ :: (IDE -> IO IDE) -> IDEM ()
modifyIDE_ f = do
    e <- ask
    e' <- lift $ (f =<< readIORef e)
    lift $ writeIORef e e'

-- | Variation on modifyIDE_ that lets you return a value
modifyIDE :: (IDE -> IO (IDE,beta)) -> IDEM beta
modifyIDE f = do
    e <- ask
    (e',result) <- lift (f =<< readIORef e)
    lift $ writeIORef e e'
    return result

withIDE :: (IDE -> IO alpha) -> IDEM alpha
withIDE f = do
    e <- ask
    lift $ f =<< readIORef e

removePaneAdmin :: (CastablePane alpha,ModelPane alpha beta) => alpha -> IDEAction
removePaneAdmin pane = do
    panes'          <-  readIDE panes
    paneMap'        <-  readIDE paneMap
    let newPanes    =   Map.delete (paneName pane) panes'
    let newPaneMap  =   Map.delete (paneName pane) paneMap'
    modifyIDE_ (\ide -> return (ide{panes = newPanes, paneMap = newPaneMap}))

addPaneAdmin :: (CastablePane alpha,ModelPane alpha beta) => alpha -> Connections -> PanePath ->  IDEAction
addPaneAdmin pane conn pp = do
    panes'          <-  readIDE panes
    paneMap'        <-  readIDE paneMap
    let newPaneMap  =   Map.insert (paneName pane) (pp, conn) paneMap'
    let newPanes    =   Map.insert (paneName pane) (PaneC pane) panes'
    modifyIDE_ (\ide -> return (ide{panes = newPanes,
                                    paneMap = newPaneMap}))

