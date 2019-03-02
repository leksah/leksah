{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.State
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | The core state of ide. This module is imported from every other module,
-- | and all data structures of the state are declared here, to avoid circular
-- | module dependencies.
--
-------------------------------------------------------------------------------

module IDE.Gtk.State (
    window

,   isStartingOrClosing

,   deactivatePane

,   postSyncIDE'
,   postAsyncIDE'
,   postSyncIDE
,   postAsyncIDE
,   postSyncIDEIdle
,   postAsyncIDEIdle
,   onIDE

,   withoutRecordingDo

,   delayedBy

,   module Reexported

) where

import Prelude ()
import Prelude.Compat
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.IO.Class (liftIO)
import IDE.Core.Types
import IDE.Gtk.Types as Reexported
import IDE.Core.State
import Graphics.UI.Frame.Panes as Reexported
import Graphics.UI.Frame.ViewFrame as Reexported --hiding (notebookInsertOrdered)
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Map as Map (lookup)
import Control.Monad (void, when)
import Control.Monad.Trans.Reader (ask, ReaderT(..))
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import GI.Gtk.Objects.Widget
       (widgetDestroy, widgetShowAll, widgetSetName, widgetGrabFocus)
import GI.Gtk.Objects.Notebook
       (notebookRemovePage, notebookPageNum)
import Data.Int (Int32)
import GI.GLib
       (timeoutAdd, pattern PRIORITY_DEFAULT_IDLE, pattern PRIORITY_DEFAULT, idleAdd)
import GI.Gtk.Objects.Label (noLabel)
import IDE.Utils.DebugUtils (traceTimeTaken)
import GHC.Stack (HasCallStack)
import Control.Lens (pre, _Just, (.~), _1, (%~))
import GI.Gtk.Objects.Window (Window(..))
import Data.GI.Base (GObject)
import Data.GI.Base.Signals (SignalHandlerId)

instance PaneMonad IDEM where
    getFrameState   =   fromMaybe (error "No gtk frame state") <$> readIDE (pre $ ideGtk . _Just . frameState)
    setFrameState v =   modifyIDE_ $ ideGtk . _Just . frameState .~ v
    runInIO f       =   reifyIDE (\ideRef -> return (\v -> reflectIDE (f v) ideRef))
    panePathForGroup id' =   do
        prefs'  <- readIDE prefs
        case id' `lookup` categoryForPane prefs' of
            Just group -> case group `lookup` pathForCategory prefs' of
                            Nothing -> return (defaultPath prefs')
                            Just p  -> return p
            Nothing    -> return (defaultPath prefs')
    getThisPane = getPanePrim
    -- getThisPane     ::  forall alpha beta . RecoverablePane alpha beta delta => Maybe PanePath -> delta alpha
    getOrBuildThisPane ePpoPid = do
        mbPane <- getPanePrim
        case mbPane of
            Nothing -> do
                pp          <-  case ePpoPid of
                                    Right pId  -> getBestPathForId pId
                                    Left ppp ->
                                        getBestPanePath ppp <$> getLayout
                nb          <-  getNotebook pp
                buildPane pp nb builder
            Just pane ->   return (Just pane)

    -- displayThisPane ::  Bool -> delta alpha
    displayThisPane pane shallGrabFocus = do
        bringPaneToFront pane
        when shallGrabFocus $ widgetGrabFocus =<< getTopWidget pane
    -- buildThisPane   ::  forall alpha beta . RecoverablePane alpha beta delta => PanePath ->
    --                    Notebook ->
    --                    (PanePath -> Notebook -> Window -> delta (alpha,Connections)) ->
    --                    delta alpha
    buildThisPane panePath notebook builder' = do
        windows'       <-  getWindows

        (mbBuf,cids)  <-  builder' panePath notebook (head windows')
        case mbBuf of
            Nothing -> return Nothing
            Just buf -> do
                panes'          <-  getPanesSt
                paneMap'        <-  getPaneMapSt
                let b1 = isNothing $ Map.lookup (paneName buf) paneMap'
                let b2 = isNothing $ Map.lookup (paneName buf) panes'
                if b1 && b2
                    then do
                        topWidget <- getTopWidget buf
                        notebookInsertOrdered notebook topWidget (paneName buf) noLabel (paneTooltipText buf) False
                        _ <- addPaneAdmin buf cids panePath
                        widgetSetName topWidget (paneName buf)
                        widgetShowAll topWidget
                        widgetGrabFocus topWidget
                        bringPaneToFront buf
                        return (Just buf)
                    else return Nothing
    --activateThisPane :: forall alpha beta . RecoverablePane alpha beta delta => alpha -> Connections -> delta ()
    activateThisPane pane conn =
        getActivePane >>= \case
            (Just (pn,_), _) | pn == paneName pane -> return ()
            (mbActive, panes') -> do
                deactivatePaneWithout
                _ <- triggerEventIDE (StatusbarChanged [CompartmentPane (paneName pane)])
                bringPaneToFront pane
                let mru = filter (/=paneName pane) panes'
                    mru' = maybe mru ((:mru) . fst) mbActive
                setActivePane (Just (paneName pane, conn), mru')
                trigger (Just (paneName pane)) (fst <$> mbActive)
                return ()
        where
            trigger :: Maybe Text -> Maybe Text -> IDEAction
            trigger s1 s2 = do
                triggerEventIDE_ (GtkEvent (RecordHistory (PaneSelected s1, PaneSelected s2)))
                triggerEventIDE_ (Sensitivity [(SensitivityEditor, False)])
    --closeThisPane   ::  forall alpha beta . RecoverablePane alpha beta delta => alpha -> delta Bool
    closeThisPane pane = do
        (panePath,_)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        i               <-  notebookPageNum nb =<< getTopWidget pane
        if i < 0
            then liftIO $ do
                _ <- error ("notebook page not found: unexpected " ++ T.unpack (paneName pane) ++ " " ++ show panePath)
                return False
            else do
                deactivatePaneIfActive pane
                notebookRemovePage nb i
                widgetDestroy =<< getTopWidget pane
                removePaneAdmin pane
                modifyIDE_ $
                    ideGtk . _Just . frameState %~ (\s -> s {
                        activePane = ( fst (activePane s)
                                     , filter (/= paneName pane) (snd (activePane s)))})
                return True

-- Main window is always the first one in the list
window :: FrameState delta -> Window
window = head . windows

isStartingOrClosing ::  IDEState -> Bool
isStartingOrClosing IsStartingUp    = True
isStartingOrClosing IsShuttingDown  = True
isStartingOrClosing _               = False

postSyncIDE' :: (MonadIDE m, HasCallStack) => Int32 -> IDEM a -> m a
postSyncIDE' priority f = reifyIDE $ \ideR -> do
    resultVar <- newEmptyMVar
    _ <- idleAdd priority $ reflectIDE (traceTimeTaken "postSyncIDE'" f) ideR >>= putMVar resultVar >> return False
    takeMVar resultVar

postSyncIDE :: (MonadIDE m, HasCallStack) => IDEM a -> m a
postSyncIDE = postSyncIDE' PRIORITY_DEFAULT

postSyncIDEIdle :: (MonadIDE m, HasCallStack) => IDEM a -> m a
postSyncIDEIdle = postSyncIDE' PRIORITY_DEFAULT_IDLE

postAsyncIDE' :: (MonadIDE m, HasCallStack) => Int32 -> IDEM () -> m ()
postAsyncIDE' priority f = reifyIDE $ \ideR ->
    void . idleAdd priority $ reflectIDE (traceTimeTaken "postAsyncIDE'" f) ideR >> return False

postAsyncIDE :: (MonadIDE m, HasCallStack) => IDEM () -> m ()
postAsyncIDE = postAsyncIDE' PRIORITY_DEFAULT

postAsyncIDEIdle :: (MonadIDE m, HasCallStack) => IDEM () -> m ()
postAsyncIDEIdle = postAsyncIDE' PRIORITY_DEFAULT_IDLE

onIDE
  :: GObject t
  => (t -> (r -> m a) -> IO SignalHandlerId)
  -> t
  -> ReaderT IDERef (ReaderT r m) a
  -> IDEM Connection
onIDE onSignal obj callback = do
    ideRef <- ask
    liftIO (ConnectC obj <$> onSignal obj (runReaderT (runReaderT callback ideRef)))

-- ---------------------------------------------------------------------
-- Activating and deactivating Panes.
-- This is here and not in Views becau	se it needs some dependencies
-- (e.g. Events for history)
--

deactivatePane :: IDEAction
deactivatePane =
    getActivePane >>= \case
        (Nothing,_)      -> return ()
        (Just (pn, _),_) -> do
            deactivatePaneWithout
            triggerEventIDE_ (GtkEvent (RecordHistory (PaneSelected Nothing,
                PaneSelected (Just pn))))
            triggerEventIDE_ (Sensitivity [(SensitivityEditor, False)])
            return ()

deactivatePaneWithout :: IDEAction
deactivatePaneWithout = do
    triggerEventIDE_ (StatusbarChanged [CompartmentPane ""])
    getActivePane >>= \case
        (Just (n,signals), mru) -> do
            signalDisconnectAll signals
            setActivePane (Nothing, n:mru)
        (Nothing, _) -> return ()

withoutRecordingDo :: IDEAction -> IDEAction
withoutRecordingDo act =
    readIDE (pre $ ideGtk . _Just . guiHistory . _1) >>= \case
        Just False -> do
            modifyIDE_ $ ideGtk . _Just . guiHistory . _1 .~ True
            act
            modifyIDE_ $ ideGtk . _Just . guiHistory . _1 .~ False
        _ -> act

deactivatePaneIfActive :: RecoverablePane alpha beta IDEM => alpha -> IDEAction
deactivatePaneIfActive pane =
    getActivePane >>= \case
        (Nothing, _) -> return ()
        (Just (n,_), _) -> when (n == paneName pane) deactivatePane

delayedBy :: MonadUnliftIO m => Int -> m () -> m ()
delayedBy n f = withRunInIO $ \run ->
    void . timeoutAdd PRIORITY_DEFAULT (fromIntegral $ n `div` 1000) $ run f >> return False
