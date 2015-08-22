{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.PaneGroups
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.PaneGroups (

    showBrowser
,   setSensitivityDebugger
,   showDebugger

) where

import IDE.Core.State (IDEM(..), readIDE, IDEAction(..))
import Graphics.UI.Frame.Panes
       (RecoverablePane, PanePath, getTopWidget, getPane, getOrBuildPane,
        PaneDirection(..), PanePathElement(..), layout, panePathForGroup)
import Graphics.UI.Frame.ViewFrame
       (viewMoveTo, getBestPanePath, getNotebook, viewSplit',
        newGroupOrBringToFront)
import Control.Monad (void, unless, when, liftM)
import IDE.Core.Types (frameState)
import Graphics.UI.Editor.Parameters (Direction(..))
import Graphics.UI.Gtk
    (widgetSetSensitive, notebookSetShowTabs, notebookSetTabPos)
import Graphics.UI.Gtk.General.Enums (PositionType(..))
import IDE.Pane.Modules (IDEModules(..))
import IDE.Pane.Info (IDEInfo(..))
import IDE.Pane.SourceBuffer
    (newTextBuffer, bufferName, allBuffers)
import IDE.Pane.Breakpoints (IDEBreakpoints(..))
import IDE.Pane.Variables (IDEVariables(..))
import IDE.Pane.Trace (IDETrace(..))
import IDE.Pane.Workspace (IDEWorkspace(..))
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Pane.WebKit.Output (IDEOutput(..))

moveOrBuildPane :: RecoverablePane alpha beta delta => PanePath -> delta (Maybe alpha)
moveOrBuildPane path = do
    mbPane <- getOrBuildPane (Left path)
    case mbPane of
        Just pane -> viewMoveTo path pane
        Nothing   -> return ()
    return mbPane

showBrowser :: IDEAction
showBrowser = do
    pp   <- panePathForGroup "*Browser"
    ret  <- newGroupOrBringToFront "Browser" pp
    layout' <- liftM layout (readIDE frameState)
    case ret of
        (Just rpp, True) -> do
            viewSplit' rpp Horizontal
            viewSplit' (rpp ++ [SplitP BottomP]) Horizontal
            let lowerP =  rpp ++ [SplitP BottomP, SplitP BottomP]
            let upperP =  rpp ++ [SplitP BottomP, SplitP TopP]
            let topP = rpp ++ [SplitP TopP]
            lower <- getNotebook lowerP
            upper <- getNotebook upperP
            top   <- getNotebook topP
            liftIO $ do
                notebookSetTabPos lower PosBottom
                notebookSetTabPos upper PosTop
                notebookSetTabPos top PosTop
                notebookSetShowTabs upper False
                notebookSetShowTabs lower False
                notebookSetShowTabs top False
            getOrBuildBrowserPanes upperP lowerP topP
        (Just rpp, False) -> do
            let lowerP  =  getBestPanePath (rpp ++  [SplitP BottomP, SplitP BottomP]) layout'
            let upperP  =  getBestPanePath (rpp ++ [SplitP BottomP, SplitP TopP]) layout'
            let topP    =  getBestPanePath (rpp ++ [SplitP TopP]) layout'
            getOrBuildBrowserPanes upperP lowerP topP
        _ -> return ()
  where
    getOrBuildBrowserPanes upperP lowerP topP = do
        moveOrBuildPane upperP :: IDEM (Maybe IDEModules)
        moveOrBuildPane lowerP :: IDEM (Maybe IDEInfo)
        moveOrBuildPane topP :: IDEM (Maybe IDEWorkspace)
        return ()

setSensitivityDebugger :: Bool -> IDEAction
setSensitivityDebugger sens = do
    mbBreakpoints :: Maybe IDEBreakpoints <- getPane
    mbVariables   :: Maybe IDEVariables   <- getPane
    mbTrace       :: Maybe IDETrace       <- getPane
    liftIO $ do
        case mbBreakpoints of
            Nothing -> return ()
            Just idePane -> widgetSetSensitive (getTopWidget idePane) sens
        case mbVariables of
            Nothing -> return ()
            Just idePane -> widgetSetSensitive (getTopWidget idePane) sens
        case mbTrace of
            Nothing -> return ()
            Just idePane -> widgetSetSensitive (getTopWidget idePane) sens

showDebugger :: IDEAction
showDebugger = do
    pp   <- panePathForGroup "*Debug"
    ret  <- newGroupOrBringToFront "Debug" pp
    layout' <- liftM layout (readIDE frameState)
    bufs <- allBuffers
    case ret of
        (Just rpp, True) -> do
            viewSplit' rpp Horizontal
            let lowerP =  rpp ++ [SplitP BottomP]
            let upperP =  rpp ++ [SplitP TopP]
            lower <- getNotebook lowerP
            upper <- getNotebook upperP
            liftIO $ do
                notebookSetTabPos lower PosTop
                notebookSetTabPos upper PosTop
                notebookSetShowTabs upper False
            getOrBuildDebugPanes upperP lowerP bufs
        (Just rpp, False) -> do
            let lowerP  =  getBestPanePath (rpp ++ [SplitP BottomP]) layout'
            let upperP =  getBestPanePath (rpp ++ [SplitP TopP]) layout'
            getOrBuildDebugPanes upperP lowerP bufs
        _ -> return ()
  where
    getOrBuildDebugPanes upperP lowerP bufs = do
        moveOrBuildPane lowerP :: IDEM (Maybe IDEBreakpoints)
        moveOrBuildPane lowerP :: IDEM (Maybe IDEVariables)
        moveOrBuildPane lowerP :: IDEM (Maybe IDETrace)
        moveOrBuildPane lowerP :: IDEM (Maybe IDEOutput)
        unless (any (\ b -> bufferName b == "_Eval.hs") bufs) $
            newTextBuffer upperP "_Eval.hs" Nothing >>= \case
                Nothing   -> return ()
                Just pane -> viewMoveTo upperP pane
        return ()

