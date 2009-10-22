{-# OPTIONS_GHC -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Group.Debugger
-- Copyright   :  2007-2009 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Group.Debugger (
    showDebugger,
    setSensitivityDebugger
) where

import IDE.Core.State
    (IDEM(..),
     getPane,
     getBestPanePath,
     frameState,
     readIDE,
     getNotebook,
     viewSplit',
     newGroupOrBringToFront,
     IDEAction(..))
import Graphics.UI.Frame.Panes
    (getOrBuildPane,
     getTopWidget,
     layout,
     PaneDirection(..),
     PanePathElement(..),
     panePathForGroup)
import IDE.Pane.SourceBuffer
    (newTextBuffer, bufferName, allBuffers)
import Graphics.UI.Editor.Parameters (Direction(..))
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk
    (widgetSetSensitive, notebookSetShowTabs, notebookSetTabPos)
import Graphics.UI.Gtk.General.Enums (PositionType(..))
import IDE.Pane.Variables (IDEVariables)
import IDE.Pane.Breakpoints
    (IDEBreakpoints)
import IDE.Pane.Trace (IDETrace)
import Control.Monad (liftM, when)

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
            viewSplit' (rpp ++ [SplitP TopP]) Horizontal
            let lowerP =  rpp ++ [SplitP BottomP]
            let middleP =  rpp ++ [SplitP TopP,SplitP BottomP]
            let upperP =  rpp ++ [SplitP TopP,SplitP TopP]
            lower <- getNotebook lowerP
            middle <- getNotebook middleP
            upper <- getNotebook upperP
            liftIO $ do
                notebookSetTabPos lower PosTop
                notebookSetTabPos middle PosTop
                notebookSetTabPos upper PosTop
                notebookSetShowTabs upper False
            getOrBuildPane (Left middleP) :: IDEM (Maybe IDEBreakpoints)
            getOrBuildPane (Left middleP) :: IDEM (Maybe IDEVariables)
            getOrBuildPane (Left lowerP)  :: IDEM (Maybe IDETrace)
            when (null $ filter (\b -> bufferName b == "_Eval.hs") bufs) $
                newTextBuffer upperP "_Eval.hs" Nothing >> return ()
            return ()
        (Just rpp, False) -> do
            let lowerP  =  getBestPanePath (rpp ++ [SplitP BottomP]) layout'
            let middleP =  getBestPanePath (rpp ++ [SplitP TopP,SplitP BottomP]) layout'
            let upperP  =  getBestPanePath (rpp ++ [SplitP TopP,SplitP TopP]) layout'
            getOrBuildPane (Left middleP) :: IDEM (Maybe IDEBreakpoints)
            getOrBuildPane (Left middleP) :: IDEM (Maybe IDEVariables)
            getOrBuildPane (Left lowerP)  :: IDEM (Maybe IDETrace)
            when (null $ filter (\b -> bufferName b == "_Eval.hs") bufs) $
                newTextBuffer upperP "_Eval.hs" Nothing >> return ()
            return ()
        _ -> return ()
