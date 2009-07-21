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
showDebugger
) where

import IDE.Core.State
    (getNotebook, viewSplit', newGroupOrBringToFront, IDEAction(..))
import Graphics.UI.Frame.Panes
    (PaneDirection(..),
     PanePathElement(..),
     panePathForGroup)
import IDE.Pane.SourceBuffer
    (newTextBuffer, bufferName, allBuffers)
import Graphics.UI.Editor.Parameters (Direction(..))
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk (notebookSetShowTabs, notebookSetTabPos)
import Graphics.UI.Gtk.General.Enums (PositionType(..))
import IDE.Pane.Variables (showVariables')
import IDE.Pane.Breakpoints (showBreakpointList')
import IDE.Pane.Trace(showTrace')
import Control.Monad (when)

showDebugger :: IDEAction
showDebugger = do
    pp   <- panePathForGroup "*Debug"
    ret  <- newGroupOrBringToFront "Debug" pp
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
            showBreakpointList' middleP
            showVariables' middleP
            showTrace' lowerP
            when (null $ filter (\b -> bufferName b == "_Eval.hs") bufs) $
                newTextBuffer upperP "_Eval.hs" Nothing >> return ()
            return ()
        (Just rpp, False) -> do
            let lowerP =  rpp ++ [SplitP BottomP]
            let middleP =  rpp ++ [SplitP TopP,SplitP BottomP]
            let upperP =  rpp ++ [SplitP TopP,SplitP TopP]
            showBreakpointList' middleP
            showVariables' middleP
            showTrace' lowerP
            when (null $ filter (\b -> bufferName b == "_Eval.hs") bufs) $
                newTextBuffer upperP "_Eval.hs" Nothing >> return ()
            return ()
        _ -> return ()
