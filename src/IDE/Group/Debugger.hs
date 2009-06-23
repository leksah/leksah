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
import IDE.Pane.Errors (showErrors')
import IDE.Pane.Variables (showVariables')
import IDE.Pane.Breakpoints (showBreakpointList')
import Control.Monad (when)

showDebugger :: IDEAction
showDebugger = do
    pp   <- panePathForGroup "*Debug"
    ret  <- newGroupOrBringToFront "Debug" pp
    bufs <- allBuffers
    case ret of
        (Just rpp, True) -> do
            viewSplit' rpp Horizontal
            let lowerP =  rpp ++ [SplitP BottomP]
            let upperP =  rpp ++ [SplitP TopP]
            lower <- getNotebook lowerP
            upper <- getNotebook upperP
            liftIO $ do
                notebookSetTabPos lower PosRight
                notebookSetTabPos upper PosRight
                notebookSetShowTabs upper False
            showBreakpointList' (rpp ++ [SplitP BottomP])
            showErrors' (rpp ++ [SplitP BottomP])
            showVariables' (rpp ++ [SplitP BottomP])
            when (null $ filter (\b -> bufferName b == "_LeksahEval.hs") bufs) $
                newTextBuffer (rpp ++ [SplitP TopP]) "_LeksahEval.hs" Nothing >> return ()
            return ()
        (Just rpp, False) -> do
            let lowerP =  rpp ++ [SplitP BottomP]
            let upperP =  rpp ++ [SplitP TopP]
            showBreakpointList' lowerP
            showErrors' lowerP
            showVariables' lowerP
            when (null $ filter (\b -> bufferName b == "_LeksahEval.hs") bufs) $
                newTextBuffer upperP "_LeksahEval.hs" Nothing >> return ()
            return ()
        _ -> return ()
