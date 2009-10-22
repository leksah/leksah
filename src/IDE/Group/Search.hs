-----------------------------------------------------------------------------
--
-- Module      :  IDE.Group.Search
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

module IDE.Group.Search (

    showSearchGroup

) where

import IDE.Core.State
    (viewSplit',
     newGroupOrBringToFront,
     getBestPanePath,
     IDEM(..),
     getNotebook,
     readIDE,
     IDEAction(..))
import Graphics.UI.Frame.Panes
    (getOrBuildPane,
     PaneDirection(..),
     PanePathElement(..),
     layout,
     panePathForGroup)
import Control.Monad (liftM)
import IDE.Core.Types (frameState)
import Graphics.UI.Editor.Parameters (Direction(..))
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk (notebookSetShowTabs, notebookSetTabPos)
import Graphics.UI.Gtk.General.Enums (PositionType(..))
import IDE.Pane.Search (IDESearch(..))
import IDE.Pane.Grep (IDEGrep(..))


showSearchGroup :: IDEAction
showSearchGroup = do
    pp   <- panePathForGroup "*Search"
    ret  <- newGroupOrBringToFront "Search" pp
    layout' <- liftM layout (readIDE frameState)
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
                notebookSetShowTabs lower False
            getOrBuildPane (Left upperP) :: IDEM (Maybe IDESearch)
            getOrBuildPane (Left lowerP) :: IDEM (Maybe IDEGrep)
            return ()
        (Just rpp, False) -> do
            let lowerP  =  getBestPanePath (rpp ++ [SplitP BottomP]) layout'
            let upperP  =  getBestPanePath (rpp ++ [SplitP TopP]) layout'
            getOrBuildPane (Left upperP) :: IDEM (Maybe IDESearch)
            getOrBuildPane (Left lowerP) :: IDEM (Maybe IDEGrep)
            return ()
        _ -> return ()
