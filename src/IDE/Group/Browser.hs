-----------------------------------------------------------------------------
--
-- Module      :  IDE.Group.Browser
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

module IDE.Group.Browser (

    showBrowser

) where


import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk (notebookSetShowTabs, notebookSetTabPos)
import Graphics.UI.Gtk.General.Enums (PositionType(..))
import IDE.Core.State
import Graphics.UI.Editor.Parameters (Direction(..))
import IDE.Pane.Modules
import IDE.Pane.Info

showBrowser :: IDEAction
showBrowser = do
    pp   <- panePathForGroup "*Browser"
    ret  <- newGroupOrBringToFront "Browser" pp
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
            getOrBuildPane (Left upperP) :: IDEM (Maybe IDEModules)
            getOrBuildPane (Left lowerP) :: IDEM (Maybe IDEInfo)
            return ()
        (Just rpp, False) -> do
            let lowerP  =  getBestPanePath (rpp ++ [SplitP BottomP]) layout'
            let upperP  =  getBestPanePath (rpp ++ [SplitP TopP]) layout'
            getOrBuildPane (Left upperP) :: IDEM (Maybe IDEModules)
            getOrBuildPane (Left lowerP) :: IDEM (Maybe IDEInfo)
            return ()
        _ -> return ()
