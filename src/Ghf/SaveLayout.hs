--
-- | Module for saving and recovering the layout
--

module Ghf.SaveLayout (
    saveLayout
,   recoverLayout
) where

import Graphics.UI.Gtk(panedGetPosition, panedSetPosition, notebookGetShowTabs,
		       notebookSetShowTabs, notebookGetTabPos, notebookSetTabPos)
import Graphics.UI.Gtk.Types()    -- Instances only
import Control.Monad.Reader(Monad(return), MonadTrans(..), mapM_)
import System.FilePath(takeFileName)
import qualified Data.Map as Map(Map.toList)
import Ghf.Log(initLog)
import Ghf.Core(PaneLayout(..), PanePath, PaneDirection(..), Direction(..),
		GhfAction, GhfM, Ghf(paneMap, layout), readGhf, posTypeToPaneDirection,
		paneDirectionToPosType, getBufferDescription)
import Ghf.Package()    -- Instances only
import Ghf.PackageEditor()    -- Instances only
import Ghf.ViewFrame(viewSplit', getNotebook, getPaned)
import Ghf.SourceEditor(newTextBuffer)
import Ghf.File(getConfigFilePathForLoad,getConfigFilePathForSave)

--
-- | Get and save the current layout
--
saveLayout :: GhfAction
saveLayout = do
    layout      <-  getLayout
    population  <-  getPopulation
    layoutPath  <-  lift $getConfigFilePathForSave "Current.layout"
    lift $writeFile layoutPath (show (layout,population))

getLayout :: GhfM(PaneLayout)
getLayout = do
    rawLayout <- readGhf layout
    getLayout' rawLayout []
    where
    getLayout' (HorizontalP l r _) pp = do
        l2          <-  getLayout' l (pp ++ [TopP])
        r2          <-  getLayout' r (pp ++ [BottomP])
        pane        <-  getPaned pp
        pos         <-  lift $panedGetPosition pane
        return (HorizontalP l2 r2 pos)
    getLayout' (VerticalP l r _) pp = do
        l2          <-  getLayout' l (pp ++ [LeftP])
        r2          <-  getLayout' r (pp ++ [RightP])
        pane        <-  getPaned pp
        pos         <-  lift $panedGetPosition pane
        return (VerticalP l2 r2 pos)
    getLayout' (TerminalP _) pp = do
        nb          <-  getNotebook pp
        showTabs    <-  lift $notebookGetShowTabs nb
        pos         <-  lift $notebookGetTabPos nb
        return (TerminalP (if showTabs
                                then Just (posTypeToPaneDirection pos)
                                else Nothing))

getPopulation :: GhfM[(String,PanePath)]
getPopulation = do
    paneMap' <- readGhf paneMap
    return (map (\ (k,v) -> (getBufferDescription k, fst v)) $Map.toList paneMap')

--
-- | Read and apply the saved layout
--

recoverLayout :: GhfAction
recoverLayout = do
    (layout,population) <- lift$ readLayout
    applyLayout layout
    populate population

readLayout :: IO (PaneLayout,[(String,PanePath)])
readLayout = do
    layoutPath  <-  getConfigFilePathForLoad "Current.layout"
    str         <-  readFile layoutPath
    return (read str)

applyLayout :: PaneLayout -> GhfAction
applyLayout layoutS = do
    old <- readGhf layout
    case old of
        TerminalP _ ->   applyLayout' layoutS []
        otherwise   ->   error "applyLayout can only be allied to empty Layout"
    where
    applyLayout' (TerminalP Nothing) pp  = do
        nb          <-  getNotebook pp
        lift $notebookSetShowTabs nb False
    applyLayout' (TerminalP (Just p)) pp = do
        nb          <-  getNotebook pp
        lift $notebookSetShowTabs nb True
        lift $notebookSetTabPos nb (paneDirectionToPosType p)
    applyLayout' (VerticalP l r pos) pp = do
        viewSplit' pp Vertical
        pane        <-  getPaned pp
        lift $panedSetPosition pane pos
        applyLayout' l (pp ++ [LeftP])
        applyLayout' r (pp ++ [RightP])
    applyLayout' (HorizontalP t b pos) pp = do
        viewSplit' pp Horizontal
        pane        <-  getPaned pp
        lift $panedSetPosition pane pos
        applyLayout' t (pp ++ [TopP])
        applyLayout' b (pp ++ [BottomP])

populate :: [(String,PanePath)] -> GhfAction
populate = mapM_ populate'
    where
    populate' ("*Log",pp) =  do nb <- getNotebook pp
                                initLog pp nb
    populate' ('?':n,pp)  =  newTextBuffer pp n Nothing
    populate' (n,pp)      =  newTextBuffer pp (takeFileName n) (Just n)





