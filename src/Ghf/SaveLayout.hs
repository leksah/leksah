--
-- | Module for saving and recovering the layout
--

module Ghf.SaveLayout (
    saveLayout
,   recoverLayout
) where

import Graphics.UI.Gtk hiding (showLayout)
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec hiding(Parser)
import Control.Monad.Reader
--import System.FilePath
import qualified Data.Map as Map

import Ghf.Log
import Ghf.Core
import Ghf.ViewFrame
import Ghf.SourceEditor
import Ghf.File
import Ghf.PrinterParser
import qualified Text.PrettyPrint.HughesPJ as PP
import Ghf.PropertyEditor

data LayoutState = LayoutState {
        layoutS             ::   PaneLayout
    ,   population          ::   [(String,PanePath)]
    ,   windowSize          ::   (Int,Int)
} deriving()

defaultLayout = LayoutState {
        layoutS             =   TerminalP (Just TopP)
    ,   population          =   [("*Log",[])]
    ,   windowSize          =   (1024,768)}

layoutDescr :: [FieldDescriptionS LayoutState]
layoutDescr = [
        mkFieldS (emptyParams
            {   paraName = Just "Layout"})
            (PP.text . show)
            readParser
            layoutS
            (\ b a -> a{layoutS = b})
    ,   mkFieldS (emptyParams
            {   paraName = Just "Population"})
            (PP.text . show)
            readParser
            population
            (\ b a -> a{population = b})
    ,   mkFieldS (emptyParams
            {   paraName = Just "Window size"})
            (PP.text . show)
            (pairParser intParser)
            windowSize
            (\(c,d) a -> a{windowSize = (c,d)})]

--
-- | Get and save the current layout
--
saveLayout :: GhfAction
saveLayout = do
    wdw         <-  readGhf window
    layout      <-  getLayout
    population  <-  getPopulation
    layoutPath  <-  lift $getConfigFilePathForSave "Current.layout"
    size        <-  lift $windowGetSize wdw
    lift $writeLayout layoutPath $LayoutState layout population size

writeLayout :: FilePath -> LayoutState -> IO ()
writeLayout fpath ls = writeFile fpath (showLayout ls layoutDescr)

showLayout ::  a ->  [FieldDescriptionS a] ->  String
showLayout prefs prefsDesc = PP.render $
    foldl (\ doc (FDS _ printer _) ->  doc PP.$+$ printer prefs) PP.empty prefsDesc

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


-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

--
-- | Read and apply the saved layout
--

recoverLayout :: GhfAction
recoverLayout = do
    wdw         <-  readGhf window
    layoutSt <- lift$ readLayout
    lift $windowSetDefaultSize wdw (fst (windowSize layoutSt))(snd (windowSize layoutSt))
    applyLayout (layoutS layoutSt)
    populate (population layoutSt)


readLayout :: IO LayoutState
readLayout = do
    layoutPath  <-  getConfigFilePathForLoad "Current.layout"
    res <- parseFromFile (prefsParser defaultLayout layoutDescr) layoutPath
    case res of
        Left pe -> error $"Error reading prefs file " ++ show layoutPath ++ " " ++ show pe
        Right r -> return r

prefsParser ::  a ->  [FieldDescriptionS a] ->  CharParser () a
prefsParser def descriptions =
    let parsersF = map fieldParser descriptions in do
        whiteSpace
        res <-  applyFieldParsers def parsersF
        return res
        <?> "layout parser"


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





