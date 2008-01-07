{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.SaveSession
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
--
-- | Module for saving and recovering the layout
--
---------------------------------------------------------------------------------


module IDE.SaveSession (
    saveSession
,   recoverSession
,   sessionClosePane
) where

import Graphics.UI.Gtk hiding (showLayout)
import Text.ParserCombinators.Parsec hiding(Parser)
import Control.Monad.Reader
import System.FilePath
import qualified Data.Map as Map
import Data.Maybe

import IDE.Core.State
import IDE.Framework.ViewFrame
import IDE.Utils.File
import IDE.PrinterParser
import qualified Text.PrettyPrint.HughesPJ as PP
import IDE.Framework.Parameters
import IDE.Package
import IDE.RecoverPanes

sessionClosePane :: IDEAction
sessionClosePane = do
    activePane'     <-  readIDE activePane
    case activePane' of
        Nothing     ->  return ()
        Just (pn,_) ->  do
            p <- paneFromName pn
            close p

sessionFilename = "Current.session"

data SessionState = SessionState {
        layoutS             ::   PaneLayout
    ,   population          ::   [(Maybe PaneState,PanePath)]
    ,   windowSize          ::   (Int,Int)
    ,   activePackage        ::   Maybe FilePath
} deriving()

defaultLayout = SessionState {
        layoutS             =   TerminalP (Just TopP)
    ,   population          =   []
    ,   windowSize          =   (1024,768)
    ,   activePackage       =   Nothing}

layoutDescr :: [FieldDescriptionS SessionState]
layoutDescr = [
        mkFieldS
            (paraName <<<- ParaName "Layout" $ emptyParams)
            (PP.text . show)
            readParser
            layoutS
            (\ b a -> a{layoutS = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Population" $ emptyParams)
            (PP.text . show)
            readParser
            population
            (\ b a -> a{population = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Window size" $ emptyParams)
            (PP.text . show)
            (pairParser intParser)
            windowSize
            (\(c,d) a -> a{windowSize = (c,d)})
    ,   mkFieldS
            (paraName <<<- ParaName "Active package" $ emptyParams)
            (PP.text . show)
            readParser
            activePackage
            (\fp a -> a{activePackage = fp})]

--
-- | Get and save the current layout
--
saveSession :: IDEAction
saveSession = do
    lift $ putStrLn "Now saving session"
    wdw         <-  readIDE window
    layout      <-  getLayout
    population  <-  getPopulation
    layoutPath  <-  lift $getConfigFilePathForSave sessionFilename
    size        <-  lift $windowGetSize wdw
    active      <-  getActive
    lift $writeLayout layoutPath $SessionState layout population size active

writeLayout :: FilePath -> SessionState -> IO ()
writeLayout fpath ls = writeFile fpath (showLayout ls layoutDescr)

showLayout ::  a ->  [FieldDescriptionS a] ->  String
showLayout prefs prefsDesc = PP.render $
    foldl (\ doc (FDS _ printer _) ->  doc PP.$+$ printer prefs) PP.empty prefsDesc

getLayout :: IDEM(PaneLayout)
getLayout = do
    rawLayout <- readIDE layout
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

getPopulation :: IDEM[(Maybe PaneState,PanePath)]
getPopulation = do
    paneMap <- readIDE paneMap
    mapM (\ (pn,v) -> do
        p <- paneFromName pn
        mbSt <- saveState p
        let st = case mbSt of
                    Nothing -> Nothing
                    Just s  -> Just (toPaneState s)
        return (st, fst v))
                $Map.toList paneMap

getActive :: IDEM(Maybe String)
getActive = do
    active <- readIDE activePack
    case active of
        Nothing -> return Nothing
        Just p -> return (Just (cabalFile p))

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

--
-- | Read and apply the saved layout
--

recoverSession :: IDEAction
recoverSession = do
    wdw         <-  readIDE window
    layoutSt <- lift$ readLayout
    lift $windowSetDefaultSize wdw (fst (windowSize layoutSt))(snd (windowSize layoutSt))
    applyLayout (layoutS layoutSt)
    case activePackage layoutSt of
        Just fp -> activatePackage fp >> return ()
        Nothing -> return ()
    populate (population layoutSt)


readLayout :: IO SessionState
readLayout = do
    layoutPath  <-  getConfigFilePathForLoad sessionFilename
    res <- parseFromFile (prefsParser defaultLayout layoutDescr) layoutPath
    case res of
        Left pe -> error $"Error reading prefs file " ++ show layoutPath ++ " " ++ show pe
        Right r -> return r

prefsParser ::  a ->  [FieldDescriptionS a] ->  CharParser () a
prefsParser def descriptions =
    let parsersF = map fieldParser descriptions in do
        whiteSpace
        applyFieldParsers def parsersF
        <?> "layout parser"

applyLayout :: PaneLayout -> IDEAction
applyLayout layoutS = do
    old <- readIDE layout
    case old of
        TerminalP _ ->   applyLayout' layoutS []
        otherwise   ->   error "apply Layout can only be allied to empty Layout"
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

populate :: [(Maybe PaneState,PanePath)] -> IDEAction
populate = mapM_ (\ (mbPs,pp) ->
            case mbPs of
                Nothing -> return ()
                Just s ->  recoverState pp (paneStateToIDEState s))


