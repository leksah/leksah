{-# OPTIONS_GHC -XTypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.SaveSession
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | Module for saving and recovering the layout
--
---------------------------------------------------------------------------------


module IDE.SaveSession (
    saveSession
,   saveSessionAs
,   saveSessionAsPrompt
,   recoverSession
,   sessionClosePane
,   loadSession
,   loadSessionPrompt
,   standardSessionFilename
) where

import Graphics.UI.Gtk hiding (showLayout)
import Text.ParserCombinators.Parsec hiding(Parser)
import Control.Monad.Reader
import System.FilePath
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable

import IDE.Core.State
import IDE.FileUtils
import IDE.PrinterParser
import qualified Text.PrettyPrint.HughesPJ as PP
import Graphics.UI.Editor.Parameters
import IDE.Pane.Modules
import IDE.Pane.SourceBuffer
import IDE.Pane.Info
import IDE.Pane.References
import IDE.Pane.Log
import IDE.Pane.Preferences
import IDE.Pane.PackageFlags
import IDE.Pane.Search
import IDE.Find
import System.Time (getClockTime)
import IDE.Package (activatePackage,deactivatePackage)
import Data.List (foldl')
import IDE.Pane.Debugger (DebuggerState(..))

-- ---------------------------------------------------------------------
-- All pane types must be in here !
--

data PaneState      =   BufferSt BufferState
                    |   LogSt LogState
                    |   InfoSt InfoState
                    |   ModulesSt ModulesState
                    |   ReferencesSt ReferencesState
                    |   PrefsSt PrefsState
                    |   FlagsSt FlagsState
                    |   SearchSt SearchState
                    |   DebuggerSt DebuggerState
    deriving(Eq,Ord,Read,Show)

asPaneState :: RecoverablePane alpha beta gamma => beta -> PaneState
asPaneState s | isJust ((cast s) :: Maybe BufferState)   =   BufferSt (fromJust $ cast s)
asPaneState s | isJust ((cast s) :: Maybe LogState)      =   LogSt (fromJust $ cast s)
asPaneState s | isJust ((cast s) :: Maybe InfoState)     =   InfoSt (fromJust $ cast s)
asPaneState s | isJust ((cast s) :: Maybe ModulesState)  =   ModulesSt (fromJust $ cast s)
asPaneState s | isJust ((cast s) :: Maybe ReferencesState)  =   ReferencesSt (fromJust $ cast s)
asPaneState s | isJust ((cast s) :: Maybe PrefsState)    =   PrefsSt (fromJust $ cast s)
asPaneState s | isJust ((cast s) :: Maybe FlagsState)    =   FlagsSt (fromJust $ cast s)
asPaneState s | isJust ((cast s) :: Maybe SearchState)   =   SearchSt (fromJust $ cast s)
asPaneState s | isJust ((cast s) :: Maybe DebuggerState) =   DebuggerSt (fromJust $ cast s)
asPaneState s                                            =   error "SaveSession>>asPaneState incomplete cast"

recover :: PanePath -> PaneState -> IDEAction
recover pp (BufferSt p)         =   recoverState pp p
recover pp (LogSt p)            =   recoverState pp p
recover pp (InfoSt p)           =   recoverState pp p
recover pp (ModulesSt p)        =   recoverState pp p
recover pp (ReferencesSt p)     =   recoverState pp p
recover pp (PrefsSt p)          =   recoverState pp p
recover pp (FlagsSt p)          =   recoverState pp p
recover pp (SearchSt p)         =   recoverState pp p
recover pp (DebuggerSt p)       =   recoverState pp p

-- ---------------------------------------------------------------------

standardSessionFilename =   "Current.session"

-- | *Implementation

sessionClosePane :: IDEAction
sessionClosePane = do
    activePane'     <-  readIDE activePane
    case activePane' of
        Nothing     ->  return ()
        Just (pn,_) ->  do
            (PaneC p) <- paneFromName pn
            close p

data SessionState = SessionState {
        saveTime            ::   String
    ,   layoutS             ::   PaneLayout
    ,   population          ::   [(Maybe PaneState,PanePath)]
    ,   windowSize          ::   (Int,Int)
    ,   activePackage       ::   Maybe FilePath
    ,   activePaneN         ::   Maybe String
    ,   toolbarVisibleS     ::   Bool
    ,   findbarState        ::   (Bool,FindState)
    ,   recentOpenedFiles   ::   [FilePath]
    ,   recentOpenedPackages  ::   [FilePath]
} deriving()

defaultLayout = SessionState {
        saveTime            =   ""
    ,   layoutS             =   TerminalP (Just TopP) (-1)
    ,   population          =   []
    ,   windowSize          =   (1024,768)
    ,   activePackage       =   Nothing
    ,   activePaneN         =   Nothing
    ,   toolbarVisibleS     =   True
    ,   findbarState        =   (False,FindState{
            entryStr        =   ""
        ,   entryHist       =   []
        ,   replaceStr      =   ""
        ,   replaceHist     =   []
        ,   caseSensitive   =   False
        ,   entireWord      =   False
        ,   wrapAround      =   True
        ,   backward        =   False
        ,   lineNr          =   1})
    ,   recentOpenedFiles   =   []
    ,   recentOpenedPackages  =   []
}

layoutDescr :: [FieldDescriptionS SessionState]
layoutDescr = [
        mkFieldS
            (paraName <<<- ParaName "Time of storage" $ emptyParams)
            (PP.text . show)
            stringParser
            saveTime
            (\ b a -> a{saveTime = b})
    ,   mkFieldS
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
            (\fp a -> a{activePackage = fp})
    ,   mkFieldS
            (paraName <<<- ParaName "Active pane" $ emptyParams)
            (PP.text . show)
            readParser
            activePaneN
            (\fp a -> a{activePaneN = fp})
    ,   mkFieldS
            (paraName <<<- ParaName "Toolbar visible" $ emptyParams)
            (PP.text . show)
            readParser
            toolbarVisibleS
            (\fp a -> a{toolbarVisibleS = fp})
    ,   mkFieldS
            (paraName <<<- ParaName "FindbarState" $ emptyParams)
            (PP.text . show)
            readParser
            findbarState
            (\fp a -> a{findbarState = fp})
    ,   mkFieldS
            (paraName <<<- ParaName "Recently opened files" $ emptyParams)
            (PP.text . show)
            readParser
            recentOpenedFiles
            (\fp a -> a{recentOpenedFiles = fp})
    ,   mkFieldS
            (paraName <<<- ParaName "Recently opened packages" $ emptyParams)
            (PP.text . show)
            readParser
            recentOpenedPackages
            (\fp a -> a{recentOpenedPackages = fp})]

--
-- | Get and save the current layout
--
saveSession :: IDEAction
saveSession = do
    sessionPath  <-  liftIO $ getConfigFilePathForSave standardSessionFilename
    saveSessionAs sessionPath

saveSessionAs :: FilePath -> IDEAction
saveSessionAs sessionPath = do
    forget          <- getForgetSession
    if forget
        then ideMessage Normal "Forget this session"
        else do
            sysMessage Normal "Now saving session"
            wdw             <-  readIDE window
            layout          <-  getLayout
            population      <-  getPopulation
            size            <-  liftIO $ windowGetSize wdw
            active          <-  getActive
            activePane'     <-  readIDE activePane
            let activeP =   case activePane' of
                                Nothing -> Nothing
                                Just (s,_) -> Just s
            (toolbarVisible,_)  <- readIDE toolbar
            findState       <- getFindState
            (findbarVisible,_)  <- readIDE findbar
            timeNow         <- liftIO getClockTime
            recentFiles'      <- readIDE recentFiles
            recentPackages'   <- readIDE recentPackages
            liftIO $ writeLayout sessionPath (SessionState {
                saveTime            =   show timeNow
            ,   layoutS             =   layout
            ,   population          =   population
            ,   windowSize          =   size
            ,   activePackage       =   active
            ,   activePaneN         =   activeP
            ,   toolbarVisibleS     =   toolbarVisible
            ,   findbarState        =   (findbarVisible,findState)
            ,   recentOpenedFiles   =   recentFiles'
            ,   recentOpenedPackages=   recentPackages'})

saveSessionAsPrompt :: IDEAction
saveSessionAsPrompt = do
    window' <- readIDE window
    response <- liftIO $ do
        configFolder <- getConfigDir
        dialog <- fileChooserDialogNew
                  (Just $ "Save Session as")
                  (Just window')
    	      FileChooserActionSave
    	      [("gtk-cancel"
    	       ,ResponseCancel)
    	      ,("gtk-save"
	          , ResponseAccept)]
        fileChooserSetCurrentFolder dialog configFolder
        widgetShow dialog
        res <- dialogRun dialog
        case res of
            ResponseAccept  ->  do
                fileName <- fileChooserGetFilename dialog
                widgetHide dialog
                return fileName
            _               ->  do
                widgetHide dialog
                return Nothing
    case response of
        Just fn -> saveSessionAs fn
        Nothing -> return ()

loadSessionPrompt :: IDEAction
loadSessionPrompt = do
    window' <- readIDE window
    response <- liftIO $ do
        configFolder <- getConfigDir
        dialog <- fileChooserDialogNew
                  (Just $ "Select session file")
                  (Just window')
    	      FileChooserActionOpen
    	      [("gtk-cancel"
    	       ,ResponseCancel)
    	      ,("gtk-open"
    	       , ResponseAccept)]
        fileChooserSetCurrentFolder dialog configFolder
        widgetShow dialog
        res <- dialogRun dialog
        case res of
            ResponseAccept  ->  do
                fileName <- fileChooserGetFilename dialog
                widgetHide dialog
                return fileName
            _               ->  do
                widgetHide dialog
                return Nothing
    case response of
        Just fn -> loadSession fn
        Nothing -> return ()

loadSession :: FilePath -> IDEAction
loadSession sessionPath = do
    saveSession :: IDEAction
    deactivatePackage
    recentFiles'      <- readIDE recentFiles
    recentPackages'   <- readIDE recentPackages
    b <- fileCloseAll
    if b
        then do
            paneCloseAll
            viewCollapseAll
            recoverSession sessionPath
            modifyIDE_ (\ide -> return ide{recentFiles = recentFiles', recentPackages = recentPackages'})
            return ()
        else return ()

paneCloseAll :: IDEAction
paneCloseAll = do
    panes' <- readIDE panes
    mapM_ (\ (PaneC p) -> close p) (Map.elems panes')

viewCollapseAll :: IDEAction
viewCollapseAll = do
    layout' <- readIDE layout
    case layout' of
        TerminalP _ _ -> return ()
        VerticalP _ _ _ -> viewCollapse' [LeftP]
        HorizontalP _ _ _ -> viewCollapse' [TopP]

writeLayout :: FilePath -> SessionState -> IO ()
writeLayout fpath ls = writeFile fpath (showLayout ls layoutDescr)

showLayout ::  a ->  [FieldDescriptionS a] ->  String
showLayout prefs prefsDesc = PP.render $
    foldl' (\ doc (FDS _ printer _) ->  doc PP.$+$ printer prefs) PP.empty prefsDesc

getLayout :: IDEM(PaneLayout)
getLayout = do
    rawLayout <- readIDE layout
    getLayout' rawLayout []
    where
    getLayout' (HorizontalP l r _) pp = do
        l2          <-  getLayout' l (pp ++ [TopP])
        r2          <-  getLayout' r (pp ++ [BottomP])
        pane        <-  getPaned pp
        pos         <-  liftIO $ panedGetPosition pane
        return (HorizontalP l2 r2 pos)
    getLayout' (VerticalP l r _) pp = do
        l2          <-  getLayout' l (pp ++ [LeftP])
        r2          <-  getLayout' r (pp ++ [RightP])
        pane        <-  getPaned pp
        pos         <-  liftIO $ panedGetPosition pane
        return (VerticalP l2 r2 pos)
    getLayout' (TerminalP _ _) pp = do
        nb          <-  getNotebook pp
        showTabs    <-  liftIO $ notebookGetShowTabs nb
        pos         <-  liftIO $ notebookGetTabPos nb
        current     <-  liftIO $ notebookGetCurrentPage nb
        return (TerminalP (if showTabs
                                then Just (posTypeToPaneDirection pos)
                                else Nothing) current)

getPopulation :: IDEM[(Maybe PaneState,PanePath)]
getPopulation = do
    paneMap <- readIDE paneMap
    mapM (\ (pn,v) -> do
        (PaneC p) <- paneFromName pn
        mbSt <- saveState p
        case mbSt of
            Nothing -> return (Nothing, fst v)
            Just st -> return (Just (asPaneState st), fst v))
                $ Map.toList paneMap

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

recoverSession :: FilePath -> IDEM (Bool,Bool)
recoverSession sessionPath = do
    wdw         <-  readIDE window
    sessionSt    <- liftIO $ readLayout sessionPath
    liftIO $ windowSetDefaultSize wdw (fst (windowSize sessionSt))(snd (windowSize sessionSt))
    applyLayout (layoutS sessionSt)
    case activePackage sessionSt of
        Just fp -> activatePackage fp >> return ()
        Nothing -> return ()
    populate (population sessionSt)
    setCurrentPages (layoutS sessionSt)
    when (isJust (activePaneN sessionSt)) $ do
        mbPane <- mbPaneFromName (fromJust (activePaneN sessionSt))
        case mbPane of
            Nothing -> return ()
            Just (PaneC p) -> makeActive p
    setFindState ((snd . findbarState) sessionSt)
    if toolbarVisibleS sessionSt
        then showToolbar
        else hideToolbar
    if (fst . findbarState) sessionSt
        then showFindbar
        else hideFindbar
    modifyIDE_ (\ide -> return ide{recentFiles = recentOpenedFiles sessionSt,
                                    recentPackages = recentOpenedPackages sessionSt})
    return (toolbarVisibleS sessionSt, (fst . findbarState) sessionSt)

readLayout :: FilePath -> IO SessionState
readLayout sessionPath = do
    res <- parseFromFile (prefsParser defaultLayout layoutDescr) sessionPath
    case res of
        Left pe -> throwIDE $"Error reading session file " ++ show sessionPath ++ " " ++ show pe
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
        TerminalP _ _ ->   applyLayout' layoutS []
        otherwise     ->   throwIDE "apply Layout can only be allied to empty Layout"
    where
    applyLayout' (TerminalP Nothing _) pp  = do
        nb          <-  getNotebook pp
        liftIO $notebookSetShowTabs nb False
    applyLayout' (TerminalP (Just p) _) pp = do
        nb          <-  getNotebook pp
        liftIO $notebookSetShowTabs nb True
        liftIO $notebookSetTabPos nb (paneDirectionToPosType p)
    applyLayout' (VerticalP l r pos) pp = do
        viewSplit' pp Vertical
        pane        <-  getPaned pp
        liftIO $panedSetPosition pane pos
        applyLayout' l (pp ++ [LeftP])
        applyLayout' r (pp ++ [RightP])
    applyLayout' (HorizontalP t b pos) pp = do
        viewSplit' pp Horizontal
        pane        <-  getPaned pp
        liftIO $panedSetPosition pane pos
        applyLayout' t (pp ++ [TopP])
        applyLayout' b (pp ++ [BottomP])

populate :: [(Maybe PaneState,PanePath)] -> IDEAction
populate = mapM_ (\ (mbPs,pp) ->
            case mbPs of
                Nothing -> return ()
                Just s ->  recover pp s)

setCurrentPages :: PaneLayout -> IDEAction
setCurrentPages layout = setCurrentPages' layout []
    where
    setCurrentPages' (HorizontalP t b _) p  =   do  setCurrentPages' t (TopP : p)
                                                    setCurrentPages' b (BottomP : p)
    setCurrentPages' (VerticalP l r _) p    =   do  setCurrentPages' l (LeftP : p)
                                                    setCurrentPages' r (RightP : p)
    setCurrentPages' (TerminalP _ ind) p    =   when (ind >=  0) $ do
                                                    nb <- getNotebook (reverse p)
                                                    liftIO $ notebookSetCurrentPage nb ind




