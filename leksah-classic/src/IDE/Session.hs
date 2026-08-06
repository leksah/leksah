{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Session
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

module IDE.Session (
    saveSession
,   saveSessionAs
,   saveSessionAsPrompt
,   recoverSession
,   sessionClosePane
,   loadSession
,   loadSessionPrompt
,   viewFullScreen
) where

import Prelude ()
import Prelude.Compat
import System.FilePath
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import qualified Data.Set as Set
import Control.Lens ((^.), (.~))

import IDE.Core.State
       (IDEAction,
       IDEM, standardSessionFilename, readIDE, workspace, wsFile,
       leksahSessionFileExtension, ideGtk, sysMessage, MessageLevel(..),
       recentFiles, recentWorkspaces, modifyIDE_, catchIDE, throwIDE)
import IDE.Gtk.State
       (RecoverablePane, PanePath, IDEPane(..), PaneLayout(..),
       recoverState, getActivePane, paneFromName, closePane, PaneDirection(..),
       PanePathElement(..), getMainWindow,
       completion, getMRUPanes, toolbar, findbar,
       getWindows, getPanesSt, getLayout, closeGroup, allGroupNames,
       viewCollapse', getPaned, getNotebook, posTypeToPaneDirection, getPaneMapSt,
       saveState, mbPaneFromName, makeActive,
       viewNest', viewDetach', paneDirectionToPosType, viewSplit',
       getActiveWindow)
import IDE.Utils.GUIUtils (chooseSaveFile, getFullScreenState, setFullScreenState, __)
import IDE.Utils.FileUtils (getConfigDir, getConfigFilePathForSave)
import Graphics.UI.Editor.Parameters (dialogRun', dialogAddButton')
import IDE.TextEditor (getBuffer, setModified)
import IDE.Pane.Modules (ModulesState)
import IDE.Pane.SourceBuffer
       (fileCloseAll, allBuffers, IDEBuffer(..), sourceView, BufferState)
import IDE.Pane.Info (InfoState(..))
import IDE.Pane.Log (LogState(..))
import IDE.Pane.PackageFlags (FlagsState)
import IDE.Pane.Search (SearchState)
import IDE.Pane.Grep (GrepState)
import IDE.Pane.HLint (HLintState)
import IDE.Pane.WebKit.Documentation (DocumentationState)
import IDE.Pane.WebKit.Output (OutputState)
import IDE.Pane.WebKit.Inspect (InspectState)
import IDE.Pane.Files (FilesState)
import IDE.Pane.Breakpoints (BreakpointsState)
import IDE.Pane.Trace (TraceState)
import IDE.Pane.Variables (VariablesState)
import IDE.Find
       (hideFindbar, showFindbar, hideToolbar, showToolbar, setFindState,
        getFindState, FindState(..))
import System.Time (getClockTime)
import IDE.Package (deactivatePackage)
import IDE.Pane.Errors (ErrorsState)
import Control.Exception (catch, SomeException(..))
import IDE.Pane.Workspace (WorkspaceState(..))
import IDE.Gtk.Workspaces (workspaceOpenThis)
import IDE.Completion (setCompletionSize)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad ((>=>), void, when)
import System.Log.Logger (errorM, debugM)
import Data.Text (Text)
import Data.Traversable (forM)
import Data.Foldable (forM_)
import IDE.Gtk.Preferences (applyInterfaceTheme)
import GI.Gtk.Objects.Window
       (windowUnfullscreen, windowFullscreen, windowSetDefaultSize,
        Window(..), windowSetTransientFor, setWindowTitle, windowGetSize)
import GI.Gtk.Enums
       (Orientation(..), ResponseType(..), FileChooserAction(..))
import GI.Gtk.Objects.FileChooserDialog (FileChooserDialog(..))
import GI.Gtk.Interfaces.FileChooser
       (fileChooserGetFilename, fileChooserSetCurrentFolder,
        fileChooserSetAction)
import GI.Gtk.Objects.Widget
       (widgetShowAll, widgetGetParent, widgetDestroy, widgetHide,
        widgetShow)
import GI.Gtk.Objects.Paned (panedSetPosition, panedGetPosition)
import GI.Gtk.Objects.Notebook
       (notebookSetCurrentPage, notebookSetTabPos, notebookSetShowTabs,
        notebookGetCurrentPage, notebookGetTabPos, notebookGetShowTabs)
import Data.GI.Base (unsafeCastTo)
import Data.GI.Base.GObject (new')
import Control.Arrow (Arrow(..))
import qualified Data.Text as T (unpack, pack)
import GI.Gtk (constructDialogUseHeaderBar)
import qualified Data.ByteString.Lazy as LBS (readFile, writeFile)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------
-- This needs to be incremented, when the session format changes
--
theSessionVersion :: Int
theSessionVersion = 3

-- ---------------------------------------------------------------------
-- All pane types must be in here !
--

data PaneState      =   BufferSt BufferState
                    |   LogSt LogState
                    |   InfoSt InfoState
                    |   ModulesSt ModulesState
                    |   FlagsSt FlagsState
                    |   SearchSt SearchState
                    |   FilesSt FilesState
                    |   GrepSt GrepState
                    |   HLintSt HLintState
                    |   DocumentationSt DocumentationState
                    |   OutputSt OutputState
                    |   InspectSt InspectState
                    |   BreakpointsSt BreakpointsState
                    |   TraceSt TraceState
                    |   VariablesSt VariablesState
                    |   ErrorsSt ErrorsState
                    |   WorkspaceSt WorkspaceState
    deriving(Eq, Ord, Read, Show, Generic)

instance ToJSON PaneState
instance FromJSON PaneState

asPaneState :: RecoverablePane alpha beta gamma => beta -> PaneState
asPaneState s | isJust (cast s :: Maybe BufferState)      =   BufferSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe LogState)         =   LogSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe InfoState)        =   InfoSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe ModulesState)     =   ModulesSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe FlagsState)       =   FlagsSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe SearchState)      =   SearchSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe FilesState)       =   FilesSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe GrepState)        =   GrepSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe HLintState)       =   HLintSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe DocumentationState) = DocumentationSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe OutputState)      =   OutputSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe InspectState)     =   InspectSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe BreakpointsState) =   BreakpointsSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe TraceState)       =   TraceSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe VariablesState)   =   VariablesSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe ErrorsState)      =   ErrorsSt (fromJust $ cast s)
asPaneState s | isJust (cast s :: Maybe WorkspaceState)   =   WorkspaceSt (fromJust $ cast s)
asPaneState _                                             =   error "SaveSession>>asPaneState incomplete cast"

recover :: PanePath -> PaneState -> IDEAction
recover pp (BufferSt p)         =   void (recoverState pp p)
recover pp (LogSt p)            =   void (recoverState pp p)
recover pp (InfoSt p)           =   void (recoverState pp p)
recover pp (ModulesSt p)        =   void (recoverState pp p)
recover pp (FlagsSt p)          =   void (recoverState pp p)
recover pp (SearchSt p)         =   void (recoverState pp p)
recover pp (FilesSt p)          =   void (recoverState pp p)
recover pp (GrepSt p)           =   void (recoverState pp p)
recover pp (HLintSt p)          =   void (recoverState pp p)
recover pp (DocumentationSt p)  =   void (recoverState pp p)
recover pp (OutputSt p)         =   void (recoverState pp p)
recover pp (InspectSt p)        =   void (recoverState pp p)
recover pp (BreakpointsSt p)    =   void (recoverState pp p)
recover pp (TraceSt p)          =   void (recoverState pp p)
recover pp (VariablesSt p)      =   void (recoverState pp p)
recover pp (ErrorsSt p)         =   void (recoverState pp p)
recover pp (WorkspaceSt p)      =   void (recoverState pp p)

-- ---------------------------------------------------------------------



-- | *Implementation

sessionClosePane :: IDEAction
sessionClosePane =
    getActivePane >>= \case
        (Nothing, _)     ->  return ()
        (Just (pn,_), _) ->  do
            (PaneC p) <- paneFromName pn
            void $ closePane p

data SessionState = SessionState {
        sessionVersion      ::   Int
    ,   saveTime            ::   Text
    ,   layoutS             ::   PaneLayout
    ,   population          ::   [(Maybe PaneState,PanePath)]
    ,   windowSize          ::   (Int,Int)
    ,   fullScreen          ::   Bool
    ,   completionSize      ::   (Int,Int)
    ,   workspacePath       ::   Maybe FilePath
    ,   activePaneN         ::   [Text]
    ,   toolbarVisibleS     ::   Bool
    ,   findbarState        ::   (Bool,FindState)
    ,   recentOpenedFiles   ::   [FilePath]
    ,   recentOpenedWorksp  ::   [FilePath]
} deriving(Eq, Show, Generic)

instance ToJSON SessionState
instance FromJSON SessionState

defaultSession :: SessionState
defaultSession = SessionState {
        sessionVersion      =   theSessionVersion
    ,   saveTime            =   ""
    ,   layoutS             =   VerticalP
                                    TerminalP {
                                        paneGroups = Map.fromList []
                                      , paneTabs = Just TopP
                                      , currentPage = -1
                                      , detachedId = Nothing
                                      , detachedSize = Nothing}
                                    (HorizontalP
                                        TerminalP {
                                            paneGroups = Map.fromList []
                                          , paneTabs = Just TopP
                                          , currentPage = -1
                                          , detachedId = Nothing
                                          , detachedSize = Nothing}
                                        TerminalP {
                                            paneGroups = Map.fromList []
                                          , paneTabs = Just TopP
                                          , currentPage = -1
                                          , detachedId = Nothing
                                          , detachedSize = Nothing}
                                        456)
                                  250
    ,   population          =   [ (Just (WorkspaceSt WorkspaceState),[SplitP LeftP])

                                ]
    ,   windowSize          =   (1024,768)
    ,   fullScreen          =   False
    ,   completionSize      =   (750,400)
    ,   workspacePath       =   Nothing
    ,   activePaneN         =   []
    ,   toolbarVisibleS     =   True
    ,   findbarState        =   (False,FindState{
            entryStr        =   ""
        ,   entryHist       =   []
        ,   replaceStr      =   ""
        ,   replaceHist     =   []
        ,   caseSensitive   =   False
        ,   entireWord      =   False
        ,   wrapAround      =   True
        ,   regex           =   False
        ,   lineNr          =   1})
    ,   recentOpenedFiles   =   []
    ,   recentOpenedWorksp  =   []
}

--
-- | Get and save the current session
--
saveSession :: IDEAction
saveSession = do
    sessionPath    <- liftIO $ getConfigFilePathForSave standardSessionFilename
    mbSessionPath2 <-
        readIDE workspace >>= \case
            Nothing -> return Nothing
            Just ws -> return $ Just (dropExtension (ws ^. wsFile) ++
                                    leksahSessionFileExtension)
    saveSessionAs sessionPath mbSessionPath2

saveSessionAs :: FilePath -> Maybe FilePath ->  IDEAction
saveSessionAs sessionPath mbSecondPath = readIDE ideGtk >>= mapM_ (\gtk -> do
    sysMessage Normal (__ "Now saving session")
    bufs <- allBuffers
    case filter (\b -> bufferName b == "_Eval.hs") bufs of
        [IDEBuffer{sourceView = sv}] -> do
            ebuf <- getBuffer sv
            setModified ebuf False
        _     -> return ()
    wdw             <-  getMainWindow
    layoutS         <-  mkLayout
    population      <-  getPopulation
    size            <-  windowGetSize wdw
    fullScreen      <-  getFullScreenState
    let (completionSize,_) = gtk ^. completion
    mbWs            <-  readIDE workspace
    activePaneN     <-  getMRUPanes
    let (toolbarVisibleS,_) = gtk ^. toolbar
    findState       <- getFindState
    let (findbarVisible,_) = gtk ^. findbar
    timeNow         <- liftIO getClockTime
    recentOpenedFiles <- readIDE recentFiles
    recentOpenedWorksp <- readIDE recentWorkspaces
    let sessionVersion      =   theSessionVersion
        saveTime            =   T.pack $ show timeNow
        windowSize          =   (fromIntegral *** fromIntegral) size
        workspacePath       =   case mbWs of
                                    Nothing -> Nothing
                                    Just ws -> Just (ws ^. wsFile)
        findbarState        =   (findbarVisible,findState)
        state = SessionState {..}
    liftIO $ LBS.writeFile sessionPath $ encodePretty state
    forM_ mbSecondPath $ \secondPath ->
        liftIO $ LBS.writeFile secondPath $ encodePretty state)

saveSessionAsPrompt :: IDEAction
saveSessionAsPrompt = do
    window <- getMainWindow
    response <- liftIO $ do
        configFolder <- getConfigDir
        chooseSaveFile window (__ "Save Session as") (Just configFolder)
    case response of
        Just fn -> saveSessionAs (if takeExtension fn == leksahSessionFileExtension
                                    then fn
                                    else addExtension fn leksahSessionFileExtension)
                                    Nothing
        Nothing -> return ()

loadSessionPrompt :: IDEAction
loadSessionPrompt = do
    window' <- getMainWindow
    configFolder <- liftIO getConfigDir
    dialog <- new' FileChooserDialog [constructDialogUseHeaderBar 1]
    setWindowTitle dialog (__ "Select session file")
    windowSetTransientFor dialog $ Just window'
    fileChooserSetAction dialog FileChooserActionOpen
    _ <- dialogAddButton' dialog "gtk-cancel" ResponseTypeCancel
    _ <- dialogAddButton' dialog "Load Session" ResponseTypeAccept
    _ <- fileChooserSetCurrentFolder dialog configFolder
    widgetShow dialog
    res <- dialogRun' dialog
    case res of
        ResponseTypeAccept  ->  do
            fileName <- fileChooserGetFilename dialog
            widgetHide dialog
            mapM_ loadSession fileName
        _ -> widgetHide dialog

loadSession :: FilePath -> IDEAction
loadSession sessionPath = do
    liftIO $ debugM "leksah" "loadSession"
    saveSession :: IDEAction
    deactivatePackage
    recentFiles'      <- readIDE recentFiles
    recentWorkspaces' <- readIDE recentWorkspaces
    _ <- fileCloseAll (\_ -> return True)
    detachedCloseAll
    paneCloseAll
    groupsCloseAll
    viewCollapseAll
    _ <- recoverSession sessionPath
    modifyIDE_ $ (recentFiles .~ recentFiles')
               . (recentWorkspaces .~ recentWorkspaces')
    return ()

detachedCloseAll :: IDEAction
detachedCloseAll = do
    windows <- getWindows
    mapM_ widgetDestroy (tail windows)

paneCloseAll :: IDEAction
paneCloseAll = do
    panes' <- getPanesSt
    mapM_ (\ (PaneC p) -> closePane p) (Map.elems panes')

groupsCloseAll :: IDEAction
groupsCloseAll = do
    layout' <- getLayout
    mapM_ closeGroup (Set.toList $ allGroupNames layout')

viewCollapseAll :: IDEAction
viewCollapseAll = do
    layout' <- getLayout
    case layout' of
        TerminalP {}   -> return ()
        VerticalP {}   -> viewCollapse' [SplitP LeftP]
        HorizontalP {} -> viewCollapse' [SplitP TopP]

mkLayout :: IDEM PaneLayout
mkLayout = do
    rawLayout <- getLayout
    getLayout' rawLayout []
    where
    getLayout' (HorizontalP l r _) pp = do
        l2          <-  getLayout' l (pp ++ [SplitP TopP])
        r2          <-  getLayout' r (pp ++ [SplitP BottomP])
        pane        <-  getPaned pp
        pos         <-  fromIntegral <$> panedGetPosition pane
        return (HorizontalP l2 r2 pos)
    getLayout' (VerticalP l r _) pp = do
        l2          <-  getLayout' l (pp ++ [SplitP LeftP])
        r2          <-  getLayout' r (pp ++ [SplitP RightP])
        pane        <-  getPaned pp
        pos         <-  fromIntegral <$> panedGetPosition pane
        return (VerticalP l2 r2 pos)
    getLayout' raw@TerminalP {paneGroups = groups} pp = do
        groups2     <-  forM (Map.toAscList groups) $ \(group, g) -> do
            l <- getLayout' g (pp ++ [GroupP group])
            return (group, l)
        nb          <-  getNotebook pp
        showTabs    <-  notebookGetShowTabs nb
        pos         <-  notebookGetTabPos nb
        current     <-  fromIntegral <$> notebookGetCurrentPage nb
        size <- case detachedId raw of
            Just _  -> do
                parent <- widgetGetParent nb >>= liftIO . unsafeCastTo Window . fromJust
                Just . (fromIntegral *** fromIntegral) <$> windowGetSize parent
            Nothing -> return $ detachedSize raw
        return raw {
                paneGroups   = Map.fromAscList groups2
            ,   paneTabs     = if showTabs then Just (posTypeToPaneDirection pos) else Nothing
            ,   currentPage  = current
            ,   detachedSize = size}

getPopulation :: IDEM[(Maybe PaneState,PanePath)]
getPopulation = do
    paneMap <- getPaneMapSt
    mapM (\ (pn,v) -> do
        liftIO $ debugM "leksah" $ "getPopulation calling saveState for " <> T.unpack pn
        (PaneC p) <- paneFromName pn
        mbSt <- saveState p
        case mbSt of
            Nothing -> return (Nothing, fst v)
            Just st -> return (Just (asPaneState st), fst v))
                $ Map.toList paneMap

--getActive :: IDEM(Maybe FilePath)
--getActive = do
--    active <- readIDE activePack
--    case active of
--        Nothing -> return Nothing
--        Just p -> return (Just (ipdCabalFile p))

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

--
-- | Read and apply the saved layout
--

recoverSession :: FilePath -> IDEM (Bool,Bool)
recoverSession sessionPath = catchIDE (do
        liftIO $ debugM "leksah" $ "recoverSession " <> sessionPath
        wdw         <-  getMainWindow
        sessionSt    <- liftIO $ catch
                            (eitherDecode <$> LBS.readFile sessionPath)
                            (\(e :: SomeException) -> return . Left $ show e) >>= \case
                                Left msg -> do
                                    errorM "leksah" $ "Error reading session file " <> sessionPath
                                                            <> " : " <> msg
                                    return defaultSession
                                Right s -> return s
        liftIO $ debugM "leksah" "recoverSession windowSetDefaultSize"
        uncurry (windowSetDefaultSize wdw) . (fromIntegral *** fromIntegral) $ windowSize sessionSt
        liftIO $ debugM "leksah" "recoverSession applyLayout"
        applyLayout (layoutS sessionSt)
        liftIO $ debugM "leksah" "recoverSession open workspace (if any)"
        forM_ (workspacePath sessionSt) (workspaceOpenThis False)
        liftIO $ debugM "leksah" "recoverSession calling populate"
        populate (population sessionSt)
        liftIO $ debugM "leksah" "recoverSession calling setCurrentPages"
        setCurrentPages (layoutS sessionSt)
        forM_ (reverse $ activePaneN sessionSt) $
            mbPaneFromName >=> mapM_ (\(PaneC p) -> makeActive p)
        liftIO $ debugM "leksah" "recoverSession setting up toolbars"
        setFindState ((snd . findbarState) sessionSt)
        if toolbarVisibleS sessionSt
            then showToolbar
            else hideToolbar
        if (fst . findbarState) sessionSt
            then showFindbar
            else hideFindbar
        uncurry setCompletionSize (completionSize sessionSt)
        modifyIDE_ $ (recentFiles .~ recentOpenedFiles sessionSt)
                   . (recentWorkspaces .~ recentOpenedWorksp sessionSt)
        setFullScreenState (fullScreen sessionSt)
        viewFullScreen
        applyInterfaceTheme
        liftIO $ debugM "leksah" "recoverSession done"
        return (toolbarVisibleS sessionSt, (fst . findbarState) sessionSt))
        (\ (e :: SomeException) -> do
            sysMessage Normal (T.pack $ show e)
            return (True,True))

applyLayout :: PaneLayout -> IDEAction
applyLayout layoutS = do
    old <- getLayout
    case old of
        TerminalP {} ->   applyLayout' layoutS []
        _            ->   throwIDE (__ "apply Layout can only be allied to empty Layout")
    where
    applyLayout' (TerminalP groups mbTabPos _ mbDetachedId mbDetachedSize) pp = do
        forM_ (Map.keys groups) $ \group -> viewNest' pp group
        nb          <-  getNotebook pp
        case (mbDetachedId, mbDetachedSize) of
            (Just id', Just (width, height)) -> do
                mbPair <- viewDetach' pp id'
                case mbPair of
                    Nothing     -> return ()
                    Just (win,_wid) -> do
                        widgetShowAll win
                        windowSetDefaultSize win (fromIntegral width) (fromIntegral height)
            _ -> return ()
        notebookSetShowTabs nb (isJust mbTabPos)
        case mbTabPos of
            Just p -> notebookSetTabPos nb (paneDirectionToPosType p)
            _      -> return ()
        forM_ (Map.toAscList groups) $ \(group, g) ->
            applyLayout' g (pp ++ [GroupP group])
    applyLayout' (VerticalP l r pos) pp = do
        viewSplit' pp OrientationVertical
        pane        <-  getPaned pp
        panedSetPosition pane (fromIntegral pos)
        applyLayout' l (pp ++ [SplitP LeftP])
        applyLayout' r (pp ++ [SplitP RightP])
    applyLayout' (HorizontalP t b pos) pp = do
        viewSplit' pp OrientationHorizontal
        pane        <-  getPaned pp
        panedSetPosition pane (fromIntegral pos)
        applyLayout' t (pp ++ [SplitP TopP])
        applyLayout' b (pp ++ [SplitP BottomP])

populate :: [(Maybe PaneState,PanePath)] -> IDEAction
populate = mapM_ (\ (mbPs,pp) -> forM_ mbPs (recover pp))

setCurrentPages :: PaneLayout -> IDEAction
setCurrentPages layout = setCurrentPages' layout []
    where
    setCurrentPages' (HorizontalP t b _) p  =   do  setCurrentPages' t (SplitP TopP : p)
                                                    setCurrentPages' b (SplitP BottomP : p)
    setCurrentPages' (VerticalP l r _) p    =   do  setCurrentPages' l (SplitP LeftP : p)
                                                    setCurrentPages' r (SplitP RightP : p)
    setCurrentPages' (TerminalP groups _ ind _ _) p  =  do
                                                    forM_ (Map.toAscList groups) $ \(group, g) ->
                                                        setCurrentPages' g (GroupP group : p)
                                                    when (ind >=  0) $ do
                                                        nb <- getNotebook (reverse p)
                                                        notebookSetCurrentPage nb (fromIntegral ind)

viewFullScreen :: IDEAction
viewFullScreen = do
    isFullScreen <- getFullScreenState
    mbWindow <- getActiveWindow
    case (mbWindow, isFullScreen) of
        (Nothing, _)         -> return ()
        (Just window, True)  -> windowFullscreen window
        (Just window, False) -> windowUnfullscreen window

