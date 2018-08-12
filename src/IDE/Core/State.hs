{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.State
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | The core state of ide. This module is imported from every other module,
-- | and all data structures of the state are declared here, to avoid circular
-- | module dependencies.
--
-------------------------------------------------------------------------------

module IDE.Core.State (
    window
,   errorRefs
,   breakpointRefs
,   contextRefs
,   currentError
,   currentBreak
,   currentContext
,   setCurrentError
,   setCurrentBreak
,   setCurrentContext
,   isInterpreting

,   isStartingOrClosing

,   triggerEventIDE
,   triggerEventIDE_

,   deactivatePane

-- * Convenience methods for accesing the IDE State
,   readIDE
,   modifyIDE
,   modifyIDE_
,   withIDE
,   getIDE
,   throwIDE
,   packageDebugState

,   reifyIDE
,   reflectIDE
,   reflectIDEI
,   catchIDE
,   postSyncIDE'
,   postAsyncIDE'
,   postSyncIDE
,   postAsyncIDE
,   postSyncIDEIdle
,   postAsyncIDEIdle
,   onIDE
,   forkIDE

,   sysMessage
,   MessageLevel(..)
,   ideMessage
,   logMessage

,   withoutRecordingDo
--,   deactivatePane
--,   deactivatePaneIfActive
--,   closePane
,   changePackage
,   changeProject

,   liftYiControl
,   liftYi

,   leksahSubDir
,   leksahOrPackageDir
,   getDataDir
,   P.version

,   module Reexported

) where

import Prelude ()
import Prelude.Compat
import Data.IORef
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import IDE.Core.Types as Reexported
import Graphics.UI.Frame.Panes as Reexported
import Graphics.UI.Frame.ViewFrame as Reexported --hiding (notebookInsertOrdered)
import Control.Event
import System.IO
import Data.Maybe (isJust)
import System.FilePath
       (dropFileName, takeDirectory, (</>), takeFileName)
import IDE.Core.CTypes as Reexported
import Control.Concurrent (forkIO)
import IDE.Utils.Utils as Reexported
import qualified Data.Map as Map (empty, lookup)
import Data.Typeable(Typeable)
import qualified IDE.TextEditor.Yi.Config as Yi
import Data.Conduit (ConduitT, ($$))
import qualified Data.Conduit as C
       (transPipe, Sink, awaitForever, yield, leftover, ($$))
import qualified Data.Conduit.List as CL
       (sourceList)
import Control.Monad (void, liftM, when)
import Control.Monad.Trans.Reader (ask, ReaderT(..))
import qualified Paths_leksah as P
import System.Environment.Executable (getExecutablePath)
import System.Directory (doesDirectoryExist)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import qualified Data.Sequence as Seq (filter)
import Data.Sequence (Seq)
import GI.Gtk.Objects.Widget
       (noWidget, Widget(..), widgetDestroy, widgetShowAll, widgetSetName,
        widgetGrabFocus)
import GI.Gtk.Objects.Notebook
       (Notebook(..), notebookRemovePage, notebookPageNum)
import Data.Int (Int32)
import GI.GLib (pattern PRIORITY_DEFAULT_IDLE, pattern PRIORITY_DEFAULT, idleAdd)
import GI.Gtk.Objects.Label (noLabel)
import Data.Foldable (forM_)
import qualified Data.Map as M (lookup, member)
import IDE.Utils.Tool (ToolState(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Distribution.Compat.Exception (catchIO)
import System.Environment (getEnv)
import IDE.Utils.DebugUtils (traceTimeTaken)
import GHC.Stack (HasCallStack)
import Data.Void (Void)

instance PaneMonad IDEM where
    getFrameState   =   readIDE frameState
    setFrameState v =   modifyIDE_ (\ide -> ide{frameState = v})
    runInIO f       =   reifyIDE (\ideRef -> return (\v -> reflectIDE (f v) ideRef))
    panePathForGroup id =   do
        prefs  <- readIDE prefs
        case id `lookup` categoryForPane prefs of
            Just group -> case group `lookup` pathForCategory prefs of
                            Nothing -> return (defaultPath prefs)
                            Just p  -> return p
            Nothing    -> return (defaultPath prefs)
    getThisPane = getPanePrim
    -- getThisPane     ::  forall alpha beta . RecoverablePane alpha beta delta => Maybe PanePath -> delta alpha
    getOrBuildThisPane ePpoPid = do
        mbPane <- getPanePrim
        case mbPane of
            Nothing -> do
                pp          <-  case ePpoPid of
                                    Right pId  -> getBestPathForId pId
                                    Left ppp -> do
                                        layout      <- getLayout
                                        return (getBestPanePath ppp layout)
                nb          <-  getNotebook pp
                buildPane pp nb builder
            Just pane ->   return (Just pane)

    -- displayThisPane ::  Bool -> delta alpha
    displayThisPane pane shallGrabFocus = do
        bringPaneToFront pane
        when shallGrabFocus $ widgetGrabFocus =<< getTopWidget pane
    -- buildThisPane   ::  forall alpha beta . RecoverablePane alpha beta delta => PanePath ->
    --                    Notebook ->
    --                    (PanePath -> Notebook -> Window -> delta (alpha,Connections)) ->
    --                    delta alpha
    buildThisPane panePath notebook builder = do
        windows       <-  getWindows

        (mbBuf,cids)  <-  builder panePath notebook (head windows)
        case mbBuf of
            Nothing -> return Nothing
            Just buf -> do
                panes'          <-  getPanesSt
                paneMap'        <-  getPaneMapSt
                let b1 = case Map.lookup (paneName buf) paneMap' of
                            Nothing -> True
                            Just it -> False
                let b2 = case Map.lookup (paneName buf) panes' of
                            Nothing -> True
                            Just it -> False
                if b1 && b2
                    then do
                        topWidget <- getTopWidget buf
                        notebookInsertOrdered notebook topWidget (paneName buf) noLabel (paneTooltipText buf) False
                        addPaneAdmin buf cids panePath
                        widgetSetName topWidget (paneName buf)
                        widgetShowAll topWidget
                        widgetGrabFocus topWidget
                        bringPaneToFront buf
                        return (Just buf)
                    else return Nothing
    --activateThisPane :: forall alpha beta . RecoverablePane alpha beta delta => alpha -> Connections -> delta ()
    activateThisPane pane conn =
        getActivePane >>= \case
            (Just (pn,_), _) | pn == paneName pane -> return ()
            (mbActive, panes) -> do
                deactivatePaneWithout
                triggerEventIDE (StatusbarChanged [CompartmentPane (Just (PaneC pane))])
                bringPaneToFront pane
                let mru = filter (/=paneName pane) panes
                    mru' = maybe mru ((:mru) . fst) mbActive
                setActivePane (Just (paneName pane, conn), mru')
                trigger (Just (paneName pane)) (fst <$> mbActive)
                return ()
        where
            trigger :: Maybe Text -> Maybe Text -> IDEAction
            trigger s1 s2 = do
                triggerEventIDE (RecordHistory (PaneSelected s1, PaneSelected s2))
                triggerEventIDE (Sensitivity [(SensitivityEditor, False)])
                return ()
    --closeThisPane   ::  forall alpha beta . RecoverablePane alpha beta delta => alpha -> delta Bool
    closeThisPane pane = do
        (panePath,_)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        i               <-  notebookPageNum nb =<< getTopWidget pane
        if i < 0
            then liftIO $ do
                error ("notebook page not found: unexpected " ++ T.unpack (paneName pane) ++ " " ++ show panePath)
                return False
            else do
                deactivatePaneIfActive pane
                notebookRemovePage nb i
                widgetDestroy =<< getTopWidget pane
                removePaneAdmin pane
                modifyIDE_ $ \ide -> ide{
                    frameState = (frameState ide){
                        activePane = ( fst (activePane (frameState ide))
                                     , filter (/= paneName pane) (snd (activePane (frameState ide))))}}
                return True

data MessageLevel = Silent | Normal | High
    deriving (Eq,Ord,Show)


-- Shall be replaced
sysMessage :: MonadIO m =>  MessageLevel -> Text -> m ()
sysMessage ml str = liftIO $ do
    putStrLn $ T.unpack str
    hFlush stdout

ideMessage :: MonadIDE m => MessageLevel -> Text -> m ()
ideMessage level str = do
    liftIO $ sysMessage level str
    triggerEventIDE (LogMessage (str <> "\n") LogTag)
    return ()

logMessage :: MonadIDE m => Text -> LogTag -> m ()
logMessage str tag = do
    triggerEventIDE (LogMessage (str <> "\n") tag)
    return ()
-- with hslogger

---- ---------------------------------------------------------------------
---- Exception handling
----

data IDEException = IDEException Text
    deriving Typeable

instance Show IDEException where
  show (IDEException str) = T.unpack str

instance Exception IDEException

throwIDE str = throw (IDEException str)


-- Main window is always the first one in the list
window = head . windows

errorRefs :: IDE -> Seq LogRef
errorRefs = Seq.filter ((`elem` [ErrorRef, WarningRef, LintRef, TestFailureRef]) . logRefType) .
               allLogRefs

breakpointRefs :: IDE -> Seq LogRef
breakpointRefs = Seq.filter ((== BreakpointRef) . logRefType) . allLogRefs

contextRefs :: IDE -> Seq LogRef
contextRefs = Seq.filter ((== ContextRef) . logRefType) . allLogRefs

currentError     = (\(e,_,_)-> e) . currentEBC
currentBreak     = (\(_,b,_)-> b) . currentEBC
currentContext   = (\(_,_,c)-> c) . currentEBC

setCurrentError e = do
    modifyIDE_ (\ide -> ide{currentEBC = (e, currentBreak ide, currentContext ide)})
    triggerEventIDE_ (CurrentErrorChanged e)
setCurrentBreak b = do
    modifyIDE_ (\ide -> ide{currentEBC = (currentError ide, b, currentContext ide)})
    triggerEventIDE_ (CurrentBreakChanged b)
setCurrentContext c = modifyIDE_ (\ide -> ide{currentEBC = (currentError ide, currentBreak ide, c)})

isStartingOrClosing ::  IDEState -> Bool
isStartingOrClosing IsStartingUp    = True
isStartingOrClosing IsShuttingDown  = True
isStartingOrClosing _               = False

isInterpreting :: MonadIDE m => (FilePath, FilePath) -> m Bool
isInterpreting projectAndPackage =
    M.member projectAndPackage <$> readIDE debugState

triggerEventIDE :: MonadIDE m => IDEEvent -> m IDEEvent
triggerEventIDE e = liftIDE $ ask >>= \ideR -> triggerEvent ideR e

triggerEventIDE_ :: MonadIDE m => IDEEvent -> m ()
triggerEventIDE_ = void . triggerEventIDE

--
-- | A reader monad for a mutable reference to the IDE state
--

reifyIDE :: MonadIDE m => (IDERef -> IO a) -> m a
reifyIDE = liftIDE . ReaderT

reflectIDE :: IDEM a -> IDERef -> IO a
reflectIDE = runReaderT

reflectIDEI :: ConduitT a Void IDEM () -> IDERef -> ConduitT a Void IO ()
reflectIDEI c ideR = C.transPipe (`reflectIDE` ideR) c

liftYiControl :: Yi.ControlM a -> IDEM a
liftYiControl f = do
    control <- readIDE yiControl
    liftIO $ Yi.runControl f control

liftYi :: Yi.YiM a -> IDEM a
liftYi = liftYiControl . Yi.liftYi

catchIDE :: (MonadIDE m, Exception e) => IDEM a -> (e -> IDEM a) -> m a
catchIDE block handler = reifyIDE (\ideR -> catch (reflectIDE block ideR) (\e -> reflectIDE (handler e) ideR))

forkIDE :: MonadIDE m => IDEAction  -> m ()
forkIDE block = reifyIDE (void . forkIO . reflectIDE block)

postSyncIDE' :: (MonadIDE m, HasCallStack) => Int32 -> IDEM a -> m a
postSyncIDE' priority f = reifyIDE $ \ideR -> do
    resultVar <- newEmptyMVar
    idleAdd priority $ reflectIDE (traceTimeTaken "postSyncIDE'" f) ideR >>= putMVar resultVar >> return False
    takeMVar resultVar

postSyncIDE :: (MonadIDE m, HasCallStack) => IDEM a -> m a
postSyncIDE = postSyncIDE' PRIORITY_DEFAULT . traceTimeTaken "postSyncIDE"

postSyncIDEIdle :: (MonadIDE m, HasCallStack) => IDEM a -> m a
postSyncIDEIdle = postSyncIDE' PRIORITY_DEFAULT_IDLE . traceTimeTaken "postSyncIDEIdle"

postAsyncIDE' :: (MonadIDE m, HasCallStack) => Int32 -> IDEM () -> m ()
postAsyncIDE' priority f = reifyIDE $ \ideR ->
    void . idleAdd priority $ reflectIDE (traceTimeTaken "postAsyncIDE'" f) ideR >> return False

postAsyncIDE :: (MonadIDE m, HasCallStack) => IDEM () -> m ()
postAsyncIDE = postAsyncIDE' PRIORITY_DEFAULT . traceTimeTaken "postAsyncIDE"

postAsyncIDEIdle :: (MonadIDE m, HasCallStack) => IDEM () -> m ()
postAsyncIDEIdle = postAsyncIDE' PRIORITY_DEFAULT_IDLE . traceTimeTaken "postAsyncIDEIdle"

onIDE onSignal obj callback = do
    ideRef <- ask
    liftIO (ConnectC obj <$> onSignal obj (runReaderT (runReaderT callback ideRef)))

-- ---------------------------------------------------------------------
-- Convenience methods for accesing the IDE State
--

-- | Read an attribute of the contents
readIDE :: MonadIDE m => (IDE -> beta) -> m beta
readIDE f = do
    e <- liftIDE ask
    liftIO $ f <$> readIORef e

-- | Modify the contents, without returning a value
modifyIDE_ :: MonadIDE m => (IDE -> IDE) -> m ()
modifyIDE_ f = let f' a  = (f a,()) in do
    e <- liftIDE ask
    liftIO (atomicModifyIORef e f')

-- | Variation on modifyIDE_ that lets you return a value
modifyIDE :: MonadIDE m => (IDE -> (IDE,beta)) -> m beta
modifyIDE f = do
    e <- liftIDE ask
    liftIO (atomicModifyIORef e f)

withIDE :: MonadIDE m => (IDE -> IO alpha) -> m alpha
withIDE f = do
    e <- liftIDE ask
    liftIO $ f =<< readIORef e

getIDE :: MonadIDE m => m IDE
getIDE = liftIDE ask >>= (liftIO . readIORef)

withoutRecordingDo :: IDEAction -> IDEAction
withoutRecordingDo act = do
    (b,l,n) <- readIDE guiHistory
    if not b then do
        modifyIDE_ (\ide -> ide{guiHistory = (True,l,n)})
        act
        (b,l,n) <- readIDE guiHistory
        modifyIDE_ (\ide -> ide{guiHistory = (False,l,n)})
        else act

packageDebugState :: PackageM (Maybe (IDEPackage, ToolState))
packageDebugState = do
    project <- lift ask
    package <- ask
    M.lookup (pjFile project, ipdCabalFile package) <$> readIDE debugState

-- ---------------------------------------------------------------------
-- Activating and deactivating Panes.
-- This is here and not in Views because it needs some dependencies
-- (e.g. Events for history)
--

deactivatePane :: IDEAction
deactivatePane =
    getActivePane >>= \case
        (Nothing,_)      -> return ()
        (Just (pn, _),_) -> do
            deactivatePaneWithout
            triggerEventIDE (RecordHistory (PaneSelected Nothing,
                PaneSelected (Just pn)))
            triggerEventIDE (Sensitivity [(SensitivityEditor, False)])
            return ()

deactivatePaneWithout :: IDEAction
deactivatePaneWithout = do
    triggerEventIDE (StatusbarChanged [CompartmentPane Nothing])
    getActivePane >>= \case
        (Just (n,signals), mru) -> do
            signalDisconnectAll signals
            setActivePane (Nothing, n:mru)
        (Nothing, _) -> return ()

deactivatePaneIfActive :: RecoverablePane alpha beta IDEM => alpha -> IDEAction
deactivatePaneIfActive pane =
    getActivePane >>= \case
        (Nothing, _) -> return ()
        (Just (n,_), _) -> when (n == paneName pane) deactivatePane

-- | Replaces an 'IDEPackage' in the workspace by the given 'IDEPackage' and
-- replaces the current package if it matches.
--  Comparison is done based on the package's build directory.
changePackage :: IDEPackage -> IDEAction
changePackage ideP = do
    oldWorkspace <- readIDE workspace
    case oldWorkspace of
        Nothing -> return ()
        Just ws ->
            modifyIDE_ $ \ide -> ide{workspace = Just ws {
                wsProjects = map (\p -> p {
                    pjPackageMap = mkPackageMap $ map exchange (pjPackages p)}) (wsProjects ws)},
                bufferProjCache = Map.empty}
    where
        key = ipdPackageDir
        idePKey = key ideP
        exchange p | key p == idePKey = ideP
                   | otherwise        = p

-- | Replaces an 'Project' in the workspace by the given 'Project' and
-- replaces the current package if it matches.
--  Comparison is done based on the package's build directory.
changeProject :: Project -> IDEAction
changeProject project =
    readIDE workspace >>= \case
        Nothing -> return ()
        Just ws -> do
            let ps = map exchange (wsProjects ws)
            modifyIDE_ (\ide -> ide{workspace = Just ws {wsProjects = ps},
                                    bufferProjCache = Map.empty})
    where
        exchange p | pjFile p == pjFile project = project
                   | otherwise        = p

-- | Find a directory relative to the leksah install directory
leksahSubDir :: FilePath    -- ^ Sub directory to look for
             -> IO (Maybe FilePath)
leksahSubDir subDir = do
    exePath <- getExecutablePath
    if takeFileName exePath == "leksah.exe"
        then do
            let dataDir = takeDirectory (takeDirectory exePath) </> subDir
            exists <- doesDirectoryExist dataDir
            return (if exists then Just dataDir else Nothing)
        else return Nothing

-- | Get the leksah data dir based on the executable name or if that fails
-- use the directroy for the package.  This is allows us to make binary packages
-- where the data directory id relative to the leksah executable.
-- This is important for Wind32 where setting environment variables for the
-- locations in a launch script causes problems (you can't pin the exe).
leksahOrPackageDir :: FilePath    -- ^ Sub directory to look for
                   -> IO FilePath -- ^ Used to get the package dir if we can't find the leksah one
                   -> IO FilePath
leksahOrPackageDir subDir getPackageDir =
    catchIO (not . null <$> getEnv (subDir <> "_datadir")) (\_ -> return False) >>= \case
        True -> getPackageDir
        False ->
            leksahSubDir subDir >>= \case
                Just result -> return result
                Nothing     -> getPackageDir

getDataDir :: MonadIO m => m FilePath
getDataDir = liftIO $ leksahOrPackageDir "leksah" P.getDataDir

