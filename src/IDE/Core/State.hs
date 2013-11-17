{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances,
             MultiParamTypeClasses, ScopedTypeVariables, CPP,
             DeriveDataTypeable #-}
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

,   deactivatePane

-- * Convenience methods for accesing the IDE State
,   readIDE
,   modifyIDE
,   modifyIDE_
,   withIDE
,   getIDE
,   throwIDE

,   reifyIDE
,   reflectIDE
,   reflectIDEI
,   catchIDE
,   postSyncIDE
,   postAsyncIDE
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
,   activeProjectDir
,   changePackage

,   liftYiControl
,   liftYi

,   leksahSubDir
,   leksahOrPackageDir
,   getDataDir
,   P.version

,   module IDE.Core.Types
,   module IDE.Core.CTypes
,   module IDE.Utils.Utils
,   module Graphics.UI.Frame.Panes
,   module Graphics.UI.Frame.ViewFrame

) where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.SourceView.SourceView ()

import Data.IORef
import Control.Exception
import Prelude hiding (catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import IDE.Core.Types
import Graphics.UI.Frame.Panes
import Graphics.UI.Frame.ViewFrame --hiding (notebookInsertOrdered)
import Control.Event
import System.IO
import Data.Maybe (isJust)
import System.FilePath
       (dropFileName, takeDirectory, (</>), takeFileName)
import IDE.Core.CTypes
import Control.Concurrent (forkIO)
import IDE.Utils.Utils
import qualified Data.Map as Map (empty, lookup)
import Data.Typeable(Typeable)
import qualified IDE.YiConfig as Yi
import Data.Conduit (($$))
import qualified Data.Conduit as C
       (transPipe, Sink, awaitForever, yield, leftover, ($$))
import qualified Data.Conduit.List as CL
       (sourceList)
import Control.Monad (liftM, when)
import Control.Monad.Trans.Reader (ask, ReaderT(..))
import qualified Paths_leksah as P
import System.Environment.Executable (getExecutablePath)
import System.Directory (doesDirectoryExist)

instance PaneMonad IDEM where
    getFrameState   =   readIDE frameState
    setFrameState v =   modifyIDE_ (\ide -> ide{frameState = v})
    runInIO f       =   reifyIDE (\ideRef -> return (\v -> reflectIDE (f v) ideRef))
    panePathForGroup id =   do
        prefs  <- readIDE prefs
        case id `lookup` (categoryForPane prefs) of
            Just group -> case group `lookup`  (pathForCategory prefs) of
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
        liftIO $ bringPaneToFront pane
        when shallGrabFocus $ liftIO $ widgetGrabFocus $ getTopWidget pane
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
                        notebookInsertOrdered notebook (getTopWidget buf) (paneName buf) Nothing False
                        addPaneAdmin buf cids panePath
                        liftIO $ do
                            widgetSetName (getTopWidget buf) (paneName buf)
                            widgetShowAll (getTopWidget buf)
                            widgetGrabFocus (getTopWidget buf)
                            bringPaneToFront buf
                        return (Just buf)
                    else return Nothing
    --activateThisPane :: forall alpha beta . RecoverablePane alpha beta delta => alpha -> Connections -> delta ()
    activateThisPane pane conn = do
        mbAP <- getActivePane
        case mbAP of
            Just (pn,_) | pn == paneName pane -> return ()
            _  -> do
                deactivatePaneWithout
                triggerEventIDE (StatusbarChanged [CompartmentPane (Just (PaneC pane))])
                liftIO $ bringPaneToFront pane
                setActivePane (Just (paneName pane,conn))
                trigger (Just (paneName pane))
                    (case mbAP of
                        Nothing -> Nothing
                        Just (pn,_) -> Just pn)
                modifyIDE_ updateRecent
                return ()
        where
            updateRecent (ide@IDE{currentState = IsFlipping _}) = ide
            updateRecent ide = ide{recentPanes = paneName pane : filter (/= paneName pane) (recentPanes ide)}
            trigger :: Maybe String -> Maybe String -> IDEAction
            trigger s1 s2 = do
                triggerEventIDE (RecordHistory ((PaneSelected s1), PaneSelected s2))
                triggerEventIDE (Sensitivity [(SensitivityEditor, False)])
                return ()
    --closeThisPane   ::  forall alpha beta . RecoverablePane alpha beta delta => alpha -> delta Bool
    closeThisPane pane = do
        (panePath,_)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        mbI             <-  liftIO $notebookPageNum nb (getTopWidget pane)
        case mbI of
            Nothing ->  liftIO $ do
                error ("notebook page not found: unexpected " ++ paneName pane ++ " " ++ show panePath)
                return False
            Just i  ->  do
                deactivatePaneIfActive pane
                liftIO $ do
                    notebookRemovePage nb i
                    widgetDestroy (getTopWidget pane)
                removePaneAdmin pane
                modifyIDE_ (\ide -> ide{recentPanes = filter (/= paneName pane) (recentPanes ide)})
                return True

data MessageLevel = Silent | Normal | High
    deriving (Eq,Ord,Show)


-- Shall be replaced
sysMessage :: MonadIO m =>  MessageLevel -> String -> m ()
sysMessage ml str = liftIO $ do
    putStrLn str
    hFlush stdout

ideMessage :: MessageLevel -> String -> IDEAction
ideMessage level str = do
    triggerEventIDE (LogMessage (str ++ "\n") LogTag)
    liftIO $ sysMessage level str

logMessage :: String -> LogTag -> IDEAction
logMessage str tag = do
    triggerEventIDE (LogMessage (str ++ "\n") tag)
    return ()
-- with hslogger

---- ---------------------------------------------------------------------
---- Exception handling
----

data IDEException = IDEException String
    deriving Typeable

instance Show IDEException where
  show (IDEException str) = str

instance Exception IDEException

throwIDE str = throw (IDEException str)


-- Main window is always the first one in the list
window = head . windows

activeProjectDir :: IDEM FilePath
activeProjectDir = do
    activePack' <- readIDE activePack
    case activePack' of
        Nothing   -> return "."
        Just pack -> return (dropFileName (ipdCabalFile pack))

errorRefs :: IDE -> [LogRef]
errorRefs = (filter ((\t -> t == ErrorRef || t == WarningRef) . logRefType)) . allLogRefs

breakpointRefs :: IDE -> [LogRef]
breakpointRefs = (filter ((== BreakpointRef) . logRefType)) . allLogRefs

contextRefs :: IDE -> [LogRef]
contextRefs = (filter ((== ContextRef) . logRefType)) . allLogRefs

currentError     = (\(e,_,_)-> e) . currentEBC
currentBreak     = (\(_,b,_)-> b) . currentEBC
currentContext   = (\(_,_,c)-> c) . currentEBC

setCurrentError e = do
    modifyIDE_ (\ide -> ide{currentEBC = (e, currentBreak ide, currentContext ide)})
    triggerEventIDE (CurrentErrorChanged e) >> return ()
setCurrentBreak b = do
    modifyIDE_ (\ide -> ide{currentEBC = (currentError ide, b, currentContext ide)})
    triggerEventIDE (CurrentBreakChanged b) >> return ()
setCurrentContext c = modifyIDE_ (\ide -> ide{currentEBC = (currentError ide, currentBreak ide, c)})

isStartingOrClosing ::  IDEState -> Bool
isStartingOrClosing IsStartingUp    = True
isStartingOrClosing IsShuttingDown  = True
isStartingOrClosing _               = False

isInterpreting :: IDEM Bool
isInterpreting = do
    readIDE debugState >>= \mb -> return (isJust mb)

triggerEventIDE :: IDEEvent -> IDEM IDEEvent
triggerEventIDE e = ask >>= \ideR -> triggerEvent ideR e

--
-- | A reader monad for a mutable reference to the IDE state
--

reifyIDE :: (IDERef -> IO a) -> IDEM a
reifyIDE = ReaderT

reflectIDE :: IDEM a -> IDERef -> IO a
reflectIDE c ideR = runReaderT c ideR

reflectIDEI :: C.Sink a IDEM () -> IDERef -> C.Sink a IO ()
reflectIDEI c ideR = C.transPipe (`reflectIDE` ideR) c

liftYiControl :: Yi.ControlM a -> IDEM a
liftYiControl f = do
    control <- readIDE yiControl
    liftIO $ Yi.runControl f control

liftYi :: Yi.YiM a -> IDEM a
liftYi = liftYiControl . Yi.liftYi

catchIDE :: Exception e	=> IDEM a -> (e -> IO a) -> IDEM a
catchIDE block handler = reifyIDE (\ideR -> catch (reflectIDE block ideR) handler)

forkIDE :: IDEAction  -> IDEAction
forkIDE block  = reifyIDE (\ideR -> forkIO  (reflectIDE block ideR) >> return ())

postSyncIDE :: IDEM a -> IDEM a
postSyncIDE f = reifyIDE (\ideR -> postGUISync (reflectIDE f ideR))

postAsyncIDE :: IDEM () -> IDEM ()
postAsyncIDE f = reifyIDE (\ideR -> postGUIAsync (reflectIDE f ideR))

onIDE obj signal callback = do
    ideRef <- ask
    liftIO $ obj `on` signal $ runReaderT callback ideRef

-- ---------------------------------------------------------------------
-- Convenience methods for accesing the IDE State
--

-- | Read an attribute of the contents
readIDE :: (IDE -> beta) -> IDEM beta
readIDE f = do
    e <- ask
    liftIO $ liftM f (readIORef e)

-- | Modify the contents, without returning a value
modifyIDE_ :: (IDE -> IDE) -> IDEM ()
modifyIDE_ f = let f' a  = (f a,()) in do
    e <- ask
    liftIO (atomicModifyIORef e f')

-- | Variation on modifyIDE_ that lets you return a value
modifyIDE :: (IDE -> (IDE,beta)) -> IDEM beta
modifyIDE f = do
    e <- ask
    liftIO (atomicModifyIORef e f)

withIDE :: (IDE -> IO alpha) -> IDEM alpha
withIDE f = do
    e <- ask
    liftIO $ f =<< readIORef e

getIDE :: IDEM(IDE)
getIDE = do
    e <- ask
    st <- liftIO $ readIORef e
    return st

withoutRecordingDo :: IDEAction -> IDEAction
withoutRecordingDo act = do
    (b,l,n) <- readIDE guiHistory
    if not b then do
        modifyIDE_ (\ide -> ide{guiHistory = (True,l,n)})
        act
        (b,l,n) <- readIDE guiHistory
        modifyIDE_ (\ide -> ide{guiHistory = (False,l,n)})
        else act

-- ---------------------------------------------------------------------
-- Activating and deactivating Panes.
-- This is here and not in Views because it needs some dependencies
-- (e.g. Events for history)
--

deactivatePane :: IDEAction
deactivatePane = do
    mbAP    <-  getActivePane
    case mbAP of
        Nothing      -> return ()
        Just (pn, _) -> do
            deactivatePaneWithout
            triggerEventIDE (RecordHistory (PaneSelected Nothing,
                PaneSelected (Just pn)))
            triggerEventIDE (Sensitivity [(SensitivityEditor, False)])
            return ()

deactivatePaneWithout :: IDEAction
deactivatePaneWithout = do
    triggerEventIDE (StatusbarChanged [CompartmentPane Nothing])
    mbAP    <-  getActivePane
    case mbAP of
        Just (_,signals) -> liftIO $do
            signalDisconnectAll signals
        Nothing -> return ()
    setActivePane Nothing

deactivatePaneIfActive :: RecoverablePane alpha beta IDEM => alpha -> IDEAction
deactivatePaneIfActive pane = do
    mbActive <- getActivePane
    case mbActive of
        Nothing -> return ()
        Just (n,_) -> if n == paneName pane
                        then deactivatePane
                        else return ()

changePackage :: IDEPackage -> IDEAction
changePackage ideP@IDEPackage{ipdCabalFile = file} = do
    oldWorkspace <- readIDE workspace
    case oldWorkspace of
        Nothing -> return ()
        Just ws -> do
            let ps = map exchange (wsPackages ws)
            modifyIDE_ (\ide -> ide{workspace = Just ws {wsPackages = ps},
                                    bufferProjCache = Map.empty})
    mbActivePack <- readIDE activePack
    case mbActivePack of
        Just activePack | ipdCabalFile ideP == ipdCabalFile activePack ->
            modifyIDE_ (\ide -> ide{activePack = Just ideP})
        _ -> return ()
    where
        exchange p | ipdCabalFile p == file = ideP
                   | otherwise              = p

-- | Find a directory relative to the leksah install directory
leksahSubDir :: FilePath    -- ^ Sub directory to look for
             -> IO (Maybe FilePath)
leksahSubDir subDir = do
    exePath <- getExecutablePath
    if takeFileName exePath == "leksah.exe"
        then do
            let dataDir = (takeDirectory $ takeDirectory exePath) </> subDir
            exists <- doesDirectoryExist dataDir
            if exists then return (Just dataDir) else return Nothing
        else return Nothing

-- | Get the leksah data dir based on the executable name or if that fails
-- use the directroy for the package.  This is allows us to make binary packages
-- where the data directory id relative to the leksah executable.
-- This is important for Wind32 where setting environment variables for the
-- locations in a launch script causes problems (you can't pin the exe).
leksahOrPackageDir :: FilePath    -- ^ Sub directory to look for
                   -> IO FilePath -- ^ Used to get the package dir if we can't find the leksah one
                   -> IO FilePath
leksahOrPackageDir subDir getPackageDir = do
    mbResult <- leksahSubDir subDir
    case mbResult of
        Just result -> return result
        Nothing     -> getPackageDir

getDataDir :: IO FilePath
getDataDir = leksahOrPackageDir "leksah" P.getDataDir

