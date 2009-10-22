{-# OPTIONS_GHC -XFlexibleContexts -XTypeSynonymInstances -XMultiParamTypeClasses
    -XScopedTypeVariables #-}
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

,   reifyIDE
,   reflectIDE
,   catchIDE
,   postSyncIDE
,   postAsyncIDE

,   ideMessage
,   logMessage
,   sysMessage
,   MessageLevel(..)

,   withoutRecordingDo
--,   deactivatePane
--,   deactivatePaneIfActive
--,   closePane

,   getCandyState
,   setCandyState
,   getForgetSession

,   getBackgroundBuildToggled
,   setBackgroundBuildToggled
,   getBackgroundLinkToggled
,   setBackgroundLinkToggled
,   getDebugToggled
,   setDebugToggled

,   getRecentFiles
,   getRecentWorkspaces
,   controlIsPressed

,   leksahSessionFileExtension
,   standardSessionFilename
,   packageSessionFilename
,   leksahWorkspaceFileExtension
,   leksahPreferencesFileExtension
,   standardPreferencesFilename
,   leksahCandyFileExtension
,   standardCandyFilename
,   leksahKeymapFileExtension
,   standardKeymapFilename
,   leksahSourcesFileExtension
,   standardSourcesFilename
,   leksahMetadataFileExtension
,   leksahMetadataDebugExtension
,   leksahCurrentMetaExtension
,   leksahTemplateFileExtension
,   standardModuleTemplateFilename

,   configDirName
#ifdef YI
,   liftYiControl
#endif

,   Session

,   module IDE.Core.Types
,   module Graphics.UI.Frame.Panes
,   module Graphics.UI.Frame.ViewFrame
,   module IDE.Exception

) where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.SourceView.SourceView ()
import Graphics.UI.Gtk.Gdk.Enums (Modifier(..))
import qualified Graphics.UI.Gtk.Gdk.Events as G (Event(..))
import Data.IORef
import Control.Monad.Reader hiding (liftIO)
import qualified Control.Monad.Reader (liftIO)
import Control.Monad.Trans
import HscTypes hiding (liftIO)
import Data.Unique
import Control.Exception
import Prelude hiding (catch)

#ifdef YI
import qualified Yi as Yi
import qualified Yi.UI.Pango.Control as Yi
import Control.Monad.State
#endif

import IDE.Core.Types
import Graphics.UI.Frame.Panes
import Graphics.UI.Frame.ViewFrame hiding (notebookInsertOrdered)
import qualified Graphics.UI.Frame.ViewFrame as VF (notebookInsertOrdered)
import IDE.Exception
import Control.Event
import System.IO
import Data.Maybe (isJust)

leksahSessionFileExtension = ".lkshs"
leksahWorkspaceFileExtension = ".lkshw"
leksahPreferencesFileExtension = ".lkshp"
leksahCandyFileExtension = ".lkshc"
leksahKeymapFileExtension = ".lkshk"
leksahSourcesFileExtension = ".lksho"
leksahMetadataFileExtension = ".lkshm"
leksahCurrentMetaExtension = ".lkshe"
leksahMetadataDebugExtension = ".lkshd"
leksahTemplateFileExtension = ".lksht"

standardSessionFilename =   "current" ++ leksahSessionFileExtension
packageSessionFilename = "leksah" ++ leksahSessionFileExtension

standardKeymapFilename =   "keymap" ++ leksahKeymapFileExtension
standardCandyFilename =   "candy" ++ leksahCandyFileExtension
standardPreferencesFilename =   "prefs" ++ leksahPreferencesFileExtension
standardSourcesFilename =   "sources" ++ leksahSourcesFileExtension
standardModuleTemplateFilename =   "module" ++ leksahTemplateFileExtension
configDirName = ".leksah-0.7"



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
                mbPane'      <-  buildPane pp nb builder
                return mbPane'
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
        windows <- getWindows
        (mbBuf,cids)  <-  builder panePath notebook (head windows)
        case mbBuf of
            Nothing -> return Nothing
            Just buf -> do
                VF.notebookInsertOrdered notebook (getTopWidget buf) (paneName buf) Nothing
                addPaneAdmin buf cids panePath
                liftIO $ do
                    widgetShowAll (getTopWidget buf)
                    widgetGrabFocus (getTopWidget buf)
                    bringPaneToFront buf
                return (Just buf)
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
                modifyIDE_ (\ide -> ide{recentPanes =
                    paneName pane : filter (/= paneName pane) (recentPanes ide)})
                return ()
    --closeThisPane   ::  forall alpha beta . RecoverablePane alpha beta delta => alpha -> delta Bool
    closeThisPane pane = do
        (panePath,_)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        mbI             <-  liftIO $notebookPageNum nb (getTopWidget pane)
        case mbI of
            Nothing ->  liftIO $ do
                throwIDE ("notebook page not found: unexpected " ++ paneName pane ++ " " ++ show panePath)
                return False
            Just i  ->  do
                deactivatePaneIfActive pane
                liftIO $ do
                    notebookRemovePage nb i
                    widgetDestroy (getTopWidget pane)
                removePaneAdmin pane
                modifyIDE_ (\ide -> ide{recentPanes = filter (/= paneName pane) (recentPanes ide)})
                return True






-- this should not be repeated here, why is it necessary?
instance MonadIO Ghc where
  liftIO ioA = Ghc $ \_ -> ioA

instance Event IDEEvent String where
    getSelector CurrentInfo             =   "CurrentInfo"
    getSelector ActivePack              =   "ActivePack"
    getSelector (LogMessage _ _)        =   "LogMessage"
    getSelector (SelectInfo _)          =   "SelectInfo"
    getSelector (SelectIdent _)         =   "SelectIdent"
    getSelector (RecordHistory _)       =   "RecordHistory"
    getSelector (Sensitivity _)         =   "Sensitivity"
    getSelector (DescrChoice _)         =   "DescrChoice"
    getSelector (SearchMeta _)          =   "SearchMeta"
    getSelector (LoadSession _)         =   "LoadSession"
    getSelector (SaveSession _)         =   "SaveSession"
    getSelector UpdateRecent            =   "UpdateRecent"
    getSelector VariablesChanged        =   "VariablesChanged"
    getSelector ErrorChanged            =   "ErrorChanged"
    getSelector (CurrentErrorChanged _) =   "CurrentErrorChanged"
    getSelector BreakpointChanged       =   "BreakpointChanged"
    getSelector (CurrentBreakChanged _) =   "CurrentBreakChanged"
    getSelector TraceChanged            =   "TraceChanged"
    getSelector (GetTextPopup _)        =   "GetTextPopup"
    getSelector (StatusbarChanged _)    =   "StatusbarChanged"
    getSelector WorkspaceChanged        =   "WorkspaceChanged"
    getSelector (WorkspaceAddPackage _) =   "WorkspaceAddPackage"

instance EventSource IDERef IDEEvent IDEM String where
    canTriggerEvent o "CurrentInfo"     =   True
    canTriggerEvent o "ActivePack"      =   True
    canTriggerEvent o "LogMessage"      =   True
    canTriggerEvent o "SelectInfo"      =   True
    canTriggerEvent o "SelectIdent"     =   True
    canTriggerEvent o "RecordHistory"   =   True
    canTriggerEvent o "Sensitivity"     =   True
    canTriggerEvent o "DescrChoice"     =   True
    canTriggerEvent o "SearchMeta"      =   True
    canTriggerEvent o "LoadSession"     =   True
    canTriggerEvent o "SaveSession"     =   True
    canTriggerEvent o "UpdateRecent"    =   True
    canTriggerEvent o "VariablesChanged" =   True
    canTriggerEvent o "ErrorChanged"    =   True
    canTriggerEvent o "CurrentErrorChanged" = True
    canTriggerEvent o "BreakpointChanged" = True
    canTriggerEvent o "CurrentBreakChanged" = True
    canTriggerEvent o "TraceChanged"    = True
    canTriggerEvent o "GetTextPopup"    = True
    canTriggerEvent o "StatusbarChanged"    = True
    canTriggerEvent o "WorkspaceChanged"    = True
    canTriggerEvent o "WorkspaceAddPackage"    = True

    canTriggerEvent _ _                 =   False

    getHandlers ideRef = do
        ide <- liftIO $ readIORef ideRef
        return (handlers ide)

    setHandlers ideRef nh = do
        ide <- liftIO $ readIORef ideRef
        liftIO $ writeIORef ideRef (ide {handlers= nh})

    myUnique _ = do
        liftIO $ newUnique

instance EventSelector String


ideMessage :: MessageLevel -> String -> IDEAction
ideMessage level str = do
    triggerEventIDE (LogMessage (str ++ "\n") LogTag)
    liftIO $ sysMessage level str

logMessage :: String -> LogTag -> IDEAction
logMessage str tag = do
    triggerEventIDE (LogMessage (str ++ "\n") tag)
    return ()

sysMessage :: MonadIO m =>  MessageLevel -> String -> m ()
sysMessage ml str = liftIO $ do
    putStrLn str
    hFlush stdout

data MessageLevel = Silent | Normal | High
    deriving (Eq,Ord,Show)


-- Main window is just the first one in the list
window = head . windows


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
    readIDE ghciState >>= \mb -> return (isJust mb)

triggerEventIDE :: IDEEvent -> IDEM IDEEvent
triggerEventIDE e = ask >>= \ideR -> triggerEvent ideR e

--
-- | A reader monad for a mutable reference to the IDE state
--

reifyIDE :: (IDERef -> IO a) -> IDEM a
reifyIDE = ReaderT

reflectIDE :: IDEM a -> IDERef -> IO a
reflectIDE c ideR = runReaderT c ideR

#ifdef YI

liftYiControl :: Yi.ControlM a -> IDEM a
liftYiControl f = do
    control <- readIDE yiControl
    liftIO $ Yi.runControl f control

#endif

catchIDE :: Exception e	=> IDEM a -> (e -> IO a) -> IDEM a
catchIDE block handler = reifyIDE (\ideR -> catch (reflectIDE block ideR) handler)

postSyncIDE :: IDEM a -> IDEM a
postSyncIDE f = reifyIDE (\ideR -> postGUISync (reflectIDE f ideR))

postAsyncIDE :: IDEM () -> IDEM ()
postAsyncIDE f = reifyIDE (\ideR -> postGUIAsync (reflectIDE f ideR))

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

trigger :: Maybe String -> Maybe String -> IDEAction
trigger s1 s2 = do
    triggerEventIDE (RecordHistory ((PaneSelected s1), PaneSelected s2))
    triggerEventIDE (Sensitivity [(SensitivityEditor, False)])
    return ()


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

-- get widget elements (menu & toolbar)

getCandyState :: PaneMonad alpha => alpha Bool
getCandyState = do
    ui <- getUIAction "ui/menubar/_Configuration/Source Candy" castToToggleAction
    liftIO $toggleActionGetActive ui

setCandyState :: PaneMonad alpha => Bool -> alpha ()
setCandyState b = do
    ui <- getUIAction "ui/menubar/_Configuration/Source Candy" castToToggleAction
    liftIO $toggleActionSetActive ui b

getForgetSession :: PaneMonad alpha => alpha  (Bool)
getForgetSession = do
    ui <- getUIAction "ui/menubar/_Configuration/Forget Session" castToToggleAction
    liftIO $toggleActionGetActive ui

getMenuItem :: String -> IDEM MenuItem
getMenuItem path = do
    uiManager' <- getUiManager
    mbWidget   <- liftIO $ uiManagerGetWidget uiManager' path
    case mbWidget of
        Nothing     -> throwIDE ("State.hs>>getMenuItem: Can't find ui path " ++ path)
        Just widget -> return (castToMenuItem widget)

getBackgroundBuildToggled :: PaneMonad alpha => alpha  (Bool)
getBackgroundBuildToggled = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/BackgroundBuild" castToToggleAction
    liftIO $ toggleActionGetActive ui

setBackgroundBuildToggled :: PaneMonad alpha => Bool -> alpha ()
setBackgroundBuildToggled b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/BackgroundBuild" castToToggleAction
    liftIO $ toggleActionSetActive ui b

getBackgroundLinkToggled :: PaneMonad alpha => alpha  (Bool)
getBackgroundLinkToggled = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/BackgroundLink" castToToggleAction
    liftIO $ toggleActionGetActive ui

setBackgroundLinkToggled :: PaneMonad alpha => Bool -> alpha ()
setBackgroundLinkToggled b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/BackgroundLink" castToToggleAction
    liftIO $ toggleActionSetActive ui b

getDebugToggled :: PaneMonad alpha => alpha  (Bool)
getDebugToggled = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/Debug" castToToggleAction
    liftIO $ toggleActionGetActive ui

setDebugToggled :: PaneMonad alpha => Bool -> alpha ()
setDebugToggled b = do
    ui <- getUIAction "ui/toolbar/BuildToolItems/Debug" castToToggleAction
    liftIO $ toggleActionSetActive ui b

getRecentFiles , getRecentWorkspaces :: IDEM MenuItem
getRecentFiles    = getMenuItem "ui/menubar/_File/_Recent Files"
getRecentWorkspaces = getMenuItem "ui/menubar/_Workspace/_Recent Workspaces"

-- (toolbar)

controlIsPressed :: G.Event -> Bool
controlIsPressed (G.Button _ _ _ _ _ mods _ _ _) | Control `elem` mods = True
controlIsPressed _                                                   = False


