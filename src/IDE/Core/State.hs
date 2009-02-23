{-# OPTIONS_GHC -XFlexibleContexts -XTypeSynonymInstances -XMultiParamTypeClasses
    -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.State
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info at leksah.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The core state of ide. This module is imported from every other module,
-- | and all data structures of the state are declared here, to avoid circular
-- | module dependencies.
--
-------------------------------------------------------------------------------

module IDE.Core.State (
    IDEObject
,   IDEEditor
,   IDE(..)
,   IDEState(..)
,   isStartingOrClosing
,   IDERef
,   IDEM
,   IDEAction
,   IDEEvent(..)

-- * Convenience methods for accesing the IDE State
,   readIDE
,   modifyIDE
,   modifyIDE_
,   withIDE
,   getIDE

,   reifyIDE
,   reflectIDE
,   catchIDE

,   ideMessage
,   logMessage
,   sysMessage
,   MessageLevel(..)

,   withoutRecordingDo
,   activatePane
,   deactivatePane
,   deactivatePaneIfActive
,   closePane

,   getCandyState
,   setCandyState
,   getForgetSession

,   getSBSpecialKeys
,   getSBActivePane
,   getSBActivePackage
,   getSBErrors
,   getStatusbarIO
,   getStatusbarLC

,   getRecentFiles
,   getRecentPackages

,   Session

,   module IDE.Core.Types
,   module Graphics.UI.Frame.Panes
,   module Graphics.UI.Frame.ViewFrame
,   module IDE.Exception

) where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.SourceView.SourceView ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Control.Monad.Reader hiding (liftIO)
import qualified Control.Monad.Reader (liftIO)
import Control.Monad.Trans
import HscTypes hiding (liftIO)
import Data.Unique
import Control.Exception
import Prelude hiding (catch)

import IDE.Core.Types
import Graphics.UI.Frame.Panes
import Graphics.UI.Frame.ViewFrame
import IDE.Exception
import Control.Event
import System.IO

-- this should not be repeated here, why is it necessary?
instance MonadIO Ghc where
  liftIO ioA = Ghc $ \_ -> ioA

ideMessage :: MessageLevel -> String -> IDEAction
ideMessage level str = do
    st <- ask
    triggerEvent st (LogMessage (str ++ "\n") LogTag)
    liftIO $ sysMessage level str

logMessage :: String -> LogTag -> IDEAction
logMessage str tag = do
    st <- ask
    triggerEvent st (LogMessage (str ++ "\n") tag)
    return ()

sysMessage :: MonadIO m =>  MessageLevel -> String -> m ()
sysMessage ml str = liftIO $ do
    putStrLn str
    hFlush stdout

data MessageLevel = Silent | Normal | High
    deriving (Eq,Ord,Show)

class IDEObject alpha
class IDEObject o => IDEEditor o


-- ---------------------------------------------------------------------
-- IDE State
--

--
-- | The IDE state
--
data IDE            =  IDE {
    window          ::   Window                  -- ^ the gtk window
,   uiManager       ::   UIManager               -- ^ the gtk uiManager
,   panes           ::   Map PaneName (IDEPane IDEM)    -- ^ a map with all panes (subwindows)
,   activePane      ::   Maybe (PaneName,Connections)
,   recentPanes     ::   [PaneName]
,   paneMap         ::   Map PaneName (PanePath, Connections)
                    -- ^ a map from the pane name to its gui path and signal connections
,   layout          ::   PaneLayout              -- ^ a description of the general gui layout
,   specialKeys     ::   SpecialKeyTable IDERef  -- ^ a structure for emacs like keystrokes
,   specialKey      ::   SpecialKeyCons IDERef   -- ^ the first of a double keystroke
,   candy           ::   CandyTable              -- ^ table for source candy
,   prefs           ::   Prefs                   -- ^ configuration preferences
,   activePack      ::   Maybe IDEPackage
,   errors          ::   [ErrorSpec]
,   currentErr      ::   Maybe Int
,   accessibleInfo  ::   (Maybe (PackageScope))     -- ^  the world scope
,   currentInfo     ::   (Maybe (PackageScope,PackageScope))
                                                -- ^ the first is for the current package,
                                                --the second is the scope in the current package
,   handlers        ::   Map String [(Unique, IDEEvent -> IDEM IDEEvent)]
                                                -- ^ event handling table
,   currentState    ::   IDEState
,   guiHistory      ::   (Bool,[GUIHistory],Int)
,   findbar         ::   Toolbar
,   toolbar         ::   Maybe Toolbar
,   findbarVisible  ::   Bool
,   toolbarVisible  ::   Bool
,   recentFiles     ::   [FilePath]
,   recentPackages  ::   [FilePath]
} --deriving Show

data IDEState =
        IsStartingUp
    |   IsShuttingDown
    |   IsRunning
    |   IsFlipping TreeView
    |   IsCompleting Window TreeView (ListStore String) Connections

isStartingOrClosing ::  IDEState -> Bool
isStartingOrClosing IsStartingUp    = True
isStartingOrClosing IsShuttingDown  = True
isStartingOrClosing _               = False

data IDEEvent  =
        CurrentInfo
    |   ActivePack
    |   SelectInfo String
    |   SelectIdent Descr
    |   LogMessage String LogTag
    |   RecordHistory GUIHistory
    |   Sensitivity [(SensitivityMask,Bool)]
    |   DescrChoice [Descr]
    |   SearchMeta String
    |   LoadSession FilePath
    |   SaveSession FilePath
    |   UpdateRecent

--
-- | A mutable reference to the IDE state
--
type IDERef = IORef IDE

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


--
-- | A reader monad for a mutable reference to the IDE state
--
type IDEM = ReaderT IDERef IO

reifyIDE :: (IDERef -> IO a) -> IDEM a
reifyIDE = ReaderT


reflectIDE :: IDEM a -> IDERef -> IO a
reflectIDE c ideR = runReaderT c ideR

catchIDE :: Exception e	=> IDEM a -> (e -> IO a) -> IDEM a
catchIDE block handler = reifyIDE (\ideR -> catch (reflectIDE block ideR) handler)

--
-- | A shorthand for a reader monad for a mutable reference to the IDE state
--   which does not return a value
--
type IDEAction = IDEM ()

instance PaneMonad IDEM where
    getWindowSt     =   readIDE window
    getUIManagerSt  =   readIDE uiManager
    getPanesSt      =   readIDE panes
    getPaneMapSt    =   readIDE paneMap
    getActivePaneSt =   readIDE activePane
    getLayoutSt     =   readIDE layout
    setPanesSt v    =   modifyIDE_ (\ide -> return ide{panes = v})
    setPaneMapSt v  =   modifyIDE_ (\ide -> return ide{paneMap = v})
    setActivePaneSt v = modifyIDE_ (\ide -> return ide{activePane = v})
    setLayoutSt v   =   modifyIDE_ (\ide -> return ide{layout = v})

    runInIO f       =   reifyIDE (\ideRef -> return (\v -> reflectIDE (f v) ideRef))



-- ---------------------------------------------------------------------
-- Convenience methods for accesing the IDE State
--

-- | Read an attribute of the contents
readIDE :: (IDE -> beta) -> IDEM beta
readIDE f = do
    e <- ask
    liftIO $ liftM f (readIORef e)

-- | Modify the contents, using an IO action.
modifyIDE_ :: (IDE -> IO IDE) -> IDEM ()
modifyIDE_ f = do
    e <- ask
    e' <- liftIO $ (f =<< readIORef e)
    liftIO $ writeIORef e e'

-- | Variation on modifyIDE_ that lets you return a value
modifyIDE :: (IDE -> IO (IDE,beta)) -> IDEM beta
modifyIDE f = do
    e <- ask
    (e',result) <- liftIO (f =<< readIORef e)
    liftIO $ writeIORef e e'
    return result

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
        modifyIDE_ (\ide -> return ide{guiHistory = (True,l,n)})
        act
        (b,l,n) <- readIDE guiHistory
        modifyIDE_ (\ide -> return ide{guiHistory = (False,l,n)})
        else act

-- ---------------------------------------------------------------------
-- Activating and deactivating Panes.
-- This is here and not in Views because it needs some dependencies
-- (e.g. Events for history)
--

activatePane :: Pane alpha IDEM => alpha -> Connections -> IDEAction
activatePane pane conn = do
    mbAP <- getActivePaneSt
    case mbAP of
        Just (pn,_) | pn == paneName pane -> return ()
        _  -> do
            deactivatePaneWithout
            sb <- getSBActivePane
            liftIO $ statusbarPop sb 1
            liftIO $ statusbarPush sb 1 (paneName pane)
            liftIO $ bringPaneToFront pane
            setActivePaneSt (Just (paneName pane,conn))
            trigger (Just (paneName pane)) (case mbAP of
                                                    Nothing -> Nothing
                                                    Just (pn,_) -> Just pn)
            modifyIDE_ (\ide -> return ide{recentPanes =
                paneName pane : filter (/= paneName pane) (recentPanes ide)})
            return ()

trigger :: Maybe String -> Maybe String -> IDEAction
trigger s1 s2 = do
    st <- ask
    triggerEvent st (RecordHistory ((PaneSelected s1), PaneSelected s2))
    triggerEvent st (Sensitivity [(SensitivityEditor, False)])
    return ()

deactivatePane :: IDEAction
deactivatePane = do
    ideR    <-  ask
    mbAP    <-  getActivePaneSt
    case mbAP of
        Nothing      -> return ()
        Just (pn, _) -> do
            deactivatePaneWithout
            triggerEvent ideR (RecordHistory (PaneSelected Nothing,
                PaneSelected (Just pn)))
            triggerEvent ideR (Sensitivity [(SensitivityEditor, False)])
            return ()

deactivatePaneWithout :: IDEAction
deactivatePaneWithout = do
    sb <- getSBActivePane
    liftIO $ statusbarPop sb 1
    liftIO $ statusbarPush sb 1 ""
    mbAP    <-  getActivePaneSt
    case mbAP of
        Just (_,signals) -> liftIO $do
            signalDisconnectAll signals
        Nothing -> return ()
    setActivePaneSt Nothing

deactivatePaneIfActive :: Pane alpha IDEM  => alpha -> IDEAction
deactivatePaneIfActive pane = do
    mbActive <- getActivePaneSt
    case mbActive of
        Nothing -> return ()
        Just (n,_) -> if n == paneName pane
                        then deactivatePane
                        else return ()

closePane :: Pane alpha IDEM  => alpha -> IDEAction
closePane pane = do
    (panePath,_)    <-  guiPropertiesFromName (paneName pane)
    nb              <-  getNotebook panePath
    mbI             <-  liftIO $notebookPageNum nb (getTopWidget pane)
    case mbI of
        Nothing ->  liftIO $ do
            sysMessage Normal "notebook page not found: unexpected"
            return ()
        Just i  ->  do
            deactivatePaneIfActive pane
            liftIO $ do
                notebookRemovePage nb i
                widgetDestroy (getTopWidget pane)
            removePaneAdmin pane
            modifyIDE_ (\ide -> return ide{recentPanes = filter (/= paneName pane) (recentPanes ide)})

-- get widget elements (menu)

getCandyState :: PaneMonad alpha => alpha  (Bool)
getCandyState = do
    ui <- getUIAction "ui/menubar/_Edit/Source Candy" castToToggleAction
    liftIO $toggleActionGetActive ui

setCandyState :: PaneMonad alpha => Bool -> alpha ()
setCandyState b = do
    ui <- getUIAction "ui/menubar/_Edit/Source Candy" castToToggleAction
    liftIO $toggleActionSetActive ui b

getForgetSession :: PaneMonad alpha => alpha  (Bool)
getForgetSession = do
    ui <- getUIAction "ui/menubar/_Session/Forget Session" castToToggleAction
    liftIO $toggleActionGetActive ui

getMenuItem :: String -> IDEM MenuItem
getMenuItem path = do
    uiManager' <- readIDE uiManager
    mbWidget   <- liftIO $ uiManagerGetWidget uiManager' path
    case mbWidget of
        Nothing     -> throwIDE ("State.hs>>getMenuItem: Can't find ui path " ++ path)
        Just widget -> return (castToMenuItem widget)

getRecentFiles , getRecentPackages :: IDEM MenuItem
getRecentFiles    = getMenuItem "ui/menubar/_File/_Recent Files"
getRecentPackages = getMenuItem "ui/menubar/_Package/_Recent Packages"

-- (toolbar)

getSBSpecialKeys :: PaneMonad alpha => alpha Statusbar
getSBSpecialKeys   = widgetGet ["topBox","statusBox","statusBarSpecialKeys"] castToStatusbar

getSBActivePane :: PaneMonad alpha => alpha Statusbar
getSBActivePane    = widgetGet ["topBox","statusBox","statusBarActivePane"] castToStatusbar

getSBActivePackage :: PaneMonad alpha => alpha Statusbar
getSBActivePackage = widgetGet ["topBox","statusBox","statusBarActiveProject"] castToStatusbar

getSBErrors :: PaneMonad alpha => alpha Statusbar
getSBErrors        = widgetGet ["topBox","statusBox","statusBarErrors"] castToStatusbar

getStatusbarIO :: PaneMonad alpha => alpha Statusbar
getStatusbarIO     =  widgetGet ["topBox","statusBox","statusBarInsertOverwrite"] castToStatusbar

getStatusbarLC :: PaneMonad alpha => alpha Statusbar
getStatusbarLC     = widgetGet ["topBox","statusBox","statusBarLineColumn"] castToStatusbar




