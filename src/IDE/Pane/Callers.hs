{-# OPTIONS_GHC -XDeriveDataTypeable -XMultiParamTypeClasses -XTypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Callers
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info at leksah.org>
-- Stability   :  experimental
-- Portability  :  portable
--
-- | The pane of ide where modules are presented in tree form with their
--   packages and exports
--
-------------------------------------------------------------------------------

module IDE.Pane.Callers (
    IDECallers(..)
,   CallersState
,   calledBy
,   showCallers
) where

import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.ModelView as New
import IDE.Find (focusFindEntry,showFindbar,setFindState,entryStr, getFindState, editFind)
import IDE.Pane.SourceBuffer (selectSourceBuf)
import Graphics.UI.Gtk.Gdk.Events
import Data.IORef (newIORef,writeIORef,readIORef,IORef(..))
import Data.Maybe
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Typeable
import Distribution.ModuleName(ModuleName)
import Distribution.Text
import Text.PrettyPrint (render)

import IDE.Core.State


-- | A callers pane description
--

data IDECallers     =   IDECallers {
    scrolledView    ::   ScrolledWindow
,   treeViewC       ::   New.TreeView
,   callersDescr    ::   IORef (Maybe Descr)
,   callersStore    ::   New.ListStore (ModuleDescr,Symbol)
,   callersEntry    ::   Entry
,   topBox          ::   VBox
} deriving Typeable

data CallersState           =   CallersState (Maybe Descr)
    deriving(Eq,Ord,Read,Show,Typeable)


instance IDEObject IDECallers

instance Pane IDECallers IDEM
    where
    primPaneName _  =   "Usage"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . topBox
    paneId b        =   "*Usage"
    makeActive p    =   activatePane p []
    close pane      =   do
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

-- | We don't recover this pane
instance RecoverablePane IDECallers CallersState IDEM where
    saveState p     =   do
        mbDescr <-  liftIO $ readIORef (callersDescr p)
        return (Just (CallersState mbDescr))
    recoverState pp (CallersState mbDescr) =   do
        nb      <-  getNotebook pp
        initCallers pp nb
        when (isJust mbDescr) (calledBy (fromJust mbDescr))

showCallers :: IDEAction
showCallers = do
    m <- getCallers
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (treeViewC m)


-- | Open a pane with the callers of this identifier
calledBy :: Descr  -> IDEAction
calledBy idDescr = do
    mbCurrentInfo   <- readIDE currentInfo
    case mbCurrentInfo of
        Nothing             ->  return ()
        Just currentInfo'   ->
            let symModPair      =   (descrName idDescr, modu $ descrModu idDescr)
                modulesList1    =   modulesForCallerFromPackages
                                        (Map.elems $ fst $ fst currentInfo') symModPair
                modulesList2    =   modulesForCallerFromPackages
                                        (Map.elems $ fst $ snd currentInfo') symModPair
                modulesList     =   nub $ modulesList1 ++ modulesList2
                finalList       =   sort $ zip modulesList (repeat (descrName idDescr))
            in do   callers             <-  getCallers
                    liftIO $ do
                        writeIORef (callersDescr callers) (Just idDescr)
                        New.listStoreClear (callersStore callers)
                        mapM_ (New.listStoreAppend (callersStore callers)) finalList
                        entrySetText (callersEntry callers)
                            $   descrName idDescr ++
                                " << " ++ showPackModule (descrModu idDescr)
                        bringPaneToFront callers

modulesForCallerFromPackages :: [PackageDescr] -> (Symbol,ModuleName) -> [ModuleDescr]
modulesForCallerFromPackages []        _            =  []
modulesForCallerFromPackages (p :rest) (sym,mod)    =
    (filter (\ md -> case mod `Map.lookup` (usagesMD md) of
                        Nothing     -> False
                        Just syms   -> sym `Set.member` syms) (exposedModulesPD p))
        ++ modulesForCallerFromPackages rest (sym,mod)

getCallers :: IDEM IDECallers
getCallers = do
    mbMod <- getPane
    case mbMod of
        Nothing -> do
            prefs       <-  readIDE prefs
            layout      <-  readIDE layout
            let pp      =   getStandardPanePath (modulesPanePath prefs) layout
            nb          <-  getNotebook pp
            initCallers pp nb
            mbMod <- getPane
            case mbMod of
                Nothing ->  throwIDE "Can't init callers"
                Just m  ->  return m
        Just m ->   return m

initCallers :: PanePath -> Notebook -> IDEAction
initCallers panePath nb = do
    panes       <-  readIDE panes
    paneMap     <-  readIDE paneMap
    prefs       <-  readIDE prefs
    currentInfo <-  readIDE currentInfo
    (buf,cids)  <-  reifyIDE $ \ideR  ->  do
        listStore   <-  New.listStoreNew []
        treeView    <-  New.treeViewNew
        New.treeViewSetModel treeView listStore

        renderer0    <- New.cellRendererPixbufNew
        set renderer0 [ cellPixbufStockId  := stockYes ]

        renderer    <- New.cellRendererTextNew
        col         <- New.treeViewColumnNew
        New.treeViewColumnSetTitle col "Modules"
        New.treeViewColumnSetSizing col TreeViewColumnAutosize
        New.treeViewColumnSetResizable col True
        New.treeViewColumnSetReorderable col True
        New.treeViewAppendColumn treeView col
        New.cellLayoutPackStart col renderer0 False
        New.cellLayoutPackStart col renderer True
        New.cellLayoutSetAttributes col renderer listStore
            $ \row -> [ New.cellText := render $ disp $ modu $ moduleIdMD $ fst row]
        New.cellLayoutSetAttributes col renderer0 listStore
            $ \row -> [cellPixbufStockId  :=
                        if isJust (mbSourcePathMD $ fst row)
                            then stockJumpTo
                            else stockYes]

        renderer2   <- New.cellRendererTextNew
        col2        <- New.treeViewColumnNew
        New.treeViewColumnSetTitle col2 "Packages"
        New.treeViewColumnSetSizing col2 TreeViewColumnAutosize
        New.treeViewColumnSetResizable col True
        New.treeViewColumnSetReorderable col2 True
        New.treeViewAppendColumn treeView col2
        New.cellLayoutPackStart col2 renderer2 True
        New.cellLayoutSetAttributes col2 renderer2 listStore
            $ \row -> [ New.cellText := render $ disp $ pack $ moduleIdMD $ fst row]

        New.treeViewSetHeadersVisible treeView True
        sel <- New.treeViewGetSelection treeView
        New.treeSelectionSetMode sel SelectionSingle

        sw <- scrolledWindowNew Nothing Nothing
        containerAdd sw treeView
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        entry           <-  entryNew
        set entry [ entryEditable := False ]
        box             <-  vBoxNew False 2
        boxPackStart box entry PackNatural 0
        boxPackEnd box sw PackGrow 0
        callersDescr' <- newIORef Nothing
        let callers = IDECallers sw treeView callersDescr' listStore entry box
        notebookInsertOrdered nb box (paneName callers) Nothing
        widgetShowAll box
        cid1 <- treeView `afterFocusIn`
            (\_ -> do reflectIDE (makeActive callers) ideR ; return True)
        treeView `onButtonPress` (treeViewPopup ideR  callers)
        return (callers,[ConnectC cid1])
    addPaneAdmin buf cids panePath
    liftIO $widgetGrabFocus (scrolledView buf)


getSelectionTree ::  New.TreeView
    -> New.ListStore (ModuleDescr,Symbol)
    -> IO (Maybe (ModuleDescr,Symbol))
getSelectionTree treeView listStore = do
    treeSelection   <-  New.treeViewGetSelection treeView
    rows           <-  New.treeSelectionGetSelectedRows treeSelection
    case rows of
        [[n]]   ->  do
            val     <-  New.listStoreGetValue listStore n
            return (Just val)
        _       ->  return Nothing

treeViewPopup :: IDERef
    -> IDECallers
    -> Event
    -> IO (Bool)
treeViewPopup ideR  callers (Button _ click _ _ _ _ button _ _) = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            item1           <-  menuItemNewWithLabel "Edit"
            item1 `onActivateLeaf` do
                sel         <-  getSelectionTree (treeViewC callers) (callersStore callers)
                case sel of
                    Just (m,_) -> case mbSourcePathMD m of
                                    Nothing     ->  return ()
                                    Just fp     ->  do
                                        text <- entryGetText (callersEntry callers)
                                        case words text of
                                            (hd : tl)    ->  do
                                                reflectIDE (selectText fp hd) ideR
                                                return ()
                                            _           ->  return ()
                    otherwise       ->  return ()
            menuShellAppend theMenu item1
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then do sel         <-  getSelectionTree (treeViewC callers)
                                            (callersStore callers)
                        case sel of
                            Just (m,_)
                                -> case mbSourcePathMD m of
                                        Nothing     ->  return False
                                        Just fp     ->  do
                                            text <- entryGetText (callersEntry callers)
                                            case words text of
                                                (hd : tl) -> do
                                                    reflectIDE (selectText fp hd) ideR
                                                    return True
                                                _ -> return False
                            otherwise       ->  return False
                else return False
treeViewPopup _ _ _ = throwIDE "treeViewPopup wrong event type"

selectText :: FilePath -> String -> IDEM Bool
selectText fp text = do
    res <- selectSourceBuf fp
    fs  <- getFindState
    case res of
        Nothing -> return False
        Just _  -> do
            setFindState fs {entryStr = text}
            showFindbar
            focusFindEntry
            reifyIDE $ \ideR  ->  do
                        idleAdd  (do
                            reflectIDE (editFind True True True text "" Forward) ideR
                            return False)
                                 priorityDefaultIdle
            return True


