{-# OPTIONS_GHC -XDeriveDataTypeable -XMultiParamTypeClasses -XTypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.References
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability  :  portable
--
-- | The pane of the ide where references to an identifier are presented
--
-------------------------------------------------------------------------------

module IDE.Pane.References (
    IDE(..)
,   ReferencesState
,   referencedFrom
,   showReferences
) where

import Graphics.UI.Gtk hiding (get)
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


-- | A References pane description
--

data IDEReferences     =   IDEReferences {
    scrolledView        ::   ScrolledWindow
,   treeViewC           ::   TreeView
,   referencesDescr     ::   IORef (Maybe Descr)
,   referencesStore     ::   ListStore (ModuleDescr,Symbol)
,   refScopeRef         ::   IORef Scope
,   referencesEntry     ::   Entry
,   topBox              ::   VBox
} deriving Typeable

data ReferencesState           =   ReferencesState {
    refTo               ::  Maybe Descr
,   refScope            ::  Scope}
    deriving(Eq,Ord,Read,Show,Typeable)


instance IDEObject IDEReferences

instance Pane IDEReferences IDEM
    where
    primPaneName _  =   "References"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . topBox
    paneId b        =   "*References"
    makeActive p    =   activatePane p []
    close           =   closePane


-- | We don't recover this pane
instance RecoverablePane IDEReferences ReferencesState IDEM where
    saveState p     =   do
        mbDescr <-  liftIO $ readIORef (referencesDescr p)
        scope   <-  liftIO $ readIORef (refScopeRef p)
        return (Just (ReferencesState mbDescr scope))
    recoverState pp (ReferencesState mbDescr scope) =   do
        nb      <-  getNotebook pp
        initReferences pp nb (Just scope)
        when (isJust mbDescr) (referencedFrom (fromJust mbDescr))

showReferences :: IDEAction
showReferences = do
    m <- getReferences
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (treeViewC m)


-- | Open a pane with the references of this identifier
referencedFrom :: Descr  -> IDEAction
referencedFrom idDescr =
    case descrModu' idDescr of
        Nothing -> return ()
        Just pm -> do
            references   <-  getReferences
            scope <- liftIO $ getScope references
            mbCurrentInfo   <- readIDE currentInfo
            mbAccessibleInfo <- readIDE accessibleInfo
            packages <- case scope of
                            System  ->  case mbAccessibleInfo of
                                            Nothing -> case mbCurrentInfo of
                                                            Nothing             ->  return []
                                                            Just currentInfo    ->  return
                                                                        ((Map.elems . fst . fst) currentInfo)
                                            Just scope -> case mbCurrentInfo of
                                                            Nothing             ->  return ((Map.elems . fst) scope)
                                                            Just currentInfo    ->  return
                                                                ((Map.elems . fst . fst) currentInfo
                                                                ++ (Map.elems . fst) scope)
                            Package ->  case mbCurrentInfo of
                                            Nothing             ->  return []
                                            Just currentInfo    ->  return ((Map.elems . fst . fst) currentInfo
                                                    ++  (Map.elems . fst . snd) currentInfo)
                            Local   ->  case mbCurrentInfo of
                                            Nothing             ->  return []
                                            Just currentInfo    ->  return ((Map.elems . fst . fst) currentInfo)
            let modulesList = modulesForCallerFromPackages packages (descrName idDescr, modu pm)
            liftIO $ do
                writeIORef (referencesDescr references) (Just idDescr)
                listStoreClear (referencesStore references)
                mapM_ (listStoreAppend (referencesStore references))
                    $ sort $ zip modulesList (repeat (descrName idDescr))
                entrySetText (referencesEntry references)
                    $   descrName idDescr ++
                        " << " ++ showPackModule pm
                bringPaneToFront references

modulesForCallerFromPackages :: [PackageDescr] -> (Symbol,ModuleName) -> [ModuleDescr]
modulesForCallerFromPackages []        _            =  []
modulesForCallerFromPackages (p :rest) (sym,mod)    =
    (filter (\ md -> case mod `Map.lookup` (referencesMD md) of
                        Nothing     -> False
                        Just syms   -> sym `Set.member` syms) (exposedModulesPD p))
        ++ modulesForCallerFromPackages rest (sym,mod)

getReferences :: IDEM IDEReferences
getReferences = do
    mbMod <- getPane
    case mbMod of
        Nothing -> do
            pp          <-  getBestPathForId "*References"
            nb          <-  getNotebook pp
            initReferences pp nb Nothing
            mbMod <- getPane
            case mbMod of
                Nothing ->  throwIDE "Can't init references"
                Just m  ->  return m
        Just m ->   return m

scopeSelection :: Scope -> IDEAction
scopeSelection scope = do
    refs <- getReferences
    liftIO $ writeIORef (refScopeRef refs) scope
    mbDescr <- liftIO $ readIORef (referencesDescr refs)
    case mbDescr of
        Nothing -> return ()
        Just descr -> referencedFrom descr

getScope :: IDEReferences -> IO Scope
getScope refs = readIORef (refScopeRef refs)

initReferences :: PanePath -> Notebook -> Maybe Scope -> IDEAction
initReferences panePath nb mbScope = do
    let scope = case mbScope of
                    Just s -> s
                    Nothing -> Package
    newPane panePath nb (builder scope)
    return ()

builder :: Scope ->
    PanePath ->
    Notebook ->
    Window ->
    IDERef ->
    IO (IDEReferences,Connections)
builder scope pp nb windows ideR = do
    scopebox        <-  hBoxNew True 2
    rb1             <-  radioButtonNewWithLabel "Local"
    rb2             <-  radioButtonNewWithLabelFromWidget rb1 "Package"
    rb3             <-  radioButtonNewWithLabelFromWidget rb1 "System"
    toggleButtonSetActive
        (case scope of
            Local   -> rb1
            Package -> rb2
            System   -> rb3) True
    boxPackStart scopebox rb1 PackNatural 2
    boxPackStart scopebox rb2 PackNatural 2
    boxPackEnd scopebox rb3 PackNatural 2

    listStore   <-  listStoreNew []
    treeView    <-  treeViewNew
    treeViewSetModel treeView listStore

    renderer0    <- cellRendererPixbufNew
    set renderer0 [ cellPixbufStockId  := stockYes ]

    renderer    <- cellRendererTextNew
    col         <- treeViewColumnNew
    treeViewColumnSetTitle col "Modules"
    treeViewColumnSetSizing col TreeViewColumnAutosize
    treeViewColumnSetResizable col True
    treeViewColumnSetReorderable col True
    treeViewAppendColumn treeView col
    cellLayoutPackStart col renderer0 False
    cellLayoutPackStart col renderer True
    cellLayoutSetAttributes col renderer listStore
        $ \row -> [ cellText := render $ disp $ modu $ moduleIdMD $ fst row]
    cellLayoutSetAttributes col renderer0 listStore
        $ \row -> [cellPixbufStockId  :=
                    if isJust (mbSourcePathMD $ fst row)
                        then stockJumpTo
                        else stockYes]

    renderer2   <- cellRendererTextNew
    col2        <- treeViewColumnNew
    treeViewColumnSetTitle col2 "Packages"
    treeViewColumnSetSizing col2 TreeViewColumnAutosize
    treeViewColumnSetResizable col True
    treeViewColumnSetReorderable col2 True
    treeViewAppendColumn treeView col2
    cellLayoutPackStart col2 renderer2 True
    cellLayoutSetAttributes col2 renderer2 listStore
        $ \row -> [ cellText := render $ disp $ pack $ moduleIdMD $ fst row]

    treeViewSetHeadersVisible treeView True
    sel <- treeViewGetSelection treeView
    treeSelectionSetMode sel SelectionSingle

    sw <- scrolledWindowNew Nothing Nothing
    containerAdd sw treeView
    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
    entry           <-  entryNew
    set entry [ entryEditable := False ]
    box             <-  vBoxNew False 2
    boxPackStart box scopebox PackNatural 0
    boxPackStart box entry PackNatural 0
    boxPackEnd box sw PackGrow 0
    referencesDescr' <- newIORef Nothing
    scopeRef <- newIORef scope
    let references = IDEReferences sw treeView referencesDescr' listStore scopeRef entry box
    widgetShowAll box
    cid1 <- treeView `afterFocusIn`
        (\_ -> do reflectIDE (makeActive references) ideR ; return True)
    treeView `onButtonPress` (treeViewPopup ideR  references)
    rb1 `onToggled` (reflectIDE (scopeSelection Local) ideR )
    rb2 `onToggled` (reflectIDE (scopeSelection Package) ideR )
    rb3 `onToggled` (reflectIDE (scopeSelection System) ideR )
    return (references,[ConnectC cid1])


getSelectionTree ::  TreeView
    -> ListStore (ModuleDescr,Symbol)
    -> IO (Maybe (ModuleDescr,Symbol))
getSelectionTree treeView listStore = do
    treeSelection   <-  treeViewGetSelection treeView
    rows           <-  treeSelectionGetSelectedRows treeSelection
    case rows of
        [[n]]   ->  do
            val     <-  listStoreGetValue listStore n
            return (Just val)
        _       ->  return Nothing

treeViewPopup :: IDERef
    -> IDEReferences
    -> Event
    -> IO (Bool)
treeViewPopup ideR  references (Button _ click _ _ _ _ button _ _) = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            item1           <-  menuItemNewWithLabel "Edit"
            item1 `onActivateLeaf` do
                sel         <-  getSelectionTree (treeViewC references) (referencesStore references)
                case sel of
                    Just (m,_) -> case mbSourcePathMD m of
                                    Nothing     ->  return ()
                                    Just fp     ->  do
                                        text <- entryGetText (referencesEntry references)
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
                then do sel         <-  getSelectionTree (treeViewC references)
                                            (referencesStore references)
                        case sel of
                            Just (m,_)
                                -> case mbSourcePathMD m of
                                        Nothing     ->  return False
                                        Just fp     ->  do
                                            text <- entryGetText (referencesEntry references)
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
                            reflectIDE (editFind True True True False text "" Forward) ideR
                            return False)
                                 priorityDefaultIdle
            return True


