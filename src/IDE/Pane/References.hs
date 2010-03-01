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
    ReferencesState
,   IDEReferences
,   referencedFrom
,   getReferences
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
import IDE.Metainfo.Provider
       (getSystemInfo, getWorkspaceInfo, getPackageInfo)


-- | A References pane description
--

data IDEReferences     =   IDEReferences {
    scrolledView        ::   ScrolledWindow
,   treeViewC           ::   TreeView
,   referencesDescr     ::   IORef (Maybe Descr)
,   referencesStore     ::   ListStore (ModuleDescr,String)
,   refScopeRef         ::   IORef Scope
,   referencesEntry     ::   Entry
,   topBox              ::   VBox
} deriving Typeable

data ReferencesState           =   ReferencesState {
    refTo               ::  Maybe Descr
,   refScope            ::  Scope}
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEReferences IDEM
    where
    primPaneName _  =   "References"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . topBox
    paneId b        =   "*References"

-- |
instance RecoverablePane IDEReferences ReferencesState IDEM where
    saveState p     =   do
        mbDescr <-  liftIO $ readIORef (referencesDescr p)
        scope   <-  liftIO $ readIORef (refScopeRef p)
        return (Just (ReferencesState mbDescr scope))
    recoverState pp (ReferencesState mbDescr scope) =  do
            nb  <-  getNotebook pp
            p   <-  buildPane pp nb builder
            scopeSelection scope
            when (isJust mbDescr) (referencedFrom (fromJust mbDescr))
            return p
    builder pp nb windows = reifyIDE $ \ ideR -> do
            scopebox        <-  hBoxNew True 2
            rb1             <-  radioButtonNewWithLabel "Package"
            rb2             <-  radioButtonNewWithLabelFromWidget rb1 "Workspace"
            rb3             <-  radioButtonNewWithLabelFromWidget rb1 "System"
            toggleButtonSetActive rb3 True
            cb2             <-  checkButtonNewWithLabel "Imports"

            boxPackStart scopebox rb1 PackGrow 2
            boxPackStart scopebox rb2 PackGrow 2
            boxPackStart scopebox rb3 PackGrow 2
            boxPackEnd scopebox cb2 PackNatural 2

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
                $ \row -> [ cellText := render $ disp $ modu $ mdModuleId $ fst row]
            cellLayoutSetAttributes col renderer0 listStore
                $ \row -> [cellPixbufStockId  :=
                            if isJust (mdMbSourcePath $ fst row)
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
                $ \row -> [ cellText := render $ disp $ pack $ mdModuleId $ fst row]

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
            scopeRef <- newIORef SystemScope
            let references = IDEReferences sw treeView referencesDescr' listStore scopeRef entry box
            widgetShowAll box
            cid1 <- treeView `afterFocusIn`
                (\_ -> do reflectIDE (makeActive references) ideR ; return True)
            treeView `onButtonPress` (treeViewPopup ideR  references)
            rb1 `onToggled` (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR )
            rb2 `onToggled` (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR )
            rb3 `onToggled` (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR )
            cb2 `onToggled` (reflectIDE (scopeSelection' rb1 rb2 rb3 cb2) ideR)
            return (Just references,[ConnectC cid1])

getReferences :: Maybe PanePath -> IDEM IDEReferences
getReferences Nothing    = forceGetPane (Right "*References")
getReferences (Just pp)  = forceGetPane (Left pp)


-- | Open a pane with the references of this identifier
referencedFrom :: Descr  -> IDEAction
referencedFrom idDescr =
    case dsMbModu idDescr of
        Nothing -> return ()
        Just pm -> do
            references      <-  getReferences Nothing
            scope           <- liftIO $ getScope references
            mbPackageInfo   <- getPackageInfo
            mbWorkspaceInfo <- getWorkspaceInfo
            mbSystemInfo    <- getSystemInfo
            let packages    =  packagesFromScope scope mbPackageInfo mbWorkspaceInfo mbSystemInfo
            let modulesList = modulesForCallerFromPackages packages (dscName idDescr, modu pm)
            liftIO $ do
                writeIORef (referencesDescr references) (Just idDescr)
                listStoreClear (referencesStore references)
                mapM_ (listStoreAppend (referencesStore references))
                    $ sort $ zip modulesList (repeat (dscName idDescr))
                entrySetText (referencesEntry references)
                    $   dscName idDescr ++
                        " << " ++ showPackModule pm
                bringPaneToFront references

packagesFromScope SystemScope
    _
    (Just (_,GenScopeC (PackScope p1 _)))
    (Just (GenScopeC (PackScope p2 _)))       = Map.elems p1 ++ Map.elems p2
packagesFromScope SystemScope
    _
    (Just (GenScopeC (PackScope p1 _),GenScopeC (PackScope p2 _)))
    _                                       = Map.elems p1 ++ Map.elems p2
packagesFromScope (WorkspaceScope True)
    _
    (Just (GenScopeC (PackScope p1 _),GenScopeC (PackScope p2 _)))
    _                                       = Map.elems p1 ++ Map.elems p2
packagesFromScope (WorkspaceScope False)
    _
    (Just (GenScopeC (PackScope p1 _),_))
    _                                       = Map.elems p1
packagesFromScope (PackageScope True)
    (Just (GenScopeC (PackScope p1 _),GenScopeC (PackScope p2 _)))
    _
    _                                       = Map.elems p1 ++ Map.elems p2
packagesFromScope (PackageScope False)
    (Just (GenScopeC (PackScope p1 _),_))
    _
    _                                       = Map.elems p1
packagesFromScope _ _ _  _ = []

modulesForCallerFromPackages :: [PackageDescr] -> (String,ModuleName) -> [ModuleDescr]
modulesForCallerFromPackages []        _            =  []
modulesForCallerFromPackages (p :rest) (sym,mod)    =
    (filter (\ md -> case mod `Map.lookup` (mdReferences md) of
                        Nothing     -> False
                        Just syms   -> sym `Set.member` syms) (pdModules p))
        ++ modulesForCallerFromPackages rest (sym,mod)

scopeSelection' rb1 rb2 rb3 cb2 = do
    scope <- liftIO $ do
        withImports <-  toggleButtonGetActive cb2
        s1 <- toggleButtonGetActive rb1
        s2 <- toggleButtonGetActive rb2
        s3 <- toggleButtonGetActive rb3
        if s1
            then return (PackageScope withImports)
            else if s2
                    then return (WorkspaceScope withImports)
                    else return (SystemScope)
    scopeSelection scope

scopeSelection :: Scope -> IDEAction
scopeSelection scope = do
    refs <- getReferences Nothing
    liftIO $ writeIORef (refScopeRef refs) scope
    mbDescr <- liftIO $ readIORef (referencesDescr refs)
    case mbDescr of
        Nothing -> return ()
        Just descr -> referencedFrom descr

getScope :: IDEReferences -> IO Scope
getScope refs = readIORef (refScopeRef refs)


getSelectionTree ::  TreeView
    -> ListStore (ModuleDescr,String)
    -> IO (Maybe (ModuleDescr,String))
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
                    Just (m,_) -> case mdMbSourcePath m of
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
                                -> case mdMbSourcePath m of
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


