{-# OPTIONS_GHC -XRecordWildCards -XTypeSynonymInstances -XMultiParamTypeClasses
    -XDeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Errors
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | A pane which displays a list of errors
--
-----------------------------------------------------------------------------

module IDE.Pane.Errors (
    IDEErrors
,   ErrorsState
,   fillErrorList
,   selectError
,   getErrors
) where

import Graphics.UI.Gtk
import Data.Typeable (Typeable(..))
import IDE.Core.State
import Control.Monad.Reader
import Graphics.UI.Gtk.General.Enums
    (Click(..), MouseButton(..))
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import IDE.ImportTool
       (addPackage, parseHiddenModule, addImport, parseNotInScope,
        resolveErrors)
import Data.List (elemIndex)
import IDE.LogRef (showSourceSpan)

-- | A breakpoints pane description
--
data IDEErrors      =   IDEErrors {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   errorStore      ::   TreeStore ErrColumn
} deriving Typeable

data ErrColumn = ErrColumn {logRef :: LogRef, string :: String, index :: Int}

data ErrorsState    =   ErrorsState {
}   deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEErrors IDEM
    where
    primPaneName _  =   "Errors"
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Errors"

instance RecoverablePane IDEErrors ErrorsState IDEM where
    saveState p     =   do
        return (Just ErrorsState)
    recoverState pp ErrorsState =   do
        nb      <-  getNotebook pp
        p <-    buildPane pp nb builder
        fillErrorList
        return p
    builder = builder'

builder' :: PanePath ->
    Notebook ->
    Window ->
    IDEM (Maybe IDEErrors, Connections)
builder' pp nb windows = reifyIDE $ \ ideR -> do
    errorStore <-  treeStoreNew []
    treeView    <-  treeViewNew
    treeViewSetModel treeView errorStore

    rendererA    <- cellRendererTextNew
    colA         <- treeViewColumnNew
    treeViewColumnSetTitle colA "Location"
    treeViewColumnSetSizing colA TreeViewColumnAutosize
    treeViewColumnSetResizable colA True
    treeViewColumnSetReorderable colA True
    treeViewAppendColumn treeView colA
    cellLayoutPackStart colA rendererA False
    cellLayoutSetAttributes colA rendererA errorStore
        $ \row -> [cellText := if index row == 0 then showSourceSpan (logRef row) else "",
                   cellTextForeground := if (logRefType (logRef row)) == WarningRef
                                            then "green"
                                            else "red" ]
    rendererB    <- cellRendererTextNew
    colB         <- treeViewColumnNew
    treeViewColumnSetTitle colB "Description"
    treeViewColumnSetSizing colB TreeViewColumnAutosize
    treeViewColumnSetResizable colB True
    treeViewColumnSetReorderable colB True
    treeViewAppendColumn treeView colB
    cellLayoutPackStart colB rendererB False
    cellLayoutSetAttributes colB rendererB errorStore
        $ \row -> [ cellText := string row]

    treeViewSetHeadersVisible treeView True
    selB <- treeViewGetSelection treeView
    treeSelectionSetMode selB SelectionSingle
    scrolledView <- scrolledWindowNew Nothing Nothing
    containerAdd scrolledView treeView
    scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic
    let pane = IDEErrors scrolledView treeView errorStore
    treeView `onButtonPress` (errorViewPopup ideR errorStore treeView)
    cid1 <- treeView `afterFocusIn`
        (\_ -> do reflectIDE (makeActive pane) ideR ; return True)
    return (Just pane,[ConnectC cid1])

getErrors :: Maybe PanePath -> IDEM IDEErrors
getErrors Nothing    = forceGetPane (Right "*Errors")
getErrors (Just pp)  = forceGetPane (Left pp)

fillErrorList :: IDEAction
fillErrorList = do
    mbErrors <- getPane
    case mbErrors of
        Nothing -> return ()
        Just pane  -> do
            refs <- readIDE errorRefs
            liftIO $ do
                treeStoreClear (errorStore pane)
                mapM_ (insertError (errorStore pane)) (zip refs [0..length refs])
    where
    insertError treeStore (lr,index) =
        case {--lines--} [refDescription lr] of
            [] -> treeStoreInsert treeStore [] index (ErrColumn lr "" 0)
            h:t -> do
                treeStoreInsert treeStore [] index (ErrColumn lr h 0)
                mapM_ (\(line,subind) ->
                    treeStoreInsert treeStore [index] subind (ErrColumn lr line (subind + 1)))
                        (zip t [0..length t])

getSelectedError ::  TreeView
    -> TreeStore ErrColumn
    -> IO (Maybe LogRef)
getSelectedError treeView treeStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        a:r ->  do
            val     <-  treeStoreGetValue treeStore a
            return (Just (logRef val))
        _  ->  return Nothing

selectError :: Maybe LogRef -> IDEAction
selectError mbLogRef = do
    errorRefs' <- readIDE errorRefs
    errors     <- getErrors Nothing
    liftIO $ do
        selection <- treeViewGetSelection (treeView errors)
        case mbLogRef of
            Nothing -> treeSelectionUnselectAll selection
            Just lr -> case lr `elemIndex` errorRefs' of
                        Nothing  -> return ()
                        Just ind -> treeSelectionSelectPath selection [ind]

errorViewPopup :: IDERef
    -> TreeStore ErrColumn
    -> TreeView
    -> Event
    -> IO Bool
errorViewPopup ideR  store treeView (Button _ click _ _ _ _ button _ _)
    = do
    if button == RightButton
        then do
            mbSel           <-  getSelectedError treeView store
            theMenu         <-  menuNew
            item0           <-  menuItemNewWithLabel "Resolve Errors"
            item0 `onActivateLeaf` do
                reflectIDE resolveErrors ideR
            menuShellAppend theMenu item0
            case mbSel of
                Just sel -> do
                    case parseNotInScope (refDescription sel) of
                        Nothing   -> do
                            return ()
                        Just _  -> do
                            item1   <-  menuItemNewWithLabel "Add Import"
                            item1 `onActivateLeaf` do
                                reflectIDE (addImport sel [] (\ _ -> return ())) ideR
                            menuShellAppend theMenu item1
                    case parseHiddenModule (refDescription sel) of
                        Nothing   -> do
                            return ()
                        Just _  -> do
                            item1   <-  menuItemNewWithLabel "Add Package"
                            item1 `onActivateLeaf` do
                                reflectIDE (addPackage sel >> return ()) ideR
                            menuShellAppend theMenu item1
                Nothing -> return ()
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then liftIO $ do
                        sel         <-  getSelectedError treeView store
                        case sel of
                            Just ref      -> reflectIDE (setCurrentError (Just ref)) ideR
                            otherwise     -> sysMessage Normal "Error>> errorViewPopup: no selection2"
                        return True
                else return False
errorViewPopup _ _ _ _ = throwIDE "errorViewPopup wrong event type"






