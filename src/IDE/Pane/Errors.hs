{-# OPTIONS_GHC -XRecordWildCards -XTypeSynonymInstances -XMultiParamTypeClasses
    -XDeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Errors
-- Copyright   :  2007-2009 Juergen Nicklisch-Franken, Hamish Mackenzie
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
,   showErrors
,   fillErrorList
,   selectError
) where

import Graphics.UI.Gtk
import Data.Typeable (Typeable(..))
import IDE.Core.State
import Control.Monad.Reader
import IDE.LogRef
import Graphics.UI.Gtk.General.Enums
    (Click(..), MouseButton(..))
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import IDE.ImportTool (addImport, parseNotInScope, addAllImports)
import Data.List (elemIndex)


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


instance IDEObject IDEErrors

instance Pane IDEErrors IDEM
    where
    primPaneName _  =   "Errors"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Errors"
    makeActive pane =   activatePane pane []
    close           =   closePane

instance RecoverablePane IDEErrors ErrorsState IDEM where
    saveState p     =   do
        return (Just ErrorsState)
    recoverState pp ErrorsState =   do
        nb      <-  getNotebook pp
        newPane pp nb builder
        fillErrorList
        return ()

showErrors :: IDEAction
showErrors = do
    m <- getErrors
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (treeView m)

getErrors :: IDEM IDEErrors
getErrors = do
    mbErrors <- getPane
    case mbErrors of
        Nothing -> do
            pp          <-  getBestPathForId "*Errors"
            nb          <-  getNotebook pp
            newPane pp nb builder
            fillErrorList
            mbErrors <- getPane
            case mbErrors of
                Nothing ->  throwIDE "Can't init errors"
                Just m  ->  return m
        Just m ->   return m

builder :: PanePath ->
    Notebook ->
    Window ->
    IDERef ->
    IO (IDEErrors, Connections)
builder pp nb windows ideR = do
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
    return (pane,[ConnectC cid1])

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
    errors     <- getErrors
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
            item0           <-  menuItemNewWithLabel "Add all imports"
            item0 `onActivateLeaf` do
                reflectIDE addAllImports ideR
            menuShellAppend theMenu item0
            case mbSel of
                Just sel ->
                    case parseNotInScope (refDescription sel) of
                    Nothing   -> do
                        return ()
                    Just _  -> do
                        item1   <-  menuItemNewWithLabel "Add import"
                        item1 `onActivateLeaf` do
                            reflectIDE (addImport sel [] >> return()) ideR
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






