{-# OPTIONS_GHC -XRecordWildCards -XTypeSynonymInstances -XMultiParamTypeClasses
    -XDeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Variables
-- Copyright   :  2007-2009 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Pane.Variables (
    IDEVariables
,   VariablesState
,   showVariables
,   fillVariablesList
) where

import Graphics.UI.Gtk
import Data.Typeable (Typeable(..))
import IDE.Core.State
import Control.Monad.Reader
import IDE.Debug (debugCommand')
import IDE.Tool (ToolOutput(..))

-- | A variables pane description
--
data IDEVariables    =   IDEVariables {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   variables       ::   ListStore String
} deriving Typeable

data VariablesState  =   VariablesState {
}   deriving(Eq,Ord,Read,Show,Typeable)


instance IDEObject IDEVariables

instance Pane IDEVariables IDEM
    where
    primPaneName _  =   "Variables"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Variables"
    makeActive pane =   activatePane pane []
    close           =   closePane

instance RecoverablePane IDEVariables VariablesState IDEM where
    saveState p     =   do
        return (Just VariablesState)
    recoverState pp VariablesState =   do
        nb      <-  getNotebook pp
        newPane pp nb builder
        return ()

showVariables :: IDEAction
showVariables = do
    m <- getVariables
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (treeView m)

getVariables :: IDEM IDEVariables
getVariables = do
    mbVar <- getPane
    case mbVar of
        Nothing -> do
            pp          <-  getBestPathForId "*Variables"
            nb          <-  getNotebook pp
            newPane pp nb builder
            mbVar <- getPane
            case mbVar of
                Nothing ->  throwIDE "Can't init variables"
                Just m  ->  return m
        Just m ->   return m

builder :: PanePath ->
    Notebook ->
    Window ->
    IDERef ->
    IO (IDEVariables, Connections)
builder pp nb windows ideR = do
    variables   <-  listStoreNew []
    treeView    <-  treeViewNew
    treeViewSetModel treeView variables

    renderer    <- cellRendererTextNew
    col         <- treeViewColumnNew
    treeViewColumnSetTitle col "Variables"
    treeViewColumnSetSizing col TreeViewColumnAutosize
    treeViewAppendColumn treeView col
    cellLayoutPackStart col renderer False
    cellLayoutSetAttributes col renderer variables
        $ \row -> [ cellText := row]

    treeViewSetHeadersVisible treeView False
    sel <- treeViewGetSelection treeView
    treeSelectionSetMode sel SelectionSingle

    scrolledView <- scrolledWindowNew Nothing Nothing
    containerAdd scrolledView treeView
    scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic

    let pane = IDEVariables {..}

    cid1 <- treeView `afterFocusIn`
        (\_ -> do reflectIDE (makeActive pane) ideR ; return True)

    return (pane,[ConnectC cid1])


fillVariablesList :: IDEAction
fillVariablesList = do
    mbVariables <- getPane
    case mbVariables of
        Nothing -> return ()
        Just deb -> do
            liftIO $ listStoreClear (variables deb)
            debugCommand' ":show bindings" (\to -> liftIO
                $ postGUIAsync
                    $ mapM_ (listStoreAppend (variables deb))
                        $ concatMap selectString to)
    where
    selectString :: ToolOutput -> [String]
    selectString (ToolOutput str)   = lines str
    selectString _                  = []


