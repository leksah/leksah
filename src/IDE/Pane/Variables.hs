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
import IDE.Debug (debugCommand, debugCommand')
import IDE.Tool (ToolOutput(..))
import Text.ParserCombinators.Parsec
    (anyChar, (<?>), char, noneOf, many, CharParser(..), parse)
import qualified Text.ParserCombinators.Parsec.Token as  P
    (symbol,
     makeTokenParser)
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Data.Either (rights, lefts)
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import Graphics.UI.Gtk.General.Enums
    (Click(..), MouseButton(..))
import IDE.LogRef (logOutput)

-- | A variables pane description
--
data IDEVariables    =   IDEVariables {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   variables       ::   TreeStore VarDescription
} deriving Typeable

data VarDescription = VarDescription {
    varName         ::  String
,   varType         ::  String
,   varValue        ::  String}

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
    variables   <-  treeStoreNew []
    treeView    <-  treeViewNew
    treeViewSetModel treeView variables

    renderer1    <- cellRendererTextNew
    col1         <- treeViewColumnNew
    treeViewColumnSetTitle col1 "Name"
    treeViewColumnSetSizing col1 TreeViewColumnAutosize
    treeViewColumnSetResizable col1 True
    treeViewColumnSetReorderable col1 True
    treeViewAppendColumn treeView col1
    cellLayoutPackStart col1 renderer1 False
    cellLayoutSetAttributes col1 renderer1 variables
        $ \row -> [ cellText := varName row]

    renderer2    <- cellRendererTextNew
    col2         <- treeViewColumnNew
    treeViewColumnSetTitle col2 "Type"
    treeViewColumnSetSizing col2 TreeViewColumnAutosize
    treeViewColumnSetResizable col2 True
    treeViewColumnSetReorderable col2 True
    treeViewAppendColumn treeView col2
    cellLayoutPackStart col2 renderer2 False
    cellLayoutSetAttributes col2 renderer2 variables
        $ \row -> [ cellText := varType row]

    renderer3    <- cellRendererTextNew
    col3         <- treeViewColumnNew
    treeViewColumnSetTitle col3 "Value"
    treeViewColumnSetSizing col3 TreeViewColumnAutosize
    treeViewColumnSetResizable col3 True
    treeViewColumnSetReorderable col3 True
    treeViewAppendColumn treeView col3
    cellLayoutPackStart col3 renderer3 False
    cellLayoutSetAttributes col3 renderer3 variables
        $ \row -> [ cellText := varValue row]

    treeViewSetHeadersVisible treeView True
    sel <- treeViewGetSelection treeView
    treeSelectionSetMode sel SelectionSingle

    scrolledView <- scrolledWindowNew Nothing Nothing
    containerAdd scrolledView treeView
    scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic

    let pane = IDEVariables scrolledView treeView variables
    treeView `onButtonPress` (variablesViewPopup ideR variables treeView)
    cid1 <- treeView `afterFocusIn`
        (\_ -> do reflectIDE (makeActive pane) ideR ; return True)
    return (pane,[ConnectC cid1])


fillVariablesList :: IDEAction
fillVariablesList = do
    mbVariables <- getPane
    case mbVariables of
        Nothing -> return ()
        Just var -> debugCommand' ":show bindings" (\to -> liftIO
                        $ postGUIAsync (do
                            let triplesOrErrors = map (parse variableParser "") (map selectString to)
                            mapM_ (\ e -> sysMessage Normal (show e)) (lefts triplesOrErrors)
                            treeStoreClear (variables var)
                            mapM_ (insertBreak (variables var))
                                (zip (rights triplesOrErrors) [0..length (rights triplesOrErrors)])))
    where
    selectString :: ToolOutput -> String
    selectString (ToolOutput str)    = str
    selectString _                   = []
    insertBreak treeStore (v,index)  = treeStoreInsert treeStore [] index v

getSelectedVariable ::  TreeView
    -> TreeStore VarDescription
    -> IO (Maybe VarDescription)
getSelectedVariable treeView treeStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        a:r ->  do
            val     <-  treeStoreGetValue treeStore a
            return (Just val)
        _  ->  return Nothing

variableParser :: CharParser () VarDescription
variableParser = do
    varName <- many (noneOf ":")
    symbol "::"
    typeStr  <- many (noneOf "=")
    char '='
    value <- many anyChar
    return (VarDescription varName typeStr value)
    <?> "buildLineParser"

lexer = P.makeTokenParser emptyDef
symbol = P.symbol lexer

variablesViewPopup :: IDERef
    -> TreeStore VarDescription
    -> TreeView
    -> Event
    -> IO (Bool)
variablesViewPopup ideR  store treeView (Button _ click _ _ _ _ button _ _)
    = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            item1           <-  menuItemNewWithLabel "Force"
            item1 `onActivateLeaf` do
                sel         <-  getSelectedVariable  treeView store
                case sel of
                    Just varDescr -> reflectIDE (debugCommand (":force " ++ (varName varDescr))
                                        logOutput) ideR
                    otherwise     -> return ()
            sep1 <- separatorMenuItemNew
            item2           <-  menuItemNewWithLabel "Print"
            item2 `onActivateLeaf` (reflectIDE undefined ideR)
            item3           <-  menuItemNewWithLabel "Update"
            item3 `onActivateLeaf` (reflectIDE fillVariablesList ideR)
            mapM_ (menuShellAppend theMenu) [castToMenuItem item1, castToMenuItem sep1,
                castToMenuItem item2, castToMenuItem item3]
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then do sel         <-  getSelectedVariable treeView store
                        case sel of
                            Just varDescr -> reflectIDE (debugCommand (":force " ++ (varName varDescr))
                                                logOutput) ideR
                            otherwise     -> return ()
                        return True
                else return False
variablesViewPopup _ _ _ _ = throwIDE "breakpointViewPopup wrong event type"
