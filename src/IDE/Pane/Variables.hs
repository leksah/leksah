{-# LANGUAGE FlexibleInstances, RecordWildCards, TypeSynonymInstances,
             MultiParamTypeClasses, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Variables
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
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
,   fillVariablesList
,   fillVariablesListQuiet
) where

import Graphics.UI.Gtk
import Data.Typeable (Typeable(..))
import IDE.Core.State
import IDE.Package (tryDebug, tryDebugQuiet)
import IDE.Debug (debugCommand')
import IDE.Utils.Tool (ToolOutput(..))
import Text.ParserCombinators.Parsec
    (anyChar,
     lookAhead,
     eof,
     (<|>),
     try,
     (<?>),
     char,
     noneOf,
     many,
     CharParser(..),
     parse)
import qualified Text.ParserCombinators.Parsec.Token as  P
    (whiteSpace, symbol, makeTokenParser)
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import Graphics.UI.Gtk.General.Enums
    (Click(..), MouseButton(..))
import IDE.Workspaces (packageTry, packageTryQuiet)
import qualified Data.Enumerator.List as EL (consume)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))

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

instance Pane IDEVariables IDEM
    where
    primPaneName _  =   "Variables"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Variables"

instance RecoverablePane IDEVariables VariablesState IDEM where
    saveState p     =   do
        return (Just VariablesState)
    recoverState pp VariablesState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder = builder'


builder' :: PanePath ->
    Notebook ->
    Window ->
    IDEM (Maybe IDEVariables, Connections)
builder' pp nb windows = reifyIDE $  \ideR -> do
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
    return (Just pane,[ConnectC cid1])


fillVariablesListQuiet :: IDEAction
fillVariablesListQuiet = packageTryQuiet $ do
    mbVariables <- lift getPane
    case mbVariables of
        Nothing -> return ()
        Just var -> tryDebugQuiet $ debugCommand' ":show bindings" $ do
            to <- EL.consume
            liftIO $ postGUIAsync $ do
                case parse variablesParser "" (selectString to) of
                    Left e -> sysMessage Normal (show e)
                    Right triples -> do
                        treeStoreClear (variables var)
                        mapM_ (insertBreak (variables var))
                            (zip triples [0..length triples])
  where
    insertBreak treeStore (v,index)  = treeStoreInsert treeStore [] index v

fillVariablesList :: IDEAction
fillVariablesList = packageTry $ do
    mbVariables <- lift getPane
    case mbVariables of
        Nothing -> return ()
        Just var -> tryDebug $ debugCommand' ":show bindings" $ do
            to <- EL.consume
            liftIO $ postGUIAsync $ do
                case parse variablesParser "" (selectString to) of
                    Left e -> sysMessage Normal (show e)
                    Right triples -> do
                        treeStoreClear (variables var)
                        mapM_ (insertBreak (variables var))
                            (zip triples [0..length triples])
  where
    insertBreak treeStore (v,index)  = treeStoreInsert treeStore [] index v

selectString :: [ToolOutput] -> String
selectString (ToolOutput str:r)  = '\n' : str ++ selectString r
selectString (_:r)               = selectString r
selectString []                  = ""

getSelectedVariable ::  TreeView
    -> TreeStore VarDescription
    -> IO (Maybe (VarDescription,TreePath))
getSelectedVariable treeView treeStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        a:r ->  do
            val     <-  treeStoreGetValue treeStore a
            return (Just (val,a))
        _  ->  return Nothing

variablesParser :: CharParser () [VarDescription]
variablesParser = do
    whiteSpace
    r <- many variableParser
    eof
    return r

variableParser :: CharParser () VarDescription
variableParser = do
    whiteSpace
    varName <- many (noneOf ":")
    symbol "::"
    typeStr  <- many (noneOf "=")
    char '='
    value <- many (do
        noneOf "\n"
        <|> try (do
                r <- char '\n'
                lookAhead (char ' ')
                return r))

    return (VarDescription varName typeStr value)
    <?> "variableParser"

valueParser :: CharParser () String
valueParser = do
    whiteSpace
    many (noneOf "=")
    char '='
    value <- many anyChar
    return (value)
    <?> "valueParser"

typeParser :: CharParser () String
typeParser = do
    whiteSpace
    many (noneOf ":")
    symbol "::"
    typeStr  <- many anyChar
    return typeStr
    <?> "typeParser"


lexer = P.makeTokenParser emptyDef
symbol = P.symbol lexer
whiteSpace = P.whiteSpace lexer


variablesViewPopup :: IDERef
    -> TreeStore VarDescription
    -> TreeView
    -> Event
    -> IO (Bool)
variablesViewPopup ideR  store treeView (Button _ click timestamp _ _ _ button _ _)
    = do
    if button == RightButton
        then do
            theMenu         <-  menuNew
            menuAttachToWidget theMenu treeView
            item1           <-  menuItemNewWithLabel "Force"
            item1 `on` menuItemActivate $ do
                mbSel  <-  getSelectedVariable treeView store
                case mbSel of
                    Just (varDescr,path) -> reflectIDE (forceVariable varDescr path store) ideR
                    otherwise     -> return ()
            sep1 <- separatorMenuItemNew
            item2           <-  menuItemNewWithLabel "Print"
            item2 `on` menuItemActivate $ do
                mbSel  <-  getSelectedVariable treeView store
                case mbSel of
                    Just (varDescr,path) -> reflectIDE (printVariable varDescr path store) ideR
                    otherwise     -> return ()
            item3           <-  menuItemNewWithLabel "Update"
            item3 `on` menuItemActivate $ postGUIAsync (reflectIDE fillVariablesList ideR)
            mapM_ (menuShellAppend theMenu) [castToMenuItem item1,
                castToMenuItem item2, castToMenuItem sep1, castToMenuItem item3]
            menuPopup theMenu $ Just (button, timestamp)
            widgetShowAll theMenu
            return True
        else if button == LeftButton && click == DoubleClick
                then do mbSel         <-  getSelectedVariable treeView store
                        case mbSel of
                            Just (varDescr,path) -> reflectIDE (forceVariable varDescr path store) ideR
                            otherwise     -> return ()
                        return True
                else return False
variablesViewPopup _ _ _ _ = throwIDE "variablesViewPopup wrong event type"

forceVariable :: VarDescription -> TreePath -> TreeStore VarDescription -> IDEAction
forceVariable varDescr path treeStore = packageTry $ tryDebug $ do
    debugCommand' (":force " ++ (varName varDescr)) $ do
        to <- EL.consume
        liftIO $ postGUIAsync $ do
            case parse valueParser "" (selectString to) of
                Left e -> sysMessage Normal (show e)
                Right value -> do
                    var <- treeStoreGetValue treeStore path
                    treeStoreSetValue treeStore path var{varValue = value}
    debugCommand' (":type " ++ (varName varDescr)) $ do
        to <- EL.consume
        liftIO $ postGUIAsync $ do
            case parse typeParser "" (selectString to) of
                Left e -> sysMessage Normal (show e)
                Right typ -> do
                    var <- treeStoreGetValue treeStore path
                    treeStoreSetValue treeStore path var{varType = typ}

printVariable :: VarDescription -> TreePath -> TreeStore VarDescription -> IDEAction
printVariable varDescr path treeStore = packageTry $ tryDebug $ do
    debugCommand' (":print " ++ (varName varDescr)) $ do
        to <- EL.consume
        liftIO $ postGUIAsync $ do
            case parse valueParser "" (selectString to) of
                Left e -> sysMessage Normal (show e)
                Right value -> do
                    var <- treeStoreGetValue treeStore path
                    treeStoreSetValue treeStore path var{varValue = value}
    debugCommand' (":type " ++ (varName varDescr)) $ do
        to <- EL.consume
        liftIO $ postGUIAsync $ do
            case parse typeParser "" (selectString to) of
                Left e -> sysMessage Normal (show e)
                Right typ -> do
                    var <- treeStoreGetValue treeStore path
                    treeStoreSetValue treeStore path var{varType = typ}

