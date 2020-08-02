{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
,   showVariables
,   fillVariablesList
,   fillVariablesListQuiet
) where

import Prelude ()
import Prelude.Compat
import Data.Typeable (Typeable)
import IDE.Core.State
       (IDEM, IDEAction, IDERef, liftIDE, sysMessage,
        MessageLevel(..), reflectIDE)
import IDE.Gtk.State
       (Pane(..), RecoverablePane(..), PanePath, Connections,
        getNotebook, onIDE, Connection(..), postAsyncIDE)
import IDE.Gtk.Package (tryDebug, tryDebugQuiet)
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
     CharParser,
     parse)
import qualified Text.ParserCombinators.Parsec.Token as  P
    (whiteSpace, symbol, makeTokenParser)
import Text.ParserCombinators.Parsec.Language (emptyDef)
import IDE.Workspaces (packageTryQuiet)
import IDE.Gtk.Workspaces (packageTry)
import qualified Data.Conduit.List as CL (consume)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils (treeViewContextMenu, __)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Control.Applicative ((<$>))
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowSetShadowType,
        scrolledWindowNew, ScrolledWindow(..))
import GI.Gtk.Objects.TreeView
       (onTreeViewRowActivated, treeViewGetSelection,
        treeViewSetHeadersVisible, treeViewAppendColumn, treeViewSetModel,
        treeViewNew, TreeView(..))
import Data.GI.Gtk.ModelView.ForestStore
       (forestStoreGetValue, ForestStore(..), forestStoreSetValue,
        forestStoreInsert, forestStoreClear, forestStoreNew)
import GI.Gtk.Objects.Widget (afterWidgetFocusInEvent, toWidget)
import GI.Gtk.Objects.Notebook (Notebook(..))
import GI.Gtk.Objects.Window (Window(..))
import GI.Gtk.Objects.CellRendererText
       (setCellRendererTextText, cellRendererTextNew)
import GI.Gtk.Objects.TreeViewColumn
       (TreeViewColumn(..), treeViewColumnSetReorderable,
        treeViewColumnSetResizable, treeViewColumnSetSizing,
        treeViewColumnSetTitle, treeViewColumnNew)
import GI.Gtk.Enums
       (PolicyType(..), ShadowType(..), SelectionMode(..),
        TreeViewColumnSizing(..))
import GI.Gtk.Interfaces.CellLayout (cellLayoutPackStart)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetDataFunction)
import GI.Gtk.Objects.TreeSelection
       (treeSelectionSetMode)
import GI.Gtk.Objects.Adjustment (Adjustment)
import GI.Gtk.Objects.Container (containerAdd)
import Control.Monad.Reader (MonadReader(..))
import GI.Gtk.Structs.TreePath
       (TreePath(..))
import GI.Gtk.Objects.Menu (Menu(..))
import GI.Gtk.Objects.MenuItem
       (toMenuItem, onMenuItemActivate, menuItemNewWithLabel)
import GI.Gtk.Objects.SeparatorMenuItem (separatorMenuItemNew)
import GI.Gtk.Objects.MenuShell (menuShellAppend)
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathNewFromIndices')
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Text.Parsec.Token (GenTokenParser)
import Data.Functor.Identity (Identity(..))
import Text.Parsec (ParsecT)

-- | Represents the Variables pane
data IDEVariables    =   IDEVariables {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   variables       ::   ForestStore VarDescription
} deriving Typeable


-- | The data for a single entry in the pane
data VarDescription = VarDescription {
    varName         ::  Text
,   varType         ::  Text
,   varValue        ::  Text}


-- | The additional state used when recovering the pane
data VariablesState  =   VariablesState
    deriving(Eq,Ord,Read,Show,Typeable,Generic)

instance ToJSON VariablesState
instance FromJSON VariablesState

instance Pane IDEVariables IDEM
    where
    primPaneName _  =   __ "Variables"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . scrolledView
    paneId _        =   "*Variables"


instance RecoverablePane IDEVariables VariablesState IDEM where
    saveState _     =   return (Just VariablesState)
    recoverState pp VariablesState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder = builder'


-- | Get the Variables pane
getVariables :: IDEM IDEVariables
getVariables = forceGetPane (Right "*Variables")


-- | Display the Variables pane
showVariables :: IDEAction
showVariables = do
    pane <- getVariables
    displayPane pane False


-- | Builds the Variables pane, together with a list
--   of connections of events
builder' :: PanePath ->
    Notebook ->
    Window ->
    IDEM (Maybe IDEVariables, Connections)
builder' _pp _nb _windows = do
    ideR <- ask
    variables   <-  forestStoreNew []
    treeView    <-  treeViewNew
    treeViewSetModel treeView (Just variables)

    renderer1    <- cellRendererTextNew
    col1         <- treeViewColumnNew
    treeViewColumnSetTitle col1 (__ "Name")
    treeViewColumnSetSizing col1 TreeViewColumnSizingAutosize
    treeViewColumnSetResizable col1 True
    treeViewColumnSetReorderable col1 True
    _ <- treeViewAppendColumn treeView col1
    cellLayoutPackStart col1 renderer1 False
    cellLayoutSetDataFunction col1 renderer1 variables
        $ setCellRendererTextText renderer1 . varName

    renderer2    <- cellRendererTextNew
    col2         <- treeViewColumnNew
    treeViewColumnSetTitle col2 (__ "Type")
    treeViewColumnSetSizing col2 TreeViewColumnSizingAutosize
    treeViewColumnSetResizable col2 True
    treeViewColumnSetReorderable col2 True
    _ <- treeViewAppendColumn treeView col2
    cellLayoutPackStart col2 renderer2 False
    cellLayoutSetDataFunction col2 renderer2 variables
        $ setCellRendererTextText renderer2 . varType

    renderer3    <- cellRendererTextNew
    col3         <- treeViewColumnNew
    treeViewColumnSetTitle col3 (__ "Value")
    treeViewColumnSetSizing col3 TreeViewColumnSizingAutosize
    treeViewColumnSetResizable col3 True
    treeViewColumnSetReorderable col3 True
    _ <- treeViewAppendColumn treeView col3
    cellLayoutPackStart col3 renderer3 False
    cellLayoutSetDataFunction col3 renderer3 variables
        $ setCellRendererTextText renderer3 . varValue

    treeViewSetHeadersVisible treeView True
    sel <- treeViewGetSelection treeView
    treeSelectionSetMode sel SelectionModeSingle

    scrolledView <- scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
    scrolledWindowSetShadowType scrolledView ShadowTypeIn
    containerAdd scrolledView treeView
    scrolledWindowSetPolicy scrolledView PolicyTypeAutomatic PolicyTypeAutomatic

    let pane = IDEVariables {..}
    cid1 <- onIDE afterWidgetFocusInEvent treeView (do
        liftIDE $ makeActive pane
        return True)
    cids2 <- treeViewContextMenu treeView $ variablesContextMenu ideR variables treeView
    cid4 <- ConnectC treeView <$> onTreeViewRowActivated treeView (variablesSelect ideR variables)
    return (Just pane, [cid1, cid4] ++ cids2)


-- | Quietly list the variable bindings of the current debug session
fillVariablesListQuiet :: IDEAction
fillVariablesListQuiet = packageTryQuiet $ do
    mbVariables <- liftIDE getPane
    case mbVariables of
        Nothing -> return ()
        Just var -> tryDebugQuiet $ debugCommand' ":show bindings" $ do
            to <- CL.consume
            lift $ postAsyncIDE $
                case parse variablesParser "" . T.unpack $ selectString to of
                    Left e -> liftIO $ sysMessage Normal (T.pack $ show e)
                    Right triples -> do
                        forestStoreClear (variables var)
                        mapM_ (insertAtRoot (variables var))
                            (zip triples [0..length triples])

insertAtRoot :: MonadIO m => ForestStore a -> (a, Int) -> m ()
insertAtRoot forestStore (v,index)  = do
    emptyPath <- treePathNewFromIndices' []
    forestStoreInsert forestStore emptyPath index v


-- | List the variable bindings of the current debug session
fillVariablesList :: IDEAction
fillVariablesList = packageTry $ do
    mbVariables <- liftIDE getPane
    case mbVariables of
        Nothing -> return ()
        Just var -> tryDebug $ debugCommand' ":show bindings" $ do
            to <- CL.consume
            lift $ postAsyncIDE $
                case parse variablesParser "" . T.unpack $ selectString to of
                    Left e -> liftIO $ sysMessage Normal (T.pack $ show e)
                    Right triples -> do
                        forestStoreClear (variables var)
                        mapM_ (insertAtRoot (variables var))
                            (zip triples [0..length triples])


-- | Concatenates the tool output lines with newlines
selectString :: [ToolOutput] -> Text
selectString (ToolOutput str:r)  = "\n" <> str <> selectString r
selectString (_:r)               = selectString r
selectString []                  = ""


-- | Tries to get the selected variable in the Pane,
--  together with the 'TreePath'
getSelectedVariable ::  TreeView
    -> ForestStore VarDescription
    -> IO (Maybe (VarDescription,TreePath))
getSelectedVariable treeView forestStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows' treeSelection
    case paths of
        a:_ ->  do
            val     <-  forestStoreGetValue forestStore a
            return (Just (val,a))
        _  ->  return Nothing


-- | Parses multiple 'VarDescription's
variablesParser :: CharParser () [VarDescription]
variablesParser = do
    whiteSpace
    r <- many variableParser
    eof
    return r


-- | Parses a 'VarDescription' from a line of ghci output
variableParser :: CharParser () VarDescription
variableParser = do
    whiteSpace
    varName <- T.pack <$> many (noneOf ":")
    _ <- symbol "::"
    typeStr  <- T.pack <$> many (noneOf "=")
    _ <- char '='
    value <- T.pack <$> many (
        noneOf "\n"
        <|> try (do
                r <- char '\n'
                _ <- lookAhead (char ' ')
                return r))

    return (VarDescription varName typeStr value)
    <?> "variableParser"


-- | Parses only the value of a variable binding from a line of
-- ghci output
valueParser :: CharParser () Text
valueParser = do
    whiteSpace
    _ <- many (noneOf "=")
    _ <- char '='
    T.pack <$> many anyChar
    <?> "valueParser"

-- | Parses only the type of a variable binding from a line of
-- ghci output
typeParser :: CharParser () Text
typeParser = do
    whiteSpace
    _ <- many (noneOf ":")
    _ <- symbol "::"
    T.pack <$> many anyChar
    <?> "typeParser"

lexer :: GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef
symbol :: String -> ParsecT String u Identity String
symbol = P.symbol lexer
whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer

-- | Constructs the context menu of the Variables pane
variablesContextMenu :: IDERef
                  -> ForestStore VarDescription
                  -> TreeView
                  -> Menu
                  -> IO ()
variablesContextMenu ideR store treeView theMenu = do
    item1           <-  menuItemNewWithLabel (__ "Force")
    _ <- onMenuItemActivate item1 $ do
        mbSel  <-  getSelectedVariable treeView store
        case mbSel of
            Just (varDescr,path) -> reflectIDE (forceVariable varDescr path store) ideR
            _ -> return ()
    sep1 <- separatorMenuItemNew >>= liftIO . toMenuItem
    item2           <-  menuItemNewWithLabel (__ "Print")
    _ <- onMenuItemActivate item2 $ do
        mbSel  <-  getSelectedVariable treeView store
        case mbSel of
            Just (varDescr,path) -> reflectIDE (printVariable varDescr path store) ideR
            _ -> return ()
    item3           <-  menuItemNewWithLabel (__ "Update")
    _ <- onMenuItemActivate item3 $ reflectIDE (postAsyncIDE fillVariablesList) ideR
    mapM_ (menuShellAppend theMenu) [item1, item2, sep1, item3]


-- | Called when a variable is selected in the Variables pane.
-- Forces the variable and updates the entry in the pane,
variablesSelect :: IDERef
                -> ForestStore VarDescription
                -> TreePath
                -> TreeViewColumn
                -> IO ()
variablesSelect ideR store path _ = do
    varDescr <- forestStoreGetValue store path
    reflectIDE (forceVariable varDescr path store) ideR


-- | Force the value of the given 'VarDescription', retrieve its type
-- and update the appropriate entry in the Variables pane
forceVariable :: VarDescription -> TreePath -> ForestStore VarDescription -> IDEAction
forceVariable varDescr path forestStore = packageTry $ tryDebug $ do
    debugCommand' (":force " <> varName varDescr) $ do
        to <- CL.consume
        lift $ postAsyncIDE $
            case parse valueParser "" . T.unpack $ selectString to of
                Left e -> liftIO $ sysMessage Normal (T.pack $ show e)
                Right value -> do
                    var <- forestStoreGetValue forestStore path
                    forestStoreSetValue forestStore path var{varValue = value}
    debugCommand' (":type " <> varName varDescr) $ do
        to <- CL.consume
        lift $ postAsyncIDE $
            case parse typeParser "" . T.unpack $ selectString to of
                Left e -> liftIO $ sysMessage Normal (T.pack $ show e)
                Right typ -> do
                    var <- forestStoreGetValue forestStore path
                    forestStoreSetValue forestStore path var{varType = typ}


-- | Print the value of the given 'VarDescription', retrieve its type
-- and update the appropriate entry in the Variables pane
printVariable :: VarDescription -> TreePath -> ForestStore VarDescription -> IDEAction
printVariable varDescr path forestStore = packageTry $ tryDebug $ do
    debugCommand' (":print " <> varName varDescr) $ do
        to <- CL.consume
        lift $ postAsyncIDE $
            case parse valueParser "" . T.unpack $ selectString to of
                Left e -> liftIO $ sysMessage Normal (T.pack $ show e)
                Right value -> do
                    var <- forestStoreGetValue forestStore path
                    forestStoreSetValue forestStore path var{varValue = value}
    debugCommand' (":type " <> varName varDescr) $ do
        to <- CL.consume
        lift $ postAsyncIDE $
            case parse typeParser "" . T.unpack $ selectString to of
                Left e -> liftIO $ sysMessage Normal (T.pack $ show e)
                Right typ -> do
                    var <- forestStoreGetValue forestStore path
                    forestStoreSetValue forestStore path var{varType = typ}

