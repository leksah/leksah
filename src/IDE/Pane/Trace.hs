{-# LANGUAGE FlexibleInstances, TypeSynonymInstances,
   MultiParamTypeClasses, DeriveDataTypeable, OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Trace
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

module IDE.Pane.Trace (
    IDETrace
,   TraceState
,   showTrace
,   fillTraceList
) where

import Data.Typeable (Typeable(..))
import IDE.Core.State
import IDE.Package (tryDebug)
import IDE.Debug
    (debugForward, debugBack, debugCommand')
import IDE.Utils.Tool (ToolOutput(..))
import IDE.LogRef (srcSpanParser)
import Text.ParserCombinators.Parsec
    (anyChar,
     skipMany,
     (<|>),
     optional,
     eof,
     try,
     parse,
     (<?>),
     noneOf,
     many,
     CharParser)
import qualified Text.ParserCombinators.Parsec.Token as  P
    (integer, whiteSpace, colon, symbol, makeTokenParser)
import Text.ParserCombinators.Parsec.Language (emptyDef)
import System.Log.Logger (debugM)
import IDE.Workspaces (packageTry)
import qualified Data.Conduit.List as CL (consume)
import Control.Applicative ((<$>))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils (treeViewContextMenu, __)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as T (pack, unpack)
import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowSetShadowType,
        scrolledWindowNew, ScrolledWindow(..))
import GI.Gtk.Objects.TreeView
       (treeViewGetSelection, treeViewSetHeadersVisible,
        treeViewAppendColumn, treeViewSetModel, treeViewNew, TreeView(..))
import Data.GI.Gtk.ModelView.ForestStore
       (forestStoreGetValue, ForestStore(..),
        forestStoreInsert, forestStoreClear, forestStoreNew)
import GI.Gtk.Objects.Widget (afterWidgetFocusInEvent, toWidget)
import GI.Gtk.Objects.Notebook (Notebook(..))
import GI.Gtk.Objects.Window (Window(..))
import GI.Gtk.Objects.CellRendererToggle
       (setCellRendererToggleActive, cellRendererToggleNew)
import GI.Gtk.Objects.TreeViewColumn
       (treeViewColumnSetReorderable, treeViewColumnSetResizable,
        treeViewColumnSetSizing, treeViewColumnSetTitle, treeViewColumnNew)
import GI.Gtk.Enums
       (PolicyType(..), ShadowType(..), SelectionMode(..),
        TreeViewColumnSizing(..))
import GI.Gtk.Interfaces.CellLayout (cellLayoutPackStart)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetDataFunction)
import GI.Gtk.Objects.CellRendererText
       (setCellRendererTextText, cellRendererTextNew)
import GI.Gtk.Objects.TreeSelection
       (onTreeSelectionChanged, treeSelectionSetMode)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Objects.Menu (Menu(..))
import GI.Gtk.Objects.MenuItem
       (toMenuItem, onMenuItemActivate, menuItemNewWithLabel)
import GI.Gtk.Objects.SeparatorMenuItem (separatorMenuItemNew)
import GI.Gtk.Objects.MenuShell (menuShellAppend)
import Control.Monad.Reader (MonadReader(..))
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathNewFromIndices')

printf :: PrintfType r => Text -> r
printf = S.printf . T.unpack

-- | A debugger pane description
--
data IDETrace    =   IDETrace {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   tracepoints     ::   ForestStore TraceHist
} deriving Typeable

data TraceState  =   TraceState {
}   deriving(Eq,Ord,Read,Show,Typeable)

data TraceHist = TraceHist {
    thSelected      ::  Bool,
    thIndex         ::  Int,
    thFunction      ::  Text,
    thPosition      ::  SrcSpan
    }

instance Pane IDETrace IDEM
    where
    primPaneName _  =   __ "Trace"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . scrolledView
    paneId b        =   "*Trace"

instance RecoverablePane IDETrace TraceState IDEM where
    saveState p     =   return (Just TraceState)
    recoverState pp TraceState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder = builder'

getTrace :: IDEM IDETrace
getTrace = forceGetPane (Right "*Trace")

showTrace :: IDEAction
showTrace = do
    pane <- getTrace
    displayPane pane False

builder' :: PanePath ->
    Notebook ->
    Window ->
    IDEM (Maybe IDETrace,Connections)
builder' pp nb windows = do
    ideR <- ask
    tracepoints <-  forestStoreNew []
    treeView    <-  treeViewNew
    treeViewSetModel treeView (Just tracepoints)

    renderer0 <- cellRendererToggleNew
    col0         <- treeViewColumnNew
    treeViewColumnSetTitle col0 ""
    treeViewColumnSetSizing col0 TreeViewColumnSizingAutosize
    treeViewColumnSetResizable col0 False
    treeViewColumnSetReorderable col0 True
    treeViewAppendColumn treeView col0
    cellLayoutPackStart col0 renderer0 False
    cellLayoutSetDataFunction col0 renderer0 tracepoints
        $ setCellRendererToggleActive renderer0 . thSelected

    renderer1    <- cellRendererTextNew
    col1         <- treeViewColumnNew
    treeViewColumnSetTitle col1 (__ "Index")
    treeViewColumnSetSizing col1 TreeViewColumnSizingAutosize
    treeViewColumnSetResizable col1 True
    treeViewColumnSetReorderable col1 True
    treeViewAppendColumn treeView col1
    cellLayoutPackStart col1 renderer1 False
    cellLayoutSetDataFunction col1 renderer1 tracepoints
        $ setCellRendererTextText renderer1 . T.pack . show . thIndex

    renderer2    <- cellRendererTextNew
    col2         <- treeViewColumnNew
    treeViewColumnSetTitle col2 (__ "Function")
    treeViewColumnSetSizing col2 TreeViewColumnSizingAutosize
    treeViewColumnSetResizable col2 True
    treeViewColumnSetReorderable col2 True
    treeViewAppendColumn treeView col2
    cellLayoutPackStart col2 renderer2 False
    cellLayoutSetDataFunction col2 renderer2 tracepoints
        $ setCellRendererTextText renderer2 . thFunction

    renderer3    <- cellRendererTextNew
    col3         <- treeViewColumnNew
    treeViewColumnSetTitle col3 (__ "Position")
    treeViewColumnSetSizing col3 TreeViewColumnSizingAutosize
    treeViewColumnSetResizable col3 True
    treeViewColumnSetReorderable col3 True
    treeViewAppendColumn treeView col3
    cellLayoutPackStart col3 renderer3 False
    cellLayoutSetDataFunction col3 renderer3 tracepoints
        $ setCellRendererTextText renderer3 . T.pack . displaySrcSpan . thPosition

    treeViewSetHeadersVisible treeView True
    sel <- treeViewGetSelection treeView
    treeSelectionSetMode sel SelectionModeSingle

    scrolledView <- scrolledWindowNew noAdjustment noAdjustment
    scrolledWindowSetShadowType scrolledView ShadowTypeIn
    containerAdd scrolledView treeView
    scrolledWindowSetPolicy scrolledView PolicyTypeAutomatic PolicyTypeAutomatic

    let pane = IDETrace scrolledView treeView tracepoints

    cid1 <- onIDE afterWidgetFocusInEvent treeView (do
        liftIDE $ makeActive pane
        return True)
    cids2 <- treeViewContextMenu treeView $ traceContextMenu ideR tracepoints treeView
    onTreeSelectionChanged sel $ do
        sel <- getSelectedTracepoint treeView tracepoints
        case sel of
            Just ref -> return () -- TODO reflectIDE (selectRef (Just ref)) ideR
            Nothing -> return ()
    return (Just pane, cid1 : cids2)

fillTraceList :: IDEAction
fillTraceList = packageTry $ do
    currentHist' <- readIDE currentHist
    mbTraces     <- liftIDE getPane
    case mbTraces of
        Nothing -> return ()
        Just tracePane -> tryDebug $ debugCommand' ":history" $ do
            to <- CL.consume
            lift $ postAsyncIDE $ do
                let parseRes = parse tracesParser "" . T.unpack $ selectString to
                r <- case parseRes of
                        Left err     -> do
                            liftIO $ debugM "leksah" (printf (__ "trace parse error %s\ninput: %s") (show err)
                                                (T.unpack $ selectString to))
                            return []
                        Right traces -> return traces
                forestStoreClear (tracepoints tracePane)
                let r' = map (\h@(TraceHist _ i _ _) -> if i == currentHist'
                                                            then h{thSelected = True}
                                                            else h) r
                mapM_ (insertTrace (tracepoints tracePane))
                    (zip r' [0..length r'])
  where
    insertTrace forestStore (tr,index)  = do
        emptyPath <- treePathNewFromIndices' []
        forestStoreInsert forestStore emptyPath index tr

selectString :: [ToolOutput] -> Text
selectString (ToolOutput str:r)  = "\n" <> str <> selectString r
selectString (_:r)               = selectString r
selectString []                  = ""

getSelectedTracepoint ::  TreeView
    -> ForestStore TraceHist
    -> IO (Maybe TraceHist)
getSelectedTracepoint treeView forestStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows' treeSelection
    case paths of
        a:r ->  do
            val     <-  forestStoreGetValue forestStore a
            return (Just val)
        _  ->  return Nothing

selectStrings :: [ToolOutput] -> [Text]
selectStrings (ToolOutput str:r)  = str : selectStrings r
selectStrings (_:r)               = selectStrings r
selectStrings []                  = []

traceContextMenu :: IDERef
                  -> ForestStore TraceHist
                  -> TreeView
                  -> Menu
                  -> IO ()
traceContextMenu ideR store treeView theMenu = do
    item1           <-  menuItemNewWithLabel (__ "Back")
    onMenuItemActivate item1 $ reflectIDE debugBack ideR
    sep1 <- separatorMenuItemNew >>= liftIO . toMenuItem
    item2           <-  menuItemNewWithLabel (__ "Forward")
    onMenuItemActivate item2 $ reflectIDE debugForward ideR
    item3           <-  menuItemNewWithLabel (__ "Update")
    onMenuItemActivate item3 $ reflectIDE fillTraceList ideR
    mapM_ (menuShellAppend theMenu) [item1, sep1, item2, item3]

tracesParser :: CharParser () [TraceHist]
tracesParser = try (do
        whiteSpace
        symbol (T.unpack $ __ "Empty history.")
        skipMany anyChar
        eof
        return [])
    <|> do
        traces <- many (try traceParser)
        whiteSpace
        symbol (T.unpack $ __ "<end of history>")
        eof
        return traces
    <|> do
        whiteSpace
        symbol (T.unpack $ __ "Not stopped at a breakpoint")
        skipMany anyChar
        eof
        return []
    <?>
        T.unpack (__ "traces parser")

traceParser :: CharParser () TraceHist
traceParser = do
    whiteSpace
    index    <- int
    colon
    optional (symbol "\ESC[1m")
    function <- T.pack <$> many (noneOf "(\ESC")
    optional (symbol "\ESC[0m")
    symbol "("
    span     <- srcSpanParser
    symbol ")"
    return (TraceHist False index function span)
    <?> T.unpack (__ "trace parser")

lexer  = P.makeTokenParser emptyDef
colon  = P.colon lexer
symbol = P.symbol lexer
whiteSpace = P.whiteSpace lexer
int = fromInteger <$> P.integer lexer



