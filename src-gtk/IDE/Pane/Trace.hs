{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
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

import Prelude ()
import Prelude.Compat
import Data.Typeable (Typeable)
import IDE.Core.State
       (SrcSpan, IDEM, IDEAction, IDERef, displaySrcSpan,
        liftIDE, readIDE, currentHist, reflectIDE)
import IDE.Gtk.State
       (Pane(..), RecoverablePane(..), PanePath, Connections,
        getNotebook, onIDE, postAsyncIDE)
import IDE.Gtk.Package (tryDebug)
import IDE.Debug
    (debugForward, debugBack, debugCommand')
import IDE.Utils.Tool (ToolOutput(..))
import IDE.LogRef (srcSpanParser)
import System.Log.Logger (debugM)
import IDE.Gtk.Workspaces (packageTry)
import qualified Data.Conduit.List as CL (consume)
import Control.Applicative (optional, (<$>), (<|>), many)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils (treeViewContextMenu, __, printf)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
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
import GI.Gtk.Objects.Adjustment (Adjustment)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Objects.Menu (Menu(..))
import GI.Gtk.Objects.MenuItem
       (toMenuItem, onMenuItemActivate, menuItemNewWithLabel)
import GI.Gtk.Objects.SeparatorMenuItem (separatorMenuItemNew)
import GI.Gtk.Objects.MenuShell (menuShellAppend)
import Control.Monad.Reader (MonadReader(..))
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathNewFromIndices')
import Data.Attoparsec.Text
       (string, manyTill, char, (<?>), endOfInput, anyChar, skipMany, try,
        parseOnly, Parser)
import qualified Data.Attoparsec.Text as AP
       (decimal, string, skipSpace)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | A debugger pane description
--
data IDETrace    =   IDETrace {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   tracepoints     ::   ForestStore TraceHist
} deriving Typeable

data TraceState  =   TraceState {
}   deriving(Eq,Ord,Read,Show,Typeable,Generic)

instance ToJSON TraceState
instance FromJSON TraceState

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
    paneId _        =   "*Trace"

instance RecoverablePane IDETrace TraceState IDEM where
    saveState _     =   return (Just TraceState)
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
builder' _pp _nb _windows = do
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
    _ <- treeViewAppendColumn treeView col0
    cellLayoutPackStart col0 renderer0 False
    cellLayoutSetDataFunction col0 renderer0 tracepoints
        $ setCellRendererToggleActive renderer0 . thSelected

    renderer1    <- cellRendererTextNew
    col1         <- treeViewColumnNew
    treeViewColumnSetTitle col1 (__ "Index")
    treeViewColumnSetSizing col1 TreeViewColumnSizingAutosize
    treeViewColumnSetResizable col1 True
    treeViewColumnSetReorderable col1 True
    _ <- treeViewAppendColumn treeView col1
    cellLayoutPackStart col1 renderer1 False
    cellLayoutSetDataFunction col1 renderer1 tracepoints
        $ setCellRendererTextText renderer1 . T.pack . show . thIndex

    renderer2    <- cellRendererTextNew
    col2         <- treeViewColumnNew
    treeViewColumnSetTitle col2 (__ "Function")
    treeViewColumnSetSizing col2 TreeViewColumnSizingAutosize
    treeViewColumnSetResizable col2 True
    treeViewColumnSetReorderable col2 True
    _ <- treeViewAppendColumn treeView col2
    cellLayoutPackStart col2 renderer2 False
    cellLayoutSetDataFunction col2 renderer2 tracepoints
        $ setCellRendererTextText renderer2 . thFunction

    renderer3    <- cellRendererTextNew
    col3         <- treeViewColumnNew
    treeViewColumnSetTitle col3 (__ "Position")
    treeViewColumnSetSizing col3 TreeViewColumnSizingAutosize
    treeViewColumnSetResizable col3 True
    treeViewColumnSetReorderable col3 True
    _ <- treeViewAppendColumn treeView col3
    cellLayoutPackStart col3 renderer3 False
    cellLayoutSetDataFunction col3 renderer3 tracepoints
        $ setCellRendererTextText renderer3 . T.pack . displaySrcSpan . thPosition

    treeViewSetHeadersVisible treeView True
    sel <- treeViewGetSelection treeView
    treeSelectionSetMode sel SelectionModeSingle

    scrolledView <- scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
    scrolledWindowSetShadowType scrolledView ShadowTypeIn
    containerAdd scrolledView treeView
    scrolledWindowSetPolicy scrolledView PolicyTypeAutomatic PolicyTypeAutomatic

    let pane = IDETrace {..}

    cid1 <- onIDE afterWidgetFocusInEvent treeView (do
        liftIDE $ makeActive pane
        return True)
    cids2 <- treeViewContextMenu treeView $ traceContextMenu ideR tracepoints treeView
    _ <- onTreeSelectionChanged sel $
        getSelectedTracepoint treeView tracepoints >>= \case
            Just _ref -> return () -- TODO reflectIDE (selectRef (Just ref)) ideR
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
                let parseRes = parseOnly tracesParser $ selectString to
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
        a:_ ->  do
            val     <-  forestStoreGetValue forestStore a
            return (Just val)
        _  ->  return Nothing

--selectStrings :: [ToolOutput] -> [Text]
--selectStrings (ToolOutput str:r)  = str : selectStrings r
--selectStrings (_:r)               = selectStrings r
--selectStrings []                  = []

traceContextMenu :: IDERef
                  -> ForestStore TraceHist
                  -> TreeView
                  -> Menu
                  -> IO ()
traceContextMenu ideR _store _treeView theMenu = do
    item1           <-  menuItemNewWithLabel (__ "Back")
    _ <- onMenuItemActivate item1 $ reflectIDE debugBack ideR
    sep1 <- separatorMenuItemNew >>= liftIO . toMenuItem
    item2           <-  menuItemNewWithLabel (__ "Forward")
    _ <- onMenuItemActivate item2 $ reflectIDE debugForward ideR
    item3           <-  menuItemNewWithLabel (__ "Update")
    _ <- onMenuItemActivate item3 $ reflectIDE fillTraceList ideR
    mapM_ (menuShellAppend theMenu) [item1, sep1, item2, item3]

tracesParser :: Parser [TraceHist]
tracesParser = try (do
        whiteSpace
        _ <- symbol (__ "Empty history.")
        skipMany anyChar
        endOfInput
        return [])
    <|> do
        traces <- many (try traceParser)
        whiteSpace
        _ <- symbol (__ "<end of history>")
        endOfInput
        return traces
    <|> do
        whiteSpace
        _ <- symbol (__ "Not stopped at a breakpoint")
        skipMany anyChar
        endOfInput
        return []
    <?>
        T.unpack (__ "traces parser")

traceParser :: Parser TraceHist
traceParser = do
    whiteSpace
    index    <- int
    _ <- char ':'
    whiteSpace
    _ <- optional (symbol "\ESC[1m")
    function <- T.pack <$> manyTill anyChar (string "(\ESC")
    _ <- optional (symbol "\ESC[0m")
    _ <- symbol "("
    span' <- srcSpanParser
    _ <- symbol ")"
    return (TraceHist False index function span')
    <?> T.unpack (__ "trace parser")

whiteSpace :: Parser ()
whiteSpace = AP.skipSpace
symbol :: Text -> Parser Text
symbol = AP.string
int :: Parser Int
int = AP.decimal



