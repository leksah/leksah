{-# LANGUAGE FlexibleInstances, RecordWildCards, TypeSynonymInstances,
             MultiParamTypeClasses, DeriveDataTypeable #-}
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
,   fillTraceList
) where

import Graphics.UI.Gtk
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
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import Graphics.UI.Gtk.General.Enums (MouseButton(..))
import System.Log.Logger (debugM)
import IDE.Workspaces (packageTry)
import qualified Data.Enumerator.List as EL (consume)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils (treeViewContextMenu, __)
import Text.Printf (printf)

-- | A debugger pane description
--
data IDETrace    =   IDETrace {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   tracepoints     ::   TreeStore TraceHist
} deriving Typeable

data TraceState  =   TraceState {
}   deriving(Eq,Ord,Read,Show,Typeable)

data TraceHist = TraceHist {
    thSelected      ::  Bool,
    thIndex         ::  Int,
    thFunction      ::  String,
    thPosition      ::  SrcSpan
    }

instance Pane IDETrace IDEM
    where
    primPaneName _  =   (__ "Trace")
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Trace"

instance RecoverablePane IDETrace TraceState IDEM where
    saveState p     =   do
        return (Just TraceState)
    recoverState pp TraceState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder = builder'

builder' :: PanePath ->
    Notebook ->
    Window ->
    IDEM (Maybe IDETrace,Connections)
builder' pp nb windows = reifyIDE $ \ ideR -> do
    tracepoints <-  treeStoreNew []
    treeView    <-  treeViewNew
    treeViewSetModel treeView tracepoints

    renderer0 <- cellRendererToggleNew
    col0         <- treeViewColumnNew
    treeViewColumnSetTitle col0 ""
    treeViewColumnSetSizing col0 TreeViewColumnAutosize
    treeViewColumnSetResizable col0 False
    treeViewColumnSetReorderable col0 True
    treeViewAppendColumn treeView col0
    cellLayoutPackStart col0 renderer0 False
    cellLayoutSetAttributes col0 renderer0 tracepoints
        $ \row -> [ cellToggleActive := thSelected row]

    renderer1    <- cellRendererTextNew
    col1         <- treeViewColumnNew
    treeViewColumnSetTitle col1 (__ "Index")
    treeViewColumnSetSizing col1 TreeViewColumnAutosize
    treeViewColumnSetResizable col1 True
    treeViewColumnSetReorderable col1 True
    treeViewAppendColumn treeView col1
    cellLayoutPackStart col1 renderer1 False
    cellLayoutSetAttributes col1 renderer1 tracepoints
        $ \row -> [ cellText := show (thIndex row)]

    renderer2    <- cellRendererTextNew
    col2         <- treeViewColumnNew
    treeViewColumnSetTitle col2 (__ "Function")
    treeViewColumnSetSizing col2 TreeViewColumnAutosize
    treeViewColumnSetResizable col2 True
    treeViewColumnSetReorderable col2 True
    treeViewAppendColumn treeView col2
    cellLayoutPackStart col2 renderer2 False
    cellLayoutSetAttributes col2 renderer2 tracepoints
        $ \row -> [ cellText := thFunction row]

    renderer3    <- cellRendererTextNew
    col3         <- treeViewColumnNew
    treeViewColumnSetTitle col3 (__ "Position")
    treeViewColumnSetSizing col3 TreeViewColumnAutosize
    treeViewColumnSetResizable col3 True
    treeViewColumnSetReorderable col3 True
    treeViewAppendColumn treeView col3
    cellLayoutPackStart col3 renderer3 False
    cellLayoutSetAttributes col3 renderer3 tracepoints
        $ \row -> [ cellText := displaySrcSpan (thPosition row)]

    treeViewSetHeadersVisible treeView True
    sel <- treeViewGetSelection treeView
    treeSelectionSetMode sel SelectionSingle

    scrolledView <- scrolledWindowNew Nothing Nothing
    containerAdd scrolledView treeView
    scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic

    let pane = IDETrace scrolledView treeView tracepoints

    cid1 <- after treeView focusInEvent $ do
        liftIO $ reflectIDE (makeActive pane) ideR
        return True
    (cid2, cid3) <- treeViewContextMenu treeView $ traceContextMenu ideR tracepoints treeView
    on sel treeSelectionSelectionChanged $ do
        sel <- getSelectedTracepoint treeView tracepoints
        case sel of
            Just ref -> return () -- TODO reflectIDE (selectRef (Just ref)) ideR
            Nothing -> return ()
    return (Just pane, map ConnectC [cid1, cid2, cid3])

fillTraceList :: IDEAction
fillTraceList = packageTry $ do
    currentHist' <- lift $ readIDE currentHist
    mbTraces     <- lift getPane
    case mbTraces of
        Nothing -> return ()
        Just tracePane -> tryDebug $ debugCommand' ":history" $ do
            to <- EL.consume
            liftIO $ postGUIAsync $ do
                let parseRes = parse tracesParser "" (selectString to)
                r <- case parseRes of
                        Left err     -> do
                            debugM "leksah" ((printf (__ "trace parse error %s\ninput: %s") (show err) (selectString to)))
                            return []
                        Right traces -> return traces
                treeStoreClear (tracepoints tracePane)
                let r' = map (\h@(TraceHist _ i _ _) -> if i == currentHist'
                                                            then h{thSelected = True}
                                                            else h) r
                mapM_ (insertTrace (tracepoints tracePane))
                    (zip r' [0..length r'])
  where
    insertTrace treeStore (tr,index)  = treeStoreInsert treeStore [] index tr

selectString :: [ToolOutput] -> String
selectString (ToolOutput str:r)  = '\n' : str ++ selectString r
selectString (_:r)               = selectString r
selectString []                  = ""

getSelectedTracepoint ::  TreeView
    -> TreeStore TraceHist
    -> IO (Maybe TraceHist)
getSelectedTracepoint treeView treeStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        a:r ->  do
            val     <-  treeStoreGetValue treeStore a
            return (Just val)
        _  ->  return Nothing

selectStrings :: [ToolOutput] -> [String]
selectStrings (ToolOutput str:r)  = str : selectStrings r
selectStrings (_:r)               = selectStrings r
selectStrings []                  = []

traceContextMenu :: IDERef
                  -> TreeStore TraceHist
                  -> TreeView
                  -> Menu
                  -> IO ()
traceContextMenu ideR store treeView theMenu = do
    item1           <-  menuItemNewWithLabel (__ "Back")
    item1 `on` menuItemActivate $ reflectIDE debugBack ideR
    sep1 <- separatorMenuItemNew
    item2           <-  menuItemNewWithLabel (__ "Forward")
    item2 `on` menuItemActivate $ reflectIDE debugForward ideR
    item3           <-  menuItemNewWithLabel (__ "Update")
    item3 `on` menuItemActivate $ reflectIDE fillTraceList ideR
    mapM_ (menuShellAppend theMenu) [castToMenuItem item1, castToMenuItem sep1,
        castToMenuItem item2, castToMenuItem item3]

tracesParser :: CharParser () [TraceHist]
tracesParser = try (do
        whiteSpace
        symbol (__ "Empty history.")
        skipMany anyChar
        eof
        return [])
    <|> do
        traces <- many (try traceParser)
        whiteSpace
        symbol (__ "<end of history>")
        eof
        return traces
    <|> do
        whiteSpace
        symbol (__ "Not stopped at a breakpoint")
        skipMany anyChar
        eof
        return []
    <?>
        (__ "traces parser")

traceParser :: CharParser () TraceHist
traceParser = do
    whiteSpace
    index    <- int
    colon
    optional (symbol "\ESC[1m")
    function <- many (noneOf "(\ESC")
    optional (symbol "\ESC[0m")
    symbol "("
    span     <- srcSpanParser
    symbol ")"
    return (TraceHist False index function span)
    <?> (__ "trace parser")

lexer  = P.makeTokenParser emptyDef
colon  = P.colon lexer
symbol = P.symbol lexer
whiteSpace = P.whiteSpace lexer
int = fmap fromInteger $ P.integer lexer



