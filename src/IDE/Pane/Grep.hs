{-# OPTIONS_GHC -XDeriveDataTypeable -XMultiParamTypeClasses -XTypeSynonymInstances
    -XRecordWildCards #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Grep
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability  :  portable
--
-- | The pane of ide where grep results are displayed
--
-------------------------------------------------------------------------------

module IDE.Pane.Grep (
    IDEGrep(..)
,   showGrep
,   setGrepResults
,   GrepState(..)
) where

import Graphics.UI.Gtk hiding (get)
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding(Parser)
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.Maybe
import Control.Monad.Reader
import Data.Typeable
import IDE.Core.State
import IDE.Tool (ToolOutput(..))
import Control.Concurrent (forkIO)
import IDE.LogRef (logOutput)
import IDE.Pane.SourceBuffer
    (goToSourceDefinition)

data GrepRecord = GrepRecord {
    file        :: FilePath
,   line        :: Int
,   context     :: String
}

-- | A grep pane description
--

data IDEGrep        =   IDEGrep {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   grepStore       ::   ListStore GrepRecord
} deriving Typeable

data GrepState      =   GrepState
    deriving(Eq,Ord,Read,Show,Typeable)

instance IDEObject IDEGrep

instance Pane IDEGrep IDEM
    where
    primPaneName _  =   "Grep"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Grep"
    makeActive p    =   activatePane p []
    close           =   closePane

instance RecoverablePane IDEGrep GrepState IDEM where
    saveState p     =   do
        return (Just GrepState)
    recoverState pp GrepState =   do
        nb      <-  getNotebook pp
        initGrep pp nb

showGrep :: IDEAction
showGrep = do
    m <- getGrep
    liftIO $ bringPaneToFront m
    liftIO $ widgetGrabFocus (treeView m)

getGrep :: IDEM IDEGrep
getGrep = do
    mbGrep <- getPane
    case mbGrep of
        Just m ->   return m
        Nothing -> do
            pp          <-  getBestPathForId "*Grep"
            nb          <-  getNotebook pp
            initGrep pp nb
            mbGrep <- getPane
            case mbGrep of
                Nothing ->  throwIDE "Can't init grep"
                Just m  ->  return m


initGrep :: PanePath -> Notebook -> IDEAction
initGrep panePath nb = do
    prefs       <-  readIDE prefs
    currentInfo <-  readIDE currentInfo
    (buf,cids)  <-  reifyIDE $ \ideR  -> do

        listStore   <-  listStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView listStore

        renderer1    <- cellRendererTextNew
        renderer10   <- cellRendererPixbufNew
        col1         <- treeViewColumnNew
        treeViewColumnSetTitle col1 "File"
        treeViewColumnSetSizing col1 TreeViewColumnAutosize
        treeViewColumnSetResizable col1 True
        treeViewColumnSetReorderable col1 True
        treeViewAppendColumn treeView col1
        cellLayoutPackStart col1 renderer10 False
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetAttributes col1 renderer1 listStore
            $ \row -> [ cellText := file row]

        renderer2   <- cellRendererTextNew
        col2        <- treeViewColumnNew
        treeViewColumnSetTitle col2 "Line"
        treeViewColumnSetSizing col2 TreeViewColumnAutosize
        treeViewColumnSetResizable col2 True
        treeViewColumnSetReorderable col2 True
        treeViewAppendColumn treeView col2
        cellLayoutPackStart col2 renderer2 True
        cellLayoutSetAttributes col2 renderer2 listStore
            $ \row -> [ cellText := show $ line row]

        renderer3    <- cellRendererTextNew
        renderer30   <- cellRendererPixbufNew
        col3         <- treeViewColumnNew
        treeViewColumnSetTitle col3 "Context"
        treeViewColumnSetSizing col3 TreeViewColumnAutosize
        treeViewColumnSetResizable col3 True
        treeViewColumnSetReorderable col3 True
        treeViewAppendColumn treeView col3
        cellLayoutPackStart col3 renderer30 False
        cellLayoutPackStart col3 renderer3 True
        cellLayoutSetAttributes col3 renderer3 listStore
            $ \row -> [ cellText := context row]


        treeViewSetHeadersVisible treeView True
        sel <- treeViewGetSelection treeView
        treeSelectionSetMode sel SelectionSingle

        sw <- scrolledWindowNew Nothing Nothing
        containerAdd sw treeView
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic

        let grep = IDEGrep sw treeView listStore
        notebookInsertOrdered nb sw (paneName grep) Nothing

        cid1 <- treeView `afterFocusIn`
            (\_ -> do reflectIDE (makeActive grep) ideR ; return True)
        sel `onSelectionChanged` do
            sel <- getSelectionGrepRecord treeView listStore
            case sel of
                Just record -> reflectIDE (do
                    goToSourceDefinition (file record)
                        $ Just $ Location (line record) 0 (line record) 0) ideR
                Nothing -> return ()

        return (grep,[ConnectC cid1])
    addPaneAdmin buf cids panePath
    liftIO $ widgetShowAll (scrolledView buf)
    liftIO $ widgetGrabFocus (scrolledView buf)

grepLineParser :: CharParser () GrepRecord
grepLineParser = try (do
        file <- many (noneOf ":")
        char ':'
        line <- int
        char ':'
        context <- many anyChar
        return $ GrepRecord {..}
    <?> "grepLineParser")

lexer = P.makeTokenParser emptyDef
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
hexadecimal = P.hexadecimal lexer
symbol = P.symbol lexer
identifier = P.identifier lexer
colon = P.colon lexer
int = fmap fromInteger $ P.integer lexer

getSelectionGrepRecord ::  TreeView
    ->  ListStore GrepRecord
    -> IO (Maybe GrepRecord)
getSelectionGrepRecord treeView listStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        [a]:r ->  do
            size    <-  listStoreGetSize listStore
            if a < size
                then fmap Just $ listStoreGetValue listStore a
                else return Nothing
        _  ->  return Nothing

setGrepResults :: [ToolOutput] -> IDEAction
setGrepResults output = do
    grep <- getGrep
    ideRef <- ask
    liftIO $ do
        let (displayed, dropped) = splitAt 10000 output
        bringPaneToFront grep
        forkIO $ do
            let errors = filter (isNothing . process) output
                    ++ if null dropped
                        then []
                        else [ToolError $ "Dropped " ++ show (length dropped) ++ " search results"]
            unless (null errors) $ reflectIDE (logOutput errors) ideRef
            return ()
        postGUIAsync $ listStoreClear (grepStore grep)
        forM_ (catMaybes (map process displayed)) $ \record -> do
            postGUIAsync $ do
                listStoreAppend (grepStore grep) record
                return ()
    where
        process (ToolOutput line) =
            case parse grepLineParser "" line of
                Right record -> Just record
                _ -> Nothing
        process _ = Nothing


