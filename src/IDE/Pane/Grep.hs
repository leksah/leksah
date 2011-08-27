{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses,
             TypeSynonymInstances, RecordWildCards #-}
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
,   grepWorkspace
,   GrepState(..)
,   getGrep
) where

import Graphics.UI.Gtk hiding (get)
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding(Parser)
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.Maybe
import Control.Monad.Reader
import Data.Typeable
import IDE.Core.State
import IDE.Utils.Tool (runTool, ToolOutput(..))
import Control.Concurrent
       (newEmptyMVar, isEmptyMVar, takeMVar, putMVar, MVar, forkIO)
import IDE.LogRef (logOutput)
import IDE.Pane.SourceBuffer
    (goToSourceDefinition)
import Control.Applicative ((<$>))
import System.FilePath ((</>), dropFileName)
import System.Exit (ExitCode(..))
import IDE.Pane.Log

data GrepRecord = GrepRecord {
            file        :: FilePath
        ,   line        :: Int
        ,   context     :: String
        ,   parDir      :: Maybe FilePath
        }

isDir GrepRecord{parDir = Nothing}  = True
isDir otherwies                     = False

-- | A grep pane description
--

data IDEGrep        =   IDEGrep {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   grepStore       ::   TreeStore GrepRecord
,   waitingGrep     ::   MVar Bool
,   activeGrep      ::   MVar Bool
} deriving Typeable

data GrepState      =   GrepState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEGrep IDEM
    where
    primPaneName _  =   "Grep"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*Grep"

instance RecoverablePane IDEGrep GrepState IDEM where
    saveState p     =   do
        return (Just GrepState)
    recoverState pp GrepState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder pp nb windows = reifyIDE $ \ ideR -> do
        grepStore   <-  treeStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView grepStore

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
        cellLayoutSetAttributes col1 renderer1 grepStore
            $ \row -> [ cellText := file row]

        renderer2   <- cellRendererTextNew
        col2        <- treeViewColumnNew
        treeViewColumnSetTitle col2 "Line"
        treeViewColumnSetSizing col2 TreeViewColumnAutosize
        treeViewColumnSetResizable col2 True
        treeViewColumnSetReorderable col2 True
        treeViewAppendColumn treeView col2
        cellLayoutPackStart col2 renderer2 True
        cellLayoutSetAttributes col2 renderer2 grepStore
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
        cellLayoutSetAttributes col3 renderer3 grepStore
            $ \row -> [ cellText := context row]


        treeViewSetHeadersVisible treeView True
        sel <- treeViewGetSelection treeView
        treeSelectionSetMode sel SelectionSingle

        scrolledView <- scrolledWindowNew Nothing Nothing
        containerAdd scrolledView treeView
        scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic

        waitingGrep <- newEmptyMVar
        activeGrep <- newEmptyMVar
        let grep = IDEGrep {..}

        cid1 <- treeView `afterFocusIn`
            (\_ -> do reflectIDE (makeActive grep) ideR ; return True)
        sel `onSelectionChanged` do
            sel <- getSelectionGrepRecord treeView grepStore
            case sel of
                Just record -> reflectIDE (do
                    case record of
                        GrepRecord {file=f, line=l, parDir=Just pp} ->
                            goToSourceDefinition (pp </> f) $ Just $ Location l 0 l 0
                        _ -> return ()) ideR
                Nothing -> return ()

        return (Just grep,[ConnectC cid1])

getGrep :: Maybe PanePath -> IDEM IDEGrep
getGrep Nothing    = forceGetPane (Right "*Grep")
getGrep (Just pp)  = forceGetPane (Left pp)

grepLineParser :: CharParser () GrepRecord
grepLineParser = try (do
        file <- many (noneOf ":")
        char ':'
        line <- int
        char ':'
        context <- many anyChar
        let parDir = Nothing
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
    ->  TreeStore GrepRecord
    -> IO (Maybe GrepRecord)
getSelectionGrepRecord treeView grepStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        p:_ ->  Just <$> treeStoreGetValue grepStore p
        _   ->  return Nothing

--TODO srp use default loglaunch probably
grepWorkspace :: String -> Bool -> WorkspaceAction
grepWorkspace "" caseSensitive = return ()
grepWorkspace regexString caseSensitive = do
    ws <- ask
    maybeActive <- lift $ readIDE activePack
    let packages = case maybeActive of
            Just active -> active : (filter (/= active) $ wsPackages ws)
            Nothing     -> wsPackages ws
    lift $ grepDirectories regexString caseSensitive $
            map (\p -> (dropFileName (ipdCabalFile p), ipdSrcDirs p)) $ packages

grepDirectories :: String -> Bool -> [(FilePath, [FilePath])] -> IDEAction
grepDirectories regexString caseSensitive dirs = do
    grep <- getGrep Nothing
    let store = grepStore grep
    ideRef <- ask
    liftIO $ do
        bringPaneToFront grep
        forkIO $ do
            putMVar (waitingGrep grep) True
            putMVar (activeGrep grep) True
            takeMVar (waitingGrep grep)

            postGUIAsync $ treeStoreClear store

            totalFound <- foldM (\a (dir, subDirs) -> do
                nooneWaiting <- isEmptyMVar (waitingGrep grep)
                found <- if nooneWaiting
                    then do
                        (output, pid) <- runTool "grep" ((if caseSensitive then [] else ["-i"])
                            ++ ["-r", "-E", "-n", "--exclude=*~", regexString] ++ subDirs) (Just dir)
                        reflectIDE (setGrepResults dir output) ideRef
                    else return 0
                return $ a + found) 0 dirs

            nooneWaiting <- isEmptyMVar (waitingGrep grep)
            when nooneWaiting $ do
                nDir <- postGUISync $ treeModelIterNChildren store Nothing
                postGUIAsync $ treeStoreInsert store [] nDir $
                    GrepRecord "Search Complete" totalFound "" Nothing

            takeMVar (activeGrep grep) >> return ()
    return ()

setGrepResults :: FilePath -> [ToolOutput] -> IDEM Int
setGrepResults dir output = do
    grep <- getGrep Nothing
    let store = grepStore grep
        view  = treeView grep
    ideRef <- ask
    defaultLogLaunch <- getDefaultLogLaunch
    liftIO $ do
        let (displayed, dropped) = splitAt 5000 output
        forkIO $ do
            let errors = filter isError output
                    ++ if null dropped
                        then []
                        else [ToolError $ "Dropped " ++ show (length dropped) ++ " search results"]
            unless (null errors) $ postGUISync $ reflectIDE (logOutput defaultLogLaunch errors) ideRef
            return ()
        case catMaybes (map (process dir) displayed) of
            []      -> return 0
            results -> do
                nDir <- postGUISync $ treeModelIterNChildren store Nothing
                postGUIAsync $ treeStoreInsert store [] nDir $ GrepRecord dir 0 "" Nothing
                forM_ (zip results [0..]) $ \(record, n) -> do
                    nooneWaiting <- isEmptyMVar (waitingGrep grep)
                    when nooneWaiting $ postGUIAsync $ do
                        treeStoreInsert store [nDir] n record
                        treeStoreChange store [nDir] (\r -> r{ line = n+1 }) >> return ()
                        when (nDir == 0 && n == 0) $
                            treeViewExpandAll view
                return $ length results
    where
        process pp (ToolOutput line) =
            case parse grepLineParser "" line of
                Right record -> Just record{parDir = Just pp}
                _ -> Nothing
        process _ _ = Nothing

        isError (ToolExit ExitSuccess) = False
        isError (ToolExit (ExitFailure 1)) = False
        isError o = isNothing (process "" o)




