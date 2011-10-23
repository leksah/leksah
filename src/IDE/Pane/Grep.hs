{-# LANGUAGE CPP, FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses,
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
import qualified Graphics.UI.Gtk.Gdk.Events as Gdk
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding(Parser)
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.Maybe
import Control.Monad.Reader
import Data.Typeable
import IDE.Core.State
import IDE.BufferMode
import IDE.Utils.Tool (runTool, ToolOutput(..))
import Control.Concurrent
       (forkOS, newEmptyMVar, isEmptyMVar, takeMVar, putMVar, MVar,
        forkIO)
import IDE.LogRef (logOutput, defaultLineLogger)
import IDE.Pane.SourceBuffer
    (goToSourceDefinition, maybeActiveBuf, IDEBuffer(..))
import IDE.TextEditor (grabFocus)
import Control.Applicative ((<$>))
import System.FilePath ((</>), dropFileName)
import System.Exit (ExitCode(..))
import IDE.Pane.Log (getLog)
import Control.DeepSeq
#ifdef MIN_VERSION_process_leksah
import IDE.System.Process (getProcessExitCode, interruptProcessGroup)
#else
import System.Process (getProcessExitCode, interruptProcessGroupOf)
#endif
import qualified Data.Enumerator as E
       (Step(..), run_, Iteratee(..), run)
import qualified Data.Enumerator.List as EL
       (foldM, head, dropWhile, isolate)
import Data.Enumerator (($$), (>>==))
import qualified Data.List as L ()

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
        let
            gotoSource :: Bool -> IO Bool
            gotoSource focus = do
                sel <- getSelectionGrepRecord treeView grepStore
                case sel of
                    Just record -> reflectIDE (do
                        case record of
                            GrepRecord {file=f, line=l, parDir=Just pp} ->
                                (goToSourceDefinition (pp </> f) $ Just $ Location l 0 l 0)
                                    ?>>= (\b -> when focus $ grabFocus (sourceView b))
                            _ -> return ()) ideR
                    Nothing -> return ()
                return True
        cid1 <- treeView `afterFocusIn`
            (\_ -> do reflectIDE (makeActive grep) ideR ; return True)
        cid2 <- treeView `onKeyPress`
            (\e ->
                case e of
                    k@(Gdk.Key _ _ _ _ _ _ _ _ _ _)
                        | Gdk.eventKeyName k == "Return"  -> do
                            gotoSource True
                        | Gdk.eventKeyName k == "Escape"  -> do
                            reflectIDE (do
                                lastActiveBufferPane ?>>= \paneName -> do
                                    (PaneC pane) <- paneFromName paneName
                                    makeActive pane
                                    return ()
                                triggerEventIDE StartFindInitial) ideR
                            return True
                            -- gotoSource True
                        | otherwise -> do
                            return False
                    _ -> return False
             )
        sel `onSelectionChanged` (void $ gotoSource False)


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

            postGUISync $ treeStoreClear store

            totalFound <- foldM (\a (dir, subDirs) -> do
                nooneWaiting <- isEmptyMVar (waitingGrep grep)
                found <- if nooneWaiting
                    then do
                        (output, pid) <- runTool "grep" ((if caseSensitive then [] else ["-i"])
                            ++ ["-r", "-E", "-n", "-I", "--exclude=*~", "--exclude-dir=.svn", regexString] ++ subDirs) (Just dir)
                        reflectIDE (do
                            E.run_ $ output $$ do
                                let max = 1000
                                step <- EL.isolate (toInteger max) $$ setGrepResults dir
                                case step of
                                    E.Continue _ -> do
#ifdef MIN_VERSION_process_leksah
                                        liftIO $ interruptProcessGroup pid
#else
                                        liftIO $ interruptProcessGroupOf pid
#endif
                                        liftIO $ postGUISync $ do
                                            nDir <- treeModelIterNChildren store Nothing
                                            liftIO $ treeStoreChange store [nDir-1] (\r -> r{ context = "(Stoped Searching)" })
                                            return ()
                                        EL.dropWhile (const True)
                                        return max
                                    E.Yield n _ -> return n
                                    _           -> return 0) ideRef
                    else return 0
                return $ a + found) 0 dirs

            nooneWaiting <- isEmptyMVar (waitingGrep grep)
            when nooneWaiting $ postGUISync $ do
                nDir <- treeModelIterNChildren store Nothing
                treeStoreInsert store [] nDir $ GrepRecord "Search Complete" totalFound "" Nothing

            takeMVar (activeGrep grep) >> return ()
    return ()

setGrepResults :: FilePath -> E.Iteratee ToolOutput IDEM Int
setGrepResults dir = do
    ideRef <- lift ask
    grep <- lift $ getGrep Nothing
    log <- lift $ getLog
    let store = grepStore grep
        view  = treeView grep
    nDir <- liftIO $ postGUISync $ do
        nDir <- treeModelIterNChildren store Nothing
        treeStoreInsert store [] nDir $ GrepRecord dir 0 "" Nothing
        when (nDir == 0) (widgetGrabFocus view >> return())
        return nDir
    EL.foldM (\count line -> do
        if isError line
            then do
                liftIO $ postGUISync $ reflectIDE (defaultLineLogger log line >> return ()) ideRef
                return count
            else do
                case process dir line of
                    Nothing     -> return count
                    Just record -> liftIO $ do
                        nooneWaiting <- isEmptyMVar (waitingGrep grep)
                        when nooneWaiting $ postGUISync $ do
                            parent <- treeModelGetIter store [nDir]
                            n <- treeModelIterNChildren store parent
                            treeStoreInsert store [nDir] n record
                            treeStoreChange store [nDir] (\r -> r{ line = n+1 })
                            when (nDir == 0 && n == 0) $
                                treeViewExpandAll view
                        return (count+1)) 0
    where
        process pp (ToolOutput line) =
            case parse grepLineParser "" line of
                Right record -> Just record{parDir = Just pp}
                _ -> Nothing
        process _ _ = Nothing

        isError (ToolExit ExitSuccess) = False
        isError (ToolExit (ExitFailure 1)) = False
        isError o = isNothing (process "" o)




