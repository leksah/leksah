{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
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
import Data.Typeable
import Data.List (isPrefixOf)
import IDE.Core.State
import IDE.BufferMode
import IDE.Utils.Tool (runTool, ToolOutput(..), getProcessExitCode, interruptProcessGroupOf)
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
import IDE.Pane.Log (getLog, getDefaultLogLaunch)
import Control.DeepSeq
import qualified Data.Conduit as C
       (Sink)
import qualified Data.Conduit.List as CL
       (foldM, head, isolate, sinkNull)
import Data.Conduit (($$), (=$))
import Control.Monad (foldM, when)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils (__)
import System.Directory (getDirectoryContents)
import qualified Data.Text as T (unpack)
import System.Log.Logger (debugM)
import Control.Exception (SomeException, catch)

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
    primPaneName _  =   (__ "Grep")
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
        col1         <- treeViewColumnNew
        treeViewColumnSetTitle col1 (__ "File")
        treeViewColumnSetSizing col1 TreeViewColumnAutosize
        treeViewColumnSetResizable col1 True
        treeViewColumnSetReorderable col1 True
        treeViewAppendColumn treeView col1
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetAttributes col1 renderer1 grepStore
            $ \row -> [ cellText := file row]

        renderer2   <- cellRendererTextNew
        col2        <- treeViewColumnNew
        treeViewColumnSetTitle col2 (__ "Line")
        treeViewColumnSetSizing col2 TreeViewColumnAutosize
        treeViewColumnSetResizable col2 True
        treeViewColumnSetReorderable col2 True
        treeViewAppendColumn treeView col2
        cellLayoutPackStart col2 renderer2 True
        cellLayoutSetAttributes col2 renderer2 grepStore
            $ \row -> [ cellText := show $ line row]

        renderer3    <- cellRendererTextNew
        col3         <- treeViewColumnNew
        treeViewColumnSetTitle col3 (__ "Context")
        treeViewColumnSetSizing col3 TreeViewColumnAutosize
        treeViewColumnSetResizable col3 True
        treeViewColumnSetReorderable col3 True
        treeViewAppendColumn treeView col3
        cellLayoutPackStart col3 renderer3 True
        cellLayoutSetAttributes col3 renderer3 grepStore
            $ \row -> [ cellText := take 2048 $ context row]


        treeViewSetHeadersVisible treeView True
        sel <- treeViewGetSelection treeView
        treeSelectionSetMode sel SelectionSingle

        scrolledView <- scrolledWindowNew Nothing Nothing
        scrolledWindowSetShadowType scrolledView ShadowIn
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
                                (goToSourceDefinition pp $ Location f l 0 l 0)
                                    ?>>= (\(IDEBuffer {sourceView = sv}) -> when focus $ grabFocus sv)
                            _ -> return ()) ideR
                    Nothing -> return ()
                return True
        cid1 <- after treeView focusInEvent $ do
            liftIO $ reflectIDE (makeActive grep) ideR
            return True
        cid2 <- on treeView keyPressEvent $ do
            name <- eventKeyName
            liftIO $ case name of
                        "Return" -> gotoSource True
                        "Escape" -> do
                            reflectIDE (do
                                lastActiveBufferPane ?>>= \paneName -> do
                                    (PaneC pane) <- paneFromName paneName
                                    makeActive pane
                                    return ()
                                triggerEventIDE StartFindInitial) ideR
                            return True
                            -- gotoSource True
                        _ -> return False
        on sel treeSelectionSelectionChanged (gotoSource False >> return ())


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
            Just active -> active : (filter (/= active) $ wsAllPackages ws)
            Nothing     -> wsAllPackages ws
    lift $ grepDirectories regexString caseSensitive $
            map (\p -> (dropFileName $ ipdCabalFile p)) $ packages

grepDirectories :: String -> Bool -> [FilePath] -> IDEAction
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

            totalFound <- foldM (\a dir -> do
                subDirs <- filter (\f ->
                       (not ("." `isPrefixOf` f))
                    && (not (f `elem` ["_darcs", "dist", "vendor"]))) <$> getDirectoryContents dir
                nooneWaiting <- isEmptyMVar (waitingGrep grep)
                found <- if nooneWaiting
                    then do
                        (output, pid) <- runTool "grep" ((if caseSensitive then [] else ["-i"])
                            ++ ["-r", "-E", "-n", "-I", "--exclude=*~",
#if !defined(darwin_HOST_OS)
                                "--exclude-dir=.svn",
                                "--exclude-dir=_darcs",
                                "--exclude-dir=.git",
#endif
                                regexString] ++ subDirs) (Just dir)
                        reflectIDE (do
                            output $$ do
                                let max = 1000
                                CL.isolate max =$ do
                                    n <- setGrepResults dir
                                    when (n >= max) . liftIO $ do
                                        debugM "leksah" $ "interrupting grep process"
                                        interruptProcessGroupOf pid
                                          `catch` \(_::SomeException) -> return ()
                                        postGUISync $ do
                                            nDir <- treeModelIterNChildren store Nothing
                                            treeStoreChange store [nDir-1] (\r -> r{ context = (__ "(Stoped Searching)") })
                                            return ()
                                    CL.sinkNull
                                    return n) ideRef
                    else return 0
                return $ a + found) 0 dirs

            nooneWaiting <- isEmptyMVar (waitingGrep grep)
            when nooneWaiting $ postGUISync $ do
                nDir <- treeModelIterNChildren store Nothing
                treeStoreInsert store [] nDir $ GrepRecord (__ "Search Complete") totalFound "" Nothing

            takeMVar (activeGrep grep) >> return ()
    return ()

setGrepResults :: FilePath -> C.Sink ToolOutput IDEM Int
setGrepResults dir = do
    ideRef <- lift ask
    grep <- lift $ getGrep Nothing
    log <- lift $ getLog
    defaultLogLaunch <- lift $ getDefaultLogLaunch
    let store = grepStore grep
        view  = treeView grep
    nDir <- liftIO $ postGUISync $ do
        nDir <- treeModelIterNChildren store Nothing
        treeStoreInsert store [] nDir $ GrepRecord dir 0 "" Nothing
        when (nDir == 0) (widgetGrabFocus view >> return())
        return nDir
    CL.foldM (\count line -> do
        if isError line
            then do
                liftIO $ postGUISync $ reflectIDE (defaultLineLogger log defaultLogLaunch line >> return ()) ideRef
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
            case parse grepLineParser "" $ T.unpack line of
                Right record -> Just record{parDir = Just pp}
                _ -> Nothing
        process _ _ = Nothing

        isError (ToolExit ExitSuccess) = False
        isError (ToolExit (ExitFailure 1)) = False
        isError o = isNothing (process "" o)




