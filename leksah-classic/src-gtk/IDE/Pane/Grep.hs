{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
-- | The pane of the ide where grep results are displayed
--
-------------------------------------------------------------------------------

module IDE.Pane.Grep (
    IDEGrep(..)
,   grepWorkspace
,   GrepState(..)
,   getGrep
) where

import Prelude ()
import Prelude.Compat

import Control.Applicative (Applicative)
import Control.Concurrent
       (newEmptyMVar, isEmptyMVar, takeMVar, putMVar, MVar, forkIO)
import Control.Exception (SomeException, catch)
import Control.Lens ((^.))
import Control.Monad (void, foldM, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ask)

import Data.Aeson (FromJSON, ToJSON)
import Data.Conduit (ConduitT, (.|), runConduit)
import qualified Data.Conduit.List as CL
       (foldM, isolate, sinkNull)
import qualified Data.Function as F (on)
import Data.Functor.Identity (Identity)
import Data.List (isPrefixOf)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T (pack, take, unpack)
import Data.Typeable (Typeable)
import Data.Void (Void)

import GHC.Generics (Generic)

import System.Directory (getDirectoryContents)
import System.Exit (ExitCode(..))
import System.Log.Logger (debugM)

import Text.Parsec (ParsecT)
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (CharParser, try, many, noneOf, char, anyChar, (<?>), parse)
import qualified Text.ParserCombinators.Parsec.Token as P

import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetDataFunction)
import Data.GI.Gtk.ModelView.ForestStore
       (forestStoreChange, forestStoreGetValue, ForestStore(..),
        forestStoreNew, forestStoreClear, forestStoreInsert)
import Data.GI.Gtk.ModelView.TreeModel
       (treeModelGetIter, treeModelIterNChildren)
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathNewFromIndices')
import GI.Gdk.Functions (keyvalName)
import GI.Gdk.Structs.EventKey (getEventKeyKeyval)
import GI.Gtk.Enums
       (PolicyType(..), ShadowType(..), SelectionMode(..),
        TreeViewColumnSizing(..))
import GI.Gtk.Interfaces.CellLayout (cellLayoutPackStart)
import GI.Gtk.Objects.Adjustment (Adjustment)
import GI.Gtk.Objects.CellRendererText
       (setCellRendererTextText, cellRendererTextNew)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowSetShadowType,
        scrolledWindowNew, ScrolledWindow(..))
import GI.Gtk.Objects.TreeSelection
       (onTreeSelectionChanged, treeSelectionSetMode)
import GI.Gtk.Objects.TreeView
       (treeViewExpandAll, treeViewGetSelection,
        treeViewSetHeadersVisible, treeViewAppendColumn, treeViewSetModel,
        treeViewNew, TreeView(..), onTreeViewRowActivated)
import GI.Gtk.Objects.TreeViewColumn
       (treeViewColumnSetReorderable, treeViewColumnSetResizable,
        treeViewColumnSetSizing, treeViewColumnSetTitle, treeViewColumnNew)
import GI.Gtk.Objects.Widget
       (widgetGrabFocus, onWidgetKeyPressEvent, afterWidgetFocusInEvent,
        toWidget)

import IDE.BufferMode (lastActiveBufferPane)
import IDE.Core.State
       (IDEM, WorkspaceAction, IDEAction, Location(..),
        (?>>=), liftIDE, triggerEventIDE_,
        IDEEvent(..), reflectIDE, readIDE, activePack,
        ipdCabalFile, wsAllPackages, ipdPackageDir)
import IDE.Gtk.State
       (Pane, RecoverablePane, PanePath, IDEPane(..), primPaneName,
        getAddedIndex, getTopWidget, paneId, saveState, recoverState, builder,
        getNotebook, buildPane, onIDE, makeActive, paneFromName,
        forceGetPane, bringPaneToFront, postSyncIDE)
import IDE.LogRef (defaultLineLogger)
import IDE.Pane.Log (getLog, getDefaultLogLaunch)
import IDE.Pane.SourceBuffer
    (goToSourceDefinition, IDEBuffer(..))
import IDE.TextEditor (grabFocus)
import IDE.Utils.GUIUtils (__, treeViewToggleRow)
import IDE.Utils.Tool (runTool, ToolOutput(..), interruptProcessGroupOf)

-- | Represents a single search result
data GrepRecord = GrepRecord {
            file        :: FilePath
        ,   line        :: Int
        ,   context     :: Text
        ,   parDir      :: Maybe FilePath
        }

---- | Determines whether a 'GrepRecord' is a directory
--isDir :: GrepRecord -> Bool
--isDir GrepRecord{parDir = Nothing}  = True
--isDir _                             = False

-- | The representation of the Grep result pane
data IDEGrep        =   IDEGrep {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   grepStore       ::   ForestStore GrepRecord
,   waitingGrep     ::   MVar Bool
,   activeGrep      ::   MVar Bool
} deriving Typeable


-- | The additional state used when recovering the pane
data GrepState      =   GrepState
    deriving(Eq,Ord,Read,Show,Typeable,Generic)

instance ToJSON GrepState
instance FromJSON GrepState

instance Pane IDEGrep IDEM
    where
    primPaneName _  =   __ "Grep"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . scrolledView
    paneId _        =   "*Grep"


instance RecoverablePane IDEGrep GrepState IDEM where
    saveState _     =   return (Just GrepState)
    recoverState pp GrepState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder _pp _nb _windows = do
        grepStore   <-  forestStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView (Just grepStore)

        renderer1    <- cellRendererTextNew
        col1         <- treeViewColumnNew
        treeViewColumnSetTitle col1 (__ "File")
        treeViewColumnSetSizing col1 TreeViewColumnSizingAutosize
        treeViewColumnSetResizable col1 True
        treeViewColumnSetReorderable col1 True
        _ <- treeViewAppendColumn treeView col1
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetDataFunction col1 renderer1 grepStore
            $ \row -> setCellRendererTextText renderer1 . T.pack $ file row

        renderer2   <- cellRendererTextNew
        col2        <- treeViewColumnNew
        treeViewColumnSetTitle col2 (__ "Line")
        treeViewColumnSetSizing col2 TreeViewColumnSizingAutosize
        treeViewColumnSetResizable col2 True
        treeViewColumnSetReorderable col2 True
        _ <- treeViewAppendColumn treeView col2
        cellLayoutPackStart col2 renderer2 True
        cellLayoutSetDataFunction col2 renderer2 grepStore
            $ setCellRendererTextText renderer2 . T.pack . show . line

        renderer3    <- cellRendererTextNew
        col3         <- treeViewColumnNew
        treeViewColumnSetTitle col3 (__ "Context")
        treeViewColumnSetSizing col3 TreeViewColumnSizingAutosize
        treeViewColumnSetResizable col3 True
        treeViewColumnSetReorderable col3 True
        _ <- treeViewAppendColumn treeView col3
        cellLayoutPackStart col3 renderer3 True
        cellLayoutSetDataFunction col3 renderer3 grepStore
            $ setCellRendererTextText renderer3 . T.take 2048 . context


        treeViewSetHeadersVisible treeView True
        sel <- treeViewGetSelection treeView
        treeSelectionSetMode sel SelectionModeSingle

        scrolledView <- scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
        scrolledWindowSetShadowType scrolledView ShadowTypeIn
        containerAdd scrolledView treeView
        scrolledWindowSetPolicy scrolledView PolicyTypeAutomatic PolicyTypeAutomatic

        waitingGrep <- liftIO newEmptyMVar
        activeGrep <- liftIO newEmptyMVar
        let grep = IDEGrep {..}
        let
            gotoSource :: Bool -> IDEM Bool
            gotoSource focus = do
                getSelectionGrepRecord treeView grepStore >>= \case
                    Just record ->
                        case record of
                            GrepRecord {file=f, line=l, parDir=Just pp} ->
                                goToSourceDefinition pp (Location f l 0 l 0) ?>>=
                                   (\ IDEBuffer{sourceView = sv} -> when focus $ grabFocus sv)
                            _ -> return ()
                    Nothing -> return ()
                return True
        cid1 <- onIDE afterWidgetFocusInEvent treeView $ do
            liftIDE $ makeActive grep
            return True
        cid2 <- onIDE onWidgetKeyPressEvent treeView $ do
            e <- lift ask
            name <- getEventKeyKeyval e >>= keyvalName
            liftIDE $
                case name of
                    Just "Return" -> gotoSource True
                    Just "Escape" -> do
                        lastActiveBufferPane ?>>= \paneName' -> do
                            (PaneC pane) <- paneFromName paneName'
                            makeActive pane
                            return ()
                        triggerEventIDE_ StartFindInitial
                        return True
                        -- gotoSource True
                    _ -> return False
        ideR <- ask
        _ <- onTreeViewRowActivated treeView $ \path _col -> do
            void $ treeViewToggleRow treeView path
            void (reflectIDE (gotoSource False) ideR)

        _ <- onTreeSelectionChanged sel (void (reflectIDE (gotoSource False) ideR))

        return (Just grep,[cid1, cid2])

-- | Get the Grep panel
getGrep :: Maybe PanePath -> IDEM IDEGrep
getGrep Nothing    = forceGetPane (Right "*Grep")
getGrep (Just pp)  = forceGetPane (Left pp)


-- | Parses a result line of grep output into a 'GrepRecord'
grepLineParser :: CharParser () GrepRecord
grepLineParser = try (do
        file <- many (noneOf ":")
        _ <- char ':'
        line <- int
        _ <- char ':'
        context <- T.pack <$> many anyChar
        let parDir = Nothing
        return GrepRecord {..}
    <?> "grepLineParser")

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef
--lexeme :: ParsecT String u Identity a -> ParsecT String u Identity a
--lexeme = P.lexeme lexer
--whiteSpace :: ParsecT String u Identity ()
--whiteSpace = P.whiteSpace lexer
--hexadecimal :: ParsecT String u Identity Integer
--hexadecimal = P.hexadecimal lexer
--symbol :: String -> ParsecT String u Identity String
--symbol = P.symbol lexer
--identifier :: ParsecT String u Identity String
--identifier = P.identifier lexer
--colon :: ParsecT String u Identity String
--colon = P.colon lexer
int :: ParsecT String u Identity Int
int = fromInteger <$> P.integer lexer


-- | Tries to get the currently selected record in the pane
getSelectionGrepRecord :: (Applicative m, MonadIO m)
                       => TreeView
                       -> ForestStore GrepRecord
                       -> m (Maybe GrepRecord)
getSelectionGrepRecord treeView grepStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows' treeSelection
    case paths of
        p:_ ->  Just <$> forestStoreGetValue grepStore p
        _   ->  return Nothing


--TODO srp use default loglaunch probably
-- | Greps the current workspace
grepWorkspace :: Text -- ^ The regex string
              -> Bool -- ^ Case sensitive
              -> WorkspaceAction
grepWorkspace "" _ = return ()
grepWorkspace regexString caseSensitive = do
    ws <- ask
    maybeActive <- lift $ readIDE activePack
    let packages = case maybeActive of
            Just active -> active : filter (((/=) `F.on` ipdCabalFile) active) (ws ^. wsAllPackages)
            Nothing     -> ws ^. wsAllPackages
    lift $ grepDirectories regexString caseSensitive $
            map ipdPackageDir packages


-- | Greps a list of directories
grepDirectories :: Text -- ^ The regex string
                -> Bool -- ^ Case sensitive
                -> [FilePath]
                -> IDEAction
grepDirectories regexString caseSensitive dirs = do
    grep <- getGrep Nothing
    let store = grepStore grep
    ideRef <- ask
    bringPaneToFront grep
    _ <- liftIO . forkIO . (`reflectIDE` ideRef) $ do
        void . liftIO $ do
            putMVar (waitingGrep grep) True
            putMVar (activeGrep grep) True
            takeMVar (waitingGrep grep)

        postSyncIDE $ forestStoreClear store

        totalFound <- foldM (\a dir -> do
            subDirs <- liftIO $ filter (\f ->
                   not ("." `isPrefixOf` f)
                && f `notElem` ["_darcs", "dist", "dist-newstyle", "dist-ghcjs", "vendor"]) <$> getDirectoryContents dir
            nooneWaiting <- liftIO $ isEmptyMVar (waitingGrep grep)
            found <- if nooneWaiting
                then do
                    (output, pid) <- liftIO $ runTool "grep" ((if caseSensitive then [] else ["-i"])
                        ++ ["-R", "-E", "-n", "-I",
                            "--exclude=*~",
                            "--exclude-dir=.svn",
                            "--exclude-dir=_darcs",
                            "--exclude-dir=.git",
                            regexString] ++ map T.pack subDirs) (Just dir) Nothing
                    runConduit $ output .| do
                        let max' = 1000
                        CL.isolate max' .| do
                            n <- setGrepResults dir
                            when (n >= max') $ do
                                liftIO $ debugM "leksah" "interrupting grep process"
                                liftIO $ interruptProcessGroupOf pid
                                  `catch` \(_::SomeException) -> return ()
                                lift $ postSyncIDE $ do
                                    nDir <- treeModelIterNChildren store Nothing
                                    p <- treePathNewFromIndices' [nDir-1]
                                    _ <- forestStoreChange store p (\r -> r{ context = __ "(Stoped Searching)" })
                                    return ()
                            CL.sinkNull
                            return n
                else return 0
            return $ a + found) 0 dirs

        nooneWaiting <- liftIO $ isEmptyMVar (waitingGrep grep)
        when nooneWaiting $ postSyncIDE $ do
            nDir <- treeModelIterNChildren store Nothing
            p <- treePathNewFromIndices' []
            forestStoreInsert store p (fromIntegral nDir) $ GrepRecord (T.unpack $ __ "Search Complete") totalFound "" Nothing

        void $ liftIO $ takeMVar (activeGrep grep)
    return ()


-- | A Sink for processing lines of grep output and counting them
setGrepResults :: FilePath -> ConduitT ToolOutput Void IDEM Int
setGrepResults dir = do
    grep <- lift $ getGrep Nothing
    log' <- lift getLog
    defaultLogLaunch <- lift getDefaultLogLaunch
    let store = grepStore grep
        view  = treeView grep
    nDir <- lift $ postSyncIDE $ do
        nDir <- treeModelIterNChildren store Nothing
        p <- treePathNewFromIndices' []
        forestStoreInsert store p (fromIntegral nDir) $ GrepRecord dir 0 "" Nothing
        when (nDir == 0) (void $ widgetGrabFocus view)
        return nDir
    CL.foldM (\count line ->
        if isError line
            then do
                postSyncIDE $ void $ defaultLineLogger log' defaultLogLaunch line
                return count
            else
                case process dir line of
                    Nothing     -> return count
                    Just record -> do
                        nooneWaiting <- liftIO $ isEmptyMVar (waitingGrep grep)
                        when nooneWaiting . postSyncIDE $ do
                            p <- treePathNewFromIndices' [nDir]
                            Just parent <- treeModelGetIter store p
                            n <- treeModelIterNChildren store (Just parent)
                            forestStoreInsert store p (fromIntegral n) record
                            _ <- forestStoreChange store p (\r -> r{ line = fromIntegral n+1 })
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




