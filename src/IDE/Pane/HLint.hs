{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses,
             TypeSynonymInstances, RecordWildCards #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.HLint
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability  :  portable
--
-- | The pane of ide where HLint results are displayed
--
-----------------------------------------------------------------------------

module IDE.Pane.HLint (
    IDEHLint(..)
,   refreshHLint
,   HLintState(..)
,   getHLint
) where


import Graphics.UI.Gtk hiding (get)
import qualified Graphics.UI.Gtk.Gdk.Events as Gdk
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding(Parser)
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.Maybe
import Data.Typeable
import IDE.Core.State hiding (SrcSpan(..))
import IDE.BufferMode
import IDE.LogRef (logOutput, defaultLineLogger)
import IDE.Pane.SourceBuffer
    (goToSourceDefinition, maybeActiveBuf, IDEBuffer(..), replaceHLintSource)
import IDE.TextEditor (grabFocus)
import Control.Applicative ((<$>))
import System.FilePath ((</>), dropFileName)
import System.Exit (ExitCode(..))
import IDE.Pane.Log (getLog)
import Control.Monad (void, forM_, foldM, when)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Language.Haskell.HLint (Suggestion(..), suggestionLocation)
import Language.Haskell.Exts (SrcLoc(..))
import Language.Haskell.Exts.SrcLoc (SrcSpan(..))

import qualified Language.Haskell.HLint2 as H
import IDE.Utils.GUIUtils (__, treeViewContextMenu)
import Data.List (isPrefixOf, findIndex)
import Debug.Trace (trace)
import Control.Exception (SomeException, catch)
import Distribution.ModuleName (ModuleName)
import IDE.Metainfo.Provider (getWorkspaceInfo)
import qualified Data.Map as Map (keys, lookup)
import Distribution.Package (PackageIdentifier(..))

data HLintRecord = HLintRecord {
            condPackage :: Maybe IDEPackage
        ,   context     :: String
        ,   condIdea    :: Maybe H.Idea
        ,   parDir      :: Maybe FilePath
        } deriving (Eq)

isDir HLintRecord{parDir = Nothing}  = True
isDir otherwies                     = False

-- | A HLint pane description
--

data IDEHLint       =   IDEHLint {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   hlintStore      ::   TreeStore HLintRecord
} deriving Typeable

data HLintState      =   HLintState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEHLint IDEM
    where
    primPaneName _  =   "HLint"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledView
    paneId b        =   "*HLint"

instance RecoverablePane IDEHLint HLintState IDEM where
    saveState p     =   return (Just HLintState)
    recoverState pp HLintState =   do
        nb      <-  getNotebook pp
        buildPane pp nb builder
    builder pp nb windows = reifyIDE $ \ ideR -> do
        hlintStore   <-  treeStoreNew []
        treeView    <-  treeViewNew
        treeViewSetModel treeView hlintStore

        renderer1    <- cellRendererTextNew
        renderer10   <- cellRendererPixbufNew
        col1         <- treeViewColumnNew
        treeViewColumnSetTitle col1 (__ "Context")
        treeViewColumnSetSizing col1 TreeViewColumnAutosize
        treeViewColumnSetResizable col1 True
        treeViewColumnSetReorderable col1 True
        treeViewAppendColumn treeView col1
        cellLayoutPackStart col1 renderer10 False
        cellLayoutPackStart col1 renderer1 True
        cellLayoutSetAttributes col1 renderer1 hlintStore
            $ \row -> [ cellText := context row ]

        treeViewSetHeadersVisible treeView False
        sel <- treeViewGetSelection treeView
        treeSelectionSetMode sel SelectionSingle

        scrolledView <- scrolledWindowNew Nothing Nothing
        containerAdd scrolledView treeView
        scrolledWindowSetPolicy scrolledView PolicyAutomatic PolicyAutomatic

        let hlint = IDEHLint {..}
        cid1 <- after treeView focusInEvent $ do
            liftIO $ reflectIDE (makeActive hlint) ideR
            return True
        cid2 <- on treeView rowExpanded $ \ iter path -> do
            record <- treeStoreGetValue hlintStore path
            case record of
                HLintRecord { condPackage = Just p, parDir = Nothing } ->
                    reflectIDE (refreshDir hlintStore iter p) ideR
                _ -> reflectIDE (ideMessage Normal (__ "Unexpected Expansion in HLint Pane")) ideR
        cid3 <- on treeView rowActivated $ \ path col -> do
            record <- treeStoreGetValue hlintStore path
            mbIter <- treeModelGetIter hlintStore path
            case (mbIter, record) of
                (Just iter, HLintRecord { condPackage = Just p, parDir = Nothing }) ->
                    reflectIDE (refreshDir hlintStore iter p) ideR
                _ -> return ()
        cid4 <- on treeView keyPressEvent $ do
            name <- eventKeyName
            liftIO $ case name of
                        "Return" -> reflectIDE (gotoSource True treeView hlintStore) ideR
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
        treeViewContextMenu treeView
            $ hlintContextMenu ideR hlintStore treeView
        on sel treeSelectionSelectionChanged (reflectIDE (void $ gotoSource False treeView hlintStore) ideR)

        return (Just hlint,map ConnectC [cid1])

getHLint :: Maybe PanePath -> IDEM IDEHLint
getHLint Nothing    = forceGetPane (Right "*HLint")
getHLint (Just pp)  = forceGetPane (Left pp)

data FindResult = WhereExpected TreeIter | Found TreeIter | NotFound

find :: Eq a => a -> TreeStore a -> Maybe TreeIter -> IO FindResult
find _ _ Nothing = return NotFound
find a store (Just iter) = do
    row <- treeModelGetRow store iter
    if row == a
        then return $ WhereExpected iter
        else treeModelIterNext store iter >>= find'
  where
    find' :: Maybe TreeIter -> IO FindResult
    find' Nothing = return NotFound
    find' (Just iter) = do
        row <- treeModelGetRow store iter
        if row == a
            then return $ Found iter
            else treeModelIterNext store iter >>= find'

removeUntil :: Eq a => a -> TreeStore a -> TreePath -> IO ()
removeUntil a store path = do
    row <- treeStoreGetValue store path
    when (row /= a) $ do
        found <- treeStoreRemove store path
        when found $ removeUntil a store path

removeRemaining :: TreeStore a -> TreePath -> IO ()
removeRemaining store path = do
    found <- treeStoreRemove store path
    when found $ removeRemaining store path


getSelectionHLintRecord ::  TreeView
    ->  TreeStore HLintRecord
    -> IO (Maybe HLintRecord)
getSelectionHLintRecord treeView hlintStore = do
    treeSelection   <-  treeViewGetSelection treeView
    paths           <-  treeSelectionGetSelectedRows treeSelection
    case paths of
        p:_ ->  Just <$> treeStoreGetValue hlintStore p
        _   ->  return Nothing

refreshHLint :: WorkspaceAction
refreshHLint = do
    ws <- ask
    maybeActive <- lift $ readIDE activePack
    let packages = case maybeActive of
            Just active -> active : filter (/= active) (wsAllPackages ws)
            Nothing     -> wsAllPackages ws
    lift $ hlintDirectories2 packages


--gotoSource :: Bool -> IDEM Bool
gotoSource focus treeView hlintStore = do
    sel <- liftIO $ getSelectionHLintRecord treeView hlintStore
    case sel of
        Just record ->
            case record of
                HLintRecord {condIdea = Just idea} ->
                    goToSourceDefinition (srcSpanFilename (H.ideaSpan idea))
                                        (Just $ Location (srcSpanStartLine (H.ideaSpan idea))
                                                         (srcSpanStartColumn (H.ideaSpan idea))
                                                         (srcSpanEndLine (H.ideaSpan idea))
                                                         (srcSpanEndColumn (H.ideaSpan idea)))
                        ?>>= (\(IDEBuffer {sourceView = sv}) -> when focus $ grabFocus sv)
                _ -> return ()
        Nothing -> return ()
    return True


hlintDirectories2 :: [IDEPackage] -> IDEAction
hlintDirectories2 packages = do
    hlint <- getHLint Nothing
    let store = hlintStore hlint
    liftIO $ do
        treeStoreClear store
        forM_ packages $ \ p -> do
            nDir <- treeModelIterNChildren store Nothing
            treeStoreInsert store [] nDir $ HLintRecord (Just p) (packageIdentifierToString (ipdPackageId p)) Nothing Nothing
            treeStoreInsert store [nDir] 0 $ HLintRecord (Just p) (packageIdentifierToString (ipdPackageId p)) Nothing Nothing

refreshDir :: TreeStore HLintRecord -> TreeIter -> IDEPackage -> IDEM ()
refreshDir store iter package = do
    mbHlintDir <- liftIO $ leksahSubDir "hlint"
    let datadirOpt = case mbHlintDir of
                        Just d  -> "--datadir":[d]
                        Nothing -> []
    (flags, classify, hint) <- liftIO H.autoSettings
    let modules = Map.keys (ipdModules package)
    pathes <- getSourcePathes (ipdPackageId package) modules
    resL <- liftIO $ mapM (\dir -> H.parseModuleEx flags dir Nothing) pathes
    let resOk = mapMaybe (\ pr -> case pr of
                                    Left e -> trace ("can't parse: " ++ H.parseErrorContents e ++
                                        " location " ++ show H.parseErrorLocation) Nothing
                                    Right r -> Just r) resL
    let ideas = H.applyHints classify hint resOk
    liftIO $ setHLint2Results store iter (packageIdentifierToString (ipdPackageId package)) ideas
    return ()


getSourcePathes :: PackageIdentifier -> [ModuleName] -> IDEM [FilePath]
getSourcePathes packId names = do
    mbWorkspaceInfo     <-  getWorkspaceInfo
    case mbWorkspaceInfo of
        Nothing -> return []
        Just (sc, _) -> return (mapMaybe (sourcePathFromScope sc) names)
    where
    sourcePathFromScope :: GenScope -> ModuleName -> Maybe FilePath
    sourcePathFromScope (GenScopeC (PackScope l _)) mn =
        case packId `Map.lookup` l of
            Just pack ->
                case filter (\md -> modu (mdModuleId md) == mn)
                                    (pdModules pack) of
                    (mod : tl) ->  mdMbSourcePath mod
                    []         -> Nothing
            Nothing -> Nothing


hlint2Record dir idea = HLintRecord {
    condPackage = Nothing,
    context     = show idea,
    condIdea    = Just idea,
    parDir      = Just dir}

setHLint2Results :: TreeStore HLintRecord -> TreeIter -> FilePath -> [H.Idea] -> IO Int
setHLint2Results store parent dir ideas = do
    parentPath <- treeModelGetPath store parent
    forM_ (zip [0..] records) $ \(n, record) -> do
        mbChild <- treeModelIterNthChild store (Just parent) n
        findResult <- find record store mbChild
        case (mbChild, findResult) of
            (_, WhereExpected _) -> return ()
            (Just iter, Found _) -> do
                path <- treeModelGetPath store iter
                removeUntil record store path
            _ -> treeStoreInsert store parentPath n record
    removeRemaining store (parentPath++[nRecords])
    return nRecords
  where
    records = map (hlint2Record dir) ideas
    nRecords = length records

hlintContextMenu :: IDERef
                  -> TreeStore HLintRecord
                  -> TreeView
                  -> Menu
                  -> IO ()
hlintContextMenu ideR store treeView theMenu = do
    mbSel           <-  getSelectionHLintRecord treeView store
    item0           <-  menuItemNewWithLabel (__ "Replace")
    item0 `on` menuItemActivate $ reflectIDE (replaceHlint store treeView mbSel) ideR
    menuShellAppend theMenu item0
  where
    replaceableSelection Nothing = False
    replaceableSelection (Just s) | isNothing (parDir s) = True
                                  | otherwise = False

replaceHlint store treeView (Just sel) =
    case condIdea sel of
        Just idea | isJust (H.ideaTo idea) ->
            let lined = lines (fromJust (H.ideaTo idea))
                startColumn = srcSpanStartColumn (H.ideaSpan idea)
                source = init $ unlines (head lined :
                                            map (\ s -> replicate startColumn ' ' ++ s) (tail lined))
            in
                replaceHLintSource (srcSpanFilename (H.ideaSpan idea))
                                   (srcSpanStartLine (H.ideaSpan idea))
                                   startColumn
                                   (srcSpanEndLine (H.ideaSpan idea))
                                   (srcSpanEndColumn (H.ideaSpan idea))
                                   source
        otherwise -> return ()
replaceHlint _ _ Nothing    = return ()
