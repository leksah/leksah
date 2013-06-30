{-# LANGUAGE CPP, FlexibleInstances, DeriveDataTypeable, MultiParamTypeClasses,
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
import IDE.Core.State
import IDE.BufferMode
import IDE.LogRef (logOutput, defaultLineLogger)
import IDE.Pane.SourceBuffer
    (goToSourceDefinition, maybeActiveBuf, IDEBuffer(..))
import IDE.TextEditor (grabFocus)
import Control.Applicative ((<$>))
import System.FilePath ((</>), dropFileName)
import System.Exit (ExitCode(..))
import IDE.Pane.Log (getLog)
import Control.DeepSeq
import qualified Data.Enumerator as E
       (Step(..), run_, Iteratee(..), run)
import qualified Data.Enumerator.List as EL
       (foldM, head, dropWhile, isolate)
import Data.Enumerator (($$), (>>==))
import qualified Data.List as L ()
import Control.Monad (forM_, foldM, when)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Language.Haskell.HLint (Suggestion(..), suggestionLocation)
import Language.Haskell.Exts (SrcLoc(..))
import qualified Language.Haskell.HLint as H (hlint)
import IDE.Utils.GUIUtils (__)

data HLintRecord = HLintRecord {
            file        :: FilePath
        ,   line        :: Int
        ,   context     :: String
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
    saveState p     =   do
        return (Just HLintState)
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
            gotoSource :: Bool -> IO Bool
            gotoSource focus = do
                sel <- getSelectionHLintRecord treeView hlintStore
                case sel of
                    Just record -> reflectIDE (do
                        case record of
                            HLintRecord {file=f, line=l, parDir=Just pp} ->
                                (goToSourceDefinition (pp </> f) $ Just $ Location l 0 l 0)
                                    ?>>= (\(IDEBuffer {sourceView = sv}) -> when focus $ grabFocus sv)
                            _ -> return ()) ideR
                    Nothing -> return ()
                return True
        cid1 <- treeView `afterFocusIn`
            (\_ -> do reflectIDE (makeActive hlint) ideR ; return True)
        cid2 <- treeView `onRowExpanded` \ iter path -> do
            record <- treeStoreGetValue hlintStore path
            case record of
                HLintRecord { file = f, parDir = Nothing } -> refreshDir hlintStore iter f
                _ -> reflectIDE (ideMessage Normal (__ "Unexpected Expansion in HLint Pane")) ideR
        cid3 <- treeView `onRowActivated` \ path col -> do
            record <- treeStoreGetValue hlintStore path
            mbIter <- treeModelGetIter hlintStore path
            case (mbIter, record) of
                (Just iter, HLintRecord { file = f, parDir = Nothing }) -> refreshDir hlintStore iter f
                _ -> return ()
        cid4 <- treeView `onKeyPress`
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
        sel `onSelectionChanged` (gotoSource False >> return ())

        return (Just hlint,map ConnectC [cid1, cid2, cid3, cid4])

getHLint :: Maybe PanePath -> IDEM IDEHLint
getHLint Nothing    = forceGetPane (Right "*HLint")
getHLint (Just pp)  = forceGetPane (Left pp)

hlintLineParser :: CharParser () HLintRecord
hlintLineParser = try (do
        file <- many (noneOf ":")
        char ':'
        line <- int
        char ':'
        context <- many anyChar
        let parDir = Nothing
        return $ HLintRecord {..}
    <?> "hlintLineParser")

lexer = P.makeTokenParser emptyDef
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
hexadecimal = P.hexadecimal lexer
symbol = P.symbol lexer
identifier = P.identifier lexer
colon = P.colon lexer
int = fmap fromInteger $ P.integer lexer

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
            Just active -> active : (filter (/= active) $ wsPackages ws)
            Nothing     -> wsPackages ws
    lift $ hlintDirectories $
            concatMap (\p -> map (dropFileName (ipdCabalFile p) </>) $ ipdSrcDirs p) $ packages

hlintDirectories :: [FilePath] -> IDEAction
hlintDirectories dirs = do
    hlint <- getHLint Nothing
    let store = hlintStore hlint
    liftIO $ do
        treeStoreClear store
        forM_ dirs $ \ dir -> do
            nDir <- treeModelIterNChildren store Nothing
            treeStoreInsert store [] nDir $ HLintRecord dir 0 dir Nothing
            treeStoreInsert store [nDir] 0 $ HLintRecord dir 0 dir Nothing

refreshDir :: TreeStore HLintRecord -> TreeIter -> FilePath -> IO ()
refreshDir store iter dir = do
    mbHlintDir <- leksahSubDir "hlint"
    let datadirOpt = case mbHlintDir of
                        Just d  -> "--datadir":[d]
                        Nothing -> []
    suggestions <- take 1000 <$> H.hlint ("--quiet":dir:datadirOpt)
    setHLintResults store iter dir suggestions
    return ()

hlintRecord dir suggestion = HLintRecord {
    file = srcFilename loc,
    line = srcLine loc,
    context = show suggestion,
    parDir = Just dir}
  where
    loc = suggestionLocation suggestion

setHLintResults :: TreeStore HLintRecord -> TreeIter -> FilePath -> [Suggestion] -> IO Int
setHLintResults store parent dir suggestions = do
    parentPath <- treeModelGetPath store parent
    forM_ (zip [0..] records) $ \(n, record) -> do
        mbChild <- treeModelIterNthChild store (Just parent) n
        findResult <- find record store mbChild
        case (mbChild, findResult) of
            (_, WhereExpected _) -> return ()
            (Just iter, Found _) -> do
                path <- treeModelGetPath store iter
                removeUntil record store path
            _ -> do
                treeStoreInsert store parentPath n $ record
    removeRemaining store (parentPath++[nRecords])
    return nRecords
  where
    records = map (hlintRecord dir) suggestions
    nRecords = length records

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
