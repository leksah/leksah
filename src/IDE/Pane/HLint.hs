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
import qualified Data.Enumerator as E
       (Step(..), run_, Iteratee(..), run)
import qualified Data.Enumerator.List as EL
       (foldM, head, dropWhile, isolate)
import Data.Enumerator (($$), (>>==))
import qualified Data.List as L ()
import Control.Monad (foldM, when)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Language.Haskell.HLint (hlint, Suggestion(..), suggestionLocation)
import Language.Haskell.Exts (SrcLoc(..))
import qualified Language.Haskell.HLint as H (hlint)

data HLintRecord = HLintRecord {
            file        :: FilePath
        ,   line        :: Int
        ,   context     :: String
        ,   parDir      :: Maybe FilePath
        }


isDir HLintRecord{parDir = Nothing}  = True
isDir otherwies                     = False

-- | A HLint pane description
--

data IDEHLint       =   IDEHLint {
    scrolledView    ::   ScrolledWindow
,   treeView        ::   TreeView
,   hlintStore      ::   TreeStore HLintRecord
,   waitingHLint    ::   MVar Bool
,   activeHLint     ::   MVar Bool
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
        treeViewColumnSetTitle col1 "Context"
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

        waitingHLint <- newEmptyMVar
        activeHLint <- newEmptyMVar
        let hlint = IDEHLint {..}
        let
            gotoSource :: Bool -> IO Bool
            gotoSource focus = do
                sel <- getSelectionHLintRecord treeView hlintStore
                case sel of
                    Just record -> reflectIDE (do
                        case record of
                            HLintRecord {file=f, line=l, parDir=Just pp} ->
                                (goToSourceDefinition (pp </> f) $ Just $ Location l 0 l 0)
                                    ?>>= (\b -> when focus $ grabFocus (sourceView b))
                            _ -> return ()) ideR
                    Nothing -> return ()
                return True
        cid1 <- treeView `afterFocusIn`
            (\_ -> do reflectIDE (makeActive hlint) ideR ; return True)
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
        sel `onSelectionChanged` (gotoSource False >> return ())


        return (Just hlint,[ConnectC cid1])

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
            map (\p -> (dropFileName (ipdCabalFile p), ipdSrcDirs p)) $ packages

hlintDirectories :: [(FilePath, [FilePath])] -> IDEAction
hlintDirectories dirs = do
    hlint <- getHLint Nothing
    let store = hlintStore hlint
    ideRef <- ask
    liftIO $ do
        bringPaneToFront hlint
        forkIO $ do
            putMVar (waitingHLint hlint) True
            putMVar (activeHLint hlint) True
            takeMVar (waitingHLint hlint)

            postGUISync $ treeStoreClear store

            totalFound <- foldM (\a (dir, subDirs) -> do
                nooneWaiting <- isEmptyMVar (waitingHLint hlint)
                found <- if nooneWaiting
                    then do
                        suggestions <- take 1000 <$> H.hlint subDirs
                        reflectIDE (setHLintResults dir suggestions) ideRef
                    else return 0
                return $ a + found) 0 dirs

            nooneWaiting <- isEmptyMVar (waitingHLint hlint)
            when nooneWaiting $ postGUISync $ do
                nDir <- treeModelIterNChildren store Nothing
                treeStoreInsert store [] nDir $ HLintRecord "Search Complete" totalFound "" Nothing

            takeMVar (activeHLint hlint) >> return ()
    return ()

setHLintResults :: FilePath -> [Suggestion] -> IDEM Int
setHLintResults dir suggestions = do
    ideRef <- ask
    hlint <- getHLint Nothing
    log <- getLog
    let store = hlintStore hlint
        view  = treeView hlint
    nDir <- liftIO $ postGUISync $ do
        nDir <- treeModelIterNChildren store Nothing
        treeStoreInsert store [] nDir $ HLintRecord dir 0 dir Nothing
        when (nDir == 0) (widgetGrabFocus view >> return())
        return nDir
    foldM (\count suggestion -> liftIO $ do
        nooneWaiting <- isEmptyMVar (waitingHLint hlint)
        when nooneWaiting $ postGUISync $ do
            parent <- treeModelGetIter store [nDir]
            n <- treeModelIterNChildren store parent
            let loc = suggestionLocation suggestion
            treeStoreInsert store [nDir] n HLintRecord {
                file = srcFilename loc,
                line = srcLine loc,
                context = show suggestion,
                parDir = Just dir }
            treeStoreChange store [nDir] (\r -> r{ line = n+1 })
            when (nDir == 0 && n == 0) $
                treeViewExpandAll view
        return (count+1)) 0 suggestions

