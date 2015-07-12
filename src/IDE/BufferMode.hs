{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.BufferMode
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.BufferMode where

import Prelude hiding(getLine)
import IDE.Core.State
import Data.List (isPrefixOf, elemIndices, isInfixOf, isSuffixOf)
import IDE.TextEditor
       (getOffset, startsLine, getIterAtMark, EditorView(..),
        getSelectionBoundMark, getInsertMark, getBuffer,
        delete, getText, forwardCharsC, insert, getIterAtLine,
        getLine, TextEditor(..), EditorBuffer(..),
        EditorIter(..))
import Data.IORef (IORef)
import System.Time (ClockTime)
import Data.Typeable (cast, Typeable)
import IDE.SourceCandy
       (getCandylessText, keystrokeCandy, transformFromCandy,
        transformToCandy)
import IDE.Utils.GUIUtils (getCandyState)
import Control.Monad (when)
import Data.Maybe (mapMaybe, catMaybes)
import IDE.Utils.FileUtils
import Graphics.UI.Gtk
       (Notebook, castToWidget, notebookPageNum, ScrolledWindow)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as T
       (isPrefixOf, lines, unlines, count, isInfixOf)

-- * Buffer Basics

--
-- | A text editor pane description
--
data IDEBuffer = forall editor. TextEditor editor => IDEBuffer {
    fileName        ::  Maybe FilePath
,   bufferName      ::  Text
,   addedIndex      ::  Int
,   sourceView      ::  EditorView editor
,   scrolledWindow  ::  ScrolledWindow
,   modTime         ::  IORef (Maybe UTCTime)
,   mode            ::  Mode
} deriving (Typeable)

instance Pane IDEBuffer IDEM
    where
    primPaneName    =   bufferName
    getAddedIndex   =   addedIndex
    getTopWidget    =   castToWidget . scrolledWindow
    paneId b        =   ""

data BufferState            =   BufferState FilePath Int
                            |   BufferStateTrans Text Text Int
    deriving(Eq,Ord,Read,Show,Typeable)

maybeActiveBuf :: IDEM (Maybe IDEBuffer)
maybeActiveBuf = do
    mbActivePane <- getActivePane
    mbPane       <- lastActiveBufferPane
    case (mbPane,mbActivePane) of
        (Just paneName1, Just (paneName2,_)) | paneName1 == paneName2 -> do
            (PaneC pane) <- paneFromName paneName1
            let mbActbuf = cast pane
            return mbActbuf
        _ -> return Nothing

lastActiveBufferPane :: IDEM (Maybe PaneName)
lastActiveBufferPane = do
    rs <- recentSourceBuffers
    case rs of
        (hd : _) -> return (Just hd)
        _        -> return Nothing

recentSourceBuffers :: IDEM [PaneName]
recentSourceBuffers = do
    recentPanes' <- readIDE recentPanes
    mbBufs       <- mapM mbPaneFromName recentPanes'
    return $ map paneName (mapMaybe (\ (PaneC p) -> cast p) (catMaybes mbBufs) :: [IDEBuffer])

getStartAndEndLineOfSelection :: TextEditor editor => EditorBuffer editor -> IDEM (Int,Int)
getStartAndEndLineOfSelection ebuf = do
    startMark   <- getInsertMark ebuf
    endMark     <- getSelectionBoundMark ebuf
    startIter   <- getIterAtMark ebuf startMark
    endIter     <- getIterAtMark ebuf endMark
    startLine   <- getLine startIter
    endLine     <- getLine endIter
    let (startLine',endLine',endIter') = if endLine >=  startLine
            then (startLine,endLine,endIter)
            else (endLine,startLine,startIter)
    b           <- startsLine endIter'
    let endLineReal = if b && endLine /= startLine then endLine' - 1 else endLine'
    return (startLine',endLineReal)

inBufContext :: MonadIDE m => alpha -> IDEBuffer -> (forall editor. TextEditor editor => Notebook -> EditorView editor -> EditorBuffer editor -> IDEBuffer -> Int -> m alpha) -> m alpha
inBufContext def (ideBuf@IDEBuffer{sourceView = v}) f = do
    (pane,_)       <-  liftIDE $ guiPropertiesFromName (paneName ideBuf)
    nb             <-  liftIDE $ getNotebook pane
    mbI            <-  liftIO $ notebookPageNum nb (scrolledWindow ideBuf)
    case mbI of
        Nothing ->  liftIO $ do
            sysMessage Normal $ bufferName ideBuf <> " notebook page not found: unexpected"
            return def
        Just i  ->  do
            ebuf <- liftIDE $ getBuffer v
            f nb v ebuf ideBuf i

inActiveBufContext :: alpha -> (forall editor. TextEditor editor => Notebook -> EditorView editor -> EditorBuffer editor -> IDEBuffer -> Int -> IDEM alpha) -> IDEM alpha
inActiveBufContext def f = do
    mbBuf <- maybeActiveBuf
    case mbBuf of
        Nothing         -> return def
        Just ideBuf ->
            inBufContext def ideBuf f

doForSelectedLines :: [a] -> (forall editor. TextEditor editor => EditorBuffer editor -> Int -> IDEM a) -> IDEM [a]
doForSelectedLines d f = inActiveBufContext d $ \_ _ ebuf currentBuffer _ -> do
    (start,end) <- getStartAndEndLineOfSelection ebuf
    beginUserAction ebuf
    result <- mapM (f ebuf) [start .. end]
    endUserAction ebuf
    return result

-- * Buffer Modes

data Mode = Mode {
    modeName               :: Text,
    modeEditComment        :: IDEAction,
    modeEditUncomment      :: IDEAction,
    modeSelectedModuleName :: IDEM (Maybe Text),
    modeEditToCandy        :: (Text -> Bool) -> IDEAction,
    modeTransformToCandy   :: forall editor . TextEditor editor => (Text -> Bool) -> EditorBuffer editor -> IDEAction,
    modeEditFromCandy      :: IDEAction,
    modeEditKeystrokeCandy :: Maybe Char -> (Text -> Bool) -> IDEAction,
    modeEditInsertCode     :: forall editor . TextEditor editor => Text -> EditorIter editor -> EditorBuffer editor -> IDEAction,
    modeEditInCommentOrString :: Text -> Bool
    }


-- | Assumes
modFromFileName :: Maybe FilePath -> Mode
modFromFileName Nothing = haskellMode
modFromFileName (Just fn) | ".hs"    `isSuffixOf` fn = haskellMode
                          | ".lhs"   `isSuffixOf` fn = literalHaskellMode
                          | ".cabal" `isSuffixOf` fn = cabalMode
                          | otherwise                = otherMode

haskellMode = Mode {
    modeName = "Haskell",
    modeEditComment = do
        doForSelectedLines [] $ \ebuf lineNr -> do
            sol <- getIterAtLine ebuf lineNr
            insert ebuf sol "--"
        return (),
    modeEditUncomment = do
        doForSelectedLines [] $ \ebuf lineNr -> do
            sol <- getIterAtLine ebuf lineNr
            sol2 <- forwardCharsC sol 2
            str   <- getText ebuf sol sol2 True
            when (str == "--") $ delete ebuf sol sol2
        return (),
    modeSelectedModuleName =
        inActiveBufContext Nothing $ \_ _ ebuf currentBuffer _ ->
            case fileName currentBuffer of
                Just filePath -> liftIO $ moduleNameFromFilePath filePath
                Nothing       -> return Nothing,
    modeTransformToCandy = \ inCommentOrString ebuf -> do
        ct <- readIDE candy
        transformToCandy ct ebuf inCommentOrString,
    modeEditToCandy = \ inCommentOrString -> do
        ct <- readIDE candy
        inActiveBufContext () $ \_ _ ebuf _ _ ->
            transformToCandy ct ebuf inCommentOrString,
    modeEditFromCandy = do
        ct      <-  readIDE candy
        inActiveBufContext () $ \_ _ ebuf _ _ ->
            transformFromCandy ct ebuf,
    modeEditKeystrokeCandy = \c inCommentOrString -> do
        ct <- readIDE candy
        inActiveBufContext () $ \_ _ ebuf _ _ ->
            keystrokeCandy ct c ebuf inCommentOrString,
    modeEditInsertCode = \ str iter buf ->
        insert buf iter str,
    modeEditInCommentOrString = \ line -> ("--" `T.isInfixOf` line)
                                        || odd (T.count "\"" line)
    }

literalHaskellMode = Mode {
    modeName = "Literal Haskell",
    modeEditComment = do
        doForSelectedLines [] $ \ebuf lineNr -> do
            sol <- getIterAtLine ebuf lineNr
            sol2 <- forwardCharsC sol 1
            str   <- getText ebuf sol sol2 True
            when (str == ">")
                (delete ebuf sol sol2)
        return (),
    modeEditUncomment = do
        doForSelectedLines [] $ \ebuf lineNr -> do
            sol <- getIterAtLine ebuf lineNr
            sol <- getIterAtLine ebuf lineNr
            sol2 <- forwardCharsC sol 1
            str  <- getText ebuf sol sol2 True
            when (str /= ">")
                (insert ebuf sol ">")
        return (),
    modeSelectedModuleName =
        inActiveBufContext Nothing $ \_ _ ebuf currentBuffer _ ->
            case fileName currentBuffer of
                Just filePath -> liftIO $ moduleNameFromFilePath filePath
                Nothing       -> return Nothing,
    modeTransformToCandy = \ inCommentOrString ebuf -> do
        ct <- readIDE candy
        transformToCandy ct ebuf inCommentOrString,
    modeEditToCandy = \ inCommentOrString -> do
        ct <- readIDE candy
        inActiveBufContext () $ \_ _ ebuf _ _ ->
            transformToCandy ct ebuf inCommentOrString,
    modeEditFromCandy = do
        ct      <-  readIDE candy
        inActiveBufContext () $ \_ _ ebuf _ _ ->
            transformFromCandy ct ebuf,
    modeEditKeystrokeCandy = \c inCommentOrString -> do
        ct <- readIDE candy
        inActiveBufContext () $ \_ _ ebuf _ _ ->
            keystrokeCandy ct c ebuf inCommentOrString,
    modeEditInsertCode = \ str iter buf ->
        insert buf iter (T.unlines $ map (\ s -> "> " <> s) $ T.lines str),
    modeEditInCommentOrString = \ line -> not (T.isPrefixOf ">" line)
                                        || odd (T.count "\"" line)
    }

cabalMode = Mode {
    modeName                 = "Cabal",
    modeEditComment = do
        doForSelectedLines [] $ \ebuf lineNr -> do
            sol <- getIterAtLine ebuf lineNr
            insert ebuf sol "--"
        return (),
    modeEditUncomment = do
        doForSelectedLines [] $ \ebuf lineNr -> do
            sol <- getIterAtLine ebuf lineNr
            sol2 <- forwardCharsC sol 2
            str   <- getText ebuf sol sol2 True
            when (str == "--") $ delete ebuf sol sol2
        return (),
    modeSelectedModuleName   = return Nothing,
    modeTransformToCandy     = \ _ _ -> return (),
    modeEditToCandy          = \ _ -> return (),
    modeEditFromCandy        = return (),
    modeEditKeystrokeCandy   = \ _ _ -> return (),
    modeEditInsertCode       = \ str iter buf -> insert buf iter str,
    modeEditInCommentOrString = T.isPrefixOf "--"

    }

otherMode = Mode {
    modeName                 = "Unknown",
    modeEditComment          = return (),
    modeEditUncomment        = return (),
    modeSelectedModuleName   = return Nothing,
    modeTransformToCandy     = \ _ _ -> return (),
    modeEditToCandy          = \ _ -> return (),
    modeEditFromCandy        = return (),
    modeEditKeystrokeCandy   = \_ _ -> return (),
    modeEditInsertCode       = \str iter buf -> insert buf iter str,
    modeEditInCommentOrString = const False
    }

isHaskellMode mode = modeName mode == "Haskell" || modeName mode == "Literal Haskell"

withCurrentMode :: alpha -> (Mode -> IDEM alpha) -> IDEM alpha
withCurrentMode def act = do
    mbBuf <- maybeActiveBuf
    case mbBuf of
        Nothing     -> return def
        Just ideBuf -> act (mode ideBuf)

editComment :: IDEAction
editComment        = withCurrentMode () modeEditComment

editUncomment :: IDEAction
editUncomment      = withCurrentMode () modeEditUncomment

selectedModuleName  :: IDEM (Maybe Text)
selectedModuleName = withCurrentMode Nothing modeSelectedModuleName

editToCandy :: IDEAction
editToCandy = withCurrentMode () (\m -> modeEditToCandy m (modeEditInCommentOrString m))

editFromCandy :: IDEAction
editFromCandy = withCurrentMode () modeEditFromCandy

editKeystrokeCandy :: Maybe Char -> IDEAction
editKeystrokeCandy c = withCurrentMode () (\m -> modeEditKeystrokeCandy m c
                            (modeEditInCommentOrString m))

editInsertCode :: TextEditor editor => EditorBuffer editor -> EditorIter editor -> Text -> IDEAction
editInsertCode buffer iter str = withCurrentMode ()
                                            (\ m -> modeEditInsertCode m str iter buffer)




