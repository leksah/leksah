{-# LANGUAGE LambdaCase #-}
module IDE.Gtk.SourceCandy (
    transformToCandy    -- ::   TextBuffer -> IO ()
,   transformFromCandy  -- ::   TextBuffer -> IO ()
,   keystrokeCandy      -- ::   Maybe Char -> TextBuffer -> IO ()
,   getCandylessText    -- ::   TextBuffer -> IO Text

,   getCandylessPart    -- ::   CandyTable -> TextBuffer -> TextIter -> TextIter -> IO Text
,   stringToCandy       -- ::   CandyTable -> Text -> IO Text
,   positionToCandy     -- ::   CandyTable -> TextBuffer -> (Int,Int) -> IO (Int,Int)
,   positionFromCandy   -- ::   CandyTable -> TextBuffer -> (Int,Int) -> IO (Int,Int)
) where

import Prelude ()
import Prelude.Compat hiding(getChar, getLine)

import qualified Data.Set as Set
import Text.Replace (listToTrie, replaceWithTrie,
        string'fromString)
import qualified Text.Replace as TR (Replace(..))

import IDE.Core.State
import IDE.TextEditor
import Control.Monad (when, unless)
import Data.Text (Text)
import qualified Data.Text as T
       (takeWhile, pack, unpack, isSuffixOf, length, index)
import GI.Gtk.Objects.TextBuffer
       (textBufferGetIterAtMark, textBufferCreateMark, textBufferSetText)
import GI.GtkSource (bufferNew)
import GI.Gtk.Objects.TextTagTable (noTextTagTable)
import IDE.SourceCandy (notBeforeId, notBeforeOp, notAfterId, notAfterOp)
import Control.Monad.IO.Class (MonadIO)
import GI.Gtk.Objects.TextMark (IsTextMark, TextMark(..))

keystrokeCandy :: TextEditor editor => CandyTable -> Char -> EditorBuffer editor -> (Text -> Bool) -> IDEM ()
keystrokeCandy (CT(transformTable,_)) c ebuf editInCommentOrString = do
    cursorMark  <-  getInsertMark ebuf
    endIter     <-  getIterAtMark ebuf cursorMark
    offset      <-  getOffset endIter
    startIter   <-  backwardToLineStartC endIter
    slice       <-  getSlice ebuf startIter endIter True
    mbc2        <-  if c /= '\0'
                        then return (Just c)
                        else getChar endIter
    let block   =  editInCommentOrString slice
    unless block $
        replace mbc2 cursorMark slice offset transformTable
    where
    -- replace ::  Maybe Char -> m -> Text -> Int -> [(Bool,Text,Text)] -> IDEM ()
    replace mbAfterChar cursorMark match offset = replace'
        where
        replace' [] = return ()
        replace' ((isOp,from,to):rest) =
            let beforeChar  =  T.index match (max 0 (T.length match - (T.length from + 1)))
                beforeOk    =  not $ if isOp
                                        then Set.member beforeChar notBeforeOp
                                        else Set.member beforeChar notBeforeId
                afterOk     =  case mbAfterChar of
                                Nothing -> True
                                Just afterChar ->
                                     not $ if isOp
                                                then Set.member afterChar notAfterOp
                                                else Set.member afterChar notAfterId
            in if T.isSuffixOf from match && beforeOk && afterOk
                then do
                    beginNotUndoableAction ebuf
                    start   <-  getIterAtOffset ebuf (offset - T.length from)
                    end     <-  getIterAtOffset ebuf offset
                    delete ebuf start end
                    ins     <-   getIterAtMark ebuf cursorMark
                    insert ebuf ins to
                    endNotUndoableAction ebuf
                else replace mbAfterChar cursorMark match offset rest

transformToCandy :: TextEditor editor => CandyTable -> EditorBuffer editor -> (Text -> Bool) -> IDEM ()
transformToCandy (CT(transformTable,_)) ebuf editInCommentOrString = do
    beginUserAction ebuf
    modified    <-  getModified ebuf
    mapM_ (\tbl ->  replaceTo ebuf tbl 0 editInCommentOrString) transformTable
    setModified ebuf modified
    endUserAction ebuf


replaceTo :: TextEditor editor => EditorBuffer editor -> (Bool,Text,Text) -> Int -> (Text -> Bool) -> IDEM ()
replaceTo buf (isOp,from,to) offset' editInCommentOrString = replaceTo' offset'
    where
    replaceTo' offset = do
        iter        <-  getIterAtOffset buf offset
        mbStartEnd  <-  forwardSearch iter from [] Nothing
        case mbStartEnd of
            Nothing         -> return ()
            Just (st,end)   -> do
                stOff <- getOffset st
                startIter   <-  backwardToLineStartC end
                slice       <-  getSlice buf startIter end True
                let block   =   editInCommentOrString slice
                unless block $ do
                    beforeOk <-
                        if stOff == 0
                            then return True
                            else do
                                getIterAtOffset buf (stOff - 1) >>= getChar >>= \case
                                    Nothing     ->  return True
                                    Just char   ->  return (not $ if isOp
                                                                    then Set.member char notBeforeOp
                                                                    else Set.member char notBeforeId)
                    when beforeOk $ do
                        afterOk <-  do
                            endOff  <-  getOffset end
                            getIterAtOffset buf endOff >>= getChar >>= \case
                                Nothing     ->  return True
                                Just char   ->  return (not $ if isOp
                                                                then Set.member char notAfterOp
                                                                else Set.member char notAfterId)
                        when afterOk $ do
                            delete buf st end
                            insert buf st to
                            return ()
                replaceTo' (stOff + 1)

transformFromCandy :: TextEditor editor => CandyTable -> EditorBuffer editor -> IDEM ()
transformFromCandy (CT(_,transformTableBack)) ebuf = do
    beginUserAction ebuf
    modified    <-  getModified ebuf
    mapM_ (\tbl ->  replaceFrom ebuf tbl 0) transformTableBack
    endUserAction ebuf
    setModified ebuf modified

simpleGtkBuffer :: Text -> IDEM (EditorBuffer GtkSourceView)
simpleGtkBuffer contents = GtkBuffer <$> do
    buffer <- bufferNew noTextTagTable
    textBufferSetText buffer contents (-1)
    return buffer

getCandylessText :: TextEditor editor => CandyTable -> EditorBuffer editor -> IDEM Text
getCandylessText ct ebuf = do
    i1          <-  getStartIter ebuf
    i2          <-  getEndIter ebuf
    getCandylessPart ct ebuf i1 i2

getCandylessPart :: TextEditor editor => CandyTable -> EditorBuffer editor -> EditorIter editor -> EditorIter editor -> IDEM Text
getCandylessPart (CT(_,transformTableBack)) ebuf i1 i2 = do
    text1 <- getText ebuf i1 i2 True
    let replacements = listToTrie [TR.Replace (string'fromString $ T.unpack from <> replicate n ' ') (T.unpack to)
                                    | (to, from, spaces) <- transformTableBack, n <- [0..spaces]]
    return . T.pack . replaceWithTrie replacements $ T.unpack text1

stringToCandy :: CandyTable -> Text -> IDEM Text
stringToCandy  candyTable text = do
    workBuffer  <-  simpleGtkBuffer text
    transformToCandy candyTable workBuffer (const False)
    i1          <-  getStartIter workBuffer
    i2          <-  getEndIter workBuffer
    getText workBuffer i1 i2 True

-- We only need a TextMark here not a SourceMark
createTextMark
  :: MonadIO m
  => EditorBuffer GtkSourceView
  -> EditorIter GtkSourceView
  -> Bool
  -> m TextMark
createTextMark (GtkBuffer sb) (GtkIter i) = textBufferCreateMark sb Nothing i

getIterAtTextMark
  :: (MonadIO m, IsTextMark b)
  => EditorBuffer GtkSourceView
  -> b
  -> m (EditorIter GtkSourceView)
getIterAtTextMark (GtkBuffer sb) m = GtkIter <$> textBufferGetIterAtMark sb m

positionFromCandy :: TextEditor editor => CandyTable -> EditorBuffer editor -> (Int,Int) -> IDEM (Int,Int)
positionFromCandy candyTable ebuf (line,column) = do
    i1          <- getIterAtLine ebuf (max 0 (line - 1))
    i2          <- forwardToLineEndC i1
    text        <-  getText ebuf i1 i2 True
    workBuffer  <-  simpleGtkBuffer text
    i3          <- getIterAtOffset workBuffer column
    mark        <- createTextMark workBuffer i3 True
    transformFromCandy candyTable workBuffer
    i4          <- getIterAtTextMark workBuffer mark
    columnNew   <- getLineOffset i4
    return (line,columnNew)

positionToCandy :: TextEditor editor => CandyTable -> EditorBuffer editor -> (Int,Int) -> IDEM (Int,Int)
positionToCandy candyTable ebuf (line,column) = do
    i1          <- getIterAtLine ebuf (max 0 (line - 1))
    i2          <- forwardToLineEndC i1
    text        <-  getText ebuf i1 i2 True
    workBuffer  <-  simpleGtkBuffer text
    transformFromCandy candyTable workBuffer
    i3          <- getIterAtOffset workBuffer column
    mark        <- createTextMark workBuffer i3 True
    transformToCandy candyTable workBuffer (const False)
    i4          <- getIterAtTextMark workBuffer mark
    columnNew   <- getLineOffset i4
    return (line,columnNew)

replaceFrom :: TextEditor editor => EditorBuffer editor -> (Text,Text,Int) -> Int -> IDEM ()
replaceFrom buf (to,from,spaces) = replaceFrom'
    where
    replaceFrom' offset' = do
        iter        <-  getIterAtOffset buf offset'
        mbStartEnd  <-  forwardSearch iter from [] Nothing
        case mbStartEnd of
            Nothing         ->  return ()
            Just (st,end)   ->  do
                offset  <-  getOffset st
                delete buf st end
                when (spaces > 0) $ do
                    iter2 <-    getIterAtOffset buf offset
                    iter3 <-    getIterAtOffset buf (offset + spaces + 1)
                    slice <-    getSlice buf iter2 iter3 True
                    let l = T.length (T.takeWhile (== ' ') slice)
                    when (l > 1) $ do
                        iter4 <- atOffset iter3 (offset + l - 1)
                        delete buf iter2 iter4
                iter'    <-  getIterAtOffset buf offset
                insert buf iter' to
                replaceFrom' offset

