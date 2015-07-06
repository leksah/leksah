{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.SourceCandy
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- |
--
---------------------------------------------------------------------------------

module IDE.SourceCandy (
    parseCandy          -- ::   FilePath -> IO alpha
,   transformToCandy    -- ::   TextBuffer -> IO ()
,   transformFromCandy  -- ::   TextBuffer -> IO ()
,   keystrokeCandy      -- ::   Maybe Char -> TextBuffer -> IO ()
,   getCandylessText    -- ::   TextBuffer -> IO Text

,   getCandylessPart    -- ::   CandyTable -> TextBuffer -> TextIter -> TextIter -> IO Text
,   stringToCandy       -- ::   CandyTable -> Text -> IO Text
,   positionToCandy     -- ::   CandyTable -> TextBuffer -> (Int,Int) -> IO (Int,Int)
,   positionFromCandy   -- ::   CandyTable -> TextBuffer -> (Int,Int) -> IO (Int,Int)
) where

import Control.Applicative
import Prelude hiding(getChar, getLine)

import Data.Char(chr)
import Data.List (elemIndices, isInfixOf, isSuffixOf)
import Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import qualified Data.Set as Set

import IDE.Core.State
import IDE.TextEditor
import Control.Monad (when, unless)
import Data.Text (Text)
import qualified Data.Text as T
       (pack, singleton, replicate, head, takeWhile, isSuffixOf, length,
        index)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Gtk.SourceView (sourceBufferNew)
import Graphics.UI.Gtk.Multiline.TextBuffer
       (textBufferGetIterAtMark, textBufferCreateMark, textBufferSetText)

---------------------------------------------------------------------------------
-- * Implementation

notBeforeId     =   Set.fromList $['a'..'z'] ++ ['A'..'Z'] ++ "_"
notAfterId      =   Set.fromList $['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
notBeforeOp     =   Set.fromList "!#$%&*+./<=>?@\\^|-~'\""
notAfterOp      =   notBeforeOp

keystrokeCandy :: TextEditor editor => CandyTable -> Maybe Char -> EditorBuffer editor -> (Text -> Bool) -> IDEM ()
keystrokeCandy (CT(transformTable,_)) mbc ebuf editInCommentOrString = do
    cursorMark  <-  getInsertMark ebuf
    endIter     <-  getIterAtMark ebuf cursorMark
    lineNr      <-  getLine endIter
    columnNr    <-  getLineOffset endIter
    offset      <-  getOffset endIter
    startIter   <-  backwardToLineStartC endIter
    slice       <-  getSlice ebuf startIter endIter True
    mbc2        <-  case mbc of
                        Just c  -> return (Just c)
                        Nothing -> getChar endIter
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
replaceTo buf (isOp,from,to) offset editInCommentOrString = replaceTo' offset
    where
    replaceTo' offset = do
        iter        <-  getIterAtOffset buf offset
        mbStartEnd  <-  forwardSearch iter from [] Nothing
        case mbStartEnd of
            Nothing         -> return ()
            Just (st,end)   -> do
                stOff <- getOffset st
                lineNr      <-  getLine end
                columnNr    <-  getLineOffset end
                startIter   <-  backwardToLineStartC end
                slice       <-  getSlice buf startIter end True
                let block   =   editInCommentOrString slice
                unless block $ do
                    beforeOk <-
                        if stOff == 0
                            then return True
                            else do
                                iter <- getIterAtOffset buf (stOff - 1)
                                mbChar <- getChar iter
                                case mbChar of
                                    Nothing     ->  return True
                                    Just char   ->  return (not $ if isOp
                                                                    then Set.member char notBeforeOp
                                                                    else Set.member char notBeforeId)
                    when beforeOk $ do
                        afterOk <-  do
                            endOff  <-  getOffset end
                            iter    <-  getIterAtOffset buf endOff
                            mbChar  <-  getChar iter
                            case mbChar of
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
simpleGtkBuffer contents = liftIO $ GtkBuffer <$> do
    buffer <- sourceBufferNew Nothing
    textBufferSetText buffer contents
    return buffer

getCandylessText :: TextEditor editor => CandyTable -> EditorBuffer editor -> IDEM Text
getCandylessText (CT(_,transformTableBack)) ebuf = do
    i1          <-  getStartIter ebuf
    i2          <-  getEndIter ebuf
    text1       <-  getText ebuf i1 i2 True
    workBuffer  <-  simpleGtkBuffer text1
    mapM_ (\tbl ->  replaceFrom workBuffer tbl 0) transformTableBack
    i1          <-  getStartIter workBuffer
    i2          <-  getEndIter workBuffer
    getText workBuffer i1 i2 True

getCandylessPart :: TextEditor editor => CandyTable -> EditorBuffer editor -> EditorIter editor -> EditorIter editor -> IDEM Text
getCandylessPart (CT(_,transformTableBack)) ebuf i1 i2 = do
    text1       <-  getText ebuf i1 i2 True
    workBuffer  <-  simpleGtkBuffer text1
    mapM_ (\tbl ->  replaceFrom workBuffer tbl 0) transformTableBack
    i1          <-  getStartIter workBuffer
    i2          <-  getEndIter workBuffer
    getText workBuffer i1 i2 True

stringToCandy :: CandyTable -> Text -> IDEM Text
stringToCandy  candyTable text = do
    workBuffer  <-  simpleGtkBuffer text
    transformToCandy candyTable workBuffer (const False)
    i1          <-  getStartIter workBuffer
    i2          <-  getEndIter workBuffer
    getText workBuffer i1 i2 True

-- We only need a TextMark here not a SourceMark
createTextMark (GtkBuffer sb) (GtkIter i) leftGravity = liftIO $  textBufferCreateMark sb Nothing i leftGravity
getIterAtTextMark (GtkBuffer sb) m = liftIO $ GtkIter <$> textBufferGetIterAtMark sb m

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
    replaceFrom' offset = do
        iter        <-  getIterAtOffset buf offset
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
                iter    <-  getIterAtOffset buf offset
                insert buf iter to
                replaceFrom' offset

type CandyTableI = [(Text,Char,Bool)]

forthFromTable :: CandyTableI -> CandyTableForth
forthFromTable = map forthFrom
    where
    forthFrom (str,chr,noTrimming) =
        let isOp = not (Set.member (T.head str) notBeforeId)
            from = str
            trailingBlanks = T.replicate (if noTrimming then 0 else T.length str - 1) (T.singleton ' ')
            to = T.singleton chr <> trailingBlanks
        in (isOp,from,to)

backFromTable :: CandyTableI -> CandyTableBack
backFromTable = map backFrom
    where
    backFrom (str,chr,noTrimming) =
        let numTrailingBlanks = if noTrimming then 0 else T.length str - 1
        in (str,T.singleton chr,numTrailingBlanks)

---Candy Parser

candyStyle  :: P.LanguageDef st
candyStyle  = emptyDef
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                }

lexer       =   P.makeTokenParser candyStyle
lexeme      =   P.lexeme lexer
whiteSpace  =   P.whiteSpace lexer
hexadecimal =   P.hexadecimal lexer
symbol      =   P.symbol lexer

parseCandy :: FilePath -> IO CandyTable
parseCandy fn = do
    res     <-  parseFromFile candyParser fn
    case res of
        Left pe ->  throwIDE $ "Error reading candy file " <> T.pack (show fn) <> " " <> T.pack (show pe)
        Right r ->  return (CT(forthFromTable r, backFromTable r))

candyParser :: CharParser () CandyTableI
candyParser = do
    whiteSpace
    ls  <-  P.many oneCandyParser
    eof
    return ls

oneCandyParser :: CharParser () (Text,Char,Bool)
oneCandyParser = do
    toReplace   <-  toReplaceParser
    replaceWith <-  replaceWithParser
    nt          <-  option True (try $do
        symbol "Trimming"
        return False)
    return (toReplace,replaceWith,nt)

toReplaceParser :: CharParser () Text
toReplaceParser   = lexeme (do
    str         <-  between (char '"')
                        (char '"' <?> "end of string")
                        (P.many $noneOf "\"")
    return $ T.pack str)
    <?> "to replace string"

replaceWithParser :: CharParser () Char
replaceWithParser = do
    char '0'
    hd  <-  lexeme hexadecimal
    return (chr (fromIntegral hd))


