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
,   getCandylessText    -- ::   TextBuffer -> IO String

,   getCandylessPart    -- ::   CandyTable -> TextBuffer -> TextIter -> TextIter -> IO String
,   stringToCandy       -- ::   CandyTable -> String -> IO String
,   positionToCandy     -- ::   CandyTable -> TextBuffer -> (Int,Int) -> IO (Int,Int)
,   positionFromCandy   -- ::   CandyTable -> TextBuffer -> (Int,Int) -> IO (Int,Int)
) where

import Prelude hiding(getChar, getLine)

import Data.Char(chr)
import Data.List(isSuffixOf)
import Control.Monad.Reader
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import qualified Data.Set as Set

import IDE.Core.State
import IDE.TextEditor

---------------------------------------------------------------------------------
-- * Implementation

notBeforeId     =   Set.fromList $['a'..'z'] ++ ['A'..'Z'] ++ ['_']
notAfterId      =   Set.fromList $['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
notBeforeOp     =   Set.fromList $['!','#','$','%','&','*','+','.','/','<','=','>','?','@','\\',
                                    '^','|','-','~','\'','"','\n']
notAfterOp      =   notBeforeOp

keystrokeCandy :: CandyTable -> Maybe Char -> EditorBuffer -> IDEM ()
keystrokeCandy (CT(transformTable,_)) mbc ebuf = do
    cursorMark  <-  getInsertMark ebuf
    endIter     <-  getIterAtMark ebuf cursorMark
    offset      <-  getOffset endIter
    let sliceStart = if offset < 8 then 0 else offset - 8
    startIter   <-  getIterAtOffset ebuf sliceStart
    slice       <-  getSlice ebuf startIter endIter True
    mbc2        <-  case mbc of
                        Just c -> return (Just c)
                        Nothing -> do
                            getChar endIter
    replace mbc2 cursorMark slice offset transformTable
    where
    replace ::  Maybe Char -> EditorMark -> String -> Int -> [(Bool,String,String)] -> IDEM ()
    replace mbAfterChar cursorMark match offset list = replace' list
        where
        replace' [] = return ()
        replace' ((isOp,from,to):rest) =
            let beforeChar  =  match !! (max 0 (length match - (length from + 1)))
                beforeOk    =  not $if isOp
                                    then Set.member beforeChar notBeforeOp
                                    else Set.member beforeChar notBeforeId
                afterOk     =  case mbAfterChar of
                                Nothing -> True
                                Just afterChar ->
                                     not $if isOp
                                        then Set.member afterChar notAfterOp
                                        else Set.member afterChar notAfterId
            in if isSuffixOf from match && beforeOk && afterOk
                then do
                    beginNotUndoableAction ebuf
                    start   <-  getIterAtOffset ebuf (offset - (length from))
                    end     <-  getIterAtOffset ebuf offset
                    delete ebuf start end
                    ins     <-   getIterAtMark ebuf cursorMark
                    insert ebuf ins to
                    endNotUndoableAction ebuf
                else replace mbAfterChar cursorMark match offset rest

transformToCandy :: CandyTable -> EditorBuffer -> IDEM ()
transformToCandy (CT(transformTable,_)) ebuf = do
    beginUserAction ebuf
    modified    <-  getModified ebuf
    mapM_ (\tbl ->  replaceTo ebuf tbl 0) transformTable
    setModified ebuf modified
    endUserAction ebuf


replaceTo :: EditorBuffer -> (Bool,String,String) -> Int -> IDEM ()
replaceTo buf (isOp,from,to) offset = replaceTo' offset
    where
    replaceTo' offset = do
        iter        <-  getIterAtOffset buf offset
        mbStartEnd  <-  forwardSearch iter from [] Nothing
        case mbStartEnd of
            Nothing         -> return ()
            Just (st,end)   -> do
                stOff <- getOffset st
                beforeOk <-
                    if stOff == 0
                        then return True
                        else do
                            iter <- getIterAtOffset buf (stOff - 1)
                            mbChar <- getChar iter
                            case mbChar of
                                Nothing     ->  return True
                                Just char   ->  return (not $if isOp
                                                                then Set.member char notBeforeOp
                                                                else Set.member char notBeforeId)
                if beforeOk
                    then do
                        afterOk <-  do
                            endOff  <-  getOffset end
                            iter    <-  getIterAtOffset buf endOff
                            mbChar  <-  getChar iter
                            case mbChar of
                                Nothing     ->  return True
                                Just char   ->  return (not $if isOp
                                                                then Set.member char notAfterOp
                                                                else Set.member char notAfterId)
                        if afterOk
                            then do
                                delete buf st end
                                insert buf st to
                                return ()
                            else do
                                return ()
                    else do
                    return ()
                replaceTo' (stOff + 1)

transformFromCandy :: CandyTable -> EditorBuffer -> IDEM ()
transformFromCandy (CT(_,transformTableBack)) ebuf = do
    beginUserAction ebuf
    modified    <-  getModified ebuf
    mapM_ (\tbl ->  replaceFrom ebuf tbl 0) transformTableBack
    endUserAction ebuf
    setModified ebuf modified

getCandylessText :: CandyTable -> EditorBuffer -> IDEM String
getCandylessText (CT(_,transformTableBack)) ebuf = do
    i1          <-  getStartIter ebuf
    i2          <-  getEndIter ebuf
    text1       <-  getText ebuf i1 i2 True
    workBuffer  <-  newGtkBuffer Nothing text1
    mapM_ (\tbl ->  replaceFrom workBuffer tbl 0) transformTableBack
    i1          <-  getStartIter workBuffer
    i2          <-  getEndIter workBuffer
    text2       <-  getText workBuffer i1 i2 True
    return text2

getCandylessPart :: CandyTable -> EditorBuffer -> EditorIter -> EditorIter -> IDEM String
getCandylessPart (CT(_,transformTableBack)) ebuf i1 i2 = do
    text1       <-  getText ebuf i1 i2 True
    workBuffer  <-  newGtkBuffer Nothing text1
    mapM_ (\tbl ->  replaceFrom workBuffer tbl 0) transformTableBack
    i1          <-  getStartIter workBuffer
    i2          <-  getEndIter workBuffer
    text2       <-  getText workBuffer i1 i2 True
    return text2

stringToCandy :: CandyTable -> String -> IDEM String
stringToCandy  candyTable text = do
    workBuffer  <-  newGtkBuffer Nothing text
    transformToCandy candyTable workBuffer
    i1          <-  getStartIter workBuffer
    i2          <-  getEndIter workBuffer
    text2       <-  getText workBuffer i1 i2 True
    return text2

positionFromCandy :: CandyTable -> EditorBuffer -> (Int,Int) -> IDEM (Int,Int)
positionFromCandy candyTable ebuf (line,column) = do
    i1          <- getIterAtLine ebuf (max 0 (line - 1))
    i2          <- copyIter i1
    forwardToLineEnd i2
    text        <-  getText ebuf i1 i2 True
    workBuffer  <-  newGtkBuffer Nothing text
    i3          <- getIterAtOffset workBuffer column
    mark        <- createMark workBuffer Nothing i3 True
    transformFromCandy candyTable workBuffer
    i4          <- getIterAtMark workBuffer mark
    columnNew   <- getLineOffset i4
    return (line,columnNew)

positionToCandy :: CandyTable -> EditorBuffer -> (Int,Int) -> IDEM (Int,Int)
positionToCandy candyTable ebuf (line,column) = do
    i1          <- getIterAtLine ebuf (max 0 (line - 1))
    i2          <- copyIter i1
    forwardToLineEnd i2
    text        <-  getText ebuf i1 i2 True
    workBuffer  <-  newGtkBuffer Nothing text
    transformFromCandy candyTable workBuffer
    i3          <- getIterAtOffset workBuffer column
    mark        <- createMark workBuffer Nothing i3 True
    transformToCandy candyTable workBuffer
    i4          <- getIterAtMark workBuffer mark
    columnNew   <- getLineOffset i4
    return (line,columnNew)

replaceFrom :: EditorBuffer -> (String,String,Int) -> Int -> IDEM ()
replaceFrom buf (to,from,spaces) offset = replaceFrom' offset
    where
    replaceFrom' offset = do
        iter        <-  getIterAtOffset buf offset
        mbStartEnd  <-  forwardSearch iter from [] Nothing
        case mbStartEnd of
            Nothing         ->  return ()
            Just (st,end)   ->  do
                offset  <-  getOffset st
                delete buf st end
                if spaces > 0
                    then do
                        iter2 <-    getIterAtOffset buf offset
                        iter3 <-    getIterAtOffset buf (offset + spaces + 1)
                        slice <-    getSlice buf iter2 iter3 True
                        let l = length (takeWhile (== ' ') slice)
                        if l > 1
                            then do
                                setOffset iter3 (offset + l - 1)
                                delete buf iter2 iter3
                            else return ()
                    else return ()
                iter    <-  getIterAtOffset buf offset
                insert buf iter to
                replaceFrom' offset

type CandyTableI = [(String,Char,Bool)]

forthFromTable :: CandyTableI -> CandyTableForth
forthFromTable table = map forthFrom table
    where
    forthFrom (str,chr,noTrimming) =
        let isOp = not (Set.member (head str) notBeforeId)
            from = str
            trailingBlanks = replicate (if noTrimming then 0 else length str - 1) ' '
            to = chr : trailingBlanks
        in (isOp,from,to)

backFromTable :: CandyTableI -> CandyTableBack
backFromTable table = map backFrom table
    where
    backFrom (str,chr,noTrimming) =
        let numTrailingBlanks = if noTrimming then 0 else length str - 1
        in (str,[chr],numTrailingBlanks)

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
        Left pe ->  throwIDE $"Error reading keymap file " ++ show fn ++ " " ++ show pe
        Right r ->  return (CT(forthFromTable r, backFromTable r))

candyParser :: CharParser () CandyTableI
candyParser = do
    whiteSpace
    ls  <-  many oneCandyParser
    eof
    return ls

oneCandyParser :: CharParser () (String,Char,Bool)
oneCandyParser = do
    toReplace   <-  toReplaceParser
    replaceWith <-  replaceWithParser
    nt          <-  option True (try $do
        symbol "Trimming"
        return False)
    return (toReplace,replaceWith,nt)

toReplaceParser :: CharParser () String
toReplaceParser   = lexeme (do
    str         <-  between (char '"')
                        (char '"' <?> "end of string")
                        (many $noneOf "\"")
    return str)
    <?> "to replace string"

replaceWithParser :: CharParser () Char
replaceWithParser = do
    char '0'
    hd  <-  lexeme hexadecimal
    return (chr (fromIntegral hd))
