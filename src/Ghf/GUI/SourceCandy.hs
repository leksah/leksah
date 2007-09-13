--
-- | ..       .
--
module Ghf.GUI.SourceCandy (
    transformToCandy
,   transformFromCandy
,   keystrokeCandy
,   parseCandy
,   getCandylessText
) where

import Data.Char(chr)
import Data.List(isSuffixOf)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView.SourceBuffer
import Control.Monad.Reader
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import qualified Data.Set as Set

import Ghf.Core

notBeforeId     =   Set.fromList $['a'..'z'] ++ ['A'..'Z'] ++ ['_']
notAfterId      =   Set.fromList $['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
notBeforeOp     =   Set.fromList $['!','#','$','%','&','*','+','.','/','<','=','>','?','@','\\',
                                    '^','|','-','~','\'','"']
notAfterOp      =   notBeforeOp


keystrokeCandy :: Maybe Char -> CandyTableForth -> TextBuffer -> IO ()
keystrokeCandy mbc transformTable gtkbuf = do
    cursorMark  <-  textBufferGetInsert gtkbuf
    endIter     <-  textBufferGetIterAtMark gtkbuf cursorMark
    offset      <-  textIterGetOffset endIter
    let sliceStart = if offset < 8 then 0 else offset - 8
    startIter   <-  textBufferGetIterAtOffset gtkbuf sliceStart
    slice       <-  textIterGetSlice startIter endIter
    replace mbc cursorMark slice offset transformTable
    where
    replace ::  Maybe Char -> TextMark -> String ->   Int -> [(Bool,String,String)] -> IO ()
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
                    sourceBufferBeginNotUndoableAction (castToSourceBuffer gtkbuf)
                    start   <-  textBufferGetIterAtOffset gtkbuf (offset - (length from))
                    end     <-  textBufferGetIterAtOffset gtkbuf offset
                    textBufferDelete gtkbuf start end
                    ins     <-   textBufferGetIterAtMark gtkbuf cursorMark
                    textBufferInsert gtkbuf ins to
                    sourceBufferEndNotUndoableAction (castToSourceBuffer gtkbuf)
                else replace mbAfterChar cursorMark match offset rest

transformToCandy :: CandyTableForth -> TextBuffer -> IO ()
transformToCandy transformTable gtkbuf = do
    textBufferBeginUserAction gtkbuf
    modified    <-  textBufferGetModified gtkbuf
    mapM_ (\tbl ->  replaceTo gtkbuf tbl 0) transformTable
    textBufferSetModified gtkbuf modified
    textBufferEndUserAction gtkbuf


replaceTo :: TextBuffer -> (Bool,String,String) -> Int -> IO ()
replaceTo buf (isOp,from,to) offset = replaceTo' offset
    where
    replaceTo' offset = do
        iter        <-  textBufferGetIterAtOffset buf offset
        mbStartEnd  <-  textIterForwardSearch iter from [] Nothing
        case mbStartEnd of
            Nothing         -> return ()
            Just (st,end)   -> do
                stOff <- textIterGetOffset st
                beforeOk <-
                    if stOff == 0
                        then return True
                        else do
                            iter <- textBufferGetIterAtOffset buf (stOff - 1)
                            mbChar <- textIterGetChar iter
                            case mbChar of
                                Nothing     ->  return True
                                Just char   ->  return (not $if isOp
                                                                then Set.member char notBeforeOp
                                                                else Set.member char notBeforeId)
                if beforeOk
                    then do
                        afterOk <-  do
                            endOff  <-  textIterGetOffset end
                            iter    <-  textBufferGetIterAtOffset buf endOff
                            mbChar  <-  textIterGetChar iter
                            case mbChar of
                                Nothing     ->  return True
                                Just char   ->  return (not $if isOp
                                                                then Set.member char notAfterOp
                                                                else Set.member char notAfterId)
                        if afterOk
                            then do
                                textBufferDelete buf st end
                                textBufferInsert buf st to
                                return ()
                            else do
                                return ()
                    else do
                    return ()
                replaceTo' (stOff + 1)

transformFromCandy :: CandyTableBack -> TextBuffer -> IO ()
transformFromCandy transformTableBack gtkbuf = do
    textBufferBeginUserAction gtkbuf
    modified    <-  textBufferGetModified gtkbuf
    mapM_ (\tbl ->  replaceFrom gtkbuf tbl 0) transformTableBack
    textBufferEndUserAction gtkbuf
    textBufferSetModified gtkbuf modified

getCandylessText :: CandyTableBack -> TextBuffer -> IO (String)
getCandylessText transformTableBack gtkbuf = do
    workBuffer  <-  textBufferNew Nothing
    i1          <-  textBufferGetStartIter gtkbuf
    i2          <-  textBufferGetEndIter gtkbuf
    text        <-  textBufferGetText gtkbuf i1 i2 True
    textBufferSetText workBuffer text
    mapM_ (\tbl ->  replaceFrom workBuffer tbl 0) transformTableBack
    i1          <-  textBufferGetStartIter workBuffer
    i2          <-  textBufferGetEndIter workBuffer
    text        <-  textBufferGetText workBuffer i1 i2 True
    return text

replaceFrom :: TextBuffer -> (String,String,Int) -> Int -> IO ()
replaceFrom buf (to,from,spaces) offset = replaceFrom' offset
    where
    replaceFrom' offset = do
        iter        <-  textBufferGetIterAtOffset buf offset
        mbStartEnd  <-  textIterForwardSearch iter from [] Nothing
        case mbStartEnd of
            Nothing         ->  return ()
            Just (st,end)   ->  do
                offset  <-  textIterGetOffset st
                textBufferDelete buf st end
                if spaces > 0
                    then do
                        iter2 <-    textBufferGetIterAtOffset buf offset
                        iter3 <-    textBufferGetIterAtOffset buf (offset + spaces + 1)
                        slice <-    textIterGetSlice iter2 iter3
                        let l = length (takeWhile (== ' ') slice)
                        if l > 1
                            then do
                                textIterSetOffset iter3 (offset + l - 1)
                                textBufferDelete buf iter2 iter3
                            else return ()
                    else return ()
                iter    <-  textBufferGetIterAtOffset buf offset
                textBufferInsert buf iter to
                replaceFrom' offset

type CandyTable = [(String,Char,Bool)]

forthFromTable :: CandyTable -> CandyTableForth
forthFromTable table = map forthFrom table
    where
    forthFrom (str,chr,noTrimming) =
        let isOp = not (Set.member (head str) notBeforeId)
            from = str
            trailingBlanks = replicate (if noTrimming then 0 else length str - 1) ' '
            to = [chr] ++ trailingBlanks
        in (isOp,from,to)

backFromTable :: CandyTable -> CandyTableBack
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

parseCandy :: FileName -> IO CandyTables
parseCandy fn = do
    res     <-  parseFromFile candyParser fn
    case res of
        Left pe ->  error $"Error reading keymap file " ++ show fn ++ " " ++ show pe
        Right r ->  return (forthFromTable r, backFromTable r)

candyParser :: CharParser () CandyTable
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
