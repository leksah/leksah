--
-- | ...
-- 
module Ghf.SourceCandy (
    transformToCandy
,   transformFromCandy
,   keystrokeCandy
,   parseCandy
) where

import Data.Char(chr)
import Data.List(isSuffixOf)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView.SourceBuffer
import Control.Monad.Reader
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)

import Ghf.Core

keystrokeCandy :: CandyTableForth -> TextBuffer -> IO ()    
keystrokeCandy transformTable gtkbuf = do
    cursorMark  <- textBufferGetInsert gtkbuf
    endIter <- textBufferGetIterAtMark gtkbuf cursorMark
    offset <- textIterGetOffset endIter
    if offset < 7 
        then return ()
        else do
            startIter <- textBufferGetIterAtOffset gtkbuf (offset - 7)
            slice <- textIterGetSlice startIter endIter
            replace cursorMark slice offset transformTable
    where 
    replace :: TextMark -> String -> Int -> [(String,String)] -> IO ()
    replace cursorMark match offset [] = return ()
    replace cursorMark match offset ((from,to):rest) = do
    if isSuffixOf from match 
        then do
            sourceBufferBeginNotUndoableAction (castToSourceBuffer gtkbuf)
            start <- textBufferGetIterAtOffset gtkbuf (offset - (length from))
            end <- textBufferGetIterAtOffset gtkbuf offset
            textBufferDelete gtkbuf start end
            ins <- textBufferGetIterAtMark gtkbuf cursorMark
            textBufferInsert gtkbuf ins to
            sourceBufferEndNotUndoableAction (castToSourceBuffer gtkbuf)
        else replace cursorMark match offset rest        

transformToCandy :: CandyTableForth -> TextBuffer -> IO () 
transformToCandy transformTable gtkbuf = do
    modified <- textBufferGetModified gtkbuf
    sourceBufferBeginNotUndoableAction (castToSourceBuffer gtkbuf)
    mapM_ (replace 0) transformTable
    sourceBufferEndNotUndoableAction (castToSourceBuffer gtkbuf)
    textBufferSetModified gtkbuf modified
    where
    replace :: Int -> (String,String) -> IO ()
    replace offset (from,to) = do
        iter <- textBufferGetIterAtOffset gtkbuf offset 
        mbStartEnd <- textIterForwardSearch iter (from ++ " ") [] Nothing 
        case mbStartEnd of 
            Nothing -> return ()
            Just (st,end) -> do
                offset <- textIterGetOffset st
                textBufferDelete gtkbuf st end
                iter <- textBufferGetIterAtOffset gtkbuf offset
                textBufferInsert gtkbuf st (to ++ " ")
                replace offset (from,to)

transformFromCandy :: CandyTableBack -> TextBuffer -> IO () 
transformFromCandy transformTableBack gtkbuf = do
    modified <- textBufferGetModified gtkbuf
    sourceBufferBeginNotUndoableAction (castToSourceBuffer gtkbuf)
    mapM_ (replace 0) transformTableBack
    sourceBufferEndNotUndoableAction (castToSourceBuffer gtkbuf)
    textBufferSetModified gtkbuf modified
    where
    replace :: Int -> (String,String,Int) -> IO ()
    replace offset (to,from,spaces) = do
        iter <- textBufferGetIterAtOffset gtkbuf offset 
        mbStartEnd <- textIterForwardSearch iter from [] Nothing 
        case mbStartEnd of 
            Nothing -> return ()
            Just (st,end) -> do
                offset <- textIterGetOffset st
                textBufferDelete gtkbuf st end
                if spaces > 0
                    then do
                        iter2 <- textBufferGetIterAtOffset gtkbuf offset
                        iter3 <- textBufferGetIterAtOffset gtkbuf (offset + spaces + 1)
                        slice <- textIterGetSlice iter2 iter3 
                        let l = length (takeWhile (== ' ') slice)
                        if l > 1 
                            then do
                                textIterSetOffset iter3 (offset + l - 1)
                                textBufferDelete gtkbuf iter2 iter3
                            else return () 
                    else return ()
                iter <- textBufferGetIterAtOffset gtkbuf offset
                textBufferInsert gtkbuf iter to
                replace offset (to,from,spaces)

type CandyTable = [(String,Char,Bool,Bool)]

forthFromTable :: CandyTable -> CandyTableForth
forthFromTable table = map forthFrom table
    where
    forthFrom (str,chr,noLeadingBlanks,noTrimming) = 
        let from = if noLeadingBlanks then str else ' ' : str
            trailingBlanks = replicate (if noTrimming then 0 else length str - 1) ' '
            to = (if noLeadingBlanks then [chr] else ' ' : [chr]) ++ trailingBlanks
        in (from,to) 

backFromTable ::  CandyTable -> CandyTableBack
backFromTable table = map backFrom table
    where
    backFrom (str,chr,noLeadingBlanks,noTrimming) = 
        let numTrailingBlanks = if noTrimming then 0 else length str - 1 
        in (str,[chr],numTrailingBlanks) 

---Candy Parser

candyStyle  :: P.LanguageDef st
candyStyle  = emptyDef                      
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                }      

lexer = P.makeTokenParser candyStyle
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
hexadecimal = P.hexadecimal lexer
symbol = P.symbol lexer

parseCandy :: FileName -> IO CandyTables
parseCandy fn = do
    res <- parseFromFile candyParser fn
    case res of
        Left pe -> error $"Error reading keymap file " ++ show fn ++ " " ++ show pe
        Right r -> return (forthFromTable r, backFromTable r) 

candyParser :: CharParser () CandyTable
candyParser = do
    whiteSpace
    ls <- many oneCandyParser 
    eof
    return ls

oneCandyParser :: CharParser () (String,Char,Bool,Bool)
oneCandyParser = do
    toReplace <- toReplaceParser
    replaceWith <- replaceWithParser    
    nlb <- option False (try $do
        symbol "NoLeadingBlank"
        return True) 
    nt <- option False (try $do
        symbol "NoTrimming"
        return True)
    return (toReplace,replaceWith,nlb,nt) 

toReplaceParser :: CharParser () String
toReplaceParser   = lexeme (do
    str <- between (char '"')                   
                (char '"' <?> "end of string")
                (many $noneOf "\"") 
    return str)
    <?> "to replace string"

replaceWithParser :: CharParser () Char 
replaceWithParser = do
    char '0'        
    hd <- lexeme hexadecimal
    return (chr (fromIntegral hd))

