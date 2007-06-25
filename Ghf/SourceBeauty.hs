module Ghf.SourceBeauty (
    transformToBeauty
,   transformFromBeauty
,   mayBeautify
) where

import Data.Char(chr)
import Data.List(isSuffixOf)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView.SourceBuffer
import Control.Monad.Reader

import Ghf.Core

mayBeautify :: TextBuffer -> IO () 
mayBeautify gtkbuf = do
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

transformToBeauty :: TextBuffer -> IO () 
transformToBeauty gtkbuf = do
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

transformFromBeauty :: TextBuffer -> IO () 
transformFromBeauty gtkbuf = do
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



transformTable :: [(String,String)]
transformTable = [
     (" ->", [ ' ',chr 0x2192,' ']) -- RIGHTWARDS ARROW
    ,(" <-", [ ' ',chr 0x2190,' ']) --LEFTWARDS ARROW
    ,(" =>", [ ' ',chr 0x21d2,' ']) --RIGHTWARDS DOUBLE ARROW
    ,(" >=", [ ' ',chr 0x2265,' ']) --GREATER-THAN OR EQUAL TO
    ,(" <=", [ ' ',chr 0x2264,' ']) --LESS-THAN OR EQUAL TO
    ,(" /=", [ ' ',chr 0x2260,' ']) --NOT EQUAL TO
    ,(" &&", [ ' ',chr 0x2227,' ']) --LOGICAL AND
    ,(" ||", [ ' ',chr 0x2228,' ']) --LOGICAL OR
    ,(" ++", [ ' ',chr 0x2295,' ']) --CIRCLED PLUS
    ,(" ::", [ ' ',chr 0x2237,' ']) --PROPORTION
    ,(" ..", [ ' ',chr 0x2025,' ']) --TWO DOT LEADER
    
    ,(" ^" , [' ',chr 0x2191]) --UPWARDS ARROW
    ,(" ." , [' ',chr 0x2218]) --RING OPERATOR
    ,("\\" , [chr 0x03bb]) --GREEK SMALL LETTER LAMDA
    ,(" forall",[' ',chr 0x2200]) --FOR ALL
    ,(" exist", [' ',chr 0x2203]) --THERE EXISTS
    ,(" not",[' ',chr 0x00ac,' ',' '])]  --NOT SIGN

transformTableBack :: [(String,String,Int)]
transformTableBack = [
     ("->", [chr 0x2192],1) -- RIGHTWARDS ARROW
    ,("<-", [chr 0x2190],1) --LEFTWARDS ARROW
    ,("=>", [chr 0x21d2],1) --RIGHTWARDS DOUBLE ARROW
    ,(">=", [chr 0x2265],1) --GREATER-THAN OR EQUAL TO
    ,("<=", [chr 0x2264],1) --LESS-THAN OR EQUAL TO
    ,("/=", [chr 0x2260],1) --NOT EQUAL TO
    ,("&&", [chr 0x2227],1) --LOGICAL AND
    ,("||", [chr 0x2228],1) --LOGICAL OR
    ,("++", [chr 0x2295],1) --CIRCLED PLUS
    ,("::", [chr 0x2237],1) --PROPORTION
    ,("..", [chr 0x2025],1) --TWO DOT LEADER
    
    ,("^" , [chr 0x2191],0) --UPWARDS ARROW
    ,("." , [chr 0x2218],0) --RING OPERATOR
    ,("\\"  , [chr 0x03bb],0) --GREEK SMALL LETTER LAMDA
    ,("forall",[chr 0x2200],0) --FOR ALL
    ,("exist", [chr 0x2203],0) --THERE EXISTS
    ,("not",[chr 0x00ac],2)]  --NOT SIGN


{--
transformTable :: [(String,String,String)]
transformTable = [
     (" -> ", [ ' ',chr 0x2192,' ',' '], [ ' ',chr 0xe2,chr 0x86,chr 0x92,' ',' ']) -- RIGHTWARDS ARROW
    ,(" <- ", [ ' ',chr 0x2190,' ',' '], [ ' ',chr 0xe2,chr 0x86,chr 0x90,' ',' ']) --LEFTWARDS ARROW
    ,(" => ", [ ' ',chr 0x21d2,' ',' '], [ ' ',chr 0xe2,chr 0x87,chr 0x92,' ',' ']) --RIGHTWARDS DOUBLE ARROW
    ,(" >= ", [ ' ',chr 0x2265,' ',' '], [ ' ',chr 0xe2,chr 0x89,chr 0xa5,' ',' ']) --GREATER-THAN OR EQUAL TO
    ,(" <= ", [ ' ',chr 0x2264,' ',' '], [ ' ',chr 0xe2,chr 0x89,chr 0xa4,' ',' ']) --LESS-THAN OR EQUAL TO
    ,(" /= ", [ ' ',chr 0x2260,' ',' '], [ ' ',chr 0xe2,chr 0x89,chr 0xa0,' ',' ']) --NOT EQUAL TO
    ,(" && ", [ ' ',chr 0x2227,' ',' '], [ ' ',chr 0xe2,chr 0x88,chr 0xa7,' ',' ']) --LOGICAL AND
    ,(" || ", [ ' ',chr 0x2228,' ',' '], [ ' ',chr 0xe2,chr 0x88,chr 0xa8,' ',' ']) --LOGICAL OR
    ,(" ++ ", [ ' ',chr 0x2295,' ',' '], [ ' ',chr 0xe2,chr 0x86,chr 0x95,' ',' ']) --CIRCLED PLUS
    ,(" :: ", [ ' ',chr 0x2237,' ',' '], [ ' ',chr 0xe2,chr 0x88,chr 0xb7,' ',' ']) --PROPORTION
    ,(" .. ", [ ' ',chr 0x2025,' ',' '], [ ' ',chr 0xe2,chr 0x80,chr 0xa5,' ',' ']) --TWO DOT LEADER
    
    ,(" ^ " , [' ',chr 0x2191,' '], [' ',chr 0xe2,chr 0x86,chr 0x91,' ']) --UPWARDS ARROW
    ,(" . " , [' ',chr 0x2218,' '], [' ',chr 0xe2,chr 0x88,chr 0x98,' ']) --RING OPERATOR
    ,("\\"  , [chr 0x03bb], [chr 0xce,chr 0xbb]) --GREEK SMALL LETTER LAMDA
    ,(" forall ",[' ',chr 0x2200],[' ',chr 0xe2,chr 0x88,chr 0x80,' ']) --FOR ALL
    ,(" exist ", [' ',chr 0x2203],[' ',chr 0xe2,chr 0x88,chr 0x83,' ']) --THERE EXISTS
    ,(" not ",[' ',chr 0x00ac,' ',' ',' '], [' ',chr 0xc2,chr 0xac,' ',' ',' '])]  --NOT SIGN
--}





