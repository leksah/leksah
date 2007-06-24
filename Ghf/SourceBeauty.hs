module Ghf.SourceBeauty (
    transformTable
,   transformToBeauty
) where

import Data.Char(chr)
import Graphics.UI.Gtk
import Control.Monad.Reader

import Ghf.Core
import Ghf.Editor

transformToBeauty :: GhfAction
transformToBeauty = do
    inBufContext' () $ \_ gtkbuf currentBuffer _ -> lift $ do
        mapM_ (replace gtkbuf) transformTable
    where
    replace :: TextBuffer -> (String,String,String) -> IO ()
    replace gtkbuf (from,to,_) = replace' 0
        where
        replace' offset = do
            iter <- textBufferGetIterAtOffset gtkbuf offset 
            mbStartEnd <- textIterForwardSearch iter from [] Nothing 
            case mbStartEnd of 
                Nothing -> return ()
                Just (st,end) -> do
                    offset <- textIterGetOffset st
                    textBufferDelete gtkbuf st end
                    iter <- textBufferGetIterAtOffset gtkbuf offset
                    textBufferInsert gtkbuf st to
                    replace' offset 


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






