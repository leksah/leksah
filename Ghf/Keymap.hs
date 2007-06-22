module Ghf.Keymap (
    parseKeymap
,   ActionString
,   KeyString
,   Keymap
) where

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Data.Map (Map,(!))

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import Data.Maybe
import Debug.Trace

import Ghf.Core


type ActionString = String
type KeyString = String
type Keymap = Map ActionString (Maybe (Either KeyString (KeyString,KeyString)), Maybe String)
 
keymapStyle :: P.LanguageDef st
keymapStyle= emptyDef                      
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                , P.identStart     = alphaNum <|> oneOf "<>"
                , P.identLetter    = alphaNum <|> oneOf "<>"
                }      
lexer = P.makeTokenParser keymapStyle
lexeme = P.lexeme lexer
identifier = P.identifier lexer
symbol =  P.symbol lexer
whiteSpace = P.whiteSpace lexer
stringLiteral = P.stringLiteral lexer

parseKeymap :: FileName -> IO Keymap
parseKeymap fn = do
    res <- parseFromFile keymapParser fn
    case res of
        Left pe -> error $"Error reading keymap file " ++ show fn ++ " " ++ show pe
        Right r -> return r  

keymapParser :: GenParser Char () Keymap
keymapParser = do
    whiteSpace
    ls <- many lineparser 
    eof
    return (Map.fromList ls)

lineparser :: GenParser Char () (ActionString, (Maybe (Either KeyString (KeyString,KeyString)), 
                                Maybe String))   
lineparser = do  
    mb1 <- option Nothing (do 
        keyDescr <- identifier
        mb2 <- option Nothing (do
            symbol "/"
            key <- identifier
            return (Just key)) 
        return (Just (keyDescr,mb2))) 
    symbol "->"
    action <- identifier
    mbs <- option Nothing (do
        str <- stringLiteral
        return (Just str)) 
    return (case mb1 of
        Nothing -> (action,(Nothing,mbs))
        Just (keyDescr,mb2) -> 
            case mb2 of 
                Just keyDescr2 -> (action,(Just (Right (keyDescr,keyDescr2)),mbs))
                Nothing -> (action,(Just (Left keyDescr),mbs)))
    <?> "lineparser"

handleSpecialKeystrokes :: Event -> Ghf Bool
handleSpecialKeystrokes = do
    