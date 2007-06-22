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
type Keymap = Map ActionString (Either KeyString (KeyString,KeyString))
 
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
charLiteral = P.charLiteral lexer
whiteSpace = P.whiteSpace lexer

parseKeymap :: FileName -> IO (Map ActionString (Either KeyString (KeyString,KeyString)))
parseKeymap fn = do
    res <- parseFromFile keymapParser fn
    case res of
        Left pe -> error $"Error reading keymap file " ++ show fn ++ " " ++ show pe
        Right r -> return r  

keymapParser :: GenParser Char () (Map String (Either String (String,String)))
keymapParser = do
    whiteSpace
    ls <- many (lexeme lineparser) 
    eof
    return (Map.fromList $map fromJust $filter isJust ls)

lineparser :: GenParser Char () (Maybe (ActionString, (Either KeyString (KeyString,KeyString)))) 
lineparser = do
    try complexkey 
    <|> try simplekey 
    <|> meaningless 
    <?> "lineparser"    

complexkey :: GenParser Char () (Maybe (ActionString, (Either KeyString (KeyString,KeyString))))    
complexkey = trace "complex" (do  
    keyDescr <- identifier
    trace "after key1" (return ())
    symbol "/"
    keyDescr2 <- identifier
    symbol "->"
    action <- identifier
    return (Just (action,(Right (keyDescr,keyDescr2))))
    <?> "complexkey")

simplekey :: GenParser Char () (Maybe (ActionString, (Either KeyString (KeyString,KeyString))))    
simplekey = trace "simple" (do  
    keyDescr <- identifier
    trace "after key2" (return ())
    symbol "->"
    trace "after symbol2" (return ())
    action <- identifier
    trace "after action2" (return ())
    return (Just (action,(Left keyDescr)))
    <?> "simplekey")

meaningless :: GenParser Char () (Maybe (ActionString, (Either KeyString (KeyString,KeyString))))    
meaningless = trace "meaningless" (do
    symbol "->"
    identifier
    return Nothing
    <?> "meaningless")  
