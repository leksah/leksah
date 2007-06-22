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
type Keymap = Map ActionString (Maybe ((Either KeyString (KeyString,KeyString)),
        MaybeString), Maybe String)
 
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

parseKeymap :: FileName -> IO Keymap
parseKeymap fn = do
    res <- parseFromFile keymapParser fn
    case res of
        Left pe -> error $"Error reading keymap file " ++ show fn ++ " " ++ show pe
        Right r -> return r  

keymapParser :: GenParser Char () (ActionString, (Maybe (Either KeyString (KeyString,KeyString)), 
                                        Maybe String)
keymapParser = do
    whiteSpace
    ls <- many lineparser 
    eof
    return (Map.fromList ls)

lineparser :: GenParser Char () (ActionString, (Maybe Either KeyString (KeyString,KeyString)))   
complexkey = do  
    mb1 <- option (do 
        keyDescr <- identifier
            mb2 <- option (do
            symbol "/"
            key <- identifier
            return (Just key) Nothing)
        return (Just (keyDescr,mb2)) Nothing)
    symbol "->"
    action <- identifier
    return (case mb1 of
        Nothing -> Nothing
        Just keyDescr -> 
            case mb2 of 
                Just keyDescr2 -> Just (action,(Right (keyDescr,keyDescr2)))
                Nothing -> Just (action,(Left keyDescr))
    <?> "lineparser"

