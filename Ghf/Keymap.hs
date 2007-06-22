module Ghf.Keymap (
    parseKeymap
,   ActionString
,   KeyString
,   Keymap

,   ActionDescr(..)
,   setKeymap
,   buildSpecialKeys
,   handleSpecialKeystrokes


) where

import Graphics.UI.Gtk
import qualified Data.Map as Map
import Data.Map (Map,(!))
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import Data.Maybe
import Debug.Trace
import Data.List(sort)


import Ghf.Core

data ActionDescr = AD {
                name :: ActionString
            ,   label :: String
            ,   tooltip ::Maybe String
            ,   stockID :: Maybe String
            ,   action :: GhfAction
            ,   accelerator :: Maybe KeyString
            ,   isToggle :: Bool}

type ActionString = String
type KeyString = String
type Keymap = Map ActionString (Maybe (Either KeyString (KeyString,KeyString)), Maybe String)

setKeymap :: [ActionDescr] -> Keymap -> [ActionDescr]
setKeymap actions keymap = map setAccel actions
    where setAccel act = case Map.lookup (name act) keymap of
                            Nothing -> act
                            Just(mbkeys,mbexpl) -> 
                                case mbkeys of
                                    Nothing     -> act{tooltip=mbexpl}
                                    Just (Right (a1,a2)) -> act{tooltip=mbexpl}
                                    Just (Left acc) -> act{accelerator=Just acc, tooltip=mbexpl}
 
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

instance Show Modifier
    where show Shift    = "Shift"	
          show Control  = "Control"	
          show Alt      = "Alt"	
          show Apple    = "Apple"	
          show Compose  = "Compose"


--
-- | Unfortunately in the IO Monad because of keyvalFromName
-- 
buildSpecialKeys :: Keymap -> [ActionDescr] ->
                    IO (Map (KeyVal,[Modifier]) (Map (KeyVal,[Modifier]) GhfAction))
buildSpecialKeys keymap actions = do
    pseudoTriples <- mapM build actions
    let map1 = Map.fromListWith (++) (map fromJust $filter isJust pseudoTriples)
    return (Map.map Map.fromList map1)    
    where
    build :: ActionDescr -> IO (Maybe ((KeyVal,[Modifier]),[((KeyVal,[Modifier]),GhfAction)]))
    build act = 
        case Map.lookup (name act) keymap of
            Nothing -> return Nothing
            Just(mbkeys,mbexpl) -> 
                case mbkeys of
                    Nothing     -> return Nothing
                    Just (Right (a1,a2)) -> do
                        a1p <- accParse a1
                        a2p <- accParse a2
                        return (Just (a1p,[(a2p,action act)]))
                    Just (Left acc) -> return Nothing 

--
-- |Have to write this until gtk_accelerator_parse gets bound in gtk2hs                
--
accParse :: String -> IO (KeyVal,[Modifier])
accParse str = case parse accparser "accelerator" str of
    Right (ks,mods) -> do  
        key <- keyvalFromName ks
        trace (show (key,mods)) return ()
        return (key,mods)
    Left e -> error $show e     
                  
accStyle :: P.LanguageDef st
accStyle= emptyDef{P.caseSensitive = False}    
  
lexer2 = P.makeTokenParser accStyle
lexeme2 = P.lexeme lexer2
symbol2 =  P.symbol lexer2
identifier2 =  P.identifier lexer2
whiteSpace2 = P.whiteSpace lexer2

accparser :: GenParser Char () (String,[Modifier])
accparser = do
    whiteSpace2
    mods <- many modparser
    key <- identifier2 
    return (key,mods)    
    
modparser :: GenParser Char () Modifier
modparser = do
    try $symbol2 "<shift>"
    return Shift
    <|> do
    try $symbol2 "<control>" 
    return Control    
    <|> do
    try $symbol2 "<alt>"
    return Alt
    <|> do
    try $symbol2 "<apple>"
    return Apple 
    <|> do
    try $symbol2 "<compose>"
    return Compose
    <?>"modparser"

handleSpecialKeystrokes :: Event -> GhfM Bool
handleSpecialKeystrokes (Key _ _ _ mods _ _ _ keyVal _ _) = do
    sk  <- readGhf specialKey    
    sks <- readGhf specialKeys    
    trace "handle" (return ())
    case sk of
        Nothing -> do
            case Map.lookup (keyVal,mods) sks of
                Nothing -> return False
                Just map -> do
                    trace "go to special" (return ())
                    modifyGhf_ (\ghf -> return (ghf{specialKey = Just map}))
                    return True
        Just map -> do
            case Map.lookup (keyVal,mods) map of
                Nothing -> return ()                    
                Just act -> act
            modifyGhf_ (\ghf -> return (ghf{specialKey = Nothing}))
            return True
                      
        
    