--
-- | Module for handling keymaps,
-- | deals with gtk accelerators and double (emacs-like) keystrokes
-- 

module Ghf.GUI.Keymap (
    parseKeymap
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
import Data.Char(toLower)
import Control.Monad(foldM)
import Control.Monad.Reader

import Ghf.Core
import Ghf.GUI.ViewFrame
import Ghf.Editor.SourceEditor

type Keymap = Map ActionString [(Maybe (Either KeyString (KeyString,KeyString)), Maybe String)]

--
-- | Loads and parses a keymap file
-- 

parseKeymap :: FileName -> IO Keymap
parseKeymap fn = do
    res <- parseFromFile keymapParser fn
    case res of
        Left pe -> error $"Error reading keymap file " ++ show fn ++ " " ++ show pe
        Right r -> return r  

--
-- | Sets the accelerators is the action descriptions from the keymap
-- 
setKeymap :: [ActionDescr] -> Keymap -> [ActionDescr]
setKeymap actions keymap = map setAccel actions
    where setAccel act = case Map.lookup (name act) keymap of
                            Nothing -> act
                            Just [] -> act
                            Just keyList -> foldl setAccelerator act keyList
          setAccelerator act (Just (Left acc),Nothing)      = act{accelerator= acc : accelerator act}
          setAccelerator act (Just (Left acc),Just expl)    = act{accelerator= acc : accelerator act, 
                                                                tooltip= Just expl}
          setAccelerator act (_, Just expl)                 = act{tooltip= Just expl} 
          setAccelerator act (_,_)                          = act   
 
--
-- | Builds a special keymap for handling double keystroke accelerators 
--   Unfortunately in the IO Monad because of keyvalFromName
-- 
buildSpecialKeys :: Keymap -> [ActionDescr] -> IO (SpecialKeyTable)
buildSpecialKeys keymap actions = do
    pseudoTriples <- mapM build actions
    let map1 = Map.fromListWith (++) $concat pseudoTriples
    return (Map.map Map.fromList map1)    
    where
    build :: ActionDescr -> IO [((KeyVal,[Modifier]),[((KeyVal,[Modifier]),ActionDescr)])]
    build act = 
        case Map.lookup (name act) keymap of
            Nothing             ->  return []  
            Just l              ->  foldM (build' act) [] l
    build' act list (Just (Right (a1,a2)),_) 
                                =   do  a1p <- accParse a1
                                        a2p <- accParse a2
                                        return ((a1p,[(a2p,act)]): list)
    build' act list _           =   return list   

--
-- | Callback function for onKeyPress of the main window, so preprocess any key
-- 

handleSpecialKeystrokes :: Event -> GhfM Bool
handleSpecialKeystrokes (Key _ _ _ mods _ _ _ keyVal name char) = 
    case char of 
        Nothing -> return False
        Just c -> do
            bs <- getCandyState
            if bs
                then editKeystrokeCandy c
                else return ()
            sk  <- readGhf specialKey    
            sks <- readGhf specialKeys 
            sb <- getSpecialKeys   
            case sk of
                Nothing -> do
                    case Map.lookup (keyVal,sort mods) sks of
                        Nothing -> do 
                            lift $statusbarPop sb 1
                            lift $statusbarPush sb 1 ""
                            return False
                        Just map -> do
                            sb <- getSpecialKeys
                            let sym = printMods mods ++ name
                            lift $statusbarPop sb 1
                            lift $statusbarPush sb 1 sym
                            modifyGhf_ (\ghf -> return (ghf{specialKey = Just (map,sym)}))
                            return True
                Just (map,sym) -> do
                    case Map.lookup (keyVal,sort mods) map of
                        Nothing -> do
                            sb <- getSpecialKeys
                            lift $statusbarPop sb 1
                            lift $statusbarPush sb 1 $sym ++ printMods mods ++ name ++ "?"
                            return ()                    
                        Just (AD actname _ _ _ ghfAction _ _) -> do
                            sb <- getSpecialKeys
                            lift $statusbarPop sb 1
                            lift $statusbarPush sb 1 
                                $sym ++ " " ++ printMods mods ++ name ++ "=" ++ actname
                            ghfAction
                    modifyGhf_ (\ghf -> return (ghf{specialKey = Nothing}))
                    return True

-- ---------------------------------------------------------------------
-- Parsing
--

keymapStyle :: P.LanguageDef st
keymapStyle= emptyDef                      
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                , P.identStart     = alphaNum <|> oneOf "<>_"
                , P.identLetter    = alphaNum <|> oneOf "<>_"
                }      
lexer = P.makeTokenParser keymapStyle
lexeme = P.lexeme lexer
identifier = P.identifier lexer
symbol =  P.symbol lexer
whiteSpace = P.whiteSpace lexer
stringLiteral = P.stringLiteral lexer

keymapParser :: CharParser () Keymap
keymapParser = do
    whiteSpace
    ls <- many lineparser 
    eof
    return (Map.fromListWith (++) ls)

lineparser :: CharParser () (ActionString, [(Maybe (Either KeyString
                                (KeyString,KeyString)), Maybe String)])
lineparser = do  
    mb1 <- option Nothing (do 
        keyDescr <- identifier
        mb2 <- option Nothing (do
            symbol "/"
            key <- identifier
            return (Just key)) 
        return (Just (keyDescr, mb2))) 
    symbol "->"
    action <- identifier
    mbs <- option Nothing (do
        str <- stringLiteral
        return (Just str)) 
    return (case mb1 of
        Nothing -> (action,[(Nothing,mbs)])
        Just (keyDescr,mb2) -> 
            case mb2 of 
                Just keyDescr2 -> (action,[(Just (Right (keyDescr,keyDescr2)),mbs)])
                Nothing -> (action,[(Just (Left keyDescr),mbs)]))
    <?> "lineparser"

--------------------------------------------------
-- Have to write this until gtk_accelerator_parse gets bound in gtk2hs
--
accParse :: String -> IO (KeyVal,[Modifier])
accParse str = case parse accparser "accelerator" str of
    Right (ks,mods) -> do  
        key <- keyvalFromName (map toLower ks)
        trace (show (key,mods)) return ()
        return (key,sort mods)
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
    try $symbol2 "<ctrl>" 
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

-- ---------------------------------------------------------------------
-- Printing
--

printMods :: [Modifier] -> String
printMods []    = ""
printMods (m:r) = show m ++ printMods r

