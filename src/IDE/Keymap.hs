--
-- | Module for handling keymaps,
-- | deals with gtk accelerators and double (emacs-like) keystrokes
--

module IDE.Keymap (
    Keymap(..)
) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events(Modifier(..))
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import Data.List (foldl',sort)
import Data.Char(toLower)
import Control.Monad.Reader

import IDE.Core.State

class Keymap alpha where
    parseKeymap         ::   FilePath -> IO alpha
    setKeymap           ::   alpha -> [ActionDescr IDERef] -> [ActionDescr IDERef]
    buildSpecialKeys    ::   alpha -> [ActionDescr IDERef] -> IO (SpecialKeyTable IDERef)


instance Keymap KeymapI where
    parseKeymap         =   parseKeymap'
    setKeymap           =   setKeymap'
    buildSpecialKeys    =   buildSpecialKeys'

--
-- | Loads and parses a keymap file
--

parseKeymap' :: FilePath -> IO KeymapI
parseKeymap' fn = do
    res <- parseFromFile keymapParser fn
    case res of
        Left pe -> throwIDE $"Error reading keymap file " ++ show fn ++ " " ++ show pe
        Right r -> return r

--
-- | Sets the accelerators is the action descriptions from the keymap
--
setKeymap' :: KeymapI -> [ActionDescr IDERef] -> [ActionDescr IDERef]
setKeymap' (KM keymap) actions  = map setAccel actions
    where setAccel act = case Map.lookup (name act) keymap of
                            Nothing -> act
                            Just [] -> act
                            Just keyList -> foldl' setAccelerator act keyList
          setAccelerator act (Just (Left acc),Nothing)      = act{accelerator= acc : accelerator act}
          setAccelerator act (Just (Left acc),Just expl)    = act{accelerator= acc : accelerator act,
                                                                tooltip= Just expl}
          setAccelerator act (_, Just expl)                 = act{tooltip= Just expl}
          setAccelerator act (_,_)                          = act

--
-- | Builds a special keymap for handling double keystroke accelerators
-- Unfortunately in the IO Monad because of keyvalFromName
--
buildSpecialKeys' :: KeymapI -> [ActionDescr IDERef] -> IO (SpecialKeyTable IDERef)
buildSpecialKeys' (KM keymap) actions = do
    pseudoTriples <- mapM build actions
    let map1 = Map.fromListWith (++) $concat pseudoTriples
    return (Map.map Map.fromList map1)
    where
    build :: ActionDescr IDERef -> IO [((KeyVal,[Modifier]),[((KeyVal,[Modifier]),
                (ActionDescr IDERef))])]
    build act =
        case Map.lookup (name act) keymap of
            Nothing             ->  return []
            Just l              ->  foldM (build' act) [] l
    build' act list (Just (Right (a1,a2)),_)
                                =   do  a1p <- accParse a1
                                        a2p <- accParse a2
                                        return ((a1p,[(a2p,act)]): list)
    build' act list _           =   return list


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

keymapParser :: CharParser () KeymapI
keymapParser = do
    whiteSpace
    ls <- many lineparser
    eof
    return (KM (Map.fromListWith (++) ls))

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
        return (key,sort mods)
    Left e -> throwIDE $show e

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
    return Super
    <|> do
    try $symbol2 "<compose>"
    return Hyper
    <?>"modparser"

