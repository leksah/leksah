module Ghf.Preferences (
    readPrefs
--,   writePrefs
--,   applyPrefs
--, editPrefs
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import Text.PrettyPrint.HughesPJ hiding (char)
import Control.Monad(foldM)

import Ghf.Core


{-- Part of Core
data Prefs = Prefs {
        showLineNumbers     ::  Bool
    ,   rightMargin         ::  Maybe Int
    ,   tabWidth            ::  Int
    ,   sourceCandy         ::  Maybe String
    ,   keymapName          ::  String 
    ,   defaultSize         ::  (Int,Int)
} deriving(Eq,Ord,Show)
--}

defaultPrefs = Prefs {
        showLineNumbers     =   True
    ,   rightMargin         =   Just 100
    ,   tabWidth            =   4
    ,   sourceCandy         =  Just("Default")
    ,   keymapName          =  "Default" 
    ,   defaultSize         =  (1024,800)}

data FieldDescription alpha = FD {
        name                ::  String
    ,   comment             ::  String
    ,   fieldPrinter        ::  alpha -> String 
    ,   fieldParser         ::  CharParser () (alpha  -> alpha)
}

field ::  String -> 
                String -> 
                (alpha -> String) -> 
                (CharParser () alpha) ->
                ((FieldDescription beta) -> alpha -> (FieldDescription beta) )
                (alpha -> FieldDescription beta ) 
                FieldDescription beta     
field name comment printer parser getter setter =
    FD  name 
        comment
        (printer . getter)
        (\a -> do
            try (symbol name)
            colon 
            value <- parser   
            return (setter a value)) 

prefsDescription :: [FieldDescription Prefs] 
prefsDescription = [
        field 
            "Show line numbers" "(True/False)" 
            (text . show) boolParser
            showLineNumbers (\ a b -> (a{showLineNumbers = b}))
    ,   field "Right margin" "Size or 0 for no right margin"
            (\a -> (text . show) (case a of Nothing -> 0; Just i -> i)) 
            (do i <- integer
                return (if i == 0 then Nothing else Just (fromIntegral i)))
            rightMargin (\a b -> a{rightMargin = b}) 
    ,   field "Tab width" ""
            (text . show) integer
            tabWidth (\a b -> a{tabWidth = b}) 
    ,   field "Source candy" "Empty for do not use or the name of a candy file in a config dir)" 
            (\a -> text (case a of Nothing -> ""; Just s -> s)) 
            (do id <- identifier
                return (if null id then Nothing else Just (id)))
            sourceCandy (\a b -> a{sourceCandy = b}) 
    ,   field "Name of the keymap"  "The name of a keymap file in a config dir" 
            text identifier
            keymapName (\a b -> a{keymapName = b}) 
    ,   field "Window default size"
            "Default size of the main ghf window specified as pair (int,int)" 
            (text.show) 
            (do char "("
                i1 <- integer
                char ","
                i2 <- integer
                char ")"
                return (i1,i2))
            defaultSize (\a b -> a{defaultSize = b})] 

readPrefs :: FileName -> IO Prefs
readPrefs fn = do
    res <- parseFromFile prefsParser defaultPrefs fn
    case res of
        Left pe -> error $"Error reading prefs file " ++ show fn ++ " " ++ show pe
        Right r -> return r  

prefsStyle  :: P.LanguageDef st
prefsStyle  = emptyDef                      
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                }      

lexer = P.makeTokenParser prefsStyle
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
hexadecimal = P.hexadecimal lexer
symbol = P.symbol lexer
identifier = P.symbol identifier

prefsParser :: Prefs -> [FieldDescription Prefs] -> CharParser () Prefs
prefsParser def descriptions = 
    let parsers = map (\ a -> try a) fieldParser descriptions in do
        whiteSpace
        ls <- (many parsers <?> "pref parser")
        eof
        foldM (\pref f -> f pref) def ls
        <?> "prefs parser" 

boolParser :: CharParser () Bool
boolParser = do
    (symbol "True" <|> symbol "true")
    return True
    <|> do
    (symbol "False"<|> symbol "false")
    return False
    <?> "bool parser"
-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

testEditorPrefs :: String
testEditorPrefs = unlines [
        "-- Required",
        "showLineNumbers: True",
        "rightMargin: 80",
        "tabWidth: 4",
        "sourceCandy: True",
        "keymapName: Default"]