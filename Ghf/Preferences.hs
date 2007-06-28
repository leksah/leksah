--
-- | Module for saving, restoring and editing preferences
-- 


module Ghf.Preferences (
    readPrefs
,   writePrefs
--,   applyPrefs
--, editPrefs
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import qualified Text.PrettyPrint.HughesPJ as PP
import Control.Monad(foldM)

import Ghf.Core


defaultPrefs = Prefs {
        showLineNumbers     =   True
    ,   rightMargin         =   Just 100
    ,   tabWidth            =   4
    ,   sourceCandy         =  Just("Default")
    ,   keymapName          =  "Default" 
    ,   defaultSize         =  (1024,800)}

data FieldDescription alpha =  FD {
        name                ::  String
    ,   comment             ::  String
    ,   fieldPrinter        ::  alpha -> PP.Doc
    ,   fieldParser         ::  alpha -> CharParser () (alpha)
}

field ::      String ->                   --name
              String ->                   --comment
              (beta -> PP.Doc) ->         --printer
              (CharParser () beta ) ->     --parser
              (alpha -> beta ) ->  --getter
              (beta -> alpha -> alpha ) -> --setter
              FieldDescription alpha 
field name comment printer parser getter setter =
    FD  name 
        comment
        (\ a -> (PP.text name PP.<> PP.colon) 
                PP.$$ (PP.nest 15 (printer (getter a)))        
                PP.$$ (PP.nest 5 (if null comment 
                                        then PP.empty 
                                        else PP.text $"--" ++ comment)))
        (\ a -> try (do
            symbol name
            colon
            value <- parser   
            return (setter value a)))

prefsDescription :: [FieldDescription Prefs] 
prefsDescription = [
        field "Show line numbers"
            "(True/False)" 
            (PP.text . show)
            boolParser
            showLineNumbers
            (\ b a -> (a{showLineNumbers = b}))
    ,   field "Right margin"
            "Size or 0 for no right margin"
            (\a -> (PP.text . show) (case a of Nothing -> 0; Just i -> i))
            (do i <- integer
                return (if i == 0 then Nothing else Just (fromIntegral i)))
            rightMargin
            (\b a -> a{rightMargin = b})
    ,   field "Tab width" ""
            (PP.text . show) integer
            (fromIntegral . tabWidth) (\b a -> a{tabWidth = (fromIntegral b)})
    ,   field "Source candy" "Empty for do not use or the name of a candy file in a config dir)" 
            (\a -> PP.text (case a of Nothing -> ""; Just s -> s)) 
            (do id <- identifier
                return (if null id then Nothing else Just (id)))
            sourceCandy (\b a -> a{sourceCandy = b})
    ,   field "Name of the keymap"  "The name of a keymap file in a config dir" 
            PP.text identifier
            keymapName (\b a -> a{keymapName = b})
    ,   field "Window default size"
            "Default size of the main ghf window specified as pair (int,int)" 
            (PP.text.show) 
            (do char '('
                i1 <- integer
                char ','
                i2 <- integer
                char ')'
                return ((fromIntegral i1),(fromIntegral i2)))
            defaultSize (\(c,d) a -> a{defaultSize = (fromIntegral c,fromIntegral d)})]


-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readPrefs :: FileName -> IO Prefs
readPrefs fn = do
    res <- parseFromFile (prefsParser defaultPrefs prefsDescription) fn
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
identifier = P.identifier lexer
colon = P.colon lexer
integer = P.integer lexer

prefsParser :: Prefs -> [FieldDescription Prefs] -> CharParser () Prefs
prefsParser def descriptions = 
    let parsersF = map fieldParser descriptions in do     
        whiteSpace
        res <- applyFieldParsers def parsersF
        return res
        <?> "prefs parser" 

applyFieldParsers :: Prefs -> [Prefs -> CharParser () (Prefs)] -> CharParser () Prefs
applyFieldParsers prefs parseF = do
    let parsers = map (\a -> a prefs) parseF
    newprefs <- choice parsers
    whiteSpace
    applyFieldParsers newprefs parseF
    <|> do
    eof
    return (prefs)
    <?> "field parser"

boolParser :: CharParser () Bool
boolParser = do
    (symbol "True" <|> symbol "true")
    return True
    <|> do
    (symbol "False"<|> symbol "false")
    return False
    <?> "bool parser"

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

writePrefs :: FilePath -> Prefs -> IO ()
writePrefs fpath prefs = writeFile fpath (showPrefs prefs)

showPrefs :: Prefs -> String
showPrefs prefs = PP.render $
    foldl (\ doc (FD _ _ printer _) -> doc PP.$+$ printer prefs) PP.empty prefsDescription 


