--
-- | Module for saving and restoring preferences
--

module Ghf.PrinterParser (

    Printer
,   Parser
,   FieldDescriptionS(..)
,   MkFieldDescriptionS
,   mkFieldS

,   applyFieldParsers
,   boolParser
,   intParser
,   lineParser
,   pairParser
,   identifier
,   emptyParser
,   whiteSpace
,   stringParser
,   readParser

,   emptyPrinter
,   symbol
,   colon


) where

import Graphics.UI.Gtk()    -- Instances only
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec hiding(Parser)
import qualified Text.PrettyPrint.HughesPJ as PP

import GUI.Ghf.Parameters
import GUI.Ghf.EditorBasics


type Printer beta       =   beta -> PP.Doc
type Parser beta        =   CharParser () beta

-- ------------------------------------------------------------
-- * Parsing with Parsec
-- ------------------------------------------------------------

data FieldDescriptionS alpha =  FDS {
        parameters      ::  Parameters
    ,   fieldPrinter    ::  alpha -> PP.Doc
    ,   fieldParser     ::  alpha -> CharParser () alpha
    }

type MkFieldDescriptionS alpha beta =
    Parameters ->
    (Printer beta) ->
    (Parser beta) ->
    (Getter alpha beta) ->
    (Setter alpha beta) ->
    FieldDescriptionS alpha

mkFieldS :: Eq beta => MkFieldDescriptionS alpha beta
mkFieldS parameters printer parser getter setter =
    FDS parameters
        (\ dat -> (PP.text (case getParameterPrim paraName parameters of
                                    Nothing -> ""
                                    Just str -> str) PP.<> PP.colon)
                PP.$$ (PP.nest 15 (printer (getter dat)))
                PP.$$ (PP.nest 5 (case getParameterPrim paraSynopsis parameters of
                                    Nothing -> PP.empty
                                    Just str -> PP.text $"--" ++ str)))
        (\ dat -> try (do
            symbol (case getParameterPrim paraName parameters of
                                    Nothing -> ""
                                    Just str -> str)
            colon
            val <- parser
            return (setter val dat)))

applyFieldParsers ::  a ->  [a ->  CharParser () a] ->  CharParser () a
applyFieldParsers prefs parseF = do
    let parsers = map (\a ->  a prefs) parseF
    newprefs <-  choice parsers
    whiteSpace
    applyFieldParsers newprefs parseF
    <|> do
    eof
    return (prefs)
    <?> "field parser"

boolParser ::  CharParser () Bool
boolParser = do
    (symbol "True" <|> symbol "true")
    return True
    <|> do
    (symbol "False"<|> symbol "false")
    return False
    <?> "bool parser"

readParser ::  Read a =>  CharParser () a
readParser = do
    str <- many (noneOf ['\n'])
    return (read str)
    <?> "bool parser"

pairParser ::  CharParser () alpha ->  CharParser () (alpha,alpha)
pairParser p2 = do
    char '('
    v1 <-  p2
    char ','
    v2 <-  p2
    char ')'
    return (v1,v2)
    <?> "pair parser"

stringParser ::  CharParser () String
stringParser = do
    char '"'
    str <- many (noneOf ['"'])
    char '"'
    return (str)
    <?> "string parser"

lineParser ::  CharParser () String
lineParser = do
    str <- many (noneOf ['\n'])
    return (str)
    <?> "line parser"


intParser ::  CharParser () Int
intParser = do
    i <-  integer
    return (fromIntegral i)

emptyParser ::  CharParser () ()
emptyParser = pzero

prefsStyle  ::  P.LanguageDef st
prefsStyle  = emptyDef  {
        P.commentStart   = "{-"
    ,   P.commentEnd     = "-}"
    ,   P.commentLine    = "--"
    }

lexer = P.makeTokenParser prefsStyle
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
hexadecimal = P.hexadecimal lexer
symbol = P.symbol lexer
identifier = P.identifier lexer
colon = P.colon lexer
integer = P.integer lexer

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------


emptyPrinter ::  () ->  PP.Doc
emptyPrinter _ = PP.empty
