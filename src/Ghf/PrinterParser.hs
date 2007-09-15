--
-- | Module for saving and restoring preferences
--

module Ghf.PrinterParser (

    Printer
,   Parser
--,   FieldDescriptionPP
,   applyFieldParsers
,   boolParser
,   intParser
,   pairParser
,   identifier
,   emptyParser
,   whiteSpace
,   stringParser

,   emptyPrinter
,   symbol
,   colon


) where

import Graphics.UI.Gtk()    -- Instances only
import Text.ParserCombinators.Parsec.Language(emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as P (P.TokenParser(P.integer, P.colon,
							 P.symbol, P.whiteSpace, P.identifier, P.lexeme, P.hexadecimal),
					   P.LanguageDef(P.commentLine, P.commentEnd, P.commentStart),
					   P.makeTokenParser)
import Text.ParserCombinators.Parsec(CharParser, noneOf, char, eof, choice,
				     many, pzero, (<|>), (<?>))
import Data.IORef()    -- Instances only
import Data.List()    -- Instances only
import Data.Maybe()    -- Instances only
import Data.Map()    -- Instances only
import Debug.Trace()    -- Instances only
import System.Directory()    -- Instances only
import Text.ParserCombinators.ReadP()    -- Instances only
import qualified Text.PrettyPrint.HughesPJ as PP(PP.Doc, PP.empty)
import Ghf.Core()    -- Instances only


type Printer beta       =   beta -> PP.Doc
type Parser beta        =   CharParser () beta

-- ------------------------------------------------------------
-- * Parsing with Parsec
-- ------------------------------------------------------------

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
