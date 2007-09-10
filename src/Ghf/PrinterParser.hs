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

import Text.ParserCombinators.Parsec hiding (Parser)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import qualified Text.PrettyPrint.HughesPJ as PP

import Graphics.UI.Gtk hiding (afterToggleOverwrite,Focus,onChanged)
import qualified Data.Map as Map
import Data.Map(Map,(!))
import Data.IORef
import Data.Maybe(isJust)
import System.Directory
import Data.List(unzip4,elemIndex)
import Text.ParserCombinators.ReadP(readP_to_S)
import Debug.Trace

import Ghf.Core

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
