--
-- | Module for saving and restoring preferences
-- 

module Ghf.PrinterParser (

    Printer
,   Parser
,   Getter
,   Setter
,   FieldDescriptionPP

,   prefsParser
,   boolParser
,   intParser
,   pairParser
,   identifier
,   emptyParser

,   showPrefs
,   emptyPrinter
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

type Getter alpha beta      =   alpha -> beta
type Setter alpha beta      =   beta -> alpha -> alpha

type MkFieldDescriptionPP alpha beta =
              String ->                         --name
              Maybe String ->                   --synopsis
              (Printer beta) ->                 
              (Parser beta) ->
              (Getter alpha beta) ->            
              (Setter alpha beta) ->            
              FieldDescriptionPP alpha

data FieldDescriptionPP alpha =  FDPP {
        name                ::  String
    ,   synopsis            ::  Maybe String
    ,   fieldPrinter        ::  alpha -> PP.Doc
    ,   fieldParser         ::  alpha -> CharParser () alpha
    }


mkFieldDescriptionPP :: Eq beta => MkFieldDescriptionPP alpha beta 
mkFieldDescriptionPP name synopsis printer parser getter setter =
    FDPP  
        name 
        synopsis
        (\ dat -> (PP.text name PP.<> PP.colon)
                PP.$$ (PP.nest 15 (printer (getter dat)))
                PP.$$ (PP.nest 5 (case synopsis of 
                                    Nothing -> PP.empty 
                                    Just str -> PP.text $"--" ++ str)))
        (\ dat -> try (do
            symbol name
            colon
            val <- parser
            return (setter val dat)))
        
-- ------------------------------------------------------------
-- * Parsing with Parsec
-- ------------------------------------------------------------

prefsParser ::  a ->  [FieldDescriptionPP a] ->  CharParser () a
prefsParser def descriptions =
    let parsersF = map fieldParser descriptions in do
        whiteSpace
        res <-  applyFieldParsers def parsersF
        return res
        <?> "prefs parser"

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

showPrefs ::  a ->  [FieldDescriptionPP a] ->  String
showPrefs prefs prefsDesc = PP.render $
    foldl (\ doc (FDPP _ _ printer _ ) ->  doc PP.$+$ printer prefs) PP.empty prefsDesc

emptyPrinter ::  () ->  PP.Doc
emptyPrinter _ = PP.empty
