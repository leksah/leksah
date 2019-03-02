{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.SourceCandy
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- |
--
---------------------------------------------------------------------------------

module IDE.SourceCandy (
    parseCandy          -- ::   FilePath -> IO alpha
  , notBeforeId
  , notBeforeOp
  , notAfterId
  , notAfterOp
) where

import Prelude ()
import Prelude.Compat hiding(getChar, getLine)

import Data.Char(chr)
import Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import Data.Set (Set)
import qualified Data.Set as Set

import IDE.Core.State
import Data.Text (Text)
import qualified Data.Text as T
       (pack, singleton, length, replicate, head)
import Data.Functor.Identity (Identity(..))
import Text.Parsec (ParsecT)

---------------------------------------------------------------------------------
-- * Implementation

notBeforeId, notAfterId, notBeforeOp, notAfterOp :: Set Char
notBeforeId     =   Set.fromList $['a'..'z'] ++ ['A'..'Z'] ++ "_"
notAfterId      =   Set.fromList $['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
notBeforeOp     =   Set.fromList "!#$%&*+./<=>?@\\^|-~'\""
notAfterOp      =   notBeforeOp

type CandyTableI = [(Text,Char,Bool)]

forthFromTable :: CandyTableI -> CandyTableForth
forthFromTable = map forthFrom
    where
    forthFrom (str,chr',noTrimming) =
        let isOp = not (Set.member (T.head str) notBeforeId)
            from = str
            trailingBlanks = T.replicate (if noTrimming then 0 else T.length str - 1) (T.singleton ' ')
            to = T.singleton chr' <> trailingBlanks
        in (isOp,from,to)

backFromTable :: CandyTableI -> CandyTableBack
backFromTable = map backFrom
    where
    backFrom (str,chr',noTrimming) =
        let numTrailingBlanks = if noTrimming then 0 else T.length str - 1
        in (str,T.singleton chr',numTrailingBlanks)

---Candy Parser

candyStyle  :: P.LanguageDef st
candyStyle  = emptyDef
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser candyStyle
lexeme :: ParsecT String u Identity a -> ParsecT String u Identity a
lexeme = P.lexeme lexer
whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer
hexadecimal :: ParsecT String u Identity Integer
hexadecimal = P.hexadecimal lexer
symbol :: String -> ParsecT String u Identity String
symbol = P.symbol lexer

parseCandy :: FilePath -> IO CandyTable
parseCandy fn = do
    res     <-  parseFromFile candyParser fn
    case res of
        Left pe ->  throwIDE $ "Error reading candy file " <> T.pack (show fn) <> " " <> T.pack (show pe)
        Right r ->  return (CT(forthFromTable r, backFromTable r))

candyParser :: CharParser () CandyTableI
candyParser = do
    whiteSpace
    ls  <-  P.many oneCandyParser
    eof
    return ls

oneCandyParser :: CharParser () (Text,Char,Bool)
oneCandyParser = do
    toReplace   <-  toReplaceParser
    replaceWith <-  replaceWithParser
    nt          <-  option True (try $do
        _ <- symbol "Trimming"
        return False)
    return (toReplace,replaceWith,nt)

toReplaceParser :: CharParser () Text
toReplaceParser   = lexeme (do
    str         <-  between (char '"')
                        (char '"' <?> "end of string")
                        (P.many $noneOf "\"")
    return $ T.pack str)
    <?> "to replace string"

replaceWithParser :: CharParser () Char
replaceWithParser = do
    _ <- char '0'
    hd  <-  lexeme hexadecimal
    return (chr (fromIntegral hd))


