-----------------------------------------------------------------------------
--
-- Module      :  Ghf.SourceCollector
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | This modulle collects information for packages with source available
--
-------------------------------------------------------------------------------

module Ghf.SourceCollector (
    buildSourceForPackageDB
) where

import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Reader
import Distribution.PackageDescription
import Distribution.Verbosity
import Distribution.Package
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import Data.Version

import Ghf.Core
import Ghf.File
import Ghf.Preferences

buildSourceForPackageDB :: IO ()
buildSourceForPackageDB = do
    prefsPath       <-  getConfigFilePathForLoad "Default.prefs"
    prefs           <-  readPrefs prefsPath
    let dirs        =   sourceDirectories prefs
    cabalFiles      <-  mapM allCabalFiles dirs
    packages        <-  mapM (\fp -> parseCabal fp) (concat cabalFiles)
    let pdToFiles   =   Map.fromListWith (++) (zip packages cabalFiles)
    filePath        <-  getConfigFilePathForSave "source_packages.ghfsp"
    writeFile filePath  (PP.render (showSourceForPackageDB pdToFiles))

showSourceForPackageDB  :: Map String [FilePath] -> PP.Doc
showSourceForPackageDB aMap = PP.vcat (map showIt (Map.toList aMap))
    where
    showIt :: (String,[FilePath]) -> PP.Doc
    showIt (pd,list) =  (foldl (\l n -> l PP.$$ (PP.text $ show n)) label list)
                             PP.<>  PP.char '\n'
        where label  =  PP.text pd PP.<> PP.colon

--parseSourceForPackageDB :: String -> Maybe (Map PackageDescription [FilePath])


--sourceForPackage :: PackageIdentifier -> Maybe Filepath
--sourceForPackage =

---Cabal PackageIdentifier parser

candyStyle  :: P.LanguageDef st
candyStyle  = emptyDef
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                }

lexer       =   P.makeTokenParser candyStyle
lexeme      =   P.lexeme lexer
whiteSpace  =   P.whiteSpace lexer
hexadecimal =   P.hexadecimal lexer
symbol      =   P.symbol lexer

parseCabal :: FileName -> IO String
parseCabal fn = do
    res     <-  parseFromFile cabalMinimalParser fn
    case res of
        Left pe ->  error $"Error reading cabal file " ++ show fn ++ " " ++ show pe
        Right r ->  return r

cabalMinimalParser :: CharParser () String
cabalMinimalParser = do
    whiteSpace
    (symbol "name:" <|> symbol "Name:")
    name       <-  (many $noneOf " \n")
    char '\n'
    (symbol "version:" <|> symbol "Version:")
    version    <-  (many $noneOf " \n")
    return (name ++ "-" ++ version)
    <|> do
        (many $noneOf "\n")
        char '\n'
        cabalMinimalParser
