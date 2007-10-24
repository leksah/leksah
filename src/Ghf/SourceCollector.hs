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
    let fCabalFiles =   concat cabalFiles
    packages        <-  mapM (\fp -> parseCabal fp) fCabalFiles
    let pdToFiles   =   Map.fromListWith (++) (zip packages (map (\a -> [a]) fCabalFiles))
    filePath        <-  getConfigFilePathForSave "source_packages.txt"
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

cabalStyle  :: P.LanguageDef st
cabalStyle  = emptyDef

lexer       =   P.makeTokenParser cabalStyle
lexeme      =   P.lexeme lexer
whiteSpace  =   P.whiteSpace lexer
hexadecimal =   P.hexadecimal lexer
symbol      =   P.symbol lexer

parseCabal :: FileName -> IO String
parseCabal fn = do
    res     <-  parseFromFile cabalMinimalParser fn
    case res of
        Left pe ->  error $"Error reading cabal file " ++ show fn ++ " " ++ show pe
        Right r ->  do
            putStrLn r
            return r


cabalMinimalParser :: CharParser () String
cabalMinimalParser = do
    r1 <- cabalMinimalP
    r2 <- cabalMinimalP
    case r1 of
        Left v -> do
            case r2 of
                Right n -> return (n ++ "-" ++ v)
                Left v -> error "Illegal cabal"
        Right n -> do
            case r2 of
                Left v -> return (n ++ "-" ++ v)
                Right n -> error "Illegal cabal"


cabalMinimalP :: CharParser () (Either String String)
cabalMinimalP =
    do  try $(symbol "name:" <|> symbol "Name:")
        whiteSpace
        name       <-  (many $noneOf " \n")
        (many $noneOf "\n")
        char '\n'
        return (Right name)
    <|> do
            try $(symbol "version:" <|> symbol "Version:")
            whiteSpace
            version    <-  (many $noneOf " \n")
            (many $noneOf "\n")
            char '\n'
            return (Left version)
    <|> do
            many $noneOf "\n"
            char '\n'
            cabalMinimalP
    <?> "cabal minimal"
