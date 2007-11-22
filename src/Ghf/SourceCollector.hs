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
    collectSources
,   buildSourceForPackageDB
,   sourceForPackage
,   parseSourceForPackageDB
,   getSourcesMap
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
import System.Directory
import Data.Maybe(catMaybes)
import Distribution.Simple.Utils(breaks)
import System.FilePath(takeBaseName,dropFileName)

import Ghf.Core.State
import Ghf.File
import Ghf.Preferences
import Ghf.Extractor

getSourcesMap :: IO (Map PackageIdentifier [FilePath])
getSourcesMap = do
        mbSources <- parseSourceForPackageDB
        case mbSources of
            Just map -> do
                --putStrLn $ "sourceDB: " ++ show map
                return map
            Nothing -> do
                buildSourceForPackageDB
                mbSources <- parseSourceForPackageDB
                case mbSources of
                    Just map -> do
                        --putStrLn $ "sourceDB: " ++ show map
                        return map
                    Nothing ->  error "can't build/open source for package file"

collectSources :: Map PackageIdentifier [FilePath] -> PackageDescr -> IO PackageDescr
collectSources sourceMap pdescr =
    case sourceForPackage (packagePD pdescr) sourceMap of
        Nothing -> return pdescr
        Just fp -> do
            let path = dropFileName fp
            sf          <- allHaskellSourceFiles path
            newModDescr <- mapM (collectSourcesForModules path sf) (exposedModulesPD pdescr)
            return (pdescr{mbSourcePathPD = Just fp,exposedModulesPD = newModDescr})

collectSourcesForModules :: FilePath -> [FilePath] -> ModuleDescr -> IO (ModuleDescr)
collectSourcesForModules filePath sourceFiles moduleDescr = do
    mbFile <-   findSourceForModule (modu $ moduleIdMD moduleDescr) filePath sourceFiles
    return (moduleDescr{mbSourcePathMD = mbFile})

findSourceForModule :: ModuleIdentifier -> FilePath -> [FilePath] -> IO (Maybe FilePath)
findSourceForModule mod filePath sourceFiles = do
    let moduleBaseName      =   last (breaks (== '.') mod)
    let filesWithRightName  =   filter (\fp -> moduleBaseName == takeBaseName fp) sourceFiles
    possibleFiles           <-  filterM (\ fn -> do
                                    mn <- moduleNameFromFilePath fn
                                    case mn of
                                        Nothing -> return False
                                        Just m  -> return (m == mod))
                                    filesWithRightName
    if null possibleFiles
        then return Nothing
        else return (Just $ head possibleFiles)

buildSourceForPackageDB :: IO ()
buildSourceForPackageDB = do
    prefsPath       <-  getConfigFilePathForLoad "Default.prefs"
    prefs           <-  readPrefs prefsPath
    let dirs        =   sourceDirectories prefs
    cabalFiles      <-  mapM allCabalFiles dirs
    fCabalFiles     <-  mapM canonicalizePath $ concat cabalFiles
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


parseSourceForPackageDB :: IO (Maybe (Map PackageIdentifier [FilePath]))
parseSourceForPackageDB = do
    filePath        <-  getConfigFilePathForLoad "source_packages.txt"
    exists          <-  doesFileExist filePath
    if exists
        then do
            res             <-  parseFromFile sourceForPackageParser filePath
            case res of
                Left pe ->  do
                    putStrLn $"Error reading source packages file "
                            ++ filePath ++ " " ++ show pe
                    return Nothing
                Right r ->  return (Just r)
        else do
            putStrLn $"No source packages file found: " ++ filePath
            return Nothing

sourceForPackageParser :: CharParser () (Map PackageIdentifier [FilePath])
sourceForPackageParser = do
    whiteSpace
    ls  <-  many onePackageParser
    whiteSpace
    eof
    return (Map.fromList (catMaybes ls))
    <?> "sourceForPackageParser"

onePackageParser :: CharParser () (Maybe (PackageIdentifier,[FilePath]))
onePackageParser = do
    mbPd        <-  packageDescriptionParser
    filePaths   <-  many filePathParser
    case mbPd of
        Nothing -> return Nothing
        Just pd -> return (Just (pd,filePaths))
    <?> "onePackageParser"

packageDescriptionParser :: CharParser () (Maybe PackageIdentifier)
packageDescriptionParser = try (do
    whiteSpace
    str <- many (noneOf ":")
    char ':'
    return (toPackageIdentifier str))
    <?> "packageDescriptionParser"

filePathParser :: CharParser () FilePath
filePathParser = try (do
    whiteSpace
    char '"'
    str <- many (noneOf ['"'])
    char '"'
    return (str))
    <?> "filePathParser"


sourceForPackage :: PackageIdentifier
    -> (Map PackageIdentifier [FilePath])
    -> Maybe FilePath
sourceForPackage id map =
    case id `Map.lookup` map of
        Nothing -> Nothing
        Just (h:_) -> Just h

---Cabal PackageIdentifier parser

cabalStyle  :: P.LanguageDef st
cabalStyle  = emptyDef
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                }

lexer       =   P.makeTokenParser cabalStyle
lexeme      =   P.lexeme lexer
whiteSpace  =   P.whiteSpace lexer
hexadecimal =   P.hexadecimal lexer
symbol      =   P.symbol lexer

parseCabal :: FilePath -> IO String
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
