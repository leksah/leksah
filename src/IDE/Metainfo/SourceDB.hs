{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.SourceDB
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Metainfo.SourceDB (

    buildSourceForPackageDB
,   sourceForPackage
,   parseSourceForPackageDB
,   getSourcesMap
,   getDataDir
,   P.version

) where

import Control.Applicative
import Prelude
import IDE.StrippedPrefs
       (getUnpackDirectory, getSourceDirectories, Prefs(..))
import Data.Map (Map)
import Distribution.Package (PackageIdentifier(..))
import IDE.Utils.Utils (standardSourcesFilename)
import qualified Data.Map as Map
       (fromList, toList, fromListWith, lookup)
import IDE.Utils.FileUtils
       (myCanonicalizePath, getConfigFilePathForLoad,
        getConfigFilePathForSave, allCabalFiles)
import System.Directory (doesFileExist)
import Data.List (foldl')
import qualified Text.PrettyPrint as PP
       (colon, (<>), text, ($$), vcat, Doc, render, char)
import Text.ParserCombinators.Parsec
       (try, char, unexpected, noneOf, eof, CharParser,
        Parser, (<?>))
import Text.ParserCombinators.Parsec.Prim (parse)
import Text.ParserCombinators.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec.Language (emptyDef)
#if MIN_VERSION_parsec(3,0,0)
import qualified Text.ParserCombinators.Parsec.Token as P
       (GenTokenParser(..), TokenParser, makeTokenParser, commentLine,
        commentEnd, commentStart, LanguageDef)
#else
import qualified Text.ParserCombinators.Parsec.Token as P
       (TokenParser(..), makeTokenParser, commentLine,
        commentEnd, commentStart, LanguageDef)
#endif
import Data.Maybe (catMaybes)
import IDE.Core.CTypes (packageIdentifierFromString)
import qualified Paths_leksah as P (getDataDir, version)
import System.Log.Logger(errorM,debugM)
import System.IO.Strict as S (readFile)
import System.Environment.Executable (getExecutablePath)
import System.FilePath
       (takeDirectory, (</>), takeFileName)
import System.Directory (doesDirectoryExist)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import Data.Monoid ((<>))

-- ---------------------------------------------------------------------
-- Function to map packages to file paths
--

getSourcesMap :: Prefs -> IO (Map PackageIdentifier [FilePath])
getSourcesMap prefs = do
        mbSources <- parseSourceForPackageDB
        case mbSources of
            Just map' -> return map'
            Nothing -> do
                buildSourceForPackageDB prefs
                mbSources' <- parseSourceForPackageDB
                case mbSources' of
                    Just map'' -> return map''
                    Nothing ->  error "can't build/open source for package file"

sourceForPackage :: PackageIdentifier
    -> Map PackageIdentifier [FilePath]
    -> Maybe FilePath
sourceForPackage pid pmap =
    case pid `Map.lookup` pmap of
        Just (h:_)  ->  Just h
        _           ->  Nothing

buildSourceForPackageDB :: Prefs -> IO ()
buildSourceForPackageDB prefs = do
    sourceDirs <- getSourceDirectories prefs
    unpackDir  <- getUnpackDirectory prefs
    let dirs        =   case unpackDir of
                            Just dir -> dir : sourceDirs
                            Nothing ->  sourceDirs
    cabalFiles      <-  mapM allCabalFiles dirs
    fCabalFiles     <-  mapM myCanonicalizePath $ concat cabalFiles
    mbPackAndFiles  <-  mapM (\fp -> do
                                mb <- parseCabal fp
                                case mb of
                                    Just s  -> return $ Just (s, [fp])
                                    Nothing -> return Nothing) fCabalFiles
    let pdToFiles   =   Map.fromListWith (++) $ catMaybes mbPackAndFiles
    filePath        <-  getConfigFilePathForSave standardSourcesFilename
    writeFile filePath  (PP.render (showSourceForPackageDB pdToFiles))

showSourceForPackageDB  :: Map Text [FilePath] -> PP.Doc
showSourceForPackageDB aMap = PP.vcat (map showIt (Map.toList aMap))
    where
    showIt :: (Text,[FilePath]) -> PP.Doc
    showIt (pd,list) =  foldl' (\l n -> l PP.$$ PP.text (show n)) label list
                             PP.<>  PP.char '\n'
        where label  =  PP.text (T.unpack pd) PP.<> PP.colon

-- Strict version
parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p f = do
    input <- S.readFile f
    return $ parse p f input

parseSourceForPackageDB :: IO (Maybe (Map PackageIdentifier [FilePath]))
parseSourceForPackageDB = do
    dataDir         <-  getDataDir
    filePath        <-  getConfigFilePathForLoad standardSourcesFilename Nothing dataDir
    exists          <-  doesFileExist filePath
    if exists
        then do
            res <- parseFromFile sourceForPackageParser filePath
            case res of
                Left pe ->  do
                    errorM "leksah-server" $ "Error reading source packages file "
                            ++ filePath ++ " " ++ show pe
                    return Nothing
                Right r ->  return (Just r)
        else do
            errorM "leksah-server" $" No source packages file found: " ++ filePath
            return Nothing

--
-- ---------------------------------------------------------------------
-- | Parser for Package DB
--
packageStyle  :: P.LanguageDef st
packageStyle  = emptyDef
                { P.commentStart   = "{-"
                , P.commentLine    = "--"
                , P.commentEnd     = "-}"
                }

lexer :: P.TokenParser st
lexer = P.makeTokenParser packageStyle

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

symbol :: Text -> CharParser st Text
symbol = (T.pack <$>) . P.symbol lexer . T.unpack

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
    return (packageIdentifierFromString (T.pack str)))
    <?> "packageDescriptionParser"

filePathParser :: CharParser () FilePath
filePathParser = try (do
    whiteSpace
    char '"'
    str <- many (noneOf "\"")
    char '"'
    return str)
    <?> "filePathParser"

parseCabal :: FilePath -> IO (Maybe Text)
parseCabal fn = do
    --putStrLn $ "Now parsing minimal " ++ fn
    res   <- parseFromFile cabalMinimalParser fn
    case res of
        Left pe ->  do
            errorM "leksah-server" $"Error reading cabal file " ++ show fn ++ " " ++ show pe
            return Nothing
        Right r ->  do
            debugM "leksah-server" (T.unpack r)
            return (Just r)


cabalMinimalParser :: CharParser () Text
cabalMinimalParser = do
    r1 <- cabalMinimalP
    r2 <- cabalMinimalP
    case r1 of
        Left v ->
            case r2 of
                Right n -> return (n <> "-" <> v)
                Left _ -> unexpected "Illegal cabal"
        Right n ->
            case r2 of
                Left v -> return (n <> "-" <> v)
                Right _ -> unexpected "Illegal cabal"

cabalMinimalP :: CharParser () (Either Text Text)
cabalMinimalP =
    do  try $(symbol "name:" <|> symbol "Name:")
        whiteSpace
        name       <-  many $noneOf " \n"
        many $noneOf "\n"
        char '\n'
        return . Right $ T.pack name
    <|> do
            try $(symbol "version:" <|> symbol "Version:")
            whiteSpace
            versionString    <-  many $noneOf " \n"
            many $noneOf "\n"
            char '\n'
            return . Left $ T.pack versionString
    <|> do
            many $noneOf "\n"
            char '\n'
            cabalMinimalP
    <?> "cabal minimal"

getDataDir :: IO FilePath
getDataDir = do
    exePath <- getExecutablePath
    if takeFileName exePath `elem` ["leksah-server.exe", "leksah.exe"]
        then do
            let dataDir = takeDirectory (takeDirectory exePath) </> "leksah"
            exists <- doesDirectoryExist dataDir
            if exists then return dataDir else P.getDataDir
        else P.getDataDir


