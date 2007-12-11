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
-- | This module collects information for packages with source available
--
-------------------------------------------------------------------------------

module Ghf.SourceCollector (
    collectAllSources
,   collectSources
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
import Data.Maybe(catMaybes,mapMaybe)
import Distribution.Simple.Utils(breaks)
import System.FilePath(takeBaseName,dropFileName)
import System.Exit
import Control.Monad.State
import System.FilePath
import Distribution.Simple.PreProcess.Unlit
import System.Process
import System.IO
import Control.Concurrent

import Digraph
import HscTypes
import GHC
import BasicTypes
import StringBuffer
import ErrUtils
import SrcLoc
import FastString
import Lexer hiding (lexer)
import Parser
import Outputable hiding (char)
import HscStats
import RdrName
import OccName
import DynFlags
import Bag

import Ghf.Core.State
import Ghf.File
import Ghf.Preferences
import {-# SOURCE #-} Ghf.InterfaceCollector

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

collectAllSources :: Session
    -> Map PackageIdentifier [FilePath]
    -> [PackageDescr]
    -> IO ([PackageDescr],Int)
collectAllSources session sourceMap pdescrs =
    foldM (\ (pdescrs, failureNum) pdescr   ->  do
        (pdescr, num) <- collectSources session sourceMap pdescr
        return (pdescr:pdescrs,failureNum + num))
            ([],0) pdescrs

collectSources :: Session
    -> Map PackageIdentifier [FilePath]
    -> PackageDescr
    -> IO (PackageDescr,Int)
collectSources session sourceMap pdescr =
    case sourceForPackage (packagePD pdescr) sourceMap of
        Nothing -> do
            --putStrLn $ "No source for package " ++ showPackageId (packagePD pdescr)
            return (pdescr,0)
        Just fp -> do
            let path        =   dropFileName fp
            sf              <-  allHaskellSourceFiles path
            newModDescr     <-  mapM (collectSourcesForModules path sf)
                                    (exposedModulesPD pdescr)
            let nPackDescr  =   pdescr{mbSourcePathPD = Just fp,exposedModulesPD = newModDescr}
            collectParseInfo session nPackDescr

collectSourcesForModules :: FilePath -> [FilePath] -> ModuleDescr -> IO (ModuleDescr)
collectSourcesForModules filePath sourceFiles moduleDescr = do
    mbFile <-   findSourceForModule (modu $ moduleIdMD moduleDescr) filePath sourceFiles
    return (moduleDescr{mbSourcePathMD = mbFile})

findSourceForModule :: ModuleIdentifier -> FilePath -> [FilePath] -> IO (Maybe FilePath)
findSourceForModule mod filePath sourceFiles =
  if null mod
    then return Nothing
    else do
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

-- ---------------------------------------------------------------------
--  | Adding information from parsing via GHC-API
--
collectParseInfo :: Session -> PackageDescr -> IO (PackageDescr,Int)
collectParseInfo session packDescr =
    foldM (collectParseInfoForModule session) (packDescr,0) (exposedModulesPD packDescr)

collectParseInfoForModule :: Session
    -> (PackageDescr,Int)
    -> ModuleDescr
    -> IO (PackageDescr,Int)
collectParseInfoForModule session (packDescr,failureNum) modDescr = do
    case mbSourcePathMD modDescr of
        Nothing -> do
            putStrLn $ "No source for module " ++ showPackModule (moduleIdMD modDescr)
            return (packDescr,failureNum)
        Just fp -> do
            --ADDING simple PREPROCESSING (unlit .lhs, apply cpp)

            dynFlags    <- getSessionDynFlags session
            str <- readFile fp
            let str' = if takeExtension fp == ".lhs"
                            then unlit fp str
                            else str
            stringBuffer <- stringToStringBuffer str'
            parseResult <- myParseModule dynFlags fp (Just stringBuffer)
            case parseResult of
                Left errMsg -> do
                            --may need to preprocess
                    tempFileName <- getConfigFilePathForSave "Temp.hspp"
                    isItTheir <- doesFileExist tempFileName
                    when isItTheir $
                        removeFile tempFileName
                    pid <- runCommand $ "ghc -cpp -E -o \""++ tempFileName ++ "\" " ++ fp
                    waitForProcess pid
                    isItTheir <- doesFileExist tempFileName
                    if isItTheir
                        then do
                            tempFile <- openFile tempFileName ReadMode
                            str <- hGetContents tempFile
                            hClose tempFile
                            let str' = if takeExtension fp == ".lhs"
                                        then unlit fp str
                                        else str
                            stringBuffer <- stringToStringBuffer str
                            parseResult <- myParseModule dynFlags fp (Just stringBuffer)
                            case parseResult of
                                Left errMsg -> do
                                    putStrLn $ "Failed to parse " ++ fp
                                    printBagOfErrors defaultDynFlags (unitBag errMsg)
                                    return (packDescr,failureNum + 1)
                                Right (L _ (HsModule _ _ _ decls _ _ _ _)) -> do
                                --putStrLn $ "Succeeded to parse " ++ fp
                                let newPackDescr =  foldl (collectParseInfoForDecl modDescr)
                                                        packDescr decls
                                return (newPackDescr,failureNum)
                        else do
                               putStrLn $ "Failed to preprocess " ++ fp
                               return (packDescr,failureNum + 1)
                Right (L _ (HsModule _ _ _ decls _ _ _ _)) -> do
                    putStrLn $ "Succeeded to parse " ++ fp
                    let newPackDescr =  foldl (collectParseInfoForDecl modDescr)
                                                            packDescr decls
                    return (newPackDescr,failureNum)

collectParseInfoForDecl :: ModuleDescr -> PackageDescr -> (LHsDecl RdrName)  -> PackageDescr
collectParseInfoForDecl modDescr packDescr (L loc decl) | not (isGoodSrcSpan loc) = packDescr
collectParseInfoForDecl modDescr packDescr (L srcDecl (ValD (FunBind lid _ _ _ _ _)))
    =   let id          =   unLoc lid
            occ         =   rdrNameOcc id
            name        =   occNameString occ
        in  packDescr{idDescriptionsPD  =  map (addLocation (occNameSpace occ))
                                                (idDescriptionsPD packDescr)}
        where
        addLocation  ::  NameSpace -> IdentifierDescr -> IdentifierDescr
        addLocation  occNameSpace identDescr
            =  if moduleIdID identDescr == moduleIdMD modDescr
                        &&  identifierTypeID identDescr `matchesOccType` occNameSpace
                    then identDescr{mbLocation = Just (srcSpanToLocation srcDecl)}
                    else identDescr
collectParseInfoForDecl modDescr packDescr _
    =   packDescr

srcSpanToLocation :: SrcSpan -> Location
srcSpanToLocation span | not (isGoodSrcSpan span)
    =   throwGhf "srcSpanToLocation: unhelpful span"
srcSpanToLocation span
    =   Location (srcSpanStartLine span) (srcSpanStartCol span)
                 (srcSpanEndLine span) (srcSpanEndCol span)

matchesOccType :: IdType -> NameSpace -> Bool
matchesOccType Function varName                 =   True
matchesOccType Data     tcName                  =   True
matchesOccType Newtype  _                       =   True
matchesOccType Synonym  _                       =   True
matchesOccType AbstractData _                   =   True
matchesOccType Class clsName                    =   True
matchesOccType Foreign _                        =   True
otherwise                                       =   trace "occType mismatch " False

-- ---------------------------------------------------------------------
--  | Parser function copied here, because it is not exported
--
myParseModule :: DynFlags -> FilePath -> Maybe StringBuffer
              -> IO (Either ErrMsg (Located (HsModule RdrName)))
myParseModule dflags src_filename maybe_src_buf
 =    --------------------------  Parser  ----------------
      showPass dflags "Parser" >>
      {-# SCC "Parser" #-} do

	-- sometimes we already have the buffer in memory, perhaps
	-- because we needed to parse the imports out of it, or get the
	-- module name.
      buf <- case maybe_src_buf of
		Just b  -> return b
		Nothing -> hGetStringBuffer src_filename

      let loc  = mkSrcLoc (mkFastString src_filename) 1 0

      case unP parseModule (mkPState buf loc dflags) of {

	PFailed span err -> return (Left (mkPlainErrMsg span err));

	POk pst rdr_module -> do {

      let {ms = getMessages pst};
      printErrorsAndWarnings dflags ms;
      when (errorsFound dflags ms) $ exitWith (ExitFailure 1);

      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;

      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
			   (ppSourceStats False rdr_module) ;

      return (Right rdr_module)
	-- ToDo: free the string buffer later.
      }}

-- ---------------------------------------------------------------------
--  | Source for package DB
--
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

