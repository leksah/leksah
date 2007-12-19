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
import Distribution.PackageDescription
import Distribution.Verbosity
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import System.Directory
import Data.Maybe(catMaybes)
import Control.Monad.State
import System.FilePath
import System.IO
import Data.List(nub)
import Distribution.Simple.PreProcess.Unlit
import System.Exit
import Distribution.Simple.Configure(configure)
import Distribution.Simple.Setup(emptyConfigFlags)
import Distribution.Simple.PreProcess(runSimplePreProcessor,ppCpp)
import Distribution.Simple.Program(defaultProgramConfiguration,requireProgram,ghcProgram)
import Distribution.Verbosity(normal)
import Distribution.Compiler(CompilerFlavor(..))
import Distribution.Simple.Compiler(Compiler(..))
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo(..))
import System.Process(runCommand,waitForProcess)
import System.Info (os, arch)
import Data.Maybe(fromJust)
import Distribution.Version(readVersion,orLaterVersion)
import qualified Control.Exception as C


import GHC
import BasicTypes
import SrcLoc
import RdrName
import OccName
import DynFlags
import Finder
import PackageConfig hiding (exposedModules)
import Module
import StringBuffer
import Bag
import ErrUtils
import FastString
import Lexer hiding (lexer)
import Parser
import Outputable hiding (char)
import HscStats
import Config(cProjectVersion)

import Ghf.Core.State hiding(trace)
import Ghf.File
import Ghf.Preferences
import Ghf.Info

import Debug.Trace

-- ---------------------------------------------------------------------
-- Function to map packages to file paths
--

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

sourceForPackage :: PackageIdentifier
    -> (Map PackageIdentifier [FilePath])
    -> Maybe FilePath
sourceForPackage id map =
    case id `Map.lookup` map of
        Just (h:_)  ->  Just h
        _           ->  Nothing

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

-- ---------------------------------------------------------------------
-- | Parser for Package DB
--

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

-- ---------------------------------------------------------------------
-- | Parser for the package name from a cabal file
--

cabalStyle  :: P.LanguageDef st
cabalStyle  = emptyDef
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                }

lexer       =   P.makeTokenParser cabalStyle
whiteSpace  =   P.whiteSpace lexer
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
                Left _ -> error "Illegal cabal"
        Right n -> do
            case r2 of
                Left v -> return (n ++ "-" ++ v)
                Right _ -> error "Illegal cabal"

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

-- ---------------------------------------------------------------------
-- Functions to collect infos from the sources
--

-- ---------------------------------------------------------------------
-- | Collect infos from sources for a list of packages
--
collectAllSources :: Session
    -> Map PackageIdentifier [FilePath]
    -> [PackageDescr]
    -> IO ([PackageDescr],Int)
collectAllSources session sourceMap pdescrs =
    foldM (\ (pdescrs, failureNum) pdescr   ->  do
        (pdescr, num) <- collectSources session sourceMap pdescr
        return (pdescr:pdescrs,failureNum + num))
            ([],0) pdescrs

-- ---------------------------------------------------------------------
-- | Collect infos from sources for one package
--
collectSources :: Session
    -> Map PackageIdentifier [FilePath]
    -> PackageDescr
    -> IO (PackageDescr,Int)
collectSources session  sourceMap pdescr = do
    case sourceForPackage (packagePD pdescr) sourceMap of
        Nothing -> do
            putStrLn $ "No source for package " ++ showPackageId (packagePD pdescr)
            return (pdescr,0)
        Just cabalPath -> do
            pkgDescr        <-  readPackageDescription silent cabalPath >>=
                                    (\gpd -> return (flattenPackageDescription gpd))
            --let allModules  =   libModules pd ++ exeModules pd
            let exposedMods =   exposedModulesPD pdescr
            let buildPaths  =   nub $ ("dist" </> "build" </> "autogen") :
                                    (concatMap hsSourceDirs $ allBuildInfo pkgDescr)
            let basePath    =   normalise $ (takeDirectory cabalPath)
            sourceFiles     <-  mapM (findSourceFile
                                        (map (\p -> basePath </> p) buildPaths)
                                        ["hs","lhs"])
                                        (map (modu . moduleIdMD) exposedMods)
            (newPackDescr,newModDescrs,failureCount)
                            <-  foldM (collectSourcesForModule session pkgDescr)
                                                (pdescr,[],0)
                                                (zip (exposedModulesPD pdescr) sourceFiles)
            let nPackDescr  =   newPackDescr{mbSourcePathPD = Just cabalPath,
                                            exposedModulesPD = newModDescrs}
            return (nPackDescr,failureCount)

-- ---------------------------------------------------------------------
-- | Collect infos from sources for one module
--
collectSourcesForModule :: Session
    -> PackageDescription
    ->  (PackageDescr,[ModuleDescr],Int)
    -> (ModuleDescr, Maybe FilePath)
    -> IO (PackageDescr,[ModuleDescr],Int)
collectSourcesForModule session pkgDescr (packDescr,moduleDescrs,failureCount) (moduleDescr,mbfp) =
    case mbfp of
        Nothing ->  do
            putStrLn $ "No source for module " ++ (modu $ moduleIdMD moduleDescr)
            return(packDescr, moduleDescr : moduleDescrs, failureCount+1)
        Just fp ->  do
            str             <-  preprocess fp pkgDescr
            stringBuffer    <-  stringToStringBuffer str
            dynFlags        <-  getSessionDynFlags session
            parseResult     <-  myParseModule dynFlags fp (Just stringBuffer)
            let newModD     =   moduleDescr{mbSourcePathMD = mbfp}
            case parseResult of
                Right (L _ (HsModule _ _ _ decls _ _ _ _)) -> do
                    putStrLn $ "Succeeded to parse " ++ fp ++ " " ++ show (length decls)
                    let symbolTable = buildSymbolTable packDescr Map.empty
                    let newSymbolTable =  foldl (collectParseInfoForDecl newModD)
                                                symbolTable decls
                    return(packDescr, -- {idDescriptionsPD=concat (Map.elems newSymbolTable)},
                            newModD : moduleDescrs, failureCount)
                Left errMsg -> do
                    putStrLn $ "Failed to parse " ++ fp
                    printBagOfErrors defaultDynFlags (unitBag errMsg)
                    return (packDescr, newModD : moduleDescrs, failureCount+1)

collectParseInfoForDecl :: ModuleDescr -> SymbolTable -> (LHsDecl RdrName)  -> SymbolTable
collectParseInfoForDecl modDescr st (L loc _) | not (isGoodSrcSpan loc) = st
collectParseInfoForDecl modDescr st (L srcDecl (ValD (FunBind lid _ _ _ _ _)))
    =   let id          =   unLoc lid
            occ         =   rdrNameOcc id
            name        =   unpackFS (occNameFS occ)
            updatef identDescr
                        =   if moduleIdID identDescr == moduleIdMD modDescr
                                    &&   identifierTypeID identDescr `matchesOccType`
                                        occNameSpace occ
                                then identDescr{mbLocation = Just (srcSpanToLocation srcDecl)}
                                else identDescr
        in  Map.adjust (map updatef) name st
collectParseInfoForDecl modDescr st _    =   st

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
matchesOccType _ _                              =   trace "occType mismatch " False


 ---------------------------------------------------------------------
--  | Simple preprocessing

preprocess :: FilePath -> PackageDescription -> IO String
preprocess fp pkgDescr =
    let aBuildInfo      =   head (allBuildInfo pkgDescr)
        needCpp         =   elem "-cpp" (hcOptions GHC (options aBuildInfo))
    in do
        str' <-  if True
                    then do
                        tempFileName    <-  getConfigFilePathForSave "Temp.hspp"
                        isItTheir       <-  doesFileExist tempFileName
                        when isItTheir  $
                            removeFile tempFileName
                        (_, conf') <- requireProgram normal ghcProgram
                                        (orLaterVersion (Version [6,2] []))
                                        defaultProgramConfiguration
                        C.catch (runSimplePreProcessor (ppCpp aBuildInfo
                            LocalBuildInfo
                                {   compiler=comp
                                ,   withPrograms=conf'})
                            fp tempFileName normal) (\e -> putStrLn $ show e)
                        isItTheir <- doesFileExist tempFileName
                        if isItTheir
                            then do
                                str <- readFile tempFileName
                                return str
                            else do
                                putStrLn "Failed to preprocess " ++ fp
                                str <- readFile fp
                                return str
                    else do
                        str <- readFile fp
                        return str
        let str2    =   if takeExtension fp == ".lhs"
                                then unlit fp str'
                                else str'
        return str2
    where
    comp = Compiler {
        compilerFlavor         = GHC,
        compilerId             = PackageIdentifier "ghc"
                                        (fromJust $ readVersion cProjectVersion),
        compilerExtensions     = []
      }

 ---------------------------------------------------------------------
--  | Parser function copied here, because it is not exported

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

