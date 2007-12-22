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
import Data.Maybe(fromJust,mapMaybe)
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
            (newModDescrs,failureCount)
                            <-  foldM (collectSourcesForModule session pkgDescr) ([],0)
                                                (zip (exposedModulesPD pdescr) sourceFiles)
            let nPackDescr  =   pdescr{mbSourcePathPD = Just cabalPath,
                                            exposedModulesPD = newModDescrs}
            return (nPackDescr,failureCount)

-- ---------------------------------------------------------------------
-- | Collect infos from sources for one module
--
collectSourcesForModule :: Session
    -> PackageDescription
    ->  ([ModuleDescr],Int)
    -> (ModuleDescr, Maybe FilePath)
    -> IO ([ModuleDescr],Int)
collectSourcesForModule session pkgDescr (moduleDescrs,failureCount) (moduleDescr,mbfp) =
    case mbfp of
        Nothing ->  do
            putStrLn $ "No source for module " ++ (modu $ moduleIdMD moduleDescr)
            return(moduleDescr : moduleDescrs, failureCount+1)
        Just fp ->  do
            str             <-  preprocess fp pkgDescr
            stringBuffer    <-  stringToStringBuffer str
            dynFlags        <-  getSessionDynFlags session
            parseResult     <-  myParseModule dynFlags fp (Just stringBuffer)
            let newModD     =   moduleDescr{mbSourcePathMD = mbfp}
            case parseResult of
                Right (L _ (HsModule _ _ _ decls _ _ _ _)) -> do
                    putStrLn $ "Succeeded to parse " ++ fp ++ " " ++ show (length decls)
                    let map'    =   convertToMap (idDescriptionsMD newModD)
                    let commentedDecls = addComments decls
                    let descrs  =   mapMaybe (collectParseInfoForDecl map') commentedDecls
                    let newModD' =  newModD{idDescriptionsMD = descrs}
                    return(newModD' : moduleDescrs, failureCount)
                Left errMsg -> do
                    putStrLn $ "Failed to parse " ++ fp
                    printBagOfErrors defaultDynFlags (unitBag errMsg)
                    return (newModD : moduleDescrs, failureCount+1)
    where
    convertToMap :: [IdentifierDescr] -> Map Symbol [IdentifierDescr]
    convertToMap  list  =
     foldl (\ st idDescr -> Map.insertWith (++) (identifierID idDescr) [idDescr] st)
        Map.empty list
    convertFromMap :: Map Symbol [IdentifierDescr]  -> [IdentifierDescr]
    convertFromMap      =   concat . Map.elems

addComments :: [LHsDecl RdrName] -> [(Maybe (LHsDecl RdrName), Maybe String)]
addComments declList = reverse $ snd $ foldl addComment (Nothing,[]) declList
    where
    addComment :: (Maybe String,[(Maybe (LHsDecl RdrName),Maybe String)])
        ->  LHsDecl RdrName
        -> (Maybe String,[(Maybe (LHsDecl RdrName),Maybe String)])
    addComment (maybeComment,((Just decl,Nothing):r)) (L srcDecl (DocD (DocCommentPrev doc))) =
        (Nothing,((Just decl,Just (printHsDoc doc)):r))
    addComment other (L srcDecl (DocD (DocCommentPrev doc))) =
        other
    addComment (maybeComment,resultList) (L srcDecl (DocD (DocCommentNext doc))) =
        (Just (printHsDoc doc),resultList)
    addComment (maybeComment,resultList) (L srcDecl (DocD (DocGroup i doc))) =
        (Nothing,(((Nothing,Just (printHsDoc doc)): resultList)))
    addComment (maybeComment,resultList) (L srcDecl (DocD (DocCommentNamed str doc))) =
        trace ("docCommentNamed " ++ str ++ printHsDoc doc)
        (Nothing,resultList)
    addComment (Nothing,resultList) decl =
        (Nothing,(Just decl,Nothing):resultList)
    addComment (Just comment,resultList) decl =
        (Nothing,(Just decl,Just comment):resultList)

collectParseInfoForDecl ::  SymbolTable ->
    (Maybe (LHsDecl RdrName),Maybe String) ->
     Maybe IdentifierDescr
collectParseInfoForDecl st (Just (L loc _),_) | not (isGoodSrcSpan loc) = Nothing
collectParseInfoForDecl st ((Just (L srcDecl (ValD (FunBind lid _ _ _ _ _)))), mbComment')
    =   let occ         =   rdrNameOcc (unLoc lid)
            name        =   unpackFS (occNameFS occ)
            mbItems     =   Map.lookup name st
            mbItem      =   case mbItems of
                                Nothing -> Nothing
                                Just [item] -> Just item
                                Just items
                                    -> case filter (\i -> matchesOccType (identifierTypeID i)
                                                    (occNameSpace occ)) items of
                                            []  ->  Nothing
                                            l   ->  Just (head l)
        in case mbItem of
            Nothing         -> Nothing
            Just identDescr -> Just (identDescr{mbLocation = Just (srcSpanToLocation srcDecl),
                                                mbComment = mbComment'})
collectParseInfoForDecl st ((Just (L srcDecl (TyClD (TyData _ _ lid _ _ _ _ _)))), mbComment')
    =   let occ         =   rdrNameOcc (unLoc lid)
            name        =   unpackFS (occNameFS occ)
            mbItems     =   Map.lookup name st
            mbItem      =   case mbItems of
                                Nothing -> Nothing
                                Just [item] -> Just item
                                Just items
                                    -> case filter (\i -> matchesOccType (identifierTypeID i)
                                                    (occNameSpace occ)) items of
                                            []  ->  Nothing
                                            l   ->  Just (head l)
        in case mbItem of
            Nothing         -> Nothing
            Just identDescr -> Just (identDescr{mbLocation = Just (srcSpanToLocation srcDecl),
                                                mbComment = mbComment'})
collectParseInfoForDecl st (Just decl,mbComment')    =
    trace (declTypeToString (unLoc decl) ++ "--" ++ (showSDocUnqual $ppr decl)) Nothing
collectParseInfoForDecl st (Nothing, mbComment')    =
    trace ("Found comment " ++ show mbComment') Nothing

    
declTypeToString :: Show alpha => HsDecl alpha -> String
declTypeToString  (TyClD	_)  =   "TyClD"
declTypeToString  (InstD _) =   "InstD"
declTypeToString  (DerivD _)=   "DerivD"
declTypeToString  (ValD	_)  =   "ValD"
declTypeToString  (SigD _)  =   "SigD"
declTypeToString  (DefD _)  =   "DefD"
declTypeToString  (ForD _)  =   "ForD"
declTypeToString  (DeprecD _)=  "DeprecD"
declTypeToString  (RuleD _) =   "RuleD"
declTypeToString  (SpliceD _) = "SpliceD"
declTypeToString  (DocD v)  =   "DocD " ++ show v


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



printHsDoc :: Show alpha => HsDoc alpha  -> String
printHsDoc DocEmpty                     =   ""
printHsDoc (DocAppend l r)              =   printHsDoc l ++ " " ++ printHsDoc r
printHsDoc (DocString str)              =   str
printHsDoc (DocParagraph d)             =   "\n" ++ printHsDoc d
printHsDoc (DocIdentifier l)            =   concatMap show l
printHsDoc (DocModule str)              =   "Module " ++ str
printHsDoc (DocEmphasis doc)            =   printHsDoc doc
printHsDoc (DocMonospaced doc)          =   printHsDoc doc
printHsDoc (DocUnorderedList l)         =   concatMap printHsDoc l
printHsDoc (DocOrderedList l)           =   concatMap printHsDoc l
printHsDoc (DocDefList li)              =   concatMap (\(l,r) -> printHsDoc l ++ " " ++ printHsDoc r) li
printHsDoc (DocCodeBlock doc)           =   printHsDoc doc
printHsDoc (DocURL str)                 =   str
printHsDoc (DocAName str)               =   str

instance Show RdrName where
    show                                =   unpackFS . occNameFS . rdrNameOcc

instance Show alpha => Show (DocDecl alpha) where
        show  (DocCommentNext doc)      =       "DocCommentNext " ++ show doc
        show  (DocCommentPrev doc)      =       "DocCommentPrev " ++ show doc
        show  (DocCommentNamed str doc) =       "DocCommentNamed" ++ " " ++ str ++ " " ++ show doc
        show  (DocGroup i doc)          =       "DocGroup" ++ " " ++ show i ++ " " ++ show doc

--instance Show alpha => Show (HsDoc alpha) where
--    show d = printHsDoc d


 ---------------------------------------------------------------------
--  | Simple preprocessing

preprocess :: FilePath -> PackageDescription -> IO String
preprocess fp pkgDescr =
    let aBuildInfo      =   head (allBuildInfo pkgDescr)
        needCpp         =   elem "-cpp" (hcOptions GHC (options aBuildInfo))
    in C.catch (do
        str' <-  if True
                    then do
                        tempFileName    <-  getConfigFilePathForSave "Temp.hspp"
                        isItTheir       <-  doesFileExist tempFileName
                        when isItTheir  $
                            removeFile tempFileName
                        (_, conf') <- requireProgram normal ghcProgram
                                        (orLaterVersion (Version [6,2] []))
                                        defaultProgramConfiguration
                        runSimplePreProcessor (ppCpp aBuildInfo
                            LocalBuildInfo
                                {   compiler=comp
                                ,   withPrograms=conf'}) fp tempFileName normal
                        isItTheir <- doesFileExist tempFileName
                        if isItTheir
                            then do
                                putStrLn $ "Succeeded to preprocess " ++ fp
                                str <- readFile tempFileName
                                return str
                            else do
                                putStrLn $ "Failed to preprocess " ++ fp
                                str <- readFile fp
                                return str
                    else do
                        str <- readFile fp
                        return str
        let str2    =   if takeExtension fp == ".lhs"
                                then unlit fp str'
                                else str'
        return str2)
        (\e -> do   putStrLn $ "unlit error " ++ show e ++ " in " ++ fp
                    str <- readFile fp
                    return str)
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
      -- when (errorsFound dflags ms) $ exitWith (ExitFailure 1);

      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;

      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
			   (ppSourceStats False rdr_module) ;

      return (Right rdr_module)
	-- ToDo: free the string buffer later.
      }}

