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

import GHC
import BasicTypes
import SrcLoc
import RdrName
import OccName
import DynFlags
import Finder
import PackageConfig hiding (exposedModules)
import Module

import Ghf.Core.State
import Ghf.File
import Ghf.Preferences

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
collectSources _ sourceMap pdescr = do
    libDir          <-  getSysLibDir
    session         <-  newSession (Just libDir)
    dflags0         <-  getSessionDynFlags session
    setSessionDynFlags session dflags0
    case sourceForPackage (packagePD pdescr) sourceMap of
        Nothing -> do
            putStrLn $ "No source for package " ++ showPackageId (packagePD pdescr)
            return (pdescr,0)
        Just cabalPath -> do
            pd              <-  readPackageDescription silent cabalPath >>=
                                    (\gpd -> return (flattenPackageDescription gpd))
            let allModules  =   libModules pd ++ exeModules pd
            let buildPaths  =   nub $ ("dist" </> "build" </> "autogen") :
                                    (concatMap hsSourceDirs $ allBuildInfos pd)
            let basePath    =   normalise $ (takeDirectory cabalPath)

            putStrLn $ "Base paths " ++ show basePath
            putStrLn $ "Build paths " ++ show buildPaths
            dflags0         <-  getSessionDynFlags session
            setSessionDynFlags session dflags0  {   topDir      =   basePath
                                                ,   importPaths =   buildPaths
                                                ,   ghcMode     =   CompManager
                                                ,   thisPackage =   mkPackageId
                                                                        (packagePD pdescr)
                                                ,   hscTarget  =    HscNothing
                                                }
            targets <- mapM (\mod -> guessTarget mod Nothing) allModules
            setTargets session targets --(allTargetsFor pd)
            succ            <-  load session LoadAllTargets
            when (not (succeeded succ)) $
                            putStrLn $ "Load for package " ++  showPackageId (packagePD pdescr)
                                                    ++ "does not succeeed."
            (newPackDescr,newModDescrs,succCount)
                            <-  foldM (collectSourcesForModule session)
                                                (pdescr,[],0)
                                                (exposedModulesPD pdescr)
            let nPackDescr  =   newPackDescr{mbSourcePathPD = Just cabalPath,
                                            exposedModulesPD = newModDescrs}
            return (nPackDescr,succCount)

-- ---------------------------------------------------------------------
-- | Collect infos from sources for one module
--
collectSourcesForModule :: Session
    ->  (PackageDescr,[ModuleDescr],Int)
    -> ModuleDescr
    -> IO (PackageDescr,[ModuleDescr],Int)
collectSourcesForModule session  (packDescr,moduleDescrs,succCount) moduleDescr =
    let moduleName          =   mkModuleName $ modu $ moduleIdMD moduleDescr
    in do
        env             <-  sessionHscEnv session
        findResult      <-  findHomeModule env moduleName
        mbFile          <-  case findResult of
                                Found modLocation _ ->  do
                                    return (ml_hs_file modLocation)
                                _                   ->  do
                                putStrLn $ "Can't find module "
                                                        ++  (modu $ moduleIdMD moduleDescr)
                                return Nothing
        let newModD     =   moduleDescr{mbSourcePathMD = mbFile}
        mbCheckedModule         <-  getModuleInfo session (mkModule
                                        (mkPackageId (packagePD packDescr)) moduleName)
        case mbCheckedModule of
            Nothing             ->  do
                putStrLn $ "Can't load module " ++  (modu $ moduleIdMD moduleDescr)
                return (packDescr,newModD:moduleDescrs,succCount)
            Just checkedModule  ->  do
                putStrLn $ "Succeeded to parse " ++ (modu $ moduleIdMD moduleDescr)
                let (L _ (HsModule _ _ _ decls _ _ _ _)) = parsedSource checkedModule
                let newPackDescr =  foldl (collectParseInfoForDecl newModD) packDescr decls
                return(newPackDescr,newModD:moduleDescrs,succCount+1)

collectParseInfoForDecl :: ModuleDescr -> PackageDescr -> (LHsDecl RdrName)  -> PackageDescr
collectParseInfoForDecl _ packDescr (L loc _) | not (isGoodSrcSpan loc) = packDescr
collectParseInfoForDecl modDescr packDescr (L srcDecl (ValD (FunBind lid _ _ _ _ _)))
    =   let id          =   unLoc lid
            occ         =   rdrNameOcc id
            -- name        =   occNameString occ
        in  packDescr{idDescriptionsPD  =  map (addLocation (occNameSpace occ))
                                                (idDescriptionsPD packDescr)}
        where
        addLocation  ::  NameSpace -> IdentifierDescr -> IdentifierDescr
        addLocation  occNameSpace identDescr
            =  if moduleIdID identDescr == moduleIdMD modDescr
                        &&  identifierTypeID identDescr `matchesOccType` occNameSpace
                    then identDescr{mbLocation = Just (srcSpanToLocation srcDecl)}
                    else identDescr
collectParseInfoForDecl _ packDescr _
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
matchesOccType _ _                              =   trace "occType mismatch " False


allBuildInfos :: PackageDescription -> [BuildInfo]
allBuildInfos pd = libBuildInfos ++ (map buildInfo $ executables pd)
    where libBuildInfos      =   case  library pd of
                                    Just lib    ->  [libBuildInfo lib]
                                    Nothing     ->  []

allTargetsFor :: PackageDescription -> [Target]
allTargetsFor pd = libTargets ++ map (\ex -> Target (TargetFile (modulePath ex) Nothing) Nothing)
                            (executables pd)
    where libTargets        =   case  library pd of
                                    Just lib    ->  map (\ m -> Target (TargetModule
                                                        (mkModuleName m)) Nothing)
                                                        $ exposedModules lib
                                    Nothing     ->  []


-- ---------------------------------------------------------------------
--  | Parser function copied here, because it is not exported
--
--myParseModule :: DynFlags -> FilePath -> Maybe StringBuffer
--              -> IO (Either ErrMsg (Located (HsModule RdrName)))
--myParseModule dflags src_filename maybe_src_buf
-- =    --------------------------  Parser  ----------------
--      showPass dflags "Parser" >>
--      {-# SCC "Parser" #-} do
--
--	-- sometimes we already have the buffer in memory, perhaps
--	-- because we needed to parse the imports out of it, or get the
--	-- module name.
--      buf <- case maybe_src_buf of
--		Just b  -> return b
--		Nothing -> hGetStringBuffer src_filename
--
--      let loc  = mkSrcLoc (mkFastString src_filename) 1 0
--
--      case unP parseModule (mkPState buf loc dflags) of {
--
--	PFailed span err -> return (Left (mkPlainErrMsg span err));
--
--	POk pst rdr_module -> do {
--
--      let {ms = getMessages pst};
--      printErrorsAndWarnings dflags ms;
--      when (errorsFound dflags ms) $ exitWith (ExitFailure 1);
--
--      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;
--
--      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
--			   (ppSourceStats False rdr_module) ;
--
--      return (Right rdr_module)
--	-- ToDo: free the string buffer later.
--      }}

--findSourceForModule :: ModuleIdentifier -> FilePath -> [FilePath] -> IO (Maybe FilePath)
--findSourceForModule mod filePath sourceFiles =
--  if null mod
--    then return Nothing
--    else do
--        let moduleBaseName      =   last (breaks (== '.') mod)
--        let filesWithRightName  =   filter (\fp -> moduleBaseName == takeBaseName fp) sourceFiles
--        possibleFiles           <-  filterM (\ fn -> do
--                                        mn <- moduleNameFromFilePath fn
--                                        case mn of
--                                            Nothing -> return False
--                                            Just m  -> return (m == mod))
--                                       filesWithRightName
--        if null possibleFiles
--            then return Nothing
--            else return (Just $ head possibleFiles)

-- ---------------------------------------------------------------------
--  | Adding information from parsing via GHC-API
--
--collectParseInfo :: Session -> PackageDescr -> IO (PackageDescr,Int)
--collectParseInfo session packDescr =
--    foldM (collectParseInfoForModule session) (packDescr,0) (exposedModulesPD packDescr)
--
--collectParseInfoForModule :: Session
--    -> (PackageDescr,Int)
--    -> ModuleDescr
--    -> IO (PackageDescr,Int)
--collectParseInfoForModule session (packDescr,failureNum) modDescr = do
--    case mbSourcePathMD modDescr of
--        Nothing -> do
--            putStrLn $ "No source for module " ++ showPackModule (moduleIdMD modDescr)
--            return (packDescr,failureNum)
--        Just fp -> do
--            --ADDING simple PREPROCESSING (unlit .lhs, apply cpp)
--
--            dynFlags    <-  getSessionDynFlags session
--            str         <-  readFile fp
--            let str'    =   if takeExtension fp == ".lhs"
--                                then unlit fp str
--                                else str
--            stringBuffer <- stringToStringBuffer str'
--            parseResult <-  myParseModule dynFlags fp (Just stringBuffer)
--            case parseResult of
--                Left errMsg -> do
--                            --may need to preprocess
--                    tempFileName    <-  getConfigFilePathForSave "Temp.hspp"
--                    isItTheir       <-  doesFileExist tempFileName
--                    when isItTheir  $
--                        removeFile tempFileName
--                    pid             <-  runCommand $ "ghc -cpp -E -o \""
--                                           ++ tempFileName ++ "\" " ++ fp
--                    waitForProcess pid
--                    isItTheir <- doesFileExist tempFileName
--                    if isItTheir
--                        then do
--                            tempFile <- openFile tempFileName ReadMode
--                            str <- hGetContents tempFile
--                            hClose tempFile
--                            let str' = if takeExtension fp == ".lhs"
--                                        then unlit fp str
--                                        else str
--                            stringBuffer <- stringToStringBuffer str
--                            parseResult <- myParseModule dynFlags fp (Just stringBuffer)
--                            case parseResult of
--                                Left errMsg -> do
--                                    putStrLn $ "Failed to parse " ++ fp
--                                    printBagOfErrors defaultDynFlags (unitBag errMsg)
--                                    return (packDescr,failureNum + 1)
--                                Right (L _ (HsModule _ _ _ decls _ _ _ _)) -> do
--                                --putStrLn $ "Succeeded to parse " ++ fp
--                                let newPackDescr =  foldl (collectParseInfoForDecl modDescr)
--                                                        packDescr decls
--                                return (newPackDescr,failureNum)
--                        else do
--                               putStrLn $ "Failed to preprocess " ++ fp
--                               return (packDescr,failureNum + 1)
--                Right (L _ (HsModule _ _ _ decls _ _ _ _)) -> do
--                    putStrLn $ "Succeeded to parse " ++ fp
--                    let newPackDescr =  foldl (collectParseInfoForDecl modDescr)
--                                                            packDescr decls
--                    return (newPackDescr,failureNum)



