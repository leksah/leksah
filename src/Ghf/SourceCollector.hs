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
import Control.Monad.State
import System.FilePath
import System.IO
import Data.List(nub,delete,sort)
import Distribution.Simple.PreProcess.Unlit
import Distribution.Simple.PreProcess(runSimplePreProcessor,ppCpp)
import Distribution.Simple.Program(defaultProgramConfiguration)
import Distribution.Compiler(CompilerFlavor(..))
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo(..))
import Distribution.Simple.Configure(configure)
import Distribution.Simple.Setup(emptyConfigFlags)
import Data.Maybe(catMaybes)
import qualified Control.Exception as C
import qualified Data.ByteString.Char8 as BS


import GHC hiding (idType)
import SrcLoc
import RdrName
import OccName
import DynFlags
import PackageConfig hiding (exposedModules)
import StringBuffer
import Bag
import ErrUtils
import FastString
import Lexer hiding (lexer)
import Parser
import Outputable hiding (char)
import HscStats

import Ghf.Core.State hiding(trace)
import Ghf.File
import Ghf.Preferences

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
        Just cabalPath -> C.catch (do
            let basePath        =   takeDirectory cabalPath
            dflags0             <-  getSessionDynFlags session
            setSessionDynFlags session dflags0
                {   topDir      =   basePath
                ,   ghcMode     =   CompManager
                }
            dflags1         <-  getSessionDynFlags session
            let flags       =   ["-fglasgow-exts",("-I" ++ basePath </> "include"),"-haddock"]
            (dflags2,_)     <-  parseDynamicFlags dflags1 flags
            putStrLn $ "flags = " ++ show flags

            setSessionDynFlags session dflags2

            genPkgDescr     <-  readPackageDescription silent cabalPath
            let pkgDescr    =   flattenPackageDescription genPkgDescr
            localBuildInfo  <-  configure (Left genPkgDescr, emptyHookedBuildInfo)
                                    (emptyConfigFlags defaultProgramConfiguration)
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
                            <-  foldM (collectSourcesForModule session genPkgDescr
                                            localBuildInfo) ([],0)
                                                (zip (exposedModulesPD pdescr) sourceFiles)
            let nPackDescr  =   pdescr{mbSourcePathPD = Just cabalPath,
                                            exposedModulesPD = newModDescrs}
            return (nPackDescr,failureCount))
            (\e -> do   putStrLn $ "source collector error " ++ show e ++ " in " ++
                            showPackageId (packagePD pdescr)
                        return (pdescr,length $ exposedModulesPD pdescr))

-- ---------------------------------------------------------------------
-- | Collect infos from sources for one module
--
collectSourcesForModule :: Session
    -> GenericPackageDescription
    -> LocalBuildInfo
    ->  ([ModuleDescr],Int)
    -> (ModuleDescr, Maybe FilePath)
    -> IO ([ModuleDescr],Int)
collectSourcesForModule session pkgDescr localBuildInfo (moduleDescrs,failureCount)
    (moduleDescr,mbfp) =
    case mbfp of
        Nothing ->  do
            putStrLn $ "No source for module " ++ (modu $ moduleIdMD moduleDescr)
            return(moduleDescr : moduleDescrs, failureCount+1)
        Just fp ->  do
            str             <-  preprocess fp pkgDescr localBuildInfo
            stringBuffer    <-  stringToStringBuffer str
            dynFlags        <-  getSessionDynFlags session
            parseResult     <-  myParseModule dynFlags fp (Just stringBuffer)
            let newModD     =   moduleDescr{mbSourcePathMD = mbfp}
            case parseResult of
                Right (L _ (HsModule _ _ _ decls _ _ _ _)) -> do
                    --putStrLn $ "Succeeded to parse " ++ fp ++ " " ++ show (length decls)
                    let map'                =   convertToMap (idDescriptionsMD newModD)
                    let commentedDecls      =   addComments (filterSignatures decls)
                    let (descrs,restMap)    =   foldl collectParseInfoForDecl ([],map')
                                                    commentedDecls
                    let newModD'            =   newModD{
                        idDescriptionsMD    =   reverse descrs ++ concat (Map.elems restMap)}
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

filterSignatures :: [LHsDecl RdrName] -> [LHsDecl RdrName]
filterSignatures declList = filter filterSignature declList
    where
    filterSignature (L srcDecl (SigD _)) = False
    filterSignature _ = True

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

collectParseInfoForDecl ::  ([IdentifierDescr],SymbolTable)
    -> (Maybe (LHsDecl RdrName),Maybe String)
    -> ([IdentifierDescr],SymbolTable)
collectParseInfoForDecl (l,st) (Just (L loc _),_) | not (isGoodSrcSpan loc) = (l,st)
collectParseInfoForDecl (l,st) ((Just (L loc (ValD (FunBind lid _ _ _ _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Function] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (TyData _ _ lid _ _ _ _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Data] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (TyFamily _ lid _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (TySynonym lid _ _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Synonym] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (ClassDecl _ lid _ _ _ _ _ _ )))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Class] []
collectParseInfoForDecl (l,st) ((Just (L loc (InstD (InstDecl lid binds sigs cldecl)))), mbComment')
    =   case unLoc lid of
            HsForAllTy	_ _ _ e ->
                case unLoc e of
                    HsPredTy p       ->
                        case p of
                            HsClassP n types
                                -> --trace ("name: " ++ unpackFS (occNameFS (rdrNameOcc n)) ++ "\n" ++
                                          --"binds : " ++ showSDocUnqual (ppr binds) ++ "\n" ++
                                          --"sigs : " ++ showSDocUnqual (ppr sigs) ++ "\n" ++
                                          --"cldecl : " ++ showSDocUnqual (ppr cldecl) ++ "\n" ++
                                   --       "types : " ++ showSDocUnqual (ppr types)) $
                                   addLocationAndComment (l,st) n loc mbComment' [Instance]
                                            (map (\t -> analyse (unLoc t)) types)
                            _   -> trace ("lid3:Other") (l,st)
                    _                   -> trace "lid2:Other"    (l,st)
            _                   -> trace "lid:Other"    (l,st)
    where
    analyse (HsTyVar n)             =   unpackFS (occNameFS (rdrNameOcc n))
    analyse (HsForAllTy n _ _ _)    =   trace "lid5:For all" ""
    analyse _                       =   trace "lid5:Other" ""
collectParseInfoForDecl (l,st) (Just decl,mbComment')
    =   trace (declTypeToString (unLoc decl) ++ "--" ++ (showSDocUnqual $ppr decl)) (l,st)
collectParseInfoForDecl (l,st) (Nothing, mbComment')    =
    trace ("Found comment " ++ show mbComment') (l,st)


addLocationAndComment :: ([IdentifierDescr],SymbolTable)
    -> RdrName
    -> SrcSpan
    -> Maybe String
    -> [IdType]
    -> [String]
    -> ([IdentifierDescr],SymbolTable)
addLocationAndComment (l,st) lid srcSpan mbComment' types insts =
    let occ         =   rdrNameOcc lid
        name        =   unpackFS (occNameFS occ)
        mbItems     =   Map.lookup name st
        (mbItem,nst)=   case mbItems of
                            Nothing     -> (Nothing,st)
                            Just [i]    ->  (Just i, Map.delete name st)
                            Just list   ->
                                case filter (\i -> matches i types insts) list of
                                    []  ->  (Nothing,st)
                                    l'  ->  (Just (head l'),
                                                Map.adjust (\li -> delete (head l') li)
                                                    name st)
    in case mbItem of
        Nothing             ->  (l,st)
        Just identDescr     ->  (identDescr{
                mbLocation  =   Just (srcSpanToLocation srcSpan),
                mbComment   =   case mbComment' of
                                    Nothing -> Nothing
                                    Just s -> Just (BS.pack s)}
                                     : l, nst)

    where
    matches :: IdentifierDescr -> [IdType] -> [String] -> Bool
    matches idDescr idTypes inst =
        case idType idDescr of
            Instance ->
                --trace ("instances " ++ show (sort (binds idDescr)) ++ " -- ?= -- " ++ show (sort inst)) $
                elem Instance idTypes && sort (binds idDescr) == sort inst
            other   -> elem other idTypes

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

printHsDoc :: Show alpha => HsDoc alpha  -> String
printHsDoc DocEmpty                     =   ""
printHsDoc (DocAppend l r)              =   printHsDoc l ++ " " ++ printHsDoc r
printHsDoc (DocString str)              =   str
printHsDoc (DocParagraph d)             =   printHsDoc d ++ "\n"
printHsDoc (DocIdentifier l)            =   concatMap show l
printHsDoc (DocModule str)              =   "Module " ++ str
printHsDoc (DocEmphasis doc)            =   printHsDoc doc
printHsDoc (DocMonospaced doc)          =   printHsDoc doc
printHsDoc (DocUnorderedList l)         =   concatMap printHsDoc l
printHsDoc (DocOrderedList l)           =   concatMap printHsDoc l
printHsDoc (DocDefList li)              =   concatMap (\(l,r) -> printHsDoc l ++ " " ++
                                                printHsDoc r) li
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

preprocess :: FilePath -> GenericPackageDescription -> LocalBuildInfo -> IO String
preprocess fp pkgDescr localBuildInfo =
    let aBuildInfo      =   head (allBuildInfo (flattenPackageDescription pkgDescr))
        needCpp         =   elem "-cpp" (hcOptions GHC (options aBuildInfo))
    in C.catch (do
        str' <-  if True
                    then do
                        tempFileName    <-  getConfigFilePathForSave "Temp.hspp"
                        isItTheir       <-  doesFileExist tempFileName
                        when isItTheir  $
                            removeFile tempFileName
--                        (_, conf') <- requireProgram normal ghcProgram
--                                        (orLaterVersion (Version [6,2] []))
--                                        defaultProgramConfiguration

--                        localBuildInfo' <- localBuildInfo pkgDescr conf'

                        runSimplePreProcessor (ppCpp aBuildInfo localBuildInfo)
                                                fp tempFileName normal
                        isItTheir <- doesFileExist tempFileName
                        if isItTheir
                            then do
                                -- putStrLn $ "Succeeded to preprocess " ++ fp
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
        (\e -> do   putStrLn $ "preprocess error " ++ show e ++ " in " ++ fp
                    str <- readFile fp
                    return str)
    where
--    comp = Compiler {
--        compilerFlavor         = GHC,
--        compilerId             = PackageIdentifier "ghc"
--                                        (fromJust $ readVersion cProjectVersion),
--        compilerExtensions     = []
--      }
--    localBuildInfo pd conf = do
--        installDirs <-  defaultInstallDirs GHC False
--        return LocalBuildInfo
--                {   installDirTemplates =   installDirs
--        	    ,   compiler            =   comp
--        	    ,   buildDir            =   ""
--        	    ,   scratchDir          =   ""
--        	    ,   packageDeps         =   []
--                ,   pkgDescrFile        =   Nothing
--                ,   localPkgDescr       =   pd
--                ,   withPrograms        =   conf
--                ,   withPackageDB       =   GlobalPackageDB
--                ,   withVanillaLib      =   False
--                ,   withProfLib         =   False
--                ,   withSharedLib       =   False
--                ,   withProfExe         =   False
--                ,   withOptimization    =   False
--                ,   withGHCiLib         =   False
--        	    ,   splitObjs           =   False}

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


 ---------------------------------------------------------------------
--  | Parser function copied here, because it is not exported

--constructGHCCmdLine
--        :: LocalBuildInfo
--        -> BuildInfo
--        -> FilePath
--        -> Verbosity
--        -> [String]
--constructGHCCmdLine lbi bi odir verbosity =
--        ["--make"]
--     ++ ghcVerbosityOptions verbosity
--        -- Unsupported extensions have already been checked by configure
--     ++ ghcOptions lbi bi odir
--
--ghcVerbosityOptions :: Verbosity -> [String]
--ghcVerbosityOptions verbosity
--     | verbosity >= deafening = ["-v"]
--     | verbosity >= normal    = []
--     | otherwise              = ["-w", "-v0"]
--
--ghcOptions :: LocalBuildInfo -> BuildInfo -> FilePath -> [String]
--ghcOptions lbi bi odir
--     =  (if compilerVersion c > Version [6,4] []
--            then ["-hide-all-packages"]
--            else [])
--     ++ (if splitObjs lbi then ["-split-objs"] else [])
--     ++ ["-i"]
--     ++ ["-i" ++ autogenModulesDir lbi]
--     ++ ["-i" ++ odir]
--     ++ ["-i" ++ l | l <- nub (hsSourceDirs bi)]
--     ++ ["-I" ++ odir]
--     ++ ["-I" ++ dir | dir <- includeDirs bi]
--     ++ ["-optP" ++ opt | opt <- cppOptions bi]
--     ++ ["-optc" ++ opt | opt <- ccOptions bi]
--     ++ [ "-#include \"" ++ inc ++ "\"" | inc <- includes bi ]
--     ++ [ "-odir",  odir, "-hidir", odir ]
--     ++ (if compilerVersion c >= Version [6,8] []
--           then ["-stubdir", odir] else [])
--     ++ (concat [ ["-package", showPackageId pkg] | pkg <- packageDeps lbi ])
--     ++ (if withOptimization lbi then ["-O"] else [])
--     ++ hcOptions GHC (options bi)
--     ++ extensionsToFlags c (extensions bi)
--    where c = compiler lbi
