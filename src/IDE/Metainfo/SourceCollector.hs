{-# OPTIONS_GHC -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.SourceCollector
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info at leksah.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- | This module collects information for packages with source available
--
-------------------------------------------------------------------------------

module IDE.Metainfo.SourceCollector (
    collectSources
,   buildSourceForPackageDB
,   sourceForPackage
,   parseSourceForPackageDB
,   getSourcesMap

,   unloadGhc
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
import Data.List (foldl',nub,delete,sort)
import Data.Maybe (catMaybes)
import qualified Control.Exception as C
import qualified Data.ByteString.Char8 as BS
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Control.Exception (SomeException)

import GHC hiding (idType)
import SrcLoc
import RdrName
import OccName
import DynFlags
--import PackageConfig hiding (exposedModules,package)
import FastString
import Outputable hiding (char)

import IDE.Core.State
import IDE.FileUtils
-- import IDE.Pane.Preferences
import Digraph (flattenSCCs)
import HscTypes (msHsFilePath)
import Distribution.Package (PackageIdentifier(..))
import Distribution.Text (display)
import GHC.Show (showSpace)


-- ---------------------------------------------------------------------
-- | Collect infos from sources for one package
--
collectSources :: Map PackageIdentifier [FilePath]
    -> PackageDescr
    -> Ghc (PackageDescr,Int)
collectSources sourceMap pdescr = do
    unloadGhc
    sysMessage Normal $ "Now collecting sources for " ++ display (packagePD pdescr)
    case sourceForPackage (packagePD pdescr) sourceMap of
        Nothing -> do
            sysMessage Normal $ "No source for package " ++ display (packagePD pdescr)
            return (pdescr,0)
        Just cabalPath' -> gcatch (do
            cabalPath       <-  liftIO $ canonicalizePath cabalPath'
            let basePath    =   takeDirectory cabalPath
            pkgDescr        <-  gcatch (liftIO $  (liftM flattenPackageDescription
                                        (readPackageDescription silent cabalPath)))
                                    (\(e :: SomeException) -> return emptyPackageDescription)
            let bis         =   allBuildInfo pkgDescr
            let buildPaths  =   if hasExes pkgDescr
                                then let exeName' = exeName (head (executables pkgDescr))
                                     in  map (\p -> basePath </> p) $
                                        ("dist" </> "build" </> exeName' </>
                                                    exeName' ++ "-tmp" </> "") :
                                        ("dist" </> "build" </> "autogen") : (".")
                                                : (nub $ concatMap hsSourceDirs bis)
                                else  map (\p -> basePath </> p) $
                                        ("dist" </> "build" </> "autogen") : (".")
                                                : (nub $ concatMap hsSourceDirs bis)
            let includes    =    map (\p -> basePath </> p)  $ nub $ concatMap includeDirs bis
            dflags             <-  getSessionDynFlags
            let dflags2 = dflags {
                topDir    = basePath,
                hscTarget = HscAsm,
                ghcMode   = CompManager,
                ghcLink   = NoLink}
            setSessionDynFlags dflags2
            let flags       =         ["-fglasgow-exts"]
                                    ++ ["-cpp"]
                                    ++ ["-I" ++ dir | dir <- includes]
                                    ++ ["-I" ++ dir | dir <- buildPaths]
                                    ++ ["-i" ++ dir | dir <- includes]
                                    ++ ["-i" ++ dir | dir <- buildPaths]
            -- trace ("flags " ++ show flags) $ return ()
            dflags3          <-  getSessionDynFlags
            (dflags4,_,_)   <-  parseDynamicFlags dflags3 (map noLoc flags)
            let dflags5     =   dopt_set dflags4 Opt_Haddock
            setSessionDynFlags dflags5
            defaultCleanupHandler dflags (do
                let allMods         =  nub (exeModules pkgDescr ++ libModules pkgDescr)
                let interestingMods =  map (modu . moduleIdMD) (exposedModulesPD pdescr)
                sourceFiles     <-  liftIO $ mapM (findSourceFile
                                                    buildPaths
                                                    ["hs","lhs","chs","hs.pp","lhs.pp","chs.pp"])
                                                    $ nub (interestingMods ++ allMods)
                liftIO $ mapM_ (\(mbSf, mn) ->
                    case mbSf of
                        Nothing -> putStrLn $ "Cant find source file for " ++ mn
                        Just _ ->  return ()) $ zip sourceFiles (map display allMods)
                (newModDescrs,failureCount) <- collectSourcesForPackage pdescr (catMaybes sourceFiles)
                                                    (map display interestingMods)
                let nPackDescr  =   pdescr{mbSourcePathPD = Just cabalPath,
                                           exposedModulesPD = newModDescrs}
                return (nPackDescr,failureCount)))
            (\(e :: SomeException) -> do
                sysMessage Normal $ "source collector throwIDE1 " ++ show e ++ " in " ++
                            display (packagePD pdescr) ++ " missed "
                                ++ show (length $ exposedModulesPD pdescr)
                return (pdescr,length $ exposedModulesPD pdescr))


-- ---------------------------------------------------------------------
-- | Collect infos from sources for modules
--
--------------------------

collectSourcesForPackage :: PackageDescr -> [String] -> [String] -> Ghc ([ModuleDescr],Int)
collectSourcesForPackage pkgDescr sourceFiles interesting = do
    targets <- mapM (\f -> guessTarget f Nothing) sourceFiles
    setTargets targets
    modgraph <- depanal [] False
    let orderedMods = flattenSCCs $ topSortModuleGraph False modgraph Nothing
    let tupels = catMaybes $ map (filterMods orderedMods) (exposedModulesPD pkgDescr)
    foldM collectSourcesForModule ([],0) tupels
        where
            moduleNameModSummary  =  moduleNameString . moduleName . ms_mod
            moduleNameModuleDescr = display . modu . moduleIdMD
            filterMods ordered mod =
                case filter (\om -> moduleNameModSummary om == moduleNameModuleDescr mod) ordered of
                    []    -> trace ("Missing source for module " ++ moduleNameModuleDescr mod) Nothing
                    (x:_) -> Just (mod,x)


collectSourcesForModule :: ([ModuleDescr],Int) -> (ModuleDescr, ModSummary) -> Ghc ([ModuleDescr],Int)
collectSourcesForModule (moduleDescrs,failureCount) (modD,modsum) = gcatch (do
    let filename            =  msHsFilePath modsum
    let dynflags            =  ms_hspp_opts modsum
    parsedMod               <- parseModule modsum
    let decls               = (hsmodDecls . unLoc . parsedSource) $ parsedMod
    let map'                =   convertToMap (idDescriptionsMD modD)
    let commentedDecls      =   addComments (filterSignatures decls)
    let (descrs,restMap)    =   foldl' collectParseInfoForDecl ([],map') commentedDecls
    let newMod              =   modD{
         idDescriptionsMD   =   reverse descrs ++ concat (Map.elems restMap),
         mbSourcePathMD     =   Just filename}
    return(newMod : moduleDescrs, failureCount))
        (\(e :: SomeException)                -> do
                    sysMessage Normal $ "source collector throwIDE2 " ++ show e ++ " in " ++
                                 msHsFilePath modsum
                    return (modD : moduleDescrs, failureCount + 1))
    where
        convertToMap :: [Descr] -> Map Symbol [Descr]
        convertToMap  list  =
         foldl' (\ st idDescr -> Map.insertWith (++) (descrName idDescr) [idDescr] st)
            Map.empty list
        convertFromMap :: Map Symbol [Descr]  -> [Descr]
        convertFromMap      =   concat . Map.elems


filterSignatures :: [LHsDecl RdrName] -> [LHsDecl RdrName]
filterSignatures declList = filter filterSignature declList
    where
    filterSignature (L srcDecl (SigD _)) = False
    filterSignature _ = True

addComments :: [LHsDecl RdrName] -> [(Maybe (LHsDecl RdrName), Maybe String)]
addComments = reverse . snd . foldl' addComment (Nothing,[])
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
        (Nothing,resultList)
    addComment (Nothing,resultList) decl = (Nothing,(Just decl,Nothing):resultList)
    addComment (Just comment,resultList) decl =  (Nothing,(Just decl,Just comment):resultList)

collectParseInfoForDecl ::  ([Descr],SymbolTable)
    -> (Maybe (LHsDecl RdrName),Maybe String)
    -> ([Descr],SymbolTable)
collectParseInfoForDecl (l,st) (Just (L loc _),_) | not (isGoodSrcSpan loc) = (l,st)
collectParseInfoForDecl (l,st) ((Just (L loc (ValD (FunBind lid _ _ _ _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Variable] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (TyData _ _ lid _ _ _ _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Data] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (TyFamily _ lid _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (TySynonym lid _ _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Type] []
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
    =   {--trace (declTypeToString (unLoc decl) ++ "--" ++ (showSDocUnqual $ppr decl))--} (l,st)
collectParseInfoForDecl (l,st) (Nothing, mbComment')    =
    {--trace ("Found comment " ++ show mbComment')--} (l,st)


addLocationAndComment :: ([Descr],SymbolTable)
    -> RdrName
    -> SrcSpan
    -> Maybe String
    -> [DescrType]
    -> [String]
    -> ([Descr],SymbolTable)
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
        Just identDescr |not (isReexported identDescr)    ->  (identDescr{
                mbLocation'  =   Just (srcSpanToLocation srcSpan),
                mbComment'   =   case mbComment' of
                                    Nothing -> Nothing
                                    Just s -> Just (BS.pack s)}
                                     : l, nst)
        otherwise           ->  (l,st)

    where
    matches :: Descr -> [DescrType] -> [String] -> Bool
    matches idDescr idTypes inst =
        case descrType (details idDescr) of
            Instance ->
                --trace ("instances " ++ show (sort (binds idDescr)) ++ " -- ?= -- " ++ show (sort inst)) $
                elem Instance idTypes && sort (binds (details idDescr)) == sort inst
            other   -> elem other idTypes

declTypeToString :: Show alpha => HsDecl alpha -> String
declTypeToString  (TyClD	_)   =   "TyClD"
declTypeToString  (InstD _)   =   "InstD"
declTypeToString  (DerivD _)  =   "DerivD"
declTypeToString  (ValD	_)    =   "ValD"
declTypeToString  (SigD _)    =   "SigD"
declTypeToString  (DefD _)    =   "DefD"
declTypeToString  (ForD _)    =   "ForD"
declTypeToString  (WarningD _)=  "WarnD"
declTypeToString  (RuleD _)   =   "RuleD"
declTypeToString  (SpliceD _) = "SpliceD"
declTypeToString  (DocD v)    =   "DocD " ++ show v

srcSpanToLocation :: SrcSpan -> Location
srcSpanToLocation span | not (isGoodSrcSpan span)
    =   throwIDE "srcSpanToLocation: unhelpful span"
srcSpanToLocation span
    =   Location (srcSpanStartLine span) (srcSpanStartCol span)
                 (srcSpanEndLine span) (srcSpanEndCol span)

printHsDoc :: Show alpha => HsDoc alpha  -> String
printHsDoc d = show (PPDoc d)

newtype PPDoc alpha = PPDoc (HsDoc alpha)

instance Show alpha => Show (PPDoc alpha)  where
    showsPrec _ (PPDoc DocEmpty)                 =   id
    showsPrec _ (PPDoc (DocAppend l r))          =   shows (PPDoc l)  . shows (PPDoc r)
    showsPrec _ (PPDoc (DocString str))          =   showString str
    showsPrec _ (PPDoc (DocParagraph d))         =   shows (PPDoc d) . showChar '\n'
    showsPrec _ (PPDoc (DocIdentifier l))        =   foldr (\i f -> showChar '\'' . shows i . showChar '\'') id l
    showsPrec _ (PPDoc (DocModule str))          =   showChar '"' . showString str . showChar '"'
    showsPrec _ (PPDoc (DocEmphasis doc))        =   showChar '/' . shows (PPDoc doc)  . showChar '/'
    showsPrec _ (PPDoc (DocMonospaced doc))      =   showChar '@' . shows (PPDoc doc) . showChar '@'
    showsPrec _ (PPDoc (DocUnorderedList l))     =
        foldr (\s r -> showString "* " . shows (PPDoc s) . showChar '\n' . r) id l
    showsPrec _ (PPDoc (DocOrderedList l))       =
        foldr (\(i,n) f -> shows n . showSpace .  shows (PPDoc i)) id (zip l [1 .. length l])
    showsPrec _ (PPDoc (DocDefList li))          =
        foldr (\(l,r) f -> showString "[@" . shows (PPDoc l) . showString "[@ " . shows (PPDoc r) . f) id li
    showsPrec _ (PPDoc (DocCodeBlock doc))      =   showChar '@' . shows (PPDoc doc) . showChar '@'
    showsPrec _ (PPDoc (DocURL str))            =   showChar '<' . showString str . showChar '>'
    showsPrec _ (PPDoc (DocAName str))          =   showChar '#' . showString str . showChar '#'
    showsPrec _ (PPDoc _)                       =   id


{--
printHsDoc :: Show alpha => HsDoc alpha  -> String
printHsDoc = (unlines . map ((++) "-- ") .  nonemptyLines) . printHsDoc'
    where
        printHsDoc' DocEmpty                     =   ""
        printHsDoc' (DocAppend l r)              =   printHsDoc' l ++ printHsDoc' r
        printHsDoc' (DocString str)              =   text str
        printHsDoc' (DocParagraph d)             =   printHsDoc' d ++ "\n"
        printHsDoc' (DocIdentifier l)            =   concatMap (\i -> '\'' : show l ++ "'")  l
        printHsDoc' (DocModule str)              =   '"' : str ++ "\""
        printHsDoc' (DocEmphasis doc)            =   '/' : printHsDoc' doc ++ "/"
        printHsDoc' (DocMonospaced doc)          =   '@' ++ printHsDoc' doc ++ "@"
        printHsDoc' (DocUnorderedList l)         =   concatMap (\i -> "* " ++ printHsDoc' i) l
        printHsDoc' (DocOrderedList l)           =   concatMap (\(i,n) -> show n ++ " " ++ printHsDoc' i)
                                                                    $ zip l [1 .. length l]
        printHsDoc' (DocDefList li)              =   concatMap (\(l,r) -> printHsDoc' l ++ " " ++
                                                        printHsDoc' r) li
        printHsDoc' (DocCodeBlock doc)           =   "@ " ++  printHsDoc' doc ++ " @"
        printHsDoc' (DocURL str)                 =   '<' : str ++ ">"
        printHsDoc' (DocAName str)               =   '#' : str ++ "#"
        printHsDoc' _                            =   ""
--}


instance Show RdrName where
    show                                =   unpackFS . occNameFS . rdrNameOcc

instance Show alpha => Show (DocDecl alpha) where
        show  (DocCommentNext doc)      =       "DocCommentNext " ++ show doc
        show  (DocCommentPrev doc)      =       "DocCommentPrev " ++ show doc
        show  (DocCommentNamed str doc) =       "DocCommentNamed" ++ " " ++ str ++ " " ++ show doc
        show  (DocGroup i doc)          =       "DocGroup" ++ " " ++ show i ++ " " ++ show doc

unloadGhc :: Ghc ()
unloadGhc = do
   setTargets []
   load LoadAllTargets
   return ()

-- ---------------------------------------------------------------------
-- Function to map packages to file paths
--

getSourcesMap :: Prefs -> IO (Map PackageIdentifier [FilePath])
getSourcesMap prefs = do
        mbSources <- parseSourceForPackageDB
        case mbSources of
            Just map -> return map
            Nothing -> do
                buildSourceForPackageDB prefs
                mbSources <- parseSourceForPackageDB
                case mbSources of
                    Just map -> do
                        return map
                    Nothing ->  throwIDE "can't build/open source for package file"

sourceForPackage :: PackageIdentifier
    -> (Map PackageIdentifier [FilePath])
    -> Maybe FilePath
sourceForPackage id map =
    case id `Map.lookup` map of
        Just (h:_)  ->  Just h
        _           ->  Nothing

buildSourceForPackageDB :: Prefs -> IO ()
buildSourceForPackageDB prefs = do
    case autoExtractTars prefs of
        Nothing     -> return ()
        Just path   -> do
            dir <- getCurrentDirectory
            autoExtractTarFiles path
            setCurrentDirectory dir
    let dirs        =   sourceDirectories prefs
    cabalFiles      <-  mapM allCabalFiles dirs
    fCabalFiles     <-  mapM canonicalizePath $ concat cabalFiles
    mbPackages      <-  mapM (\fp -> parseCabal fp) fCabalFiles
    let pdToFiles   =   Map.fromListWith (++)
                $ map (\(Just p,o ) -> (p,o))
                    $ filter (\(mb, _) -> case mb of
                                            Nothing -> False
                                            _       -> True )
                        $ zip mbPackages (map (\a -> [a]) fCabalFiles)
    filePath        <-  getConfigFilePathForSave "source_packages.txt"
    writeFile filePath  (PP.render (showSourceForPackageDB pdToFiles))

showSourceForPackageDB  :: Map String [FilePath] -> PP.Doc
showSourceForPackageDB aMap = PP.vcat (map showIt (Map.toList aMap))
    where
    showIt :: (String,[FilePath]) -> PP.Doc
    showIt (pd,list) =  (foldl' (\l n -> l PP.$$ (PP.text $ show n)) label list)
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
                    sysMessage Normal $"Error reading source packages file "
                            ++ filePath ++ " " ++ show pe
                    return Nothing
                Right r ->  return (Just r)
        else do
            sysMessage Normal $"No source packages file found: " ++ filePath
            return Nothing

---- Returns the package name as a string (e.g. ohohoh-0.1.0)
--parseCabal :: FilePath -> IO (Maybe String)
--parseCabal cabalPath = gcatch (do
--    gpd <- readPackageDescription silent cabalPath
--    return (Just ((display . package . packageDescription) gpd)))
--        $ \ (e :: SomeException) -> do
--            sysMessage Normal ("SourceCollector>>parseCabal:Can't parse cabal " ++ show e)
--            return Nothing
--
-- ---------------------------------------------------------------------
-- | Parser for Package DB
--
packageStyle  :: P.LanguageDef st
packageStyle  = emptyDef
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                }

lexer       =   P.makeTokenParser packageStyle
whiteSpace  =   P.whiteSpace lexer
symbol      =   P.symbol lexer

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

parseCabal :: FilePath -> IO (Maybe String)
parseCabal fn = do
    --putStrLn $ "Now parsing minimal " ++ fn
    res     <-  parseFromFile cabalMinimalParser fn
    case res of
        Left pe ->  do
            sysMessage Normal $"Error reading cabal file " ++ show fn ++ " " ++ show pe
            return Nothing
        Right r ->  do
            sysMessage Normal r
            return (Just r)

cabalMinimalParser :: CharParser () String
cabalMinimalParser = do
    r1 <- cabalMinimalP
    r2 <- cabalMinimalP
    case r1 of
        Left v -> do
            case r2 of
                Right n -> return (n ++ "-" ++ v)
                Left _ -> unexpected "Illegal cabal"
        Right n -> do
            case r2 of
                Left v -> return (n ++ "-" ++ v)
                Right _ -> unexpected "Illegal cabal"

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
