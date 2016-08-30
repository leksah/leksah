{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.FileUtils
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

module IDE.Utils.FileUtils (
    allModules
,   allHiFiles
,   allHaskellSourceFiles
,   isEmptyDirectory
,   cabalFileName
,   allCabalFiles
,   getConfigFilePathForLoad
,   hasSavedConfigFile
,   getConfigDir
,   getConfigFilePathForSave
,   getCollectorPath
,   getSysLibDir
,   moduleNameFromFilePath
,   moduleNameFromFilePath'
,   moduleCollectorFileName
,   findKnownPackages
,   isSubPath
,   findSourceFile
,   findSourceFile'
,   haskellSrcExts
,   getCabalUserPackageDir
,   autoExtractCabalTarFiles
,   autoExtractTarFiles
,   getInstalledPackages
,   findProjectRoot
,   getPackageDBs'
,   getPackageDBs
,   figureOutGhcOpts
,   figureOutHaddockOpts
,   allFilesWithExtensions
,   myCanonicalizePath
) where

import Control.Applicative
import Prelude hiding (readFile)
import System.FilePath
       (splitFileName, dropExtension, takeExtension, isDrive,
        combine, addExtension, (</>), normalise, splitPath, takeFileName,takeDirectory)
import Distribution.ModuleName (toFilePath, ModuleName)
import Control.Monad (when, foldM, filterM, forM)
import Data.Maybe (mapMaybe, catMaybes)
import Distribution.Simple.PreProcess.Unlit (unlit)
import System.Directory
       (getAppUserDataDirectory, canonicalizePath, doesDirectoryExist,
        doesFileExist, setCurrentDirectory, getCurrentDirectory,
        getDirectoryContents, createDirectory, getHomeDirectory)
import Text.ParserCombinators.Parsec.Language (haskellDef, haskell)
import qualified Text.ParserCombinators.Parsec.Token as P
       (GenTokenParser(..), TokenParser, identStart)
import Text.ParserCombinators.Parsec
       (GenParser, parse, oneOf, alphaNum, noneOf, char, try,
        (<?>), CharParser)
import Data.Set (Set)
import Data.List
       (intercalate, isPrefixOf, isSuffixOf, stripPrefix, nub)
import qualified Data.Set as  Set (empty, fromList)
import Distribution.Package (UnitId, PackageIdentifier)
import Data.Char (ord)
import Distribution.Text (simpleParse, display)

import IDE.Utils.Utils
import IDE.Core.CTypes(configDirName, ModuleKey(..))
import qualified Distribution.Text as  T (simpleParse)
import System.Log.Logger(errorM,warningM,debugM)
import IDE.Utils.Tool
import Control.Monad.IO.Class (MonadIO(..), MonadIO)
import Control.Exception as E (SomeException, catch)
import System.IO.Strict (readFile)
import qualified Data.Text as T
       (pack, stripPrefix, isSuffixOf, take, length, unpack, init,
        last, words, splitOn)
import Data.Monoid ((<>))
import Data.Text (Text)
import Control.DeepSeq (deepseq)
import IDE.Utils.VersionUtils (getDefaultGhcVersion)
import System.Environment (getEnvironment)
import Data.Aeson (eitherDecodeStrict')
import IDE.Utils.CabalPlan (PlanJson(..), PlanItem(..))
import IDE.Utils.CabalProject (findProjectRoot, projectPackages)
import qualified Data.ByteString as BS (readFile)

haskellSrcExts :: [FilePath]
haskellSrcExts = ["hs","lhs","chs","hs.pp","lhs.pp","chs.pp","hsc"]

-- | canonicalizePath without crashing
myCanonicalizePath :: FilePath -> IO FilePath
myCanonicalizePath fp = do
    exists <- doesFileExist fp
    if exists
        then canonicalizePath fp
        else return fp


-- | Returns True if the second path is a location which starts with the first path
isSubPath :: FilePath -> FilePath -> Bool
isSubPath fp1 fp2 =
    let fpn1    =   splitPath $ normalise fp1
        fpn2    =   splitPath $ normalise fp2
        res     =   isPrefixOf fpn1 fpn2
    in res

findSourceFile :: [FilePath]
    -> [FilePath]
    -> ModuleName
    -> IO (Maybe FilePath)
findSourceFile directories exts modId  =
    let modulePath      =   toFilePath modId
        allPathes       =   map (</> modulePath) directories
        allPossibles    =   concatMap (\ p -> map (addExtension p) exts)
                                allPathes
    in  find' allPossibles

findSourceFile' :: [FilePath]
    -> FilePath
    -> IO (Maybe FilePath)
findSourceFile' directories modulePath  =
    let allPathes       =   map (</> modulePath) directories
    in  find' allPathes


find' :: [FilePath] -> IO (Maybe FilePath)
find' []            =   return Nothing
find' (h:t)         =   E.catch (do
    exists <- doesFileExist h
    if exists
        then Just <$> canonicalizePath h
        else find' t)
        $ \ (_ :: SomeException) -> return Nothing

-- | The directory where config files reside
--
getConfigDir :: IO FilePath
getConfigDir = do
    d <- getHomeDirectory
    let filePath = d </> configDirName
    exists <- doesDirectoryExist filePath
    if exists
        then return filePath
        else do
            createDirectory filePath
            return filePath

getConfigDirForLoad :: IO (Maybe FilePath)
getConfigDirForLoad = do
    d <- getHomeDirectory
    let filePath = d </> configDirName
    exists <- doesDirectoryExist filePath
    if exists
        then return (Just filePath)
        else return Nothing

hasSavedConfigFile :: FilePath -> IO Bool
hasSavedConfigFile fn = do
    savedConfigFile <- getConfigFilePathForSave fn
    doesFileExist savedConfigFile


getConfigFilePathForLoad :: FilePath -> Maybe FilePath -> FilePath -> IO FilePath
getConfigFilePathForLoad fn mbFilePath dataDir = do
    mbCd <- case mbFilePath of
                Just p -> return (Just p)
                Nothing -> getConfigDirForLoad
    case mbCd of
        Nothing -> getFromData
        Just cd -> do
            ex <- doesFileExist (cd </> fn)
            if ex
                then return (cd </> fn)
                else getFromData
    where getFromData = do
            ex <- doesFileExist (dataDir </> "data" </> fn)
            if ex
                then return (dataDir </> "data" </> fn)
                else error $"Config file not found: " ++ fn

getConfigFilePathForSave :: FilePath -> IO FilePath
getConfigFilePathForSave fn = do
    cd <- getConfigDir
    return (cd </> fn)

allModules :: FilePath -> IO [ModuleName]
allModules filePath = E.catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            let filesAndDirs' = map (combine filePath)
                                    $filter (\s -> s /= "." && s /= ".." && s /= "_darcs" && s /= "dist" && s /= "dist-newstyle"
                                        && s /= "Setup.lhs") filesAndDirs
            dirs <-  filterM doesDirectoryExist filesAndDirs'
            files <-  filterM doesFileExist filesAndDirs'
            let hsFiles =   filter (\f -> let ext = takeExtension f in
                                            ext == ".hs" || ext == ".lhs") files
            mbModuleStrs <- mapM moduleNameFromFilePath hsFiles
            let mbModuleNames = mapMaybe
                                  (maybe Nothing (simpleParse . T.unpack))
                                  mbModuleStrs
            otherModules <- mapM allModules dirs
            return (mbModuleNames ++ concat otherModules)
        else return [])
            $ \ (_ :: SomeException) -> return []

allHiFiles :: FilePath -> IO [FilePath]
allHiFiles = allFilesWithExtensions [".hi"] True []

allCabalFiles :: FilePath -> IO [FilePath]
allCabalFiles = allFilesWithExtensions [".cabal"] False []

allHaskellSourceFiles :: FilePath -> IO [FilePath]
allHaskellSourceFiles = allFilesWithExtensions [".hs",".lhs"] True []

allFilesWithExtensions :: [FilePath] -> Bool -> [FilePath] -> FilePath -> IO [FilePath]
allFilesWithExtensions extensions recurseFurther collecting filePath = E.catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            let filesAndDirs' = map (combine filePath)
                                    $filter (\s -> s /= "." && s /= ".." && s /= "_darcs") filesAndDirs
            dirs    <-  filterM doesDirectoryExist filesAndDirs'
            files   <-  filterM doesFileExist filesAndDirs'
            let choosenFiles =   filter (\f -> let ext = takeExtension f in
                                                    elem ext extensions) files
            if recurseFurther || (not recurseFurther && null choosenFiles)
                then foldM (allFilesWithExtensions extensions recurseFurther) (choosenFiles ++ collecting) dirs
                else return (choosenFiles ++ collecting)
        else return collecting)
            $ \ (_ :: SomeException) -> return collecting


moduleNameFromFilePath :: FilePath -> IO (Maybe Text)
moduleNameFromFilePath fp = E.catch (do
    exists <- doesFileExist fp
    if exists
        then do
            str <-  readFile fp
            moduleNameFromFilePath' fp str
        else return Nothing)
            $ \ (_ :: SomeException) -> return Nothing

moduleNameFromFilePath' :: FilePath -> FilePath -> IO (Maybe Text)
moduleNameFromFilePath' fp str = do
    let unlitRes = if takeExtension fp == ".lhs"
                    then unlit fp str
                    else Left str
    case unlitRes of
        Right err -> do
            errorM "leksah-server" (show err)
            return Nothing
        Left str' -> do
            let parseRes = parse moduleNameParser fp str'
            case parseRes of
                Left _ -> return Nothing
                Right str'' -> return (Just str'')

-- | Get the file name to use for the module collector results
-- we want to store the file name for Main module since there can be several in one package
moduleCollectorFileName
    :: ModuleKey -- ^ The module key
    -> String -- ^ The name to use for the collector file (without extension)
moduleCollectorFileName (LibModule name) = display name
moduleCollectorFileName (MainModule sourcePath) =
    "Main_" ++ "_" ++ takeFileName (takeDirectory sourcePath) ++ dropExtension (takeFileName sourcePath)

lexer :: P.TokenParser st
lexer = haskell

lexeme :: CharParser st a -> CharParser st a
lexeme = P.lexeme lexer

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

symbol :: Text -> CharParser st Text
symbol = (T.pack <$>) . P.symbol lexer . T.unpack

moduleNameParser :: CharParser () Text
moduleNameParser = do
    whiteSpace
    many skipPreproc
    whiteSpace
    symbol "module"
    lexeme mident
    <?> "module identifier"

skipPreproc :: CharParser () ()
skipPreproc =
    try (do
        whiteSpace
        char '#'
        many (noneOf "\n")
        return ())
    <?> "preproc"

mident :: GenParser Char st Text
mident
        = do{ c <- P.identStart haskellDef
            ; cs <- many (alphaNum <|> oneOf "_'.")
            ; return (T.pack (c:cs))
            }
        <?> "midentifier"

findKnownPackages :: FilePath -> IO (Set Text)
findKnownPackages filePath = E.catch (do
    paths           <-  getDirectoryContents filePath
    let nameList    =   map (T.pack . dropExtension) $
            filter (\s -> leksahMetadataSystemFileExtension `isSuffixOf` s) paths
    return (Set.fromList nameList))
        $ \ (_ :: SomeException) -> return Set.empty

isEmptyDirectory :: FilePath -> IO Bool
isEmptyDirectory filePath = E.catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            return . null $ filter (not . ("." `isPrefixOf`) . takeFileName) filesAndDirs
        else return False)
        (\ (_ :: SomeException) -> return False)

cabalFileName :: FilePath -> IO (Maybe FilePath)
cabalFileName filePath = E.catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- map (filePath </>) <$> getDirectoryContents filePath
            files <-  filterM doesFileExist filesAndDirs
            case filter (\f -> let ext = takeExtension f in ext == ".cabal") files of
                [f] -> return (Just f)
                []  -> return Nothing
                _   -> do
                    warningM "leksah-server" "Multiple cabal files"
                    return Nothing
        else return Nothing)
        (\ (_ :: SomeException) -> return Nothing)

getCabalUserPackageDir :: IO (Maybe FilePath)
getCabalUserPackageDir = do
    (!output,_) <- runTool' "cabal" ["help"] Nothing Nothing
    output `deepseq` case T.stripPrefix "  " (toolline $ last output) of
        Just s | "config" `T.isSuffixOf` s -> return . Just . T.unpack $ T.take (T.length s - 6) s <> "packages"
        _ -> return Nothing

autoExtractCabalTarFiles :: FilePath -> IO ()
autoExtractCabalTarFiles filePath = do
    dir <- getCurrentDirectory
    autoExtractTarFiles' filePath
    setCurrentDirectory dir

autoExtractTarFiles :: FilePath -> IO ()
autoExtractTarFiles filePath = do
    dir <- getCurrentDirectory
    autoExtractTarFiles' filePath
    setCurrentDirectory dir

autoExtractTarFiles' :: FilePath -> IO ()
autoExtractTarFiles' filePath =
    E.catch (do
        exists <- doesDirectoryExist filePath
        when exists $ do
            filesAndDirs             <- getDirectoryContents filePath
            let filesAndDirs'        =  map (combine filePath)
                                            $ filter (\s -> s /= "." && s /= ".." && not ("00-index" `isPrefixOf` s)) filesAndDirs
            dirs                     <- filterM doesDirectoryExist filesAndDirs'
            files                    <- filterM doesFileExist filesAndDirs'
            let choosenFiles         =  filter (isSuffixOf ".tar.gz") files
            let decompressionTargets =  filter (\f -> (dropExtension . dropExtension) f `notElem` dirs) choosenFiles
            mapM_ (\f -> let (dir,fn) = splitFileName f
                             command = "tar -zxf " ++ fn in do
                                setCurrentDirectory dir
                                handle   <- runCommand command
                                waitForProcess handle
                                return ())
                    decompressionTargets
            mapM_ autoExtractTarFiles' dirs
            return ()
    ) $ \ (_ :: SomeException) -> return ()


getCollectorPath :: MonadIO m => m FilePath
getCollectorPath = liftIO $ do
    configDir <- getConfigDir
    let filePath = configDir </> "metadata"
    exists    <- doesDirectoryExist filePath
    if exists
        then return filePath
        else do
            createDirectory filePath
            return filePath

getSysLibDir :: FilePath -> IO FilePath
getSysLibDir ver = E.catch (do
    (!output,_) <- runTool' ("ghc-" ++ ver) ["--print-libdir"] Nothing Nothing
    let libDir = head [line | ToolOutput line <- output]
        libDir2 = if ord (T.last libDir) == 13
                    then T.init libDir
                    else libDir
    output `deepseq` return $ normalise $ T.unpack libDir2
    ) $ \ (e :: SomeException) -> error ("FileUtils>>getSysLibDir failed with " ++ show e)

getStackPackages :: FilePath -> IO [(UnitId, [FilePath])]
getStackPackages dir = do
    packageDBs <- getStackPackageDBs dir
    (!output', _) <- runTool' "stack" ["--stack-yaml", T.pack (dir </> "stack.yaml"), "exec", "ghc-pkg", "--", "list", "--simple-output"] Nothing Nothing
    output' `deepseq` return $ map (,packageDBs) $ concatMap ghcPkgOutputToPackages output'

ghcPkgOutputToPackages :: ToolOutput -> [UnitId]
ghcPkgOutputToPackages (ToolOutput n) = mapMaybe (T.simpleParse . T.unpack) (T.words n)
ghcPkgOutputToPackages _ = []

getCabalPackages :: FilePath -> FilePath -> IO [(UnitId, [FilePath])]
getCabalPackages ghcVer dir = do
    packageDBs <- getCabalPackageDBs ghcVer dir
    projectRoot <- findProjectRoot dir
    (eitherDecodeStrict' <$> BS.readFile (projectRoot </> "dist-newstyle" </> "cache" </> "plan.json"))
        >>= \ case
                Left _ -> return []
                Right plan -> return . map (,packageDBs) $
                    mapMaybe (T.simpleParse . T.unpack . piId) (pjPlan plan)


getPackages :: [FilePath] -> IO [(UnitId, [FilePath])]
getPackages packageDBs = do
    (!output', _) <- runTool' "ghc-pkg" (["list", "--simple-output"] ++ map (("--package-db"<>) . T.pack) packageDBs) Nothing Nothing
    output' `deepseq` return $ map (,packageDBs) $ concatMap ghcPkgOutputToPackages output'

-- | Find the packages that the packages in the workspace
getInstalledPackages :: FilePath -> [FilePath] -> IO [(UnitId, [FilePath])]
getInstalledPackages ghcVer dirs = do
    globalDBs <- sequence [getGlobalPackageDB ghcVer, getStorePackageDB ghcVer]
        >>= filterM doesDirectoryExist
    globalPackages <- E.catch (getPackages globalDBs) $ \ (_ :: SomeException) -> return []
    -- Do our best to get the stack package ids
    stackPackages <- forM dirs (\dir -> E.catch (do
            useStack <- liftIO . doesFileExist $ dir </> "stack.yaml"
            if useStack
                then getStackPackages dir
                else return []
        ) $ \ (_ :: SomeException) -> return [])
    return . nub $ concat (globalPackages : stackPackages)

getGlobalPackageDB :: FilePath -> IO FilePath
getGlobalPackageDB ghcVersion = do
    ghcLibDir <- getSysLibDir ghcVersion
    return $ ghcLibDir </> "package.conf.d"

getStorePackageDB :: FilePath -> IO FilePath
getStorePackageDB ghcVersion = do
    cabalDir <- getAppUserDataDirectory "cabal"
    return $ cabalDir </> "store" </> "ghc-" ++ ghcVersion </> "package.db"

getProjectPackageDB :: FilePath -> FilePath -> IO FilePath
getProjectPackageDB ghcVersion dir = do
    projectRoot <- findProjectRoot dir
    return $ projectRoot </> "dist-newstyle/packagedb" </> "ghc-" ++ ghcVersion

getCabalPackageDBs :: FilePath -> FilePath -> IO [FilePath]
getCabalPackageDBs ghcVersion dir =
    sequence [getGlobalPackageDB ghcVersion, getStorePackageDB ghcVersion, getProjectPackageDB ghcVersion dir]
        >>= filterM doesDirectoryExist

getStackPackageDBs :: FilePath -> IO [FilePath]
getStackPackageDBs dir = do
    (!output, _) <- runTool' "stack" ["--stack-yaml", T.pack (dir </> "stack.yaml"), "path", "--ghc-package-path"] Nothing Nothing
    filterM doesDirectoryExist $ concatMap paths output
  where
    paths (ToolOutput n) = map T.unpack (T.splitOn ":" n)
    paths _ = []

getPackageDBs' :: FilePath -> FilePath -> IO [FilePath]
getPackageDBs' ghcVersion dir =
    E.catch (do
        useStack <- liftIO . doesFileExist $ dir </> "stack.yaml"
        if useStack
            then getStackPackageDBs dir
            else getCabalPackageDBs ghcVersion dir
     ) $ \ (_ :: SomeException) -> return []
  where
    paths (ToolOutput n) = map T.unpack (T.splitOn ":" n)
    paths _ = []

getPackageDBs :: [FilePath] -> IO [[FilePath]]
getPackageDBs dirs = do
    ghcVersion <- getDefaultGhcVersion
    nub <$> forM dirs (getPackageDBs' ghcVersion)

figureOutHaddockOpts :: IO [Text]
figureOutHaddockOpts = do
    (!output,_) <- runTool' "cabal" ["haddock", "--with-haddock=leksahecho", "--executables"] Nothing Nothing
    let opts = concat [words $ T.unpack l | ToolOutput l <- output]
    let res = filterOptGhc opts
    debugM "leksah-server" ("figureOutHaddockOpts " ++ show res)
    output `deepseq` return $ map T.pack res
    where
        filterOptGhc :: [String] -> [String]
        filterOptGhc []    = []
        filterOptGhc (s:r) = case stripPrefix "--optghc=" s of
                                    Nothing -> filterOptGhc r
                                    Just s'  -> s' : filterOptGhc r

figureOutGhcOpts :: FilePath -> IO [Text]
figureOutGhcOpts dir = do
    debugM "leksah-server" "figureOutGhcOpts"
    ghcVersion <- getDefaultGhcVersion
    packageDBs <- liftIO $ getPackageDBs' ghcVersion dir
    flags <- doesFileExist (dir </> "base.cabal") >>= \case
        True -> return ["-finteger-gmp", "-finteger-gmp2"]
        False -> return []
    (!output,_) <- runTool' "cabal" ("configure" : flags <> map (("--package-db=" <>) . T.pack) packageDBs) Nothing Nothing
    output `deepseq` do
        (!output,_) <- runTool' "cabal" ["build","--with-ghc=leksahecho","--with-ghcjs=leksahecho"] Nothing Nothing
        let res = case catMaybes [findMake $ T.unpack l | ToolOutput l <- output] of
                    options:_ -> words options
                    _         -> []
        debugM "leksah-server" ("figureOutGhcOpts " ++ show res)
        output `deepseq` return $ map T.pack res
  where
    findMake :: String -> Maybe String
    findMake [] = Nothing
    findMake line@(_:xs) =
            case stripPrefix "--make " line of
                Nothing -> findMake xs
                s -> s
