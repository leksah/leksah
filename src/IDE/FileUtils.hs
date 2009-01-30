module IDE.FileUtils (
    allModules
,   allHiFiles
,   allHaskellSourceFiles
,   cabalFileName
,   allCabalFiles
,   getConfigFilePathForLoad
,   hasConfigDir
,   getConfigDir
,   getConfigFilePathForSave
,   getCollectorPath
,   getSysLibDir
,   moduleNameFromFilePath
,   findKnownPackages
,   isSubPath
,   findSourceFile
,   autoExtractTarFiles

) where

import System.FilePath
import System.Directory
import System.IO
import System.Process
import Text.ParserCombinators.Parsec hiding (Parser)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(haskell,haskellDef)
import Data.Maybe (catMaybes)
import Distribution.Simple.PreProcess.Unlit
import Control.Monad
import Control.Monad.Trans(MonadIO,liftIO)
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (isSuffixOf,isPrefixOf)
import Distribution.ModuleName(ModuleName,toFilePath)
import Distribution.Text(simpleParse)
import Debug.Trace

import Paths_leksah
import IDE.Core.State
import Data.Char (ord)

-- | Returns True if the second path is a location which starts with the first path
isSubPath :: FilePath -> FilePath -> Bool
isSubPath fp1 fp2 =
    let fpn1    =   splitPath $ normalise fp1
        fpn2    =   splitPath $ normalise fp2
        res     =   isPrefixOf fpn1 fpn2
    in res

findSourceFile :: [FilePath]
    -> [String]
    -> ModuleName
    -> IO (Maybe FilePath)
findSourceFile directories exts modId  =
    let modulePath      =   toFilePath modId
        allPathes       =   map (\ d -> d </> modulePath) directories
        allPossibles    =   concatMap (\ p -> map (addExtension p) exts)
                                allPathes
    in  find' allPossibles

find' :: [FilePath] -> IO (Maybe FilePath)
find' []            =   return Nothing
find' (h:t)         =   catch (do
    exists <- doesFileExist h
    if exists
        then return (Just h)
        else find' t)
        $ \ _ -> return Nothing

dots_to_slashes = map (\c -> if c == '.' then pathSeparator else c)

-- The directory where config files reside
--
getConfigDir :: IO FilePath
getConfigDir = do
    d <- getHomeDirectory
    let filePath = d </> ".leksah"
    exists <- doesDirectoryExist filePath
    if exists
        then return filePath
        else do
            createDirectory filePath
            return filePath

hasConfigDir :: IO Bool
hasConfigDir = do
    d <- getHomeDirectory
    let filePath = d </> ".leksah"
    doesDirectoryExist filePath


getConfigFilePathForLoad :: String -> IO FilePath
getConfigFilePathForLoad fn = do
    cd <- getConfigDir
    ex <- doesFileExist (cd </> fn)
    if ex
        then return (cd </> fn)
        else do
            dd <- getDataDir
            ex <- doesFileExist (dd </> "data" </> fn)
            if ex
                then return (dd </> "data" </> fn)
                else throwIDE $"Config file not found: " ++ fn

getConfigFilePathForSave :: String -> IO FilePath
getConfigFilePathForSave fn = do
    cd <- getConfigDir
    return (cd </> fn)

allModules :: FilePath -> IO [ModuleName]
allModules filePath = catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            let filesAndDirs' = map (\s -> combine filePath s)
                                    $filter (\s -> s /= "." && s /= ".." && s /= "_darcs" && s /= "dist"
                                        && s /= "Setup.lhs") filesAndDirs
            dirs <-  filterM (\f -> doesDirectoryExist f) filesAndDirs'
            files <-  filterM (\f -> doesFileExist f) filesAndDirs'
            let hsFiles =   filter (\f -> let ext = takeExtension f in
                                            ext == ".hs" || ext == ".lhs") files
            mbModuleNames <- mapM moduleNameFromFilePath hsFiles
            otherModules <- mapM allModules dirs
            return (catMaybes mbModuleNames ++ concat otherModules)
        else return [])
            $ \ _ -> return []

allHiFiles :: FilePath -> IO [FilePath]
allHiFiles = allFilesWithExtensions [".hi"] True []

allCabalFiles :: FilePath -> IO [FilePath]
allCabalFiles = allFilesWithExtensions [".cabal"] False []

allHaskellSourceFiles :: FilePath -> IO [FilePath]
allHaskellSourceFiles = allFilesWithExtensions [".hs",".lhs"] True []

allFilesWithExtensions :: [String] -> Bool -> [FilePath] -> FilePath -> IO [FilePath]
allFilesWithExtensions extensions recurseFurther collecting filePath = catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            let filesAndDirs' = map (\s -> combine filePath s)
                                    $filter (\s -> s /= "." && s /= ".." && s /= "_darcs") filesAndDirs
            dirs    <-  filterM (\f -> doesDirectoryExist f) filesAndDirs'
            files   <-  filterM (\f -> doesFileExist f) filesAndDirs'
            let choosenFiles =   filter (\f -> let ext = takeExtension f in
                                                    List.elem ext extensions) files
            allFiles <-
                if recurseFurther || (not recurseFurther && null choosenFiles)
                    then foldM (allFilesWithExtensions extensions recurseFurther) (choosenFiles ++ collecting) dirs
                    else return (choosenFiles ++ collecting)
            return (allFiles)
        else return collecting)
            $ \ _ -> return collecting


moduleNameFromFilePath :: FilePath -> IO (Maybe ModuleName)
moduleNameFromFilePath fp = catch (do
    exists <- doesFileExist fp
    if exists
        then do
            str <- readFile fp
            let unlitRes = if takeExtension fp == ".lhs"
                            then unlit fp str
                            else Left str
            case unlitRes of
                Right err -> do
                    sysMessage Normal $show err
                    return Nothing
                Left str' -> do
                    let parseRes = parse moduleNameParser fp str'
                    case parseRes of
                        Left err -> do
                            sysMessage Normal $show err
                            return Nothing
                        Right str -> do
                            let res = simpleParse str
                            case res of
                                Nothing -> do
                                    sysMessage Normal $ "FileUtils>>moduleNameFromFilePath " ++
                                        "Can't parse module name " ++ str
                                    return Nothing
                                Just mn -> return (Just mn)
        else return Nothing)
            $ \ _ -> return Nothing

lexer = haskell
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
hexadecimal = P.hexadecimal lexer
symbol = P.symbol lexer

moduleNameParser :: CharParser () String
moduleNameParser = do
    whiteSpace
    many skipPreproc
    whiteSpace
    symbol "module"
    str <- lexeme mident
--    skipMany anyChar
--    eof
    return str
    <?> "module identifier"

skipPreproc :: CharParser () ()
skipPreproc = do
    try (do
        whiteSpace
        char '#'
        many (noneOf "\n")
        return ())
    <?> "preproc"

mident
        = do{ c <- P.identStart haskellDef
            ; cs <- many (alphaNum <|> oneOf "_'.")
            ; return (c:cs)
            }
        <?> "midentifier"

findKnownPackages :: FilePath -> IO (Set String)
findKnownPackages filePath = catch (do
    paths           <-  getDirectoryContents filePath
    let nameList    =   map dropExtension  $filter (\s -> ".pack" `isSuffixOf` s) paths
    return (Set.fromList nameList))
        $ \ _ -> return (Set.empty)

cabalFileName :: FilePath -> IO (Maybe String)
cabalFileName filePath = catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            files <-  filterM (\f -> doesFileExist f) filesAndDirs
            let cabalFiles =   filter (\f -> let ext = takeExtension f in ext == ".cabal") files
            if null cabalFiles
                then return Nothing
                else if length cabalFiles == 1
                    then return (Just $head cabalFiles)
                    else do
                        sysMessage Normal "Multiple cabal files"
                        return Nothing
        else return Nothing)
        (\_ -> return Nothing)

autoExtractTarFiles :: FilePath -> IO ()
autoExtractTarFiles filePath =
    catch (do
        exists <- doesDirectoryExist filePath
        if exists
            then do
                filesAndDirs             <- getDirectoryContents filePath
                let filesAndDirs'        =  map (\s -> combine filePath s)
                                                $ filter (\s -> s /= "." && s /= ".." && not (isPrefixOf "00-index" s)) filesAndDirs
                dirs                     <- filterM (\f -> doesDirectoryExist f) filesAndDirs'
                files                    <- filterM (\f -> doesFileExist f) filesAndDirs'
                let choosenFiles         =  filter (\f -> isSuffixOf ".tar.gz" f) files
                let decompressionTargets =  filter (\f -> (dropExtension . dropExtension) f `notElem` dirs) choosenFiles
                mapM_ (\f -> let (dir,fn) = splitFileName f in do
                                setCurrentDirectory dir
                                handle   <- runCommand $ "tar -zxf " ++ fn
                                waitForProcess handle
                                trace ("extracted " ++ fn) $ return ())
                        decompressionTargets
                mapM_ autoExtractTarFiles dirs
                return ()
            else return ()
    ) $ \ _ -> trace ("error extractTarFiles" ++ filePath) $ return ()


getCollectorPath :: MonadIO m => String -> m FilePath
getCollectorPath version = liftIO $ do
    configDir <- getConfigDir
    let filePath = configDir </> "ghc-" ++ version
    exists <- doesDirectoryExist filePath
    if exists
        then return filePath
        else do
            createDirectory filePath
            return filePath

getSysLibDir :: IO FilePath
getSysLibDir = do
    (_, out, _, pid) <- runInteractiveProcess "ghc" ["--print-libdir"] Nothing Nothing
    libDir <- hGetLine out
    let libDir2 = if ord (last libDir) == 13
                    then List.init libDir
                    else libDir
    return (normalise libDir2)




