module IDE.Utils.File (
    allModules
,   allHiFiles
,   allHaskellSourceFiles
,   cabalFileName
,   allCabalFiles
,   getConfigFilePathForLoad
,   getConfigFilePathForSave
,   getCollectorPath
,   getSysLibDir
,   moduleNameFromFilePath
,   findKnownPackages
,   isSubPath

,   readOut
,   readErr
,   runExternal
,   findSourceFile

) where

import System.FilePath
import System.Directory
import System.IO
import Data.Char
import System.Process
import Text.ParserCombinators.Parsec hiding (Parser)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(haskell,haskellDef)
import Data.Maybe (catMaybes)
import Distribution.Simple.PreProcess.Unlit
--import Debug.Trace
import Control.Monad
import qualified Data.List as List
import Paths_leksah
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List(isSuffixOf, isPrefixOf)

import IDE.Core.State
import {-# SOURCE #-} IDE.Log

-- | Returns True if the second path is a location which starts with the first path
isSubPath :: FilePath -> FilePath -> Bool
isSubPath fp1 fp2 =
    let fpn1    =   splitPath $ normalise fp1
        fpn2    =   splitPath $ normalise fp2
    in isPrefixOf fpn1 fpn2

findSourceFile :: [FilePath]
    -> [String]
    -> ModuleIdentifier
    -> IO (Maybe FilePath)
findSourceFile directories exts modId  =
    let modulePath      =   dots_to_slashes modId
        allPathes       =   map (\ d -> d </> modulePath) directories
        allPossibles    =   concatMap (\ p -> map (addExtension p) exts)
                                allPathes
    in  find' allPossibles

find' :: [FilePath] -> IO (Maybe FilePath)
find' []            =   return Nothing
find' (h:t)         =   do
    exists <- doesFileExist h
    if exists
        then return (Just h)
        else find' t

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

getConfigFilePathForLoad :: String -> IO FilePath
getConfigFilePathForLoad fn = do
    cd <- getConfigDir
    ex <- doesFileExist (cd </> fn)
    if ex
        then return (cd </> fn)
        else do
            dd <- getDataDir
            return (dd </> "data" </> fn)
--            ex <- doesFileExist (dd </> fn)
--            if ex
--                then return (dd </> fn)
--                else error $"Config file not found: " ++ fn

getConfigFilePathForSave :: String -> IO FilePath
getConfigFilePathForSave fn = do
    cd <- getConfigDir
    return (cd </> fn)

allModules :: FilePath -> IO [String]
allModules filePath = do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            putStrLn $show filesAndDirs
            let filesAndDirs' = map (\s -> combine filePath s)
                                    $filter (\s -> s /= "." && s /= ".." && s /= "_darcs" && s /= "dist"
                                        && s /= "Setup.lhs") filesAndDirs
            putStrLn $show filesAndDirs'
            dirs <-  filterM (\f -> doesDirectoryExist f) filesAndDirs'
            files <-  filterM (\f -> doesFileExist f) filesAndDirs'
            let hsFiles =   filter (\f -> let ext = takeExtension f in
                                            ext == ".hs" || ext == ".lhs") files
            mbModuleNames <- mapM moduleNameFromFilePath hsFiles
            otherModules <- mapM allModules dirs
            return (catMaybes mbModuleNames ++ concat otherModules)
        else return []

allHiFiles :: FilePath -> IO [FilePath]
allHiFiles = allFilesWithExtensions [".hi"] True

allCabalFiles :: FilePath -> IO [FilePath]
allCabalFiles = allFilesWithExtensions [".cabal"] False

allHaskellSourceFiles :: FilePath -> IO [FilePath]
allHaskellSourceFiles = allFilesWithExtensions [".hs",".lhs"] True

allFilesWithExtensions :: [String] -> Bool -> FilePath -> IO [FilePath]
allFilesWithExtensions extensions recurseFurther filePath = do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            --putStrLn $show filesAndDirs
            let filesAndDirs' = map (\s -> combine filePath s)
                                    $filter (\s -> s /= "." && s /= ".." && s /= "_darcs") filesAndDirs
            --putStrLn $show filesAndDirs'
            dirs    <-  filterM (\f -> doesDirectoryExist f) filesAndDirs'
            files   <-  filterM (\f -> doesFileExist f) filesAndDirs'
            let choosenFiles =   filter (\f -> let ext = takeExtension f in
                                                    List.elem ext extensions) files
            otherFiles <-
                if recurseFurther || (not recurseFurther && null choosenFiles)
                    then mapM (allFilesWithExtensions extensions recurseFurther) dirs
                    else return []
            return (choosenFiles ++ concat otherFiles)
        else return []



moduleNameFromFilePath :: FilePath -> IO (Maybe String)
moduleNameFromFilePath fp = do
    exists <- doesFileExist fp
    if exists
        then do
            str <- readFile fp
            let str' = if takeExtension fp == ".lhs"
                            then unlit fp str
                            else str
            let parseRes = parse moduleNameParser fp str'
            case parseRes of
                Left err -> do
                    putStrLn $show err
                    return Nothing
                Right str -> do
                    return (Just str)
        else return Nothing

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
findKnownPackages filePath = do
    paths           <-  getDirectoryContents filePath
    let nameList    =   map dropExtension  $filter (\s -> ".pack" `isSuffixOf` s) paths
    return (Set.fromList nameList)

cabalFileName :: FilePath -> IO (Maybe String)
cabalFileName filePath = do
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
                        putStrLn "Multiple cabal files"
                        return Nothing
        else return Nothing

getCollectorPath :: String -> IO FilePath
getCollectorPath version = do
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
                    then take (length libDir - 1) libDir
                    else libDir
    waitForProcess pid
    return (normalise libDir2)

-- Spawning external processes

readOut :: IDELog -> Handle -> IO ()
readOut log hndl =
     catch (readAndShow)
       (\e -> do
        --appendLog log ("----------------------------------------\n") FrameTag
        hClose hndl
        return ())
    where
    readAndShow = do
        line <- hGetLine hndl
        appendLog log (line ++ "\n") LogTag
        readAndShow

readErr :: IDELog -> Handle -> IO ()
readErr log hndl =
     catch (readAndShow)
       (\e -> do
        hClose hndl
        return ())
    where
    readAndShow = do
        line <- hGetLine hndl
        appendLog log (line ++ "\n") ErrorTag
        readAndShow

runExternal :: FilePath -> [String] -> IO (Handle, Handle, Handle, ProcessHandle)
runExternal path args = do
    hndls@(inp, out, err, _) <- runInteractiveProcess path args Nothing Nothing
    putStrLn $ "Starting external tool: " ++ path ++ " with args " ++ (show args)
    hSetBuffering out NoBuffering
    hSetBuffering err NoBuffering
    hSetBuffering inp NoBuffering
    hSetBinaryMode out True
    hSetBinaryMode err True
    return hndls





