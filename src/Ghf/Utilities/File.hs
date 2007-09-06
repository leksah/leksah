module Ghf.Utilities.File (
    allModules
,   cabalFileName
) where

import System.FilePath
import System.Directory
import Text.ParserCombinators.Parsec hiding (Parser)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(haskell,haskellDef)
import Data.Maybe (catMaybes)
import Control.Monad(filterM)
import Distribution.PreProcess.Unlit
import Debug.Trace

allModules :: FilePath -> IO [String]
allModules filePath = do 
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            putStrLn $show filesAndDirs
            let filesAndDirs' = map (\s -> combine filePath s)
                                    $filter (\s -> s /= "." && s /= ".." && s /= "_darcs"
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

moduleNameFromFilePath :: FilePath -> IO (Maybe String)
moduleNameFromFilePath fp = do
    exists <- doesFileExist fp
    if exists
        then do
            str <- readFile fp
            let str' = if takeExtension fp == ".lhs" 
                            then trace "unlit" (unlit fp str)
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
    symbol "module"
    str <- lexeme mident
    skipMany anyChar
    eof
    return str
    <?> "module identifier" 

mident           
        = do{ c <- P.identStart haskellDef
            ; cs <- many (alphaNum <|> oneOf "_'.") 
            ; return (c:cs)
            }
        <?> "midentifier"
    
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
                
    




