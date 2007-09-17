--
-- | The packages methods of ghf.
--

module Ghf.Package (
    packageConfig
,   packageBuild
,   packageDoc
,   packageClean
,   packageCopy
,   packageRun

,   packageInstall
,   packageRegister
,   packageUnregister
,   packageTest
,   packageSdist
,   packageOpenDoc
) where

import Graphics.UI.Gtk
import Control.Monad.Reader
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Program
import Distribution.Setup
import System.FilePath
import Control.Concurrent
import Control.Exception hiding(try)
import System.Directory
import System.IO
import System.Process
import Prelude hiding (catch)
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec hiding(Parser)
import Control.Monad.Reader


import Ghf.Log
import Ghf.Core
import Ghf.PackageEditor


getActivePackage :: GhfM (Maybe GhfPackage)
getActivePackage = do
    active <- readGhf activePack
    case active of
        Just p -> return (Just p)
        Nothing -> selectActivePackage

selectActivePackage :: GhfM (Maybe GhfPackage)
selectActivePackage = do
    window  <- readGhf window
    mbFilePath <- lift $choosePackageFile window
    case mbFilePath of
        Nothing -> return Nothing
        Just filePath -> do
            let flags = emptyConfigFlags defaultProgramConfiguration
            packageD <- lift $readPackageDescription filePath
            let pack = GhfPackage (package packageD) filePath [] [] [] [] [] [] [] []
            modifyGhf_ (\ghf -> return (ghf{activePack = (Just pack)}))
            lift $putStrLn $"Set current directory " ++ dropFileName filePath
            lift $setCurrentDirectory $dropFileName filePath
            return (Just pack)

packageConfig :: Bool -> GhfAction
packageConfig force = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","configure"] ++ (configFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageBuild :: Bool -> GhfAction
packageBuild force = do
    mbPackage   <- getActivePackage
    log         <- getLog
    ghfR        <- ask
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","build"] ++ (buildFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (runReaderT (readErrForBuild log err) ghfR)
            return ()

packageDoc :: GhfAction
packageDoc = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","haddock"] ++ (haddockFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageClean :: GhfAction
packageClean = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","clean"] ++ (haddockFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageCopy :: GhfAction
packageCopy = do
    mbPackage   <- getActivePackage
    log         <- getLog
    mbDir       <- chooseDir "Select the target directory"
    case mbDir of
        Nothing -> return ()
        Just fp ->
            case mbPackage of
                Nothing         -> return ()
                Just package    -> lift $do
                    (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","copy"] ++ ["--destdir=" ++ fp])
                    oid <- forkIO (readOut log out)
                    eid <- forkIO (readErr log err)
                    return ()

packageRun :: GhfAction
packageRun = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            pd <- readPackageDescription (cabalFile package)
            case executables pd of
                [(Executable name _ _)] -> do
                    let path = "dist/build" </> pkgName (packageId package) </> name
                    (inp,out,err,pid) <- runExternal path (exeFlags package)
                    oid <- forkIO (readOut log out)
                    eid <- forkIO (readErr log err)
                    return ()
                otherwise -> do
                    putStrLn "no single executable in selected package"
                    return ()

packageInstall :: GhfAction
packageInstall = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","install"] ++ (installFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageRegister :: GhfAction
packageRegister = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","register"] ++ (registerFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageUnregister :: GhfAction
packageUnregister = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","unregister"] ++ (unregisterFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageTest :: GhfAction
packageTest = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","test"])
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageSdist :: GhfAction
packageSdist = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","sdist"] ++ (sdistFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageOpenDoc :: GhfAction
packageOpenDoc = do
    mbPackage   <- getActivePackage
    prefs       <- readGhf prefs
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            let path = "dist/doc/html" </> pkgName (packageId package) </> "index.html"
            (inp,out,err,pid) <- runExternal (browser prefs) [path]
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

chooseDir :: String -> GhfM (Maybe FilePath)
chooseDir str = do
    win <- readGhf window
    lift $do
        dialog <- fileChooserDialogNew
                        (Just $ str)
                        (Just win)
                    FileChooserActionSelectFolder
                    [("gtk-cancel"
                    ,ResponseCancel)
                    ,("gtk-open"
                    ,ResponseAccept)]
        widgetShow dialog
        response <- dialogRun dialog
        case response of
            ResponseAccept -> do
                fn <- fileChooserGetFilename dialog
                widgetDestroy dialog
                return fn
            ResponseCancel -> do
                widgetDestroy dialog
                return Nothing
            ResponseDeleteEvent -> do
                widgetDestroy dialog
                return Nothing

readOut :: GhfLog -> Handle -> IO ()
readOut log hndl =
     catch (readAndShow)
       (\e -> do
        appendLog log ("----------------------------------------\n") FrameTag
        hClose hndl
        return ())
    where
    readAndShow = do
        line <- hGetLine hndl
        appendLog log (line ++ "\n") LogTag
        readAndShow

readErr :: GhfLog -> Handle -> IO ()
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

readErrForBuild :: GhfLog -> Handle -> GhfAction
readErrForBuild log hndl = do
    errs <- lift $readAndShow False []
    lift $putStrLn $"Errors " ++ (show errs)
    modifyGhf_ (\ghf -> return (ghf{errors = reverse errs}))
    where
    readAndShow inError errs = do
        isEnd <- hIsEOF hndl
        if isEnd
            then return errs
            else do
                line    <-  hGetLine hndl
                let parsed  = parse buildLineParser "" line
                lineNr  <-  appendLog log (line ++ "\n") ErrorTag
                case (parsed, errs) of
                    (Left e,_) -> do
                        putStrLn (show e)
                        readAndShow False errs
                    (Right ne@(ErrorLine fp l c str),_) ->
                        readAndShow True ((ErrorSpec fp l c str (lineNr,lineNr)):errs)
                    (Right (OtherLine str1),(ErrorSpec fp i1 i2 str (l1,l2)):tl) ->
                        if inError
                            then readAndShow True ((ErrorSpec fp i1 i2
                                                    (if null str
                                                        then line
                                                        else str ++ "\n" ++ line)
                                                    (l1,lineNr)) : tl)
                            else readAndShow False errs
                    otherwise -> readAndShow False errs

runExternal :: FilePath -> [String] -> IO (Handle, Handle, Handle, ProcessHandle)
runExternal path args = do
    hndls@(inp, out, err, _) <- runInteractiveProcess path args Nothing Nothing
    putStrLn $ "Starting external tool: " ++ path
    hSetBuffering out NoBuffering
    hSetBuffering err NoBuffering
    hSetBuffering inp NoBuffering
    hSetBinaryMode out True
    hSetBinaryMode err True
    return hndls

data BuildError =   BuildLine
                |   EmptyLine
                |   ErrorLine FilePath Int Int String
                |   OtherLine String



buildLineParser :: CharParser () BuildError
buildLineParser = try (do
        char '['
        integer
        symbol "of"
        integer
        char '['
        many (anyChar)
        return BuildLine)
    <|> try (do
        filePath <- many (noneOf ":")
        char ':'
        line <- integer
        char ':'
        column <- integer
        char ':'
        whiteSpace
        text <- many anyChar
        return (ErrorLine filePath (fromIntegral line) (fromIntegral column) text))
    <|> try (do
        whiteSpace
        eof
        return EmptyLine)
    <|> try (do
        text <- many anyChar
        eof
        return (OtherLine text))
    <?> "buildLineParser"


lexer = P.makeTokenParser emptyDef
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
hexadecimal = P.hexadecimal lexer
symbol = P.symbol lexer
identifier = P.identifier lexer
colon = P.colon lexer
integer = P.integer lexer

