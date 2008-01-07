-----------------------------------------------------------------------------
--
-- Module      :  IDE.Package
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
--
-- | The packages methods of ide.
--
---------------------------------------------------------------------------------


module IDE.Package (
    packageOpen
,   packageConfig
,   packageBuild
,   packageDoc
,   packageClean
,   packageCopy
,   packageRun
,   nextError
,   previousError
,   activatePackage
,   deactivatePackage
,   packageFlags

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
import Distribution.Verbosity
import System.FilePath
import Control.Concurrent
import System.Directory
import System.IO
import System.Process
import Prelude hiding (catch)
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec hiding(Parser)
import Data.Maybe(isJust,fromJust)

import IDE.Log
import IDE.Core.State
import IDE.PackageEditor
import IDE.SourceEditor
import IDE.PackageFlags
import IDE.Framework.ViewFrame
--import IDE.Extractor
import IDE.Metainfo.Info
--import IDE.Extractor
import IDE.Utils.File

packageOpen :: IDEAction
packageOpen = do
    active <- readIDE activePack
    case active of
        Just p -> deactivatePackage
        Nothing -> return ()
    selectActivePackage
    return ()

getActivePackage :: IDEM (Maybe IDEPackage)
getActivePackage = do
    active <- readIDE activePack
    case active of
        Just p -> return (Just p)
        Nothing -> selectActivePackage

activatePackage :: FilePath -> IDEM (Maybe IDEPackage)
activatePackage filePath = do
    session <- readIDE session
    let ppath = dropFileName filePath
    lift $setCurrentDirectory ppath
    packageD <- lift $readPackageDescription normal filePath >>= return . flattenPackageDescription
    let packp = IDEPackage (package packageD) filePath [] [] [] [] [] [] [] []
    pack <- (do
        flagFileExists <- lift $doesFileExist (ppath </> "IDE.flags")
        if flagFileExists
            then lift $readFlags (ppath </> "IDE.flags") packp
            else return packp)
    modifyIDE_ (\ide -> return (ide{activePack = (Just pack)}))
    buildCurrentInfo (buildDepends packageD)
    sb <- getSBActivePackage
    lift $statusbarPop sb 1
    lift $statusbarPush sb 1 (showPackageId $packageId pack)
    return (Just pack)

deactivatePackage :: IDEAction
deactivatePackage = do
    modifyIDE_ (\ide -> return (ide{activePack = Nothing}))
    sb <- getSBActivePackage
    lift $statusbarPop sb 1
    lift $statusbarPush sb 1 ""
    return ()

packageFlags :: IDEAction
packageFlags = do
    active <- getActivePackage
    case active of
        Nothing ->   return ()
        Just p  ->   do
            editFlags
            active2 <- getActivePackage
            case active2 of
                Nothing -> do
                    lift $putStrLn "no more active package"
                    return ()
                Just p  ->
                    lift $writeFlags ((dropFileName (cabalFile p)) </> "IDE.flags") p

selectActivePackage :: IDEM (Maybe IDEPackage)
selectActivePackage = do
    window  <- readIDE window
    mbFilePath <- lift $choosePackageFile window
    case mbFilePath of
        Nothing -> return Nothing
        Just filePath -> activatePackage filePath


packageConfig :: IDEAction
packageConfig = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","configure"]
                                            ++ (configFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageBuild :: IDEAction
packageBuild = do
    mbPackage   <- getActivePackage
    log         <- getLog
    ideR        <- ask
    case mbPackage of
        Nothing         -> return ()
        Just package    -> do
            sb <- getSBErrors
            lift $statusbarPop sb 1
            lift $statusbarPush sb 1 "Building"
            unmarkCurrentError
            lift $do
                (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","build"]
                                                ++ buildFlags package)
                oid <- forkIO (readOut log out)
                eid <- forkIO (runReaderT (readErrForBuild log err) ideR)
                forkIO (rebuild pid ideR)
                return ()
    where
        rebuild pid ideR     =   do
            res <- do   threadDelay 50
                        tryRebuild pid ideR
            if not res
                then rebuild pid ideR
                else return ()
        tryRebuild pid ideR = do
            res <- getProcessExitCode pid
            case res of
                Nothing -> return False
                Just _ -> do
                    putStrLn "About to build Active Info"
                    runReaderT buildActiveInfo ideR
                    putStrLn "After building Active Info"
                    return True


packageDoc :: IDEAction
packageDoc = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","haddock"]
                                            ++ (haddockFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageClean :: IDEAction
packageClean = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","clean"]
                                            ++ (haddockFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageCopy :: IDEAction
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
                    (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","copy"]
                                            ++ ["--destdir=" ++ fp])
                    oid <- forkIO (readOut log out)
                    eid <- forkIO (readErr log err)
                    return ()

packageRun :: IDEAction
packageRun = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            pd <- readPackageDescription normal (cabalFile package) >>= return . flattenPackageDescription
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

packageInstall :: IDEAction
packageInstall = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","install"]
                                            ++ (installFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageRegister :: IDEAction
packageRegister = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","register"]
                                            ++ (registerFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageUnregister :: IDEAction
packageUnregister = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","unregister"]
                                            ++ (unregisterFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageTest :: IDEAction
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

packageSdist :: IDEAction
packageSdist = do
    mbPackage   <- getActivePackage
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","sdist"]
                                            ++ (sdistFlags package))
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

packageOpenDoc :: IDEAction
packageOpenDoc = do
    mbPackage   <- getActivePackage
    prefs       <- readIDE prefs
    log         <- getLog
    case mbPackage of
        Nothing         -> return ()
        Just package    -> lift $do
            let path = "dist/doc/html" </> pkgName (packageId package) </> "index.html"
            (inp,out,err,pid) <- runExternal (browser prefs) [path]
            oid <- forkIO (readOut log out)
            eid <- forkIO (readErr log err)
            return ()

chooseDir :: String -> IDEM (Maybe FilePath)
chooseDir str = do
    win <- readIDE window
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
            _ -> return Nothing

-- ---------------------------------------------------------------------
-- Handling of Compiler errors
--

readErrForBuild :: IDELog -> Handle -> IDEAction
readErrForBuild log hndl = do
    errs <- lift $readAndShow False []
    lift $message $"Err " ++ (show errs)
    modifyIDE_ (\ide -> return (ide{errors = reverse errs, currentErr = Nothing}))
    sb <- getSBErrors
    lift $statusbarPop sb 1
    lift $statusbarPush sb 1 $show (length errs) ++ " Errors"
    if not (null errs)
        then nextError
        else return ()
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

selectErr :: Int -> IDEAction
selectErr index = do
    errors <- readIDE errors
    if length errors < index + 1
        then return ()
        else do
            let thisErr = errors !! index
            succ <- selectSourceBuf (filePath thisErr)
            if succ
                then markErrorInSourceBuf (line thisErr) (column thisErr)
                        (errDescription thisErr)
                else return ()
            markErrorInLog (logLines thisErr)

unmarkCurrentError :: IDEAction
unmarkCurrentError = do
    currentErr'     <-  readIDE currentErr
    errors'         <-  readIDE errors
    when (isJust currentErr') $ do
        let theError =  errors' !! fromJust currentErr'
        allBufs     <-  allBuffers
        fpc         <-  lift $ canonicalizePath $ filePath theError
        let theBufs =   filter (\ buf -> isJust (fileName buf) &&
                                            equalFilePath fpc (fromJust (fileName buf)))
                            allBufs
        mapM_ removeMark theBufs
        where
        removeMark buf = lift $ do
            gtkbuf  <-  textViewGetBuffer (sourceView buf)
            i1      <-  textBufferGetStartIter gtkbuf
            i2      <-  textBufferGetEndIter gtkbuf
            textBufferRemoveTagByName gtkbuf "activeErr" i1 i2

nextError :: IDEAction
nextError = do
    errs <- readIDE errors
    currentErr <- readIDE currentErr
    if null errs
        then return ()
        else do
            case currentErr of
                Nothing -> do
                    modifyIDE_ (\ide -> return (ide{currentErr = Just 0}))
                    selectErr 0
                Just n | (n + 1) < length errs -> do
                    modifyIDE_ (\ide -> return (ide{currentErr = Just (n + 1)}))
                    selectErr (n + 1)
                Just n  -> selectErr n

previousError :: IDEAction
previousError = do
    errs <- readIDE errors
    currentErr <- readIDE currentErr
    if null errs
        then return ()
        else do
            case currentErr of
                Nothing -> do
                    modifyIDE_ (\ide -> return (ide{currentErr = Just (length errs - 1)}))
                    selectErr (length errs - 1)
                Just n | n > 0 -> do
                    modifyIDE_ (\ide -> return (ide{currentErr = Just (n - 1)}))
                    selectErr (n - 1)
                otherwise -> selectErr 0

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
