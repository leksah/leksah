-----------------------------------------------------------------------------
--
-- Module      :  Ghf.Package
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
--
-- | The packages methods of ghf.
--
---------------------------------------------------------------------------------


module Ghf.Package (
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
import Graphics.UI.Gtk.SourceView.SourceBuffer
import Control.Monad.Reader
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Verbosity
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
import Data.Maybe(isJust,fromJust)

import Ghf.Log
import Ghf.Core.State
import Ghf.PackageEditor
import Ghf.SourceEditor
import Ghf.PackageFlags
import Ghf.ViewFrame
import Ghf.Extractor
import Ghf.Info
import Ghf.Extractor
import Ghf.SourceModel
import Ghf.File

packageOpen :: GhfAction
packageOpen = do
    active <- readGhf activePack
    case active of
        Just p -> deactivatePackage
        Nothing -> return ()
    selectActivePackage
    return ()

getActivePackage :: GhfM (Maybe GhfPackage)
getActivePackage = do
    active <- readGhf activePack
    case active of
        Just p -> return (Just p)
        Nothing -> selectActivePackage

activatePackage :: FilePath -> GhfM (Maybe GhfPackage)
activatePackage filePath = do
    session <- readGhf session
    let ppath = dropFileName filePath
    lift $setCurrentDirectory ppath
    packageD <- lift $readPackageDescription normal filePath >>= return . flattenPackageDescription
    let packp = GhfPackage (package packageD) filePath [] [] [] [] [] [] [] []
    pack <- (do
        flagFileExists <- lift $doesFileExist (ppath </> "Ghf.flags")
        if flagFileExists
            then lift $readFlags (ppath </> "Ghf.flags") packp
            else return packp)
    modifyGhf_ (\ghf -> return (ghf{activePack = (Just pack)}))
    buildCurrentInfo (buildDepends packageD)
    sb <- getSBActivePackage
    lift $statusbarPop sb 1
    lift $statusbarPush sb 1 (showPackageId $packageId pack)
    return (Just pack)

deactivatePackage :: GhfAction
deactivatePackage = do
    modifyGhf_ (\ghf -> return (ghf{activePack = Nothing}))
    sb <- getSBActivePackage
    lift $statusbarPop sb 1
    lift $statusbarPush sb 1 ""
    return ()

packageFlags :: GhfAction
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
                    lift $writeFlags ((dropFileName (cabalFile p)) </> "Ghf.flags") p

selectActivePackage :: GhfM (Maybe GhfPackage)
selectActivePackage = do
    window  <- readGhf window
    mbFilePath <- lift $choosePackageFile window
    case mbFilePath of
        Nothing -> return Nothing
        Just filePath -> activatePackage filePath


packageConfig :: GhfAction
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

packageBuild :: GhfAction
packageBuild = do
    mbPackage   <- getActivePackage
    log         <- getLog
    ghfR        <- ask
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
                eid <- forkIO (runReaderT (readErrForBuild log err) ghfR)
                forkIO (rebuild pid ghfR)
                return ()
    where
        rebuild pid ghfR     =   do
            res <- do   threadDelay 50
                        tryRebuild pid ghfR
            if not res
                then rebuild pid ghfR
                else return ()
        tryRebuild pid ghfR = do
            res <- getProcessExitCode pid
            case res of
                Nothing -> return False
                Just _ -> do
                    putStrLn "About to build Active Info"
                    runReaderT buildActiveInfo ghfR
                    putStrLn "After building Active Info"
                    return True


packageDoc :: GhfAction
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

packageClean :: GhfAction
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
                    (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","copy"]
                                            ++ ["--destdir=" ++ fp])
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

packageInstall :: GhfAction
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

packageRegister :: GhfAction
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

packageUnregister :: GhfAction
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
            (inp,out,err,pid) <- runExternal "runhaskell" (["Setup","sdist"]
                                            ++ (sdistFlags package))
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

-- ---------------------------------------------------------------------
-- Handling of Compiler errors
--

readErrForBuild :: GhfLog -> Handle -> GhfAction
readErrForBuild log hndl = do
    errs <- lift $readAndShow False []
    lift $message $"Err " ++ (show errs)
    modifyGhf_ (\ghf -> return (ghf{errors = reverse errs, currentErr = Nothing}))
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

selectErr :: Int -> GhfAction
selectErr index = do
    errors <- readGhf errors
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

unmarkCurrentError :: GhfAction
unmarkCurrentError = do
    currentErr'     <-  readGhf currentErr
    errors'         <-  readGhf errors
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

markErrorInSourceBuf ::  Int -> Int -> String -> GhfAction
markErrorInSourceBuf line column string = do
    mbbuf <- maybeActiveBuf
    case mbbuf of
        Nothing -> do
            return ()
        Just (buf,_) -> lift $do
            gtkbuf <- textViewGetBuffer (sourceView buf)
            i1 <- textBufferGetStartIter gtkbuf
            i2 <- textBufferGetEndIter gtkbuf
            textBufferRemoveTagByName gtkbuf "activeErr" i1 i2

            lines   <-  textBufferGetLineCount gtkbuf
            iter    <-  textBufferGetIterAtLine gtkbuf (max 0 (min (lines-1) (line-1)))
            chars   <-  textIterGetCharsInLine iter
            textIterSetLineOffset iter (max 0 (min (chars-1) column))
            iter2 <- textIterCopy iter
            textIterForwardWordEnd iter2
            textBufferApplyTagByName gtkbuf "activeErr" iter iter2
            textBufferPlaceCursor gtkbuf iter
            mark <- textBufferGetInsert gtkbuf
            textViewScrollToMark (sourceView buf) mark 0.0 (Just (0.3,0.3))

nextError :: GhfAction
nextError = do
    errs <- readGhf errors
    currentErr <- readGhf currentErr
    if null errs
        then return ()
        else do
            case currentErr of
                Nothing -> do
                    modifyGhf_ (\ghf -> return (ghf{currentErr = Just 0}))
                    selectErr 0
                Just n | (n + 1) < length errs -> do
                    modifyGhf_ (\ghf -> return (ghf{currentErr = Just (n + 1)}))
                    selectErr (n + 1)
                Just n  -> selectErr n

previousError :: GhfAction
previousError = do
    errs <- readGhf errors
    currentErr <- readGhf currentErr
    if null errs
        then return ()
        else do
            case currentErr of
                Nothing -> do
                    modifyGhf_ (\ghf -> return (ghf{currentErr = Just (length errs - 1)}))
                    selectErr (length errs - 1)
                Just n | n > 0 -> do
                    modifyGhf_ (\ghf -> return (ghf{currentErr = Just (n - 1)}))
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
