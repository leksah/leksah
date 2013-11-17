{-# LANGUAGE CPP, ScopedTypeVariables, OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.LogRef
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- |
--
---------------------------------------------------------------------------------


module IDE.LogRef (
    nextError
,   previousError
,   nextBreakpoint
,   previousBreakpoint
,   markLogRefs
,   unmarkLogRefs
,   defaultLineLogger
,   defaultLineLogger'
,   logOutputLines
,   logOutputLines_
,   logOutputLines_Default
,   logOutput
,   logOutputDefault
,   logOutputPane
,   logOutputForBuild
,   logOutputForBreakpoints
,   logOutputForSetBreakpoint
,   logOutputForSetBreakpointDefault
,   logOutputForLiveContext
,   logOutputForLiveContextDefault
,   logOutputForHistoricContext
,   logOutputForHistoricContextDefault
,   selectRef
,   setBreakpointList
,   showSourceSpan
,   srcSpanParser
) where

import Graphics.UI.Gtk
import Control.Monad.Reader
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding(Parser)
import qualified Text.ParserCombinators.Parsec.Token as P

import IDE.Core.State
import IDE.TextEditor
import IDE.Pane.SourceBuffer
import qualified IDE.Pane.Log as Log
import IDE.Utils.Tool
import System.FilePath (equalFilePath)
import Data.List (stripPrefix, elemIndex, isPrefixOf)
import Data.Maybe (catMaybes, isJust)
import System.Exit (ExitCode(..))
import System.Log.Logger (debugM)
import IDE.Utils.FileUtils(myCanonicalizePath)
import IDE.Pane.Log (getDefaultLogLaunch, IDELog(..), getLog)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Conduit ((=$))
import IDE.Pane.WebKit.Output(setOutput)
import Data.IORef (atomicModifyIORef, IORef, readIORef)
import Data.Text (Text)
import Control.Applicative ((<$>))
import qualified Data.Text as T (unpack, unlines, pack, null)
import Data.Monoid ((<>))

showSourceSpan :: LogRef -> String
showSourceSpan = displaySrcSpan . logRefSrcSpan

selectRef :: Maybe LogRef -> IDEAction
selectRef (Just ref) = do
    logRefs <- readIDE allLogRefs
    case elemIndex ref logRefs of
        Nothing    -> liftIO $ debugM "leksah" "no index" >> return ()
        Just index -> do
            mbBuf         <- selectSourceBuf (logRefFullFilePath ref)
            case mbBuf of
                Just buf  -> markRefInSourceBuf index buf ref True
                Nothing   -> liftIO $ debugM "leksah" "no buf" >> return ()
            log :: Log.IDELog <- Log.getLog
            Log.markErrorInLog log (logLines ref)
selectRef Nothing = return ()

forOpenLogRefs :: (Int -> LogRef -> IDEBuffer -> IDEAction) -> IDEAction
forOpenLogRefs f = do
    logRefs <- readIDE allLogRefs
    allBufs <- allBuffers
    forM_ [0 .. ((length logRefs)-1)] (\index -> do
        let ref = logRefs !! index
            fp = logRefFullFilePath ref
        fpc <- liftIO $ myCanonicalizePath fp
        forM_ (filter (\buf -> case (fileName buf) of
                Just fn -> equalFilePath fpc fn
                Nothing -> False) allBufs) (f index ref))

markLogRefs :: IDEAction
markLogRefs = do
    forOpenLogRefs $ \index logRef buf -> markRefInSourceBuf index buf logRef False

unmarkLogRefs :: IDEAction
unmarkLogRefs = do
    forOpenLogRefs $ \index logRef (IDEBuffer {sourceView = sv}) -> do
            buf     <-  getBuffer sv
            removeTagByName buf (show (logRefType logRef) ++ show index)

setErrorList :: [LogRef] -> IDEAction
setErrorList errs = do
    unmarkLogRefs
    breaks <- readIDE breakpointRefs
    contexts <- readIDE contextRefs
    modifyIDE_ (\ide -> ide{allLogRefs = errs ++ breaks ++ contexts})
    setCurrentError Nothing
    markLogRefs
    triggerEventIDE ErrorChanged
    return ()

setBreakpointList :: [LogRef] -> IDEAction
setBreakpointList breaks = do
    ideR <- ask
    unmarkLogRefs
    errs <- readIDE errorRefs
    contexts <- readIDE contextRefs
    modifyIDE_ (\ide -> ide{allLogRefs = errs ++ breaks ++ contexts})
    setCurrentBreak Nothing
    markLogRefs
    triggerEventIDE BreakpointChanged
    return ()

addLogRefs :: [LogRef] -> IDEAction
addLogRefs refs = do
    ideR <- ask
    unmarkLogRefs
    modifyIDE_ (\ide -> ide{allLogRefs = (allLogRefs ide) ++ refs})
    setCurrentError Nothing
    markLogRefs
    triggerEventIDE ErrorChanged
    triggerEventIDE BreakpointChanged
    triggerEventIDE TraceChanged
    return ()

nextError :: IDEAction
nextError = do
    errs <- readIDE errorRefs
    currentError <- readIDE currentError
    if null errs
        then return ()
        else do
            let new = case currentError of
                        Nothing -> 0
                        Just ref ->
                            case elemIndex ref errs of
                                Nothing -> 0
                                Just n | (n + 1) < length errs -> (n + 1)
                                Just n -> n
            setCurrentError (Just $ errs!!new)
            selectRef $ Just (errs!!new)

previousError :: IDEAction
previousError = do
    errs <- readIDE errorRefs
    currentError <- readIDE currentError
    if null errs
        then return ()
        else do
            let new = case currentError of
                        Nothing -> (length errs - 1)
                        Just ref ->
                            case elemIndex ref errs of
                                Nothing -> (length errs - 1)
                                Just n | n > 0 -> (n - 1)
                                Just n -> 0
            setCurrentError (Just $ errs!!new)
            selectRef $ Just (errs!!new)

nextBreakpoint :: IDEAction
nextBreakpoint = do
    breaks <- readIDE breakpointRefs
    currentBreak <- readIDE currentBreak
    if null breaks
        then return ()
        else do
            let new = case currentBreak of
                        Nothing -> 0
                        Just ref ->
                            case elemIndex ref breaks of
                                Nothing -> 0
                                Just n | (n + 1) < length breaks -> (n + 1)
                                Just n -> n
            setCurrentBreak (Just $ breaks!!new)
            selectRef $ Just (breaks!!new)

previousBreakpoint :: IDEAction
previousBreakpoint = do
    breaks <- readIDE breakpointRefs
    currentBreak <- readIDE currentBreak
    if null breaks
        then return ()
        else do
            let new = case currentBreak of
                        Nothing -> (length breaks - 1)
                        Just ref ->
                            case elemIndex ref breaks of
                                Nothing -> (length breaks - 1)
                                Just n | n > 0 -> (n - 1)
                                Just n -> 0
            setCurrentBreak (Just $ breaks!!new)
            selectRef $ Just (breaks!!new)

nextContext :: IDEAction
nextContext = do
    contexts <- readIDE contextRefs
    currentContext <- readIDE currentContext
    if null contexts
        then return ()
        else do
            let new = case currentContext of
                        Nothing -> 0
                        Just ref ->
                            case elemIndex ref contexts of
                                Nothing -> 0
                                Just n | (n + 1) < length contexts -> (n + 1)
                                Just n -> n
            setCurrentContext (Just $ contexts!!new)
            selectRef $ Just (contexts!!new)

previousContext :: IDEAction
previousContext = do
    contexts <- readIDE contextRefs
    currentContext <- readIDE currentContext
    if null contexts
        then return ()
        else do
            let new = case currentContext of
                        Nothing -> (length contexts - 1)
                        Just ref ->
                            case elemIndex ref contexts of
                                Nothing -> (length contexts - 1)
                                Just n | n > 0 -> (n - 1)
                                Just n -> 0
            setCurrentContext (Just $ contexts!!new)
            selectRef $ Just (contexts!!new)

lastContext :: IDEAction
lastContext = do
    contexts <- readIDE contextRefs
    currentContext <- readIDE currentContext
    if null contexts
        then return ()
        else do
            let new = (last contexts)
            setCurrentContext (Just new)
            selectRef $ Just new

#if MIN_VERSION_ghc(7,0,1)
fixColumn c = max 0 (c - 1)
#else
fixColumn = id
#endif

srcPathParser :: CharParser () FilePath
srcPathParser = try (do
        symbol "dist/build/tmp-" -- Support for cabal haddock
        many digit
        char '/'
        many (noneOf ":"))
    <|> many (noneOf ":")

srcSpanParser :: CharParser () SrcSpan
srcSpanParser = try (do
        filePath <- srcPathParser
        char ':'
        char '('
        beginLine <- int
        char ','
        beginCol <- int
        char ')'
        char '-'
        char '('
        endLine <- int
        char ','
        endCol <- int
        char ')'
        return $ SrcSpan filePath beginLine (fixColumn beginCol) endLine (fixColumn endCol))
    <|> try (do
        filePath <- srcPathParser
        char ':'
        line <- int
        char ':'
        beginCol <- int
        char '-'
        endCol <- int
        return $ SrcSpan filePath line (fixColumn beginCol) line (fixColumn endCol))
    <|> try (do
        filePath <- srcPathParser
        char ':'
        line <- int
        char ':'
        col <- int
        return $ SrcSpan filePath line (fixColumn col) line (fixColumn col))
    <?> "srcLocParser"

data BuildError =   BuildLine
                |   EmptyLine
                |   ErrorLine SrcSpan LogRefType String
                |   WarningLine String
                |   OtherLine String

buildLineParser :: CharParser () BuildError
buildLineParser = try (do
        char '['
        int
        symbol "of"
        int
        char ']'
        many (anyChar)
        return BuildLine)
    <|> try (do
        whiteSpace
        span <- srcSpanParser
        char ':'
        whiteSpace
        refType <- try (do
                symbol "Warning:"
                return WarningRef)
            <|> return ErrorRef
        text <- many anyChar
        return (ErrorLine span refType text))
    <|> try (do
        whiteSpace
        eof
        return EmptyLine)
    <|> try (do
        whiteSpace
        symbol "Warning:"
        text <- many anyChar
        return (WarningLine ("Warning:" ++ text)))
    <|> try (do
        text <- many anyChar
        eof
        return (OtherLine text))
    <?> "buildLineParser"

data BreakpointDescription = BreakpointDescription Int SrcSpan

breaksLineParser :: CharParser () BreakpointDescription
breaksLineParser = try (do
        char '['
        n <- int
        char ']'
        whiteSpace
        many (noneOf " ")
        whiteSpace
        span <- srcSpanParser
        return (BreakpointDescription n span))
    <?> "buildLineParser"

setBreakpointLineParser :: CharParser () BreakpointDescription
setBreakpointLineParser = try (do
        symbol "Breakpoint"
        whiteSpace
        n <- int
        whiteSpace
        symbol "activated"
        whiteSpace
        symbol "at"
        whiteSpace
        span <- srcSpanParser
        return (BreakpointDescription n span))
    <?> "setBreakpointLineParser"

lexer = P.makeTokenParser emptyDef
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
hexadecimal = P.hexadecimal lexer
symbol = P.symbol lexer
identifier = P.identifier lexer
colon = P.colon lexer
int = fmap fromInteger $ P.integer lexer

defaultLineLogger :: IDELog -> LogLaunch -> ToolOutput -> IDEM Int
defaultLineLogger log logLaunch out = liftIO $ defaultLineLogger' log logLaunch out

defaultLineLogger' :: IDELog -> LogLaunch -> ToolOutput -> IO Int
defaultLineLogger' log logLaunch out = do
    case out of
        ToolInput  line            -> appendLog' (line <> "\n") InputTag
        ToolOutput line            -> appendLog' (line <> "\n") LogTag
        ToolError  line            -> appendLog' (line <> "\n") ErrorTag
        ToolPrompt line            -> do
            unless (T.null line) $ appendLog' (line <> "\n") LogTag >> return ()
            appendLog' (T.pack (concat (take 20 (repeat "- "))) <> "-\n") FrameTag
        ToolExit   ExitSuccess     -> appendLog' (T.pack (take 41 (repeat '-')) <> "\n") FrameTag
        ToolExit   (ExitFailure 1) -> appendLog' (T.pack (take 41 (repeat '=')) <> "\n") FrameTag
        ToolExit   (ExitFailure n) -> appendLog' (T.pack (take 41 ("========== " ++ show n <> " " ++ repeat '=')) <> "\n") FrameTag
    where
        appendLog' = Log.appendLog log logLaunch

paneLineLogger :: IDELog -> LogLaunch -> ToolOutput -> IDEM (Maybe Text)
paneLineLogger log logLaunch out = liftIO $ paneLineLogger' log logLaunch out

paneLineLogger' :: IDELog -> LogLaunch -> ToolOutput -> IO (Maybe Text)
paneLineLogger' log logLaunch out = do
    case out of
        ToolInput  line            -> appendLog' (line <> "\n") InputTag >> return Nothing
        ToolOutput line            -> appendLog' (line <> "\n") LogTag >> return (Just line)
        ToolError  line            -> appendLog' (line <> "\n") ErrorTag >> return Nothing
        ToolPrompt line            -> do
            unless (T.null line) $ appendLog' (line <> "\n") LogTag >> return ()
            appendLog' (T.pack (concat (take 20 (repeat "- "))) <> "-\n") FrameTag
            return Nothing
        ToolExit   ExitSuccess     -> appendLog' (T.pack (take 41 (repeat '-')) <> "\n") FrameTag >> return Nothing
        ToolExit   (ExitFailure 1) -> appendLog' (T.pack (take 41 (repeat '=')) <> "\n") FrameTag >> return Nothing
        ToolExit   (ExitFailure n) -> appendLog' (T.pack (take 41 ("========== " ++ show n ++ " " ++ repeat '=')) <> "\n") FrameTag >> return Nothing
    where
        appendLog' = Log.appendLog log logLaunch

logOutputLines :: LogLaunch -- ^ logLaunch
               -> (IDELog -> LogLaunch -> ToolOutput -> IDEM a)
               -> C.Sink ToolOutput IDEM [a]
logOutputLines logLaunch lineLogger = do
    log :: Log.IDELog <- lift $ postSyncIDE Log.getLog
    results <- (CL.mapM $ postSyncIDE . lineLogger log logLaunch) =$ CL.consume
    lift . postAsyncIDE $ do
        triggerEventIDE (StatusbarChanged [CompartmentState "", CompartmentBuild False])
        return ()
    return results

logOutputLines_ :: LogLaunch
                -> (IDELog -> LogLaunch -> ToolOutput -> IDEM a)
                -> C.Sink ToolOutput IDEM ()
logOutputLines_ logLaunch lineLogger = do
    logOutputLines logLaunch lineLogger
    return ()

logOutputLines_Default :: (IDELog -> LogLaunch -> ToolOutput -> IDEM a)
                       -> C.Sink ToolOutput IDEM ()
logOutputLines_Default lineLogger = do
    defaultLogLaunch <- lift $ getDefaultLogLaunch
    logOutputLines_  defaultLogLaunch lineLogger

logOutput :: LogLaunch
          -> C.Sink ToolOutput IDEM ()
logOutput logLaunch = do
    logOutputLines logLaunch defaultLineLogger
    return ()

logOutputDefault :: C.Sink ToolOutput IDEM ()
logOutputDefault = do
    defaultLogLaunch <- lift $ getDefaultLogLaunch
    logOutput defaultLogLaunch

logOutputPane :: IORef [Text] -> C.Sink ToolOutput IDEM ()
logOutputPane buffer = do
    defaultLogLaunch <- lift $ getDefaultLogLaunch
    result <- catMaybes <$> logOutputLines defaultLogLaunch paneLineLogger
    when (not $ null result) $ do
        new <- liftIO . atomicModifyIORef buffer $ \x -> let new = x ++ result in (new, new)
        lift . postSyncIDE . setOutput $ T.unpack $ T.unlines new

logOutputForBuild :: IDEPackage
                  -> Bool
                  -> Bool
                  -> C.Sink ToolOutput IDEM [LogRef]
logOutputForBuild package backgroundBuild jumpToWarnings = do
    log    <- lift getLog
    logLaunch <- lift $ Log.getDefaultLogLaunch
    (_, _, errs) <- CL.foldM (readAndShow logLaunch) (log, False, [])
    ideR <- lift ask
    liftIO $ postGUISync $ reflectIDE (do
        setErrorList $ reverse errs
        triggerEventIDE (Sensitivity [(SensitivityError,not (null errs))])
        let errorNum    =   length (filter isError errs)
        let warnNum     =   length errs - errorNum
        triggerEventIDE (StatusbarChanged [CompartmentState
            (show errorNum ++ " Errors, " ++ show warnNum ++ " Warnings"), CompartmentBuild False])
        unless (backgroundBuild || (not jumpToWarnings && errorNum == 0)) nextError
        return errs) ideR
  where
    readAndShow :: LogLaunch -> (IDELog, Bool, [LogRef]) -> ToolOutput -> IDEM (IDELog, Bool, [LogRef])
    readAndShow logLaunch (log, inError, errs) output = do
        ideR <- ask
        liftIO $ postGUISync $ case output of
            ToolError line -> do
                let parsed  =  parse buildLineParser "" $ T.unpack line
                let nonErrorPrefixes = ["Linking ", "ar:", "ld:", "ld warning:"]
                tag <- case parsed of
                    Right BuildLine -> return InfoTag
                    Right (OtherLine text) | "Linking " `isPrefixOf` text -> do
                        -- when backgroundBuild $ lift interruptProcess
                        reflectIDE (do
                                setErrorList $ reverse errs
                            ) ideR
                        return InfoTag
                    Right (OtherLine text) | any (`isPrefixOf` text) nonErrorPrefixes -> do
                        return InfoTag
                    _ -> return ErrorTag
                lineNr <- Log.appendLog log logLaunch (line <> "\n") tag
                case (parsed, errs) of
                    (Left e,_) -> do
                        sysMessage Normal (show e)
                        return (log, False, errs)
                    (Right ne@(ErrorLine span refType str),_) ->
                        return (log, True, ((LogRef span package (T.pack str) (lineNr,lineNr) refType):errs))
                    (Right (OtherLine str1),(LogRef span rootPath str (l1,l2) refType):tl) ->
                        if inError
                            then return (log, True, ((LogRef span
                                                    rootPath
                                                    (if T.null str
                                                        then line
                                                        else str <> "\n" <> line)
                                                    (l1,lineNr) refType) : tl))
                            else return (log, False, errs)
                    (Right (WarningLine str1),(LogRef span rootPath str (l1,l2) isError):tl) ->
                        if inError
                            then return (log, True, ((LogRef span
                                                    rootPath
                                                    (if T.null str
                                                        then line
                                                        else str <> "\n" <> line)
                                                    (l1,lineNr) WarningRef) : tl))
                            else return (log, False, errs)
                    otherwise -> return (log, False, errs)
            ToolOutput line -> do
                Log.appendLog log logLaunch (line <> "\n") LogTag
                return (log, inError, errs)
            ToolInput line -> do
                Log.appendLog log logLaunch (line <> "\n") InputTag
                return (log, inError, errs)
            ToolPrompt line -> do
                unless (T.null line) $ Log.appendLog log logLaunch (line <> "\n") LogTag >> return ()
                let errorNum    =   length (filter isError errs)
                let warnNum     =   length errs - errorNum
                case errs of
                    [] -> defaultLineLogger' log logLaunch output
                    _ -> Log.appendLog log logLaunch (T.pack $ "- - - " ++ show errorNum ++ " errors - "
                                            ++ show warnNum ++ " warnings - - -\n") FrameTag
                return (log, inError, errs)
            ToolExit _ -> do
                let errorNum    =   length (filter isError errs)
                let warnNum     =   length errs - errorNum
                case errs of
                    [] -> defaultLineLogger' log logLaunch output
                    _ -> Log.appendLog log logLaunch (T.pack $ "----- " ++ show errorNum ++ " errors -- "
                                            ++ show warnNum ++ " warnings -----\n") FrameTag
                return (log, inError, errs)


--logOutputLines :: String -- ^ logLaunch
--               -> (LogLaunch -> ToolOutput -> IDEM a)
--               -> [ToolOutput]
--               -> IDEM [a]

logOutputForBreakpoints :: IDEPackage
                        -> LogLaunch           -- ^ loglaunch
                        -> C.Sink ToolOutput IDEM ()
logOutputForBreakpoints package logLaunch = do
    breaks <- logOutputLines logLaunch (\log logLaunch out -> do
        case out of
            ToolOutput line -> do
                logLineNumber <- liftIO $ Log.appendLog log logLaunch (line <> "\n") LogTag
                case parse breaksLineParser "" $ T.unpack line of
                    Right (BreakpointDescription n span) ->
                        return $ Just $ LogRef span package line (logLineNumber, logLineNumber) BreakpointRef
                    _ -> return Nothing
            _ -> do
                defaultLineLogger log logLaunch out
                return Nothing)
    lift $ setBreakpointList $ catMaybes breaks

logOutputForSetBreakpoint :: IDEPackage
                        -> LogLaunch           -- ^ loglaunch
                        -> C.Sink ToolOutput IDEM ()
logOutputForSetBreakpoint package logLaunch = do
    breaks <- logOutputLines logLaunch (\log logLaunch out -> do
        case out of
            ToolOutput line -> do
                logLineNumber <- liftIO $ Log.appendLog log logLaunch (line <> "\n") LogTag
                case parse setBreakpointLineParser "" $ T.unpack line of
                    Right (BreakpointDescription n span) ->
                        return $ Just $ LogRef span package line (logLineNumber, logLineNumber) BreakpointRef
                    _ -> return Nothing
            _ -> do
                defaultLineLogger log logLaunch out
                return Nothing)
    lift $ addLogRefs $ catMaybes breaks

logOutputForSetBreakpointDefault :: IDEPackage
                                 -> C.Sink ToolOutput IDEM ()
logOutputForSetBreakpointDefault package = do
    defaultLogLaunch <- lift $ getDefaultLogLaunch
    logOutputForSetBreakpoint package defaultLogLaunch

logOutputForContext :: IDEPackage
                    -> LogLaunch                   -- ^ loglaunch
                    -> (String -> [SrcSpan])
                    -> C.Sink ToolOutput IDEM ()
logOutputForContext package loglaunch getContexts = do
    refs <- fmap catMaybes $ logOutputLines loglaunch (\log logLaunch out -> do
        case out of
            ToolOutput line -> do
                logLineNumber <- liftIO $ Log.appendLog log logLaunch (line <> "\n") LogTag
                let contexts = getContexts $ T.unpack line
                if null contexts
                    then return Nothing
                    else return $ Just $ LogRef (last contexts) package line (logLineNumber, logLineNumber) ContextRef
            _ -> do
                defaultLineLogger log logLaunch out
                return Nothing)
    lift $ unless (null refs) $ do
        addLogRefs [last refs]
        lastContext

logOutputForLiveContext :: IDEPackage
                        -> LogLaunch           -- ^ loglaunch
                        -> C.Sink ToolOutput IDEM ()
logOutputForLiveContext package logLaunch = logOutputForContext package logLaunch getContexts
    where
        getContexts [] = []
        getContexts line@(x:xs) = case stripPrefix "Stopped at " line of
            Just rest -> case parse srcSpanParser "" rest of
                Right desc -> desc : getContexts xs
                _          -> getContexts xs
            _ -> getContexts xs

logOutputForLiveContextDefault :: IDEPackage
                               -> C.Sink ToolOutput IDEM ()
logOutputForLiveContextDefault package = do
    defaultLogLaunch <- lift $ getDefaultLogLaunch
    logOutputForLiveContext package defaultLogLaunch


logOutputForHistoricContext :: IDEPackage
                            -> LogLaunch           -- ^ loglaunch
                            -> C.Sink ToolOutput IDEM ()
logOutputForHistoricContext package logLaunch = logOutputForContext package logLaunch getContexts
    where
        getContexts line = case stripPrefix "Logged breakpoint at " line of
            Just rest -> case parse srcSpanParser "" rest of
                Right desc -> [desc]
                _          -> []
            _ -> []

logOutputForHistoricContextDefault :: IDEPackage
                                   -> C.Sink ToolOutput IDEM ()
logOutputForHistoricContextDefault package = do
    defaultLogLaunch <- lift $ getDefaultLogLaunch
    logOutputForHistoricContext package defaultLogLaunch
