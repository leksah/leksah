{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}
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
,   logOutputLinesDefault_
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
import Data.List (partition, stripPrefix, elemIndex, isPrefixOf)
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
import qualified Data.Text as T
       (length, stripPrefix, isPrefixOf, unpack, unlines, pack, null)
import Data.Monoid ((<>))
import qualified Data.Set as S (notMember, member, insert, empty)
import Data.Set (Set)
import System.FilePath.Windows ((</>))
import Data.Sequence (ViewR(..), Seq)
import qualified Data.Foldable as F (toList, forM_)
import qualified Data.Sequence as Seq
       (null, singleton, viewr, reverse, fromList)

showSourceSpan :: LogRef -> Text
showSourceSpan = T.pack . displaySrcSpan . logRefSrcSpan

selectRef :: Maybe LogRef -> IDEAction
selectRef (Just ref) = do
    mbBuf         <- selectSourceBuf (logRefFullFilePath ref)
    case mbBuf of
        Just buf  -> markRefInSourceBuf buf ref True
        Nothing   -> liftIO . void $ debugM "leksah" "no buf"
    log :: Log.IDELog <- Log.getLog
    maybe (return ()) (Log.markErrorInLog log) (logLines ref)
selectRef Nothing = return ()

forOpenLogRefs :: (LogRef -> IDEBuffer -> IDEAction) -> IDEAction
forOpenLogRefs f = do
    logRefs <- readIDE allLogRefs
    allBufs <- allBuffers
    F.forM_ logRefs $ \ref -> do
        let fp = logRefFullFilePath ref
        fpc <- liftIO $ myCanonicalizePath fp
        forM_ (filter (\buf -> case fileName buf of
                Just fn -> equalFilePath fpc fn
                Nothing -> False) allBufs) (f ref)

markLogRefs :: IDEAction
markLogRefs =
    forOpenLogRefs $ \logRef buf -> markRefInSourceBuf buf logRef False

unmarkLogRefs :: IDEAction
unmarkLogRefs =
    forOpenLogRefs $ \logRef (IDEBuffer {sourceView = sv}) -> do
            buf     <-  getBuffer sv
            removeTagByName buf (T.pack $ show (logRefType logRef))

setBreakpointList :: Seq LogRef -> IDEAction
setBreakpointList breaks = do
    ideR <- ask
    unmarkLogRefs
    errs <- readIDE errorRefs
    contexts <- readIDE contextRefs
    modifyIDE_ (\ide -> ide{allLogRefs = errs <> breaks <> contexts})
    setCurrentBreak Nothing
    markLogRefs
    triggerEventIDE BreakpointChanged
    return ()

addLogRefs :: Seq LogRef -> IDEAction
addLogRefs refs = do
    ideR <- ask
    unmarkLogRefs
    modifyIDE_ (\ide -> ide{allLogRefs = allLogRefs ide <> refs})
    setCurrentError Nothing
    markLogRefs
    triggerEventIDE (ErrorChanged False)
    triggerEventIDE BreakpointChanged
    triggerEventIDE TraceChanged
    return ()

next :: (IDE -> Seq LogRef)
     -> (IDE -> Maybe LogRef)
     -> (Maybe LogRef -> IDEAction)
     -> IDEAction
next all current set = do
    all <- F.toList <$> readIDE all
    current <- readIDE current
    let isCurrent = (== current) . Just
    case dropWhile isCurrent (dropWhile (not . isCurrent) all) <> all of
        (n:_) -> do
            set (Just n)
            selectRef (Just n)
        _ -> return ()

nextError :: IDEAction
nextError = next errorRefs currentError setCurrentError

previousError :: IDEAction
previousError = next (Seq.reverse . errorRefs) currentError setCurrentError

nextBreakpoint :: IDEAction
nextBreakpoint = next breakpointRefs currentBreak setCurrentBreak

previousBreakpoint :: IDEAction
previousBreakpoint = next (Seq.reverse . breakpointRefs) currentBreak setCurrentBreak

nextContext :: IDEAction
nextContext = next contextRefs currentContext setCurrentContext

previousContext :: IDEAction
previousContext = next (Seq.reverse . contextRefs) currentContext setCurrentContext

lastContext :: IDEAction
lastContext = do
    contexts <- readIDE contextRefs
    currentContext <- readIDE currentContext
    case contexts of
        (Seq.viewr -> _ :> l) -> do
            setCurrentContext $ Just l
            selectRef $ Just l
        _ -> return ()

fixColumn c = max 0 (c - 1)

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
    <?> "srcSpanParser"

data BuildOutput = BuildProgress Int Int FilePath
                 | DocTestFailure SrcSpan Text

buildOutputParser :: CharParser () BuildOutput
buildOutputParser = try (do
        char '['
        n <- int
        whiteSpace
        symbol "of"
        whiteSpace
        total <- int
        char ']'
        whiteSpace
        symbol "Compiling"
        many (noneOf "(")
        char '('
        whiteSpace
        file <- many (noneOf ",")
        char ','
        many anyChar
        return $ BuildProgress n total file)
    <|> try (do
        symbol "###"
        whiteSpace
        symbol "Failure"
        whiteSpace
        symbol "in"
        whiteSpace
        file <- many (noneOf ":")
        char ':'
        line <- int
        char ':'
        whiteSpace
        text <- T.pack <$> many anyChar
        return $ DocTestFailure (SrcSpan file line 7 line (T.length text - 7)) $ "Failure in " <> text)
    <?> "buildOutputParser"

data BuildError =   BuildLine
                |   EmptyLine
                |   ErrorLine SrcSpan LogRefType Text
                |   WarningLine Text
                |   OtherLine Text

buildErrorParser :: CharParser () BuildError
buildErrorParser = try (do
        char '['
        int
        symbol "of"
        int
        char ']'
        many anyChar
        return BuildLine)
    <|> try (do
        whiteSpace
        span <- srcSpanParser
        char ':'
        whiteSpace
        refType <- try (do
                symbol "Warning:" <|> symbol "warning:"
                return WarningRef)
            <|> (do
                symbol "Error:" <|> symbol "error:"
                return ErrorRef)
            <|> return ErrorRef
        text <- T.pack <$> many anyChar
        return (ErrorLine span refType text))
    <|> try (do
        whiteSpace
        eof
        return EmptyLine)
    <|> try (do
        whiteSpace
        warning <- T.pack <$> (symbol "Warning:" <|> symbol "warning:")
        text <- T.pack <$> many anyChar
        return (WarningLine (warning <> text)))
    <|> try (do
        text <- T.pack <$> many anyChar
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
    <?> "breaksLineParser"

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
int = fromInteger <$> P.integer lexer

defaultLineLogger :: IDELog -> LogLaunch -> ToolOutput -> IDEM Int
defaultLineLogger log logLaunch out = liftIO $ defaultLineLogger' log logLaunch out

defaultLineLogger' :: IDELog -> LogLaunch -> ToolOutput -> IO Int
defaultLineLogger' log logLaunch out =
    case out of
        ToolInput  line            -> appendLog' (line <> "\n") InputTag
        ToolOutput line            -> appendLog' (line <> "\n") LogTag
        ToolError  line            -> appendLog' (line <> "\n") ErrorTag
        ToolPrompt line            -> do
            unless (T.null line) $ void (appendLog' (line <> "\n") LogTag)
            appendLog' (T.pack (concat (replicate 20 "- ")) <> "-\n") FrameTag
        ToolExit   ExitSuccess     -> appendLog' (T.pack (replicate 41 '-') <> "\n") FrameTag
        ToolExit   (ExitFailure 1) -> appendLog' (T.pack (replicate 41 '=') <> "\n") FrameTag
        ToolExit   (ExitFailure n) -> appendLog' (T.pack (take 41 ("========== " ++ show n <> " " ++ repeat '=')) <> "\n") FrameTag
    where
        appendLog' = Log.appendLog log logLaunch

paneLineLogger :: IDELog -> LogLaunch -> ToolOutput -> IDEM (Maybe Text)
paneLineLogger log logLaunch out = liftIO $ paneLineLogger' log logLaunch out

paneLineLogger' :: IDELog -> LogLaunch -> ToolOutput -> IO (Maybe Text)
paneLineLogger' log logLaunch out =
    case out of
        ToolInput  line            -> appendLog' (line <> "\n") InputTag >> return Nothing
        ToolOutput line            -> appendLog' (line <> "\n") LogTag >> return (Just line)
        ToolError  line            -> appendLog' (line <> "\n") ErrorTag >> return Nothing
        ToolPrompt line            -> do
            unless (T.null line) $ void (appendLog' (line <> "\n") LogTag)
            appendLog' (T.pack (concat (replicate 20 "- ")) <> "-\n") FrameTag
            return Nothing
        ToolExit   ExitSuccess     -> appendLog' (T.pack (replicate 41 '-') <> "\n") FrameTag >> return Nothing
        ToolExit   (ExitFailure 1) -> appendLog' (T.pack (replicate 41 '=') <> "\n") FrameTag >> return Nothing
        ToolExit   (ExitFailure n) -> appendLog' (T.pack (take 41 ("========== " ++ show n ++ " " ++ repeat '=')) <> "\n") FrameTag >> return Nothing
    where
        appendLog' = Log.appendLog log logLaunch

logOutputLines :: LogLaunch -- ^ logLaunch
               -> (IDELog -> LogLaunch -> ToolOutput -> IDEM a)
               -> C.Sink ToolOutput IDEM [a]
logOutputLines logLaunch lineLogger = do
    log :: Log.IDELog <- lift $ postSyncIDE Log.getLog
    results <- CL.mapM (postSyncIDE . lineLogger log logLaunch) =$ CL.consume
    lift . postSyncIDE $ triggerEventIDE (StatusbarChanged [CompartmentState "", CompartmentBuild False])
    return results

logOutputLines_ :: LogLaunch
                -> (IDELog -> LogLaunch -> ToolOutput -> IDEM a)
                -> C.Sink ToolOutput IDEM ()
logOutputLines_ logLaunch lineLogger = do
    logOutputLines logLaunch lineLogger
    return ()

logOutputLinesDefault_ :: (IDELog -> LogLaunch -> ToolOutput -> IDEM a)
                       -> C.Sink ToolOutput IDEM ()
logOutputLinesDefault_ lineLogger = do
    defaultLogLaunch <- lift getDefaultLogLaunch
    logOutputLines_  defaultLogLaunch lineLogger

logOutput :: LogLaunch
          -> C.Sink ToolOutput IDEM ()
logOutput logLaunch = do
    logOutputLines logLaunch defaultLineLogger
    return ()

logOutputDefault :: C.Sink ToolOutput IDEM ()
logOutputDefault = do
    defaultLogLaunch <- lift getDefaultLogLaunch
    logOutput defaultLogLaunch

logOutputPane :: Text -> IORef [Text] -> C.Sink ToolOutput IDEM ()
logOutputPane command buffer = do
    defaultLogLaunch <- lift getDefaultLogLaunch
    result <- catMaybes <$> logOutputLines defaultLogLaunch paneLineLogger
    unless (null result) $ do
        new <- liftIO . atomicModifyIORef buffer $ \x -> let new = x ++ result in (new, new)
        mbURI <- lift $ readIDE autoURI
        unless (isJust mbURI) . lift . postSyncIDE . setOutput command $ T.unlines new

data BuildOutputState = BuildOutputState { log           :: IDELog
                                         , inError       :: Bool
                                         , inDocTest     :: Bool
                                         , errs          :: [LogRef]
                                         , testFails     :: [LogRef]
                                         , filesCompiled :: Set FilePath
                                         }

-- Not quite a Monoid
initialState :: IDELog -> BuildOutputState
initialState log = BuildOutputState log False False [] [] S.empty

logOutputForBuild :: IDEPackage
                  -> Bool
                  -> Bool
                  -> C.Sink ToolOutput IDEM [LogRef]
logOutputForBuild package backgroundBuild jumpToWarnings = do
    liftIO $ debugM "leksah" "logOutputForBuild"
    log    <- lift getLog
    logLaunch <- lift Log.getDefaultLogLaunch
    BuildOutputState {..} <- CL.foldM (readAndShow logLaunch) $ initialState log
    lift $ postSyncIDE $ do
        allErrorLikeRefs <- readIDE errorRefs
        triggerEventIDE (Sensitivity [(SensitivityError,not (Seq.null allErrorLikeRefs))])
        let errorNum    =   length (filter isError errs)
        let warnNum     =   length errs - errorNum
        triggerEventIDE (StatusbarChanged [CompartmentState
            (T.pack $ show errorNum ++ " Errors, " ++ show warnNum ++ " Warnings"), CompartmentBuild False])
        return errs
  where
    readAndShow :: LogLaunch -> BuildOutputState -> ToolOutput -> IDEM BuildOutputState
    readAndShow logLaunch state@BuildOutputState {..} output = do
        ideR <- ask
        let logPrevious (previous:_) = reflectIDE (addLogRef False backgroundBuild previous) ideR
            logPrevious _ = return ()
        liftIDE $ postSyncIDE $ liftIO $ case output of
            -- stack prints everything to stderr, so let's process errors as normal output first
            ToolError line -> processNormalOutput ideR logLaunch state logPrevious line $ do
                let parsed  =  parse buildErrorParser "" $ T.unpack line
                let nonErrorPrefixes = ["Linking ", "ar:", "ld:", "ld warning:"]
                tag <- case parsed of
                    Right BuildLine -> return InfoTag
                    Right (OtherLine text) | "Linking " `T.isPrefixOf` text ->
                        -- when backgroundBuild $ lift interruptProcess
                        return InfoTag
                    Right (OtherLine text) | any (`T.isPrefixOf` text) nonErrorPrefixes ->
                        return InfoTag
                    _ -> return ErrorTag
                lineNr <- Log.appendLog log logLaunch (line <> "\n") tag
                case (parsed, errs) of
                    (Left e,_) -> do
                        sysMessage Normal . T.pack $ show e
                        return state { inError = False }
                    (Right ne@(ErrorLine span refType str),_) -> do
                        let ref  = LogRef span package str Nothing (Just (lineNr,lineNr)) refType
                            root = logRefRootPath ref
                            file = logRefFilePath ref
                            fullFilePath = logRefFullFilePath ref
                        unless (fullFilePath `S.member` filesCompiled) $
                            reflectIDE (removeBuildLogRefs root file) ideR
                        when inError $ logPrevious errs
                        return state { inError = True
                                     , errs = ref:errs
                                     , filesCompiled = S.insert fullFilePath filesCompiled
                                     }
                    (Right (OtherLine str1), ref@(LogRef span rootPath str Nothing (Just (l1,l2)) refType):tl) ->
                        if inError
                            then return state { errs = LogRef span rootPath
                                                         (if T.null str then line else str <> "\n" <> line)
                                                         Nothing
                                                         (Just (l1, lineNr))
                                                         refType
                                                         : tl
                                              }
                            else return state
                    (Right (WarningLine str1),LogRef span rootPath str Nothing (Just (l1, l2)) isError : tl) ->
                        if inError
                            then return state { errs = LogRef span rootPath
                                                         (if T.null str then line else str <> "\n" <> line)
                                                         Nothing
                                                         (Just (l1, lineNr))
                                                         WarningRef
                                                         : tl
                                              }
                            else return state
                    _ -> do
                        when inError $ logPrevious errs
                        return state { inError = False }
            ToolOutput line ->
                processNormalOutput ideR logLaunch state logPrevious line $ do
                  case (inDocTest, testFails) of
                    (True, LogRef span rootPath str Nothing (Just (l1, l2)) refType : tl) -> do
                        logLn <- Log.appendLog log logLaunch (line <> "\n") ErrorTag
                        return state { testFails = LogRef span
                                            rootPath
                                            (str <> "\n" <> line)
                                            Nothing (Just (l1,logLn)) TestFailureRef : tl
                                     }
                    _ -> do
                        Log.appendLog log logLaunch (line <> "\n") LogTag
                        when inDocTest $ logPrevious testFails
                        return state { inDocTest = False }
            ToolInput line -> do
                Log.appendLog log logLaunch (line <> "\n") InputTag
                return state
            ToolPrompt line -> do
                unless (T.null line) . void $ Log.appendLog log logLaunch (line <> "\n") LogTag
                when inError $ logPrevious errs
                when inDocTest $ logPrevious testFails
                let errorNum    =   length (filter isError errs)
                let warnNum     =   length errs - errorNum
                case errs of
                    [] -> defaultLineLogger' log logLaunch output
                    _ -> Log.appendLog log logLaunch (T.pack $ "- - - " ++ show errorNum ++ " errors - "
                                            ++ show warnNum ++ " warnings - - -\n") FrameTag
                return state { inError = False, inDocTest = False }
            ToolExit _ -> do
                let errorNum    =   length (filter isError errs)
                    warnNum     =   length errs - errorNum
                when inError $ logPrevious errs
                when inDocTest $ logPrevious testFails
                case (errs, testFails) of
                    ([], []) -> defaultLineLogger' log logLaunch output
                    _ -> Log.appendLog log logLaunch (T.pack $ "----- " ++ show errorNum ++ " errors -- "
                                            ++ show warnNum ++ " warnings -- "
                                            ++ show (length testFails) ++ " doctest failures -----\n") FrameTag
                return state { inError = False, inDocTest = False }
    -- process output line as normal, otherwise calls given alternative
    processNormalOutput :: (IORef IDE) -> LogLaunch -> BuildOutputState -> ([LogRef]->IO()) -> Text -> IO BuildOutputState -> IO BuildOutputState
    processNormalOutput ideR logLaunch state@BuildOutputState {..} logPrevious line altFunction =
      case (parse buildOutputParser "" $ T.unpack line) of
        (Right (BuildProgress n total file)) -> do
            logLn <- Log.appendLog log logLaunch (line <> "\n") LogTag
            reflectIDE (triggerEventIDE (StatusbarChanged [CompartmentState
                (T.pack $ "Compiling " ++ show n ++ " of " ++ show total), CompartmentBuild False])) ideR
            let root = ipdPackageDir package
                fullFilePath = root </> file
            reflectIDE (removeBuildLogRefs root file) ideR
            when inDocTest $ logPrevious testFails
            return state { inDocTest = False }
        (Right (DocTestFailure span exp)) -> do
            logLn <- Log.appendLog log logLaunch (line <> "\n") ErrorTag
            when inDocTest $ logPrevious testFails
            return state { inDocTest = True
                         , testFails = LogRef span
                                package
                                exp
                                Nothing (Just (logLn,logLn)) TestFailureRef : testFails
                         }
        _ -> altFunction

--logOutputLines :: Text -- ^ logLaunch
--               -> (LogLaunch -> ToolOutput -> IDEM a)
--               -> [ToolOutput]
--               -> IDEM [a]

logOutputForBreakpoints :: IDEPackage
                        -> LogLaunch           -- ^ loglaunch
                        -> C.Sink ToolOutput IDEM ()
logOutputForBreakpoints package logLaunch = do
    breaks <- logOutputLines logLaunch (\log logLaunch out -> postSyncIDE $
        case out of
            ToolOutput line -> do
                logLineNumber <- liftIO $ Log.appendLog log logLaunch (line <> "\n") LogTag
                case parse breaksLineParser "" $ T.unpack line of
                    Right (BreakpointDescription n span) ->
                        return $ Just $ LogRef span package line Nothing (Just (logLineNumber, logLineNumber)) BreakpointRef
                    _ -> return Nothing
            _ -> do
                defaultLineLogger log logLaunch out
                return Nothing)
    lift . setBreakpointList . Seq.fromList $ catMaybes breaks

logOutputForSetBreakpoint :: IDEPackage
                        -> LogLaunch           -- ^ loglaunch
                        -> C.Sink ToolOutput IDEM ()
logOutputForSetBreakpoint package logLaunch = do
    breaks <- logOutputLines logLaunch (\log logLaunch out ->
        case out of
            ToolOutput line -> do
                logLineNumber <- liftIO $ Log.appendLog log logLaunch (line <> "\n") LogTag
                case parse setBreakpointLineParser "" $ T.unpack line of
                    Right (BreakpointDescription n span) ->
                        return $ Just $ LogRef span package line Nothing (Just (logLineNumber, logLineNumber)) BreakpointRef
                    _ -> return Nothing
            _ -> do
                defaultLineLogger log logLaunch out
                return Nothing)
    lift . postSyncIDE . addLogRefs . Seq.fromList $ catMaybes breaks

logOutputForSetBreakpointDefault :: IDEPackage
                                 -> C.Sink ToolOutput IDEM ()
logOutputForSetBreakpointDefault package = do
    defaultLogLaunch <- lift getDefaultLogLaunch
    logOutputForSetBreakpoint package defaultLogLaunch

logOutputForContext :: IDEPackage
                    -> LogLaunch                   -- ^ loglaunch
                    -> (Text -> [SrcSpan])
                    -> C.Sink ToolOutput IDEM ()
logOutputForContext package loglaunch getContexts = do
    refs <- catMaybes <$> logOutputLines loglaunch (\log logLaunch out ->
        case out of
            ToolOutput line -> do
                logLineNumber <- liftIO $ Log.appendLog log logLaunch (line <> "\n") LogTag
                let contexts = getContexts line
                if null contexts
                    then return Nothing
                    else return $ Just $ LogRef (last contexts) package line Nothing (Just (logLineNumber, logLineNumber)) ContextRef
            _ -> do
                defaultLineLogger log logLaunch out
                return Nothing)
    lift . unless (null refs) . postSyncIDE $ do
        addLogRefs . Seq.singleton $ last refs
        lastContext

contextParser :: CharParser () SrcSpan
contextParser = try (do
        whiteSpace
        symbol "Logged breakpoint at" <|> symbol "Stopped at"
        whiteSpace
        srcSpanParser)
    <?> "historicContextParser"

logOutputForLiveContext :: IDEPackage
                        -> LogLaunch           -- ^ loglaunch
                        -> C.Sink ToolOutput IDEM ()
logOutputForLiveContext package logLaunch = logOutputForContext package logLaunch (getContexts . T.unpack)
    where
        getContexts [] = []
        getContexts line@(x:xs) = case parse contextParser "" line of
                                    Right desc -> desc : getContexts xs
                                    _          -> getContexts xs

logOutputForLiveContextDefault :: IDEPackage
                               -> C.Sink ToolOutput IDEM ()
logOutputForLiveContextDefault package = do
    defaultLogLaunch <- lift getDefaultLogLaunch
    logOutputForLiveContext package defaultLogLaunch


logOutputForHistoricContext :: IDEPackage
                            -> LogLaunch           -- ^ loglaunch
                            -> C.Sink ToolOutput IDEM ()
logOutputForHistoricContext package logLaunch = logOutputForContext package logLaunch getContexts
    where
        getContexts line = case parse contextParser "" $ T.unpack line of
                                Right desc -> [desc]
                                _          -> []

logOutputForHistoricContextDefault :: IDEPackage
                                   -> C.Sink ToolOutput IDEM ()
logOutputForHistoricContextDefault package = do
    defaultLogLaunch <- lift getDefaultLogLaunch
    logOutputForHistoricContext package defaultLogLaunch
