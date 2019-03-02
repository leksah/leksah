{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
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
,   foldOutputLines
,   logOutputLines
,   logOutputLines_
,   logOutputLinesDefault_
,   logOutput
,   logOutputDefault
,   logOutputPane
,   logIdleOutput
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

import Prelude ()
import Prelude.Compat

import Control.Applicative ((<$>), (<|>))
import Control.DeepSeq (NFData(..))
import Control.Exception (evaluate)
import Control.Lens ((.~), (%~), Getting, to)
import Control.Monad.Reader

import Data.Attoparsec.Text
       (many', parseOnly, endOfInput, option, anyChar, manyTill,
        takeText, (<?>), char, try, Parser)
import qualified Data.Attoparsec.Text as AP
       (takeWhile, decimal, string, skipSpace, takeWhile1)
import Data.Char (isDigit)
import Data.Conduit ((.|), ConduitT)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Foldable as F (toList, forM_)
import Data.IORef (atomicModifyIORef, IORef)
import Data.Maybe (catMaybes, isJust)
import Data.Sequence (ViewR(..), Seq)
import qualified Data.Sequence as Seq
       (null, singleton, viewr, reverse, fromList)
import Data.Set (Set)
import qualified Data.Set as S (member, insert, empty)
import Data.Text (Text)
import qualified Data.Text as T
       (dropEnd, length, isPrefixOf, unpack, unlines, pack,
        null)
import Data.Void (Void)

import Distribution.Text (simpleParse)

import GHC.Stack (HasCallStack, SrcLoc(..))

import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath (equalFilePath, isAbsolute, (</>))
import System.Log.Logger (debugM)

import IDE.Core.State
       (IDEAction, IDE, SrcSpan(..), LogRefType(..), IDEM, Project(..),
        IDEPackage(..), Log(..), logRefSrcSpan, LogRef(..),
        displaySrcSpan, logRefFullFilePath, readIDE, allLogRefs,
        errorRefs, contextRefs, modifyIDE_, setCurrentBreak,
        triggerEventIDE_, IDEEvent(..), setCurrentError,
        currentError, breakpointRefs, currentBreak, setCurrentContext,
        LogTag(..), StatusbarCompartment(..), autoURI, liftIDE,
        Location(..), logRootPath, PackModule(..),
        packageIdentifierFromString, ipdPackageDir,
        pjPackages, SensitivityMask(..), isError, sysMessage,
        MessageLevel(..), logRefRootPath, logRefFilePath,
        triggerEventIDE)
import IDE.Gtk.State (LogLaunch, postSyncIDE, bringPaneToFront)
import IDE.Pane.Log (getDefaultLogLaunch, IDELog(..), getLog)
import qualified IDE.Pane.Log as Log
import IDE.Pane.SourceBuffer
import IDE.Pane.WebKit.Output(setOutput)
--import IDE.TextEditor
import IDE.Utils.DebugUtils (traceTimeTaken)
import IDE.Utils.FileUtils(myCanonicalizePath)
import IDE.Utils.Tool

showSourceSpan :: LogRef -> Text
showSourceSpan = T.pack . displaySrcSpan . logRefSrcSpan

selectRef :: Maybe LogRef -> IDEAction
selectRef (Just ref) = do
    mbBuf         <- selectSourceBuf (logRefFullFilePath ref)
    case mbBuf of
        Just buf  -> markRefInSourceBuf buf ref True
        Nothing   -> liftIO . void $ debugM "leksah" "no buf"
    log' :: Log.IDELog <- Log.getLog
    maybe (return ()) (Log.markErrorInLog log') (logLines ref)
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
    forOpenLogRefs $ flip unmarkRefInSourceBuf

setBreakpointList :: Seq LogRef -> IDEAction
setBreakpointList breaks = do
    unmarkLogRefs
    errs <- readIDE errorRefs
    contexts <- readIDE contextRefs
    modifyIDE_ $ allLogRefs .~ errs <> breaks <> contexts
    setCurrentBreak Nothing
    markLogRefs
    triggerEventIDE_ BreakpointChanged

addLogRefs :: Seq LogRef -> IDEAction
addLogRefs refs = do
    unmarkLogRefs
    modifyIDE_ $ allLogRefs %~ (<> refs)
    setCurrentError Nothing
    markLogRefs
    triggerEventIDE_ (ErrorChanged False)
    triggerEventIDE_ BreakpointChanged
    triggerEventIDE_ TraceChanged

next :: Getting (Seq LogRef) IDE (Seq LogRef)
     -> Getting (Maybe LogRef) IDE (Maybe LogRef)
     -> (Maybe LogRef -> IDEAction)
     -> IDEAction
next allRefs current set = do
    allRefs' <- F.toList <$> readIDE allRefs
    current' <- readIDE current
    let isCurrent = (== current') . Just
    case dropWhile isCurrent (dropWhile (not . isCurrent) allRefs') <> allRefs' of
        (n:_) -> do
            set (Just n)
            selectRef (Just n)
        _ -> return ()

nextError :: IDEAction
nextError = next errorRefs currentError setCurrentError

previousError :: IDEAction
previousError = next (errorRefs . to Seq.reverse) currentError setCurrentError

nextBreakpoint :: IDEAction
nextBreakpoint = next breakpointRefs currentBreak setCurrentBreak

previousBreakpoint :: IDEAction
previousBreakpoint = next (breakpointRefs . to Seq.reverse) currentBreak setCurrentBreak

--nextContext :: IDEAction
--nextContext = next contextRefs currentContext setCurrentContext
--
--previousContext :: IDEAction
--previousContext = next (contextRefs . to Seq.reverse) currentContext setCurrentContext

lastContext :: IDEAction
lastContext = do
    contexts <- readIDE contextRefs
    case contexts of
        (Seq.viewr -> _ :> l) -> do
            setCurrentContext $ Just l
            selectRef $ Just l
        _ -> return ()

fixColumn :: (Ord a, Num a) => a -> a
fixColumn c = max 0 (c - 1)

srcPathParser :: Parser FilePath
srcPathParser = T.unpack <$> (try (do
        _ <- symbol "dist/build/tmp-" -- Support for cabal haddock
        _ <- AP.takeWhile1 isDigit
        _ <- char '/'
        AP.takeWhile (/=':'))
    <|> AP.takeWhile (/=':'))

srcSpanParser :: Parser SrcSpan
srcSpanParser = try (do
        filePath <- srcPathParser
        _ <- char ':'
        _ <- char '('
        beginLine <- int
        _ <- char ','
        beginCol <- int
        _ <- char ')'
        _ <- char '-'
        _ <- char '('
        endLine <- int
        _ <- char ','
        endCol <- int
        _ <- char ')'
        return $ SrcSpan filePath beginLine (fixColumn beginCol) endLine (fixColumn endCol))
    <|> try (do
        filePath <- srcPathParser
        _ <- char ':'
        line <- int
        _ <- char ':'
        beginCol <- int
        _ <- char '-'
        endCol <- int
        return $ SrcSpan filePath line (fixColumn beginCol) line (fixColumn endCol))
    <|> try (do
        filePath <- srcPathParser
        _ <- char ':'
        line <- int
        _ <- char ':'
        col <- int
        return $ SrcSpan filePath line (fixColumn col) line (fixColumn col))
    <|> try (do
        filePath <- srcPathParser
        _ <- char ':'
        line <- int
        return $ SrcSpan filePath line 0 line 0)
    <?> "srcSpanParser"

data BuildOutput = BuildProgress Int Int FilePath
                 | DocTestFailure SrcSpan Text

buildOutputParser :: Parser BuildOutput
buildOutputParser = try (do
        _ <- char '['
        whiteSpace
        n <- int
        whiteSpace
        _ <- symbol "of"
        whiteSpace
        total <- int
        _ <- char ']'
        whiteSpace
        _ <- symbol "Compiling"
        _ <- AP.takeWhile (/= '(')
        _ <- char '('
        whiteSpace
        file <- AP.takeWhile (/= ',')
        _ <- char ','
        _text <- takeText
        return $ BuildProgress n total (T.unpack file))
    <|> try (do
        _ <- symbol "###"
        whiteSpace
        _ <- symbol "Failure"
        whiteSpace
        _ <- symbol "in"
        whiteSpace
        file <- AP.takeWhile (/= ':')
        _ <- char ':'
        line <- int
        _ <- char ':'
        whiteSpace
        text <- takeText
        let colGuess = T.length $ case T.unpack text of
                        ('\\':_) -> "-- prop> "
                        _        -> "-- >>> "
        return $ DocTestFailure (SrcSpan (T.unpack file) line colGuess line (T.length text - colGuess)) $ "Failure in " <> text)
    <?> "buildOutputParser"

data BuildError =   BuildLine
                |   EmptyLine
                |   ErrorLine SrcSpan LogRefType Text
                |   WarningLine Text
                |   OtherLine Text
                |   ElmFile FilePath Text
                |   ElmLine Int
                |   ElmPointLine Int
                |   ElmColumn Int Int

buildErrorParser :: Parser BuildError
buildErrorParser = try (do
        _ <- char '['
        whiteSpace
        _ <- int
        whiteSpace
        _ <- symbol "of"
        whiteSpace
        _ <- int
        whiteSpace
        _ <- char ']'
        _ <- takeText
        return BuildLine)
    <|> try (do
        -- Nix format
        _ <- symbol "error: "
        text <- T.pack <$> manyTill anyChar (symbol ", at ")
        span' <- srcSpanParser
        return (ErrorLine span' ErrorRef text))
    <|> try (do
        whiteSpace
        span' <- srcSpanParser
        _ <- char ':'
        whiteSpace
        refType <- try (do
                _ <- symbol "Warning:" <|> symbol "warning:"
                return WarningRef)
            <|> (do
                _ <- symbol "Error:" <|> symbol "error:"
                return ErrorRef)
            <|> (do
                _ <- symbol "failure"
                return TestFailureRef)
            <|> return ErrorRef
        text <- takeText
        return (ErrorLine span' refType text))
    <|> try (do
        _ <- char '-'
        _ <- char '-'
        _ <- char ' '
        whiteSpace
        text <- T.dropEnd 1 <$> AP.takeWhile (/= '-')
        _ <- char '-'
        _ <- char '-'
        _ <- AP.takeWhile (== '-')
        whiteSpace
        option () (char '.' >> char '/' >> pure ())
        file <- takeText
        return (ElmFile (T.unpack file) text))
    <|> try (do
        line <- int
        _ <- char '|'
        pointer <- char '>' <|> char ' '
        _text <- takeText
        return $ (case pointer of
                    '>' -> ElmPointLine
                    _   -> ElmLine) line)
    <|> try (do
        col1 <- T.length <$> AP.takeWhile (== ' ')
        _ <- char '^'
        col2 <- T.length <$> AP.takeWhile (== '^')
        endOfInput
        return (ElmColumn col1 (col1 + col2)))
    <|> try (do
        whiteSpace
        endOfInput
        return EmptyLine)
    <|> try (do
        whiteSpace
        warning <- symbol "Warning:" <|> symbol "warning:"
        text <- takeText
        return (WarningLine (warning <> text)))
    <|> try (do
        text <- takeText
        endOfInput
        return (OtherLine text))
    <?> "buildLineParser"

data BreakpointDescription = BreakpointDescription Int SrcSpan

breaksLineParser :: Parser BreakpointDescription
breaksLineParser = try (do
        _ <- char '['
        n <- int
        _ <- char ']'
        whiteSpace
        _ <- AP.takeWhile (/=' ')
        whiteSpace
        span' <- srcSpanParser
        return (BreakpointDescription n span'))
    <?> "breaksLineParser"

setBreakpointLineParser :: Parser BreakpointDescription
setBreakpointLineParser = try (do
        _ <- symbol "Breakpoint"
        whiteSpace
        n <- int
        whiteSpace
        _ <- symbol "activated"
        whiteSpace
        _ <- symbol "at"
        whiteSpace
        span' <- srcSpanParser
        return (BreakpointDescription n span'))
    <?> "setBreakpointLineParser"

whiteSpace :: Parser ()
whiteSpace = AP.skipSpace
symbol :: Text -> Parser Text
symbol = AP.string
int :: Parser Int
int = AP.decimal

defaultLineLogger :: IDELog -> LogLaunch -> ToolOutput -> IDEM Int
defaultLineLogger log' logLaunch out =
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
        appendLog' = Log.appendLog log' logLaunch

paneLineLogger :: IDELog -> LogLaunch -> ToolOutput -> IDEM (Maybe Text)
paneLineLogger log' logLaunch out =
    case out of
        ToolInput  line            -> appendLog' (line <> "\n") InputTag >> return Nothing
        ToolOutput line            -> appendLog' (line <> "\n") LogTag >> return (Just line)
        ToolError  line            -> appendLog' (line <> "\n") ErrorTag >> return Nothing
        ToolPrompt line            -> do
            unless (T.null line) $ void (appendLog' (line <> "\n") LogTag)
            _ <- appendLog' (T.pack (concat (replicate 20 "- ")) <> "-\n") FrameTag
            return Nothing
        ToolExit   ExitSuccess     -> appendLog' (T.pack (replicate 41 '-') <> "\n") FrameTag >> return Nothing
        ToolExit   (ExitFailure 1) -> appendLog' (T.pack (replicate 41 '=') <> "\n") FrameTag >> return Nothing
        ToolExit   (ExitFailure n) -> appendLog' (T.pack (take 41 ("========== " ++ show n ++ " " ++ repeat '=')) <> "\n") FrameTag >> return Nothing
    where
        appendLog' = Log.appendLog log' logLaunch

foldOutputLines :: LogLaunch -- ^ logLaunch
               -> (IDELog -> LogLaunch -> a -> ToolOutput -> IDEM a)
               -> a
               -> ConduitT ToolOutput Void IDEM a
foldOutputLines logLaunch lineLogger a = do
    log' :: Log.IDELog <- lift $ postSyncIDE Log.getLog
    results <- CL.foldM (\x y -> postSyncIDE $ lineLogger log' logLaunch x y) a
    lift . postSyncIDE $ triggerEventIDE_ (StatusbarChanged [CompartmentState "", CompartmentBuild False])
    return results

logOutputLines :: LogLaunch -- ^ logLaunch
               -> (IDELog -> LogLaunch -> ToolOutput -> IDEM a)
               -> ConduitT ToolOutput Void IDEM [a]
logOutputLines logLaunch lineLogger = do
    log' :: Log.IDELog <- lift $ postSyncIDE Log.getLog
    results <- CL.mapM (postSyncIDE . lineLogger log' logLaunch) .| CL.consume
    lift . postSyncIDE $ triggerEventIDE_ (StatusbarChanged [CompartmentState "", CompartmentBuild False])
    return results

logOutputLines_ :: LogLaunch
                -> (IDELog -> LogLaunch -> ToolOutput -> IDEM a)
                -> ConduitT ToolOutput Void IDEM ()
logOutputLines_ logLaunch lineLogger = do
    _ <- logOutputLines logLaunch lineLogger
    return ()

logOutputLinesDefault_ :: (IDELog -> LogLaunch -> ToolOutput -> IDEM a)
                       -> ConduitT ToolOutput Void IDEM ()
logOutputLinesDefault_ lineLogger = do
    defaultLogLaunch <- lift getDefaultLogLaunch
    logOutputLines_  defaultLogLaunch lineLogger

logOutput :: LogLaunch
          -> ConduitT ToolOutput Void IDEM ()
logOutput logLaunch = do
    void $ logOutputLines logLaunch defaultLineLogger

logOutputDefault :: ConduitT ToolOutput Void IDEM ()
logOutputDefault = do
    defaultLogLaunch <- lift getDefaultLogLaunch
    logOutput defaultLogLaunch

logOutputPane :: Text -> IORef [Text] -> ConduitT ToolOutput Void IDEM ()
logOutputPane command buffer = do
    defaultLogLaunch <- lift getDefaultLogLaunch
    result <- catMaybes <$> logOutputLines defaultLogLaunch paneLineLogger
    unless (null result) $ do
        liftIO $ debugM "leskah" "logOutputPane has result"
        new <- liftIO . atomicModifyIORef buffer $ \x -> let new = x ++ result in (new, new)
        mbURI <- lift $ readIDE autoURI
        unless (isJust mbURI) . lift . postSyncIDE . setOutput command $ T.unlines new

idleOutputParser :: Parser SrcLoc
idleOutputParser = try (do
        _ <- symbol "OPEN"
        whiteSpace
        span' <- srcSpanParser
        whiteSpace
        _ <- symbol "in"
        whiteSpace
        package <- AP.takeWhile (/=':')
        _ <- char ':'
        mod' <- takeText
        return $ SrcLoc (T.unpack package) (T.unpack mod') (srcSpanFilename span')
            (srcSpanStartLine span') (srcSpanStartColumn span')
            (srcSpanEndLine span') (srcSpanEndColumn span'))
    <?> "idleOutputParser"

logIdleOutput
    :: Project
    -> IDEPackage
    -> ConduitT ToolOutput Void IDEM ()
logIdleOutput project package = loop
  where
    loop = C.await >>= maybe (return ()) (\output -> do
        case output of
            ToolError s ->
                case parseOnly idleOutputParser s of
                    Left _ -> return ()
                    Right srcLoc ->
                        lift . liftIDE . postSyncIDE $ do
--                            descrs <- getSymbols symbol
--                            case (simpleParse $ srcLocPackage loc, simpleParse $ srcSpan) of
--                                (Just pid, Just mName) -> do
--                                    descrs <- getSymbols symbol
--                                    case filter (\case
--                                            Real rd -> dscMbModu' rd == Just (PM pid mName)
--                                            _ -> False) descrs of
--                                        [a] -> do
--                                            liftIO . tryPutMVar lookup . Just . postAsyncIDE $ selectIdentifier a activatePanes openDefinition
--                                            return True
--                                        _   -> return worked
--                                _ -> return worked

                            log' <- liftIO $ findLog project (LogCabal $ ipdCabalFile package) (srcLocFile srcLoc)
                            let loc = Location (srcLocFile srcLoc) (srcLocStartLine srcLoc) (srcLocStartCol srcLoc + 1) (srcLocEndLine srcLoc) (srcLocEndCol srcLoc)
                            goToSourceDefinition (logRootPath log') loc >>= \case
                                Just pane -> bringPaneToFront pane
                                Nothing -> goToLocation (PM
                                                <$> packageIdentifierFromString (T.pack $ srcLocPackage srcLoc)
                                                <*> simpleParse (srcLocModule srcLoc)) $ Just loc
            _ -> return ()
        loop)

data BuildOutputState = BuildOutputState { inError       :: Bool
                                         , inDocTest     :: Bool
                                         , errs          :: [LogRef]
                                         , elmLine       :: Int
                                         , testFails     :: [LogRef]
                                         , filesCompiled :: Set FilePath
                                         }

-- Not quite a Monoid
initialState :: BuildOutputState
initialState = BuildOutputState False False [] 1 [] S.empty

-- Sometimes we get error spans relative to a build dependency
findLog :: Project -> Log -> FilePath -> IO Log
findLog project log' file =
    doesFileExist (logRootPath log' </> file) >>= \case
        True -> return log'
        False ->
            -- If the file only exists in one package in the project it is probably the right one
            filterM (\p -> doesFileExist $ ipdPackageDir p </> file) (pjPackages project) >>= \case
                [p] -> return . LogCabal $ ipdCabalFile p
                _   -> return log' -- Not really sure where this file is

logOutputForBuild :: Project
                  -> Log
                  -> Bool
                  -> Bool
                  -> ConduitT ToolOutput Void IDEM [LogRef]
logOutputForBuild project logSource backgroundBuild jumpToWarnings =
  lift getLog >>= \log' -> logOutputForBuild' project logSource backgroundBuild jumpToWarnings log'

logOutputForBuild' :: Project
                  -> Log
                  -> Bool
                  -> Bool
                  -> IDELog
                  -> ConduitT ToolOutput Void IDEM [LogRef]
logOutputForBuild' project logSource backgroundBuild _jumpToWarnings log' = do
    liftIO $ debugM "leksah" "logOutputForBuild"
    logLaunch <- lift Log.getDefaultLogLaunch
    -- Elm does not log files compiled so just clear all the log refs for elm files
    lift $ postSyncIDE $ removeFileExtLogRefs logSource ".elm" [ErrorRef, WarningRef]
    lift $ postSyncIDE $ removeFileExtLogRefs logSource ".nix" [ErrorRef, WarningRef]
    BuildOutputState {..} <- CL.foldM (readAndShow logLaunch) initialState
    lift $ postSyncIDE $ do
        allErrorLikeRefs <- readIDE errorRefs
        triggerEventIDE_ (Sensitivity [(SensitivityError,not (Seq.null allErrorLikeRefs))])
        let errorNum    =   length (filter isError errs)
        let warnNum     =   length errs - errorNum
        triggerEventIDE_ (StatusbarChanged [CompartmentState
            (T.pack $ show errorNum ++ " Errors, " ++ show warnNum ++ " Warnings"), CompartmentBuild False])
        return errs
  where
    readAndShow :: LogLaunch -> BuildOutputState -> ToolOutput -> IDEM BuildOutputState
    readAndShow logLaunch state@BuildOutputState {..} output = do
        let logPrevious (previous:_) = addLogRef False backgroundBuild previous
            logPrevious _ = return ()
--        liftIO $ debugM "leksah" $ "readAndShow " ++ show output
        liftIO . evaluate $ rnf output
        liftIDE $ postSyncIDE $
          case output of
            -- stack prints everything to stderr, so let's process errors as normal output first
            ToolError line -> processNormalOutput logLaunch state logPrevious line $ do
                let parsed  =  parseOnly buildErrorParser line
                let nonErrorPrefixes = ["Linking ", "ar:", "ld:", "ld warning:"]
                tag <- case parsed of
                    Right BuildLine -> return InfoTag
                    Right (OtherLine text) | "Linking " `T.isPrefixOf` text ->
                        -- when backgroundBuild $ lift interruptProcess
                        return InfoTag
                    Right (OtherLine text) | any (`T.isPrefixOf` text) nonErrorPrefixes ->
                        return InfoTag
                    _ -> return ErrorTag
                lineNr <- traceTimeTaken "appendLog" $ Log.appendLog log' logLaunch (line <> "\n") tag
                case (parsed, errs, testFails) of
                    (Left e, _, _) -> do
                        traceTimeTaken "sysMessage" $ sysMessage Normal . T.pack $ show e
                        return state { inError = False }
                    (Right (ErrorLine span' refType str), _, _) -> do
                        foundLog <- traceTimeTaken "findLog" $ liftIO $ findLog project logSource (srcSpanFilename span')
                        let ref  = LogRef span' foundLog str Nothing (Just (lineNr,lineNr)) refType
                            root = logRefRootPath ref
                            file = logRefFilePath ref
                            fullFilePath = logRefFullFilePath ref
                        unless (fullFilePath `S.member` filesCompiled) $
                            traceTimeTaken "removeBuildLogRefs" $ removeBuildLogRefs (root </> file)
                        when inError $ traceTimeTaken "logPrevious" $ logPrevious errs
                        return state { inError = True
                                     , errs = ref:errs
                                     , elmLine = 1
                                     , filesCompiled = S.insert fullFilePath filesCompiled
                                     }
                    (Right (ElmFile efile str), _, _) -> do
                        let ref  = LogRef (SrcSpan efile 1 0 1 0) logSource str Nothing (Just (lineNr,lineNr)) ErrorRef
                            -- root = logRefRootPath ref
                            -- file = logRefFilePath ref
                            fullFilePath = logRefFullFilePath ref
                        when inError $ traceTimeTaken "logPrevious" $ logPrevious errs
                        return state { inError = True
                                     , errs = ref:errs
                                     , elmLine = 1
                                     , filesCompiled = S.insert fullFilePath filesCompiled
                                     }
                    (Right (ElmLine eline), _, _) ->
                        if inError
                            then return state
                                { elmLine = eline
                                }
                            else return state
                    (Right (ElmPointLine eline), ref:tl, _) ->
                        if inError
                            then return state
                                { errs = ref
                                    { logRefSrcSpan =
                                        case logRefSrcSpan ref of
                                             SrcSpan f 1 0 1 0 -> SrcSpan f eline 0 (eline + 1) 0
                                             SrcSpan f l _ _ _ -> SrcSpan f l     0 (eline + 1) 0
                                    } : tl
                                }
                            else return state
                    (Right (ElmColumn c1 c2), ref@LogRef{logRefSrcSpan = _span}:tl, _) ->
                        if inError
                            then do
                                let line' = max 1 elmLine
                                    leftMargin = 2 + length (show line')
                                return state
                                    { errs = ref
                                        { logRefSrcSpan = (logRefSrcSpan ref)
                                            { srcSpanStartColumn = max 0 (c1 - leftMargin)
                                            , srcSpanEndColumn = max 0 (c2 - leftMargin)
                                            , srcSpanStartLine = line'
                                            , srcSpanEndLine = line'
                                            }
                                        } : tl
                                    }
                            else return state
                    (Right (OtherLine _str1), LogRef span' rootPath str Nothing (Just (l1,_l2)) refType:tl, _)
                        | inError -> return state
                                { errs = LogRef span' rootPath
                                            (if T.null str then line else str <> "\n" <> line)
                                            Nothing
                                            (Just (l1, lineNr))
                                            refType
                                            : tl
                                }
                    (Right (OtherLine _str1), _, LogRef span' rootPath str Nothing (Just (l1,_l2)) refType:tl)
                        | inDocTest -> return state
                                { testFails = LogRef span' rootPath
                                            (if T.null str then line else str <> "\n" <> line)
                                            Nothing
                                            (Just (l1,lineNr))
                                            refType
                                            : tl
                                }
                    (Right (OtherLine _str1), _, _) -> return state
                    (Right (WarningLine _str1), LogRef span' rootPath str Nothing (Just (l1, _l2)) _isError : tl, _) ->
                        if inError
                            then return state { errs = LogRef span' rootPath
                                                         (if T.null str then line else str <> "\n" <> line)
                                                         Nothing
                                                         (Just (l1, lineNr))
                                                         WarningRef
                                                         : tl
                                              }
                            else return state
                    (Right EmptyLine, _, _) -> return state -- Elm errors can contain empty lines
                    _ -> do
                        when inError $ traceTimeTaken "logPrevious (err)" $ logPrevious errs
                        when inDocTest $ traceTimeTaken "logPrevious (test)" $ logPrevious testFails
                        return state { inError = False, inDocTest = False }
            ToolOutput line ->
                processNormalOutput logLaunch state logPrevious line $
                  case (inDocTest, testFails) of
                    (True, LogRef span' rootPath str Nothing (Just (l1, _l2)) _refType : tl) -> do
                        logLn <- traceTimeTaken "appendLog" $ Log.appendLog log' logLaunch (line <> "\n") ErrorTag
                        return state { testFails = LogRef span'
                                            rootPath
                                            (str <> "\n" <> line)
                                            Nothing (Just (l1,logLn)) TestFailureRef : tl
                                     }
                    _ -> do
                        _ <- traceTimeTaken "appendLog" $ Log.appendLog log' logLaunch (line <> "\n") LogTag
                        when inDocTest $ logPrevious testFails
                        return state { inDocTest = False }
            ToolInput line -> do
                _ <- traceTimeTaken "appendLog" $ Log.appendLog log' logLaunch (line <> "\n") InputTag
                return state
            ToolPrompt line -> do
                unless (T.null line) . void $ traceTimeTaken "appendLog" $ Log.appendLog log' logLaunch (line <> "\n") LogTag
                when inError $ traceTimeTaken "logPrevious" $ logPrevious errs
                when inDocTest $ traceTimeTaken "logPrevious" $ logPrevious testFails
                let errorNum    =   length (filter isError errs)
                let warnNum     =   length errs - errorNum
                _ <- case errs of
                    [] -> traceTimeTaken "defaultLineLogger'" $ defaultLineLogger log' logLaunch output
                    _ -> traceTimeTaken "appendLog" $ Log.appendLog log' logLaunch (T.pack $ "- - - " ++ show errorNum ++ " errors - "
                                            ++ show warnNum ++ " warnings - - -\n") FrameTag
                return state { inError = False, inDocTest = False }
            ToolExit _ -> do
                let errorNum    =   length (filter isError errs)
                    warnNum     =   length errs - errorNum
                when inError $ logPrevious errs
                when inDocTest $ logPrevious testFails
                _ <- case (errs, testFails) of
                    ([], []) -> defaultLineLogger log' logLaunch output
                    _ -> Log.appendLog log' logLaunch (T.pack $ "----- " ++ show errorNum ++ " errors -- "
                                            ++ show warnNum ++ " warnings -- "
                                            ++ show (length testFails) ++ " doctest failures -----\n") FrameTag
                return state { inError = False, inDocTest = False }
    -- process output line as normal, otherwise calls given alternative
    processNormalOutput :: HasCallStack => LogLaunch -> BuildOutputState -> ([LogRef]->IDEAction) -> Text -> IDEM BuildOutputState -> IDEM BuildOutputState
    processNormalOutput logLaunch state@BuildOutputState {..} logPrevious line altFunction =
      case parseOnly buildOutputParser line of
        (Right (BuildProgress n total file)) -> do
            _logLn <- traceTimeTaken "appendLog" $ Log.appendLog log' logLaunch (line <> "\n") LogTag
            _ <- traceTimeTaken "StatusbarChanged" $ triggerEventIDE (StatusbarChanged [CompartmentState
                (T.pack $ "Compiling " ++ show n ++ " of " ++ show total), CompartmentBuild False])
            f <- if isAbsolute file
                    then return file
                    else traceTimeTaken "findLog" $ liftIO $ (</> file) . logRootPath <$> findLog project logSource file
            traceTimeTaken "removeBuildLogRefs" $ removeBuildLogRefs f
            when inDocTest $ traceTimeTaken "logPrevious" $ logPrevious testFails
            return state { inDocTest = False }
        (Right (DocTestFailure span' exp')) -> do
            logLn <- traceTimeTaken "appendLog" $ Log.appendLog log' logLaunch (line <> "\n") ErrorTag
            when inError $ traceTimeTaken "logPrevious" $ logPrevious errs
            when inDocTest $ traceTimeTaken "logPrevious" $ logPrevious testFails
            return state { inDocTest = True
                         , inError = False
                         , testFails = LogRef span'
                                logSource
                                exp'
                                Nothing (Just (logLn,logLn)) TestFailureRef : testFails
                         }
        _ -> traceTimeTaken "altFunction" altFunction

--logOutputLines :: Text -- ^ logLaunch
--               -> (LogLaunch -> ToolOutput -> IDEM a)
--               -> [ToolOutput]
--               -> IDEM [a]

logOutputForBreakpoints :: IDEPackage
                        -> LogLaunch           -- ^ loglaunch
                        -> ConduitT ToolOutput Void IDEM ()
logOutputForBreakpoints package logLaunch = do
    breaks <- logOutputLines logLaunch (\log' logLaunch' out -> postSyncIDE $
        case out of
            ToolOutput line -> do
                logLineNumber <- Log.appendLog log' logLaunch' (line <> "\n") LogTag
                case parseOnly breaksLineParser line of
                    Right (BreakpointDescription _n span') ->
                        return $ Just $ LogRef span' (LogCabal $ ipdCabalFile package) line Nothing (Just (logLineNumber, logLineNumber)) BreakpointRef
                    _ -> return Nothing
            _ -> do
                _ <- defaultLineLogger log' logLaunch' out
                return Nothing)
    lift . setBreakpointList . Seq.fromList $ catMaybes breaks

logOutputForSetBreakpoint :: FilePath
                        -> LogLaunch           -- ^ loglaunch
                        -> ConduitT ToolOutput Void IDEM ()
logOutputForSetBreakpoint basePath logLaunch = do
    breaks <- logOutputLines logLaunch (\log' logLaunch' out ->
        case out of
            ToolOutput line -> do
                logLineNumber <- Log.appendLog log' logLaunch' (line <> "\n") LogTag
                case parseOnly setBreakpointLineParser line of
                    Right (BreakpointDescription _n span') ->
                        return $ Just $ LogRef span' (LogProject basePath) line Nothing (Just (logLineNumber, logLineNumber)) BreakpointRef
                    _ -> return Nothing
            _ -> do
                _ <- defaultLineLogger log' logLaunch' out
                return Nothing)
    lift . postSyncIDE . addLogRefs . Seq.fromList $ catMaybes breaks

logOutputForSetBreakpointDefault :: FilePath
                                 -> ConduitT ToolOutput Void IDEM ()
logOutputForSetBreakpointDefault basePath = do
    defaultLogLaunch <- lift getDefaultLogLaunch
    logOutputForSetBreakpoint basePath defaultLogLaunch

logOutputForContext :: FilePath
                    -> LogLaunch                   -- ^ loglaunch
                    -> (Text -> [SrcSpan])
                    -> ConduitT ToolOutput Void IDEM ()
logOutputForContext basePath loglaunch getContexts = do
    refs <- catMaybes <$> logOutputLines loglaunch (\log' logLaunch out ->
        case out of
            ToolOutput line -> do
                logLineNumber <- Log.appendLog log' logLaunch (line <> "\n") LogTag
                let contexts = getContexts line
                if null contexts
                    then return Nothing
                    else return $ Just $ LogRef (last contexts) (LogProject basePath) line Nothing (Just (logLineNumber, logLineNumber)) ContextRef
            _ -> do
                _ <- defaultLineLogger log' logLaunch out
                return Nothing)
    lift . unless (null refs) . postSyncIDE $ do
        addLogRefs . Seq.singleton $ last refs
        lastContext

contextParser :: Parser SrcSpan
contextParser = try (do
        whiteSpace
        _ <- symbol "Logged breakpoint at" <|> symbol "Stopped at"
        whiteSpace
        srcSpanParser)
    <?> "contextParser"

contextsParser :: Parser [SrcSpan]
contextsParser = try (
        catMaybes <$> many' (
              (Just <$> contextParser)
          <|> (anyChar >> pure Nothing)))
    <?> "contextsParser"

logOutputForLiveContext :: FilePath
                        -> LogLaunch           -- ^ loglaunch
                        -> ConduitT ToolOutput Void IDEM ()
logOutputForLiveContext basePath logLaunch = logOutputForContext basePath logLaunch getContexts
    where
        getContexts line = either (const []) id $ parseOnly contextsParser line

logOutputForLiveContextDefault :: FilePath
                               -> ConduitT ToolOutput Void IDEM ()
logOutputForLiveContextDefault basePath = do
    defaultLogLaunch <- lift getDefaultLogLaunch
    logOutputForLiveContext basePath defaultLogLaunch


logOutputForHistoricContext :: FilePath
                            -> LogLaunch           -- ^ loglaunch
                            -> ConduitT ToolOutput Void IDEM ()
logOutputForHistoricContext basePath logLaunch = logOutputForContext basePath logLaunch getContexts
    where
        getContexts line = case parseOnly contextParser line of
                                Right desc -> [desc]
                                _          -> []

logOutputForHistoricContextDefault :: FilePath
                                   -> ConduitT ToolOutput Void IDEM ()
logOutputForHistoricContextDefault basePath = do
    defaultLogLaunch <- lift getDefaultLogLaunch
    logOutputForHistoricContext basePath defaultLogLaunch

