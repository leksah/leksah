{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Debug
-- Copyright   :  (c) Hamish Mackenzie, Juergen Nicklisch-Franken
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | The debug methods of ide.
--
---------------------------------------------------------------------------------


module IDE.Debug (
    debugCommand
,   debugCommand'
,   debugToggled
,   debugQuit
,   debugExecuteSelection
,   debugExecuteAndShowSelection

,   debugSetBreakpoint
,   debugDeleteAllBreakpoints
,   debugDeleteBreakpoint

,   debugContinue
,   debugAbandon
,   debugStop

,   debugStep
,   debugStepExpression
,   debugStepExpr
,   debugStepLocal
,   debugStepModule

,   debugTrace
,   debugTraceExpression
,   debugTraceExpr
,   debugHistory
,   debugBack
,   debugForward

,   debugForce
,   debugPrint
,   debugSimplePrint

,   debugShowBindings
,   debugShowBreakpoints
,   debugShowContext
,   debugShowModules
,   debugShowPackages
,   debugShowLanguages

,   debugInformation
,   debugKind
,   debugType

,   debugSetPrintEvldWithShow
,   debugSetBreakOnException
,   debugSetBreakOnError
,   debugSetPrintBindResult
) where

import IDE.Core.State
import IDE.LogRef
import Control.Exception (SomeException(..))
import IDE.Pane.SourceBuffer
       (selectedLocation, selectedText, selectedModuleName,
        insertTextAfterSelection, selectedTextOrCurrentLine)
import IDE.Metainfo.Provider (getActivePackageDescr)
import Distribution.Text (display)
import IDE.Pane.Log
import Data.List (intersperse, stripPrefix, isSuffixOf)
import IDE.Utils.GUIUtils (getDebugToggled)
import IDE.Package (debugStart, executeDebugCommand, tryDebug, printBindResultFlag,
        breakOnErrorFlag, breakOnExceptionFlag, printEvldWithShowFlag)
import IDE.Utils.Tool (ToolOutput(..), toolProcess, interruptProcessGroupOf)
import IDE.Workspaces (packageTry)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Applicative (Alternative(..), (<$>), (<*>))
import Data.IORef (newIORef)
import Data.Monoid ((<>), Monoid(..))
import Data.Text (Text)
import qualified Data.Text as T
       (concat, intersperse, pack, lines, stripPrefix, unlines,
        isSuffixOf, unpack)
import System.Exit (ExitCode(..))
import IDE.Pane.WebKit.Output (loadOutputUri)
import qualified Data.Sequence as Seq (filter, empty)

-- | Get the last item
sinkLast = CL.fold (\_ a -> Just a) Nothing

debugCommand :: Text -> C.Sink ToolOutput IDEM () -> DebugAction
debugCommand command handler = do
    debugCommand' command handler
    lift $ triggerEventIDE VariablesChanged
    return ()

debugCommand' :: Text -> C.Sink ToolOutput IDEM () -> DebugAction
debugCommand' command handler = do
    ghci <- ask
    lift $ catchIDE (runDebug (executeDebugCommand command handler) ghci)
        (\(e :: SomeException) -> (print e))

debugToggled :: IDEAction
debugToggled = do
    toggled <- getDebugToggled
    maybeDebug <- readIDE debugState
    case (toggled, maybeDebug) of
        (True, Nothing) -> packageTry debugStart
        (False, Just _) -> debugQuit
        _               -> return ()

debugQuit :: IDEAction
debugQuit = do
    maybeDebug <- readIDE debugState
    case maybeDebug of
        Just debug -> runDebug (debugCommand ":quit" logOutputDefault) debug
        _          -> return ()

-- | Remove haddock code prefix from selected text so it can be run
-- in ghci
--
-- Press Ctrl + Enter on these to try it out...
--
-- > stripComments "-- > Wow this is meta"
--
-- > stripComments "-- This is still a comment"
stripComments :: Text -> Text
stripComments t = maybe t (T.concat . intersperse "\n") $
        mapM (T.stripPrefix "-- >>>") lines'
    <|> mapM (T.stripPrefix "-- >") lines'
  where
    lines' = T.lines t

debugExecuteSelection :: IDEAction
debugExecuteSelection = do
    maybeText   <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> do
            let command = packageTry $ tryDebug $ do
                    debugSetLiberalScope
                    buffer <- liftIO $ newIORef mempty
                    debugCommand (stripComments text) $ do
                        _ <- C.getZipSink $ const
                            <$> C.ZipSink sinkLast
                            <*> C.ZipSink (logOutputPane text buffer)
                        mbURI <- lift $ readIDE autoURI
                        case mbURI of
                            Just uri -> lift . postSyncIDE . loadOutputUri $ T.unpack uri
                            Nothing -> return ()
            modifyIDE_ $ \ide -> ide {autoCommand = command, autoURI = Nothing}
            command
        Nothing   -> ideMessage Normal "Please select some text in the editor to execute"

debugExecuteAndShowSelection :: IDEAction
debugExecuteAndShowSelection = do
    maybeText   <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ do
            debugSetLiberalScope
            debugCommand (stripComments text) $ do
                out <- C.getZipSink $ const
                        <$> C.ZipSink (CL.fold buildOutputString "")
                        <*> C.ZipSink logOutputDefault
                lift . insertTextAfterSelection $ " " <> out
        Nothing   -> ideMessage Normal "Please select some text in the editor to execute"
    where
    buildOutputString :: Text -> ToolOutput -> Text
    buildOutputString "" (ToolOutput str) = str
    buildOutputString s  (ToolOutput str) = s <> "\n" <> str
    buildOutputString s  _                = s

debugSetLiberalScope :: DebugAction
debugSetLiberalScope = do
    maybeModuleName <- lift selectedModuleName
    case maybeModuleName of
        Just moduleName ->
            debugCommand (":module *" <> moduleName) CL.sinkNull
        Nothing -> do
            mbPackage <- lift getActivePackageDescr
            case mbPackage of
                Nothing -> return ()
                Just p -> let packageNames = map (T.pack . display . modu . mdModuleId) (pdModules p)
                    in debugCommand' (foldl (\a b -> a <> " *" <> b) ":module + " packageNames)
                        CL.sinkNull

debugAbandon :: IDEAction
debugAbandon =
    packageTry $ tryDebug $ debugCommand ":abandon" logOutputDefault

debugBack :: IDEAction
debugBack = packageTry $ do
    currentHist' <- lift $ readIDE currentHist
    liftIDE $ modifyIDE_ (\ide -> ide{currentHist = min (currentHist' - 1) 0})
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":back" (logOutputForHistoricContextDefault debugPackage)

debugForward :: IDEAction
debugForward = packageTry $ do
    currentHist' <- lift $ readIDE currentHist
    liftIDE $ modifyIDE_ (\ide -> ide{currentHist = currentHist' + 1})
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":forward" (logOutputForHistoricContextDefault debugPackage)

debugStop :: IDEAction
debugStop = do
    maybeDebug <- readIDE debugState
    liftIO $ case maybeDebug of
        Just (_, ghci) -> toolProcess ghci >>= interruptProcessGroupOf
        Nothing -> return ()

debugContinue :: IDEAction
debugContinue = packageTry $ tryDebug $ do
    (debugPackage, _) <- ask
    debugCommand ":continue" (logOutputForHistoricContextDefault debugPackage)

debugDeleteAllBreakpoints :: IDEAction
debugDeleteAllBreakpoints = do
    packageTry $ tryDebug $ debugCommand ":delete *" logOutputDefault
    setBreakpointList Seq.empty

debugDeleteBreakpoint :: Text -> LogRef -> IDEAction
debugDeleteBreakpoint indexString lr = do
    packageTry $ tryDebug $ debugCommand (":delete " <> indexString) logOutputDefault
    bl <- readIDE breakpointRefs
    setBreakpointList $ Seq.filter (/= lr) bl
    ideR <- ask
    return ()

debugForce :: IDEAction
debugForce = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ debugCommand (":force " <> stripComments text) logOutputDefault
        Nothing   -> ideMessage Normal "Please select an expression in the editor"

debugHistory :: IDEAction
debugHistory = packageTry $ tryDebug $ debugCommand ":history" logOutputDefault

debugPrint :: IDEAction
debugPrint = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ debugCommand (":print " <> stripComments text) logOutputDefault
        Nothing   -> ideMessage Normal "Please select an name in the editor"

debugSimplePrint :: IDEAction
debugSimplePrint = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ debugCommand (":force " <> stripComments text) logOutputDefault
        Nothing   -> ideMessage Normal "Please select an name in the editor"

debugStep :: IDEAction
debugStep = packageTry $ tryDebug $ do
    (debugPackage, _) <- ask
    debugSetLiberalScope
    debugCommand ":step" (logOutputForHistoricContextDefault debugPackage)

debugStepExpression :: IDEAction
debugStepExpression = do
    maybeText <- selectedTextOrCurrentLine
    packageTry $ tryDebug $ do
        debugSetLiberalScope
        debugStepExpr maybeText

debugStepExpr :: Maybe Text -> DebugAction
debugStepExpr maybeText = do
    (debugPackage, _) <- ask
    case maybeText of
        Just text -> debugCommand (":step " <> stripComments text) (logOutputForHistoricContextDefault debugPackage)
        Nothing   -> lift $ ideMessage Normal "Please select an expression in the editor"

debugStepLocal :: IDEAction
debugStepLocal = packageTry $ tryDebug $ do
    (debugPackage, _) <- ask
    debugCommand ":steplocal" (logOutputForHistoricContextDefault debugPackage)

debugStepModule :: IDEAction
debugStepModule = packageTry $ tryDebug $ do
    (debugPackage, _) <- ask
    debugCommand ":stepmodule" (logOutputForHistoricContextDefault debugPackage)


logTraceOutput debugPackage = do
    logOutputForLiveContextDefault debugPackage
    lift $ triggerEventIDE TraceChanged
    return ()

debugTrace :: IDEAction
debugTrace = packageTry $ tryDebug $ do
    (debugPackage, _) <- ask
    debugCommand ":trace" $ logTraceOutput debugPackage

debugTraceExpression :: IDEAction
debugTraceExpression = do
    maybeText <- selectedTextOrCurrentLine
    packageTry $ tryDebug $ do
        debugSetLiberalScope
        debugTraceExpr maybeText

debugTraceExpr :: Maybe Text -> DebugAction
debugTraceExpr maybeText = do
    (debugPackage, _) <- ask
    case maybeText of
        Just text -> debugCommand (":trace " <> stripComments text) $ logTraceOutput debugPackage
        Nothing   -> lift $ ideMessage Normal "Please select an expression in the editor"


debugShowBindings :: IDEAction
debugShowBindings = packageTry $ tryDebug $ debugCommand ":show bindings" logOutputDefault

debugShowBreakpoints :: IDEAction
debugShowBreakpoints = packageTry $ tryDebug $ do
    (debugPackage, _) <- ask
    debugCommand ":show breaks" (logOutputForSetBreakpointDefault debugPackage)

debugShowContext :: IDEAction
debugShowContext = packageTry $ tryDebug $ do
    (debugPackage, _) <- ask
    debugCommand ":show context" (logOutputForHistoricContextDefault debugPackage)

debugShowModules :: IDEAction
debugShowModules = packageTry $ tryDebug $ debugCommand ":show modules" $
    logOutputLinesDefault_ $ \log logLaunch output -> liftIO $ do
        case output of
            ToolInput  line -> appendLog log logLaunch (line <> "\n") InputTag
            ToolOutput line | ", interpreted )" `T.isSuffixOf` line
                            -> appendLog log logLaunch (line <> "\n") LogTag
            ToolOutput line -> appendLog log logLaunch (line <> "\n") InfoTag
            ToolError  line -> appendLog log logLaunch (line <> "\n") ErrorTag
            ToolPrompt _    -> defaultLineLogger' log logLaunch output
            ToolExit _      -> appendLog log logLaunch "X--X--X ghci process exited unexpectedly X--X--X" FrameTag
        return ()

debugShowPackages :: IDEAction
debugShowPackages = packageTry $ tryDebug $ debugCommand ":show packages" logOutputDefault

debugShowLanguages :: IDEAction
debugShowLanguages = packageTry $ tryDebug $ debugCommand ":show languages" logOutputDefault

debugInformation :: IDEAction
debugInformation = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ do
            debugSetLiberalScope
            debugCommand (":info "<>stripComments text) logOutputDefault
        Nothing   -> ideMessage Normal "Please select a name in the editor"

debugKind :: IDEAction
debugKind = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ do
            debugSetLiberalScope
            debugCommand (":kind "<>stripComments text) logOutputDefault
        Nothing   -> ideMessage Normal "Please select a type in the editor"

debugType :: IDEAction
debugType = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ do
            debugSetLiberalScope
            debugCommand (":type "<>stripComments text) logOutputDefault
        Nothing   -> ideMessage Normal "Please select an expression in the editor"

debugSetBreakpoint :: IDEAction
debugSetBreakpoint = do
    maybeModuleName <- selectedModuleName
    case maybeModuleName of
        Just moduleName -> do
            -- ###           debugCommand (":add *"++moduleName) $ logOutputForBuild True
            maybeText <- selectedText
            case maybeText of
                Just text -> packageTry $ tryDebug $ do
                    (debugPackage, _) <- ask
                    debugCommand (":module *" <> moduleName) logOutputDefault
                    debugCommand (":break " <> text) (logOutputForSetBreakpointDefault debugPackage)
                Nothing   -> do
                    maybeLocation <- selectedLocation
                    case maybeLocation of
                        Just (line, lineOffset) -> packageTry $ tryDebug $ do
                            (debugPackage, _) <- ask
                            debugCommand (":break " <> moduleName <> " " <> T.pack (show $ line + 1) <> " " <> T.pack (show lineOffset))
                                         (logOutputForSetBreakpointDefault debugPackage)
                        Nothing -> ideMessage Normal "Unknown error setting breakpoint"
            ref <- ask
            return ()
        Nothing   -> ideMessage Normal "Please select module file in the editor"

debugSet :: (Bool -> Text) -> Bool -> IDEAction
debugSet flag value =
    packageTry $ tryDebug $ debugCommand (":set " <> flag value) logOutputDefault

debugSetPrintEvldWithShow :: Bool -> IDEAction
debugSetPrintEvldWithShow = debugSet printEvldWithShowFlag

debugSetBreakOnException :: Bool -> IDEAction
debugSetBreakOnException = debugSet breakOnExceptionFlag

debugSetBreakOnError :: Bool -> IDEAction
debugSetBreakOnError = debugSet breakOnErrorFlag

debugSetPrintBindResult :: Bool -> IDEAction
debugSetPrintBindResult = debugSet printBindResultFlag


