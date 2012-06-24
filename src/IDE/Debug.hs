{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}
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
import IDE.Pane.Log (appendLog)
import Data.List (isSuffixOf)
import IDE.Utils.GUIUtils (getDebugToggled)
import IDE.Package (debugStart, executeDebugCommand, tryDebug, printBindResultFlag,
        breakOnErrorFlag, breakOnExceptionFlag, printEvldWithShowFlag)
import IDE.Utils.Tool (ToolOutput(..), toolProcess, interruptProcessGroupOf)
import IDE.Workspaces (packageTry)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))

debugCommand :: String -> E.Iteratee ToolOutput IDEM () -> DebugAction
debugCommand command handler = do
    debugCommand' command $ do
        handler
        lift $ triggerEventIDE VariablesChanged
        return ()

debugCommand' :: String -> E.Iteratee ToolOutput IDEM () -> DebugAction
debugCommand' command handler = do
    ghci <- ask
    lift $ catchIDE (runDebug (executeDebugCommand command handler) ghci)
        (\(e :: SomeException) -> putStrLn (show e))

debugToggled :: IDEAction
debugToggled = do
    toggled <- getDebugToggled
    maybeDebug <- readIDE debugState
    case (toggled, maybeDebug) of
        (True, Nothing) -> packageTry $ debugStart
        (False, Just _) -> debugQuit
        _               -> return ()

debugQuit :: IDEAction
debugQuit = do
    maybeDebug <- readIDE debugState
    case maybeDebug of
        Just debug -> runDebug (debugCommand ":quit" logOutput) debug
        _          -> return ()

debugExecuteSelection :: IDEAction
debugExecuteSelection = do
    maybeText   <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ do
            debugSetLiberalScope
            debugCommand text logOutput
        Nothing   -> ideMessage Normal "Please select some text in the editor to execute"

debugExecuteAndShowSelection :: IDEAction
debugExecuteAndShowSelection = do
    maybeText   <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ do
            debugSetLiberalScope
            debugCommand text $ do
                (out, _) <- EL.zip (EL.fold buildOutputString "") logOutput
                lift $ insertTextAfterSelection $ " " ++ out
        Nothing   -> ideMessage Normal "Please select some text in the editor to execute"
    where
    buildOutputString :: String -> ToolOutput -> String
    buildOutputString "" (ToolOutput str) = str
    buildOutputString s  (ToolOutput str) = s ++ "\n" ++ str
    buildOutputString s  _                = s

debugSetLiberalScope :: DebugAction
debugSetLiberalScope = do
    maybeModuleName <- lift selectedModuleName
    case maybeModuleName of
        Just moduleName -> do
            debugCommand' (":module *" ++ moduleName) logOutput
        Nothing -> do
            mbPackage <- lift getActivePackageDescr
            case mbPackage of
                Nothing -> return ()
                Just p -> let packageNames = map (display . modu . mdModuleId) (pdModules p)
                    in debugCommand' (foldl (\a b -> a ++ " *" ++ b) ":module " packageNames)
                        logOutput

debugAbandon :: IDEAction
debugAbandon = packageTry $ tryDebug $ debugCommand ":abandon" logOutput

debugBack :: IDEAction
debugBack = packageTry $ do
    currentHist' <- lift $ readIDE currentHist
    rootPath <- lift activeProjectDir
    lift $ modifyIDE_ (\ide -> ide{currentHist = min (currentHist' - 1) 0})
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":back" (logOutputForHistoricContext debugPackage)

debugForward :: IDEAction
debugForward = packageTry $ do
    currentHist' <- lift $ readIDE currentHist
    rootPath <- lift activeProjectDir
    lift $ modifyIDE_ (\ide -> ide{currentHist = currentHist' + 1})
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":forward" (logOutputForHistoricContext debugPackage)

debugStop :: IDEAction
debugStop = do
    maybeDebug <- readIDE debugState
    liftIO $ case maybeDebug of
        Just (_, ghci) -> toolProcess ghci >>= interruptProcessGroupOf
        Nothing -> return ()

debugContinue :: IDEAction
debugContinue = packageTry $ do
    rootPath <- lift $ activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":continue" (logOutputForLiveContext debugPackage)

debugDeleteAllBreakpoints :: IDEAction
debugDeleteAllBreakpoints = do
    packageTry $ tryDebug $ debugCommand ":delete *" logOutput
    setBreakpointList []

debugDeleteBreakpoint :: String -> LogRef -> IDEAction
debugDeleteBreakpoint indexString lr = do
    packageTry $ tryDebug $ debugCommand (":delete " ++ indexString) logOutput
    bl <- readIDE breakpointRefs
    setBreakpointList $ filter (/= lr) bl
    ideR <- ask
    return ()

debugForce :: IDEAction
debugForce = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ debugCommand (":force " ++ text) logOutput
        Nothing   -> ideMessage Normal "Please select an expression in the editor"

debugHistory :: IDEAction
debugHistory = packageTry $ tryDebug $ debugCommand ":history" logOutput

debugPrint :: IDEAction
debugPrint = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ debugCommand (":print " ++ text) logOutput
        Nothing   -> ideMessage Normal "Please select an name in the editor"

debugSimplePrint :: IDEAction
debugSimplePrint = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ debugCommand (":force " ++ text) logOutput
        Nothing   -> ideMessage Normal "Please select an name in the editor"

debugStep :: IDEAction
debugStep = packageTry $ do
    rootPath <- lift $ activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugSetLiberalScope
        debugCommand ":step" (logOutputForLiveContext debugPackage)

debugStepExpression :: IDEAction
debugStepExpression = do
    maybeText <- selectedTextOrCurrentLine
    packageTry $ tryDebug $ do
        debugSetLiberalScope
        debugStepExpr maybeText

debugStepExpr :: Maybe String -> DebugAction
debugStepExpr maybeText = do
    (debugPackage, _) <- ask
    rootPath <- lift $ activeProjectDir
    case maybeText of
        Just text -> debugCommand (":step " ++ text) (logOutputForLiveContext debugPackage)
        Nothing   -> lift $ ideMessage Normal "Please select an expression in the editor"

debugStepLocal :: IDEAction
debugStepLocal = packageTry $ do
    rootPath <- lift $ activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":steplocal" (logOutputForLiveContext debugPackage)

debugStepModule :: IDEAction
debugStepModule = packageTry $ do
    rootPath <- lift $ activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":stepmodule" (logOutputForLiveContext debugPackage)

debugTrace :: IDEAction
debugTrace = packageTry $ do
    rootPath <- lift $ activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":trace" $ do
            logOutputForLiveContext debugPackage
            lift $ triggerEventIDE TraceChanged
            return ()

debugTraceExpression :: IDEAction
debugTraceExpression = do
    maybeText <- selectedTextOrCurrentLine
    packageTry $ tryDebug $ do
        debugSetLiberalScope
        debugTraceExpr maybeText

debugTraceExpr :: Maybe String -> DebugAction
debugTraceExpr maybeText = do
    (debugPackage, _) <- ask
    case maybeText of
        Just text -> debugCommand (":trace " ++ text) $ do
--            rootPath <- activeProjectDir
            logOutputForLiveContext debugPackage
            lift $ triggerEventIDE TraceChanged
            return ()
        Nothing   -> lift $ ideMessage Normal "Please select an expression in the editor"


debugShowBindings :: IDEAction
debugShowBindings = packageTry $ tryDebug $ debugCommand ":show bindings" logOutput

debugShowBreakpoints :: IDEAction
debugShowBreakpoints = packageTry $ do
    rootPath <- lift activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":show breaks" (logOutputForBreakpoints debugPackage)

debugShowContext :: IDEAction
debugShowContext = packageTry $ do
    rootPath <- lift activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":show context" (logOutputForLiveContext debugPackage)

debugShowModules :: IDEAction
debugShowModules = packageTry $ tryDebug $ debugCommand ":show modules" $
    logOutputLines_ $ \log output -> liftIO $ do
        case output of
            ToolInput  line -> appendLog log (line ++ "\n") InputTag
            ToolOutput line | ", interpreted )" `isSuffixOf` line
                            -> appendLog log (line ++ "\n") LogTag
            ToolOutput line -> appendLog log (line ++ "\n") InfoTag
            ToolError  line -> appendLog log (line ++ "\n") ErrorTag
            ToolPrompt _    -> defaultLineLogger' log output
            ToolExit _      -> appendLog log "X--X--X ghci process exited unexpectedly X--X--X" FrameTag
        return ()

debugShowPackages :: IDEAction
debugShowPackages = packageTry $ tryDebug $ debugCommand ":show packages" logOutput

debugShowLanguages :: IDEAction
debugShowLanguages = packageTry $ tryDebug $ debugCommand ":show languages" logOutput

debugInformation :: IDEAction
debugInformation = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ do
            debugSetLiberalScope
            debugCommand (":info "++text) logOutput
        Nothing   -> ideMessage Normal "Please select a name in the editor"

debugKind :: IDEAction
debugKind = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ do
            debugSetLiberalScope
            debugCommand (":kind "++text) logOutput
        Nothing   -> ideMessage Normal "Please select a type in the editor"

debugType :: IDEAction
debugType = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ do
            debugSetLiberalScope
            debugCommand (":type "++text) logOutput
        Nothing   -> ideMessage Normal "Please select an expression in the editor"

debugSetBreakpoint :: IDEAction
debugSetBreakpoint = do
    rootPath <- activeProjectDir
    maybeModuleName <- selectedModuleName
    case maybeModuleName of
        Just moduleName -> do
            -- ###           debugCommand (":add *"++moduleName) $ logOutputForBuild True
            maybeText <- selectedText
            case maybeText of
                Just text -> packageTry $ tryDebug $ do
                    (debugPackage, _) <- ask
                    debugCommand' (":module *" ++ moduleName) logOutput
                    debugCommand (":break " ++ text) (logOutputForSetBreakpoint debugPackage)
                Nothing   -> do
                    maybeLocation <- selectedLocation
                    case maybeLocation of
                        Just (line, lineOffset) -> packageTry $ tryDebug $ do
                            (debugPackage, _) <- ask
                            debugCommand (":break " ++ moduleName ++ " " ++ (show (line+1)) ++ " " ++
                                (show lineOffset)) (logOutputForSetBreakpoint debugPackage)
                        Nothing -> ideMessage Normal "Unknown error setting breakpoint"
            ref <- ask
            return ()
        Nothing   -> ideMessage Normal "Please select module file in the editor"

debugSet :: (Bool -> String) -> Bool -> IDEAction
debugSet flag value = do
    packageTry $ tryDebug $ debugCommand (":set "++(flag value)) logOutput

debugSetPrintEvldWithShow :: Bool -> IDEAction
debugSetPrintEvldWithShow = debugSet printEvldWithShowFlag

debugSetBreakOnException :: Bool -> IDEAction
debugSetBreakOnException = debugSet breakOnExceptionFlag

debugSetBreakOnError :: Bool -> IDEAction
debugSetBreakOnError = debugSet breakOnErrorFlag

debugSetPrintBindResult :: Bool -> IDEAction
debugSetPrintBindResult = debugSet printBindResultFlag


