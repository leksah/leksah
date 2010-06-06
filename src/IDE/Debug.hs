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

import Control.Monad.Reader
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
import IDE.System.Process (interruptProcessGroup)
import IDE.Utils.GUIUtils (getDebugToggled)
import IDE.Package (debugStart, executeDebugCommand, tryDebug_, printBindResultFlag,
        breakOnErrorFlag, breakOnExceptionFlag, printEvldWithShowFlag)
import IDE.Utils.Tool (ToolOutput(..), toolProcess)

debugCommand :: String -> ([ToolOutput] -> IDEAction) -> DebugAction
debugCommand command handler = debugCommand' command
    (\to -> do
        handler to
        triggerEventIDE VariablesChanged
        return ())

debugCommand' :: String -> ([ToolOutput] -> IDEAction) -> DebugAction
debugCommand' command handler = do
    ghci <- ask
    liftIDEM $ catchIDE (runDebug (executeDebugCommand command handler) ghci)
        (\(e :: SomeException) -> putStrLn (show e))

debugToggled :: IDEAction
debugToggled = do
    toggled <- getDebugToggled
    maybeGhci <- readIDE ghciState
    case (toggled, maybeGhci) of
        (True, Nothing) -> debugStart
        (False, Just _) -> debugQuit
        _               -> return ()

debugQuit :: IDEAction
debugQuit = do
    maybeGhci <- readIDE ghciState
    case maybeGhci of
        Just ghci -> runDebug (debugCommand ":quit" logOutput) ghci
        _         -> return ()

debugExecuteSelection :: IDEAction
debugExecuteSelection = do
    maybeText   <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> tryDebug_ $ do
            debugSetLiberalScope
            debugCommand text logOutput
        Nothing   -> ideMessage Normal "Please select some text in the editor to execute"

debugExecuteAndShowSelection :: IDEAction
debugExecuteAndShowSelection = do
    maybeText   <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> tryDebug_ $ do
            debugSetLiberalScope
            debugCommand text (\to -> do
                insertTextAfterSelection $ " " ++ buildOutputString to
                logOutput to)
        Nothing   -> ideMessage Normal "Please select some text in the editor to execute"
    where
    buildOutputString :: [ToolOutput] -> String
    buildOutputString (ToolOutput str:[]) = str
    buildOutputString (ToolOutput str:r)  = str ++ "\n" ++ (buildOutputString r)
    buildOutputString (_:r)               = buildOutputString r
    buildOutputString []                  = ""

debugSetLiberalScope :: DebugAction
debugSetLiberalScope = do
    maybeModuleName <- liftIDEM selectedModuleName
    case maybeModuleName of
        Just moduleName -> do
            debugCommand (":module *" ++ moduleName) (\ _ -> return ())
        Nothing -> do
            mbPackage <- liftIDEM getActivePackageDescr
            case mbPackage of
                Nothing -> return ()
                Just p -> let packageNames = map (display . modu . mdModuleId) (pdModules p)
                    in debugCommand' (foldl (\a b -> a ++ " *" ++ b) ":module + " packageNames)
                        (\ _ -> return ())

debugAbandon :: IDEAction
debugAbandon = tryDebug_ $ debugCommand ":abandon" logOutput

debugBack :: IDEAction
debugBack = do
    currentHist' <- readIDE currentHist
    rootPath <- activeProjectDir
    modifyIDE_ (\ide -> ide{currentHist = min (currentHist' - 1) 0})
    tryDebug_ $ debugCommand ":back" (logOutputForHistoricContext rootPath)

debugForward :: IDEAction
debugForward = do
    currentHist' <- readIDE currentHist
    rootPath <- activeProjectDir
    modifyIDE_ (\ide -> ide{currentHist = currentHist' + 1})
    tryDebug_ $ debugCommand ":forward" (logOutputForHistoricContext rootPath)


debugStop :: IDEAction
debugStop = do
    maybeGhci <- readIDE ghciState
    liftIO $ case maybeGhci of
        Just ghci -> toolProcess ghci >>= interruptProcessGroup
        Nothing -> return ()

debugContinue :: IDEAction
debugContinue = do
    rootPath <- activeProjectDir
    tryDebug_ $ debugCommand ":continue" (logOutputForLiveContext rootPath)

debugDeleteAllBreakpoints :: IDEAction
debugDeleteAllBreakpoints = do
    tryDebug_ $ debugCommand ":delete *" $ \output -> do
        logOutput output
    setBreakpointList []

debugDeleteBreakpoint :: String -> LogRef -> IDEAction
debugDeleteBreakpoint indexString lr = do
    tryDebug_ $ debugCommand (":delete " ++ indexString) $ \output -> do
        logOutput output
    bl <- readIDE breakpointRefs
    setBreakpointList $ filter (/= lr) bl
    ideR <- ask
    return ()

debugForce :: IDEAction
debugForce = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> tryDebug_ $ debugCommand (":force " ++ text) logOutput
        Nothing   -> ideMessage Normal "Please select an expression in the editor"

debugHistory :: IDEAction
debugHistory = tryDebug_ $ debugCommand ":history" logOutput

debugPrint :: IDEAction
debugPrint = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> tryDebug_ $ debugCommand (":print " ++ text) logOutput
        Nothing   -> ideMessage Normal "Please select an name in the editor"

debugSimplePrint :: IDEAction
debugSimplePrint = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> tryDebug_ $ debugCommand (":force " ++ text) logOutput
        Nothing   -> ideMessage Normal "Please select an name in the editor"

debugStep :: IDEAction
debugStep = do
    rootPath <- activeProjectDir
    tryDebug_ $ do
        debugSetLiberalScope
        debugCommand ":step" (logOutputForLiveContext rootPath)

debugStepExpression :: IDEAction
debugStepExpression = do
    maybeText <- selectedTextOrCurrentLine
    tryDebug_ $ do
        debugSetLiberalScope
        debugStepExpr maybeText

debugStepExpr :: Maybe String -> DebugAction
debugStepExpr maybeText = do
    rootPath <- liftIDEM $ activeProjectDir
    case maybeText of
        Just text -> debugCommand (":step " ++ text) (logOutputForLiveContext rootPath)
        Nothing   -> liftIDEM $ ideMessage Normal "Please select an expression in the editor"

debugStepLocal :: IDEAction
debugStepLocal = do
    rootPath <- activeProjectDir
    tryDebug_ $ debugCommand ":steplocal" (logOutputForLiveContext rootPath)

debugStepModule :: IDEAction
debugStepModule = do
    rootPath <- activeProjectDir
    tryDebug_ $ debugCommand ":stepmodule" (logOutputForLiveContext rootPath)

debugTrace :: IDEAction
debugTrace = do
    rootPath <- activeProjectDir
    tryDebug_ $ debugCommand ":trace" (\to -> do
        logOutputForLiveContext rootPath to
        triggerEventIDE TraceChanged
        return ())

debugTraceExpression :: IDEAction
debugTraceExpression = do
    maybeText <- selectedTextOrCurrentLine
    tryDebug_ $ do
        debugSetLiberalScope
        debugTraceExpr maybeText

debugTraceExpr :: Maybe String -> DebugAction
debugTraceExpr maybeText =
    case maybeText of
        Just text -> debugCommand (":trace " ++ text) (\to -> do
            rootPath <- activeProjectDir
            logOutputForLiveContext rootPath to
            triggerEventIDE TraceChanged
            return ())
        Nothing   -> liftIDEM $ ideMessage Normal "Please select an expression in the editor"


debugShowBindings :: IDEAction
debugShowBindings = tryDebug_ $ debugCommand ":show bindings" logOutput

debugShowBreakpoints :: IDEAction
debugShowBreakpoints = do
    rootPath <- activeProjectDir
    tryDebug_ $ debugCommand ":show breaks" (logOutputForBreakpoints rootPath)

debugShowContext :: IDEAction
debugShowContext = do
    rootPath <- activeProjectDir
    tryDebug_ $ debugCommand ":show context" (logOutputForLiveContext rootPath)

debugShowModules :: IDEAction
debugShowModules = tryDebug_ $ debugCommand ":show modules" $
    logOutputLines_ $ \log output -> liftIO $ do
        case output of
            ToolInput  line -> appendLog log (line ++ "\n") InputTag
            ToolOutput line | ", interpreted )" `isSuffixOf` line
                            -> appendLog log (line ++ "\n") LogTag
            ToolOutput line -> appendLog log (line ++ "\n") InfoTag
            ToolError  line -> appendLog log (line ++ "\n") ErrorTag
            ToolPrompt      -> defaultLineLogger' log output
            ToolExit _      -> appendLog log "X--X--X ghci process exited unexpectedly X--X--X" FrameTag
        return ()

debugShowPackages :: IDEAction
debugShowPackages = tryDebug_ $ debugCommand ":show packages" logOutput

debugShowLanguages :: IDEAction
debugShowLanguages = tryDebug_ $ debugCommand ":show languages" logOutput

debugInformation :: IDEAction
debugInformation = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> tryDebug_ $ do
            debugSetLiberalScope
            debugCommand (":info "++text) logOutput
        Nothing   -> ideMessage Normal "Please select a name in the editor"

debugKind :: IDEAction
debugKind = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> tryDebug_ $ do
            debugSetLiberalScope
            debugCommand (":kind "++text) logOutput
        Nothing   -> ideMessage Normal "Please select a type in the editor"

debugType :: IDEAction
debugType = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> tryDebug_ $ do
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
                Just text -> tryDebug_ $ do
                    debugCommand (":module *" ++ moduleName) logOutput
                    debugCommand (":break " ++ text) (logOutputForSetBreakpoint rootPath)
                Nothing   -> do
                    maybeLocation <- selectedLocation
                    case maybeLocation of
                        Just (line, lineOffset) -> tryDebug_ $
                            debugCommand (":break " ++ moduleName ++ " " ++ (show (line+1)) ++ " " ++
                                (show lineOffset)) (logOutputForSetBreakpoint rootPath)
                        Nothing -> ideMessage Normal "Unknown error setting breakpoint"
            ref <- ask
            return ()
        Nothing   -> ideMessage Normal "Please select module file in the editor"

debugSet :: (Bool -> String) -> Bool -> IDEAction
debugSet flag value = do
    tryDebug_ $ debugCommand (":set "++(flag value)) logOutput

debugSetPrintEvldWithShow :: Bool -> IDEAction
debugSetPrintEvldWithShow = debugSet printEvldWithShowFlag

debugSetBreakOnException :: Bool -> IDEAction
debugSetBreakOnException = debugSet breakOnExceptionFlag

debugSetBreakOnError :: Bool -> IDEAction
debugSetBreakOnError = debugSet breakOnErrorFlag

debugSetPrintBindResult :: Bool -> IDEAction
debugSetPrintBindResult = debugSet printBindResultFlag


