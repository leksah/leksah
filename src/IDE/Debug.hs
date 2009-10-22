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
    executeDebugCommand
,   debugCommand
,   debugCommand'
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

,   interactiveFlags
,   debugSetPrintEvldWithShow
,   debugSetBreakOnException
,   debugSetBreakOnError
,   debugSetPrintBindResult
) where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
-- import System.Process (getProcessExitCode, ProcessHandle(..))
-- import Data.Maybe (isNothing)
import GHC.ConsoleHandler (Handler(..), installHandler)
import System.Win32
    (-- th32SnapEnumProcesses,
     DWORD(..),
     cTRL_BREAK_EVENT,
     generateConsoleCtrlEvent)
     -- tH32CS_SNAPPROCESS,
     -- withTh32Snap)
import System.Process.Internals
    (withProcessHandle, ProcessHandle__(..))
import Control.Concurrent.MVar (tryTakeMVar)
#else
import System.Posix
    (sigINT,
     installHandler,
     signalProcessGroup,
     getProcessGroupID)
import System.Posix.Signals (Handler(..))
#endif

import Control.Monad.Reader
import IDE.Core.State
import IDE.Tool
import IDE.LogRef
import Control.Exception (SomeException(..))
import IDE.Pane.SourceBuffer
    (selectedLocation,
     selectedModuleName,
     selectedText,
     selectedTextOrCurrentLine,
     insertTextAfterSelection)
import IDE.Pane.Log (appendLog)
import Data.List (isSuffixOf)
import IDE.Metainfo.Provider (getActivePackageDescr)
import Distribution.Text (display)

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
foreign import stdcall unsafe "winbase.h GetCurrentProcessId"
    c_GetCurrentProcessId :: IO DWORD

foreign import stdcall unsafe "winbase.h GetProcessId"
    c_GetProcessId :: DWORD -> IO DWORD
#endif

executeDebugCommand :: String -> ([ToolOutput] -> IDEAction) -> IDEAction
executeDebugCommand command handler = do
    maybeGhci <- readIDE ghciState
    case maybeGhci of
        Just ghci -> do
            triggerEventIDE (StatusbarChanged [CompartmentState command])
            reifyIDE $ \ideR -> do
                executeGhciCommand ghci command $ \output ->
                    reflectIDE (do
                        handler output
                        triggerEventIDE (StatusbarChanged [CompartmentState ""])
                        return ()
                        ) ideR
        _ -> sysMessage Normal "Debugger not running"

debugCommand :: String -> ([ToolOutput] -> IDEAction) -> IDEAction
debugCommand command handler = debugCommand' command
    (\to -> do
        handler to
        triggerEventIDE VariablesChanged
        return ())

debugCommand' :: String -> ([ToolOutput] -> IDEAction) -> IDEAction
debugCommand' command handler = do
    ideR <- ask
    catchIDE (executeDebugCommand command handler)
        (\(e :: SomeException) -> putStrLn (show e))

debugQuit :: IDEAction
debugQuit = debugCommand ":quit" logOutput

debugExecuteSelection :: IDEAction
debugExecuteSelection = do
    maybeText   <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> do
            debugSetLiberalScope
            debugCommand text logOutput
        Nothing   -> ideMessage Normal "Please select some text in the editor to execute"

debugExecuteAndShowSelection :: IDEAction
debugExecuteAndShowSelection = do
    maybeText   <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> do
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

debugSetLiberalScope :: IDEAction
debugSetLiberalScope = do
    maybeModuleName <- selectedModuleName
    case maybeModuleName of
        Just moduleName -> do
            debugCommand (":module *" ++ moduleName) (\ _ -> return ())
        Nothing -> do
            mbPackage <- getActivePackageDescr
            case mbPackage of
                Nothing -> return ()
                Just p -> let packageNames = map (display . modu . moduleIdMD) (exposedModulesPD p)
                    in debugCommand' (foldl (\a b -> a ++ " *" ++ b) ":module + " packageNames)
                        (\ _ -> return ())

debugAbandon :: IDEAction
debugAbandon = debugCommand ":abandon" logOutput

debugBack :: IDEAction
debugBack = do
    currentHist' <- readIDE currentHist
    modifyIDE_ (\ide -> ide{currentHist = min (currentHist' - 1) 0})
    debugCommand ":back" logOutputForHistoricContext

debugForward :: IDEAction
debugForward = do
    currentHist' <- readIDE currentHist
    modifyIDE_ (\ide -> ide{currentHist = currentHist' + 1})
    debugCommand ":forward" logOutputForHistoricContext


debugStop :: IDEAction
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
debugStop = do
    maybeGhci <- readIDE ghciState
    case maybeGhci of
        Just ghci -> do
            liftIO $ do
                maybeProcess <- tryTakeMVar (toolProcess ghci)
                processGroupId <- case maybeProcess of
                    Just h -> do
                        withProcessHandle h (\h2 -> do
                            case h2 of
                                OpenHandle oh -> do
                                    pid <- c_GetProcessId oh
                                    return (h2, pid)
                                _ -> return (h2, 0))
                    _ -> return 0
                old <- installHandler Ignore
                putStrLn $ show processGroupId
                generateConsoleCtrlEvent cTRL_BREAK_EVENT processGroupId
                installHandler old
                return ()
        Nothing -> return ()
#else
debugStop = do
    maybeGhci <- readIDE ghciState
    case maybeGhci of
        Just ghci -> liftIO $ do
            group <- getProcessGroupID
            old_int <- installHandler sigINT Ignore Nothing
            signalProcessGroup sigINT group
            installHandler sigINT old_int Nothing
            return ()
        Nothing -> return ()
#endif

debugContinue :: IDEAction
debugContinue = debugCommand ":continue" logOutputForLiveContext

debugDeleteAllBreakpoints :: IDEAction
debugDeleteAllBreakpoints = do
    debugCommand ":delete *" $ \output -> do
        logOutput output
    setBreakpointList []

debugDeleteBreakpoint :: String -> LogRef -> IDEAction
debugDeleteBreakpoint indexString lr = do
    debugCommand (":delete " ++ indexString) $ \output -> do
        logOutput output
    bl <- readIDE breakpointRefs
    setBreakpointList $ filter (/= lr) bl
    ideR <- ask
    return ()

debugForce :: IDEAction
debugForce = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> debugCommand (":force " ++ text) logOutput
        Nothing   -> ideMessage Normal "Please select an expression in the editor"

debugHistory :: IDEAction
debugHistory = debugCommand ":history" logOutput

debugPrint :: IDEAction
debugPrint = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> debugCommand (":print " ++ text) logOutput
        Nothing   -> ideMessage Normal "Please select an name in the editor"

debugSimplePrint :: IDEAction
debugSimplePrint = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> debugCommand (":force " ++ text) logOutput
        Nothing   -> ideMessage Normal "Please select an name in the editor"

debugStep :: IDEAction
debugStep = do
    debugSetLiberalScope
    debugCommand ":step" logOutputForLiveContext

debugStepExpression :: IDEAction
debugStepExpression = do
    maybeText <- selectedTextOrCurrentLine
    debugSetLiberalScope
    debugStepExpr maybeText

debugStepExpr :: Maybe String -> IDEAction
debugStepExpr maybeText = do
    case maybeText of
        Just text -> debugCommand (":step " ++ text) logOutputForLiveContext
        Nothing   -> ideMessage Normal "Please select an expression in the editor"

debugStepLocal :: IDEAction
debugStepLocal = debugCommand ":steplocal" logOutputForLiveContext

debugStepModule :: IDEAction
debugStepModule = debugCommand ":stepmodule" logOutputForLiveContext

debugTrace :: IDEAction
debugTrace = do
    debugCommand ":trace" (\to -> do
        logOutputForLiveContext to
        triggerEventIDE TraceChanged
        return ())

debugTraceExpression :: IDEAction
debugTraceExpression = do
    maybeText <- selectedTextOrCurrentLine
    debugSetLiberalScope
    debugTraceExpr maybeText

debugTraceExpr :: Maybe String -> IDEAction
debugTraceExpr maybeText =
    case maybeText of
        Just text -> debugCommand (":trace " ++ text) (\to -> do
            logOutputForLiveContext to
            triggerEventIDE TraceChanged
            return ())
        Nothing   -> ideMessage Normal "Please select an expression in the editor"


debugShowBindings :: IDEAction
debugShowBindings = debugCommand ":show bindings" logOutput

debugShowBreakpoints :: IDEAction
debugShowBreakpoints = debugCommand ":show breaks" logOutputForBreakpoints

debugShowContext :: IDEAction
debugShowContext = debugCommand ":show context" logOutputForLiveContext

debugShowModules :: IDEAction
debugShowModules = debugCommand ":show modules" $
    logOutputLines_ $ \log output -> liftIO $ do
        case output of
            ToolInput  line -> appendLog log (line ++ "\n") InputTag
            ToolOutput line | ", interpreted )" `isSuffixOf` line
                            -> appendLog log (line ++ "\n") LogTag
            ToolOutput line -> appendLog log (line ++ "\n") InfoTag
            ToolError  line -> appendLog log (line ++ "\n") ErrorTag
        return ()

debugShowPackages :: IDEAction
debugShowPackages = debugCommand ":show packages" logOutput

debugShowLanguages :: IDEAction
debugShowLanguages = debugCommand ":show languages" logOutput

debugInformation :: IDEAction
debugInformation = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> do
            debugSetLiberalScope
            debugCommand (":info "++text) logOutput
        Nothing   -> ideMessage Normal "Please select a name in the editor"

debugKind :: IDEAction
debugKind = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> do
            debugSetLiberalScope
            debugCommand (":kind "++text) logOutput
        Nothing   -> ideMessage Normal "Please select a type in the editor"

debugType :: IDEAction
debugType = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> do
            debugSetLiberalScope
            debugCommand (":type "++text) logOutput
        Nothing   -> ideMessage Normal "Please select an expression in the editor"

debugSetBreakpoint :: IDEAction
debugSetBreakpoint = do
    maybeModuleName <- selectedModuleName
    case maybeModuleName of
        Just moduleName -> do
            -- ###           debugCommand (":add *"++moduleName) $ logOutputForBuild True
            maybeText <- selectedText
            case maybeText of
                Just text -> do
                    debugCommand (":module *" ++ moduleName) logOutput
                    debugCommand (":break " ++ text) logOutputForSetBreakpoint
                Nothing   -> do
                    maybeLocation <- selectedLocation
                    case maybeLocation of
                        Just (line, lineOffset) ->
                            debugCommand (":break " ++ moduleName ++ " " ++ (show (line+1)) ++ " " ++
                                (show lineOffset)) logOutputForSetBreakpoint
                        Nothing -> ideMessage Normal "Unknown error setting breakpoint"
            ref <- ask
            return ()
        Nothing   -> ideMessage Normal "Please select module file in the editor"

interactiveFlag :: String -> Bool -> String
interactiveFlag name f = (if f then "-f" else "-fno-") ++ name

printEvldWithShowFlag :: Bool -> String
printEvldWithShowFlag = interactiveFlag "print-evld-with-show"

breakOnExceptionFlag :: Bool -> String
breakOnExceptionFlag = interactiveFlag "break-on-exception"

breakOnErrorFlag :: Bool -> String
breakOnErrorFlag = interactiveFlag "break-on-error"

printBindResultFlag :: Bool -> String
printBindResultFlag = interactiveFlag "print-bind-result"

interactiveFlags :: Prefs -> [String]
interactiveFlags prefs =
    (printEvldWithShowFlag $ printEvldWithShow prefs)
    : (breakOnExceptionFlag $ breakOnException prefs)
    : (breakOnErrorFlag $ breakOnError prefs)
    : [printBindResultFlag $ printBindResult prefs]

debugSet :: (Bool -> String) -> Bool -> IDEAction
debugSet flag value = do
    debugCommand (":set "++(flag value)) logOutput

debugSetPrintEvldWithShow :: Bool -> IDEAction
debugSetPrintEvldWithShow = debugSet printEvldWithShowFlag

debugSetBreakOnException :: Bool -> IDEAction
debugSetBreakOnException = debugSet breakOnExceptionFlag

debugSetBreakOnError :: Bool -> IDEAction
debugSetBreakOnError = debugSet breakOnErrorFlag

debugSetPrintBindResult :: Bool -> IDEAction
debugSetPrintBindResult = debugSet printBindResultFlag


