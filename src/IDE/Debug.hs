{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.List (stripPrefix, isSuffixOf)
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
import Control.Applicative (Alternative(..))

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
stripComments :: String -> String
stripComments t = maybe t unlines $
        sequence (map (stripPrefix "-- >>>") lines')
    <|> sequence (map (stripPrefix "-- >") lines')
  where
    lines' = lines t

debugExecuteSelection :: IDEAction
debugExecuteSelection = do
    maybeText   <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> do
            let command = packageTry $ tryDebug $ do
                debugSetLiberalScope
                debugCommand (stripComments text) logOutputDefault
            modifyIDE_ $ \ide -> ide {autoCommand = command}
            command
        Nothing   -> ideMessage Normal "Please select some text in the editor to execute"

debugExecuteAndShowSelection :: IDEAction
debugExecuteAndShowSelection = do
    maybeText   <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ do
            debugSetLiberalScope
            debugCommand (stripComments text) $ do
                (out, _) <- EL.zip (EL.fold buildOutputString "") logOutputDefault
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
            debugCommand (":module *" ++ moduleName) (return ())
        Nothing -> do
            mbPackage <- lift getActivePackageDescr
            case mbPackage of
                Nothing -> return ()
                Just p -> let packageNames = map (display . modu . mdModuleId) (pdModules p)
                    in debugCommand' (foldl (\a b -> a ++ " *" ++ b) ":module + " packageNames)
                        (return ())

debugAbandon :: IDEAction
debugAbandon = do

    packageTry $ tryDebug $ debugCommand ":abandon" logOutputDefault

debugBack :: IDEAction
debugBack = packageTry $ do
    currentHist' <- lift $ readIDE currentHist
    rootPath <- lift activeProjectDir
    lift $ modifyIDE_ (\ide -> ide{currentHist = min (currentHist' - 1) 0})
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":back" (logOutputForHistoricContextDefault debugPackage)

debugForward :: IDEAction
debugForward = packageTry $ do
    currentHist' <- lift $ readIDE currentHist
    rootPath <- lift activeProjectDir
    lift $ modifyIDE_ (\ide -> ide{currentHist = currentHist' + 1})
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
debugContinue = packageTry $ do
    rootPath <- lift $ activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":continue" (logOutputForHistoricContextDefault debugPackage)

debugDeleteAllBreakpoints :: IDEAction
debugDeleteAllBreakpoints = do
    packageTry $ tryDebug $ debugCommand ":delete *" logOutputDefault
    setBreakpointList []

debugDeleteBreakpoint :: String -> LogRef -> IDEAction
debugDeleteBreakpoint indexString lr = do
    packageTry $ tryDebug $ debugCommand (":delete " ++ indexString) logOutputDefault
    bl <- readIDE breakpointRefs
    setBreakpointList $ filter (/= lr) bl
    ideR <- ask
    return ()

debugForce :: IDEAction
debugForce = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ debugCommand (":force " ++ stripComments text) logOutputDefault
        Nothing   -> ideMessage Normal "Please select an expression in the editor"

debugHistory :: IDEAction
debugHistory = packageTry $ tryDebug $ debugCommand ":history" logOutputDefault

debugPrint :: IDEAction
debugPrint = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ debugCommand (":print " ++ stripComments text) logOutputDefault
        Nothing   -> ideMessage Normal "Please select an name in the editor"

debugSimplePrint :: IDEAction
debugSimplePrint = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ debugCommand (":force " ++ stripComments text) logOutputDefault
        Nothing   -> ideMessage Normal "Please select an name in the editor"

debugStep :: IDEAction
debugStep = packageTry $ do
    rootPath <- lift $ activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugSetLiberalScope
        debugCommand ":step" (logOutputForHistoricContextDefault debugPackage)

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
        Just text -> debugCommand (":step " ++ stripComments text) (logOutputForHistoricContextDefault debugPackage)
        Nothing   -> lift $ ideMessage Normal "Please select an expression in the editor"

debugStepLocal :: IDEAction
debugStepLocal = packageTry $ do
    rootPath <- lift $ activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":steplocal" (logOutputForHistoricContextDefault debugPackage)

debugStepModule :: IDEAction
debugStepModule = packageTry $ do
    rootPath <- lift $ activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":stepmodule" (logOutputForHistoricContextDefault debugPackage)


logTraceOutput debugPackage = do
    logOutputForLiveContextDefault debugPackage
    lift $ triggerEventIDE TraceChanged
    return ()

debugTrace :: IDEAction
debugTrace = packageTry $ do
    rootPath <- lift $ activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":trace" $ logTraceOutput debugPackage

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
        Just text -> debugCommand (":trace " ++ stripComments text) $ logTraceOutput debugPackage
        Nothing   -> lift $ ideMessage Normal "Please select an expression in the editor"


debugShowBindings :: IDEAction
debugShowBindings = packageTry $ tryDebug $ debugCommand ":show bindings" logOutputDefault

debugShowBreakpoints :: IDEAction
debugShowBreakpoints = packageTry $ do
    rootPath <- lift activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":show breaks" (logOutputForSetBreakpointDefault debugPackage)

debugShowContext :: IDEAction
debugShowContext = packageTry $ do
    rootPath <- lift activeProjectDir
    tryDebug $ do
        (debugPackage, _) <- ask
        debugCommand ":show context" (logOutputForHistoricContextDefault debugPackage)

debugShowModules :: IDEAction
debugShowModules = packageTry $ tryDebug $ debugCommand ":show modules" $
    logOutputLines_Default $ \log logLaunch output -> liftIO $ do
        case output of
            ToolInput  line -> appendLog log logLaunch (line ++ "\n") InputTag
            ToolOutput line | ", interpreted )" `isSuffixOf` line
                            -> appendLog log logLaunch (line ++ "\n") LogTag
            ToolOutput line -> appendLog log logLaunch (line ++ "\n") InfoTag
            ToolError  line -> appendLog log logLaunch (line ++ "\n") ErrorTag
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
            debugCommand (":info "++stripComments text) logOutputDefault
        Nothing   -> ideMessage Normal "Please select a name in the editor"

debugKind :: IDEAction
debugKind = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ do
            debugSetLiberalScope
            debugCommand (":kind "++stripComments text) logOutputDefault
        Nothing   -> ideMessage Normal "Please select a type in the editor"

debugType :: IDEAction
debugType = do
    maybeText <- selectedTextOrCurrentLine
    case maybeText of
        Just text -> packageTry $ tryDebug $ do
            debugSetLiberalScope
            debugCommand (":type "++stripComments text) logOutputDefault
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
                    debugCommand (":module *" ++ moduleName) logOutputDefault
                    debugCommand (":break " ++ text) (logOutputForSetBreakpointDefault debugPackage)
                Nothing   -> do
                    maybeLocation <- selectedLocation
                    case maybeLocation of
                        Just (line, lineOffset) -> packageTry $ tryDebug $ do
                            (debugPackage, _) <- ask
                            debugCommand (":break " ++ moduleName ++ " " ++ (show (line+1)) ++ " " ++
                                (show lineOffset)) (logOutputForSetBreakpointDefault debugPackage)
                        Nothing -> ideMessage Normal "Unknown error setting breakpoint"
            ref <- ask
            return ()
        Nothing   -> ideMessage Normal "Please select module file in the editor"

debugSet :: (Bool -> String) -> Bool -> IDEAction
debugSet flag value = do
    packageTry $ tryDebug $ debugCommand (":set "++(flag value)) logOutputDefault

debugSetPrintEvldWithShow :: Bool -> IDEAction
debugSetPrintEvldWithShow = debugSet printEvldWithShowFlag

debugSetBreakOnException :: Bool -> IDEAction
debugSetBreakOnException = debugSet breakOnExceptionFlag

debugSetBreakOnError :: Bool -> IDEAction
debugSetBreakOnError = debugSet breakOnErrorFlag

debugSetPrintBindResult :: Bool -> IDEAction
debugSetPrintBindResult = debugSet printBindResultFlag


