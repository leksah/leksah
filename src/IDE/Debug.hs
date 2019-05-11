{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, LambdaCase #-}
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

import Prelude ()
import Prelude.Compat
import IDE.Core.Types
       (LogTag(..), IDEPackage(..), Project(..), runPackage, runProject,
        IDEEvent(..), MonadIDE(..), Prefs(..))
import IDE.Core.CTypes (pdModules, mdModuleId, modu)
import IDE.Core.State
       (debugState, breakpointRefs, packageDebugState, MessageLevel(..),
        ideMessage, readIDE, modifyIDE_, runDebug, catchIDE,
        triggerEventIDE_, LogRef, currentHist, autoURI, autoCommand,
        PackageAction, prefs, IDEAction, DebugAction, IDEM, DebugState(..))
import IDE.Gtk.State (postSyncIDE)
import IDE.LogRef
import Control.Exception (SomeException(..))
import IDE.Pane.SourceBuffer
       (IDEBuffer, belongsToPackages', selectedLocation, selectedText,
        selectedModuleName, insertTextAfterSelection,
        selectedTextOrCurrentLine)
import IDE.Metainfo.Provider (getActivePackageDescr)
import Distribution.Text (display)
import IDE.Pane.Log
import Data.List (intersperse)
import IDE.Package (executeDebugCommand, printBindResultFlag,
        breakOnErrorFlag, breakOnExceptionFlag, printEvldWithShowFlag)
import IDE.Gtk.Package (tryDebug)
import IDE.Utils.Tool (ToolOutput(..), toolProcess, interruptProcessGroupOf)
import IDE.Gtk.Workspaces (workspaceTry, packageTry)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Applicative (Alternative(..), (<$>), (<*>))
import Control.Lens ((%~), (.~))
import Data.IORef (newIORef)
import Data.Text (Text)
import qualified Data.Text as T
       (concat, pack, lines, stripPrefix, isSuffixOf, unpack)
import IDE.Pane.WebKit.Output (loadOutputUri)
import qualified Data.Sequence as Seq (filter, empty)
import Control.Monad (void, unless)
import Data.Conduit (ConduitT)
import Data.Void (Void)

-- | Get the last item
sinkLast :: Monad m => ConduitT a o m (Maybe a)
sinkLast = CL.fold (\_ a -> Just a) Nothing

debugCommand :: Text -> ConduitT ToolOutput Void IDEM () -> DebugAction
debugCommand command handler = do
    debugCommand' command handler
    lift $ triggerEventIDE_ VariablesChanged

debugCommand' :: Text -> ConduitT ToolOutput Void IDEM () -> DebugAction
debugCommand' command handler = do
    ghci <- ask
    lift $ catchIDE (runDebug (executeDebugCommand command handler) ghci)
        (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

debugToggled :: Bool -> IDEAction
debugToggled toggled = do
    modifyIDE_ $ prefs %~ (\p -> p{debug = toggled})
    unless toggled $
        readIDE debugState >>= mapM_ (runDebug (debugCommand ":quit" logOutputDefault))

debugQuit :: PackageAction
debugQuit =
    packageDebugState >>=
        liftIDE . mapM_ (runDebug (debugCommand ":quit" logOutputDefault))

-- | Remove haddock code prefix from selected text so it can be run
-- in ghci
--
-- Press Ctrl + Enter on these to try it out...
--
-- > stripComments $ T.pack "-- > Wow this is meta"
--
-- > stripComments $ T.pack "-- This is still a comment"
stripComments :: Text -> Text
stripComments t = maybe t (T.concat . intersperse "\n") $
        mapM (T.stripPrefix "-- >>>") lines'
    <|> mapM (T.stripPrefix "-- >") lines'
    <|> stripBlock lines'
  where
    lines' = T.lines t

    stripBlock ("-- > :{":rest) =
        case reverse rest of
            "-- :}":revBody -> mapM (T.stripPrefix "-- ") (reverse revBody)
            _ -> Nothing
    stripBlock ("-- >>> :{":rest) =
        case reverse rest of
            "-- :}":revBody -> mapM (T.stripPrefix "-- ") (reverse revBody)
            _ -> Nothing
    stripBlock (first:rest) | "> :{" `T.isSuffixOf` first =
        case reverse rest of
            ":}":revBody -> Just $ reverse revBody
            _ -> Nothing
    stripBlock _ = Nothing

debugExecuteSelection :: IDEAction
debugExecuteSelection = do
    maybeText   <- selectedTextOrCurrentLine
    case maybeText of
        Nothing   -> ideMessage Normal "Please select some text in the editor to execute"
        Just (buf, text) -> do
            let command = do
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
            mbAutoCmd <- belongsToPackages' buf >>= \case
                [] -> return Nothing -- (("", ""), packageTry $ tryDebug command)
                (project, package):_ -> return $ Just ((pjKey project, ipdCabalFile package),
                    workspaceTry . (`runProject` project) . (`runPackage` package) $ tryDebug command)
            modifyIDE_ $ (autoCommand .~ mbAutoCmd) . (autoURI .~ Nothing)
            case mbAutoCmd of
              Just autoCmd -> snd autoCmd
              Nothing -> packageTry $ tryDebug command

debugBuffer :: IDEBuffer -> DebugAction -> IDEAction
debugBuffer buf f =
    belongsToPackages' buf >>= \case
        [] -> packageTry $ tryDebug f
        (project, package):_ -> workspaceTry $ (`runProject` project) $ (`runPackage` package) $ tryDebug f

debugSelection :: Text -> (Text -> DebugAction) -> IDEAction
debugSelection message f = do
    maybeText   <- selectedTextOrCurrentLine
    case maybeText of
        Nothing          -> ideMessage Normal message
        Just (buf, text) -> debugBuffer buf $ f text

debugSelection' :: (Maybe Text -> DebugAction) -> IDEAction
debugSelection' f = do
    maybeText   <- selectedTextOrCurrentLine
    case maybeText of
        Nothing          -> packageTry . tryDebug $ f Nothing
        Just (buf, text) -> debugBuffer buf . f $ Just text

debugExecuteAndShowSelection :: IDEAction
debugExecuteAndShowSelection =
    debugSelection "Please select some text in the editor to execute" $ \text -> do
        debugSetLiberalScope
        debugCommand (stripComments text) $ do
            out <- C.getZipSink $ const
                    <$> C.ZipSink (CL.fold buildOutputString "")
                    <*> C.ZipSink logOutputDefault
            lift . insertTextAfterSelection $ " " <> out
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
    debugSelection' $ \_ -> debugCommand ":abandon" logOutputDefault

debugBack :: IDEAction
debugBack = do
    modifyIDE_ $ currentHist %~ (max 0 . subtract 1)
    debugSelection' $ \_ -> do
        basePath <- dsBasePath <$> ask
        debugCommand ":back" (logOutputForHistoricContextDefault basePath)

debugForward :: IDEAction
debugForward = do
    modifyIDE_ $ currentHist %~ (+1)
    debugSelection' $ \_ -> do
        basePath <- dsBasePath <$> ask
        debugCommand ":forward" (logOutputForHistoricContextDefault basePath)

debugStop :: PackageAction
debugStop =
    packageDebugState >>= \case
        Just DebugState{..} -> liftIO $ toolProcess dsToolState >>= interruptProcessGroupOf
        Nothing -> return ()

debugContinue :: IDEAction
debugContinue = debugSelection' $ \_ -> do
    basePath <- dsBasePath <$> ask
    debugCommand ":continue" (logOutputForHistoricContextDefault basePath)

debugDeleteAllBreakpoints :: IDEAction
debugDeleteAllBreakpoints = do
    debugSelection' $ \_ -> debugCommand ":delete *" logOutputDefault
    setBreakpointList Seq.empty

debugDeleteBreakpoint :: Text -> LogRef -> IDEAction
debugDeleteBreakpoint indexString lr = do
    debugSelection' $ \_ -> debugCommand (":delete " <> indexString) logOutputDefault
    bl <- readIDE breakpointRefs
    setBreakpointList $ Seq.filter (/= lr) bl

debugForce :: IDEAction
debugForce = debugSelection "Please select an expression in the editor" $ \text ->
                    debugCommand (":force " <> stripComments text) logOutputDefault

debugHistory :: IDEAction
debugHistory = debugSelection' $ \_ -> debugCommand ":history" logOutputDefault

debugPrint :: IDEAction
debugPrint = debugSelection "Please select an name in the editor" $ \text ->
                    debugCommand (":print " <> stripComments text) logOutputDefault

debugSimplePrint :: IDEAction
debugSimplePrint = debugSelection "Please select an name in the editor" $ \text ->
                        debugCommand (":force " <> stripComments text) logOutputDefault

debugStep :: IDEAction
debugStep = debugSelection' $ \_ -> do
    basePath <- dsBasePath <$> ask
    debugSetLiberalScope
    debugCommand ":step" (logOutputForHistoricContextDefault basePath)

debugStepExpression :: IDEAction
debugStepExpression = debugSelection' $ \maybeText -> do
    debugSetLiberalScope
    debugStepExpr maybeText

debugStepExpr :: Maybe Text -> DebugAction
debugStepExpr maybeText = do
    basePath <- dsBasePath <$> ask
    case maybeText of
        Just text -> debugCommand (":step " <> stripComments text) (logOutputForHistoricContextDefault basePath)
        Nothing   -> lift $ ideMessage Normal "Please select an expression in the editor"

debugStepLocal :: IDEAction
debugStepLocal = debugSelection' $ \_ -> do
    basePath <- dsBasePath <$> ask
    debugCommand ":steplocal" (logOutputForHistoricContextDefault basePath)

debugStepModule :: IDEAction
debugStepModule = debugSelection' $ \_ -> do
    basePath <- dsBasePath <$> ask
    debugCommand ":stepmodule" (logOutputForHistoricContextDefault basePath)

logTraceOutput :: FilePath -> ConduitT ToolOutput Void IDEM ()
logTraceOutput debugPackage = do
    logOutputForLiveContextDefault debugPackage
    lift $ triggerEventIDE_ TraceChanged

debugTrace :: IDEAction
debugTrace = debugSelection' $ \_ -> do
    basePath <- dsBasePath <$> ask
    debugCommand ":trace" $ logTraceOutput basePath

debugTraceExpression :: IDEAction
debugTraceExpression = debugSelection' $ \maybeText -> do
    debugSetLiberalScope
    debugTraceExpr maybeText

debugTraceExpr :: Maybe Text -> DebugAction
debugTraceExpr maybeText = do
    basePath <- dsBasePath <$> ask
    case maybeText of
        Just text -> debugCommand (":trace " <> stripComments text) $ logTraceOutput basePath
        Nothing   -> lift $ ideMessage Normal "Please select an expression in the editor"


debugShowBindings :: IDEAction
debugShowBindings = debugSelection' $ \_ -> debugCommand ":show bindings" logOutputDefault

debugShowBreakpoints :: IDEAction
debugShowBreakpoints = debugSelection' $ \_ -> do
    basePath <- dsBasePath <$> ask
    debugCommand ":show breaks" (logOutputForSetBreakpointDefault basePath)

debugShowContext :: IDEAction
debugShowContext = debugSelection' $ \_ -> do
    basePath <- dsBasePath <$> ask
    debugCommand ":show context" (logOutputForHistoricContextDefault basePath)

debugShowModules :: IDEAction
debugShowModules = debugSelection' $ \_ -> debugCommand ":show modules" $
    logOutputLinesDefault_ $ \log' logLaunch output -> void $
        case output of
            ToolInput  line -> appendLog log' logLaunch (line <> "\n") InputTag
            ToolOutput line | ", interpreted )" `T.isSuffixOf` line
                            -> appendLog log' logLaunch (line <> "\n") LogTag
            ToolOutput line -> appendLog log' logLaunch (line <> "\n") InfoTag
            ToolError  line -> appendLog log' logLaunch (line <> "\n") ErrorTag
            ToolPrompt _    -> defaultLineLogger log' logLaunch output
            ToolExit _      -> appendLog log' logLaunch "X--X--X ghci process exited unexpectedly X--X--X" FrameTag

debugShowPackages :: IDEAction
debugShowPackages = debugSelection' $ \_ -> debugCommand ":show packages" logOutputDefault

debugShowLanguages :: IDEAction
debugShowLanguages = debugSelection' $ \_ -> debugCommand ":show languages" logOutputDefault

debugInformation :: IDEAction
debugInformation = debugSelection "Please select a name in the editor" $ \text -> do
    debugSetLiberalScope
    debugCommand (":info "<>stripComments text) logOutputDefault

debugKind :: IDEAction
debugKind = debugSelection "Please select a type in the editor" $ \text -> do
    debugSetLiberalScope
    debugCommand (":kind "<>stripComments text) logOutputDefault

debugType :: IDEAction
debugType = debugSelection "Please select an expression in the editor" $ \text -> do
    debugSetLiberalScope
    debugCommand (":type "<>stripComments text) logOutputDefault

debugSetBreakpoint :: IDEAction
debugSetBreakpoint = do
    maybeModuleName <- selectedModuleName
    case maybeModuleName of
        Just moduleName -> do
            -- ###           debugCommand (":add *"++moduleName) $ logOutputForBuild True
            maybeText <- selectedText
            case maybeText of
                (Just buf, Just text) -> debugBuffer buf $ do
                    basePath <- dsBasePath <$> ask
                    debugCommand (":module *" <> moduleName) logOutputDefault
                    debugCommand (":break " <> text) (logOutputForSetBreakpointDefault basePath)
                (mbBuf, _) -> do
                    maybeLocation <- selectedLocation
                    case maybeLocation of
                        Just (line, lineOffset) -> maybe (packageTry . tryDebug) debugBuffer mbBuf $ do
                            basePath <- dsBasePath <$> ask
                            debugCommand (":break " <> moduleName <> " " <> T.pack (show $ line + 1) <> " " <> T.pack (show lineOffset))
                                         (logOutputForSetBreakpointDefault basePath)
                        Nothing -> ideMessage Normal "Unknown error setting breakpoint"
            return ()
        Nothing   -> ideMessage Normal "Please select module file in the editor"

debugSet :: (Bool -> Text) -> Bool -> IDEAction
debugSet flag value =
    debugSelection' $ \_ -> debugCommand (":set " <> flag value) logOutputDefault

debugSetPrintEvldWithShow :: Bool -> IDEAction
debugSetPrintEvldWithShow = debugSet printEvldWithShowFlag

debugSetBreakOnException :: Bool -> IDEAction
debugSetBreakOnException = debugSet breakOnExceptionFlag

debugSetBreakOnError :: Bool -> IDEAction
debugSetBreakOnError = debugSet breakOnErrorFlag

debugSetPrintBindResult :: Bool -> IDEAction
debugSetPrintBindResult = debugSet printBindResultFlag


