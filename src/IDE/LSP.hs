{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Stage 1 Language Server Protocol integration for the leksah web UI.
--
-- One @haskell-language-server@ process is run per project root (found by
-- walking up from a source file for a @cabal.project@ \/ @*.cabal@ \/
-- @stack.yaml@).  Editor documents are mirrored to the server with full-text
-- sync ('documentOpened' \/ 'documentChanged' \/ 'documentSaved' \/
-- 'documentClosed'), and the server's @textDocument/publishDiagnostics@
-- notifications are turned into leksah 'LogRef's and pushed into the shared
-- IDE state — so they render in the Errors pane, the Log pane and as CM6
-- editor squiggles through the same path GHC build errors already use.
--
-- The client itself is the vendored @lsp-types-client@ package; this module
-- is only the glue between it and leksah's state.
module IDE.LSP
    ( documentOpened
    , documentChanged
    , documentSaved
    , documentClosed
    ) where

import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import           Control.Concurrent.STM
import           Control.Exception (SomeException, catch, try)
import           Control.Lens ((%~))
import           Control.Monad (join, void, when)
import           Data.Aeson
import           Data.Aeson.Types (Parser, parseMaybe)
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Int (Int32)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Directory (doesFileExist, listDirectory, makeAbsolute)
import           System.FilePath (takeDirectory, takeExtension, (</>))
import           System.IO.Unsafe (unsafePerformIO)
import           System.Log.Logger (debugM)

import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types (InitializeParams, filePathToUri, uriToFilePath)

import           Language.LSP.Client (Client, ClientConfig(..), defaultClientConfig,
                                       notify, request, start)

import           IDE.Core.CTypes (SrcSpan(..))
import           IDE.Core.Types (Log(..), LogRef(..), LogRefType(..), allLogRefs)
import           IDE.Core.State (IDEAction, modifyIDE_, reflectIDE)
import           IDE.Web.IDERefStore (getGlobalIDERef)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | How to launch a language server.  Stage 1 uses @haskell-language-server@
-- on the ambient @PATH@ (the wrapper, which picks a GHC per project, is not
-- always present — e.g. leksah's own dev shell ships the bare binary).
-- Making this a per-project preference is a later stage.
serverCommand :: (FilePath, [String])
serverCommand = ("haskell-language-server", ["--lsp"])

-- | Extensions we treat as Haskell source worth a language server.
isHaskellFile :: FilePath -> Bool
isHaskellFile f = takeExtension f `elem` [".hs", ".lhs"]

--------------------------------------------------------------------------------
-- Global state
--------------------------------------------------------------------------------

-- | A running (or failed) server per project root.  @Nothing@ marks a root we
-- tried and failed to start, so we do not respawn on every keystroke.
{-# NOINLINE registry #-}
registry :: MVar (Map FilePath (Maybe ServerState))
registry = unsafePerformIO (newMVar Map.empty)

-- | The exact 'LogRef's we last published for each file, so a fresh
-- @publishDiagnostics@ can remove precisely those (by value) and leave build
-- diagnostics in 'allLogRefs' untouched.
{-# NOINLINE lastRefs #-}
lastRefs :: IORef (Map FilePath [LogRef])
lastRefs = unsafePerformIO (newIORef Map.empty)

-- | Cache of file -> project root, so we do not walk the filesystem on every
-- keystroke's @didChange@.
{-# NOINLINE rootCache #-}
rootCache :: IORef (Map FilePath FilePath)
rootCache = unsafePerformIO (newIORef Map.empty)

projectRootOf :: FilePath -> IO FilePath
projectRootOf file = readIORef rootCache >>= \c ->
    case Map.lookup file c of
        Just r  -> return r
        Nothing -> do
            r <- findProjectRoot file
            atomicModifyIORef' rootCache (\m -> (Map.insert file r m, ()))
            return r

data ServerState = ServerState
    { ssClient   :: Client
    , ssReady    :: TVar Bool          -- ^ initialize response received
    , ssPending  :: TVar [IO ()]       -- ^ actions queued until ready (LIFO)
    , ssVersions :: TVar (Map FilePath Int32)  -- ^ open documents + versions
    }

--------------------------------------------------------------------------------
-- Public API (called from the editor widget)
--------------------------------------------------------------------------------

-- | A document was opened in the editor (or its full text otherwise became
-- available).  Ensures a server for its project and mirrors the text.
documentOpened :: FilePath -> Text -> IO ()
documentOpened = touch

-- | The document's text changed; mirror the new full text to the server.
documentChanged :: FilePath -> Text -> IO ()
documentChanged = touch

-- | The document was saved to disk.
documentSaved :: FilePath -> Text -> IO ()
documentSaved file text = withServer file $ \ss -> onReady ss $ do
    open <- atomically $ Map.member file <$> readTVar (ssVersions ss)
    when open $
        notify (ssClient ss) SMethod_TextDocumentDidSave $ buildParams $ object
            [ "textDocument" .= object [ "uri" .= toJSON (filePathToUri file) ]
            , "text" .= text ]

-- | The document was closed in the editor.
documentClosed :: FilePath -> IO ()
documentClosed file = withServer file $ \ss -> onReady ss $ do
    open <- atomically $ do
        m <- readTVar (ssVersions ss)
        writeTVar (ssVersions ss) (Map.delete file m)
        return (Map.member file m)
    when open $
        notify (ssClient ss) SMethod_TextDocumentDidClose $ buildParams $ object
            [ "textDocument" .= object [ "uri" .= toJSON (filePathToUri file) ] ]

-- | Shared by 'documentOpened' \/ 'documentChanged': send @didOpen@ the first
-- time we see a file and @didChange@ (full text) thereafter.
touch :: FilePath -> Text -> IO ()
touch file text = when (isHaskellFile file) $ do
    root <- projectRootOf file
    ensureServer root >>= \case
        Nothing -> return ()
        Just ss -> onReady ss $ do
            let uri = toJSON (filePathToUri file)
            mv <- atomically $ Map.lookup file <$> readTVar (ssVersions ss)
            case mv of
                Nothing -> do
                    atomically $ modifyTVar' (ssVersions ss) (Map.insert file 1)
                    notify (ssClient ss) SMethod_TextDocumentDidOpen $ buildParams $ object
                        [ "textDocument" .= object
                            [ "uri" .= uri, "languageId" .= ("haskell" :: Text)
                            , "version" .= (1 :: Int), "text" .= text ] ]
                Just v -> do
                    let v' = v + 1
                    atomically $ modifyTVar' (ssVersions ss) (Map.insert file v')
                    notify (ssClient ss) SMethod_TextDocumentDidChange $ buildParams $ object
                        [ "textDocument" .= object [ "uri" .= uri, "version" .= v' ]
                        , "contentChanges" .= [ object [ "text" .= text ] ] ]

--------------------------------------------------------------------------------
-- Server lifecycle
--------------------------------------------------------------------------------

-- | Look up an already-running server for a file's project (never spawns).
withServer :: FilePath -> (ServerState -> IO ()) -> IO ()
withServer file act = when (isHaskellFile file) $ do
    root <- projectRootOf file
    m <- readMVarMap
    case Map.lookup root m of
        Just (Just ss) -> act ss
        _              -> return ()
  where readMVarMap = modifyMVar registry (\m -> return (m, m))

-- | Get (spawning if necessary) the server for a project root.
ensureServer :: FilePath -> IO (Maybe ServerState)
ensureServer root = modifyMVar registry $ \m ->
    case Map.lookup root m of
        Just entry -> return (m, entry)
        Nothing -> try (spawnAndInit root) >>= \case
            Right ss -> return (Map.insert root (Just ss) m, Just ss)
            Left (e :: SomeException) -> do
                debugM "leksah" ("IDE.LSP: could not start language server in "
                                 <> root <> ": " <> show e)
                return (Map.insert root Nothing m, Nothing)

spawnAndInit :: FilePath -> IO ServerState
spawnAndInit root = do
    ready   <- newTVarIO False
    pending <- newTVarIO []
    vers    <- newTVarIO Map.empty
    let (cmd, args) = serverCommand
        cfg = defaultClientConfig
            { onNotification = handleNotification root
            , onStderr = \l -> debugM "leksah" ("HLS[" <> root <> "]: " <> T.unpack l) }
    client <- start cmd args (Just root) Nothing cfg
    let ss = ServerState client ready pending vers
    debugM "leksah" ("IDE.LSP: started language server in " <> root)
    void $ request client SMethod_Initialize (initParams root) $ \case
        Right _ -> do
            notify client SMethod_Initialized (buildParams (object []))
            acts <- atomically $ do
                writeTVar ready True
                a <- readTVar pending
                writeTVar pending []
                return (reverse a)
            sequence_ acts
        Left err ->
            debugM "leksah" ("IDE.LSP: initialize failed in " <> root <> ": " <> show err)
    return ss

-- | Run an action now if the server has initialized, otherwise queue it.
onReady :: ServerState -> IO () -> IO ()
onReady ss act = join . atomically $ do
    r <- readTVar (ssReady ss)
    if r
        then return act
        else modifyTVar' (ssPending ss) (act :) >> return (return ())

initParams :: FilePath -> InitializeParams
initParams root = buildParams $ object
    [ "processId" .= Null
    , "rootUri" .= toJSON (filePathToUri root)
    , "capabilities" .= object
        [ "textDocument" .= object
            [ "synchronization" .= object [ "didSave" .= True ]
            , "publishDiagnostics" .= object [ "relatedInformation" .= True ] ]
        , "workspace" .= object [ "configuration" .= True ] ]
    ]

--------------------------------------------------------------------------------
-- Diagnostics: publishDiagnostics -> LogRef -> shared IDE state
--------------------------------------------------------------------------------

handleNotification :: FilePath -> Text -> Value -> IO ()
handleNotification root method params = case method of
    "textDocument/publishDiagnostics" -> case parsePublish params of
        Just (uriText, diags)
            | Just file <- uriTextToFilePath uriText -> do
                let newRefs = map (toLogRef root file) diags
                old <- atomicModifyIORef' lastRefs $ \m ->
                    (Map.insert file newRefs m, Map.findWithDefault [] file m)
                runIDE (replaceRefs old newRefs)
        _ -> return ()
    _ -> return ()

replaceRefs :: [LogRef] -> [LogRef] -> IDEAction
replaceRefs old new =
    modifyIDE_ $ allLogRefs %~ \s ->
        Seq.filter (`notElem` old) s <> Seq.fromList new

runIDE :: IDEAction -> IO ()
runIDE act = getGlobalIDERef >>= \case
    Just ideR -> reflectIDE act ideR
    Nothing   -> return ()

-- | A single diagnostic, in the fields we care about (0-based line\/char).
data Diag = Diag
    { dStartLine, dStartCol, dEndLine, dEndCol :: !Int
    , dSeverity :: Maybe Int
    , dMessage  :: Text
    }

toLogRef :: FilePath -> FilePath -> Diag -> LogRef
toLogRef root file Diag{..} = LogRef
    { logRefSrcSpan  = SrcSpan
        { srcSpanFilename    = file
        , srcSpanStartLine   = dStartLine + 1        -- leksah lines are 1-based
        , srcSpanStartColumn = dStartCol             -- leksah columns are 0-based
        , srcSpanEndLine     = dEndLine + 1
        , srcSpanEndColumn   = max 0 (dEndCol - 1)   -- CM re-adds +1 for exclusive end
        }
    , logRefLog      = LogProject root
    , refDescription = dMessage
    , logRefIdea     = Nothing
    , logLines       = Nothing
    , logRefType     = severityToType dSeverity
    }

-- | LSP DiagnosticSeverity: 1=Error 2=Warning 3=Information 4=Hint.
severityToType :: Maybe Int -> LogRefType
severityToType (Just 1) = ErrorRef
severityToType (Just 2) = WarningRef
severityToType (Just 3) = LintRef
severityToType (Just 4) = LintRef
severityToType _        = WarningRef

--------------------------------------------------------------------------------
-- Minimal JSON parsing of incoming diagnostics (protocol shape is stable)
--------------------------------------------------------------------------------

parsePublish :: Value -> Maybe (Text, [Diag])
parsePublish = parseMaybe $ withObject "PublishDiagnosticsParams" $ \o -> do
    uri   <- o .: "uri"
    diags <- o .: "diagnostics" >>= mapM parseDiag
    return (uri, diags)

parseDiag :: Value -> Parser Diag
parseDiag = withObject "Diagnostic" $ \o -> do
    (sl, sc, el, ec) <- o .: "range" >>= withObject "Range" (\r -> do
        (sl, sc) <- r .: "start" >>= parsePos
        (el, ec) <- r .: "end"   >>= parsePos
        return (sl, sc, el, ec))
    sev <- o .:? "severity"
    msg <- o .: "message"
    return (Diag sl sc el ec sev msg)
  where
    parsePos = withObject "Position" $ \p ->
        (,) <$> p .: "line" <*> p .: "character"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Decode a value we constructed ourselves into a typed protocol param.  A
-- failure here is a programming error (a wrong JSON shape), not runtime data.
buildParams :: FromJSON a => Value -> a
buildParams v = case fromJSON v of
    Success x -> x
    Error e   -> error ("IDE.LSP.buildParams: " <> e)

uriTextToFilePath :: Text -> Maybe FilePath
uriTextToFilePath t = case fromJSON (String t) of
    Success u -> uriToFilePath u
    Error _   -> Nothing

-- | Walk up from a source file to the nearest project root.
findProjectRoot :: FilePath -> IO FilePath
findProjectRoot file = do
    abs' <- makeAbsolute file
    let dir0 = takeDirectory abs'
        loop dir = hasMarker dir >>= \case
            True  -> return (Just dir)
            False -> let up = takeDirectory dir
                     in if up == dir then return Nothing else loop up
    loop dir0 >>= maybe (return dir0) return

hasMarker :: FilePath -> IO Bool
hasMarker dir = do
    pj  <- doesFileExist (dir </> "cabal.project")
    sy  <- doesFileExist (dir </> "stack.yaml")
    cbl <- any ((== ".cabal") . takeExtension)
             <$> (listDirectory dir `catch` \(_ :: SomeException) -> return [])
    return (pj || sy || cbl)
