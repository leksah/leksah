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
    , requestHover
    , requestCompletion
    , requestDefinition
    , requestReferences
    ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import           Control.Concurrent.STM
import           Control.Exception (SomeException, catch, try)
import           Control.Lens ((%~))
import           Control.Monad (forM, join, void, when)
import           Data.Aeson
import           Data.Aeson.Types (Parser, parseMaybe)
import           Data.Foldable (toList)
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Int (Int32)
import           Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
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
import           IDE.Core.State (IDEAction, modifyIDE_, reflectIDE, readIDE, prefs,
                                 lspEnabled, lspServerCommand)
import           IDE.Web.IDERefStore (getGlobalIDERef)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | The fallback server command: @haskell-language-server@ on the ambient
-- @PATH@ (the bare binary, not the @-wrapper@ — leksah's own dev shell ships
-- only that).  Overridden by the 'lspServerCommand' pref, and per-project by a
-- @.leksah-lsp@ file (see 'serverCommandFor').
defaultServerCommand :: (FilePath, [String])
defaultServerCommand = ("haskell-language-server", ["--lsp"])

-- | The current LSP prefs — @(enabled, command-override)@ — read live from the
-- shared IDE state.  Defaults (enabled, no override) before the IDE exists.
lspConfig :: IO (Bool, Text)
lspConfig = getGlobalIDERef >>= \case
    Just ideR -> do
        p <- reflectIDE (readIDE prefs) ideR
        return (lspEnabled p, lspServerCommand p)
    Nothing   -> return (True, "")

-- | Resolve the server command for a project root: a @.leksah-lsp@ file in the
-- root (first non-blank, non-@#@-comment line) wins; then the global pref
-- override; then 'defaultServerCommand'.  The command line is split on
-- whitespace (no shell quoting).
serverCommandFor :: FilePath -> Text -> IO (FilePath, [String])
serverCommandFor root globalCmd = do
    mFile <- readOverrideFile (root </> ".leksah-lsp")
    return $ case mFile <|> nonBlank globalCmd of
        Just cmdline | (c:as) <- T.words cmdline -> (T.unpack c, map T.unpack as)
        _                                        -> defaultServerCommand
  where
    nonBlank t = let s = T.strip t in if T.null s then Nothing else Just s

readOverrideFile :: FilePath -> IO (Maybe Text)
readOverrideFile f = doesFileExist f >>= \case
    False -> return Nothing
    True  -> do
        ls <- (T.lines <$> TIO.readFile f) `catch` \(_ :: SomeException) -> return []
        return $ listToMaybe
            [ l | l <- map T.strip ls, not (T.null l), not ("#" `T.isPrefixOf` l) ]

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
-- Hover (textDocument/hover)
--------------------------------------------------------------------------------

-- | Request hover information at a position (LSP 0-based @line@\/@char@) in an
-- already-open document.  Non-blocking: @cb@ is invoked with the hover text
-- (rendered from the server's markup\/marked-string contents) or 'Nothing'.
-- If no server is running for the file, or it is not a Haskell file, @cb@ is
-- called with 'Nothing'.
requestHover :: FilePath -> Int -> Int -> (Maybe Text -> IO ()) -> IO ()
requestHover file line ch cb
    | not (isHaskellFile file) = cb Nothing
    | otherwise = do
        root <- projectRootOf file
        m <- modifyMVar registry (\mp -> return (mp, mp))
        case Map.lookup root m of
            Just (Just ss) -> onReady ss $
                void $ request (ssClient ss) SMethod_TextDocumentHover
                    (buildParams $ object
                        [ "textDocument" .= object [ "uri" .= toJSON (filePathToUri file) ]
                        , "position" .= object [ "line" .= line, "character" .= ch ] ])
                    (\case
                        Right res -> cb (extractHover (toJSON res))
                        Left _    -> cb Nothing)
            _ -> cb Nothing

-- | Pull a single plain-text blob out of an LSP @Hover@ result.  @contents@
-- may be a @MarkupContent {kind,value}@, a @MarkedString@ (a bare string or
-- @{language,value}@), or an array of marked strings; @null@ hover yields
-- 'Nothing'.
extractHover :: Value -> Maybe Text
extractHover = fmap T.strip . nonEmpty . parseMaybe (withObject "Hover" $ \o -> o .: "contents" >>= parseContents)
  where
    nonEmpty (Just t) | not (T.null (T.strip t)) = Just t
    nonEmpty _ = Nothing
    parseContents :: Value -> Parser Text
    parseContents (String s) = pure s
    parseContents (Object c) = c .: "value"
    parseContents (Array a)  = T.intercalate "\n\n" <$> mapM parseMarked (toList a)
    parseContents _          = fail "unrecognized hover contents"
    parseMarked :: Value -> Parser Text
    parseMarked (String s) = pure s
    parseMarked (Object c) = c .: "value"
    parseMarked _          = pure ""

--------------------------------------------------------------------------------
-- Completion (textDocument/completion)
--------------------------------------------------------------------------------

-- | Cap on completion items returned to the editor — HLS can emit thousands
-- (every in-scope identifier); CM6 filters as you type, so a generous prefix
-- is plenty and keeps the JS payload small.
maxCompletions :: Int
maxCompletions = 200

-- | Request completions at a position (LSP 0-based @line@\/@char@) in an
-- already-open document.  Non-blocking: @cb@ receives a JSON array string of
-- @{label, detail, kind, apply}@ objects (empty @\"[]\"@ if none / no server /
-- not a Haskell file), ready to hand to the CM6 @resolveComplete@ bridge.
requestCompletion :: FilePath -> Int -> Int -> (Text -> IO ()) -> IO ()
requestCompletion file line ch cb
    | not (isHaskellFile file) = cb "[]"
    | otherwise = do
        root <- projectRootOf file
        m <- modifyMVar registry (\mp -> return (mp, mp))
        case Map.lookup root m of
            Just (Just ss) -> onReady ss $
                void $ request (ssClient ss) SMethod_TextDocumentCompletion
                    (buildParams $ object
                        [ "textDocument" .= object [ "uri" .= toJSON (filePathToUri file) ]
                        , "position" .= object [ "line" .= line, "character" .= ch ] ])
                    (\case
                        Right res -> cb (encodeToText (parseCompletions (toJSON res)))
                        Left _    -> cb "[]")
            _ -> cb "[]"

-- | Reduce an LSP @CompletionList@ (or bare @CompletionItem[]@, or @null@) to a
-- capped list of compact @{label, apply, detail?, kind?}@ objects.
parseCompletions :: Value -> [Value]
parseCompletions v = take maxCompletions $ case v of
    Array a  -> mapMaybe compactItem (toList a)
    Object _ -> case parseMaybe (withObject "CompletionList" (.: "items")) v of
        Just (Array a) -> mapMaybe compactItem (toList a)
        _              -> []
    _        -> []

-- | Pull the display label, the text to insert, and the kind out of one
-- @CompletionItem@.  Prefer @insertText@, then @textEdit.newText@, else the
-- label itself.
compactItem :: Value -> Maybe Value
compactItem = parseMaybe $ withObject "CompletionItem" $ \o -> do
    label  <- o .: "label"
    detail <- o .:? "detail"
    kind   <- o .:? "kind"
    insert <- o .:? "insertText"
    te     <- o .:? "textEdit"
    let newTxt = te >>= parseMaybe (withObject "TextEdit" (.: "newText"))
        apply  = fromMaybe (label :: Text) (insert <|> newTxt)
    pure $ object $
        [ "label" .= label, "apply" .= apply ]
        <> maybe [] (\d -> [ "detail" .= (d :: Text) ]) detail
        <> maybe [] (\k -> [ "kind"   .= (k :: Int)  ]) kind

--------------------------------------------------------------------------------
-- Go to definition (textDocument/definition)
--------------------------------------------------------------------------------

-- | Request the definition site at a position (LSP 0-based @line@\/@char@).
-- Non-blocking: @cb@ receives the target as a leksah 'SrcSpan' (1-based line,
-- 0-based column; its filename is the file to open) or 'Nothing'.
requestDefinition :: FilePath -> Int -> Int -> (Maybe SrcSpan -> IO ()) -> IO ()
requestDefinition file line ch cb
    | not (isHaskellFile file) = cb Nothing
    | otherwise = withServerReady file (cb Nothing) $ \ss ->
        void $ request (ssClient ss) SMethod_TextDocumentDefinition
            (buildParams (posParams file line ch))
            (\case
                Right res -> cb (firstLocation (toJSON res))
                Left _    -> cb Nothing)

-- | The @textDocument/definition@ result is a @Location@, a @Location[]@, or a
-- @LocationLink[]@ (or @null@).  Take the first and turn it into a 'SrcSpan'.
firstLocation :: Value -> Maybe SrcSpan
firstLocation v = case v of
    Array a  -> listToMaybe (mapMaybe locToSpan (toList a))
    Object _ -> locToSpan v
    _        -> Nothing

locToSpan :: Value -> Maybe SrcSpan
locToSpan = parseMaybe $ withObject "Location" $ \o -> do
    uriV <- (o .: "uri") <|> (o .: "targetUri")
    rng  <- (o .: "range") <|> (o .: "targetSelectionRange") <|> (o .: "targetRange")
    (sl, sc) <- flip (withObject "Range") rng $ \r -> r .: "start" >>= parsePos
    (el, ec) <- flip (withObject "Range") rng $ \r -> r .: "end"   >>= parsePos
    file     <- maybe (fail "bad uri") pure (uriTextToFilePath uriV)
    pure SrcSpan
        { srcSpanFilename    = file
        , srcSpanStartLine   = sl + 1
        , srcSpanStartColumn = sc
        , srcSpanEndLine     = el + 1
        , srcSpanEndColumn   = ec }

--------------------------------------------------------------------------------
-- Find references (textDocument/references)
--------------------------------------------------------------------------------

-- | Cap on reference results fed to the Grep pane.
maxReferences :: Int
maxReferences = 500

-- | Request all references to the symbol at a position (LSP 0-based
-- @line@\/@char@), including its declaration.  Non-blocking: @cb@ receives
-- @(file, 1-based line, trimmed line text)@ rows, ready for the Grep pane.
requestReferences :: FilePath -> Int -> Int -> ([(FilePath, Int, Text)] -> IO ()) -> IO ()
requestReferences file line ch cb
    | not (isHaskellFile file) = cb []
    | otherwise = withServerReady file (cb []) $ \ss ->
        void $ request (ssClient ss) SMethod_TextDocumentReferences
            (buildParams $ object
                [ "textDocument" .= object [ "uri" .= toJSON (filePathToUri file) ]
                , "position" .= object [ "line" .= line, "character" .= ch ]
                , "context" .= object [ "includeDeclaration" .= True ] ])
            (\case
                Right res -> attachContext (parseLocations (toJSON res)) >>= cb
                Left _    -> cb [])

-- | Reduce a @Location[]@ (or single @Location@) to @(file, 0-based line)@ pairs.
parseLocations :: Value -> [(FilePath, Int)]
parseLocations v = case v of
    Array a  -> mapMaybe locFileLine (toList a)
    Object _ -> maybe [] pure (locFileLine v)
    _        -> []

locFileLine :: Value -> Maybe (FilePath, Int)
locFileLine = parseMaybe $ withObject "Location" $ \o -> do
    uriV     <- (o .: "uri") <|> (o .: "targetUri")
    rng      <- (o .: "range") <|> (o .: "targetSelectionRange") <|> (o .: "targetRange")
    (sl, _)  <- flip (withObject "Range") rng $ \r -> r .: "start" >>= parsePos
    file     <- maybe (fail "bad uri") pure (uriTextToFilePath uriV)
    pure (file, sl)

-- | Read each referenced file once to attach the (trimmed) source line as
-- context, de-duplicating and sorting lines within a file, then cap the total.
attachContext :: [(FilePath, Int)] -> IO [(FilePath, Int, Text)]
attachContext locs = do
    let byFile = Map.toList $ Map.fromListWith (++) [ (f, [l]) | (f, l) <- locs ]
    rows <- forM byFile $ \(f, ls) -> do
        ls' <- (T.lines <$> TIO.readFile f)
                 `catch` \(_ :: SomeException) -> return []
        let lineAt i | i >= 0 && i < length ls' = T.strip (ls' !! i)
                     | otherwise                = ""
        return [ (f, l + 1, lineAt l) | l <- sort (nub ls) ]
    return $ take maxReferences (concat rows)

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

-- | Get (spawning if necessary) the server for a project root.  When LSP is
-- disabled in prefs, never spawns and returns 'Nothing' without caching, so
-- re-enabling takes effect on the next edit.
ensureServer :: FilePath -> IO (Maybe ServerState)
ensureServer root = do
    (enabled, cmdPref) <- lspConfig
    if not enabled
        then return Nothing
        else modifyMVar registry $ \m -> case Map.lookup root m of
            Just entry -> return (m, entry)
            Nothing -> do
                cmdArgs <- serverCommandFor root cmdPref
                try (spawnAndInit root cmdArgs) >>= \case
                    Right ss -> return (Map.insert root (Just ss) m, Just ss)
                    Left (e :: SomeException) -> do
                        debugM "leksah" ("IDE.LSP: could not start language server in "
                                         <> root <> ": " <> show e)
                        return (Map.insert root Nothing m, Nothing)

spawnAndInit :: FilePath -> (FilePath, [String]) -> IO ServerState
spawnAndInit root (cmd, args) = do
    ready   <- newTVarIO False
    pending <- newTVarIO []
    vers    <- newTVarIO Map.empty
    let cfg = defaultClientConfig
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
            , "publishDiagnostics" .= object [ "relatedInformation" .= True ]
            -- Ask for plain-text completions (snippetSupport = False) so items
            -- arrive without @${1:…}@ placeholders — CM6 inserts them verbatim.
            , "completion" .= object
                [ "completionItem" .= object [ "snippetSupport" .= False ] ] ]
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

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Encode a JSON value to strict 'Text' (for handing to the CM6 JS bridge).
encodeToText :: ToJSON a => a -> Text
encodeToText = TL.toStrict . TLE.decodeUtf8 . encode

-- | Run @act@ against the (already-running, ready) server for a file's project,
-- or @onNone@ if there is none.  Never spawns.
withServerReady :: FilePath -> IO () -> (ServerState -> IO ()) -> IO ()
withServerReady file onNone act = do
    root <- projectRootOf file
    m <- modifyMVar registry (\mp -> return (mp, mp))
    case Map.lookup root m of
        Just (Just ss) -> onReady ss (act ss)
        _              -> onNone

-- | @TextDocumentPositionParams@ for a file + LSP 0-based position.
posParams :: FilePath -> Int -> Int -> Value
posParams file line ch = object
    [ "textDocument" .= object [ "uri" .= toJSON (filePathToUri file) ]
    , "position" .= object [ "line" .= line, "character" .= ch ] ]

-- | Parse an LSP @Position@ to a @(line, character)@ pair (both 0-based).
parsePos :: Value -> Parser (Int, Int)
parsePos = withObject "Position" $ \p -> (,) <$> p .: "line" <*> p .: "character"

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
