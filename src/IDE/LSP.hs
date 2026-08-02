{-# LANGUAGE CPP                 #-}
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
    , requestTerminalHover
    , requestCompletion
    , requestDefinition
    , requestReferences
    , shutdownServers
    ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import           Control.Concurrent.STM
import           Control.Exception (SomeException, catch, try)
import           Control.Lens ((%~), (^.))
import           Control.Monad (forM, join, void, when)
import           Data.Aeson
import           Data.Aeson.Types (Parser, parseMaybe)
import           Data.Foldable (toList)
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Int (Int32)
import           Data.List (find, nub, sort, sortOn)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           System.Directory (doesDirectoryExist, doesFileExist, findExecutable,
                                   listDirectory, makeAbsolute)
import           System.FilePath (addTrailingPathSeparator, takeDirectory, takeExtension, (</>))
import           System.IO.Unsafe (unsafePerformIO)
import           System.Log.Logger (debugM)

import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types (InitializeParams, filePathToUri, uriToFilePath)

import           Language.LSP.Client (Client, ClientConfig(..), defaultClientConfig,
                                       notify, request, start, stop, alive)

import           IDE.Core.CTypes (SrcSpan(..))
import           IDE.Core.Types (Log(..), LogRef(..), LogRefType(..), allLogRefs)
import           IDE.Core.State (IDEAction, modifyIDE_, reflectIDE, readIDE, prefs,
                                 lspEnabled, lspServerCommand,
                                 workspace, wsProjects, wsSettingsFor,
                                 ProjectSettings(..), pjKey, pjDir)
import           IDE.Utils.FileUtils (isSubPath)
import           IDE.Utils.RemotePath (isRemotePath, parseRemotePath, renderRemotePath)
import           IDE.Web.FS (fsReadFile, fsDoesFileExist)
import           IDE.Web.IDERefStore (getGlobalIDERef)
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
#if !defined(ghcjs_HOST_OS)
import           IDE.Utils.RemoteExec (remoteSshArgs, runSsh, shellQuote)
#endif
#if defined(ghcjs_HOST_OS)
import           IDE.Web.DemoHovers (demoHover)
#endif

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Per-language LSP config: the @languageId@ sent in @didOpen@ and the default
-- server command (used when no @.leksah-lsp@/pref override applies).
data LangConfig = LangConfig
    { lcLanguageId  :: Text
    , lcDefaultCmd  :: (FilePath, [String])
    , lcRootMarkers :: [String]
      -- ^ Marker files whose directory is the project root.  Empty = use the
      -- Haskell-style walk (@cabal.project@ \/ @stack.yaml@ \/ @*.cabal@).
      -- Non-empty markers fall back to the nearest @.git@ dir, then the
      -- file's own directory.
    }

-- | The built-in language server for each recognised extension.  Haskell uses
-- @haskell-language-server@ (the bare binary, not the @-wrapper@ — leksah's own
-- dev shell ships only that); Nix uses @nixd@ (added to the dev shell).  The
-- rest are launched only when their binary is on PATH (see 'ensureServer').
-- Extend this list to support more languages.
languageOf :: FilePath -> Maybe LangConfig
languageOf f = case takeExtension f of
    ".hs"   -> Just haskell
    ".lhs"  -> Just haskell
    ".nix"  -> Just (LangConfig "nix" ("nixd", [])
                                 ["flake.nix", "shell.nix", "default.nix"])
    ".ts"   -> Just (ts "typescript")
    ".tsx"  -> Just (ts "typescriptreact")
    ".js"   -> Just (ts "javascript")
    ".jsx"  -> Just (ts "javascriptreact")
    ".mjs"  -> Just (ts "javascript")
    ".cjs"  -> Just (ts "javascript")
    ".rs"   -> Just (LangConfig "rust" ("rust-analyzer", []) ["Cargo.toml"])
    ".go"   -> Just (LangConfig "go" ("gopls", []) ["go.work", "go.mod"])
    ".py"   -> Just (LangConfig "python" ("pyright-langserver", ["--stdio"])
                                ["pyproject.toml", "setup.py", "requirements.txt"])
    ".c"    -> Just (clangd "c")
    ".h"    -> Just (clangd "c")
    ".cpp"  -> Just (clangd "cpp")
    ".cc"   -> Just (clangd "cpp")
    ".cxx"  -> Just (clangd "cpp")
    ".hpp"  -> Just (clangd "cpp")
    ".hh"   -> Just (clangd "cpp")
    _       -> Nothing
  where
    haskell = LangConfig "haskell" ("haskell-language-server", ["--lsp"]) []
    ts lang = LangConfig lang ("typescript-language-server", ["--stdio"])
                         ["tsconfig.json", "package.json"]
    clangd lang = LangConfig lang ("clangd", []) ["compile_commands.json", ".clangd"]

-- | Registry key part: one server process per (root, server binary), so e.g.
-- @.ts@ and @.js@ files in one project share a single
-- @typescript-language-server@ (each file still sends its own @languageId@ in
-- @didOpen@).
serverKey :: LangConfig -> Text
serverKey = T.pack . fst . lcDefaultCmd

-- | Does this file have a language server we know how to launch?
isSupportedFile :: FilePath -> Bool
isSupportedFile = isJust . languageOf

-- | The current LSP prefs — @(enabled, command-override)@ — read live from the
-- shared IDE state.  Defaults (enabled, no override) before the IDE exists.
lspConfig :: IO (Bool, Text)
lspConfig = getGlobalIDERef >>= \case
    Just ideR -> do
        p <- reflectIDE (readIDE prefs) ideR
        return (lspEnabled p, lspServerCommand p)
    Nothing   -> return (True, "")

-- | Resolve the server command for a project root + language.  For Haskell a
-- @.leksah-lsp@ file in the root (first non-blank, non-@#@ line) wins, then the
-- global pref override, then the language's default command.  Other languages
-- always use their built-in default (the pref/@.leksah-lsp@ target Haskell HLS).
serverCommandFor :: FilePath -> Text -> LangConfig -> IO (FilePath, [String])
serverCommandFor root globalCmd lc = do
    let haskellOverride = lcLanguageId lc == "haskell"
    mFile <- if haskellOverride then readOverrideFile (root </> ".leksah-lsp")
                                else return Nothing
    let override = if haskellOverride then mFile <|> nonBlank globalCmd else Nothing
    return $ case override of
        Just cmdline | (c:as) <- T.words cmdline -> (T.unpack c, map T.unpack as)
        _                                        -> lcDefaultCmd lc
  where
    nonBlank t = let s = T.strip t in if T.null s then Nothing else Just s

-- Read through the FS seam ('fsDoesFileExist'\/'fsReadFile') so a @.leksah-lsp@
-- in a REMOTE (@ssh:\/\/@) project root is found over ssh — a plain
-- 'doesFileExist' would test the literal @ssh:\/\/…@ path on the local disk and
-- always miss it.
readOverrideFile :: FilePath -> IO (Maybe Text)
readOverrideFile f = fsDoesFileExist f >>= \case
    False -> return Nothing
    True  -> do
        ls <- (T.lines . decodeUtf8With lenientDecode <$> fsReadFile f)
                 `catch` \(_ :: SomeException) -> return []
        return $ listToMaybe
            [ l | l <- map T.strip ls, not (T.null l), not ("#" `T.isPrefixOf` l) ]

--------------------------------------------------------------------------------
-- Global state
--------------------------------------------------------------------------------

-- | A running (or failed) server per project root.  @Nothing@ marks a root we
-- tried and failed to start, so we do not respawn on every keystroke.
{-# NOINLINE registry #-}
registry :: MVar (Map (FilePath, Text) (Maybe ServerState))  -- key = (project root, 'serverKey')
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

-- (A file's language is fixed by its extension, so keying the cache by file
-- alone stays sound with per-language root walks.)
projectRootOf :: LangConfig -> FilePath -> IO FilePath
projectRootOf lc file = readIORef rootCache >>= \c ->
    case Map.lookup file c of
        Just r  -> return r
        Nothing -> do
            r <- findProjectRoot lc file
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
#if defined(ghcjs_HOST_OS)
documentSaved _ _ = return ()
#else
documentSaved file text = withServer file $ \ss -> onReady ss $ do
    open <- atomically $ Map.member file <$> readTVar (ssVersions ss)
    when open $
        notify (ssClient ss) SMethod_TextDocumentDidSave $ buildParams $ object
            [ "textDocument" .= object [ "uri" .= fileUri file ]
            , "text" .= text ]
#endif

-- | The document was closed in the editor.
documentClosed :: FilePath -> IO ()
#if defined(ghcjs_HOST_OS)
documentClosed _ = return ()
#else
documentClosed file = withServer file $ \ss -> onReady ss $ do
    open <- atomically $ do
        m <- readTVar (ssVersions ss)
        writeTVar (ssVersions ss) (Map.delete file m)
        return (Map.member file m)
    when open $
        notify (ssClient ss) SMethod_TextDocumentDidClose $ buildParams $ object
            [ "textDocument" .= object [ "uri" .= fileUri file ] ]
#endif

-- | Shared by 'documentOpened' \/ 'documentChanged': send @didOpen@ the first
-- time we see a file and @didChange@ (full text) thereafter.
touch :: FilePath -> Text -> IO ()
#if defined(ghcjs_HOST_OS)
-- Browser demo: no language server to mirror documents to (and spawning one
-- would fail at runtime); hovers are served from the precomputed map below.
touch _ _ = return ()
#else
touch file text = case languageOf file of
  Nothing -> return ()
  Just lc -> do
    root <- projectRootOf lc file
    ensureServer root lc >>= \case
        Nothing -> return ()
        Just ss -> onReady ss $ do
            let uri = fileUri file
            mv <- atomically $ Map.lookup file <$> readTVar (ssVersions ss)
            case mv of
                Nothing -> do
                    atomically $ modifyTVar' (ssVersions ss) (Map.insert file 1)
                    notify (ssClient ss) SMethod_TextDocumentDidOpen $ buildParams $ object
                        [ "textDocument" .= object
                            [ "uri" .= uri, "languageId" .= lcLanguageId lc
                            , "version" .= (1 :: Int), "text" .= text ] ]
                Just v -> do
                    let v' = v + 1
                    atomically $ modifyTVar' (ssVersions ss) (Map.insert file v')
                    notify (ssClient ss) SMethod_TextDocumentDidChange $ buildParams $ object
                        [ "textDocument" .= object [ "uri" .= uri, "version" .= v' ]
                        , "contentChanges" .= [ object [ "text" .= text ] ] ]
#endif

--------------------------------------------------------------------------------
-- Hover (textDocument/hover)
--------------------------------------------------------------------------------

-- | Request hover information at a position (LSP 0-based @line@\/@char@) in an
-- already-open document.  Non-blocking: @cb@ is invoked with the hover text
-- (rendered from the server's markup\/marked-string contents) or 'Nothing'.
-- If no server is running for the file, or it is not a Haskell file, @cb@ is
-- called with 'Nothing'.
requestHover :: FilePath -> Int -> Int -> (Maybe Text -> IO ()) -> IO ()
#if defined(ghcjs_HOST_OS)
-- Browser demo: serve the precomputed hover map (window.leksahDemoHovers) —
-- real HLS responses captured at build time by gen-demo-hovers.py — instead
-- of a live server.  Pure span lookup; see IDE.Web.DemoHovers.
requestHover file line ch cb = demoHover file line ch >>= cb
#else
requestHover file line ch cb
    | not (isSupportedFile file) = cb Nothing
    | otherwise = withServerReady file (cb Nothing) $ \ss ->
        void $ request (ssClient ss) SMethod_TextDocumentHover
            (buildParams $ object
                [ "textDocument" .= object [ "uri" .= fileUri file ]
                , "position" .= object [ "line" .= line, "character" .= ch ] ])
            (\case
                Right res -> cb (extractHover (toJSON res))
                Left _    -> cb Nothing)
#endif

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

-- | Tooltip for a file reference found in terminal output (git-diff @+++@\/@---@
-- header paths, Claude Code @Update(...)@ headers, @file:line@ tokens; see
-- @terminalLinksJs@ in "IDE.Web.Main").  LSP-backed two ways: a diagnostics
-- summary drawn from the shared 'allLogRefs' store — which is populated by
-- @textDocument/publishDiagnostics@ (and GHC builds) so it needs no open
-- document — plus, when a line is known and the file is a Haskell source that is
-- open in the language server, @textDocument/hover@ for the symbol at that line.
-- Non-blocking: @cb@ is invoked exactly once with the tooltip text, or 'Nothing'
-- when there is nothing useful to show.
requestTerminalHover :: FilePath -> Maybe Int -> Maybe Int -> (Maybe Text -> IO ()) -> IO ()
#if defined(ghcjs_HOST_OS)
-- Browser demo: no diagnostics store worth summarising and no makeAbsolute
-- (there is no cwd) — straight to the precomputed hover lookup.
requestTerminalHover file mline mcol cb = case mline of
    Just ln | isSupportedFile file ->
        requestHover file (max 0 (ln - 1)) (maybe 0 (max 0) mcol) cb
    _ -> cb Nothing
#else
requestTerminalHover file mline mcol cb = do
    absFile <- if isRemotePath file
                 then return file  -- ssh:// is already absolute; makeAbsolute mangles it
                 else makeAbsolute file `catch` \(_ :: SomeException) -> return file
    diag    <- diagnosticsSummary absFile mline
    case mline of
        Just ln | isSupportedFile absFile ->
            -- @mcol@ is the 0-based column of the hovered symbol when the caller
            -- knows it (an identifier on a diff code line, or a @file:line:col@
            -- token); otherwise default to the start of the line.
            requestHover absFile (max 0 (ln - 1)) (maybe 0 (max 0) mcol) $ \mhov ->
                cb (joinTip [diag, mhov])
        _ -> cb diag
#endif

-- | Combine tooltip fragments (diagnostics summary, hover blurb), dropping the
-- empty ones; 'Nothing' when nothing remains.
joinTip :: [Maybe Text] -> Maybe Text
joinTip parts = case filter (not . T.null) (map T.strip (catMaybes parts)) of
    [] -> Nothing
    xs -> Just (T.intercalate "\n\n" xs)

-- | A short diagnostics blurb for @file@ from the shared LogRef store: a header
-- counting errors\/warnings, then the message of the diagnostic on (or nearest)
-- @mline@ if a line is given, else the first one.  'Nothing' when the file has
-- no error\/warning\/lint refs.
diagnosticsSummary :: FilePath -> Maybe Int -> IO (Maybe Text)
diagnosticsSummary file mline = getGlobalIDERef >>= \case
    Nothing   -> return Nothing
    Just ideR -> do
        refs <- reflectIDE (readIDE allLogRefs) ideR
        let mine = [ r | r <- toList refs
                       , srcSpanFilename (logRefSrcSpan r) == file
                       , logRefType r `elem` [ErrorRef, WarningRef, LintRef, TestFailureRef] ]
        if null mine then return Nothing else do
            let count t   = length (filter ((== t) . logRefType) mine)
                errs      = count ErrorRef + count TestFailureRef
                warns     = count WarningRef + count LintRef
                pick      = listToMaybe $ case mline of
                                Just ln -> sortOn (\r -> abs (srcSpanStartLine (logRefSrcSpan r) - ln)) mine
                                Nothing -> mine
                header    = T.intercalate ", " $
                                [ tshow errs  <> " error"   <> plural errs  | errs  > 0 ] ++
                                [ tshow warns <> " warning" <> plural warns | warns > 0 ]
                body      = maybe "" (\r -> "\n" <> firstLine (refDescription r)) pick
            return (Just (header <> body))
  where
    plural n  = if n == (1 :: Int) then "" else "s"
    tshow     = T.pack . show
    firstLine = T.strip . T.takeWhile (/= '\n')

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
#if defined(ghcjs_HOST_OS)
requestCompletion _ _ _ cb = cb "[]"
#else
requestCompletion file line ch cb
    | not (isSupportedFile file) = cb "[]"
    | otherwise = withServerReady file (cb "[]") $ \ss ->
        void $ request (ssClient ss) SMethod_TextDocumentCompletion
            (buildParams $ object
                [ "textDocument" .= object [ "uri" .= fileUri file ]
                , "position" .= object [ "line" .= line, "character" .= ch ] ])
            (\case
                Right res -> cb (encodeToText (parseCompletions (toJSON res)))
                Left _    -> cb "[]")
#endif

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
#if defined(ghcjs_HOST_OS)
requestDefinition _ _ _ cb = cb Nothing
#else
requestDefinition file line ch cb
    | not (isSupportedFile file) = cb Nothing
    | otherwise = withServerReady file (cb Nothing) $ \ss ->
        void $ request (ssClient ss) SMethod_TextDocumentDefinition
            (buildParams (posParams file line ch))
            (\case
                Right res -> cb (qualifySpan file <$> firstLocation (toJSON res))
                Left _    -> cb Nothing)
#endif

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
#if defined(ghcjs_HOST_OS)
requestReferences _ _ _ cb = cb []
#else
requestReferences file line ch cb
    | not (isSupportedFile file) = cb []
    | otherwise = withServerReady file (cb []) $ \ss ->
        void $ request (ssClient ss) SMethod_TextDocumentReferences
            (buildParams $ object
                [ "textDocument" .= object [ "uri" .= fileUri file ]
                , "position" .= object [ "line" .= line, "character" .= ch ]
                , "context" .= object [ "includeDeclaration" .= True ] ])
            (\case
                Right res -> attachContext file (parseLocations (toJSON res)) >>= cb
                Left _    -> cb [])
#endif

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
-- Result paths from a remote server are bare (host-local); @ref@ (the file the
-- request was made from) carries the host to re-attach so the Grep pane rows
-- open the right remote file, and remote files are read through the FS seam.
attachContext :: FilePath -> [(FilePath, Int)] -> IO [(FilePath, Int, Text)]
attachContext ref locs = do
    let byFile = Map.toList $
                   Map.fromListWith (++) [ (qualifyLike ref f, [l]) | (f, l) <- locs ]
    rows <- forM byFile $ \(f, ls) -> do
        ls' <- readSourceLines f `catch` \(_ :: SomeException) -> return []
        let lineAt i | i >= 0 && i < length ls' = T.strip (ls' !! i)
                     | otherwise                = ""
        return [ (f, l + 1, lineAt l) | l <- sort (nub ls) ]
    return $ take maxReferences (concat rows)

-- | Read a source file's lines, routing remote (@ssh:\/\/@) paths through the
-- FS seam and local paths through 'TIO.readFile' as before.
readSourceLines :: FilePath -> IO [Text]
#if defined(ghcjs_HOST_OS)
readSourceLines f = T.lines <$> TIO.readFile f
#else
readSourceLines f
    | isRemotePath f = T.lines . decodeUtf8With lenientDecode <$> fsReadFile f
    | otherwise      = T.lines <$> TIO.readFile f
#endif

--------------------------------------------------------------------------------
-- Server lifecycle
--------------------------------------------------------------------------------

-- | Terminate every running language server and empty the registry.  Called
-- on shutdown, and registered as a ghci-mode cleanup so a @:reload@ + fresh
-- @:main@ tears down this run's servers first — otherwise each rebuild spawned
-- a new HLS per root and orphaned the old ones under the long-lived ghci
-- process (they piled up because nothing ever killed them).  Safe to call
-- repeatedly and when nothing is running.
shutdownServers :: IO ()
shutdownServers = do
    old <- modifyMVar registry (\m -> return (Map.empty, m))
    mapM_ reap (Map.toList old)
  where
    reap ((root, _), Just ss) = do
        void (try (stop (ssClient ss)) :: IO (Either SomeException ()))
        debugM "leksah" ("IDE.LSP: stopped language server in " <> root)
    reap (_, Nothing) = return ()

-- | Look up an already-running server for a file's project (never spawns).
withServer :: FilePath -> (ServerState -> IO ()) -> IO ()
withServer file act = case languageOf file of
  Nothing -> return ()
  Just lc -> do
    root <- projectRootOf lc file
    m <- modifyMVar registry (\mp -> return (mp, mp))
    case Map.lookup (root, serverKey lc) m of
        Just (Just ss) -> act ss
        _              -> return ()

-- | Get (spawning if necessary) the server for a project root.  When LSP is
-- disabled in prefs, never spawns and returns 'Nothing' without caching, so
-- re-enabling takes effect on the next edit.
ensureServer :: FilePath -> LangConfig -> IO (Maybe ServerState)
ensureServer root lc = do
    (enabled, cmdPref) <- lspConfig
    -- Remote (ssh://) roots are only supported for Haskell and Nix so far —
    -- the other servers' remote environments have not been wired up.
    let remoteUnsupported = isRemotePath root
                         && lcLanguageId lc `notElem` ["haskell", "nix"]
    if not enabled || remoteUnsupported
        then return Nothing
        else do
            -- A remote server is launched over ssh under the project's command
            -- prefix (e.g. @nix develop .# -c@ / @nix shell … -c@), which is what
            -- puts ghc\/cabal\/HLS on PATH.  Resolve it BEFORE touching the
            -- registry: if the workspace\/project settings are not loaded yet
            -- (e.g. an editor restored from the session before the workspace
            -- opens), DEFER — return 'Nothing' WITHOUT caching, so a later
            -- 'touch' retries once settings are available.  Caching a prefixless
            -- failure here is what left remote LSP permanently dead.
            mPrefix <- if isRemotePath root then remotePrefixFor root
                                            else Just <$> localPrefixFor lc root
            case mPrefix of
                Nothing -> do
                    debugM "leksah" ("IDE.LSP: deferring remote server in " <> root
                                     <> " (workspace settings not loaded yet)")
                    return Nothing
                Just prefix -> do
                    -- (Re-)spawn into the registry.  Factored out so a cached
                    -- entry whose process has since died can be reaped and
                    -- replaced through the same path as a first-time spawn.
                    let spawn m = do
                            cmdArgs@(cmd, _) <- serverCommandFor root cmdPref lc
                            -- A local server whose binary is not on PATH is simply
                            -- unavailable: cache the miss (per root) and stay quiet
                            -- rather than fail a spawn on every new root.
                            -- With a command prefix (remote ssh, or a local
                            -- @nix develop -c@) the server binary lives in THAT
                            -- environment, not on the ambient PATH — so skip the
                            -- local PATH probe (a nix-develop wrap resolves it).
                            available <- if isRemotePath root || isJust prefix
                                            then return True
                                            else isJust <$> findExecutable cmd
                            if not available
                                then do
                                    debugM "leksah" ("IDE.LSP: " <> cmd
                                                     <> " not on PATH; no language server for " <> root)
                                    return (Map.insert key Nothing m, Nothing)
                                else try (spawnAndInit root prefix cmdArgs) >>= \case
                                    Right ss -> return (Map.insert key (Just ss) m, Just ss)
                                    Left (e :: SomeException) -> do
                                        debugM "leksah" ("IDE.LSP: could not start language server in "
                                                         <> root <> ": " <> show e)
                                        return (Map.insert key Nothing m, Nothing)
                    modifyMVar registry $ \m -> case Map.lookup key m of
                        -- Known-unavailable (binary missing / start failed): stay
                        -- quiet, don't retry the spawn on every keystroke.
                        Just Nothing   -> return (m, Nothing)
                        -- A cached server — reuse only if its process is still
                        -- alive; otherwise reap the dead handles/threads and
                        -- respawn (covers an HLS that crashed or was killed).
                        Just (Just ss) -> alive (ssClient ss) >>= \case
                            True  -> return (m, Just ss)
                            False -> do
                                debugM "leksah" ("IDE.LSP: language server in " <> root
                                                 <> " had exited; respawning")
                                void (try (stop (ssClient ss)) :: IO (Either SomeException ()))
                                spawn m
                        Nothing        -> spawn m
  where key = (root, serverKey lc)

spawnAndInit :: FilePath -> Maybe Text -> (FilePath, [String]) -> IO ServerState
spawnAndInit root prefix (cmd, args) = do
    ready   <- newTVarIO False
    pending <- newTVarIO []
    vers    <- newTVarIO Map.empty
    let cfg = defaultClientConfig
            { onNotification = handleNotification root
            , onStderr = \l -> debugM "leksah" ("HLS[" <> root <> "]: " <> T.unpack l) }
    client <- spawnClient root prefix cmd args cfg
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

-- | Spawn the language-server client for a project root.  A local root runs
-- the server in @root@ as before; a remote (@ssh:\/\/host\/…@) root runs it ON
-- the host over ssh stdio: @ssh host \'cd rroot && exec \<prefix\> \<cmd\>\'@.
-- cwd\/env MUST be 'Nothing' — an ssh:\/\/ cwd would crash createProcess, and
-- the local env (nix PATH, …) must not reach the ssh client.  The per-project
-- command @prefix@ (e.g. @nix develop .# -c@), resolved by the caller, supplies
-- the remote server's environment, exactly as it does for remote builds.
spawnClient :: FilePath -> Maybe Text -> FilePath -> [String] -> ClientConfig -> IO Client
#if defined(ghcjs_HOST_OS)
spawnClient root _prefix cmd args cfg = start cmd args (Just root) Nothing cfg
#else
spawnClient root prefix cmd args cfg = case parseRemotePath root of
    Just (host, rroot) -> do
        let remoteCmd = "cd " <> shellQuote (T.pack rroot) <> " && exec "
                     <> maybe "" (<> " ") prefix
                     <> T.unwords (map (shellQuote . T.pack) (cmd : args))
            (sshCmd, sshArgs) = remoteSshArgs host remoteCmd
        debugM "leksah" ("IDE.LSP: starting remote HLS on " <> T.unpack host
                         <> ": " <> T.unpack remoteCmd)
        start sshCmd (map T.unpack sshArgs) Nothing Nothing cfg
    Nothing -> case prefix of
        -- A local project with a command prefix (e.g. @nix develop -c@, from the
        -- project's settings or auto-detected from a flake.nix) runs its server
        -- INSIDE that environment, so rust-analyzer / pyright / … come from the
        -- flake's dev shell rather than the ambient PATH.  cwd = root so the
        -- flake there is the one entered.
        Just p | not (T.null (T.strip p)) -> do
            let full = "exec " <> p <> " "
                    <> T.unwords (map (shellQuote . T.pack) (cmd : args))
            debugM "leksah" ("IDE.LSP: starting local server in " <> root
                             <> " under prefix: " <> T.unpack full)
            start "sh" ["-c", T.unpack full] (Just root) Nothing cfg
        _ -> start cmd args (Just root) Nothing cfg
#endif

-- | Resolve the command prefix for a REMOTE project root from the live
-- workspace settings.  The nesting distinguishes two cases the caller must
-- treat differently:
--
--   * @Nothing@        — no workspace, or no project matching @root@, is loaded
--     yet (e.g. an editor restored before the workspace opened).  The caller
--     should DEFER the spawn rather than launch without a prefix.
--   * @Just prefix@     — a project was found; @prefix@ is its (possibly
--     'Nothing') @psCmdPrefix@.
remotePrefixFor :: FilePath -> IO (Maybe (Maybe Text))
remotePrefixFor root = getGlobalIDERef >>= \case
    Nothing   -> return Nothing
    Just ideR -> do
        mbWs <- reflectIDE (readIDE workspace) ideR
        -- 'isSubPath' compares 'splitPath' components, and 'splitPath' leaves a
        -- trailing slash on every component EXCEPT the last — so a bare dir
        -- @…\/proj@ (last component @proj@, no slash) never matches the same
        -- name appearing mid-path (@proj\/@).  'pjDir' happens to carry a
        -- trailing slash ('dropFileName') but @root@ (from 'renderRemotePath')
        -- does not, which broke the exact-match case; forcing a trailing slash
        -- on BOTH normalises every component and makes 'isSubPath' correct for
        -- @pjDir == root@ (single-package) and @pjDir@ an ancestor of @root@
        -- (multi-package, file under a sub-package).
        let root' = addTrailingPathSeparator root
            res = do
                ws      <- mbWs
                project <- find (\p -> addTrailingPathSeparator (pjDir (pjKey p))
                                         `isSubPath` root') (ws ^. wsProjects)
                Just (psCmdPrefix (wsSettingsFor (pjKey project) ws))
        debugM "leksah" ("IDE.LSP: remotePrefixFor " <> root <> " -> "
                         <> show (fmap (fmap T.unpack) res)
                         <> " (workspace=" <> show (isJust mbWs)
                         <> " projects=" <> show (maybe 0 (length . (^. wsProjects)) mbWs) <> ")")
        return res

-- | The command prefix for a LOCAL project server.  Prefers the project's
-- explicit @psCmdPrefix@ (Project Settings…), and otherwise — for a non-Haskell
-- project whose root has a @flake.nix@ — defaults to @nix develop -c@ so the
-- server (rust-analyzer, pyright, …) runs inside the flake's dev shell.  Haskell
-- HLS is deliberately left on the ambient PATH (as before), since wrapping it
-- could hide a working install when the flake doesn't provide it.
localPrefixFor :: LangConfig -> FilePath -> IO (Maybe Text)
localPrefixFor lc root = localExplicitPrefix root >>= \case
    Just p | not (T.null (T.strip p)) -> return (Just p)
    _ -> do
        hasFlake <- doesFileExist (root </> "flake.nix")
        return $ if hasFlake && lcLanguageId lc /= "haskell"
                    then Just "nix develop -c"
                    else Nothing

-- | The explicit @psCmdPrefix@ of the workspace project containing a LOCAL
-- @root@ (mirrors 'remotePrefixFor'\''s project lookup; 'Nothing' if no IDE /
-- workspace / matching project / prefix).
localExplicitPrefix :: FilePath -> IO (Maybe Text)
localExplicitPrefix root = getGlobalIDERef >>= \case
    Nothing   -> return Nothing
    Just ideR -> do
        mbWs <- reflectIDE (readIDE workspace) ideR
        let root' = addTrailingPathSeparator root
        return $ do
            ws      <- mbWs
            project <- find (\p -> addTrailingPathSeparator (pjDir (pjKey p))
                                     `isSubPath` root') (ws ^. wsProjects)
            psCmdPrefix (wsSettingsFor (pjKey project) ws)

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
    , "rootUri" .= fileUri root
    , "capabilities" .= object
        [ "textDocument" .= object
            [ "synchronization" .= object [ "didSave" .= True ]
            , "publishDiagnostics" .= object [ "relatedInformation" .= True ]
            -- Advertise the request features we actually use.  For hover, prefer
            -- plain text but accept markdown ('extractHover' handles both).
            , "hover" .= object
                [ "contentFormat" .= (["plaintext", "markdown"] :: [Text]) ]
            , "definition" .= object [ "linkSupport" .= False ]
            , "references" .= object []
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
            | Just file0 <- uriTextToFilePath uriText -> do
                -- The server reports host-local paths; re-attach the root's
                -- host so refs match the editor's ssh:// buffer.
                let file    = qualifyLike root file0
                    newRefs = map (toLogRef root file) diags
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
withServerReady file onNone act = case languageOf file of
  Nothing -> onNone
  Just lc -> do
    root <- projectRootOf lc file
    m <- modifyMVar registry (\mp -> return (mp, mp))
    case Map.lookup (root, serverKey lc) m of
        Just (Just ss) -> onReady ss (act ss)
        _              -> onNone

-- | @TextDocumentPositionParams@ for a file + LSP 0-based position.
posParams :: FilePath -> Int -> Int -> Value
posParams file line ch = object
    [ "textDocument" .= object [ "uri" .= fileUri file ]
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

--------------------------------------------------------------------------------
-- URI boundary: leksah ssh://host/abs paths  <->  the remote-local paths the
-- language server (running ON the host) speaks.  A server is bound to one root,
-- hence one host, so we strip the host on the way out and re-attach the
-- requesting file's/root's host on the way back (every path a remote server
-- returns — including /nix/store dependency paths — lives on that host).
--------------------------------------------------------------------------------

-- | The path the server expects: for @ssh:\/\/host\/abs@ that is @\/abs@; a
-- local path is unchanged.
localPart :: FilePath -> FilePath
localPart p = maybe p snd (parseRemotePath p)

-- | Build the @DocumentUri@ 'Value' for a (possibly remote) leksah path,
-- stripping any @ssh:\/\/host@ so the server sees its own filesystem.
fileUri :: FilePath -> Value
fileUri = toJSON . filePathToUri . localPart

-- | Re-attach the host of a reference leksah path (the requesting file, or the
-- server root) to a bare local path the server returned.  Local reference ->
-- returned path unchanged.
qualifyLike :: FilePath -> FilePath -> FilePath
qualifyLike ref p = case parseRemotePath ref of
    Just (host, _) -> renderRemotePath host p
    Nothing        -> p

-- | 'qualifyLike' applied to a 'SrcSpan' filename.
qualifySpan :: FilePath -> SrcSpan -> SrcSpan
qualifySpan ref s = s { srcSpanFilename = qualifyLike ref (srcSpanFilename s) }

-- | Walk up from a source file to the nearest project root.  Remote files walk
-- up ON the host in one ssh round trip (never 'makeAbsolute', which mangles
-- @ssh:\/\/@); local files as before.  Languages with 'lcRootMarkers' look for
-- one of their marker files, falling back to the nearest @.git@ dir; Haskell
-- (empty markers) keeps the @cabal.project@ \/ @stack.yaml@ \/ @*.cabal@ walk.
findProjectRoot :: LangConfig -> FilePath -> IO FilePath
findProjectRoot lc file
    | isRemotePath file = remoteFindProjectRoot file
    | otherwise = do
        abs' <- makeAbsolute file
        let dir0 = takeDirectory abs'
        case lcRootMarkers lc of
            [] -> do
                let loop dir = hasMarker dir >>= \case
                        True  -> return (Just dir)
                        False -> let up = takeDirectory dir
                                 in if up == dir then return Nothing else loop up
                loop dir0 >>= maybe (return dir0) return
            markers -> do
                -- Remember the lowest .git dir seen on the way up, but keep
                -- walking in case a marker sits above it.
                let loop dir mbGit = do
                        hit <- or <$> mapM (\mk -> doesFileExist (dir </> mk)) markers
                        if hit
                            then return dir
                            else do
                                mbGit' <- case mbGit of
                                    Just _  -> return mbGit
                                    Nothing -> doesDirectoryExist (dir </> ".git") >>= \g ->
                                        return (if g then Just dir else Nothing)
                                let up = takeDirectory dir
                                if up == dir
                                    then return (fromMaybe dir0 mbGit')
                                    else loop up mbGit'
                loop dir0 Nothing

-- | Remote analogue: one ssh script walks up from the file's directory.  It
-- PREFERS the @cabal.project@ \/ @stack.yaml@ root (returned as soon as one is
-- seen walking up) over an intermediate package's @*.cabal@ — so a file under a
-- sub-package of a multi-package project resolves to the project root, which is
-- where @cabal.project@ and the flake live (a per-project @nix develop .#@
-- prefix must run there, not in the package dir).  It only falls back to the
-- lowest bare @*.cabal@ dir when no @cabal.project@\/@stack.yaml@ exists above.
-- The result is re-prefixed with the file's host.
remoteFindProjectRoot :: FilePath -> IO FilePath
#if defined(ghcjs_HOST_OS)
remoteFindProjectRoot = return . takeDirectory
#else
remoteFindProjectRoot file = case parseRemotePath file of
    Nothing             -> return (takeDirectory file)
    Just (host, rlocal) -> do
        let startDir = takeDirectory rlocal
            -- The cabal-file test uses `find … -name '*.cabal'` rather than a
            -- shell glob so this source has no slash-star (CPP would read that
            -- as the start of a C comment).
            script = T.intercalate "\n"
                [ "d=\"$0\""
                , "pkg=\"\""
                , "while :; do"
                -- A cabal.project / stack.yaml IS the project root: return at once.
                , "  if [ -f \"$d/cabal.project\" ] || [ -f \"$d/stack.yaml\" ]; then"
                , "    echo \"$d\"; exit 0"
                , "  fi"
                -- Remember the lowest bare *.cabal dir as a fallback, but keep
                -- walking up in case a cabal.project sits above it.
                , "  if [ -z \"$pkg\" ] && "
                    <> "[ -n \"$(find \"$d\" -maxdepth 1 -name '*.cabal' 2>/dev/null)\" ]; then"
                , "    pkg=\"$d\""
                , "  fi"
                , "  p=$(dirname \"$d\")"
                , "  [ \"$p\" = \"$d\" ] && break"
                , "  d=\"$p\""
                , "done"
                , "echo \"$pkg\"" ]
            fallback = renderRemotePath host startDir
            pick (_ec, out, _) =
                case T.strip (decodeUtf8With lenientDecode out) of
                    r | T.null r  -> fallback
                      | otherwise -> renderRemotePath host (T.unpack r)
        (pick <$> runSsh host script [T.pack startDir] mempty)
            `catch` \(_ :: SomeException) -> return fallback
#endif

hasMarker :: FilePath -> IO Bool
hasMarker dir = do
    pj  <- doesFileExist (dir </> "cabal.project")
    sy  <- doesFileExist (dir </> "stack.yaml")
    cbl <- any ((== ".cabal") . takeExtension)
             <$> (listDirectory dir `catch` \(_ :: SomeException) -> return [])
    return (pj || sy || cbl)
