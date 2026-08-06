-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Compiler-output diagnostics: the conduit sinks the build\/run commands
-- pipe their 'ToolOutput' through.  Each sink writes every line to the Log
-- pane (tagged input\/output\/error\/frame) and, for the build sinks, parses
-- error\/warning spans into 'LogRef's for the Errors pane, the editor
-- gutters and the status-bar counts.
--
-- A fresh replacement for the old @IDE.LogRef@: the GHC\/stack\/nix state
-- machine is re-expressed from the observable output formats; the
-- cargo\/rustc parser and the remote-root span handling carry over from the
-- recent cargo\/remote-projects work.  The classic-era machinery (elm
-- support, doctest failures, ghci breakpoints\/contexts, error navigation —
-- the web UI navigates its own 'allLogRefs' state) is gone.
module IDE.Diagnostics
  ( logOutput
  , logOutputDefault
  , logOutputForBuild
  , logOutputForCargoBuild
    -- * Exposed for testing
  , spanParser
  , cargoHeaderParser
  , cargoLocationParser
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (rnf)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (filterM, unless, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

import Data.Attoparsec.Text
       ((<?>), Parser, anyChar, char, decimal, endOfInput, manyTill,
        parseOnly, skipSpace, string, takeText, try)
import qualified Data.Attoparsec.Text as AP (takeWhile, takeWhile1)
import Data.Char (isDigit)
import Data.Conduit (ConduitT, Void)
import qualified Data.Conduit.List as CL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M (insert, lookup)
import Data.Set (Set)
import qualified Data.Set as S (empty, insert, member)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), isAbsolute, makeRelative)
import System.IO.Unsafe (unsafePerformIO)

import IDE.Core.State
       (IDEM, Log(..), LogRef(..), LogRefType(..), LogTag(..),
        Project(..), SrcSpan(..), ipdCabalFile, ipdPackageDir, isError,
        liftIDE, logRefFullFilePath, logRootPath, pjPackages)
import IDE.Gtk.State (LogLaunch, postSyncIDE)
import IDE.Pane.Log (IDELog, getDefaultLogLaunch, getLog)
import qualified IDE.Pane.Log as Log
import IDE.Pane.SourceBuffer
       (addLogRef, removeBuildLogRefs, removeFileExtLogRefs)
import IDE.Utils.Process (ToolOutput(..))
import IDE.Utils.RemoteExec (runSsh)
import IDE.Utils.RemotePath (isRemotePath, parseRemotePath, renderRemotePath)

-- ---------------------------------------------------------------------
-- Plain log streaming (no diagnostic parsing)
-- ---------------------------------------------------------------------

-- | Write one 'ToolOutput' to the log, tagged; prompts and exits draw a
-- separator frame.  Returns the log line number.
logLine :: IDELog -> LogLaunch -> ToolOutput -> IDEM Int
logLine ideLog logLaunch = \case
    ToolInput  line -> append (line <> "\n") InputTag
    ToolOutput line -> append (line <> "\n") LogTag
    ToolError  line -> append (line <> "\n") ErrorTag
    ToolPrompt line -> do
        unless (T.null line) . void $ append (line <> "\n") LogTag
        append (frameText Nothing) FrameTag
    ToolExit code -> append (frameText (Just code)) FrameTag
  where
    append = Log.appendLog ideLog logLaunch

-- | The separator drawn at a prompt or exit; a non-zero exit shows its code.
frameText :: Maybe ExitCode -> Text
frameText = \case
    Just ExitSuccess     -> T.replicate 41 "-" <> "\n"
    Just (ExitFailure n) ->
        let label = " exit " <> T.pack (show n) <> " "
            pad   = max 0 (41 - T.length label)
        in T.replicate (pad `div` 2) "=" <> label
           <> T.replicate (pad - pad `div` 2) "=" <> "\n"
    Nothing              -> T.replicate 41 "-" <> "\n"

-- | Stream tool output to a specific log launch, no parsing.
logOutput :: LogLaunch -> ConduitT ToolOutput Void IDEM ()
logOutput logLaunch = do
    ideLog <- lift $ postSyncIDE getLog
    CL.mapM_ (postSyncIDE . void . logLine ideLog logLaunch)

-- | 'logOutput' to the default log launch.
logOutputDefault :: ConduitT ToolOutput Void IDEM ()
logOutputDefault = lift getDefaultLogLaunch >>= logOutput

-- ---------------------------------------------------------------------
-- GHC / stack / cabal / nix build output
-- ---------------------------------------------------------------------

-- | One classified line of build output.
data BuildLine
    = LProgress FilePath      -- ^ @[ 3 of 133] Compiling M ( file, … )@
    | LSpan SrcSpan LogRefType Text
                              -- ^ a diagnostic header carrying a source span
    | LInfo                   -- ^ progress noise that is not an error even on
                              --   stderr (Linking, ld:, ar:, …)
    | LBlank
    | LOther Text

classifyLine :: Text -> BuildLine
classifyLine line = either (const (LOther line)) id (parseOnly lineParser line)
  where
    lineParser :: Parser BuildLine
    lineParser =
            try (do
                -- [ 12 of 133] Compiling IDE.Foo ( src/IDE/Foo.hs, … )
                _ <- char '[' *> skipSpace *> int
                _ <- skipSpace *> string "of" *> skipSpace *> int
                _ <- char ']' *> skipSpace *> string "Compiling"
                _ <- AP.takeWhile (/= '(') <* char '(' <* skipSpace
                file <- AP.takeWhile (\c -> c /= ',' && c /= ')')
                _ <- takeText
                return (LProgress (T.unpack (T.strip file))))
        <|> try (do
                -- nix: error: MESSAGE, at /path/file.nix:12:3
                _ <- string "error: "
                msg <- T.pack <$> manyTill anyChar (string ", at ")
                sp <- spanParser
                endOfInput
                return (LSpan sp ErrorRef msg))
        <|> try (do
                -- GHC/stack: FILE:SPAN: [error:|warning:|…] MESSAGE
                skipSpace
                sp <- spanParser
                _ <- char ':'
                skipSpace
                refType <-
                        (WarningRef <$ (string "Warning:" <|> string "warning:"))
                    <|> (ErrorRef <$ (string "Error:" <|> string "error:"))
                    <|> (TestFailureRef <$ string "failure")
                    <|> pure ErrorRef
                LSpan sp refType <$> takeText)
        <|> try (do
                -- progress noise that arrives on stderr
                _ <- string "Linking " <|> string "ld: " <|> string "ld warning: "
                     <|> string "ar: " <|> string "Preprocessing "
                     <|> string "Building " <|> string "Configuring "
                _ <- takeText
                return LInfo)
        <|> try (skipSpace *> endOfInput *> pure LBlank)
        <?> "buildLine"

-- | A GHC-style source span, in any of the shapes compilers print:
-- @file:12:3@, @file:12:3-9@, @file:(12,3)-(14,9)@ or bare @file:12@.
-- cabal haddock's @dist\/build\/tmp-NNN\/@ prefix is stripped.
spanParser :: Parser SrcSpan
spanParser = do
    file <- pathParser
    _ <- char ':'
    parens file <|> plain file
  where
    parens file = do
        (l1, c1) <- pair
        _ <- char '-'
        (l2, c2) <- pair
        return (SrcSpan file l1 (cvt c1) l2 (cvt c2))
    plain file = do
        l <- int
        (do _ <- char ':'
            c1 <- int
            (do _ <- char '-'
                c2 <- int
                return (SrcSpan file l (cvt c1) l (cvt c2)))
              <|> return (SrcSpan file l (cvt c1) l (cvt c1)))
          <|> return (SrcSpan file l 0 l 0)
    pair = do
        _ <- char '('
        l <- int
        _ <- char ','
        c <- int <* char ')'
        return (l, c)
    -- Diagnostics are 1-based; the stored span is 0-based columns.
    cvt c = max 0 (c - 1)

pathParser :: Parser FilePath
pathParser = T.unpack <$>
        (try (do _ <- string "dist/build/tmp-"
                 _ <- AP.takeWhile1 isDigit
                 _ <- char '/'
                 AP.takeWhile (/= ':'))
         <|> AP.takeWhile1 (/= ':'))

int :: Parser Int
int = decimal

-- | The build sink: stream everything to the log; collect a 'LogRef' per
-- diagnostic (multi-line messages accumulate until the next recognisable
-- line); clear a file's old refs when the compiler recompiles it; a summary
-- frame line closes the run.  Returns the collected refs.
--
-- The third argument is the backgroundBuild flag (a background build's refs
-- don't steal the selection); the fourth (jump-to-warnings) is accepted for
-- call-site compatibility and unused, as before.
logOutputForBuild :: Project -> Log -> Bool -> Bool
                  -> ConduitT ToolOutput Void IDEM [LogRef]
logOutputForBuild project logSource backgroundBuild _jumpToWarnings = do
    ideLog <- lift $ postSyncIDE getLog
    logLaunch <- lift getDefaultLogLaunch
    -- nix builds don't announce which files they compile, so their old refs
    -- can't be cleared per-file below — wipe them up front.
    lift . postSyncIDE $ removeFileExtLogRefs logSource ".nix" [ErrorRef, WarningRef]
    st <- CL.foldM (step ideLog logLaunch) emptySt
    lift . postSyncIDE $ return (reverse (bsDone (closePending st)))
  where
    step ideLog logLaunch st out = do
        liftIO . evaluate $ rnf out
        liftIDE . postSyncIDE $ case out of
            ToolInput line -> do
                _ <- Log.appendLog ideLog logLaunch (line <> "\n") InputTag
                return st
            ToolOutput line -> online ideLog logLaunch st line LogTag
            ToolError  line -> online ideLog logLaunch st line ErrorTag
            ToolPrompt line -> do
                unless (T.null line) . void $
                    Log.appendLog ideLog logLaunch (line <> "\n") LogTag
                finish ideLog logLaunch st Nothing
            ToolExit code -> finish ideLog logLaunch st (Just code)
    -- stack prints everything to stderr; classify both channels the same and
    -- only fall back to the channel's tag for unrecognised lines.
    online ideLog logLaunch st line chanTag = case classifyLine line of
        LProgress file -> do
            _ <- Log.appendLog ideLog logLaunch (line <> "\n") LogTag
            full <- liftIO $ resolveCompiledFile project logSource file
            removeBuildLogRefs full
            emit (closePending st)
        LSpan sp refType msg -> do
            let tag = if refType == WarningRef then LogTag else ErrorTag
            lineNr <- Log.appendLog ideLog logLaunch (line <> "\n") tag
            lg <- liftIO $ findLog project logSource (srcSpanFilename sp)
            let ref = LogRef (remoteNormalizeSpan lg sp) lg msg
                             Nothing (Just (lineNr, lineNr)) refType
                full = logRefFullFilePath ref
            -- The compiler's first diagnostic for a file it did not announce
            -- (ghci loads, cabal repl) still invalidates that file's old refs.
            unless (full `S.member` bsCleared st) $ removeBuildLogRefs full
            st' <- emit (closePending st)
            return st' { bsPending = Just ref
                       , bsCleared = S.insert full (bsCleared st') }
        LInfo -> do
            _ <- Log.appendLog ideLog logLaunch (line <> "\n") InfoTag
            return st
        LBlank -> do
            _ <- Log.appendLog ideLog logLaunch (line <> "\n") chanTag
            -- GHC messages can span blank lines; keep the pending ref open.
            return st
        LOther _ -> case bsPending st of
            -- A continuation line of the open diagnostic: extend its message
            -- and its log-line range.
            Just ref -> do
                lineNr <- Log.appendLog ideLog logLaunch (line <> "\n")
                              (if logRefType ref == WarningRef then LogTag else ErrorTag)
                let msg' = if T.null (refDescription ref)
                              then line
                              else refDescription ref <> "\n" <> line
                    span' = fmap (\(l1, _) -> (l1, lineNr)) (logLines ref)
                return st { bsPending = Just ref { refDescription = msg'
                                                 , logLines = span' } }
            Nothing -> do
                _ <- Log.appendLog ideLog logLaunch (line <> "\n") chanTag
                return st
      where
        emit s = case bsDone s of
            (ref : _) | justClosed s -> do
                addLogRef False backgroundBuild ref
                return s { bsJustClosed = False }
            _ -> return s
    finish ideLog logLaunch st mbCode = do
        st' <- case closePending st of
            s | bsJustClosed s, (ref : _) <- bsDone s -> do
                    addLogRef False backgroundBuild ref
                    return s { bsJustClosed = False }
              | otherwise -> return s
        let refs = bsDone st'
            errorNum = length (filter isError refs)
            warnNum  = length refs - errorNum
        _ <- if null refs
            then Log.appendLog ideLog logLaunch (frameText mbCode) FrameTag
            else Log.appendLog ideLog logLaunch
                    ("----- " <> T.pack (show errorNum) <> " errors -- "
                     <> T.pack (show warnNum) <> " warnings -----\n") FrameTag
        return st'

-- | The build fold's state: the diagnostic still accumulating lines, the
-- finished ones (newest first), and which files have had their stale refs
-- cleared this run.
data BuildSt = BuildSt
    { bsPending    :: Maybe LogRef
    , bsDone       :: [LogRef]
    , bsCleared    :: Set FilePath
    , bsJustClosed :: Bool  -- ^ head of 'bsDone' not yet added to the store
    }

emptySt :: BuildSt
emptySt = BuildSt Nothing [] S.empty False

justClosed :: BuildSt -> Bool
justClosed = bsJustClosed

closePending :: BuildSt -> BuildSt
closePending st = case bsPending st of
    Nothing  -> st
    Just ref -> st { bsPending = Nothing
                   , bsDone = ref : bsDone st
                   , bsJustClosed = True }

-- | Where a file the compiler announced actually lives (for the
-- clear-old-refs key): absolute stays; else resolve against the log root or
-- the one project package that has it.
resolveCompiledFile :: Project -> Log -> FilePath -> IO FilePath
resolveCompiledFile project logSource file
    | isAbsolute file && not (isRemotePath (logRootPath logSource)) = return file
    | otherwise = do
        lg <- findLog project logSource file
        let file' = srcSpanFilename
                        (remoteNormalizeSpan lg (SrcSpan file 0 0 0 0))
        return $ if isAbsolute file' || isRemotePath file'
                    then file'
                    else logRootPath lg </> file'

-- | Diagnostics sometimes name files relative to another package of the
-- project (dependency builds): pin the ref to the package that actually has
-- the file.  Remote roots can't be probed with local 'doesFileExist' — one
-- batched (and memoised) ssh existence check disambiguates instead.
findLog :: Project -> Log -> FilePath -> IO Log
findLog project lg file
    | isRemotePath (logRootPath lg) = findRemoteLog project lg file
    | otherwise =
        doesFileExist (logRootPath lg </> file) >>= \case
            True -> return lg
            False ->
                filterM (\p -> doesFileExist (ipdPackageDir p </> file))
                        (pjPackages project) >>= \case
                    [p] -> return (LogCabal (ipdCabalFile p))
                    _   -> return lg

-- Cache of remote findLog answers, keyed by (root, error filename); a build
-- of a big project can emit hundreds of spans for the same few files and
-- each miss costs an ssh round trip.
{-# NOINLINE remoteFindLogCache #-}
remoteFindLogCache :: IORef (Map (FilePath, FilePath) Log)
remoteFindLogCache = unsafePerformIO (newIORef mempty)

findRemoteLog :: Project -> Log -> FilePath -> IO Log
findRemoteLog project lg file = do
    let key = (logRootPath lg, file)
    cached <- M.lookup key <$> readIORef remoteFindLogCache
    case cached of
        Just l  -> return l
        Nothing -> do
            l <- probe `catch` \(_ :: SomeException) -> return lg
            atomicModifyIORef' remoteFindLogCache (\m -> (M.insert key l m, ()))
            return l
  where
    candidates =
        (lg, logRootPath lg </> file)
        : [ (LogCabal (ipdCabalFile p), ipdPackageDir p </> file)
          | p <- pjPackages project ]
    -- ONE ssh exec: print the index of the first candidate that exists.
    probe = case parseRemotePath (logRootPath lg) of
        Nothing -> return lg
        Just (host, _) -> do
            let rpaths = [ maybe p snd (parseRemotePath p) | (_, p) <- candidates ]
            -- args bind $0,$1,…: $0 is NOT in "$@", so pass a dummy $0.
            (code, out, _) <- runSsh host
                "i=0; for f in \"$@\"; do if [ -f \"$f\" ]; then echo $i; exit 0; fi; i=$((i+1)); done; exit 44"
                ("leksah-findlog" : map T.pack rpaths) mempty
            return $ case (code, reads (T.unpack (T.strip (decodeUtf8 out)))) of
                (ExitSuccess, [(i, "")]) | i < length candidates -> fst (candidates !! i)
                _ -> lg

-- | Rewrite an error span whose filename is absolute on the REMOTE host
-- (GHC prints /home/… paths for out-of-package spans): relative to the
-- root when it is under it, else re-prefixed with ssh://host — so every
-- stored span composes/deduplicates consistently downstream.
remoteNormalizeSpan :: Log -> SrcSpan -> SrcSpan
remoteNormalizeSpan lg sp =
    case parseRemotePath (logRootPath lg) of
        Just (host, rroot)
          | isAbsolute f && not (isRemotePath f) ->
            let rel = makeRelative rroot f
            in sp { srcSpanFilename =
                        if rel /= f then rel else renderRemotePath host f }
        _ -> sp
  where f = srcSpanFilename sp

-- ---------------------------------------------------------------------
-- cargo / rustc build output
-- ---------------------------------------------------------------------

-- | A rustc/cargo diagnostic header at column 0: @error[E0432]: …@,
-- @error: …@ or @warning: …@.  Returns the ref type and the (re-labelled)
-- message.  Summary lines like @error: could not compile …@ also match here,
-- but 'logOutputForCargoBuild' only records a ref once a @-->@ location line
-- follows, and summaries have none — so they never inflate the count.
cargoHeaderParser :: Parser (LogRefType, Text)
cargoHeaderParser = try (do
        _ <- string "error"
        _ <- (char '[' *> AP.takeWhile (/= ']') <* char ']') <|> pure ""
        _ <- string ": "
        msg <- takeText
        return (ErrorRef, "error: " <> msg))
    <|> try (do
        _ <- string "warning: "
        msg <- takeText
        return (WarningRef, "warning: " <> msg))
    <?> "cargoHeaderParser"

-- | A rustc/cargo location line: @  --> src/main.rs:12:9@.
cargoLocationParser :: Parser SrcSpan
cargoLocationParser = skipSpace *> string "-->" *> skipSpace *> spanParser
    <?> "cargoLocationParser"

-- | Build-output consumer for cargo/rustc (Rust projects added via \"Open
-- Folder\").  The GHC/stack parser in 'logOutputForBuild' mis-reads cargo and
-- rustup output — tagging plain @info:@/progress lines as errors and inflating
-- the Errors count.  This parser instead records an error/warning ref ONLY for
-- a rustc diagnostic that has a real source location (a @-->@ line following an
-- @error…:@ / @warning:@ header), so the status-bar count matches the number
-- of clickable diagnostics.  Everything else — rustup toolchain downloads,
-- @Compiling …@ progress, the final @could not compile … due to N errors@
-- summary — is logged plainly and not counted.
logOutputForCargoBuild :: Project -> Log -> Bool
                       -> ConduitT ToolOutput Void IDEM [LogRef]
logOutputForCargoBuild project logSource backgroundBuild = do
    logLaunch <- lift getDefaultLogLaunch
    ideLog <- lift $ postSyncIDE getLog
    -- NB: do NOT clear .rs refs here — for a Rust project rust-analyzer owns
    -- the live .rs diagnostics in the same store, and wiping them on every
    -- build would fight the LSP.  cargo's own build errors are added on top.
    (_pending, refs) <- CL.foldM (step logLaunch ideLog) (Nothing, [])
    return (reverse refs)
  where
    tagFor rt = if rt == ErrorRef then ErrorTag else LogTag
    step logLaunch ideLog st output = do
        liftIO . evaluate $ rnf output
        liftIDE . postSyncIDE $ case output of
            ToolInput line -> do
                _ <- Log.appendLog ideLog logLaunch (line <> "\n") InputTag
                return st
            ToolPrompt _  -> finish logLaunch ideLog st Nothing
            ToolExit code -> finish logLaunch ideLog st (Just code)
            ToolOutput line -> processLine logLaunch ideLog st line
            ToolError  line -> processLine logLaunch ideLog st line
    -- cargo/rustc/rustup interleave on stdout and stderr, so both channels go
    -- through the same diagnostic parsing.
    processLine logLaunch ideLog (pending, refs) line =
        case (pending, parseOnly cargoLocationParser line) of
            (Just (rt, msg), Right sp) -> do
                lineNr <- Log.appendLog ideLog logLaunch (line <> "\n") (tagFor rt)
                lg <- liftIO $ findLog project logSource (srcSpanFilename sp)
                let ref = LogRef (remoteNormalizeSpan lg sp) lg msg
                                 Nothing (Just (lineNr, lineNr)) rt
                addLogRef False backgroundBuild ref
                return (Nothing, ref : refs)
            _ -> case parseOnly cargoHeaderParser line of
                Right hdr@(rt, _) -> do
                    _ <- Log.appendLog ideLog logLaunch (line <> "\n") (tagFor rt)
                    return (Just hdr, refs)
                _ -> do
                    _ <- Log.appendLog ideLog logLaunch (line <> "\n") LogTag
                    return (pending, refs)
    finish logLaunch ideLog st@(_pending, refs) mbCode = do
        let errorNum = length (filter isError refs)
            warnNum  = length refs - errorNum
        _ <- if null refs
            then Log.appendLog ideLog logLaunch (frameText mbCode) FrameTag
            else Log.appendLog ideLog logLaunch
                    ("----- " <> T.pack (show errorNum) <> " errors -- "
                     <> T.pack (show warnNum) <> " warnings -----\n") FrameTag
        return st
