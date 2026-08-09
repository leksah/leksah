{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- | Generate demo-hovers.js — precomputed LSP hover tooltips for the demo.
--
-- This is the demo's stand-in for a live language server, built from REAL
-- haskell-language-server responses:
--
--   1. Enumerate every hoverable identifier
--        * in the static terminal windows (terminals/*.ans): walk each dump
--          with the same header/gutter/identifier recognition the runtime
--          does in JS (terminalLinksJs, leksah/src/IDE/Web/Main.hs — keep the
--          regexes in sync!), so every token the demo will underline gets a
--          tooltip; and
--        * in the packed demo sources (editor hovers): every identifier span
--          in each .hs file of DemoManifest.hoverRoots.
--   2. Query a real `haskell-language-server --lsp` over the real repo files
--      at each position (stdio JSON-RPC; run from the dev shell) and render
--      the hover contents the way IDE.LSP.extractHover does.
--   3. Emit window.leksahDemoHovers = { demoPath: { line0: [[colStart,
--      colEnd, markdown], …] } } — IDE.Web.DemoHovers serves lookups from it.
--
-- Failure policy ("find all the tooltips needed"): a terminal-derived
-- identifier with no HLS hover FAILS the run (listing the misses), except
--   * Haskell keywords / pragma words (keywords below),
--   * tokens listed in hover-ignore.txt (curated exceptions),
--   * tokens on '+' diff lines whose text doesn't match the real file (the
--     fixture marks those as hypothetical edits — the runtime can't hover
--     them either, since HLS positions wouldn't line up).
-- Editor-sweep misses are only counted (a whole file always has spans with
-- nothing to say).
--
-- Responses are cached in .hover-cache.json keyed by file-content hash +
-- position, so re-runs without source changes skip HLS entirely.
--
-- Run from docs/website/try (dev shell):  runghc gen-demo-hovers.hs
-- Env: GEN_HOVERS_STDERR=<file> to keep HLS stderr, GEN_HOVERS_TRACE=1 to
-- log server->client traffic.
module Main (main) where

import Control.Exception
  (Exception, catch, finally, throwIO, try, SomeException)
import Control.Monad (foldM, forM, forM_, guard, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit, isSpace, toLower)
import Data.Foldable (asum)
import Data.IORef
import Data.List (find, foldl', nub, sort, sortOn, isSuffixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import GHC.Clock (getMonotonicTime)
import System.Directory
  (canonicalizePath, doesFileExist, findExecutable, getHomeDirectory,
   listDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), die, exitWith)
import System.FilePath (takeFileName, (</>))
import System.IO
import System.Posix.Process (getProcessID)
import System.Process
import System.Timeout (timeout)
import Text.Printf (printf)

import DemoManifest (hoverRoots, sourceFiles)
import DemoUtil

cacheFile, ignoreFile, outFile :: FilePath
cacheFile = ".hover-cache.json"
ignoreFile = "hover-ignore.txt"
outFile = "demo-hovers.js"

-- ---------------------------------------------------------------------------
-- Terminal-dump traversal — ports of the JS in terminalLinksJs
-- (leksah/src/IDE/Web/Main.hs).  If those regexes change, change these.
--   HDR: (?:Update|Edit|Write|Read)\(([^)]+)\)|^\s*\+\+\+ (?:b/)?(\S+)
--   GUT: ^(\s+)(\d+) ([-+ ])
--   ID:  [A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*
-- (CSI/OSC stripping lives in DemoUtil.visible.)
-- ---------------------------------------------------------------------------

keywords :: S.Set Text
keywords = S.fromList
  [ "case", "class", "data", "default", "deriving", "do", "else", "foreign"
  , "if", "import", "in", "infix", "infixl", "infixr", "instance", "let"
  , "module", "newtype", "of", "then", "type", "where", "qualified"
  , "hiding", "as", "forall", "mdo", "rec", "proc", "family", "role"
  , "pattern", "LANGUAGE", "NOINLINE", "INLINE", "INLINABLE", "UNPACK"
  , "e", "g"  -- the "e.g." in comments
  ]

-- | Leftmost HDR match: headers naming the file a following diff belongs to.
-- The @^\\s*\\+\\+\\+@ alternative can only fire at position 0.
hdrSearch :: Text -> Maybe Text
hdrSearch t = case wordParen t of
  Just tok -> Just tok
  Nothing -> case plusMatch t of
    Just tok -> Just tok
    Nothing
      | T.null t -> Nothing
      | otherwise -> scan (T.tail t)
  where
    scan s
      | T.null s = Nothing
      | Just tok <- wordParen s = Just tok
      | otherwise = scan (T.tail s)

-- (?:Update|Edit|Write|Read)\(([^)]+)\)   anchored at the given position
wordParen :: Text -> Maybe Text
wordParen s = do
  rest <- asum [T.stripPrefix (w <> "(") s | w <- ["Update", "Edit", "Write", "Read"]]
  let (tok, r') = T.break (== ')') rest
  guard (not (T.null tok) && not (T.null r'))
  pure tok

-- ^\s*\+\+\+ (?:b/)?(\S+)
plusMatch :: Text -> Maybe Text
plusMatch t = do
  r1 <- T.stripPrefix "+++ " (T.dropWhile isSpace t)
  let word = T.takeWhile (not . isSpace)
      tok = case T.stripPrefix "b/" r1 of
        Just r2 | not (T.null (word r2)) -> word r2
        _ -> word r1
  guard (not (T.null tok))
  pure tok

-- | ^(\s+)(\d+) ([-+ ])  — returns (line number, mark, match end offset);
-- the end offset is the 0-based file column of the first code character.
gutMatch :: Text -> Maybe (Int, Char, Int)
gutMatch t = do
  let (ind, r1) = T.span isSpace t
  guard (not (T.null ind))
  let (num, r2) = T.span isDigit r1
  guard (not (T.null num))
  (' ', r3) <- T.uncons r2
  (mark, r4) <- T.uncons r3
  guard (mark `elem` ("-+ " :: String))
  _ <- T.stripPrefix "  " r4
  pure (read (T.unpack num), mark, T.length ind + T.length num + 4)

-- | Non-overlapping identifier tokens with codepoint (start, end) columns.
idTokens :: Text -> [(Int, Int, Text)]
idTokens = go 0
  where
    go !i s = case T.uncons s of
      Nothing -> []
      Just (c, r)
        | isStart c ->
            let (tok, rest) = grab s
                n = T.length tok
            in (i, i + n, tok) : go (i + n) rest
        | otherwise -> go (i + 1) r
    isStart c = c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    isCont c = isStart c || (c >= '0' && c <= '9') || c == '\''
    grab s0 =
      let (seg, r0) = T.span isCont s0   -- first char isStart by construction
          (more, r1) = quals r0
      in (seg <> more, r1)
    quals s = case T.uncons s of
      Just ('.', r)
        | Just (c, _) <- T.uncons r, isStart c ->
            let (seg, r') = T.span isCont r
                (more, r'') = quals r'
            in (T.cons '.' seg <> more, r'')
      _ -> ("", s)

-- | The runtime resolves header paths against the workspace file set;
-- here, against the manifest's real paths (same suffix rule).
resolveHeader :: Text -> Maybe FilePath
resolveHeader tok0 =
  let t1 = T.strip tok0
      tok = case () of
        _ | Just r <- T.stripPrefix "./" t1 -> r
          | "a/" `T.isPrefixOf` t1 || "b/" `T.isPrefixOf` t1 -> T.drop 2 t1
          | otherwise -> t1
      matches real =
        let r = T.pack real
        in r == tok || ("/" <> tok) `T.isSuffixOf` r || ("/" <> r) `T.isSuffixOf` tok
  in find matches (map fst sourceFiles)

-- (real file, line0, colStart, colEnd, token, required)
type Target = (FilePath, Int, Int, Int, Text, Bool)

isDump :: FilePath -> Bool
isDump f = case f of
  (a:b:'-':rest) -> isDigit a && isDigit b && ".ans" `isSuffixOf` rest
  _ -> False

-- | Per identifier on a diff/content line of a terminal dump, with the same
-- coordinates the JS sends at runtime.  required=False for hypothetical '+'
-- lines whose text doesn't match the real file.
terminalTargets :: FilePath -> IO [Target]
terminalTargets repo = do
  names <- sort . filter isDump <$> listDirectory "terminals"
  fmap concat . forM names $ \dumpName -> do
    content <- readFileUtf8 ("terminals" </> dumpName)
    let lns = map visible (T.splitOn "\n" content)
    (_, acc) <- foldM (step dumpName) (Nothing, []) lns
    pure (reverse acc)
  where
    step :: FilePath -> (Maybe (FilePath, [Text]), [Target]) -> Text
         -> IO (Maybe (FilePath, [Text]), [Target])
    step dumpName (gov, acc) t
      | Just tok0 <- hdrSearch t = do
          let tok = T.strip tok0
          case resolveHeader tok of
            Just real -> do
              realLines <- T.splitOn "\n" <$> readFileUtf8 (repo </> real)
              pure (Just (real, realLines), acc)
            Nothing -> do
              printf "  note: %s: header '%s' not in the demo manifest — no hovers for its block (unresolvable at runtime too)\n"
                     dumpName (T.unpack tok)
              pure (Nothing, acc)
      | Just (line1, mark, end) <- gutMatch t
      , Just (real, realLines) <- gov = do
          let code = T.drop end t
              inFile = line1 - 1 >= 0 && line1 - 1 < length realLines
              realLine = if inFile then realLines !! (line1 - 1) else ""
          acc' <- foldM
            (\a (s, e, tok) ->
              if S.member tok keywords then pure a else do
                let match = inFile && T.take (e - s) (T.drop s realLine) == tok
                when (not match && mark /= '+') $
                  die (printf "%s: line %d col %d: '%s' does not match the real %s — stale fixture/capture"
                       dumpName line1 s (T.unpack tok) real)
                pure ((real, line1 - 1, s, e, tok, match) : a))
            acc (idTokens code)
          pure (gov, acc')
      | otherwise = pure (gov, acc)

editorTargets :: FilePath -> IO [Target]
editorTargets repo =
  fmap concat . forM hoverRoots $ \(real, _) -> do
    text <- readFileUtf8 (repo </> real)
    pure [ (real, i, s, e, tok, False)
         | (i, line) <- zip [0 ..] (T.splitOn "\n" text)
         , (s, e, tok) <- idTokens line
         , not (S.member tok keywords) ]

-- ---------------------------------------------------------------------------
-- Minimal LSP client over stdio
-- ---------------------------------------------------------------------------

data HoverTimeout = HoverTimeout deriving Show
instance Exception HoverTimeout
data HoverEOF = HoverEOF deriving Show
instance Exception HoverEOF

-- | Buffered pipe reader whose reads honour a deadline (a blocked read with
-- no deadline could hang the whole run while HLS typechecks silently).
data Reader = Reader Handle (IORef BS.ByteString)

fillR :: Reader -> Double -> IO ()
fillR (Reader h buf) deadline = do
  now <- getMonotonicTime
  let remaining = max 0 (deadline - now)
  mchunk <- timeout (ceiling (remaining * 1e6)) (BS.hGetSome h 65536)
  case mchunk of
    Nothing -> throwIO HoverTimeout
    Just c
      | BS.null c -> throwIO HoverEOF
      | otherwise -> modifyIORef' buf (<> c)

readLineR :: Reader -> Double -> IO BS.ByteString
readLineR r@(Reader _ buf) deadline = do
  b <- readIORef buf
  case BC.elemIndex '\n' b of
    Just i -> writeIORef buf (BS.drop (i + 1) b) >> pure (BS.take (i + 1) b)
    Nothing -> fillR r deadline >> readLineR r deadline

readNR :: Reader -> Int -> Double -> IO BS.ByteString
readNR r@(Reader _ buf) n deadline = do
  b <- readIORef buf
  if BS.length b >= n
    then writeIORef buf (BS.drop n b) >> pure (BS.take n b)
    else fillR r deadline >> readNR r n deadline

data Lsp = Lsp
  { lspRoot :: FilePath
  , lspIn :: Handle
  , lspOut :: Reader
  , lspPH :: ProcessHandle
  , lspNext :: IORef Int
  , lspTimedOut :: IORef Bool
  , lspTrace :: Bool
  }

sendMsg :: Lsp -> [(Text, Value)] -> IO ()
sendMsg lsp fields = do
  let body = TE.encodeUtf8 (TL.toStrict (dumps Nothing (JObj (("jsonrpc", JStr "2.0") : fields))))
  BS.hPut (lspIn lsp)
    (BC.pack ("Content-Length: " ++ show (BS.length body) ++ "\r\n\r\n") <> body)
  hFlush (lspIn lsp)

notify :: Lsp -> Text -> Value -> IO ()
notify lsp method params = sendMsg lsp [("method", JStr method), ("params", params)]

readMsg :: Lsp -> Double -> IO Value
readMsg lsp deadline = do
  n <- readHeaders Nothing
  body <- readNR (lspOut lsp) n deadline
  either (\e -> die ("HLS sent unparseable JSON: " ++ e)) pure
         (parseJson (TE.decodeUtf8 body))
  where
    bstrip = BC.dropWhile isSpace . fst . BC.spanEnd isSpace
    readHeaders mlen = do
      raw <- readLineR (lspOut lsp) deadline
      let l = bstrip raw
      if BS.null l
        then maybe (die "HLS header block had no Content-Length") pure mlen
        else do
          let (k, v) = BC.break (== ':') l
          if BC.map toLower k == "content-length"
            then readHeaders (Just (read (BC.unpack (bstrip (BS.drop 1 v)))))
            else readHeaders mlen

-- | Answer server->client requests with the boring defaults so HLS doesn't
-- stall; other notifications (diagnostics, progress, logs) are dropped.
handleMsg :: Lsp -> Value -> IO ()
handleMsg lsp msg = do
  when (lspTrace lsp) $ case objLookup "method" msg of
    Just (JStr m) -> printf "    <- %s\n" (T.unpack m)
    _ -> pure ()
  case (objLookup "method" msg, objLookup "id" msg) of
    (Just (JStr m), Just mid) -> do
      let result
            | m == "workspace/configuration" =
                case objLookup "params" msg >>= objLookup "items" of
                  Just (JArr xs) -> JArr (map (const JNull) xs)
                  _ -> JArr []
            | m == "workspace/applyEdit" = JObj [("applied", JBool False)]
            | otherwise = JNull
      sendMsg lsp [("id", mid), ("result", result)]
    _ -> pure ()

request :: Lsp -> Text -> Value -> Int -> IO (Maybe Value)
request lsp method params tmo = do
  rid <- atomicModifyIORef' (lspNext lsp) (\n -> (n + 1, n + 1))
  sendMsg lsp [("id", JInt (fromIntegral rid)), ("method", JStr method), ("params", params)]
  now <- getMonotonicTime
  let deadline = now + fromIntegral tmo
  writeIORef (lspTimedOut lsp) False
  let loop = do
        r <- try (readMsg lsp deadline)
        case r of
          Left HoverTimeout -> do
            writeIORef (lspTimedOut lsp) True
            if method == "initialize"
              then die (printf "HLS at %s: initialize timed out after %ds" (lspRoot lsp) tmo)
              else do
                printf "  warning: HLS at %s: %s timed out after %ds\n"
                       (lspRoot lsp) (T.unpack method) tmo
                pure Nothing
          Right msg
            | objLookup "id" msg == Just (JInt (fromIntegral rid))
            , isJust (objLookup "result" msg) || isJust (objLookup "error" msg)
              -> pure (objLookup "result" msg)
            | otherwise -> handleMsg lsp msg >> loop
  loop `catch` \HoverEOF ->
    die (printf "HLS at %s died (method %s)" (lspRoot lsp) (T.unpack method))

newLsp :: FilePath -> IO Lsp
newLsp root = do
  exe <- findExecutable "haskell-language-server" >>= maybe
           (die "haskell-language-server not on PATH — run from the dev shell") pure
  errlog <- fromMaybe "/dev/null" <$> lookupEnv "GEN_HOVERS_STDERR"
  errH <- openFile errlog AppendMode
  trace <- isJust <$> lookupEnv "GEN_HOVERS_TRACE"
  (Just hin, Just hout, _, ph) <- createProcess (proc exe ["--lsp"])
    { cwd = Just root, std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle errH }
  hSetBinaryMode hin True
  hSetBinaryMode hout True
  buf <- newIORef BS.empty
  nid <- newIORef 0
  to <- newIORef False
  let lsp = Lsp root hin (Reader hout buf) ph nid to trace
  pid <- getProcessID
  let uri = "file://" <> T.pack root
  r <- request lsp "initialize" (JObj
        [ ("processId", JInt (fromIntegral pid))
        , ("rootUri", JStr uri)
        , ("capabilities", JObj [("window", JObj [("workDoneProgress", JBool True)])])
        , ("workspaceFolders", JArr [JObj [("uri", JStr uri), ("name", JStr (T.pack (takeFileName root)))]])
        ]) 120
  case r of
    Just v | v /= JNull -> pure ()
    _ -> die (printf "HLS at %s: no initialize response" root)
  notify lsp "initialized" (JObj [])
  pure lsp

didOpen :: Lsp -> FilePath -> IO ()
didOpen lsp path = do
  text <- readFileUtf8 path
  notify lsp "textDocument/didOpen" (JObj [("textDocument", JObj
    [ ("uri", JStr ("file://" <> T.pack path))
    , ("languageId", JStr "haskell")
    , ("version", JInt 1)
    , ("text", JStr text)
    ])])

hoverReq :: Lsp -> FilePath -> Int -> Int -> Int -> IO (Maybe Value)
hoverReq lsp path line col tmo = request lsp "textDocument/hover" (JObj
  [ ("textDocument", JObj [("uri", JStr ("file://" <> T.pack path))])
  , ("position", JObj [("line", JInt (fromIntegral line)), ("character", JInt (fromIntegral col))])
  ]) tmo

closeLsp :: Lsp -> IO ()
closeLsp lsp = do
  hClose (lspIn lsp) `catch` \(_ :: SomeException) -> pure ()
  terminateProcess (lspPH lsp) `catch` \(_ :: SomeException) -> pure ()

-- | IDE.LSP.extractHover's logic: contents may be MarkupContent
-- {kind,value}, a MarkedString (string or {language,value}), or a list.
renderHover :: (Text -> Text) -> Maybe Value -> Maybe Text
renderHover scrub mres = do
  res <- mres
  guard (truthy res)
  c <- objLookup "contents" res
  text <- case c of
    JStr s -> Just s
    JObj _ -> Just (strVal (objLookup "value" c))
    JArr xs -> Just (T.intercalate "\n\n"
                     (map (\case JStr s -> s; o -> strVal (objLookup "value" o)) xs))
    _ -> Nothing
  let t = scrub (T.strip text)
  guard (not (T.null t))
  pure t
  where
    truthy JNull = False
    truthy (JObj []) = False
    truthy (JArr []) = False
    truthy (JBool b) = b
    truthy (JStr s) = not (T.null s)
    truthy _ = True
    strVal (Just (JStr s)) = s
    strVal _ = ""

-- | The demo is published: hover markdown must not leak local absolute paths
-- ("*Defined at /Users/…*", "file://…" links).  Rewrite paths of packed
-- files to their demo names, everything else under the repo to /demo, and
-- any remaining home-dir prefix to ~.
scrubPaths :: FilePath -> FilePath -> Text -> Text
scrubPaths repo home t0 =
  let t1 = foldl' (\t (real, demo) -> T.replace (T.pack (repo </> real)) (T.pack demo) t)
                  t0 sourceFiles
      t2 = T.replace (T.pack repo) "/demo" t1
  in T.replace (T.pack home) "~" t2

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  here <- doesFileExist "index.html"
  unless here $ die "gen-demo-hovers.hs: run from docs/website/try"
  repo <- canonicalizePath "../../.."
  home <- getHomeDirectory
  let scrub = scrubPaths repo home

  haveIgnore <- doesFileExist ignoreFile
  ignore <- if haveIgnore
    then (\txt -> S.fromList [ T.strip l | l <- T.splitOn "\n" txt
                             , not (T.null (T.strip l)), not ("#" `T.isPrefixOf` l) ])
         <$> readFileUtf8 ignoreFile
    else pure S.empty

  term <- terminalTargets repo
  edit <- editorTargets repo
  -- Dedupe by position, letting terminal targets carry their required flag.
  let byPos = foldl'
        (\m t@(f, l, s, _, _, req) ->
          M.insertWith (\(f', l', s', e', tok', r') (_, _, _, _, _, oldR) ->
                          (f', l', s', e', tok', r' || oldR))
                       (f, l, s) t m)
        M.empty (edit ++ term)
      -- Required (terminal-derived) positions first, so a stalled cradle for
      -- an editor-only file can't block the tooltips the demo terminal needs.
      targets = sortOn (\t@(_, _, _, _, _, req) -> (not req, t)) (M.elems byPos)
  printf "%d hover positions (%d from terminal dumps, %d required)\n"
         (length targets) (length term) (length (filter (\(_,_,_,_,_,r) -> r) targets))

  haveCache <- doesFileExist cacheFile
  cache0 <- if haveCache
    then do
      -- scrub on load too: older caches hold pre-scrub values.
      txt <- readFileUtf8 cacheFile
      case parseJson txt of
        Right (JObj kvs) -> pure (M.fromList
          [ (k, case v of JStr s -> Just (scrub s); _ -> Nothing) | (k, v) <- kvs ])
        _ -> die (cacheFile ++ ": unparseable — delete it and re-run")
    else pure M.empty
  -- Cache-buster per file: crc32+length is plenty (not security-relevant).
  hashes <- M.fromList <$> mapM
    (\real -> do
      bytes <- BS.readFile (repo </> real)
      pure (real, printf "%08x%x" (crc32 bytes) (BS.length bytes) :: String))
    (nub [f | (f, _, _, _, _, _) <- targets])

  cacheRef <- newIORef cache0
  serversRef <- newIORef (M.empty :: M.Map FilePath Lsp)
  openedRef <- newIORef (S.empty :: S.Set FilePath)
  deadRef <- newIORef (S.empty :: S.Set FilePath)    -- HLS session never became ready
  -- Null hovers are cached too (pragmas, keywords, blank spots — no point
  -- re-asking), but ONLY for files that produced at least one real hover
  -- this run: a broken session (bad cradle, stalled typecheck) answers null
  -- for everything, and caching those would poison every later run.
  okRef <- newIORef (S.empty :: S.Set FilePath)
  pendingRef <- newIORef ([] :: [(FilePath, Text)])  -- held until the file proves alive
  hoversRef <- newIORef ([] :: [(Text, [(Text, [Value])])])
  missesRef <- newIORef ([] :: [String])
  skippedRef <- newIORef (0 :: Int)

  let getHover :: FilePath -> Int -> Int -> IO (Maybe Text)
      getHover real line col = do
        dead <- readIORef deadRef
        if S.member real dead then pure Nothing else do
          let ck = T.pack (printf "%s:%d:%d" (hashes M.! real) line col :: String)
          cached <- M.lookup ck <$> readIORef cacheRef
          case cached of
            Just v -> pure v
            Nothing -> do
              let rootRel = fromMaybe "." (lookup real hoverRoots)
                  root = if rootRel == "." then repo else repo </> rootRel
              servers <- readIORef serversRef
              lsp <- case M.lookup root servers of
                Just l -> pure l
                Nothing -> do
                  printf "starting haskell-language-server in %s …\n" root
                  l <- newLsp root
                  modifyIORef' serversRef (M.insert root l)
                  pure l
              let path = repo </> real
              opened <- readIORef openedRef
              md <- if S.member real opened
                then renderHover scrub <$> hoverReq lsp path line col 120
                else do
                  -- ghcide computes lazily: nothing typechecks until a request
                  -- forces it, so the file's FIRST hover doubles as the
                  -- readiness wait (it blocks until the typecheck finishes) —
                  -- give it the session-load budget.  Only a TIMEOUT means the
                  -- session is broken/stalled (skip the file, uncached, so a
                  -- rerun retries); an ANSWERED null is a legitimate "nothing
                  -- hoverable here" — a healthy lazy ghcide session publishes
                  -- no diagnostics either, so absence of diagnostics proves
                  -- nothing.
                  didOpen lsp path
                  modifyIORef' openedRef (S.insert real)
                  printf "first hover of %s (forces its typecheck) …\n" real
                  t0 <- getMonotonicTime
                  r <- hoverReq lsp path line col 900
                  t1 <- getMonotonicTime
                  printf "  answered in %.0fs\n" (t1 - t0)
                  pure (renderHover scrub r)
              timedOut <- readIORef (lspTimedOut lsp)
              if isNothing md && timedOut
                then do
                  printf "  warning: hover timed out — skipping the rest of %s\n" real
                  modifyIORef' deadRef (S.insert real)
                  pure Nothing
                else do
                  case md of
                    Nothing -> modifyIORef' pendingRef ((real, ck) :)
                    Just m -> do
                      modifyIORef' cacheRef (M.insert ck (Just m))
                      modifyIORef' okRef (S.insert real)
                  pure md

      addHover demo lineKey sp = modifyIORef' hoversRef (upsert demo)
        where
          upsert d [] = [(d, [(lineKey, [sp])])]
          upsert d ((k, ls) : rest)
            | k == d = (k, upLine ls) : rest
            | otherwise = (k, ls) : upsert d rest
          upLine [] = [(lineKey, [sp])]
          upLine ((lk, ss) : rest)
            | lk == lineKey = (lk, ss ++ [sp]) : rest
            | otherwise = (lk, ss) : upLine rest

  let processAll = forM_ targets $ \(real, line, s, e, tok, required) -> do
        md <- getHover real line s
        case md of
          Nothing
            | required && not (S.member tok ignore) ->
                modifyIORef' missesRef (printf "%s:%d:%d %s" real (line + 1) s (T.unpack tok) :)
            | otherwise -> modifyIORef' skippedRef (+ 1)
          Just m -> do
            let demo = fromMaybe (error ("not in manifest: " ++ real)) (lookup real sourceFiles)
            addHover (T.pack demo) (T.pack (show line)) (JArr [JInt (fromIntegral s), JInt (fromIntegral e), JStr m])

      cleanup = do
        pend <- readIORef pendingRef
        ok <- readIORef okRef
        forM_ pend $ \(real, ck) ->
          when (S.member real ok) $ modifyIORef' cacheRef (M.insert ck Nothing)
        cache <- readIORef cacheRef
        writeFileUtf8 cacheFile (TL.toStrict (dumps Nothing
          (JObj [(k, maybe JNull JStr v) | (k, v) <- M.toList cache])))
        servers <- readIORef serversRef
        mapM_ closeLsp (M.elems servers)

  processAll `finally` cleanup

  misses <- reverse <$> readIORef missesRef
  unless (null misses) $ do
    putStrLn "\nTOOLTIPS MISSING for terminal identifiers (add to hover-ignore.txt only if genuinely unhoverable):"
    mapM_ (putStrLn . ("  " ++)) misses
    exitWith (ExitFailure 1)

  hovers <- readIORef hoversRef
  skipped <- readIORef skippedRef
  let n = sum [length ss | (_, ls) <- hovers, (_, ss) <- ls]
  writeFileUtf8 outFile $
    "// Generated by gen-demo-hovers.hs — real haskell-language-server hover responses.\n\
    \window.leksahDemoHovers = "
    <> TL.toStrict (dumps (Just 1)
         (JObj [(d, JObj [(lk, JArr ss) | (lk, ss) <- ls]) | (d, ls) <- hovers]))
    <> ";\n"
  printf "wrote %s (%d spans across %d files; %d spans without hover skipped)\n"
         outFile n (length hovers) skipped
