{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A tmux control-mode (@tmux -C@) client: the foundation for rendering
-- each tmux pane as its own xterm.js instance (iTerm2-style \"-CC\" support).
--
-- One process speaks tmux's control protocol over plain pipes:
--
--   * stdin:  one tmux command per line;
--   * stdout: @%begin \<time\> \<num\> \<flags\>@ … body … @%end@\/@%error@
--     reply blocks (FIFO — the n-th completed block answers the n-th command
--     we sent), interleaved with asynchronous notifications
--     (@%output@, @%layout-change@, @%window-add@, …).
--
-- Design notes (measured against tmux 3.6a; see docs/tmux-control-mode.md):
--
--   * a single reader thread parses stdout and pushes 'TmuxEvent's into an
--     ordered 'Chan' — the same one-ordered-channel discipline as
--     @IDE.Utils.Tool@, but no stderr-sentinel handshake is needed because
--     control mode multiplexes everything onto one stream by construction;
--   * command submission holds a lock around (enqueue reply-var, write line)
--     so FIFO correlation cannot interleave;
--   * an attach emits one implicit reply block (flags=0) before any command:
--     reply blocks with an empty pending queue are discarded;
--   * @%output@ data has control bytes and backslash octal-escaped
--     (@\\ooo@); modern tmux passes valid UTF-8 raw, older tmux escapes
--     @>=0x7f@ too — 'unescapeOctal' handles both.  Output stays a
--     'ByteString': xterm.js does its own stateful UTF-8 decoding, so
--     multi-byte sequences split across @%output@ lines are fine;
--   * killing a pane emits NO pane-close notification, only
--     @%layout-change@ — pane lifetime is derived by diffing 'layoutPanes'
--     of successive layouts;
--   * EOF fails all pending commands and emits 'EvExit', so callers never
--     deadlock on a dead client.
module IDE.Web.TmuxCC
  ( -- * Types
    CC
  , PaneId
  , WindowId
  , SessionId
  , TmuxEvent(..)
  , Layout(..)
  , LayoutCell(..)
    -- * Client lifecycle
  , startCC
  , startCCWith
  , stopCC
  , ccAlive
  , ccStderrText
    -- * Commands and events
  , ccCommand
  , ccSend
  , ccCommandTagged
  , ccEvents
  , ccEventsBatch
  , coalesceOutputs
  , ccSendBytes
  , ccSendBytesBig
  , ccResize
  , ccResizeWindow
  , ccClearWindowSize
    -- * Pure helpers (exposed for tests)
  , unescapeOctal
  , unescapeOctalBS
  , parseLayout
  , layoutPanes
  , layoutDividers
  , parseEventLine
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
       (TChan, atomically, newTChanIO, readTChan, tryReadTChan, writeTChan)
import Control.Exception (SomeException, catch, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isDigit)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Word (Word8)
import Numeric (showHex)
import System.IO
       (BufferMode(LineBuffering, NoBuffering), Handle, hClose,
        hSetBuffering, hSetBinaryMode)
import qualified System.IO as IO
import System.Process
       (CreateProcess(..), ProcessHandle, StdStream(CreatePipe),
        createProcess, getProcessExitCode, proc, terminateProcess,
        waitForProcess)
import System.Timeout (timeout)

-- | tmux ids, kept verbatim (@\"%3\"@, @\"\@1\"@, @\"$0\"@) so they can be
-- used directly as @-t@ targets.
type PaneId    = Text
type WindowId  = Text
type SessionId = Text

-- | Asynchronous notifications from the server (plus 'EvExit' for EOF).
data TmuxEvent
  = EvOutput PaneId BS.ByteString
      -- ^ pane output, unescaped raw bytes (feed straight to xterm.write)
  | EvExtendedOutput PaneId BS.ByteString
      -- ^ @%extended-output@ (flow-control mode); age dropped
  | EvLayoutChange WindowId (Maybe Layout) (Maybe Layout) Text
      -- ^ window, layout, visible layout, window flags (e.g. @*@)
  | EvWindowAdd WindowId
  | EvWindowClose WindowId
  | EvUnlinkedWindowAdd WindowId
  | EvUnlinkedWindowClose WindowId
  | EvWindowRenamed WindowId Text
  | EvWindowPaneChanged WindowId PaneId
  | EvSessionChanged SessionId Text
  | EvSessionRenamed Text
  | EvSessionWindowChanged SessionId WindowId
  | EvSessionsChanged
  | EvPaneModeChanged PaneId
  | EvPause PaneId
  | EvContinue PaneId
  | EvReply Text (Either Text [Text])
      -- ^ the reply to a 'ccCommandTagged' command, delivered IN STREAM
      -- ORDER relative to output — the property that makes a capture-based
      -- \"jump ahead\" race-free (any @%output@ before the reply is state
      -- the capture already includes; anything after applies on top)
  | EvExit (Maybe Text)
      -- ^ stream ended (reason if tmux gave one); the client is dead
  | EvUnknown Text
      -- ^ an unrecognised @%@-line, kept verbatim (forward compatibility)
  deriving (Eq, Show)

-- | A parsed layout tree node: geometry plus content.
data Layout = Layout
  { lW :: Int, lH :: Int, lX :: Int, lY :: Int
  , lCell :: LayoutCell
  } deriving (Eq, Show)

data LayoutCell
  = LPane PaneId              -- ^ leaf (id includes the @%@)
  | LRow [Layout]             -- ^ @{…}@ left-to-right split
  | LCol [Layout]             -- ^ @[…]@ top-to-bottom split
  deriving (Eq, Show)

type ReplyVar = MVar (Either Text [Text])

-- | How a command's reply is delivered: to a waiting caller ('PendingVar',
-- the 'ccCommand' path), or into the event channel at its stream position
-- ('PendingTag' → 'EvReply', the 'ccCommandTagged' path).
data Pending = PendingVar ReplyVar | PendingTag Text

data CC = CC
  { ccIn      :: Handle
  , ccProc    :: ProcessHandle
  , ccChan    :: TChan TmuxEvent
  , ccPending :: MVar [Pending]    -- ^ FIFO queue (append on send, pop on reply)
  , ccLock    :: MVar ()           -- ^ held around (enqueue, write command)
  , ccClosed  :: IORef Bool
  , ccErr     :: IORef Text        -- ^ accumulated child stderr (ssh/tmux errors)
  }

-- | The child's stderr captured so far — the ssh/tmux diagnostics that used to
-- vanish into leksah's own log.  Read on 'EvExit' to show the user WHY a
-- (remote) connection dropped instead of the tab silently disappearing.
ccStderrText :: CC -> IO Text
ccStderrText = readIORef . ccErr

-- | Swallowed-exception / protocol-anomaly log: this module deliberately
-- keeps the client alive through errors, but every ignored condition is at
-- least reported here (stderr — the leksah-nix.sh window) so failures are
-- diagnosable instead of silent.
ccWarn :: String -> IO ()
ccWarn msg = IO.hPutStrLn IO.stderr ("TmuxCC: " <> msg)
    `catch` \(_ :: SomeException) -> return ()

-- | Spawn @tmux -C \<socketArgs\> \<attachArgs\>@,
-- e.g. @startCC [\"-L\",\"leksah\"] [\"attach-session\",\"-t\",\"$3\"]@ or
-- @startCC [\"-L\",\"leksah\"] [\"new-session\",\"-s\",\"x\",\"-x\",\"200\",\"-y\",\"50\"]@.
startCC :: [String] -> [String] -> IO CC
startCC socketArgs attachArgs =
    startCCWith ("tmux" : "-C" : socketArgs ++ attachArgs)

-- | Spawn an arbitrary argv that speaks tmux control mode on its pipes — the
-- remote case: @startCCWith [\"ssh\", host, \"tmux\", \"-C\", \"new-session\",
-- \"-A\", \"-s\", \"leksah\"]@.  No PTY is involved (ssh without @-t@), so the
-- protocol stream is byte-identical to the local case.
startCCWith :: [String] -> IO CC
startCCWith argv = do
    let (cmd, args) = case argv of
            (c : rest) -> (c, rest)
            []         -> ("tmux", ["-C"])   -- unreachable; keeps this total
    -- stderr is CAPTURED (not Inherit): ssh/tmux write connection failures
    -- there ("Permission denied", "Could not resolve hostname", host-key
    -- mismatches, …), and we surface those to the user on exit rather than
    -- letting the tab vanish silently.
    (Just hin, Just hout, Just herr, ph) <- createProcess
        (proc cmd args)
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    hSetBuffering hin LineBuffering
    hSetBinaryMode hout True
    hSetBuffering hout NoBuffering
    errRef <- newIORef T.empty
    cc <- CC hin ph <$> newTChanIO <*> newMVar [] <*> newMVar () <*> newIORef False
                    <*> pure errRef
    -- Drain stderr into the buffer (kept bounded so a chatty child can't grow it
    -- without limit); lenient decode since it's human text, not the protocol.
    _ <- forkIO $ (do
            hSetBinaryMode herr True
            let pump = do
                    eof <- IO.hIsEOF herr
                    unless eof $ do
                        chunk <- BS.hGetSome herr 4096
                        unless (BS.null chunk) $ do
                            modifyIORef' errRef $ \acc ->
                                T.takeEnd 8192 (acc <> TE.decodeUtf8With TEE.lenientDecode chunk)
                            pump
            pump) `catch` \(_ :: SomeException) -> return ()
    _ <- forkIO $ reader cc hout
    return cc

-- | The reader thread: one ordered pass over stdout.
reader :: CC -> Handle -> IO ()
reader cc hout = loop `catch` \(e :: SomeException) -> do
    ccWarn ("reader died: " <> show e)
    finish Nothing
  where
    loop = do
        eof <- IO.hIsEOF hout
        if eof then finish Nothing else do
            raw <- getRaw
            dispatchRaw raw
            loop
    getRaw = stripCR <$> BSC.hGetLine hout
    getLine' = TE.decodeUtf8With TEE.lenientDecode <$> getRaw
    stripCR b = if not (BS.null b) && BS.last b == 13 then BS.init b else b

    -- %output payloads are RAW BYTES and must never round-trip through Text:
    -- tmux chunks pane output at arbitrary byte boundaries and does NOT
    -- octal-escape a UTF-8 sequence split across two %output lines (measured:
    -- under bursts ~half the lines end mid-character).  A per-line lenient
    -- decode turns both halves into U+FFFD — seen as '�' cells accumulating
    -- during scroll storms.  xterm's own UTF-8 decode is stateful across
    -- writes, so handing it the raw bytes reassembles the character.
    dispatchRaw raw
      | Just rest <- BS.stripPrefix "%output " raw =
            let (p, dat) = BSC.break (== ' ') rest
            in emit (EvOutput (TE.decodeUtf8With TEE.lenientDecode p)
                              (unescapeOctalBS (BS.drop 1 dat)))
      -- %extended-output is what %output becomes once the pause-after flag
      -- is set; the extra field is just the buffered age — normalise to
      -- EvOutput so consumers have one output event.
      | Just rest <- BS.stripPrefix "%extended-output " raw =
            let (p, dat0) = BSC.break (== ' ') rest
                dat = snd (BS.breakSubstring " : " dat0)
            in emit (EvOutput (TE.decodeUtf8With TEE.lenientDecode p)
                              (unescapeOctalBS (BS.drop 3 dat)))
      | otherwise = dispatch (TE.decodeUtf8With TEE.lenientDecode raw)

    dispatch line
      | Just hdr <- T.stripPrefix "%begin " line = collectBlock (clientBlock hdr) []
      | Just rest <- T.stripPrefix "%exit" line = do
            emit (EvExit (mbReason rest))
            finish (Just ())
      | otherwise = emit (parseEventLine line)
    mbReason r = let t = T.strip r in if T.null t then Nothing else Just t

    -- The third %begin field is a flags word; bit 1 = the block answers a
    -- command from THIS client.  The implicit attach block has flags 0 and
    -- must not consume a pending reply slot (measured: it can arrive after
    -- our first command was already submitted).
    clientBlock hdr = case T.words hdr of
        (_t : _n : fl : _) -> maybe False odd (readIntT fl)
        _                  -> False
    readIntT t = case reads (T.unpack t) of
        [(n :: Int, "")] -> Just n
        _                -> Nothing

    -- Collect body lines until the matching %end/%error, then answer the
    -- oldest pending command (server-initiated blocks are discarded).
    collectBlock forUs acc = do
        eof <- IO.hIsEOF hout
        if eof then finish Nothing else do
            line <- getLine'
            case () of
              _ | Just _ <- T.stripPrefix "%end " line   -> deliver forUs (Right (reverse acc))
                | Just _ <- T.stripPrefix "%error " line -> deliver forUs (Left (T.unlines (reverse acc)))
                | otherwise -> collectBlock forUs (line : acc)
    deliver forUs result
      | not forUs = return ()
      | otherwise = do
        mv <- modifyMVar (ccPending cc) $ \case
            []       -> return ([], Nothing)
            (v:rest) -> return (rest, Just v)
        case mv of
          Just (PendingVar v)  -> void $ tryPutMVar v result
          Just (PendingTag t)  -> emit (EvReply t result)
          -- A client-flagged reply with no pending command means the
          -- correlation is off — later replies would answer the wrong
          -- callers.  Loud, because it should never happen.
          Nothing -> ccWarn "reply block with no pending command (correlation lost?)"

    emit = atomically . writeTChan (ccChan cc)

    finish exited = do
        writeIORef (ccClosed cc) True
        vs <- modifyMVar (ccPending cc) $ \vs -> return ([], vs)
        forM_ vs $ \case
            PendingVar v -> void $ tryPutMVar v (Left "tmux control client exited")
            PendingTag t -> emit (EvReply t (Left "tmux control client exited"))
        case exited of
          Just () -> return ()   -- EvExit already emitted (with reason)
          Nothing -> emit (EvExit Nothing)

-- | Parse one notification line into a 'TmuxEvent'.  Exposed for tests.
parseEventLine :: Text -> TmuxEvent
parseEventLine line = case T.words line of
    ("%output" : p : _) -> EvOutput p (unescapeOctal (dropPrefix ("%output " <> p <> " ")))
    ("%extended-output" : p : _) ->
        -- %extended-output %P age ... : data
        let rest = dropPrefix ("%extended-output " <> p <> " ")
            dat  = snd (T.breakOn " : " rest)
        in EvExtendedOutput p (unescapeOctal (T.drop 3 dat))
    ("%layout-change" : w : rest) ->
        let (l, vl, fl) = case rest of
              (a : b : c : _) -> (parseLayout a, parseLayout b, c)
              (a : b : _)     -> (parseLayout a, parseLayout b, "")
              (a : _)         -> (parseLayout a, Nothing, "")
              _               -> (Nothing, Nothing, "")
        in EvLayoutChange w l vl fl
    ["%window-add", w]             -> EvWindowAdd w
    ["%window-close", w]           -> EvWindowClose w
    ["%unlinked-window-add", w]    -> EvUnlinkedWindowAdd w
    ["%unlinked-window-close", w]  -> EvUnlinkedWindowClose w
    ("%window-renamed" : w : _)    -> EvWindowRenamed w (dropPrefix ("%window-renamed " <> w <> " "))
    ["%window-pane-changed", w, p] -> EvWindowPaneChanged w p
    ("%session-changed" : s : _)   -> EvSessionChanged s (dropPrefix ("%session-changed " <> s <> " "))
    ("%session-renamed" : _)       -> EvSessionRenamed (dropPrefix "%session-renamed ")
    ["%session-window-changed", s, w] -> EvSessionWindowChanged s w
    ["%sessions-changed"]          -> EvSessionsChanged
    ["%pane-mode-changed", p]      -> EvPaneModeChanged p
    ["%pause", p]                  -> EvPause p
    ["%continue", p]               -> EvContinue p
    _                              -> EvUnknown line
  where
    dropPrefix pre = maybe line id (T.stripPrefix pre line)

-- | Decode @%output@ escaping: @\\ooo@ (exactly three octal digits) is a
-- byte; everything else passes through (modern tmux sends UTF-8 raw — and
-- possibly INCOMPLETE at the end of a chunk, so this must stay byte-exact;
-- see 'reader').
unescapeOctalBS :: BS.ByteString -> BS.ByteString
unescapeOctalBS = BS.pack . go . BS.unpack
  where
    go :: [Word8] -> [Word8]
    go (92 : a : b : c : rest)          -- 92 = '\\'
      | octal a && octal b && octal c =
          (dv a * 64 + dv b * 8 + dv c) : go rest
    go (x : rest) = x : go rest
    go [] = []
    octal w = w >= 48 && w <= 55
    dv w = w - 48

-- | Text-input variant of 'unescapeOctalBS' — kept for tests and for
-- 'parseEventLine' (whose Text interface cannot represent a split UTF-8
-- sequence; the live reader parses @%output@ from the raw bytes instead).
unescapeOctal :: Text -> BS.ByteString
unescapeOctal = unescapeOctalBS . TE.encodeUtf8

-- | Parse a tmux layout string (checksum-prefixed).  Returns 'Nothing' on
-- malformed input rather than erroring — a notification must never kill the
-- reader.
parseLayout :: Text -> Maybe Layout
parseLayout t = case T.breakOn "," t of
    (_csum, rest) | not (T.null rest) ->
        case node (T.unpack (T.drop 1 rest)) of
          Just (l, "") -> Just l
          _            -> Nothing
    _ -> Nothing
  where
    node :: String -> Maybe (Layout, String)
    node s0 = do
        (w, s1) <- num s0
        s2 <- expect 'x' s1
        (h, s3) <- num s2
        s4 <- expect ',' s3
        (x, s5) <- num s4
        s6 <- expect ',' s5
        (y, s7) <- num s6
        case s7 of
          (',' : d : _) | isDigit d -> do
              (pid, s8) <- num (drop 1 s7)
              Just (Layout w h x y (LPane (T.pack ('%' : show pid))), s8)
          ('{' : s8) -> do
              (kids, s9) <- nodes s8 '}'
              Just (Layout w h x y (LRow kids), s9)
          ('[' : s8) -> do
              (kids, s9) <- nodes s8 ']'
              Just (Layout w h x y (LCol kids), s9)
          _ -> Nothing
    nodes s close = do
        (n1, s1) <- node s
        case s1 of
          (c : s2) | c == close -> Just ([n1], s2)
                   | c == ','   -> do (rest, s3) <- nodes s2 close
                                      Just (n1 : rest, s3)
          _ -> Nothing
    num s = case span isDigit s of
        ("", _) -> Nothing
        (ds, r) -> Just (read ds :: Int, r)
    expect c (x : xs) | x == c = Just xs
    expect _ _ = Nothing

-- | The leaves of a layout: @(pane id, x, y, w, h)@ in cells.
layoutPanes :: Layout -> [(PaneId, Int, Int, Int, Int)]
layoutPanes l = case lCell l of
    LPane p  -> [(p, lX l, lY l, lW l, lH l)]
    LRow ks  -> concatMap layoutPanes ks
    LCol ks  -> concatMap layoutPanes ks

-- | The separator cells BETWEEN sibling panes — one entry per divider,
-- spanning the split's full extent: @(vertical, x, y, w, h, target)@ in
-- cells (a vertical divider is 1 cell wide, a horizontal one 1 cell tall).
-- tmux draws its line characters in these cells for regular clients; a
-- front end placing panes at exact cell rectangles gets them as blank
-- gutters and can draw a crisp divider instead (as iTerm2 does).
--
-- @target@ is the pane to @resize-pane@ when the divider is dragged: a leaf
-- of the BEFORE-sibling whose right (resp. bottom) edge lies on the divider,
-- so growing it right/down (or shrinking left/up) moves exactly this line.
layoutDividers :: Layout -> [(Bool, Int, Int, Int, Int, PaneId)]
layoutDividers l = case lCell l of
    LPane _ -> []
    LRow ks -> [ (True, lX k + lW k, lY l, 1, lH l, p)
               | k <- dropLast ks
               , p <- take 1 [ q | (q, qx, _, qw, _) <- layoutPanes k
                                 , qx + qw == lX k + lW k ] ]
               <> concatMap layoutDividers ks
    LCol ks -> [ (False, lX l, lY k + lH k, lW l, 1, p)
               | k <- dropLast ks
               , p <- take 1 [ q | (q, _, qy, _, qh) <- layoutPanes k
                                 , qy + qh == lY k + lH k ] ]
               <> concatMap layoutDividers ks
  where
    dropLast [] = []
    dropLast xs = init xs

-- | Blocking dequeue of the next event (drain from a dedicated thread).
ccEvents :: CC -> IO TmuxEvent
ccEvents = atomically . readTChan . ccChan

-- | Blocking dequeue of the next event PLUS everything else already queued,
-- in one go.  Under an output storm (a TUI redrawing on every scroll tick)
-- thousands of @%output@ lines arrive per second; draining them in batches
-- lets the consumer collapse them (see 'coalesceOutputs') instead of paying
-- a reflex propagation + jsaddle round trip per line — which is what made
-- typing choppy while a pane was busy.
ccEventsBatch :: CC -> IO [TmuxEvent]
ccEventsBatch cc = atomically $ do
    e  <- readTChan (ccChan cc)
    es <- drain
    return (e : es)
  where
    drain = tryReadTChan (ccChan cc) >>= \case
        Nothing -> return []
        Just x  -> (x :) <$> drain

-- | Merge runs of consecutive 'EvOutput' for the SAME pane into one event
-- (order between panes and around other events is preserved), so a batch of
-- storm output becomes a single @xterm.write@ per pane.
coalesceOutputs :: [TmuxEvent] -> [TmuxEvent]
coalesceOutputs (EvOutput p a : rest) =
    let (bs, rest') = run rest
    in EvOutput p (BS.concat (a : bs)) : coalesceOutputs rest'
  where
    run (EvOutput q b : r) | q == p = let (bs, r') = run r in (b : bs, r')
    run r = ([], r)
coalesceOutputs (e : rest) = e : coalesceOutputs rest
coalesceOutputs [] = []

-- | Submit a command line: enqueue its reply slot and write it (correlation
-- is by order, so both happen under the lock).  'False' if the client is
-- dead / the write failed.
ccSubmit :: CC -> Pending -> Text -> IO Bool
ccSubmit cc pending cmd = do
    dead <- readIORef (ccClosed cc)
    if dead then return False else
        withMVar (ccLock cc) $ \() -> do
            r <- try $ do
                modifyMVar_ (ccPending cc) $ \vs -> return (vs ++ [pending])
                BSC.hPutStrLn (ccIn cc) (TE.encodeUtf8 cmd)
                IO.hFlush (ccIn cc)
            case r of
              Left (e :: SomeException) -> do
                  ccWarn ("command write failed (" <> show e <> "): " <> T.unpack cmd)
                  return False
              Right () -> return True

-- | Send a command line and wait for its correlated reply.
-- @Left@ = @%error@ body (or client death).
ccCommand :: CC -> Text -> IO (Either Text [Text])
ccCommand cc cmd = do
    v  <- newEmptyMVar
    ok <- ccSubmit cc (PendingVar v) cmd
    if ok then takeMVar v else return (Left "tmux control client exited")

-- | Submit a command whose reply comes back as an 'EvReply' with the given
-- tag, IN STREAM ORDER on the event channel — use when the reply's position
-- relative to @%output@ matters (the capture-based jump-ahead resync).
ccCommandTagged :: CC -> Text -> Text -> IO ()
ccCommandTagged cc tag cmd = void $ ccSubmit cc (PendingTag tag) cmd

-- | Fire-and-forget command: submitted (and ordered) like 'ccCommand', but
-- the caller doesn't wait for the reply — for keystrokes and other
-- latency-sensitive sends, whose reply would otherwise be waited on BEHIND
-- the processing of any output burst in progress.  Errors are still
-- reported ('ccWarn') when the reply eventually arrives.
ccSend :: CC -> Text -> IO ()
ccSend cc cmd = do
    v  <- newEmptyMVar
    ok <- ccSubmit cc (PendingVar v) cmd
    when ok . void . forkIO $ takeMVar v >>= \case
        Left err | err /= "tmux control client exited" ->
            ccWarn ("async command failed: " <> T.unpack cmd
                    <> " -> " <> T.unpack (T.strip err))
        _ -> return ()

-- | Type raw bytes into a pane (@send-keys -H@, chunked to respect argv
-- limits).  This is the xterm @onData@ → tmux path.  Fire-and-forget
-- ('ccSend'): typing must neither wait behind output processing nor be
-- reordered (submission order is the lock order of the calling thread).
ccSendBytes :: CC -> PaneId -> BS.ByteString -> IO ()
ccSendBytes = ccSendBytesChunked 128

-- | 'ccSendBytes' with kilobyte chunks: for machine-to-machine streams (the
-- jsaddle-terminal tunnel's RESULTS/SYNC frames), where per-command overhead
-- dominates.  Verified against tmux 3.6a: ~12KB command lines work, ~49KB
-- kills the client — 1024 bytes (~3KB lines) is comfortably inside.
ccSendBytesBig :: CC -> PaneId -> BS.ByteString -> IO ()
ccSendBytesBig = ccSendBytesChunked 1024

ccSendBytesChunked :: Int -> CC -> PaneId -> BS.ByteString -> IO ()
ccSendBytesChunked n cc pane = mapM_ send1 . chunks
  where
    chunks bs | BS.null bs = []
              | otherwise  = let (a, b) = BS.splitAt n bs in a : chunks b
    send1 chunk = ccSend cc $
        "send-keys -t " <> pane <> " -H " <>
        T.unwords [ T.pack (pad (showHex w "")) | w <- BS.unpack chunk ]
    pad [c] = ['0', c]
    pad s   = s

-- | Tell tmux the size of our (virtual) client, in cells.
ccResize :: CC -> Int -> Int -> IO ()
ccResize cc w h = void . ccCommand cc $
    "refresh-client -C " <> T.pack (show w) <> "x" <> T.pack (show h)

-- | Set this client's size for ONE window (@refresh-client -C \@win:WxH@).
-- Unlike the plain client size, a per-window size is a hard clamp in tmux's
-- window-size calculation no matter which client is \"latest\" — with
-- @window-size latest@ a control client can never become the latest client
-- (only real key input updates it), so this is the only way a control-mode
-- UI stays authoritative over the window it displays while a regular client
-- is also attached (iTerm2 does the same).  It dies with the client, so no
-- cleanup is needed on exit.
ccResizeWindow :: CC -> WindowId -> Int -> Int -> IO ()
ccResizeWindow cc win w h = void . ccCommand cc $
    "refresh-client -C " <> win <> ":" <> T.pack (show w) <> "x" <> T.pack (show h)

-- | Drop the per-window size again (@refresh-client -C \@win:*@), e.g. when
-- the widget stops displaying that window.
ccClearWindowSize :: CC -> WindowId -> IO ()
ccClearWindowSize cc win = void . ccCommand cc $
    "refresh-client -C " <> win <> ":*"

-- | Is the client still running?
ccAlive :: CC -> IO Bool
ccAlive cc = do
    closed <- readIORef (ccClosed cc)
    if closed then return False
      else (== Nothing) <$> getProcessExitCode (ccProc cc)

-- | Detach politely; kill if it doesn't die promptly.
stopCC :: CC -> IO ()
stopCC cc = do
    _ <- (BSC.hPutStrLn (ccIn cc) "detach-client" >> IO.hFlush (ccIn cc))
             `catch` \(_ :: SomeException) -> return ()
    r <- timeout 2000000 (waitForProcess (ccProc cc))
    case r of
      Just _  -> return ()
      Nothing -> do
          terminateProcess (ccProc cc) `catch` \(_ :: SomeException) -> return ()
          void $ timeout 2000000 (waitForProcess (ccProc cc))
    hClose (ccIn cc) `catch` \(_ :: SomeException) -> return ()
    writeIORef (ccClosed cc) True
