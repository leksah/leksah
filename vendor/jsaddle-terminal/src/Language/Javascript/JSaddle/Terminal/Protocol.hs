{-# LANGUAGE OverloadedStrings #-}
-- | The jsaddle-terminal wire protocol: jsaddle 'Batch'/'Results' JSON
-- tunnelled over a tmux pane's own stdin/stdout.
--
-- Two stream shapes, one 'Frame' type:
--
-- * __exe → IDE__ (the exe's stdout, seen by the IDE as tmux control-mode
--   @%output@ bytes): OSC frames @ESC ] 5799 ; TYPE ; base64(payload) BEL@.
--   Unknown OSC is invisible on a plain terminal, and tmux forwards it
--   verbatim to control clients, so the frames ride the existing channel —
--   local or over ssh.  Bytes outside frames are ordinary pane output.
--
-- * __IDE → exe__ (injected into the pane's stdin via @send-keys -H@; the
--   exe has its tty in raw mode): @RS TYPE ; base64(payload) LF@
--   (RS = 0x1e).  All frame bytes are printable ASCII plus RS/LF, so no
--   tty processing can mangle them (the exe also clears IEXTEN — the one
--   raw-mode gap on macOS, where VDISCARD/VLNEXT would eat 0x0f/0x16).
--
-- Both scanners are incremental: frames can straddle input chunks
-- arbitrarily (tmux splits @%output@ at arbitrary byte boundaries).
module Language.Javascript.JSaddle.Terminal.Protocol
  ( protoVersion
  , FrameType(..)
  , Frame(..)
  , renderFrameType
  , parseFrameType
    -- * exe → IDE (OSC framing on the pane's stdout)
  , encodeOsc
  , OscScan
  , emptyOscScan
  , scanOsc
    -- * IDE → exe (RS framing on the pane's stdin)
  , encodeRs
  , RsScan
  , emptyRsScan
  , scanRs
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64

-- | Protocol version sent in HELLO/ACK.
protoVersion :: Int
protoVersion = 1

-- | Every message either direction.  Which types are legal in which
-- direction is a convention, not enforced here:
--
-- * exe → IDE: 'Hello', 'Batch', 'SyncReply', 'Log', 'Bye'
-- * IDE → exe: 'Ack', 'Results', 'Sync', 'Close'
data FrameType
  = Hello      -- ^ exe requests a tunnel: @{"proto":1,"caps":["sync"]}@
  | Batch      -- ^ a jsaddle @Batch@ (Haskell → JS commands)
  | SyncReply  -- ^ the @Batch@ answering a pending 'Sync'
  | Log        -- ^ @{"level":…,"msg":…}@ for the IDE to log
  | Bye        -- ^ exe is shutting down; IDE restores the terminal
  | Ack        -- ^ IDE accepted the tunnel (iframe is ready)
  | Results    -- ^ a jsaddle @Results@ (async, JS → Haskell)
  | Sync       -- ^ a jsaddle @Results@ requiring a 'SyncReply' @Batch@
  | Close      -- ^ IDE is tearing down; exe should exit (or fall back)
  deriving (Eq, Show)

data Frame = Frame
  { frameType    :: FrameType
  , framePayload :: ByteString  -- ^ raw (decoded) payload, normally JSON
  } deriving (Eq, Show)

renderFrameType :: FrameType -> ByteString
renderFrameType t = case t of
  Hello     -> "HELLO"
  Batch     -> "BATCH"
  SyncReply -> "SYNCR"
  Log       -> "LOG"
  Bye       -> "BYE"
  Ack       -> "ACK"
  Results   -> "RESULTS"
  Sync      -> "SYNC"
  Close     -> "CLOSE"

parseFrameType :: ByteString -> Maybe FrameType
parseFrameType t = case t of
  "HELLO"   -> Just Hello
  "BATCH"   -> Just Batch
  "SYNCR"   -> Just SyncReply
  "LOG"     -> Just Log
  "BYE"     -> Just Bye
  "ACK"     -> Just Ack
  "RESULTS" -> Just Results
  "SYNC"    -> Just Sync
  "CLOSE"   -> Just Close
  _         -> Nothing

-- | @ESC ] 5799 ;@ — the OSC frame introducer.
oscMarker :: ByteString
oscMarker = "\ESC]5799;"

oscTerminator :: Char
oscTerminator = '\BEL'

-- | A frame whose body grows past this without a terminator is flushed
-- through as plain output (protects the scanner from unbounded buffering
-- if a stray marker appears in binary output).
oscMaxFrame :: Int
oscMaxFrame = 8 * 1024 * 1024

-- | exe → IDE frame bytes (write to the pane's stdout).
encodeOsc :: Frame -> ByteString
encodeOsc (Frame t p) =
  oscMarker <> renderFrameType t <> ";" <> B64.encode p <> BSC.singleton oscTerminator

-- | Incremental scanner state for the OSC (pane stdout) stream: the bytes
-- held back because they might be the start of an unterminated frame.
newtype OscScan = OscScan ByteString
  deriving (Eq, Show)

emptyOscScan :: OscScan
emptyOscScan = OscScan BS.empty

-- | Feed a chunk; get (passthrough bytes for the terminal, complete frames,
-- new state).  Malformed frames (unknown type / bad base64) are passed
-- through verbatim so nothing is silently swallowed.
scanOsc :: OscScan -> ByteString -> (ByteString, [Frame], OscScan)
scanOsc (OscScan held) chunk = go mempty [] (held <> chunk)
  where
    go passOut frames buf =
      case BS.breakSubstring oscMarker buf of
        (before, rest)
          | BS.null rest ->
              -- No full marker: pass everything except a trailing proper
              -- prefix of the marker, which we hold for the next chunk.
              let keep = longestMarkerPrefix before
                  passN = BS.length before - keep
              in ( passOut <> BS.take passN before
                 , reverse frames
                 , OscScan (BS.drop passN before) )
          | otherwise ->
              let body = BS.drop (BS.length oscMarker) rest
              in case BSC.elemIndex oscTerminator body of
                   Nothing
                     | BS.length body > oscMaxFrame ->
                         -- Unterminated and huge: give up on it as a frame.
                         go (passOut <> before <> oscMarker) frames body
                     | otherwise ->
                         ( passOut <> before
                         , reverse frames
                         , OscScan (oscMarker <> body) )
                   Just i ->
                     let inner = BS.take i body
                         rest' = BS.drop (i + 1) body
                     in case decodeInner inner of
                          Just f  -> go (passOut <> before) (f : frames) rest'
                          Nothing ->
                            -- Malformed: emit the raw bytes untouched.
                            go (passOut <> before <> oscMarker
                                        <> BS.take (i + 1) body)
                               frames rest'
    decodeInner inner =
      let (t, rest) = BSC.break (== ';') inner
      in case (parseFrameType t, BS.uncons rest) of
           (Just ft, Just (_, b64)) ->
             either (const Nothing) (Just . Frame ft) (B64.decode b64)
           _ -> Nothing
    -- Length of the longest suffix of @bs@ that is a proper prefix of the
    -- marker (bounded by the marker length, so this is O(1)-ish).
    longestMarkerPrefix bs =
      let m = min (BS.length bs) (BS.length oscMarker - 1)
          candidates = [ k | k <- [m, m-1 .. 1]
                           , BS.drop (BS.length bs - k) bs
                               == BS.take k oscMarker ]
      in case candidates of
           (k:_) -> k
           []    -> 0

-- | @RS@ — the stdin frame introducer.
rsMarker :: Char
rsMarker = '\RS'  -- 0x1e

rsTerminator :: Char
rsTerminator = '\n'

-- | IDE → exe frame bytes (inject into the pane's stdin).
encodeRs :: Frame -> ByteString
encodeRs (Frame t p) =
  BSC.singleton rsMarker <> renderFrameType t <> ";" <> B64.encode p
    <> BSC.singleton rsTerminator

-- | Incremental scanner state for the RS (exe stdin) stream.  Bytes outside
-- frames are dropped (stray typing into the pane).
newtype RsScan = RsScan ByteString
  deriving (Eq, Show)

emptyRsScan :: RsScan
emptyRsScan = RsScan BS.empty

scanRs :: RsScan -> ByteString -> ([Frame], RsScan)
scanRs (RsScan held) chunk = go [] (held <> chunk)
  where
    go frames buf =
      case BSC.elemIndex rsMarker buf of
        Nothing -> (reverse frames, RsScan BS.empty)   -- drop non-frame bytes
        Just i ->
          let body = BS.drop (i + 1) buf
          in case BSC.elemIndex rsTerminator body of
               Nothing -> (reverse frames, RsScan (BS.drop i buf))
               Just j ->
                 let inner = BS.take j body
                     rest  = BS.drop (j + 1) body
                     (t, r) = BSC.break (== ';') inner
                     mf = case (parseFrameType t, BS.uncons r) of
                            (Just ft, Just (_, b64)) ->
                              either (const Nothing) (Just . Frame ft)
                                     (B64.decode b64)
                            _ -> Nothing
                 in go (maybe frames (: frames) mf) rest
