{-# LANGUAGE OverloadedStrings #-}

-- | The wire transport for the Language Server Protocol: spawning a server
-- process and reading\/writing JSON-RPC messages with @Content-Length@
-- framing over its stdio, as specified by the base protocol
-- (<https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#baseProtocol>).
--
-- This module is deliberately protocol-agnostic: it moves opaque
-- 'ByteString' payloads and knows nothing about the JSON content.  The
-- typed layer lives in "Language.LSP.Client.Session".
module Language.LSP.Client.Transport
    ( ServerHandles(..)
    , spawnServer
    , withServerHandles
    , sendFramed
    , recvFramed
    ) where

import           Control.Exception (throwIO, ErrorCall(..))
import           Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Word (Word8)
import           System.IO
import           System.Process

-- | The stdio handles and process handle of a running language server.
data ServerHandles = ServerHandles
    { shIn   :: Handle          -- ^ server stdin  (we write requests here)
    , shOut  :: Handle          -- ^ server stdout (we read responses here)
    , shErr  :: Handle          -- ^ server stderr (human-readable log)
    , shProc :: ProcessHandle
    }

-- | Spawn a language server.  The three stdio streams are attached to
-- pipes and put in binary, unbuffered\/block-buffered mode ready for
-- 'sendFramed' \/ 'recvFramed'.
spawnServer
    :: FilePath                 -- ^ executable (e.g. @haskell-language-server@)
    -> [String]                 -- ^ arguments (e.g. @[\"--lsp\"]@)
    -> Maybe FilePath           -- ^ working directory (project root)
    -> Maybe [(String, String)] -- ^ environment (e.g. leksah's per-project nix env)
    -> IO ServerHandles
spawnServer cmd args mcwd menv = do
    (Just hin, Just hout, Just herr, ph) <- createProcess (proc cmd args)
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , cwd     = mcwd
        , env     = menv
        }
    hSetBinaryMode hin  True
    hSetBinaryMode hout True
    hSetBuffering  hin  (BlockBuffering Nothing)
    hSetBuffering  hout NoBuffering
    hSetBuffering  herr LineBuffering
    return ServerHandles { shIn = hin, shOut = hout, shErr = herr, shProc = ph }

-- | Spawn a server, run an action with its handles, and make sure the
-- process is torn down afterwards.
withServerHandles
    :: FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)]
    -> (ServerHandles -> IO a) -> IO a
withServerHandles cmd args mcwd menv act = do
    sh <- spawnServer cmd args mcwd menv
    r  <- act sh
    _  <- terminateProcess (shProc sh) `seqAfter` waitForProcess (shProc sh)
    return r
  where
    seqAfter a b = a >> b

-- | Write one JSON-RPC message with a @Content-Length@ header.  The payload
-- must already be UTF-8 encoded JSON; the byte length in the header is the
-- length of exactly those bytes.
sendFramed :: Handle -> BS.ByteString -> IO ()
sendFramed h payload = do
    BSC.hPutStr h header
    BS.hPut     h payload
    hFlush      h
  where
    header = BSC.concat
        [ "Content-Length: ", BSC.pack (show (BS.length payload)), "\r\n\r\n" ]

-- | Read one framed JSON-RPC message.  Returns 'Nothing' at end of input
-- (the server closed its stdout / exited).  Throws on a malformed header.
recvFramed :: Handle -> IO (Maybe BS.ByteString)
recvFramed h = do
    mlen <- readHeaders h Nothing
    case mlen of
        Nothing  -> return Nothing
        Just len -> Just <$> hGetExactly h len

-- | Read header lines until the blank separator line, returning the
-- @Content-Length@ value.  Other headers (e.g. @Content-Type@) are ignored.
readHeaders :: Handle -> Maybe Int -> IO (Maybe Int)
readHeaders h acc = do
    eof <- hIsEOF h
    if eof
        then return Nothing   -- clean EOF before any header => stream closed
        else do
            line <- stripCR <$> hGetLineBS h
            if BS.null line
                then case acc of
                    Just _  -> return acc
                    Nothing -> throwIO (ErrorCall "lsp-client: message with no Content-Length header")
                else case parseContentLength line of
                    Just n  -> readHeaders h (Just n)
                    Nothing -> readHeaders h acc

parseContentLength :: BS.ByteString -> Maybe Int
parseContentLength line =
    case BS.break (== colon) line of
        (name, rest)
            | lower name == "content-length"
            , Just (n, _) <- BSC.readInt (BSC.dropWhile (== ' ') (BS.drop 1 rest))
            -> Just n
        _ -> Nothing
  where
    colon = 58 :: Word8
    lower = BSC.map toLowerC
    toLowerC c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- | Read a line terminated by @\\n@ as a strict 'ByteString' (without the
-- @\\n@).  'BSC.hGetLine' does this but drops encoding concerns; the handle
-- is in binary mode so we get raw bytes.
hGetLineBS :: Handle -> IO BS.ByteString
hGetLineBS = BSC.hGetLine

-- | Read exactly @n@ bytes, retrying short reads until satisfied or EOF.
hGetExactly :: Handle -> Int -> IO BS.ByteString
hGetExactly h n = go n []
  where
    go 0 chunks = return (BS.concat (reverse chunks))
    go k chunks = do
        chunk <- BS.hGet h k
        when (BS.null chunk) $
            throwIO (ErrorCall "lsp-client: unexpected EOF mid-message")
        go (k - BS.length chunk) (chunk : chunks)

stripCR :: BS.ByteString -> BS.ByteString
stripCR bs
    | not (BS.null bs) && BS.last bs == cr = BS.init bs
    | otherwise                            = bs
  where cr = 13 :: Word8
