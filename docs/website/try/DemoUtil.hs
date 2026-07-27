{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Shared plumbing for the try/ generator scripts (boot libraries only, so
-- everything runs with a bare @runghc@ from the dev shell).
--
-- The JSON encoder reproduces Python's @json.dumps@ output byte for byte
-- (@ensure_ascii=True@, the default separators, and its @indent=n@ layout):
-- the generated demo-*.js artifacts were originally produced by Python
-- scripts, and keeping the serialisation identical keeps their diffs
-- reviewable and made the port verifiable by direct comparison.
module DemoUtil
  ( Value(..)
  , dumps
  , parseJson
  , objLookup
  , b64encode
  , crc32
  , stripOsc, stripCsi, visible
  , readFileUtf8, writeFileUtf8
  ) where

import Data.Array (Array, listArray, (!))
import Data.Bits (shiftR, xor, (.&.), testBit)
import qualified Data.ByteString as BS
import Data.Char (chr, ord, isDigit, isHexDigit, digitToInt)
import Data.List (foldl', intersperse)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Data.Word (Word32, Word8)
import Text.Printf (printf)

-- ---------------------------------------------------------------------------
-- JSON

-- | Ints and doubles are separate: JSON output must render 12 as @12@, never
-- @12.0@ (Python distinguishes them the same way).
data Value
  = JNull
  | JBool !Bool
  | JInt !Integer
  | JDouble !Double
  | JStr !Text
  | JArr [Value]
  | JObj [(Text, Value)]   -- ^ assoc list: key order is significant (insertion order, like a Python dict)
  deriving (Eq, Show)

-- | @dumps Nothing@ = @json.dumps(x)@; @dumps (Just n)@ = @json.dumps(x, indent=n)@.
dumps :: Maybe Int -> Value -> TL.Text
dumps mind = B.toLazyText . go 0
  where
    go :: Int -> Value -> B.Builder
    go _ JNull        = "null"
    go _ (JBool True) = "true"
    go _ (JBool False)= "false"
    go _ (JInt n)     = B.fromString (show n)
    go _ (JDouble d)  = B.fromString (printf "%g" d)
    go _ (JStr s)     = escStr s
    go _ (JArr [])    = "[]"
    go _ (JObj [])    = "{}"
    go lvl (JArr xs)  = case mind of
      Nothing -> "[" <> mconcat (intersperse ", " (map (go 0) xs)) <> "]"
      Just _  -> "[" <> nl (lvl + 1)
                     <> mconcat (intersperse ("," <> nl (lvl + 1)) (map (go (lvl + 1)) xs))
                     <> nl lvl <> "]"
    go lvl (JObj kvs) = case mind of
      Nothing -> "{" <> mconcat (intersperse ", " (map (item 0) kvs)) <> "}"
      Just _  -> "{" <> nl (lvl + 1)
                     <> mconcat (intersperse ("," <> nl (lvl + 1)) (map (item (lvl + 1)) kvs))
                     <> nl lvl <> "}"
    item lvl (k, v) = escStr k <> ": " <> go lvl v
    nl lvl = case mind of
      Nothing -> mempty
      Just n  -> "\n" <> B.fromText (T.replicate (lvl * n) " ")

-- Python-style string escaping (ensure_ascii): printable ASCII literal,
-- everything else \uXXXX (lower-case hex, surrogate pairs for astral chars).
escStr :: Text -> B.Builder
escStr s = "\"" <> T.foldr (\c acc -> escChar c <> acc) mempty s <> "\""
  where
    escChar '"'  = "\\\""
    escChar '\\' = "\\\\"
    escChar '\n' = "\\n"
    escChar '\r' = "\\r"
    escChar '\t' = "\\t"
    escChar '\b' = "\\b"
    escChar '\f' = "\\f"
    escChar c
      | c >= ' ' && c <= '~' = B.singleton c
      | cp < 0x10000 = u cp
      | otherwise =
          let cp' = cp - 0x10000
          in u (0xD800 + (cp' `shiftR` 10)) <> u (0xDC00 + (cp' .&. 0x3FF))
      where
        cp = ord c
        u n = B.fromString (printf "\\u%04x" n)

-- | Parse a complete JSON document (trailing garbage is an error, like
-- @json.loads@).
parseJson :: Text -> Either String Value
parseJson t0 = do
  (v, rest) <- pValue (skipWs t0)
  if T.null (skipWs rest) then Right v else Left "trailing data after JSON value"
  where
    skipWs = T.dropWhile (`elem` (" \t\n\r" :: String))

    pValue :: Text -> Either String (Value, Text)
    pValue t = case T.uncons t of
      Nothing -> Left "unexpected end of input"
      Just (c, r)
        | c == '{'  -> pObj r []
        | c == '['  -> pArr r []
        | c == '"'  -> do (s, r') <- pStr r []; pure (JStr s, r')
        | Just r' <- T.stripPrefix "true" t  -> pure (JBool True, r')
        | Just r' <- T.stripPrefix "false" t -> pure (JBool False, r')
        | Just r' <- T.stripPrefix "null" t  -> pure (JNull, r')
        | c == '-' || isDigit c -> pNum t
        | otherwise -> Left ("unexpected character " ++ show c)

    pObj t acc = case T.uncons (skipWs t) of
      Just ('}', r) | null acc -> pure (JObj [], r)
      _ -> do
        t1 <- expect '"' (skipWs t)
        (k, t2) <- pStr t1 []
        t3 <- expect ':' (skipWs t2)
        (v, t4) <- pValue (skipWs t3)
        case T.uncons (skipWs t4) of
          Just (',', r) -> pObj r ((k, v) : acc)
          Just ('}', r) -> pure (JObj (reverse ((k, v) : acc)), r)
          _ -> Left "expected ',' or '}' in object"

    pArr t acc = case T.uncons (skipWs t) of
      Just (']', r) | null acc -> pure (JArr [], r)
      _ -> do
        (v, t1) <- pValue (skipWs t)
        case T.uncons (skipWs t1) of
          Just (',', r) -> pArr r (v : acc)
          Just (']', r) -> pure (JArr (reverse (v : acc)), r)
          _ -> Left "expected ',' or ']' in array"

    expect c t = case T.uncons t of
      Just (c', r) | c' == c -> Right r
      _ -> Left ("expected " ++ show c)

    -- acc is a reversed [Char]; escapes (incl. \uXXXX surrogate pairs) folded in.
    pStr t acc = case T.uncons t of
      Nothing -> Left "unterminated string"
      Just ('"', r) -> pure (T.pack (reverse acc), r)
      Just ('\\', r) -> case T.uncons r of
        Just ('n', r')  -> pStr r' ('\n' : acc)
        Just ('t', r')  -> pStr r' ('\t' : acc)
        Just ('r', r')  -> pStr r' ('\r' : acc)
        Just ('b', r')  -> pStr r' ('\b' : acc)
        Just ('f', r')  -> pStr r' ('\f' : acc)
        Just ('/', r')  -> pStr r' ('/' : acc)
        Just ('\\', r') -> pStr r' ('\\' : acc)
        Just ('"', r')  -> pStr r' ('"' : acc)
        Just ('u', r')  -> do
          (hi, r1) <- hex4 r'
          if hi >= 0xD800 && hi <= 0xDBFF
            then case T.stripPrefix "\\u" r1 of
              Just r2 -> do
                (lo, r3) <- hex4 r2
                if lo >= 0xDC00 && lo <= 0xDFFF
                  then pStr r3 (chr (0x10000 + ((hi - 0xD800) * 0x400) + (lo - 0xDC00)) : acc)
                  else pStr r3 (chr lo : chr hi : acc)
              Nothing -> pStr r1 (chr hi : acc)
            else pStr r1 (chr hi : acc)
        _ -> Left "bad escape"
      Just (c, r) -> pStr r (c : acc)

    hex4 t
      | T.length h == 4 && T.all isHexDigit h =
          Right (T.foldl' (\n c -> n * 16 + digitToInt c) 0 h, T.drop 4 t)
      | otherwise = Left "bad \\u escape"
      where h = T.take 4 t

    pNum t =
      let (body, rest) = T.span (`elem` ("+-.eE0123456789" :: String)) t
          s = T.unpack body
      in if any (`elem` (".eE" :: String)) s
           then case reads s :: [(Double, String)] of
                  [(d, "")] -> Right (JDouble d, rest)
                  _ -> Left ("bad number " ++ s)
           else case reads s :: [(Integer, String)] of
                  [(n, "")] -> Right (JInt n, rest)
                  _ -> Left ("bad number " ++ s)

objLookup :: Text -> Value -> Maybe Value
objLookup k (JObj kvs) = lookup k kvs
objLookup _ _ = Nothing

-- ---------------------------------------------------------------------------
-- base64 (standard alphabet, padded) — bytestring ships no encoder.

b64encode :: BS.ByteString -> Text
b64encode = T.pack . go . BS.unpack
  where
    alph = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    at :: Int -> Char
    at i = alph !! i
    go :: [Word8] -> [Char]
    go (a:b:c:r) =
      let n = fromIntegral a * 65536 + fromIntegral b * 256 + fromIntegral c :: Int
      in at (n `shiftR` 18) : at ((n `shiftR` 12) .&. 63) : at ((n `shiftR` 6) .&. 63) : at (n .&. 63) : go r
    go [a, b] =
      let n = fromIntegral a * 65536 + fromIntegral b * 256 :: Int
      in [at (n `shiftR` 18), at ((n `shiftR` 12) .&. 63), at ((n `shiftR` 6) .&. 63), '=']
    go [a] =
      let n = fromIntegral a * 65536 :: Int
      in [at (n `shiftR` 18), at ((n `shiftR` 12) .&. 63), '=', '=']
    go [] = []

-- ---------------------------------------------------------------------------
-- CRC-32 (IEEE, zlib.crc32-compatible) — used only as a cache-buster key.

crcTable :: Array Int Word32
crcTable = listArray (0, 255)
  [ foldl' step (fromIntegral i) [1 .. 8 :: Int] | i <- [0 .. 255 :: Int] ]
  where
    step c _ = if testBit c 0 then (c `shiftR` 1) `xor` 0xEDB88320 else c `shiftR` 1

crc32 :: BS.ByteString -> Word32
crc32 = xor 0xffffffff . BS.foldl' step 0xffffffff
  where
    step acc b = (acc `shiftR` 8) `xor` (crcTable ! fromIntegral ((acc `xor` fromIntegral b) .&. 0xff))

-- ---------------------------------------------------------------------------
-- ANSI stripping — ports of the regexes shared by the Python generators and
-- terminalLinksJs (src/IDE/Web/Main.hs).  If the JS changes, change these:
--   CSI: \x1b\[[0-9;:?]*[ -/]*[@-~]
--   OSC: \x1b\][^\x07\x1b]*(\x07|\x1b\\)
-- Unmatched introducers are left in place, exactly like re.sub.

stripOsc :: Text -> Text
stripOsc t = case T.breakOn "\x1b]" t of
  (pre, rest)
    | T.null rest -> pre
    | otherwise ->
        let body = T.drop 2 rest
            (run, after) = T.span (\c -> c /= '\x07' && c /= '\x1b') body
        in case () of
          _ | Just more <- T.stripPrefix "\x07" after -> pre <> stripOsc more
            | Just more <- T.stripPrefix "\x1b\\" after -> pre <> stripOsc more
            | otherwise -> pre <> "\x1b]" <> run <> stripOsc after

stripCsi :: Text -> Text
stripCsi t = case T.breakOn "\x1b[" t of
  (pre, rest)
    | T.null rest -> pre
    | otherwise ->
        let body = T.drop 2 rest
            (params, r1) = T.span (`elem` ("0123456789;:?" :: String)) body
            (inter, r2)  = T.span (\c -> c >= ' ' && c <= '/') r1
        in case T.uncons r2 of
          Just (f, more) | f >= '@' && f <= '~' -> pre <> stripCsi more
          _ -> pre <> "\x1b[" <> params <> inter <> stripCsi r2

-- | OSC first, then CSI — the order the Python `visible` helpers used.
visible :: Text -> Text
visible = stripCsi . stripOsc

-- ---------------------------------------------------------------------------
-- Strict UTF-8 file IO (locale-independent; invalid UTF-8 is an error, the
-- same behaviour the Python scripts had).

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 p = TE.decodeUtf8 <$> BS.readFile p

writeFileUtf8 :: FilePath -> Text -> IO ()
writeFileUtf8 p = BS.writeFile p . TE.encodeUtf8
