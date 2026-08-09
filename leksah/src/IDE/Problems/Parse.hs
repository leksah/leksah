-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

-- | Incremental, tool-auto-detecting parser for build-tool output.
--
-- Feed it the interleaved stdout+stderr of a build, one line at a time
-- (via 'parseLine'), and it emits 'Problem's as they complete; call
-- 'parseEnd' when the stream ends to flush a trailing message.
--
-- Recognised shapes:
--
--   * GHC \/ cabal \/ stack diagnostics
--     (@PATH:LINE:COL: error:@ headers with indented bodies, all the
--     span variants, old bare headers, @|@-gutter caret context),
--   * rustc \/ cargo diagnostics
--     (@error[E0308]: …@ headers followed by @ --> PATH:LINE:COL@ and a
--     @|@-gutter body).
--
-- Everything else (progress lines, linker noise, solver output) is
-- ignored.  ANSI SGR escapes are stripped before matching, so coloured
-- output parses the same as plain.  Parsing is total: no input line can
-- make it crash.
--
-- Compilers print 1-based lines\/columns with inclusive end columns;
-- the 'Problem' model is 0-based half-open — conversion happens here.
module IDE.Problems.Parse
  ( ParseSt
  , newParseSt
  , parseLine
  , parseEnd
  ) where

import Data.Char (isAlpha, isDigit)
import Data.Text (Text)
import qualified Data.Text as T

import IDE.Problems.Types

-- ---------------------------------------------------------------------------
-- Public interface

-- | Opaque parser state: at most one partially-accumulated message.
newtype ParseSt = ParseSt Pending

-- | The state to start a fresh stream with.
newParseSt :: ParseSt
newParseSt = ParseSt PNone

-- | Feed one line (without its trailing newline).  Returns the new state
-- and any problems that COMPLETED on this line (a message is complete when
-- the line after its body arrives, so emission lags by one line).
parseLine :: ParseSt -> Text -> (ParseSt, [Problem])
parseLine (ParseSt p) raw =
    let (p', ps) = step p (clean raw)
    in (ParseSt p', ps)
  where
    clean = T.dropWhileEnd (== '\r') . stripAnsi

-- | End of stream: flush a message still being accumulated.
parseEnd :: ParseSt -> [Problem]
parseEnd (ParseSt p) = case p of
    PNone       -> []
    PGhc a      -> [finishGhc a]
    PRustHead _ -> []             -- header never got a --> line: no path
    PRustBody a -> [finishRust a]

-- ---------------------------------------------------------------------------
-- State machine

-- | GHC message being accumulated (header seen, collecting body).
data GhcAcc = GhcAcc
    { gPath :: !FilePath
    , gRange :: !Range
    , gSev :: !Severity
    , gCode :: !(Maybe Text)
    , gHead :: !Text    -- ^ message text on the header line ("" if none)
    , gBody :: ![Text]  -- ^ body lines, reversed, still indented
    }

-- | rustc header seen, waiting for its @ --> PATH:LINE:COL@ line.
data RustHead = RustHead
    { rhSev :: !Severity
    , rhCode :: !(Maybe Text)
    , rhMsg :: !Text
    , rhWait :: !Int    -- ^ lines seen since the header without a -->
    }

-- | rustc message with its span known, consuming the gutter body.
data RustAcc = RustAcc
    { rSev :: !Severity
    , rCode :: !(Maybe Text)
    , rPath :: !FilePath
    , rRange :: !Range
    , rMsg :: !Text
    , rExtra :: ![Text] -- ^ @= note:@ / @= help:@ lines, reversed
    }

data Pending
    = PNone
    | PGhc !GhcAcc
    | PRustHead !RustHead
    | PRustBody !RustAcc

step :: Pending -> Text -> (Pending, [Problem])

step PNone line
    | isBlank line               = (PNone, [])
    | Just a <- ghcHeader line   = (PGhc a, [])
    | Just h <- rustHeader line  = (PRustHead h, [])
    | otherwise                  = (PNone, [])

step (PGhc a) line
    | isBlank line    = (PNone, [finishGhc a])
    | isGutter line   = (PGhc a, [])   -- source/caret context: not message
    | isIndented line = (PGhc a { gBody = line : gBody a }, [])
    | otherwise       =                -- message over; line may start another
        let (p', ps) = step PNone line
        in (p', finishGhc a : ps)

step (PRustHead h) line
    | Just (path, rng) <- arrowLine line =
        (PRustBody RustAcc
            { rSev = rhSev h, rCode = rhCode h
            , rPath = path, rRange = rng
            , rMsg = rhMsg h, rExtra = [] }, [])
    | isBlank line              = (PNone, [])   -- no span: not a problem
    | Just _ <- ghcHeader line  = step PNone line
    | Just _ <- rustHeader line = step PNone line
    | rhWait h >= 3             = step PNone line
    | otherwise                 = (PRustHead h { rhWait = rhWait h + 1 }, [])

step (PRustBody a) line
    | isBlank line                    = (PNone, [finishRust a])
    | Just t <- rustNoteHelp line     = (PRustBody a { rExtra = t : rExtra a }, [])
    | isGutter line || isIndented line = (PRustBody a, [])  -- rendering only
    | otherwise                       =
        let (p', ps) = step PNone line
        in (p', finishRust a : ps)

-- ---------------------------------------------------------------------------
-- Completion

finishGhc :: GhcAcc -> Problem
finishGhc a = Problem
    { pPath = gPath a
    , pRange = gRange a
    , pSeverity = gSev a
    , pCode = gCode a
    , pMessage = T.intercalate "\n" parts
    , pTool = "ghc"
    }
  where
    body = reverse (gBody a)
    nonBlank = filter (not . T.null . T.strip) body
    indent
        | null nonBlank = 0
        | otherwise = minimum (map (T.length . T.takeWhile (== ' ')) nonBlank)
    dedented = map (T.stripEnd . T.drop indent) body
    parts = [gHead a | not (T.null (gHead a))] ++ dedented

finishRust :: RustAcc -> Problem
finishRust a = Problem
    { pPath = rPath a
    , pRange = rRange a
    , pSeverity = rSev a
    , pCode = rCode a
    , pMessage = T.intercalate "\n" (rMsg a : reverse (rExtra a))
    , pTool = "cargo"
    }

-- ---------------------------------------------------------------------------
-- GHC header

-- | Try to read a line as a GHC diagnostic header:
-- @PATH:SPAN: [severity:] [\[tags\]] [message…]@.
ghcHeader :: Text -> Maybe GhcAcc
ghcHeader line
    | T.null line || isIndented line = Nothing
    | otherwise = firstJust (map try (colonPositions line))
  where
    try i
        | isDriveColon i = Nothing
        | otherwise = do
            let path = T.take i line
            (rng, rest) <- parseSpan (T.drop (i + 1) line)
            if validPath path
                then Just (mkGhcAcc (T.unpack path) rng rest)
                else Nothing
    -- C:\x\y.hs — a single letter followed by :\ or :/ is a drive,
    -- not the end of the path.
    isDriveColon i =
        i == 1
        && isAlpha (T.index line 0)
        && T.length line > 2
        && T.index line 2 `elem` ("\\/" :: String)
    -- Timestamps (12:34:56: …) would otherwise parse as headers; real
    -- paths always contain a non-digit.
    validPath p = not (T.null p) && T.any (not . isDigit) p

colonPositions :: Text -> [Int]
colonPositions t = [i | (i, c) <- zip [0 ..] (T.unpack t), c == ':']

-- | Parse the span (and its closing colon) at the start of the text.
-- Variants: @L:C:@, @L:C-C2:@ (C..C2 inclusive), @(L,C)-(L2,C2):@
-- (both endpoints inclusive).  Compilers are 1-based; the model is
-- 0-based half-open, so start columns get -1 and inclusive end columns
-- carry over unchanged.
parseSpan :: Text -> Maybe (Range, Text)
parseSpan t
    | Just t1 <- T.stripPrefix "(" t = do
        (l1, t2) <- num t1
        t3 <- T.stripPrefix "," t2
        (c1, t4) <- num t3
        t5 <- T.stripPrefix ")-(" t4
        (l2, t6) <- num t5
        t7 <- T.stripPrefix "," t6
        (c2, t8) <- num t7
        t9 <- T.stripPrefix "):" t8
        pure (Range (Pos (l1 - 1) (c1 - 1)) (Pos (l2 - 1) c2), t9)
    | otherwise = do
        (l, t1) <- num t
        t2 <- T.stripPrefix ":" t1
        (c, t3) <- num t2
        case T.stripPrefix "-" t3 of
            Just t4 -> do
                (c2, t5) <- num t4
                t6 <- T.stripPrefix ":" t5
                pure (Range (Pos (l - 1) (c - 1)) (Pos (l - 1) c2), t6)
            Nothing -> do
                t4 <- T.stripPrefix ":" t3
                pure (pointRange (Pos (l - 1) (c - 1)), t4)

num :: Text -> Maybe (Int, Text)
num t =
    let (d, r) = T.span isDigit t
    in if T.null d || T.length d > 9 then Nothing else Just (read (T.unpack d), r)

-- | Severity word, bracketed tags, and any same-line message text.
mkGhcAcc :: FilePath -> Range -> Text -> GhcAcc
mkGhcAcc path rng rest = GhcAcc
    { gPath = path
    , gRange = rng
    , gSev = sev
    , gCode = pickCode codes
    , gHead = T.strip afterCodes
    , gBody = []
    }
  where
    r1 = T.stripStart rest
    (sev, afterSev) = severityWord r1
    (codes, afterCodes) = bracketTags afterSev

-- | @error:@ \/ @warning:@ \/ @note:@ (either capitalisation); a header
-- with no severity word is an old-GHC error.
severityWord :: Text -> (Severity, Text)
severityWord t = go
    [ ("error:", SevError), ("Error:", SevError)
    , ("warning:", SevWarning), ("Warning:", SevWarning)
    , ("note:", SevInfo), ("Note:", SevInfo)
    ]
  where
    go [] = (SevError, t)
    go ((w, s):rest) = case T.stripPrefix w t of
        Just x  -> (s, x)
        Nothing -> go rest

-- | Zero or more leading @[…]@ groups (GHC can print several, e.g.
-- @[GHC-66111] [-Wunused-imports]@).
bracketTags :: Text -> ([Text], Text)
bracketTags t0 =
    let t = T.stripStart t0
    in case T.stripPrefix "[" t of
        Just t1 ->
            let (c, r) = T.breakOn "]" t1
            in if T.null r
                then ([], t0)          -- unclosed: not a tag
                else let (cs, rest) = bracketTags (T.drop 1 r)
                     in (c : cs, rest)
        Nothing -> ([], t0)

-- | Prefer a warning-flag tag; @[-Wname, Werror=name]@ means the name
-- before the comma.
pickCode :: [Text] -> Maybe Text
pickCode codes = case filter ("-W" `T.isPrefixOf`) codes of
    (w:_) -> Just (T.strip (T.takeWhile (/= ',') w))
    []    -> case codes of
        (c:_) -> Just (T.strip c)
        []    -> Nothing

-- ---------------------------------------------------------------------------
-- rustc / cargo

-- | @error[E0308]: msg@, @warning: msg@, @note: msg@, @help: msg@ at the
-- start of a line (rustc prints these lowercase and unindented).
rustHeader :: Text -> Maybe RustHead
rustHeader line
    | T.null line || isIndented line = Nothing
    | otherwise = firstJust (map try kws)
  where
    kws =
        [ ("error", SevError), ("warning", SevWarning)
        , ("note", SevInfo), ("help", SevHint)
        ]
    try (kw, sv) = do
        t1 <- T.stripPrefix kw line
        (code, t2) <- case T.stripPrefix "[" t1 of
            Just t' ->
                let (c, r) = T.breakOn "]" t'
                in if T.null r || T.null c
                    then Nothing
                    else Just (Just c, T.drop 1 r)
            Nothing -> Just (Nothing, t1)
        t3 <- T.stripPrefix ":" t2
        pure RustHead { rhSev = sv, rhCode = code, rhMsg = T.strip t3, rhWait = 0 }

-- | @ --> PATH:LINE:COL@ (splitting from the right, so Windows drive
-- colons never interfere).
arrowLine :: Text -> Maybe (FilePath, Range)
arrowLine line = do
    t1 <- T.stripPrefix "-->" (T.stripStart line)
    let t2 = T.strip t1
    (rest1, colD) <- breakLastColon t2
    (pathT, lineD) <- breakLastColon rest1
    c <- readDigits colD
    l <- readDigits lineD
    if T.null pathT
        then Nothing
        else pure (T.unpack pathT, pointRange (Pos (l - 1) (c - 1)))

breakLastColon :: Text -> Maybe (Text, Text)
breakLastColon t =
    let (a, b) = T.breakOnEnd ":" t
    in if T.null a then Nothing else Just (T.dropEnd 1 a, b)

readDigits :: Text -> Maybe Int
readDigits t
    | not (T.null t) && T.length t <= 9 && T.all isDigit t = Just (read (T.unpack t))
    | otherwise = Nothing

-- | A @= note:@ \/ @= help:@ line inside a rustc body; returns the text
-- with the @= @ marker dropped.
rustNoteHelp :: Text -> Maybe Text
rustNoteHelp line =
    let t = T.stripStart line
    in if "= note:" `T.isPrefixOf` t || "= help:" `T.isPrefixOf` t
        then Just (T.drop 2 t)
        else Nothing

-- ---------------------------------------------------------------------------
-- Line shapes

isBlank :: Text -> Bool
isBlank = T.all (== ' ') . T.filter (/= '\t')

isIndented :: Text -> Bool
isIndented t = case T.uncons t of
    Just (c, _) -> c == ' ' || c == '\t'
    Nothing     -> False

-- | Source/caret gutter context: optional spaces, optional line number,
-- optional spaces, then @|@ — GHC 9.6+ and rustc both print these.
isGutter :: Text -> Bool
isGutter line =
    let s1 = T.dropWhile (== ' ') line
        s2 = T.dropWhile isDigit s1
        s3 = T.dropWhile (== ' ') s2
    in case T.uncons s3 of
        Just ('|', _) -> True
        _             -> False

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr (\x acc -> maybe acc Just x) Nothing

-- ---------------------------------------------------------------------------
-- ANSI stripping

-- | Remove ANSI escape sequences (CSI, e.g. SGR colour codes, and OSC);
-- any other escape drops the ESC and the byte after it.  Total on any
-- input, including truncated sequences.
stripAnsi :: Text -> Text
stripAnsi t = case T.breakOn "\ESC" t of
    (a, b)
        | T.null b  -> a
        | otherwise -> a <> stripAnsi (dropSeq b)
  where
    dropSeq s =                       -- s starts with ESC
        case T.uncons (T.drop 1 s) of
            Just ('[', r) ->          -- CSI: params, intermediates, final
                let r1 = T.dropWhile (\c -> c >= '\x30' && c <= '\x3F') r
                    r2 = T.dropWhile (\c -> c >= '\x20' && c <= '\x2F') r1
                in T.drop 1 r2
            Just (']', r) -> dropOsc r
            Just (_, r)   -> r
            Nothing       -> T.empty
    dropOsc r =                       -- OSC: until BEL or ST (ESC \)
        case T.breakOn "\a" r of
            (_, b) | not (T.null b) -> T.drop 1 b
            _ -> case T.breakOn "\ESC\\" r of
                (_, b) | not (T.null b) -> T.drop 2 b
                _                       -> T.empty
