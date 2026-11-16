{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | What Claude Code's status line tells leksah about each session: how full
-- its context window is, what it has cost, and how much of the account's
-- rate limits are used.
--
-- Claude Code runs a @statusLine@ command with a JSON description of the
-- session on stdin — at session start, after each assistant message, after
-- @\/compact@ and on a permission-mode change.  The sessions leksah launches
-- get @leksah-cmd statusline@ as that command (see
-- 'IDE.Web.Claude.claudeHooksFlag'), which forwards the JSON here and then
-- runs the user's own status line.  That is the supported way to see these
-- figures: the transcript format they could also be dug out of is internal.
--
-- In memory only, like "IDE.Web.AgentEvents": a restart forgets them, and the
-- session's next status-line run brings them back.
module IDE.Web.AgentUsage
  ( SessionUsage(..)
  , RateWindow(..)
  , recordStatusLine
  , sessionUsage
  , usageFor
  , usageContextPct
  , usageChip
  , usageLevel
  , usageLines
  , usageSummary
  ) where

import Data.Aeson (Value(..), decodeStrict', parseJSON)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as Key (fromText)
import qualified Data.Aeson.KeyMap as KM (lookup)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (TimeZone, defaultTimeLocale, formatTime, utcToLocalTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Numeric (showFFloat)

import System.IO.Unsafe (unsafePerformIO)

-- | One rate-limit window: how much of it is used (0–100) and when it resets
-- (POSIX seconds).
data RateWindow = RateWindow
  { rwUsedPct  :: Double
  , rwResetsAt :: Double
  } deriving (Eq, Show)

-- | One session's latest status-line figures.  Any of them may be missing:
-- Claude Code leaves some out before the first response, after @\/compact@,
-- and (the rate limits) on plans that have none.
data SessionUsage = SessionUsage
  { suModel        :: Maybe Text    -- ^ the model's display name
  , suEffort       :: Maybe Text    -- ^ reasoning effort level
  , suContextPct   :: Maybe Double  -- ^ context window used, 0–100
  , suContextUsed  :: Maybe Int     -- ^ tokens in the context window now
  , suContextSize  :: Maybe Int     -- ^ the context window's size
  , suCostUsd      :: Maybe Double  -- ^ the session's cost so far
  , suApiMs        :: Maybe Int     -- ^ time spent waiting on the API
  , suLinesAdded   :: Maybe Int
  , suLinesRemoved :: Maybe Int
  , suFiveHour     :: Maybe RateWindow
  , suSevenDay     :: Maybe RateWindow
  , suCacheWarm    :: Maybe Bool    -- ^ prompt cache still within its TTL
  , suCacheExpires :: Maybe Double  -- ^ when it goes cold (POSIX seconds)
  , suCacheHit     :: Maybe Double  -- ^ cache hit ratio, 0–1
  , suAt           :: Double        -- ^ when this was reported
  } deriving (Eq, Show)

{-# NOINLINE usageRef #-}
usageRef :: IORef (Map Text SessionUsage)
usageRef = unsafePerformIO (newIORef M.empty)

sessionUsage :: IO (Map Text SessionUsage)
sessionUsage = readIORef usageRef

usageFor :: Text -> IO (Maybe SessionUsage)
usageFor sid = M.lookup sid <$> readIORef usageRef

-- | How many sessions to remember (the oldest report goes first).
maxSessions :: Int
maxSessions = 256

-- | Record one status-line run's JSON.  O(1)-ish and total: a payload that
-- does not parse, or has no session id, is ignored.
recordStatusLine :: Text -> IO ()
recordStatusLine raw =
  case decodeStrict' (TE.encodeUtf8 raw) of
    Nothing -> return ()
    Just v -> case str ["session_id"] v of
      Nothing  -> return ()
      Just sid | T.null sid -> return ()
      Just sid -> do
        now <- realToFrac <$> getPOSIXTime
        let u = parseUsage now v
        atomicModifyIORef' usageRef $ \m ->
          let m' = M.insert sid u m
          in ( if M.size m' > maxSessions
                 then M.delete (fst (minimumBy (comparing (suAt . snd)) (M.toList m'))) m'
                 else m'
             , () )

parseUsage :: Double -> Value -> SessionUsage
parseUsage now v = SessionUsage
  { suModel        = str ["model", "display_name"] v
  , suEffort       = str ["effort", "level"] v
  , suContextPct   = case num ["context_window", "used_percentage"] v of
      Just p  -> Just p
      -- Null early on and right after /compact: work it out when the counts
      -- are there (and mean something).
      Nothing -> case (used, size) of
        (Just t, Just s) | t > 0, s > 0 -> Just (100 * fromIntegral t / fromIntegral s)
        _ -> Nothing
  , suContextUsed  = used
  , suContextSize  = size
  , suCostUsd      = num ["cost", "total_cost_usd"] v
  , suApiMs        = int ["cost", "total_api_duration_ms"] v
  , suLinesAdded   = int ["cost", "total_lines_added"] v
  , suLinesRemoved = int ["cost", "total_lines_removed"] v
  , suFiveHour     = window "five_hour"
  , suSevenDay     = window "seven_day"
  , suCacheWarm    = case at ["prompt_cache", "warm"] v of
      Just (Bool b) -> Just b
      _             -> Nothing
  , suCacheExpires = num ["prompt_cache", "expires_at"] v
  , suCacheHit     = num ["prompt_cache", "hit_ratio"] v
  , suAt           = now
  }
  where
    used = int ["context_window", "total_input_tokens"] v
    size = int ["context_window", "context_window_size"] v
    window w = RateWindow <$> num ["rate_limits", w, "used_percentage"] v
                          <*> num ["rate_limits", w, "resets_at"] v

at :: [Text] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KM.lookup (Key.fromText k) o >>= at ks
at _ _ = Nothing

str :: [Text] -> Value -> Maybe Text
str p v = at p v >>= \case
  String t -> Just t
  _        -> Nothing

num :: [Text] -> Value -> Maybe Double
num p v = at p v >>= \case
  n@(Number _) -> parseMaybe parseJSON n
  _        -> Nothing

int :: [Text] -> Value -> Maybe Int
int p v = round <$> num p v

--------------------------------------------------------------------------------
-- presentation (shared by the agent cards and `leksah-cmd agent …`)

-- | The context-window figure, rounded.
usageContextPct :: SessionUsage -> Maybe Int
usageContextPct = fmap round . suContextPct

-- | The card's compact figure: @43% · $1.20@ (either half may be missing).
usageChip :: SessionUsage -> Text
usageChip u = T.intercalate " · " $ catMaybes
  [ (\p -> T.pack (show p) <> "%") <$> usageContextPct u
  , dollars <$> suCostUsd u ]

-- | How full the context window is, as a class suffix: @""@, @"high"@ (80%
-- and up — auto-compaction is near) or @"full"@ (90% and up).
usageLevel :: SessionUsage -> Text
usageLevel u = case usageContextPct u of
  Just p | p >= 90 -> "full"
         | p >= 80 -> "high"
  _ -> ""

-- | One line for `agent status`: @ctx 43% · $1.20 · 5h 23% · 7d 41%@.
usageSummary :: Double -> SessionUsage -> Text
usageSummary now u = T.intercalate " · " $ catMaybes
  [ (\p -> "ctx " <> T.pack (show p) <> "%") <$> usageContextPct u
  , dollars <$> suCostUsd u
  , rate "5h" =<< live (suFiveHour u)
  , rate "7d" =<< live (suSevenDay u) ]
  where
    live mw = mw >>= \w -> if rwResetsAt w > now then Just w else Nothing
    rate k w = Just (k <> " " <> pct (rwUsedPct w))

-- | Everything, one fact per line, for a tooltip, the expanded card and
-- `agent events`.  Reset times are local clock times (a weekday too when
-- more than a day away); a window that has already reset is left out, as
-- Claude Code itself drops it.
usageLines :: TimeZone -> Double -> SessionUsage -> [Text]
usageLines tz now u = catMaybes
  [ (\m -> m <> maybe "" (\e -> " · effort " <> e) (suEffort u)) <$> suModel u
  , context
  , (\c -> "cost " <> dollars c
           <> maybe "" (\ms -> " · API time " <> duration (fromIntegral ms / 1000)) (suApiMs u)
           <> lineCounts) <$> suCostUsd u
  , rate "5-hour limit" =<< suFiveHour u
  , rate "7-day limit" =<< suSevenDay u
  , cache ]
  where
    context = case (suContextUsed u, suContextSize u, suContextPct u) of
      (Just t, Just s, mp) | s > 0, t > 0 ->
        Just ("context " <> kTokens t <> " of " <> kTokens s <> " tokens"
              <> maybe "" (\p -> " (" <> pct p <> ")") mp)
      (_, _, Just p) -> Just ("context " <> pct p <> " used")
      _ -> Nothing
    lineCounts = case (suLinesAdded u, suLinesRemoved u) of
      (Just a, Just r) | a + r > 0 ->
        " · +" <> T.pack (show a) <> " −" <> T.pack (show r) <> " lines"
      _ -> ""
    rate k w
      | rwResetsAt w <= now = Nothing
      | otherwise = Just (k <> " " <> pct (rwUsedPct w) <> " used · resets "
                          <> clock (rwResetsAt w))
    cache = case suCacheWarm u of
      Just True -> Just ("prompt cache warm"
                         <> maybe "" (\e -> if e > now then " until " <> clock e else "")
                                  (suCacheExpires u)
                         <> hit)
      Just False -> Just ("prompt cache cold" <> hit)
      Nothing -> Nothing
    hit = maybe "" (\h -> " · hit ratio " <> pct (100 * h)) (suCacheHit u)
    clock t =
      let lt = utcToLocalTime tz (posixSecondsToUTCTime (realToFrac t))
          fmt = if t - now > 20 * 3600 then "%a %H:%M" else "%H:%M"
      in T.pack (formatTime defaultTimeLocale fmt lt)

pct :: Double -> Text
pct p = T.pack (show (round p :: Int)) <> "%"

dollars :: Double -> Text
dollars c = "$" <> T.pack (showFFloat (Just 2) c "")

-- | @86k@, @1.2M@.
kTokens :: Int -> Text
kTokens n
  | n >= 1000000 = T.pack (showFFloat (Just 1) (fromIntegral n / 1e6 :: Double) "") <> "M"
  | n >= 1000    = T.pack (show (round (fromIntegral n / 1e3 :: Double) :: Int)) <> "k"
  | otherwise    = T.pack (show n)

duration :: Double -> Text
duration s
  | s < 90    = T.pack (show (round s :: Int)) <> "s"
  | s < 5400  = T.pack (show (round (s / 60) :: Int)) <> "m"
  | otherwise = let m = round (s / 60) :: Int
                in T.pack (show (m `div` 60)) <> "h " <> T.pack (show (m `mod` 60)) <> "m"
