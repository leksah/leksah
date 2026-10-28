{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Every Claude Code session leksah sees gets a NAME — an animal: "Wren",
-- "Otter", "Kestrel"… — so the tab, the status surfaces, the Agents pane and
-- the spoken bell all say something a person can remember, instead of
-- @claude-3@ or a 36-character session id.
--
-- A name belongs to a session for good (it survives a @--resume@), but is only
-- RESERVED while the session has been seen recently: a name nobody has used for
-- a week goes back in the pool.  The pool is the 26 animals of 'agentAnimals',
-- one per initial letter, so no two names in use start with the same sound —
-- which is what makes them work when spoken.
--
-- FS\/JSON only, and deliberately low in the import graph (nothing from
-- "IDE.Web.Claude" or above), so the launcher ('IDE.Web.Claude.claudeCommandLine'),
-- the status poll and the Agents pane can all ask it.
module IDE.Web.AgentNames
  ( agentAnimals
  , agentNameFor
  , touchAgentNames
  , agentNames
  , renameAgent
  , resolveAgentName
  , sessionOfRunKey
  ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Exception (catch, SomeException)
import Control.Monad (when)

import Data.Aeson
       (FromJSON(..), ToJSON(..), object, (.=), (.:), withObject, encode, decode')
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isHexDigit, ord, toUpper)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

import IDE.Paths (sidecarPath)

-- | The pool: one animal per letter, each short and distinct when spoken.
-- "IDE.Web.AgentFace" draws a face for every one of them.
agentAnimals :: [Text]
agentAnimals =
  [ "Alpaca", "Badger", "Crane", "Dingo", "Egret", "Fox", "Gecko", "Heron"
  , "Ibis", "Jay", "Koala", "Lynx", "Mole", "Newt", "Otter", "Puffin"
  , "Quail", "Raven", "Seal", "Tapir", "Urial", "Vole", "Wren", "Xerus"
  , "Yak", "Zebra" ]

-- | One session's name, and when it was last seen (POSIX seconds) — the
-- reservation clock.
data NameRec = NameRec
  { nrName :: Text
  , nrSeen :: Double
  } deriving (Eq, Show)

instance ToJSON NameRec where
  toJSON r = object [ "name" .= nrName r, "seen" .= nrSeen r ]

instance FromJSON NameRec where
  parseJSON = withObject "NameRec" $ \o -> NameRec <$> o .: "name" <*> o .: "seen"

-- | How long a name stays reserved for a session that has gone quiet.
reserveFor :: Double
reserveFor = 7 * 24 * 3600

-- | 'nrSeen' is only bumped (and the sidecar only rewritten) when it is this
-- stale — the status poll touches every live session every 3s, and an exact
-- clock is worth nothing here.
seenSlack :: Double
seenSlack = 600

--------------------------------------------------------------------------------
-- the store (the AgentInfo sidecar pattern: lazily loaded, one MVar)

{-# NOINLINE namesVar #-}
namesVar :: MVar (Maybe (Map Text NameRec))
namesVar = unsafePerformIO (newMVar Nothing)

loadNames :: IO (Map Text NameRec)
loadNames = (`catch` \(_ :: SomeException) -> return M.empty) $ do
  p  <- sidecarPath "agent-names.json"
  ex <- doesFileExist p
  if not ex then return M.empty else fromMaybe M.empty . decode' <$> LBS.readFile p

saveNames :: Map Text NameRec -> IO ()
saveNames m = (`catch` \(_ :: SomeException) -> return ()) $ do
  p <- sidecarPath "agent-names.json"
  LBS.writeFile p (encode m)

-- | Update the store, writing it back only when it changed.
withNames :: (Double -> Map Text NameRec -> (Map Text NameRec, a)) -> IO a
withNames f = do
  now <- realToFrac <$> getPOSIXTime
  modifyMVar namesVar $ \m0 -> do
    m <- maybe loadNames return m0
    let (m', a) = f now m
    when (m' /= m) (saveNames m')
    return (Just m', a)

--------------------------------------------------------------------------------
-- naming

-- | The session's name, giving it one if it has none yet.
agentNameFor :: Text -> IO Text
agentNameFor sid = withNames $ \now m -> let m' = touch now m sid
                                         in (m', maybe "" nrName (M.lookup sid m'))

-- | Name every one of these sessions that has no name yet, mark them all as
-- seen, and answer the whole session → name map.  The status poll's call: it
-- is how a session leksah did NOT start (@claude -c@, one typed in a terminal)
-- gets a name.
touchAgentNames :: [Text] -> IO (Map Text Text)
touchAgentNames sids = withNames $ \now m ->
  let m' = foldl' (touch now) m sids in (m', M.map nrName m')

-- | The session → name map, without changing anything.
agentNames :: IO (Map Text Text)
agentNames = withNames $ \_ m -> (m, M.map nrName m)

-- | Mark one session seen, naming it first if it has no name.
touch :: Double -> Map Text NameRec -> Text -> Map Text NameRec
touch now m sid = case M.lookup sid m of
  Just r | now - nrSeen r < seenSlack -> m
         | otherwise                  -> M.insert sid r { nrSeen = now } m
  Nothing -> M.insert sid (NameRec (pick now m sid) now) m

-- | A name for a new session: an animal no other session holds (seen within
-- 'reserveFor'), the one whose last use is oldest — so a name that has just
-- been freed isn't immediately handed to someone else, which would be
-- confusing.  Ties (never-used animals) go by a hash of the session id, so the
-- choice doesn't always walk the alphabet.  With all 26 held, an animal gets a
-- number (@Wren-2@).
pick :: Double -> Map Text NameRec -> Text -> Text
pick now m sid = case sortOn order free of
    (n : _) -> n
    []      -> numbered (2 :: Int)
  where
    held    = [ nrName r | r <- M.elems m, now - nrSeen r < reserveFor ]
    free    = [ a | a <- rotated, a `notElem` held ]
    lastUse = M.fromListWith max [ (nrName r, nrSeen r) | r <- M.elems m ]
    rotated = let (a, b) = splitAt (hashOf sid `mod` length agentAnimals) agentAnimals
              in b <> a
    order a = (M.findWithDefault 0 a lastUse, indexIn a)
    indexIn a = length (takeWhile (/= a) rotated)
    numbered k = let n = firstOf rotated <> "-" <> T.pack (show k)
                 in if n `elem` held then numbered (k + 1) else n
    firstOf = fromMaybe "Agent" . listToMaybe
    hashOf = T.foldl' (\h c -> (h * 31 + ord c) `mod` 1000003) 7

-- | Give a session a name of the user's choosing.  Refused when the name is
-- empty, too long, or held by ANOTHER session that is still reserved — two
-- agents answering to the same name is exactly what names are for avoiding.
renameAgent :: Text -> Text -> IO (Either Text Text)
renameAgent sid raw
  | T.null name = return (Left "a name needs at least one letter or digit")
  | T.length name > 24 = return (Left "names are at most 24 characters")
  | otherwise = withNames $ \now m ->
      let clash = [ s | (s, r) <- M.toList m, s /= sid
                      , T.toLower (nrName r) == T.toLower name
                      , now - nrSeen r < reserveFor ]
      in case clash of
           (_ : _) -> (m, Left (name <> " is already another agent's name"))
           []      -> (M.insert sid (NameRec name now) m, Right name)
  where
    name = capitalise . T.filter (\c -> isAlphaNum c || c == '-') $ T.strip raw
    capitalise t = case T.uncons t of
      Just (c, r) -> T.cons (toUpper c) r
      Nothing     -> t

-- | Turn what a person or agent typed — a name (any case), a session-id prefix
-- of at least six characters, or a whole id — into a session id.  @live@ are
-- the running sessions: a name held by a running session beats an older
-- holder of the same name.
--
-- 'Left' only when the answer is AMBIGUOUS (a prefix matching several
-- sessions), with the candidates spelled out.  Anything that matches nothing is
-- passed through unchanged, so the callers keep working on ids leksah has no
-- name for (an exited session found only through its transcript).
resolveAgentName :: [Text] -> Text -> IO (Either Text Text)
resolveAgentName live arg0 = withNames $ \_ m -> (m, resolve m)
  where
    arg = T.strip arg0
    resolve m
      | arg `elem` live || arg `M.member` m = Right arg
      | (s : _) <- byName m = Right s
      | T.length arg >= 6, T.all (\c -> isHexDigit c || c == '-') arg =
          case filter (arg `T.isPrefixOf`) known of
            [s] -> Right s
            []  -> Right arg
            ss  -> Left $ "\"" <> arg <> "\" matches several sessions: "
                     <> T.intercalate ", " [ label m s | s <- ss ]
      | otherwise = Right arg
      where known = M.keys (M.fromList [ (s, ()) | s <- live ] <> (() <$ m))
    -- Live holders first, then the most recently seen.
    byName m = map fst . sortOn (\(s, r) -> (s `notElem` live, Down (nrSeen r)))
      $ [ (s, r) | (s, r) <- M.toList m, T.toLower (nrName r) == T.toLower arg ]
    label m s = maybe s (\r -> nrName r <> " (" <> s <> ")") (M.lookup s m)

-- | The session a Claude pane's @\@leksah_run@ key names — the
-- @\<dir\>#claude#\<id\>@ shape a pinned @--session-id@ launch gets.  A @#fork#@
-- or @#ask#@ key does not name the pane's own session, so 'Nothing'.
sessionOfRunKey :: Text -> Maybe Text
sessionOfRunKey key = do
  sid <- T.stripPrefix "#claude#" (snd (T.breakOn "#claude#" key))
  listToMaybe [ sid | not (T.null sid), T.all (\c -> isHexDigit c || c == '-') sid ]
