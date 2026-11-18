{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Every Claude Code session leksah sees gets a NAME — an animal: "Wren",
-- "Otter", "Kestrel"… — so the tab, the status surfaces, the Agents pane and
-- the spoken bell all say something a person can remember, instead of
-- @claude-3@ or a 36-character session id.
--
-- A name belongs to a session for good (it survives a @--resume@), but is only
-- RESERVED while the session has been seen recently: a name nobody has used for
-- a week goes back in the pool.  The pool is the animals of 'agentAnimals':
-- short, everyday, and distinct when spoken (the bell announces them).
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
  , adoptContinuation
  , agentContinuations
  , agentLastSeen
  , holdAgentNames
    -- * Names on other machines
  , NameRec(..)
  , nameSessions
  , namesHeldElsewhere
  , setRemoteNames
  , remoteNameOf
  , noteRemoteName
  ) where

import Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef, atomicModifyIORef')
import qualified Data.Set as S
import Control.Exception (catch, SomeException)
import Control.Monad (mfilter)

import Data.Aeson
       (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), withObject, encode, decode')
import Data.Char (isAlphaNum, isHexDigit, ord, toUpper)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.IO.Unsafe (unsafePerformIO)

import IDE.Web.SharedStore (SharedStore, newSharedStore, peekStore, modifyStore)

-- | The pool, each short and distinct when spoken.  "IDE.Web.AgentFace" draws
-- a face for every one of them — add the face with the name.
agentAnimals :: [Text]
agentAnimals =
  [ "Alpaca", "Badger", "Bear", "Beaver", "Bee", "Bison", "Camel", "Cat"
  , "Cow", "Crab", "Crane", "Deer", "Dingo", "Dodo", "Duck", "Eagle"
  , "Egret", "Elephant", "Ferret", "Flamingo", "Fox", "Frog", "Gecko", "Giraffe"
  , "Goat", "Gorilla", "Hamster", "Hedgehog", "Heron", "Hippo", "Horse", "Hyena"
  , "Ibis", "Jaguar", "Jay", "Kangaroo", "Kiwi", "Koala", "Lemur", "Lion"
  , "Lynx", "Magpie", "Mole", "Monkey", "Moose", "Narwhal", "Newt", "Octopus"
  , "Otter", "Owl", "Panda", "Parrot", "Pelican", "Penguin", "Pig", "Puffin"
  , "Quail", "Rabbit", "Raccoon", "Raven", "Robin", "Seal", "Shark", "Sheep"
  , "Skunk", "Sloth", "Squirrel", "Swan", "Tapir", "Tiger", "Turtle", "Urial"
  , "Vole", "Walrus", "Wolf", "Wombat", "Wren", "Xerus", "Yak", "Zebra" ]

-- | One session's name, and when it was last seen (POSIX seconds) — the
-- reservation clock.
data NameRec = NameRec
  { nrName :: Text
  , nrSeen :: Double
  , nrContinuedIn :: Maybe Text
                     -- ^ this session carried on under another id (Claude
                     --   Code backgrounding it, or a fork-resume that replaces
                     --   it): that id is the same agent, and has its name
  } deriving (Eq, Show)

instance ToJSON NameRec where
  toJSON r = object [ "name" .= nrName r, "seen" .= nrSeen r
                    , "continuedIn" .= nrContinuedIn r ]

instance FromJSON NameRec where
  parseJSON = withObject "NameRec" $ \o ->
    NameRec <$> o .: "name" <*> o .: "seen" <*> o .:? "continuedIn"

-- | How long a name stays reserved for a session that has gone quiet.
reserveFor :: Double
reserveFor = 7 * 24 * 3600

-- | 'nrSeen' is only bumped (and the sidecar only rewritten) when it is this
-- stale — the status poll touches every live session every 3s, and an exact
-- clock is worth nothing here.
seenSlack :: Double
seenSlack = 600

--------------------------------------------------------------------------------
-- the store: shared with every other leksah that shows this machine's agents
-- ("IDE.Web.SharedStore": locked, re-read whenever it changed)

{-# NOINLINE namesStore #-}
namesStore :: SharedStore (Map Text NameRec)
namesStore = unsafePerformIO $
  newSharedStore "agent-names.json" decode' encode M.empty

-- | Update the store from what is in the file now, writing it back only when
-- it changed.
withNames :: (Double -> Map Text NameRec -> (Map Text NameRec, a)) -> IO a
withNames f = do
  now <- realToFrac <$> getPOSIXTime
  (`catch` \(e :: SomeException) -> fallback e) $
    modifyStore namesStore (return . f now)
  where
    -- (an unwritable store must not take naming down with it: answer from
    -- what can be read, unchanged)
    fallback _ = do
      now <- realToFrac <$> getPOSIXTime
      snd . f now <$> peekStore namesStore

-- | Look at the store without changing it.
viewNames :: (Map Text NameRec -> a) -> IO a
viewNames g = g <$> peekStore namesStore

--------------------------------------------------------------------------------
-- naming

-- | The session's name, giving it one if it has none yet.
agentNameFor :: Text -> IO Text
agentNameFor sid = remoteNameOf sid >>= \case
 -- (a session on another machine is named in that machine's store, never here)
 Just n  -> return n
 Nothing -> do
  held <- readIORef heldRef
  elsewhere <- namesHeldElsewhere Nothing
  withNames $ \now m -> let m' = touch held elsewhere now m sid
                        in (m', maybe "" nrName (M.lookup sid m'))

-- | Name every one of these sessions that has no name yet, mark them all as
-- seen, and answer the whole session → name map.  The status poll's call: it
-- is how a session leksah did NOT start (@claude -c@, one typed in a terminal)
-- gets a name.
touchAgentNames :: [Text] -> IO (Map Text Text)
touchAgentNames sids = do
  held <- readIORef heldRef
  elsewhere <- namesHeldElsewhere Nothing
  withNames $ \now m ->
    let m' = foldl' (touch held elsewhere now) m sids in (m', M.map nrName m')

-- | When each session was last seen running (POSIX seconds; accurate to
-- 'seenSlack'), without changing anything.  Sessions that carried on under
-- another id are left out: their own clock stopped when they handed over.
agentLastSeen :: IO (Map Text Double)
agentLastSeen = viewNames $ \m ->
  M.map nrSeen (M.filter (isNothing . nrContinuedIn) m)

-- | Sessions that keep their names whatever their clock says: suspended
-- agents, and agents interrupted by a restart, are still on the team, and
-- one resumed after a week must not come back to find its name handed to a
-- newcomer.  Replaces the previous set (process-local; whoever tracks those
-- states calls it again whenever they change).
holdAgentNames :: [Text] -> IO ()
holdAgentNames = atomicWriteIORef heldRef . S.fromList

{-# NOINLINE heldRef #-}
heldRef :: IORef (S.Set Text)
heldRef = unsafePerformIO (newIORef S.empty)

-- | Is this record's name still taken?
reserved :: S.Set Text -> Double -> Text -> NameRec -> Bool
reserved held now sid r = isNothing (nrContinuedIn r)
  && (now - nrSeen r < reserveFor || sid `S.member` held)

-- | The session → name map, without changing anything — this machine's
-- sessions and, from their own machines' stores, the remote ones this leksah
-- shows ('setRemoteNames').
agentNames :: IO (Map Text Text)
agentNames = do
  local  <- viewNames (M.map nrName)
  remote <- M.unions . M.elems <$> readIORef remoteNamesRef
  return (local `M.union` M.map nrName remote)

--------------------------------------------------------------------------------
-- Names on other machines
--
-- An agent's name lives in the store of the machine it runs on, so every
-- leksah that shows it agrees.  "IDE.Web.RemoteAgents" reads those stores
-- (and names a remote session no-one has named yet, there) and hands the
-- result here, so lookups by name find remote agents too.

-- | Name every one of @sids@ that has no name in @m@ (a machine's store) yet,
-- and mark them seen: 'touchAgentNames' for a store that is not this
-- machine's.
--
-- @held@: sessions whose names stay reserved whatever their clock says (the
-- machine's suspended agents — 'holdAgentNames' for this machine's store).
nameSessions :: Double -> S.Set Text -> S.Set Text -> [Text] -> Map Text NameRec
             -> Map Text NameRec
nameSessions now held elsewhere sids m = foldl' (touch held elsewhere now) m sids

-- | The names held — by a session seen within the reservation — in the stores
-- of every machine this leksah knows of EXCEPT @forHost@'s ('Nothing': this
-- machine's): what a new name for a session there must avoid.  (A leksah on
-- another machine can only avoid the names IT can see, so this narrows clashes
-- rather than ruling them out.)
namesHeldElsewhere :: Maybe Text -> IO (S.Set Text)
namesHeldElsewhere forHost = do
  now <- realToFrac <$> getPOSIXTime
  let held m = S.fromList [ nrName r | (s, r) <- M.toList m, reserved S.empty now s r ]
  local  <- case forHost of
    Nothing -> return S.empty
    Just _  -> viewNames held
  remote <- readIORef remoteNamesRef
  return (local <> S.unions [ held m | (h, m) <- M.toList remote, Just h /= forHost ])

-- | Replace what is known of @host@'s store.
--
-- A name this leksah has just given a session there ('noteRemoteName') and a
-- snapshot taken before that write landed has not got yet is kept, as long as
-- the session is still recent: one snapshot later the store has it.
setRemoteNames :: Text -> Map Text NameRec -> IO ()
setRemoteNames host m = do
  now <- realToFrac <$> getPOSIXTime
  atomicModifyIORef' remoteNamesRef $ \r ->
    let noted = M.filter (\x -> now - nrSeen x < 60) (M.findWithDefault M.empty host r)
    in (M.insert host (m `M.union` noted) r, ())

-- | Remember at once what @host@'s store now calls @sid@ (the next snapshot
-- would bring it, but a launch needs the name before then).
noteRemoteName :: Text -> Text -> Text -> IO ()
noteRemoteName host sid name = do
  now <- realToFrac <$> getPOSIXTime
  atomicModifyIORef' remoteNamesRef $ \r ->
    (M.insertWith M.union host (M.singleton sid (NameRec name now Nothing)) r, ())

-- | A remote session's name, from its machine's store.
remoteNameOf :: Text -> IO (Maybe Text)
remoteNameOf sid = fmap nrName . M.lookup sid . M.unions . M.elems <$> readIORef remoteNamesRef

{-# NOINLINE remoteNamesRef #-}
remoteNamesRef :: IORef (Map Text (Map Text NameRec))
remoteNamesRef = unsafePerformIO (newIORef M.empty)

-- | Mark one session seen, naming it first if it has no name.
touch :: S.Set Text -> S.Set Text -> Double -> Map Text NameRec -> Text -> Map Text NameRec
touch held elsewhere now m sid = case M.lookup sid m of
  Just r | now - nrSeen r < seenSlack -> m
         | otherwise                  -> M.insert sid r { nrSeen = now } m
  Nothing -> M.insert sid (NameRec (pick held elsewhere now m sid) now Nothing) m

-- | A name for a new session: an animal no other session holds (seen within
-- 'reserveFor'), the one whose last use is oldest — so a name that has just
-- been freed isn't immediately handed to someone else, which would be
-- confusing.  Ties (never-used animals) go by a hash of the session id, so the
-- choice doesn't always walk the alphabet.  With every animal held, one gets a
-- number (@Wren-2@).  @elsewhere@: names other machines' stores hold
-- ('namesHeldElsewhere'), avoided too — two agents in one Agents pane must
-- not answer to the same name.
pick :: S.Set Text -> S.Set Text -> Double -> Map Text NameRec -> Text -> Text
pick heldSids elsewhere now m sid = case sortOn order free of
    (n : _) -> n
    []      -> numbered (2 :: Int)
  where
    -- (A session that continued elsewhere has handed its name on.)
    held    = [ nrName r | (s, r) <- M.toList m, reserved heldSids now s r ]
              <> S.toList elsewhere
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
  | otherwise = readIORef heldRef >>= \held -> withNames $ \now m ->
      let clash = [ s | (s, r) <- M.toList m, s /= sid
                      , T.toLower (nrName r) == T.toLower name
                      , reserved held now s r ]
      in case clash of
           (_ : _) -> (m, Left (name <> " is already another agent's name"))
           []      -> (M.insert sid (NameRec name now Nothing) m, Right name)
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
resolveAgentName live arg0 = do
  -- (remote agents answer to their names too; this machine's win a clash)
  remote <- M.unions . M.elems <$> readIORef remoteNamesRef
  viewNames (resolve . (`M.union` remote))
  where
    arg = T.strip arg0
    resolve m = followOn m <$> resolve0 m
    resolve0 m
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
      $ [ (s, r) | (s, r) <- M.toList m, T.toLower (nrName r) == T.toLower arg
                 , isNothing (nrContinuedIn r) ]
    label m s = maybe s (\r -> nrName r <> " (" <> s <> ")") (M.lookup s m)

-- | Where a session carried on, following a chain of continuations (bounded,
-- in case of a cycle in a hand-edited file) — the session itself if it did not.
followOn :: Map Text NameRec -> Text -> Text
followOn m = go (8 :: Int)
  where go 0 s = s
        go k s = maybe s (go (k - 1)) (M.lookup s m >>= nrContinuedIn)

-- | Record that session @old@ carried on as @new@ — the same agent under a new
-- id (Claude Code wrote @continued-in@ at the end of @old@'s transcript).
-- @new@ takes @old@'s name (dropping any it was given meanwhile, which is how
-- a backgrounded "Mole" stops being a new "Monkey"), and @old@ stops holding
-- it.  Answers @old@'s name, if it had one.
adoptContinuation :: Text -> Text -> IO (Maybe Text)
adoptContinuation old new = withNames $ \now m ->
  let oldName = nrName <$> M.lookup old m
      m1 = M.alter (\case
             Just r  -> Just r { nrContinuedIn = Just new }
             Nothing -> Just (NameRec "" now (Just new))) old m
      m2 = case oldName of
             Just nm | not (T.null nm) -> M.insert new (NameRec nm now Nothing) m1
             _                         -> m1
  in (m2, mfilter (not . T.null) oldName)

-- | Every session that carried on elsewhere, as @old → new@ (chains followed to
-- the end).  The live-session readers leave the old ids out: their process is
-- only the host that started the new one.
agentContinuations :: IO (Map Text Text)
agentContinuations = viewNames $ \m ->
  M.fromList [ (s, followOn m s) | (s, r) <- M.toList m, isJust (nrContinuedIn r) ]

-- | The session a Claude pane's @\@leksah_run@ key names — the
-- @\<dir\>#claude#\<id\>@ shape a pinned @--session-id@ launch gets.  A @#fork#@
-- or @#ask#@ key does not name the pane's own session, so 'Nothing'.
sessionOfRunKey :: Text -> Maybe Text
sessionOfRunKey key = do
  sid <- T.stripPrefix "#claude#" (snd (T.breakOn "#claude#" key))
  listToMaybe [ sid | not (T.null sid), T.all (\c -> isHexDigit c || c == '-') sid ]
