{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The canned terminal sessions of the in-browser web demo.
--
-- The hosting page (docs/website/try) defines @window.leksahDemoTerminals@ —
-- an ordered array of @{ id, name, b64 }@ objects, each a raw ANSI dump of a
-- captured tmux pane (base64, exactly the payload @LeksahTerm.write@ takes).
-- The demo's terminal tabs render these dumps in real xterms, statically: no
-- PTY, no input.  Same page-seeded pattern as "IDE.Web.FS".
--
-- Natively both functions are inert stubs so shared call sites can import
-- this module unconditionally.
-- Sessions the VISITOR creates ("New local session") have no dump — there is no
-- shell in a browser — but they must still exist as sessions, or no tab opens
-- and the "+" appears to do nothing.  'demoCreateSession' mints one and
-- 'demoCreatedSessions' hands them to the synthesized tmux tree alongside the
-- canned ones; the terminal widget then shows its demo notice, since
-- 'demoTerminalB64' has nothing for them.
module IDE.Web.DemoTerminals
  ( demoTerminals
  , demoTerminalB64
  , demoCreateSession
  , demoCreatedSessions
  , demoKillSession
  , demoAgentInfo
  , demoAgentName
  ) where

import Data.Map (Map)
import Data.Text (Text)

#if defined(ghcjs_HOST_OS)

import Data.Aeson
       (FromJSON(..), eitherDecodeStrict, withObject, (.:), (.:?))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.IORef
       (IORef, newIORef, readIORef, atomicWriteIORef, atomicModifyIORef')
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.IO (unsafePerformIO)
import Language.Javascript.JSaddle (eval, valToText)

data DemoTerm = DemoTerm
  { dtId   :: Text
  , dtName :: Text
  , dtB64  :: Text
  }

instance FromJSON DemoTerm where
  parseJSON = withObject "DemoTerm" $ \o ->
    DemoTerm <$> o .: "id" <*> o .: "name" <*> o .: "b64"

{-# NOINLINE demoState #-}
demoState :: IORef (Maybe [DemoTerm])
demoState = unsafePerformIO (newIORef Nothing)

-- Seed once from the page (JSM is in-process under the JS backend, so this is
-- safe from plain IO); an array (not an object) so the tab order is explicit.
getDemo :: IO [DemoTerm]
getDemo = readIORef demoState >>= \case
  Just ts -> return ts
  Nothing -> do
    txt <- valToText =<< eval ("JSON.stringify(window.leksahDemoTerminals || [])" :: Text)
    ts <- case eitherDecodeStrict (encodeUtf8 txt) of
      Left err -> [] <$ putStrLn ("leksahDemoTerminals did not decode: " <> err)
      Right ts -> return ts
    atomicWriteIORef demoState (Just ts)
    return ts

-- | The canned sessions as @(session id, name)@ in page (= tab) order.
demoTerminals :: IO [(Text, Text)]
demoTerminals = map (\t -> (dtId t, dtName t)) <$> getDemo

-- | The base64 ANSI dump for a session id — pass straight to
-- @LeksahTerm.write@ (it is already in that encoding).
demoTerminalB64 :: Text -> IO (Maybe Text)
demoTerminalB64 tid = lookup tid . map (\t -> (dtId t, dtB64 t)) <$> getDemo

{-# NOINLINE createdState #-}
createdState :: IORef [(Text, Text)]
createdState = unsafePerformIO (newIORef [])

-- | Register a session the visitor just asked for, returning its
-- @(session id, window id)@ the way 'createTerminalSession' does natively.
-- Ids are tmux-shaped — a leading @$@ and no colon, which is what tab keys and
-- pane keys assume (see @paneKey@) — and unique for the life of the page.
demoCreateSession :: Text -> IO (Text, Text)
demoCreateSession name = do
  sid <- atomicModifyIORef' createdState $ \ss ->
    let sid = "$new" <> T.pack (show (length ss + 1))
    in (ss ++ [(sid, name)], sid)
  return (sid, "@" <> sid)

-- | The visitor-created sessions as @(session id, name)@, oldest first.
demoCreatedSessions :: IO [(Text, Text)]
demoCreatedSessions = readIORef createdState

-- | Forget a visitor-created session, given its session id, window id or pane
-- id (@$new1@ \/ @\@$new1@ \/ @%$new1@ — the synthesized tree derives the last
-- two from the first).  Canned sessions are not removable.
demoKillSession :: Text -> IO ()
demoKillSession ident =
  atomicModifyIORef' createdState $ \ss ->
    (filter ((/= sid) . fst) ss, ())
  where sid = if T.isPrefixOf "$" ident then ident else T.drop 1 ident

-- One row of @window.leksahDemoAgents@, children included; only what a
-- terminal pane needs.
data DemoAgentRow = DemoAgentRow Text (Maybe Text) Text Text [DemoAgentRow]

instance FromJSON DemoAgentRow where
  parseJSON = withObject "DemoAgentRow" $ \o -> DemoAgentRow
    <$> o .: "session" <*> o .:? "name" <*> o .: "title" <*> o .: "state"
    <*> (fromMaybe [] <$> o .:? "children")

-- | (name -> (title, state), session -> name)
{-# NOINLINE agentInfoState #-}
agentInfoState :: IORef (Maybe (Map Text (Text, Text), Map Text Text))
agentInfoState = unsafePerformIO (newIORef Nothing)

demoAgents :: IO (Map Text (Text, Text), Map Text Text)
demoAgents = readIORef agentInfoState >>= \case
  Just m -> return m
  Nothing -> do
    txt <- valToText =<< eval ("JSON.stringify(window.leksahDemoAgents || [])" :: Text)
    let flat (DemoAgentRow s n t st kids) =
          [ (s, nm, t, st) | Just nm <- [n] ] ++ concatMap flat kids
    rows <- case eitherDecodeStrict (encodeUtf8 txt) of
      Left err -> [] <$ putStrLn ("leksahDemoAgents did not decode: " <> err)
      Right (rs :: [DemoAgentRow]) -> return (concatMap flat rs)
    let m = ( M.fromList [ (nm, (t, st)) | (_, nm, t, st) <- rows ]
            , M.fromList [ (s, nm) | (s, nm, _, _) <- rows ] )
    atomicWriteIORef agentInfoState (Just m)
    return m

-- | The page's canned agents (@window.leksahDemoAgents@, the Agents pane's
-- data) by name: their title and state.  A canned terminal NAMED after one
-- of them is that agent's pane, so its tab wears the agent's name and face.
demoAgentInfo :: IO (Map Text (Text, Text))
demoAgentInfo = fst <$> demoAgents

-- | The name of the canned agent with this session id (the Agents pane's
-- rows carry session ids; their panes are found by name).
demoAgentName :: Text -> IO (Maybe Text)
demoAgentName sid = M.lookup sid . snd <$> demoAgents

#else

demoTerminals :: IO [(Text, Text)]
demoTerminals = return []

demoTerminalB64 :: Text -> IO (Maybe Text)
demoTerminalB64 _ = return Nothing

demoCreateSession :: Text -> IO (Text, Text)
demoCreateSession name = return (name, name)

demoCreatedSessions :: IO [(Text, Text)]
demoCreatedSessions = return []

demoKillSession :: Text -> IO ()
demoKillSession _ = return ()

demoAgentInfo :: IO (Map Text (Text, Text))
demoAgentInfo = return mempty

demoAgentName :: Text -> IO (Maybe Text)
demoAgentName _ = return Nothing

#endif
