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
  ) where

import Data.Text (Text)

#if defined(ghcjs_HOST_OS)

import Data.Aeson
       (FromJSON(..), eitherDecodeStrict, withObject, (.:))
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

#endif
