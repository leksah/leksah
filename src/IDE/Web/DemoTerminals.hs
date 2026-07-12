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
module IDE.Web.DemoTerminals
  ( demoTerminals
  , demoTerminalB64
  ) where

import Data.Text (Text)

#if defined(ghcjs_HOST_OS)

import Data.Aeson
       (FromJSON(..), eitherDecodeStrict, withObject, (.:))
import Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef)
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

#else

demoTerminals :: IO [(Text, Text)]
demoTerminals = return []

demoTerminalB64 :: Text -> IO (Maybe Text)
demoTerminalB64 _ = return Nothing

#endif
