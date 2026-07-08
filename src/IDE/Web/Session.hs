{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Persisted session state for the web UIs (leksah-warp / leksah-wkwebview /
-- leksah-webkitgtk).  This is deliberately separate from the GTK session
-- (@IDE.Session@, @current.lkshs@): the GTK 'SessionState' is built around a
-- GTK-specific @PaneLayout@, so we keep our own small record and our own file
-- and never touch the GTK one.  Same technique though — a versioned record
-- serialized as pretty JSON in the config directory.
--
-- What we save is the part of the UI that isn't already restored some other
-- way: the open editor files, which terminals are open as tabs, and which tab
-- is visible in each layout area.  (Terminal /contents/ are restored by tmux;
-- the toolbar toggles live in the preferences file; the window geometry is
-- persisted natively by the wkwebview front-end.)
module IDE.Web.Session
  ( WebSession(..)
  , WebWindowSession(..)
  , emptyWebSession
  , readWebSession
  , writeWebSession
  ) where

import Control.Exception (catch, SomeException)

import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS (readFile, writeFile)
import Data.Text (Text)

import GHC.Generics (Generic)

import IDE.Core.Types (TallVisibility(..))
import IDE.Utils.FileUtils (getConfigFilePathForSave)
import IDE.Web.Events (TabKey(..))

-- | The current on-disk format version.  Bump when the shape changes so an old
-- file is ignored rather than mis-read.
-- v3: terminal tabs are keyed by tmux session id (Text) instead of an Int.
-- v4: multi-window — the wide0 tabs / visibility are per-OS-window ('wsWindows'),
--     ordered by window (index = restored 'WindowId'); positions persist natively
--     via Cocoa's per-window frame autosave, so no frame is stored here.
webSessionVersion :: Int
webSessionVersion = 4

-- | One OS window's persisted state.  The list position in 'wsWindows' is the
-- window's restored id (0 = the primary window).
data WebWindowSession = WebWindowSession
  { wwsWide0  :: [TabKey]           -- ^ this window's wide0 tabs, MRU order
                                    --   (open editors/terminals = the Editor/
                                    --   TerminalKey entries)
  , wwsActive :: Maybe TabKey       -- ^ the wide0 tab shown in this window
  , wwsTall   :: TallVisibility     -- ^ side-pane visibility
  , wwsWide1  :: TallVisibility     -- ^ bottom-pane visibility
  } deriving (Eq, Show, Generic)

data WebSession = WebSession
  { wsVersion :: Int                -- ^ format version
  , wsWindows :: [WebWindowSession] -- ^ per-OS-window state, ordered by window id
  , wsVisible :: [(Text, TabKey)]   -- ^ shared side/bottom area -> tab visible there
  , wsRecentFiles :: Maybe [FilePath] -- ^ recently opened files, most recent first
  } deriving (Eq, Show, Generic)

instance ToJSON WebWindowSession
instance FromJSON WebWindowSession
instance ToJSON WebSession
instance FromJSON WebSession

-- TabKey is defined in IDE.Web.Events and TallVisibility in IDE.Core.Types; we
-- serialize them here (orphan instances, internal use only).  An absent
-- wsTall in an older file decodes as Nothing, so the format stays compatible.
instance ToJSON TabKey
instance FromJSON TabKey
instance ToJSON TallVisibility
instance FromJSON TallVisibility

emptyWebSession :: WebSession
emptyWebSession = WebSession webSessionVersion [] [] Nothing

webSessionPath :: IO FilePath
webSessionPath = getConfigFilePathForSave "web-session.json"

-- | Read the saved web session, or 'Nothing' if there is none, it can't be read,
-- or it was written by an incompatible version.
readWebSession :: IO (Maybe WebSession)
readWebSession = (`catch` \(_ :: SomeException) -> return Nothing) $ do
    path <- webSessionPath
    eitherDecode <$> LBS.readFile path >>= \case
        Right s | wsVersion s == webSessionVersion -> return (Just s)
        _                                          -> return Nothing

writeWebSession :: WebSession -> IO ()
writeWebSession s = (`catch` \(_ :: SomeException) -> return ()) $ do
    path <- webSessionPath
    LBS.writeFile path (encodePretty s { wsVersion = webSessionVersion })
