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
webSessionVersion :: Int
webSessionVersion = 3

data WebSession = WebSession
  { wsVersion :: Int                -- ^ format version
  , wsTabs    :: [TabKey]           -- ^ every open tab, in flipper (MRU) order;
                                    --   the open files and terminals are exactly
                                    --   the 'EditorKey'/'TerminalKey' entries
  , wsVisible :: [(Text, TabKey)]   -- ^ layout area -> the tab visible there
  , wsTall    :: Maybe TallVisibility -- ^ side-pane visibility (show/auto-hide/hide)
  , wsRecentFiles :: Maybe [FilePath] -- ^ recently opened files, most recent first
  , wsWide1   :: Maybe TallVisibility -- ^ bottom-pane visibility (show/auto-hide/hide)
  } deriving (Eq, Show, Generic)

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
emptyWebSession = WebSession webSessionVersion [] [] Nothing Nothing Nothing

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
