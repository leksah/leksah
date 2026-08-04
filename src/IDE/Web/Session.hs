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
  , viewLeafAllowed
  ) where

import Control.Exception (catch, SomeException)

import Control.Monad (unless)
import Data.Aeson
       (FromJSON(..), ToJSON(..), Value, eitherDecode, object, withObject,
        (.=), (.:), (.:?))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS (readFile, writeFile)
import qualified Data.Map as M (fromList, toAscList)
import Data.Text (Text)
import qualified Data.Text as T (unpack)

import GHC.Generics (Generic)

import IDE.Core.Types
       (AIPaneRef(..), TallVisibility(..), FlipItem(..), LeafId(..),
        LeksahWindow(..), PaneContent(..), PaneKind(..), SplitOrientation(..),
        SplitTree(..))
import IDE.Utils.FileUtils (getConfigFilePathForSave)
import IDE.Web.Events (TabKey(..))

-- | The current on-disk format version.  Bump when the shape changes so an old
-- file is ignored rather than mis-read.
-- v3: terminal tabs are keyed by tmux session id (Text) instead of an Int.
-- v4: multi-window — the wide0 tabs / visibility are per-OS-window ('wsWindows'),
--     ordered by window (index = restored 'WindowId'); positions persist natively
--     via Cocoa's per-window frame autosave, so no frame is stored here.
-- v5: pane overlays retired (the ⌘D views lived in per-SESSION native split
--     layouts, persisted as @leksah_layout tmux session options, not here).
-- v6: leksah windows — tabs are 'LeksahWinKey' entries; 'wsLeksahWindows'
--     stores the SESSIONLESS windows' layouts (session-backed ones live in
--     @leksah_layout v2 on their tmux session).  v5 files still load: their
--     'TerminalKey' tab slots are replaced at seed by the session's migrated
--     leksah-window tabs.
webSessionVersion :: Int
webSessionVersion = 6

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
  , wsLeksahWindows :: Maybe [(Text, LeksahWindow)]
      -- ^ SESSIONLESS leksah windows (pure native views) by id — the ones
      --   with no tmux session to carry an @leksah_layout option.  Optional
      --   so v5 files (which have none) still decode.
  , wsFlipMru :: Maybe [FlipItem]
      -- ^ the flipper MRU (⌘\` order): the shared '_flipMru'.  Optional (absent
      --   in older files → restored empty); entries whose tab/pane no longer
      --   exist are filtered out on rebuild, so a stale list is harmless.  Saved
      --   so a restart / zero-downtime handoff keeps the flip order.
  , wsPaneAI :: Maybe [(AIPaneRef, Text)]
      -- ^ each pane's EXPLICITLY chosen default AI session ('_paneAISession'),
      --   as @(pane, Claude session id)@.  Optional, so files written before it
      --   existed still decode (→ restored empty) — no version bump needed.
      --   Session ids of long-gone sessions are kept deliberately: a closed
      --   default is resumed on next use, not forgotten.
  } deriving (Eq, Show, Generic)

instance ToJSON WebWindowSession
instance FromJSON WebWindowSession
instance ToJSON WebSession
instance FromJSON WebSession
instance ToJSON FlipItem
instance FromJSON FlipItem
-- Generic (tagged-object) instances, like 'FlipItem''s above: 'wsPaneAI' stores
-- these as a list of pairs, so no 'ToJSONKey' is involved.
instance ToJSON AIPaneRef
instance FromJSON AIPaneRef

-- TabKey is defined in IDE.Web.Events and TallVisibility in IDE.Core.Types; we
-- serialize them here (orphan instances, internal use only).  An absent
-- wsTall in an older file decodes as Nothing, so the format stays compatible.
instance ToJSON TabKey
instance FromJSON TabKey
instance ToJSON TallVisibility
instance FromJSON TallVisibility

--
-- Native split layout instances (orphans, internal use only).  Shared by
-- 'wsLeksahWindows' here and the @leksah_layout v2 tmux option codec in
-- "IDE.Web.SplitLayout" (which imports this module).  Kept compact and
-- explicit so the schema is stable against refactors of the Haskell types.
--

unLeafId :: LeafId -> Int
unLeafId (LeafId n) = n

-- | Only these views may live in a native leksah pane.  Preferences,
-- Shortcuts and the fixed side/bottom panes stay ordinary tabs; the codec
-- drops anything else at decode so a hand-edited option can't smuggle one in.
viewLeafAllowed :: TabKey -> Bool
viewLeafAllowed EditorKey{}  = True
viewLeafAllowed GitLogKey{}  = True
viewLeafAllowed ReviewKey{}  = True
viewLeafAllowed BrowserKey{} = True
viewLeafAllowed _            = False

instance ToJSON SplitTree where
  toJSON (SplitLeaf l)          = toJSON ("leaf" :: Text, unLeafId l)
  toJSON (SplitNode SplitH ks)  = toJSON ("h" :: Text, ks)
  toJSON (SplitNode SplitV ks)  = toJSON ("v" :: Text, ks)

instance FromJSON SplitTree where
  parseJSON v = do
    (tag, payload) <- parseJSON v :: Parser (Text, Value)
    case tag of
      "leaf" -> SplitLeaf . LeafId <$> parseJSON payload
      "h"    -> node SplitH payload
      "v"    -> node SplitV payload
      _      -> fail ("unknown split tree tag " <> T.unpack tag)
    where
      node o payload = parseJSON payload >>= \case
        []  -> fail "empty split node"
        ks  -> return (SplitNode o ks)

instance ToJSON PaneContent where
  toJSON (PaneContent k f) = case k of
    PaneTmux w -> object
      [ "kind" .= ("tmux" :: Text), "window" .= w, "font" .= f ]
    PaneView t -> object
      [ "kind" .= ("view" :: Text), "tab" .= t, "font" .= f ]

instance FromJSON PaneContent where
  parseJSON = withObject "PaneContent" $ \o -> do
    kind <- o .: "kind"
    font <- o .:? "font"
    k <- case kind :: Text of
      "tmux" -> PaneTmux <$> o .: "window"
      "view" -> do
        t <- o .: "tab"
        unless (viewLeafAllowed t) $
          fail "view pane must be an editor, git log or browser"
        return (PaneView t)
      _ -> fail ("unknown pane kind " <> T.unpack kind)
    return (PaneContent k font)

instance ToJSON LeksahWindow where
  toJSON lw = object
    [ "session" .= lwSession lw
    , "tree"    .= lwTree lw
    , "panes"   .= [ (unLeafId l, c) | (l, c) <- M.toAscList (lwPanes lw) ]
    , "focused" .= (unLeafId <$> lwFocused lw)
    , "zoomed"  .= (unLeafId <$> lwZoomed lw)
    , "next"    .= lwNext lw
    ]

instance FromJSON LeksahWindow where
  parseJSON = withObject "LeksahWindow" $ \o -> do
    session <- o .:? "session"
    tree    <- o .: "tree"
    panes   <- o .: "panes"
    focused <- o .:? "focused"
    zoomed  <- o .:? "zoomed"
    next    <- o .: "next"
    return LeksahWindow
      { lwSession = session
      , lwTree    = tree
      , lwPanes   = M.fromList [ (LeafId n, c) | (n, c) <- panes ]
      , lwFocused = LeafId <$> focused
      , lwZoomed  = LeafId <$> zoomed
      , lwNext    = next
      }

emptyWebSession :: WebSession
emptyWebSession =
    WebSession webSessionVersion [] [] Nothing Nothing Nothing Nothing

webSessionPath :: IO FilePath
webSessionPath = getConfigFilePathForSave "web-session.json"

-- | Read the saved web session, or 'Nothing' if there is none, it can't be read,
-- or it was written by an incompatible version.
readWebSession :: IO (Maybe WebSession)
readWebSession = (`catch` \(_ :: SomeException) -> return Nothing) $ do
    path <- webSessionPath
    eitherDecode <$> LBS.readFile path >>= \case
        -- v5 decodes with the same record shape ('wsLeksahWindows' is
        -- optional); seed migrates its TerminalKey tabs to leksah windows.
        Right s | wsVersion s `elem` [5, webSessionVersion] -> return (Just s)
        _                                                   -> return Nothing

writeWebSession :: WebSession -> IO ()
writeWebSession s = (`catch` \(_ :: SomeException) -> return ()) $ do
    path <- webSessionPath
    LBS.writeFile path (encodePretty s { wsVersion = webSessionVersion })
