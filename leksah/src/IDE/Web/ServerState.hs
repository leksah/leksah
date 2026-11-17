{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: Apache-2.0
-- | What the Workspace tree's server nodes show about a remote host, and the
-- menu every server node has (the Terminals tree's and the Workspace
-- tree's), so the two can't drift apart.
module IDE.Web.ServerState
  ( ServerState(..)
  , serverMenuItems
  ) where

import Data.Text (Text)

import IDE.Web.Events (TerminalsEvents(..))

-- | One remote host, as this leksah last saw it.
data ServerState = ServerState
  { svReachable :: Maybe Bool  -- ^ answered its last look ('Nothing': not
                               --   looked at yet)
  , svLeksah    :: Maybe Bool  -- ^ a leksah is running there ('Nothing':
                               --   unknown — only hosts whose agents this
                               --   leksah shares are watched for one)
  , svHome      :: Maybe Text  -- ^ its home directory, once known
  , svStartup   :: Bool        -- ^ opened at startup (remote.startup)
  , svWake      :: Bool        -- ^ Wake-on-LAN is set up for it
  } deriving (Eq, Show)

-- | A server node's menu, each item a function of (opened at startup,
-- Wake-on-LAN set up) — a fixed list, so a caller maps each over one
-- Dynamic of the pair.
serverMenuItems :: Text -> [(Bool, Bool) -> (Text, TerminalsEvents)]
serverMenuItems host =
  [ const ("Open Server Connection", SelectRemoteHost host)
  , const ("New Session",            NewRemoteTerminal host)
    -- Open its default session as a leksah window at startup.
  , \(on, _) -> ((if on then "\10003 " else "") <> "Open at Startup", ToggleRemoteStartup host)
    -- Wake the machine now (or set Wake-on-LAN up for it).
  , \(_, on) -> (if on then "Wake on LAN" else "Wake on LAN\8230 (set up)", WakeRemoteHost host)
  ]
