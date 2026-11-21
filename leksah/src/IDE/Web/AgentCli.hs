{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Which command-line agent a session runs in: Claude Code, OpenAI's Codex,
-- or Google's Antigravity CLI (@agy@).
--
-- Everything that shows sessions treats them alike ("IDE.Web.Claude"'s
-- 'IDE.Web.Claude.ClaudeLive' carries the tag, 'cliTag'); the few places
-- where the CLIs differ — how one is started, resumed, driven, and where its
-- transcript is — switch on this.  Dependency-free, so anything can import it.
module IDE.Web.AgentCli
  ( AgentCli(..)
  , cliTag
  , cliFromTag
  , cliLabel
  , cliProgram
  , allClis
  ) where

import Data.Text (Text)

data AgentCli = CliClaude | CliCodex | CliAgy
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | The tag a session carries ('IDE.Web.Claude.clAgent'), and the word
-- @leksah-cmd@ and the MCP tools use for the CLI.
cliTag :: AgentCli -> Text
cliTag = \case
  CliClaude -> "claude"
  CliCodex  -> "codex"
  CliAgy    -> "agy"

-- | 'cliTag' back again ("antigravity" too, the name people say).
cliFromTag :: Text -> Maybe AgentCli
cliFromTag = \case
  "claude"      -> Just CliClaude
  "codex"       -> Just CliCodex
  "agy"         -> Just CliAgy
  "antigravity" -> Just CliAgy
  _             -> Nothing

-- | The name to show people: menu items ("New Antigravity Session"), notes.
cliLabel :: AgentCli -> Text
cliLabel = \case
  CliClaude -> "Claude"
  CliCodex  -> "Codex"
  CliAgy    -> "Antigravity"

-- | The program on PATH.
cliProgram :: AgentCli -> FilePath
cliProgram = \case
  CliClaude -> "claude"
  CliCodex  -> "codex"
  CliAgy    -> "agy"

allClis :: [AgentCli]
allClis = [minBound .. maxBound]
