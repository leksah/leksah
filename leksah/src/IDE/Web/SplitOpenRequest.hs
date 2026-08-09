{-# LANGUAGE OverloadedStrings #-}
-- | A process-global queue of "open into a split pane" requests.
--
-- Holding @Option@ (⌥) while opening something from the workspace tree — a
-- file, a git-log view, a terminal, a Claude session — means "don't open a new
-- window; split the ACTIVE pane and put this there" (⌥⇧ = split the other way,
-- like ⌘⇧D).  The tree widgets can't reach the split machinery directly (it
-- lives in 'IDE.Web.Main', outside their scope), so — like
-- 'IDE.Web.ConvertRequest' — they drop a request here and 'IDE.Web.Main' drains
-- it into a reflex 'Event' that runs the split-open pipeline.
--
-- Deliberately dependency-light (primitive fields only, no cycle with
-- 'IDE.Web.Claude' etc.): 'IDE.Web.Main' maps each constructor to the real
-- command / overlay.
module IDE.Web.SplitOpenRequest
  ( SplitTarget(..)
  , requestSplitOpen
  , nextSplitOpenRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)
import Data.Text (Text)

-- | What to open into the freshly-split pane.
data SplitTarget
  = STFile FilePath              -- ^ the file's editor, rendered over the pane
  | STGitLog FilePath Text       -- ^ dir, branch — the git-log view, over the pane
  | STTermDir FilePath           -- ^ a shell in this (local) directory
  | STClaudeNew FilePath         -- ^ @claude@ in the project dir
  | STClaudeContinue FilePath    -- ^ @claude -c@
  | STClaudeResume FilePath Text -- ^ @claude --resume <id>@ (dir, session id)
  | STClaudeFork FilePath Text   -- ^ @claude --resume <id> --fork-session@
  | STClaudeAsk FilePath         -- ^ @claude "<explain this file>"@ (the file)
  | STBrowser Int                -- ^ a browser pane by its (pre-minted) id —
                                 --   the URL is already in the pane registry
                                 --   ('IDE.Web.Widget.Browser.rememberUrl')
  | STRunCmd Bool FilePath Text Text Text
      -- ^ a command in a terminal — keep-shell-after?, dir, run-key suffix,
      --   window name, command line (the 'IDE.Web.ReplTmux.runInTerminal'
      --   arguments; e.g. a git action from the workspace git menus)
  deriving (Eq, Show)

{-# NOINLINE splitOpenChan #-}
-- | The 'Bool' is the split direction: 'True' = vertical (⇧ held, Split Down),
-- 'False' = horizontal (side by side, like ⌘D).
splitOpenChan :: Chan (SplitTarget, Bool)
splitOpenChan = unsafePerformIO newChan

-- | Ask to open @target@ into a split of the active pane (@True@ = vertical).
requestSplitOpen :: (SplitTarget, Bool) -> IO ()
requestSplitOpen = writeChan splitOpenChan

-- | Block until the next split-open request (drained by the reflex bridge).
nextSplitOpenRequest :: IO (SplitTarget, Bool)
nextSplitOpenRequest = readChan splitOpenChan
