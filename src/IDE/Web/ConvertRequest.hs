{-# LANGUAGE OverloadedStrings #-}
-- | A process-global queue of convert-to-pane requests.
--
-- ⌘D (Split Right\/Down) on an editor or git-log tab means "convert this tab
-- to its backing tmux pane, then split": the key lands in
-- 'IDE.Web.TerminalInput.splitActiveTerminal' (an 'IDEAction', outside the
-- reflex network), which drops the request here; 'IDE.Web.Main' drains it
-- into a reflex 'Event' that runs the conversion pipeline (save if dirty,
-- register the pane overlay, close the tab, open the backing session's
-- terminal tab, split).
module IDE.Web.ConvertRequest
  ( requestConvert
  , nextConvertRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

import IDE.Core.Types (TabKey)

{-# NOINLINE convertChan #-}
convertChan :: Chan (TabKey, Bool)
convertChan = unsafePerformIO newChan

-- | Ask to convert @tab@ to its backing tmux pane and split it
-- (@True@ = horizontal, Split Right).
requestConvert :: (TabKey, Bool) -> IO ()
requestConvert = writeChan convertChan

-- | Block until the next conversion request (drained by the reflex bridge).
nextConvertRequest :: IO (TabKey, Bool)
nextConvertRequest = readChan convertChan
