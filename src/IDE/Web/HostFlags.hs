{-# LANGUAGE OverloadedStrings #-}
-- | Process-global facts about the host this leksah front end runs in.
--
-- \"Browser-hosted\" means the UI lives in a browser tab the OS also owns
-- shortcuts for (the @leksah-warp@ front end and the in-browser web demo) —
-- as opposed to the native webview apps, where leksah is the whole window.
-- The flag decides which modifier drives the tab flipper: Cmd+` is the
-- macOS window flipper (and browsers won't reliably deliver it), so
-- browser-hosted builds use Ctrl+` instead.
--
-- Set exactly once, from @newIDE@ (its @showMenubar@ argument is true for
-- precisely the browser-hosted front ends).  A process-global 'IORef'
-- (pattern: "IDE.Web.IDERefStore") because the deepest reader — the tab
-- shortcut badges inside the terminal widgets — is many widget layers away
-- from anywhere the flag is in scope.
module IDE.Web.HostFlags
  ( setBrowserHosted
  , getBrowserHosted
  , flipHintText
  ) where

import Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef)
import Data.Text (Text)
import GHC.IO (unsafePerformIO)

{-# NOINLINE browserHostedRef #-}
browserHostedRef :: IORef Bool
browserHostedRef = unsafePerformIO (newIORef False)

setBrowserHosted :: Bool -> IO ()
setBrowserHosted = atomicWriteIORef browserHostedRef

getBrowserHosted :: IO Bool
getBrowserHosted = readIORef browserHostedRef

-- | The flip-shortcut suffix shown in tab badges: @⌃`@ when browser-hosted
-- (Ctrl drives the flipper there), @⌘`@ natively.
flipHintText :: Bool -> Text
flipHintText browser = if browser then " \8963`" else " \8984`"
