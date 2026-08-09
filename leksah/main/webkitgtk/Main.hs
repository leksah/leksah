module Main (main) where

import System.Environment (getArgs)

import IDE.Web.GtkApp (runGtkApp)
import IDE.Web.Instance (leksahPort)
import IDE.Web.Main (newIDE, startJSaddle)
import IDE.Web.ThreadPriority (ThreadPriority(..), raiseCurrentThreadPriority)

main :: IO ()
main = do
  -- This thread runs the GTK/WebKitGTK main loop (the UI); raise it so the UI
  -- keeps CPU when background compilations saturate the machine.  Best-effort
  -- on Linux (see IDE.Web.ThreadPriority): only bites with a raised RLIMIT_NICE.
  raiseCurrentThreadPriority Interactive
  dev <- elem "--develop-leksah" <$> getArgs
  -- Native GTK menubar (showMenubar=False), no mac titlebar.
  newIDE False False dev $ startJSaddle leksahPort runGtkApp
