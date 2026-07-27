module Main (main) where

import Data.Default (def)

import System.Environment (getArgs)

import Language.Javascript.JSaddle.WKWebView as JSaddleWK (runHTMLWithBaseURL)

import IDE.Web.Instance (assetPort)
import IDE.Web.Main (newIDE, startJSaddle)
import IDE.Web.MacMenu (installMacMenu, setupMacTitlebar)
import IDE.Web.MacGlue (takeFirstLaunch, resumeApp)
import IDE.Web.GhciMode (ghciMode)
import IDE.Web.NewWindowRequest (requestOpenWindow)
import IDE.Web.ThreadPriority (ThreadPriority(..), raiseCurrentThreadPriority)

main :: IO ()
main = do
  -- This thread runs the Cocoa/WKWebView run loop (the UI) and jsaddle-wkwebview
  -- drives its JS bridge here too; raise it so the UI keeps CPU when background
  -- compilations saturate the machine.
  raiseCurrentThreadPriority Interactive
  -- Build the native macOS menu bar (its install is scheduled onto the main
  -- thread, so calling it before the app's run loop starts is fine).
  installMacMenu
  -- Make the web toolbar sit in the window's title bar (transparent title bar +
  -- full-size content view).  Like the menu, this is scheduled onto the main
  -- thread and retries until the window exists, so calling it now is fine.
  setupMacTitlebar
  dev <- elem "--develop-leksah" <$> getArgs
  -- Hide the web menu bar (native macOS menu instead) and let the toolbar use
  -- the native title bar.  Window 0 comes up via the jsaddle-wkwebview
  -- app-launch path on first launch; on a ghci-mode reload (:reload + fresh
  -- :main, so NSApp already exists) that path's one-shot
  -- applicationWillFinishLaunching never re-fires and the window's webview would
  -- be left unwired — so recreate window 0 through leksah's runtime new-window
  -- path (requestOpenWindow → macAttachWindow attaches a fresh reflex network to
  -- a new WKWebView, exactly like File ▸ New Window) and re-enter the run loop.
  -- newIDE already re-seeded WebWindow 0 (with the restored session tabs), so
  -- wid 0 is reused; the jsm argument is unused on that path.
  newIDE False True dev $ startJSaddle assetPort $ \html url jsm -> do
    first <- takeFirstLaunch
    if ghciMode && not first
      then requestOpenWindow 0 >> resumeApp
      else JSaddleWK.runHTMLWithBaseURL html url def jsm

