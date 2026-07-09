module Main (main) where

import Data.Default (def)

import System.Environment (getArgs)

import Language.Javascript.JSaddle.WKWebView as JSaddleWK (runHTMLWithBaseURL)

import IDE.Web.Main (newIDE, startJSaddle)
import IDE.Web.MacMenu (installMacMenu, setupMacTitlebar)

main :: IO ()
main = do
  -- Build the native macOS menu bar (its install is scheduled onto the main
  -- thread, so calling it before the app's run loop starts is fine).
  installMacMenu
  -- Make the web toolbar sit in the window's title bar (transparent title bar +
  -- full-size content view).  Like the menu, this is scheduled onto the main
  -- thread and retries until the window exists, so calling it now is fine.
  setupMacTitlebar
  dev <- elem "--develop-leksah" <$> getArgs
  -- Hide the web menu bar (native macOS menu instead) and let the toolbar use
  -- the native title bar.
  newIDE False True dev $ startJSaddle 3367 (\html url -> JSaddleWK.runHTMLWithBaseURL html url def)

