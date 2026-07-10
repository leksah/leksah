{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Default (def)

import System.Environment (getArgs)

import Language.Javascript.JSaddle.WebView2
       (run', WebView2Config(..))
import Language.Javascript.JSaddle.WebView2.Internal (jsaddleMainURL)

import IDE.Web.Instance (leksahPort)
import IDE.Web.Main (newIDE, startJSaddle)
import IDE.Web.Win32Menu (installWin32Menu)

main :: IO ()
main = do
  dev <- elem "--develop-leksah" <$> getArgs
  let cfg = def { _webView2Config_title = Just "Leksah"
                , _webView2Config_width = 1200
                , _webView2Config_height = 800
                }
  -- Native Win32 menu bar (showMenubar=False), no mac titlebar.  The run'
  -- callback runs on the UI thread before navigation — exactly when the HWND
  -- exists and the menu can be installed — then navigates to the warp server
  -- (which serves the index page at /).
  newIDE False False dev $ startJSaddle leksahPort $ \_html url jsm ->
    run' cfg $ \wv -> do
      installWin32Menu wv
      jsaddleMainURL url jsm wv
