module Main (main) where

import Data.Default (def)

import Language.Javascript.JSaddle.WKWebView as JSaddleWK (runHTMLWithBaseURL)

import IDE.Web.Main (newIDE, startJSaddle)

main :: IO ()
main =
  newIDE $ startJSaddle 3367 (\html url -> JSaddleWK.runHTMLWithBaseURL html url def)

