module Main (main) where

import Data.Default (def)

import Language.Javascript.JSaddle.WebKitGTK as JSaddleWK (run)

import IDE.Web.Main (newIDE, startJSaddle)

main :: IO ()
main =
  newIDE $ startJSaddle 3367 (\html url -> JSaddleWK.run)

