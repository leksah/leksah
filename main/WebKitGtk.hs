module Main (main) where

import Data.Default (def)

import System.Environment (getArgs)

import Language.Javascript.JSaddle.WebKitGTK as JSaddleWK (run)

import IDE.Web.Main (newIDE, startJSaddle)

main :: IO ()
main = do
  dev <- elem "--develop-leksah" <$> getArgs
  -- Keep the web menu bar: this front end has no native menu (yet).
  newIDE True False dev $ startJSaddle 3367 (\html url -> JSaddleWK.run)

