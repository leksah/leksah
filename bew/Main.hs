module Main (
    main
) where

import Graphics.UI.Gtk (postGUIAsync)
import GHCJS.DOM (runWebGUI, webViewGetDomDocument)

main =
  runWebGUI $ \ webView -> postGUIAsync $ do
    Just doc <- webViewGetDomDocument webView
    putStrLn "Bewleksah is a version of Leksah that can run in a web browser."
    putStrLn "Nothing working yet."


