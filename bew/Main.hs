module Main (
    main
) where

import Graphics.UI.Gtk (postGUIAsync)
import GHCJS.DOM.Document (documentGetBody)
import GHCJS.DOM (runWebGUI, webViewGetDomDocument)

main = do
  runWebGUI $ \ webView -> postGUIAsync $ do
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc
    putStrLn "Bewleksah is a version of Leksah that can run in a web browser."
    putStrLn "Nothing working yet."


