module Main (
    main
) where

import Graphics.UI.Gtk (postGUIAsync)
import Graphics.UI.Gtk.WebKit.WebView (webViewGetDomDocument)
import Graphics.UI.Gtk.WebKit.DOM.Document (documentGetBody)
import Graphics.UI.Gtk.WebKit.GHCJS (runWebGUI)

main = do
  runWebGUI $ \ webView -> postGUIAsync $ do
    doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc
    putStrLn "Bewleksah is a version of Leksah that can run in a web browser."
    putStrLn "Nothing working yet."


