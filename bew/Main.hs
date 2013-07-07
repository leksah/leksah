module Main (
    main
) where

import Graphics.UI.Gtk (postGUIAsync)
import GHCJS.DOM.DOMWindow (domWindowGetDocument)
import GHCJS.DOM.Document (documentGetBody)
import GHCJS.DOM (runWebGUI)

main = do
  runWebGUI $ \ window -> postGUIAsync $ do
    Just doc <- domWindowGetDocument window
    Just body <- documentGetBody doc
    putStrLn "Bewleksah is a version of Leksah that can run in a web browser."
    putStrLn "Nothing working yet."


