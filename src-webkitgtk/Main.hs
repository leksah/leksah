module Main (main) where

import System.Environment (getArgs)

import IDE.Web.GtkApp (runGtkApp)
import IDE.Web.Instance (leksahPort)
import IDE.Web.Main (newIDE, startJSaddle)

main :: IO ()
main = do
  dev <- elem "--develop-leksah" <$> getArgs
  -- Native GTK menubar (showMenubar=False), no mac titlebar.
  newIDE False False dev $ startJSaddle leksahPort runGtkApp
