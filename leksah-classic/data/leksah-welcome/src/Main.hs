-- Welcome to Leksah.  This is a quick sample package for you to
-- try things out with.  We hope it will be useful for those new
-- to Haskell or just new to Leksah.

-- If you are new to haskell then here are some great sites to visit
-- http://learnyouahaskell.com/
-- http://tryhaskell.org/
-- http://book.realworldhaskell.org/

-- To build this package use
--   * Just make a change while background build is activated
--   * Ctrl+B (OS X Command+B)
--   * Package -> Build

-- When you are ready to create your own workspace and package.
--   * File -> New -> Package

-- Next we are importing some things from other modules.
-- Leksah can normally add these imports for you, just
-- press Ctrl+R (OS X Command+R)

import Data.Monoid ((<>))

-- This strange looking comment adds code only needed when running the
-- doctest tests embedded in the comments
-- $setup
-- >>> import Data.List (stripPrefix)

-- | Simple function to create a hello message.
--
-- The following tells doctest & QuickCheck that if you strip "Hello "
-- from the start of `hello s` you will be left with `s` (for any `s`).
-- QuickCheck will create the test data needed to run this test.
--
-- To automatically run the tests when the package is built, click on
-- the green tick on the tool leksah tool bar.
--
-- To see how this is configured take a look in leksah-welecome.cabal
-- (you can open it with Package -> Edit).
--
-- The code that runs the tests is in the file test/Main.hs.
--
-- prop> stripPrefix "Hello " (hello s) == Just s
hello :: String -> String
hello s = "Hello " <> s

-- | Executable Entry Point
--
-- Here is the entry point for the leksah-welcome executable
--
-- To run it
--   * Select Leksah menu item Package -> Run (or the cogs on the toolbar)
--   * Select `main` and press Ctrl+Enter to run them in ghci
--   * Run "leksah-wellcome" from the command line
main :: IO ()
main = putStrLn (hello "World!")

