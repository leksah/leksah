{-# LANGUAGE CPP, TemplateHaskell #-}
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
--   * Package -> New
--   * When asked for a root folder for your package select a new folder
--     with the desired name of your package

-- This is the "Main" module and it exports a "main" function
module Main (
    main
) where

-- Next we are importing some things from other modules.
-- Leksah can normally addd these imports for you, just
-- press Ctrl+R (OS X Command+R)
import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)

-- Simple function to create a hello message.
hello s = "Hello " ++ s

-- QuickCheck is a great tool for writing tests.
-- The following tells QuickCheck that if you strip "Hello "
-- from the start of hello s you will be left with s (for any s).
-- QuickCheck will create the test data needed to run this test.
prop_hello s = stripPrefix "Hello " (hello s) == Just s

-- exeMain : Executable Entry Point
-- --------------------------------
-- Here is the entry point for the leksah-welcome executable
--
-- To run it
--   * Select Leksah menu item Package -> Run (or the cogs on the toolbar)
--   * Select "exeMain" and press Ctrl+Enter to run them in ghci
--   * Run "leksah-wellcome" from the command line
exeMain = do
    putStrLn (hello "World")

-- testMain : Unit Testing Entry Point
-- -----------------------------------
-- This is the main function uses by the cabal test
--
-- To run these tests
--   * Select Leksah menu item Package -> Test
--   * Select the tick icon on the Leksa toolbar (to enable "cabal test" during builds)
--   * Select "testMain" and press Ctrl+Enter to run them in ghci
--   * Run "cabal test" from the command line in the package directory
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

