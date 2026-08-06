{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Tests
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- |
--
-------------------------------------------------------------------------------
module Main (main) where

import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import IDE.ImportTool (parseHiddenModule, HiddenModuleResult(..))
import Control.Monad (unless)
import Distribution.Package
       (PackageName(..), PackageIdentifier(..))
import Distribution.Version (Version(..))
import Graphics.UI.Gtk (timeoutAddFull, initGUI)
import IDE.TextEditor.Tests (testEditors)
import System.Log.Logger
       (errorM, setLevel, rootLoggerName, updateGlobalLogger)
import System.Log (Priority(..))
import Control.Concurrent
       (yield, takeMVar, putMVar, newEmptyMVar, threadDelay, forkIO)
import System.Glib.MainLoop (priorityLow)
import Data.Monoid ((<>))

testString =    "    Could not find module `Graphics.UI.Gtk':\n"
             <> "      It is a member of the hidden package `gtk-0.11.0'.\n"
             <> "      Perhaps you need to add `gtk' to the build-depends in your .cabal file.\n"
             <> "      Use -v to see a list of the files searched for."

prop_parseHiddenModule = parseHiddenModule testString == Just HiddenModuleResult {hiddenModule = "Graphics.UI.Gtk", missingPackage = PackageIdentifier {pkgName = PackageName "gtk", pkgVersion = Version {versionBranch = [0,11,0], versionTags = []}}}

-- At some point the : was removed from this message...
testString2 =   "    Could not find module `Data.Attoparsec.Lazy'\n"
             <> "    It is a member of the hidden package `attoparsec-0.10.2.0'.\n"
             <> "    Perhaps you need to add `attoparsec' to the build-depends in your .cabal file.\n"
             <> "    Use -v to see a list of the files searched for.\n"

prop_parseHiddenModule2 = parseHiddenModule testString2 == Just HiddenModuleResult {hiddenModule = "Data.Attoparsec.Lazy", missingPackage = PackageIdentifier {pkgName = PackageName "attoparsec", pkgVersion = Version {versionBranch = [0,10,2,0], versionTags = []}}}

-- workaround for issue with $quickcheckall
-- see https://hackage.haskell.org/package/QuickCheck-2.4.2/docs/Test-QuickCheck-All.html
return []
main = do
    result <- newEmptyMVar
    forkIO $ do
        updateGlobalLogger rootLoggerName (setLevel DEBUG)
        allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
        initGUI
        timeoutAdd PRIORITY_LOW 10 $ yield >> return True
        editorsOk <- testEditors
        putMVar result (allPass && editorsOk)
    forkIO $ do
        threadDelay 60000000
        errorM "leksah tests" "Test took too long to run"
        putMVar result False
    r <- takeMVar result
    unless r exitFailure
