-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | Windows systems do not often have a real echo executable (so --with-ghc=echo fails)
--
-----------------------------------------------------------------------------

module Main (
main
) where

import System.Environment (getArgs)
import IDE.Utils.VersionUtils (getHaddockVersion, getDefaultGhcVersion, getGhcInfo)
import qualified Data.Text as T (unpack)
import Control.Applicative
import Prelude

main :: IO ()
main = do
    args <- getArgs
--    appendFile "/Users/hamish/lecho.log" $ show args ++ "/n"
    if elem  "--version" args
        then putStrLn =<< T.unpack <$> getHaddockVersion
        else if elem  "--ghc-version" args
                then putStrLn =<<  getDefaultGhcVersion
                else if elem  "--info" args
                        then putStrLn =<< T.unpack <$> (getGhcInfo =<< getDefaultGhcVersion)
                        else if elem  "--numeric-version" args
                                then putStrLn =<<  getDefaultGhcVersion
                                else putStrLn $ unwords args

