-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  2007-2009 Jürgen Nicklisch-Franken, Hamish Mackenzie
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

import System.Environment

main = do
    args <- getArgs
    putStrLn $ unwords args

