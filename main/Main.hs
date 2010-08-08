{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
--  Main function of Leksah, an Haskell IDE written in Haskell
--
---------------------------------------------------------------------------------

module Main (main) where

import IDE.Leksah (leksah)
#ifdef LEKSAH_WITH_YI
import IDE.YiConfig (defaultYiConfig)
#endif

main :: IO ()
main = do
    putStrLn "Using default Yi configuration"
    leksah
#ifdef LEKSAH_WITH_YI
        defaultYiConfig
#endif

