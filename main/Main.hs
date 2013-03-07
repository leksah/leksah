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
import IDE.YiConfig (defaultYiConfig)

import Text.I18N.GetText
import System.Locale.SetLocale

main :: IO ()
main = do
    putStrLn "Using default Yi configuration"
    setLocale LC_ALL (Just "")
    bindTextDomain __MESSAGE_CATALOG_DOMAIN__ (Just __MESSAGE_CATALOG_DIR__)
    textDomain (Just __MESSAGE_CATALOG_DOMAIN__)
    leksah defaultYiConfig

