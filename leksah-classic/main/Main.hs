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

import System.Environment (getArgs)
import IDE.Leksah (leksah)
import IDE.TextEditor.Yi.Config (defaultYiConfig)

#ifdef LOCALIZATION

import Text.I18N.GetText
import System.Locale.SetLocale

-- | Support for localization
initLocale = do
    setLocale LC_ALL (Just "")
    bindTextDomain __MESSAGE_CATALOG_DOMAIN__ (Just __MESSAGE_CATALOG_DIR__)
    textDomain (Just __MESSAGE_CATALOG_DOMAIN__)

#else

-- | no localization support
initLocale = return ()

#endif

main :: IO ()
main = do
    putStrLn "Using default Yi configuration"
    initLocale
    getArgs >>= leksah defaultYiConfig

