-----------------------------------------------------------------------------
--
-- Module      :  Ghf.Collector
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | This modulle collects information for packages to make navigation and information
--  accessible to ghf
--
-------------------------------------------------------------------------------

module Ghf.Collector (
    collectInstalled
,   collectUninstalled
) where

import HscTypes

collectInstalled :: Session -> String -> Bool -> IO()
collectUninstalled :: Session -> String -> FilePath -> IO ()
