{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.InfoPane
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The GUI stuff for infos
--
-------------------------------------------------------------------------------

module IDE.InfoPane (
    setInfos
) where

import IDE.Core.State


setInfos :: [IdentifierDescr] -> IDEM ()

