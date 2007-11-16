-----------------------------------------------------------------------------
--
-- Module      :  Ghf.Completion
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Module for completion feature
--
-----------------------------------------------------------------------------------

module Ghf.Completion (
    complete

) where

data Completion =   Compl String
                |   ComplRange String String Int

complete :: String -> SymbolTable -> [Completion]
complete


