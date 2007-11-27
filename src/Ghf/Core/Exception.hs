{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Ghf.Core.Exception
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | A Type for Ghf exceptions
--
-------------------------------------------------------------------------------

module Ghf.Core.Exception (
  GhfException,
  throwGhf
) where


import Data.Typeable
import Control.Exception


data GhfException = GhfException String deriving Typeable
throwGhf str = throwDyn (GhfException str)


instance Show GhfException where
  show (GhfException str) = str




