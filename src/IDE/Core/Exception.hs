{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.Exception
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | A Type for IDE exceptions
--
-------------------------------------------------------------------------------

module IDE.Core.Exception (
  IDEException,
  throwIDE
) where


import Data.Typeable
import Control.Exception


data IDEException = IDEException String deriving Typeable
throwIDE str = throwDyn (IDEException str)


instance Show IDEException where
  show (IDEException str) = str




