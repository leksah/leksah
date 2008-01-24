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
    IDEException
,   throwIDE
,   sysMessage
,   MessageLevel(..)
,   handleTopExceptions
) where

import Data.Typeable
import Control.Exception
import System.IO
import Prelude hiding(catch)
import System.Exit

import Panic
import Bag
import ErrUtils
import DynFlags

data IDEException = IDEException String deriving Typeable

throwIDE str = throwDyn (IDEException str)

instance Show IDEException where
  show (IDEException str) = str

sysMessage :: MessageLevel -> String -> IO ()
sysMessage ml str = putStrLn str

data MessageLevel = Silent | Normal | High
    deriving (Eq,Ord,Show)


-- ---------------------------------------------------------------------
-- Exception handling
--

handleTopExceptions =
  handleNormalExceptions . handleIDEExceptions . handleGhcExceptions

handleNormalExceptions inner =
  catch inner (\exception -> do
    hFlush stdout
    case exception of
      AsyncException StackOverflow -> do
        sysMessage Normal "stack overflow: use -g +RTS -K<size> to increase it"
        exitFailure
      ExitException code -> exitWith code
      _other -> do
        sysMessage Normal ("ide: internal IDE error: " ++ show exception)
        exitFailure
  )


handleIDEExceptions inner =
  catchDyn inner (\(e::IDEException) -> do
    sysMessage Normal $ "ide: " ++ (show e)
    exitFailure
  )


handleGhcExceptions inner =
  -- throwIDE messages propagated as exceptions
  let inner2 = catchDyn inner (\dyn -> do
        hFlush stdout
        case dyn of
          PhaseFailed _ code -> exitWith code
          Interrupted -> exitFailure
          _ -> do
            print (dyn :: GhcException)
            exitFailure)
  in
  -- compilation errors: messages with locations attached
  catchDyn inner2 (\dyn -> do
    sysMessage Normal "ide: Compilation error(s):"
    printBagOfErrors defaultDynFlags (unitBag dyn)
    exitFailure
  )
