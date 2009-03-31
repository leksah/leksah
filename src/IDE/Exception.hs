{-# OPTIONS_GHC -XScopedTypeVariables -XDeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Exception
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | A Type for IDE exceptions
-- Taken from Haddock example.
--
-------------------------------------------------------------------------------

module IDE.Exception (
    IDEException
,   throwIDE
,   handleTopExceptions
) where

import Data.Typeable
import System.IO
import Prelude hiding(catch)
import System.Exit

import Debug.Trace
import Control.OldException (Exception(..),throwDyn)
import Control.Exception (catch)
import GHC.IOBase (AsyncException(..))

data IDEException = IDEException String
    deriving Typeable

instance Show IDEException where
  show (IDEException str) = str

throwIDE str = trace str (throwDyn (IDEException str))

sysMessage' :: MessageLevel -> String -> IO ()
sysMessage' ml str = do
    putStrLn str
    hFlush stdout

data MessageLevel = Silent | Normal | High
    deriving (Eq,Ord,Show)


-- ---------------------------------------------------------------------
-- Exception handling
--

handleTopExceptions =
  handleNormalExceptions -- . handleIDEExceptions . handleGhcExceptions

handleNormalExceptions inner =
  catch inner (\exception -> do
    hFlush stdout
    case exception of
      AsyncException StackOverflow -> do
        sysMessage' Normal "stack overflow: use -g +RTS -K<size> to increase it"
        exitFailure
      ExitException code -> exitWith code
      _other -> do
        sysMessage' Normal ("ide: internal IDE error: " ++ show exception)
        exitFailure
  )

{--
handleIDEExceptions inner =
  catchDyn inner (\(e::IDEException) -> do
    sysMessage' Normal $ "ide: " ++ (show e)
    exitFailure
  )


handleGhcExceptions inner =
  -- throwIDE messages propagated as exceptions
  let inner2 = catchDyn inner (\(dyn::GhcException) -> do
        hFlush stdout
        case dyn of
          PhaseFailed _ code -> exitWith code
          Interrupted -> exitFailure
          _ -> do
            print dyn
            exitFailure)
  in
  -- compilation errors: messages with locations attached
  catchDyn inner2 (\dyn -> do
    sysMessage' Normal "ide: Compilation error(s):"
    printBagOfErrors defaultDynFlags (unitBag dyn)
    exitFailure
  )
--}
