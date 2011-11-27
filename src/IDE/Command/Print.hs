-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.Print
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | TODO this module should be moved to some other place, maybe utils
--
-----------------------------------------------------------------------------

module IDE.Command.Print (
    print
    , PrintError (..)
) where

import System.Process
import System.Exit
import Prelude hiding (print)
import IDE.Core.Types
import IDE.Core.State

data PrintError = PrintError {
    exitCode :: Int
    , stderr :: String
    , printCmd :: FilePath
    } deriving (Read,Show)

printCommand = "lpr"

print :: FilePath -> IO (Either PrintError String)
print fileName = do
        (ec, out, err) <- readProcessWithExitCode printCommand [fileName] ""
        case ec of
                ExitSuccess -> return $ Right out
                ExitFailure i -> return $ Left $ PrintError i err printCommand


