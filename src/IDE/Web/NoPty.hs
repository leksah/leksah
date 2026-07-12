-- | Browser stub of the posix-pty surface (GHC JavaScript backend only).
--
-- There are no PTYs in a browser — posix-pty itself does not even compile
-- for the JS target (it imports System.Process.Internals internals the JS
-- process library doesn't provide).  The terminal widgets still compile
-- against this stand-in; actually spawning a shell fails with a plain
-- IOError, which the web demo surfaces instead of a terminal.
module IDE.Web.NoPty
  ( Pty
  , dummyPty
  , spawnWithPty
  , readPty
  , writePty
  , resizePty
  , threadWaitReadPty
  ) where

import Data.ByteString (ByteString)
import System.Process (ProcessHandle)

data Pty = DummyPty

-- | An inert stand-in for widgets that render without a shell (the demo's
-- canned terminals): every operation on it fails with the plain IOError the
-- call sites already swallow ('ignorePtyError' / 'try').
dummyPty :: Pty
dummyPty = DummyPty

noPty :: String -> IO a
noPty op = ioError (userError ("IDE.Web.NoPty." <> op <> ": no PTY in the browser"))

spawnWithPty :: Maybe [(String, String)] -> Bool -> FilePath -> [String]
             -> (Int, Int) -> IO (Pty, ProcessHandle)
spawnWithPty _ _ _ _ _ = noPty "spawnWithPty"

readPty :: Pty -> IO ByteString
readPty _ = noPty "readPty"

writePty :: Pty -> ByteString -> IO ()
writePty _ _ = noPty "writePty"

resizePty :: Pty -> (Int, Int) -> IO ()
resizePty _ _ = noPty "resizePty"

threadWaitReadPty :: Pty -> IO ()
threadWaitReadPty _ = noPty "threadWaitReadPty"
