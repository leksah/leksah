{-# LANGUAGE ForeignFunctionInterface #-}
-- | A ConPTY-backed pseudo-terminal for Windows, presenting the slice of the
-- @posix-pty@ interface that 'IDE.Web.Widget.Terminal' uses.
--
-- On POSIX the terminal widget drives a child shell through @posix-pty@
-- (forkpty).  Windows has no forkpty; instead the Win32 Pseudoconsole (ConPTY,
-- Windows 10 1809+) gives a pair of pipes carrying the child's VT byte stream —
-- the same stream xterm.js renders and accepts.  This module wraps the C shim
-- (@src\/IDE\/Web\/conpty.c@) so the widget can @import@ it in place of
-- @System.Posix.Pty@ and share one code path across platforms (this is exactly
-- how node-pty / VS Code bridge the two worlds).
--
-- Differences from @posix-pty@ that the widget relies on: the blocking read
-- happens inside 'readPty' (a @safe@ FFI call, so it parks only its own
-- capability under the threaded RTS), so 'threadWaitReadPty' is a no-op; and
-- 'readPty' throws an EOF 'IOError' when the child exits, which the widget's
-- reader loop already catches to close the tab.
--
-- Not supported (all POSIX-only in the widget, guarded there): there is no
-- tmux on Windows, so terminals here are plain, non-persistent shells.
module IDE.Web.ConPty
  ( Pty
  , spawnWithPty
  , readPty
  , writePty
  , resizePty
  , threadWaitReadPty
  , closePty
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (ord)
import Foreign.C.Types (CChar, CInt(..), CShort(..), CWchar)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray, pokeArray, withArray0)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import System.IO.Error (ioeSetErrorString, mkIOError, eofErrorType)

-- | An open pseudo-terminal (opaque; mirrors @posix-pty@'s @Pty@).
newtype Pty = Pty (Ptr ConPtyStruct)

data ConPtyStruct

foreign import ccall unsafe "leksah_conpty_spawn"
  c_spawn :: Ptr CWchar -> Ptr CWchar -> Ptr CWchar -> CShort -> CShort
          -> IO (Ptr ConPtyStruct)
-- Blocking pipe I/O: safe so the RTS scheduler keeps running other threads.
foreign import ccall safe "leksah_conpty_read"
  c_read :: Ptr ConPtyStruct -> Ptr CChar -> CInt -> IO CInt
foreign import ccall safe "leksah_conpty_write"
  c_write :: Ptr ConPtyStruct -> Ptr CChar -> CInt -> IO CInt
foreign import ccall unsafe "leksah_conpty_resize"
  c_resize :: Ptr ConPtyStruct -> CShort -> CShort -> IO ()
foreign import ccall unsafe "leksah_conpty_close"
  c_close :: Ptr ConPtyStruct -> IO ()

-- | Spawn a child on a fresh pseudoconsole.  Signature matches the
-- @posix-pty@ call the terminal widget makes: @spawnWithPty env searchPath cmd
-- args (cols, rows)@.  The @searchPath@ flag is ignored (CreateProcessW does
-- its own resolution); the returned @()@ stands in for @posix-pty@'s
-- @ProcessHandle@, which the widget discards.
spawnWithPty :: Maybe [(String, String)]
             -> Bool
             -> String
             -> [String]
             -> (Int, Int)
             -> IO (Pty, ())
spawnWithPty mEnv _searchPath cmd args (cols, rows) =
    withCWStringLen0 (buildCommandLine (cmd : args)) $ \cmdline ->
    withEnvBlock mEnv $ \env -> do
        p <- c_spawn cmdline nullPtr env (fromIntegral cols) (fromIntegral rows)
        when (p == nullPtr) $
            ioError (userError "IDE.Web.ConPty.spawnWithPty: CreatePseudoConsole/CreateProcess failed")
        return (Pty p, ())

-- | Block until the child produces output, then return it.  Throws an EOF
-- 'IOError' when the child has exited (the widget's reader loop catches it).
readPty :: Pty -> IO ByteString
readPty (Pty p) =
    allocaBytes bufSize $ \buf -> do
        n <- c_read p buf (fromIntegral bufSize)
        if n <= 0
            then ioError (eof "readPty")
            else BS.packCStringLen (buf, fromIntegral n)
  where
    bufSize = 65536 :: Int
    eof loc = ioeSetErrorString (mkIOError eofErrorType ("IDE.Web.ConPty." ++ loc) Nothing Nothing)
                                "pseudoconsole closed"

-- | Write bytes to the child's input, as if typed.
writePty :: Pty -> ByteString -> IO ()
writePty (Pty p) bs =
    BSU.unsafeUseAsCStringLen bs $ \(buf, len) -> go buf len
  where
    go _   0    = return ()
    go buf len  = do
        w <- c_write p buf (fromIntegral len)
        when (w < 0) $ ioError (userError "IDE.Web.ConPty.writePty: WriteFile failed")
        let w' = fromIntegral w
        when (w' < len) $ go (buf `plusPtr` w') (len - w')

-- | Resize the pseudoconsole (columns, rows) — the SIGWINCH equivalent.
resizePty :: Pty -> (Int, Int) -> IO ()
resizePty (Pty p) (cols, rows) = c_resize p (fromIntegral cols) (fromIntegral rows)

-- | No-op: the blocking wait happens inside 'readPty' (see the module note).
threadWaitReadPty :: Pty -> IO ()
threadWaitReadPty _ = return ()

-- | Tear the session down, terminating the child.
closePty :: Pty -> IO ()
closePty (Pty p) = c_close p

-- Build a single Windows command line from an argv, quoting per the
-- CommandLineToArgvW rules (spaces/tabs/quotes; backslashes only matter before
-- a quote).  Our uses are simple (a shell path, maybe a flag), but quote
-- properly so a spaced install path still works.
buildCommandLine :: [String] -> String
buildCommandLine = unwords . map quote
  where
    quote s
        | not (null s) && not (any (`elem` (" \t\"" :: String)) s) = s
        | otherwise = '"' : go s
    go [] = "\""
    go ('"' : cs) = '\\' : '"' : go cs
    go ('\\' : cs) =
        let (bs, rest) = span (== '\\') cs
            slashes = '\\' : bs
        in case rest of
            ('"' : _) -> slashes ++ slashes ++ go rest   -- backslashes before a quote: double them
            []        -> slashes ++ slashes ++ "\""       -- ...and before the closing quote
            _         -> slashes ++ go rest
    go (c : cs) = c : go cs

-- Marshal a Haskell String to a NUL-terminated UTF-16 buffer (BMP only, which
-- covers command lines here).
withCWStringLen0 :: String -> (Ptr CWchar -> IO a) -> IO a
withCWStringLen0 s = withArray0 0 (map toUnit s)

-- Build a double-NUL-terminated UTF-16 environment block ("K=V\0...K=V\0\0"),
-- or pass NULL to inherit the parent's.  BMP only (env vars are ASCII here).
withEnvBlock :: Maybe [(String, String)] -> (Ptr CWchar -> IO a) -> IO a
withEnvBlock Nothing    act = act nullPtr
withEnvBlock (Just kvs) act =
    let units = concatMap (\(k, v) -> map toUnit (k ++ "=" ++ v) ++ [0]) kvs ++ [0]
    in allocaArray (length units) $ \p -> pokeArray p units >> act p

toUnit :: Char -> CWchar
toUnit = fromIntegral . ord
