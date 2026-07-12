{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The file-system seam for the web UI's file access.
--
-- Natively every function is a passthrough to the real file system, so this
-- module changes nothing.  Under the GHC JavaScript backend (the in-browser
-- web demo) there is no file system at all: the same functions run against a
-- process-global in-memory tree seeded from @window.leksahDemoFiles@ — a
-- plain @{ "/demo/path": "contents", … }@ object the hosting page defines
-- (docs/website/try).  Keeping the seed in the page means the demo project
-- can be edited without recompiling leksah.
--
-- Only the operations the UI shell actually performs go through here (editor
-- buffer load/save, file-tree listing, workspace read/write); native-only
-- subsystems keep their direct imports and are compiled out or stubbed for
-- the JS build instead.
module IDE.Web.FS
  ( fsReadFile
  , fsWriteFile
  , fsReadFileLazy
  , fsWriteFileLazy
  , fsDoesFileExist
  , fsDoesDirectoryExist
  , fsGetDirectoryContents
#if defined(ghcjs_HOST_OS)
  , fsListFilesRecursive
#endif
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString, fromStrict, toStrict)

#if defined(ghcjs_HOST_OS)

import Data.Aeson (eitherDecodeStrict)
import Data.IORef
       (IORef, newIORef, readIORef, atomicModifyIORef', atomicWriteIORef)
import Data.List (isPrefixOf, nub)
import qualified Data.Map as M (Map, empty, lookup, insert, member, keys, toList)
import qualified Data.Map as Map (fromList)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Text.Encoding (encodeUtf8)
import GHC.IO (unsafePerformIO)
import Language.Javascript.JSaddle (eval, valToText)
import System.FilePath (normalise, dropTrailingPathSeparator)
import System.IO.Error (doesNotExistErrorType, mkIOError)
import Control.Exception (throwIO)

-- The in-memory tree: absolute (normalised) path → contents.  Directories are
-- implicit — a directory exists iff some file lives beneath it.
{-# NOINLINE fsState #-}
fsState :: IORef (Maybe (M.Map FilePath ByteString))
fsState = unsafePerformIO (newIORef Nothing)

-- Seed on first use from window.leksahDemoFiles.  Under the JS backend the
-- jsaddle context IS the page (JSM runs in-process), so this is safe from
-- plain IO before any reflex network exists.  One JSON round-trip keeps the
-- marshalling to a single eval.
getFS :: IO (M.Map FilePath ByteString)
getFS = readIORef fsState >>= \case
  Just m  -> return m
  Nothing -> do
    txt <- valToText =<< eval ("JSON.stringify(window.leksahDemoFiles || {})" :: Text)
    let m = case eitherDecodeStrict (encodeUtf8 txt) of
              Right (kv :: M.Map FilePath Text) ->
                Map.fromList [ (norm k, encodeUtf8 v) | (k, v) <- M.toList kv ]
              Left _ -> M.empty
    atomicWriteIORef fsState (Just m)
    return m

norm :: FilePath -> FilePath
norm = dropTrailingPathSeparator . normalise

notFound :: String -> FilePath -> IO a
notFound op p = throwIO (mkIOError doesNotExistErrorType op Nothing (Just p))

fsReadFile :: FilePath -> IO ByteString
fsReadFile p = getFS >>= \m -> maybe (notFound "readFile" p) return (M.lookup (norm p) m)

fsWriteFile :: FilePath -> ByteString -> IO ()
fsWriteFile p c = do
  _ <- getFS
  atomicModifyIORef' fsState $ \case
    Just m  -> (Just (M.insert (norm p) c m), ())
    Nothing -> (Just (M.insert (norm p) c M.empty), ())

fsReadFileLazy :: FilePath -> IO LBS.ByteString
fsReadFileLazy = fmap LBS.fromStrict . fsReadFile

fsWriteFileLazy :: FilePath -> LBS.ByteString -> IO ()
fsWriteFileLazy p = fsWriteFile p . LBS.toStrict

fsDoesFileExist :: FilePath -> IO Bool
fsDoesFileExist p = M.member (norm p) <$> getFS

fsDoesDirectoryExist :: FilePath -> IO Bool
fsDoesDirectoryExist p = do
  m <- getFS
  let d = norm p <> "/"
  return (any (d `isPrefixOf`) (M.keys m))

-- | Every file path in the tree under the given directory, any depth (JS
-- build only — the callers that would Glob a real file system use this on
-- the mock tree instead).
fsListFilesRecursive :: FilePath -> IO [FilePath]
fsListFilesRecursive p = do
  m <- getFS
  let d = norm p <> "/"
  return [ k | k <- M.keys m, d `isPrefixOf` k ]

-- Immediate children (files and sub-directories) of a directory, names only —
-- matching System.Directory.getDirectoryContents minus the "." and ".."
-- entries the callers filter out anyway.
fsGetDirectoryContents :: FilePath -> IO [FilePath]
fsGetDirectoryContents p = do
  m <- getFS
  let d = norm p <> "/"
      children = nub
        [ takeWhile (/= '/') rest
        | k <- M.keys m
        , d `isPrefixOf` k
        , let rest = drop (length d) k
        , not (null rest)
        ]
  if null children then notFound "getDirectoryContents" p else return children

#else

import qualified Data.ByteString as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy as LBS (readFile, writeFile)
import System.Directory
       (doesFileExist, doesDirectoryExist, getDirectoryContents)

fsReadFile :: FilePath -> IO ByteString
fsReadFile = BS.readFile

fsWriteFile :: FilePath -> ByteString -> IO ()
fsWriteFile = BS.writeFile

fsReadFileLazy :: FilePath -> IO LBS.ByteString
fsReadFileLazy = LBS.readFile

fsWriteFileLazy :: FilePath -> LBS.ByteString -> IO ()
fsWriteFileLazy = LBS.writeFile

fsDoesFileExist :: FilePath -> IO Bool
fsDoesFileExist = doesFileExist

fsDoesDirectoryExist :: FilePath -> IO Bool
fsDoesDirectoryExist = doesDirectoryExist

fsGetDirectoryContents :: FilePath -> IO [FilePath]
fsGetDirectoryContents = getDirectoryContents

#endif
