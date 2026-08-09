{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The file-system seam for the web UI's file access.
--
-- Natively every function is a passthrough to the real file system — except
-- for @ssh:\/\/HOST\/…@ paths (remote projects, see "IDE.Utils.RemotePath"),
-- which route to one-shot pooled ssh operations in "IDE.Utils.RemoteExec".
-- Under the GHC JavaScript backend (the in-browser web demo) there is no
-- file system at all: the same functions run against a process-global
-- in-memory tree seeded from @window.leksahDemoFiles@ — a plain
-- @{ "/demo/path": "contents", … }@ object the hosting page defines
-- (docs/website/try).  Keeping the seed in the page means the demo project
-- can be edited without recompiling leksah.
--
-- Only the operations the UI shell actually performs go through here (editor
-- buffer load/save, file-tree listing, workspace read/write, project
-- loading); native-only subsystems keep their direct imports and are
-- compiled out or stubbed for the JS build instead.
module IDE.Web.FS
  ( fsReadFile
  , fsWriteFile
  , fsReadFileLazy
  , fsWriteFileLazy
  , fsDoesFileExist
  , fsDoesDirectoryExist
  , fsGetDirectoryContents
  , fsListDirectory
  , fsListFilesRecursive
  , fsCreateDirectoryIfMissing
  , fsEffects
  ) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString, fromStrict, toStrict)

import IDE.Ws.Types (Effects(..), defaultEffects)

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

-- | Immediate children with an is-directory flag (one call — matters for
-- the remote backend, a no-op difference here).
fsListDirectory :: FilePath -> IO [(FilePath, Bool)]
fsListDirectory p = do
  names <- fsGetDirectoryContents p
  mapM (\n -> (,) n <$> fsDoesDirectoryExist (norm p <> "/" <> n)) names

-- | Directories are implicit in the mock tree.
fsCreateDirectoryIfMissing :: FilePath -> IO ()
fsCreateDirectoryIfMissing _ = return ()

#else

import Control.Monad (forM)
import qualified Data.ByteString as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy as LBS (readFile, writeFile)
import Data.Text (Text)
import System.Directory
       (createDirectoryIfMissing, doesFileExist, doesDirectoryExist,
        getDirectoryContents, listDirectory)
import System.FilePath ((</>))

import IDE.Utils.RemoteExec
       (remoteCreateDirectoryIfMissing, remoteDirExists, remoteFileExists,
        remoteListDirectoryAnnotated, remoteListFilesRecursive,
        remoteReadFile, remoteWriteFile)
import IDE.Utils.RemotePath (parseRemotePath, renderRemotePath)

-- Route on the path: ssh:// → RemoteExec (host, host-local path), else the
-- local passthrough.
withRemote :: FilePath -> (Text -> FilePath -> IO a) -> IO a -> IO a
withRemote p remote local = case parseRemotePath p of
  Just (host, rp) -> remote host rp
  Nothing         -> local

fsReadFile :: FilePath -> IO ByteString
fsReadFile p = withRemote p remoteReadFile (BS.readFile p)

fsWriteFile :: FilePath -> ByteString -> IO ()
fsWriteFile p c = withRemote p (\h rp -> remoteWriteFile h rp c) (BS.writeFile p c)

-- Laziness is a local-IO detail; the remote backend is strict either way.
fsReadFileLazy :: FilePath -> IO LBS.ByteString
fsReadFileLazy p =
  withRemote p (\h rp -> LBS.fromStrict <$> remoteReadFile h rp) (LBS.readFile p)

fsWriteFileLazy :: FilePath -> LBS.ByteString -> IO ()
fsWriteFileLazy p c =
  withRemote p (\h rp -> remoteWriteFile h rp (LBS.toStrict c)) (LBS.writeFile p c)

fsDoesFileExist :: FilePath -> IO Bool
fsDoesFileExist p = withRemote p remoteFileExists (doesFileExist p)

fsDoesDirectoryExist :: FilePath -> IO Bool
fsDoesDirectoryExist p = withRemote p remoteDirExists (doesDirectoryExist p)

fsGetDirectoryContents :: FilePath -> IO [FilePath]
fsGetDirectoryContents p =
  withRemote p (\h rp -> map fst <$> remoteListDirectoryAnnotated h rp)
               (getDirectoryContents p)

-- | Immediate children with an is-directory flag — ONE round trip remotely
-- (a per-child doesDirectoryExist would be N+1).
fsListDirectory :: FilePath -> IO [(FilePath, Bool)]
fsListDirectory p = withRemote p remoteListDirectoryAnnotated $ do
  names <- listDirectory p
  forM names $ \n -> (,) n <$> doesDirectoryExist (p </> n)

-- | Every file under the directory, any depth, full paths (remote results
-- keep their @ssh:\/\/host@ prefix).
fsListFilesRecursive :: FilePath -> IO [FilePath]
fsListFilesRecursive p =
  withRemote p
    (\h rp -> map (renderRemotePath h) <$> remoteListFilesRecursive h rp)
    (walk p)
  where
    walk d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        let q = d </> n
        isDir <- doesDirectoryExist q
        if isDir then walk q else return [q]

fsCreateDirectoryIfMissing :: FilePath -> IO ()
fsCreateDirectoryIfMissing p =
  withRemote p remoteCreateDirectoryIfMissing (createDirectoryIfMissing True p)

#endif

-- | The project model's file-access 'Effects', routed through THIS seam
-- instead of straight at @System.Directory@: natively that is the real file
-- system (and @ssh:\/\/host\/…@ roots for free), in the browser demo it is
-- the page-seeded mock tree — which is the only way the demo's workspace
-- and its packages can be detected and enumerated at all.
--
-- Only the read side is overridden; 'eRunTool' keeps 'defaultEffects''
-- behaviour, which already answers 'Nothing' when the program cannot be
-- run (there is no cargo, and no process at all, in the browser).
fsEffects :: Effects
fsEffects = defaultEffects
  { eReadFile  = \p -> either (const Nothing) Just <$> tryIO (fsReadFile p)
  , eListDir   = \p -> either (const []) (map fst) <$> tryIO (fsListDirectory p)
  , eDoesExist = \p -> orFalse . tryIO $ do
      isFile <- fsDoesFileExist p
      if isFile then return True else fsDoesDirectoryExist p
  , eIsDir     = \p -> orFalse (tryIO (fsDoesDirectoryExist p))
  }
 where
  tryIO :: IO a -> IO (Either SomeException a)
  tryIO = try
  orFalse = fmap (either (const False) id)
