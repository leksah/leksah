{-# LANGUAGE OverloadedStrings #-}
-- | Remote (ssh) paths carried inside ordinary 'FilePath's.
--
-- A file on another machine is written @ssh:\/\/HOST\/abs\/path@ and rides
-- through the existing 'FilePath' fields (ProjectKey, ipdCabalFile, editor
-- tab keys, LogRefs, …) unchanged — no new path type threads through the
-- app.  The host is everything between @ssh:\/\/@ and the first @\/@; the
-- rest (including that @\/@) is the absolute path on that host.  Terminal
-- tab keys use @ssh:\/\/HOST#TARGET@ (no @\/@ after the host), so the two
-- namespaces never collide.
--
-- == Path algebra on @ssh:\/\/@ strings
--
-- /Safe/ (pure suffix\/join ops): 'System.FilePath.takeFileName',
-- 'System.FilePath.takeExtension', 'System.FilePath.dropExtension',
-- 'System.FilePath.dropFileName', 'System.FilePath.splitFileName',
-- @dir 'System.FilePath.</>' relative@, and
-- @'System.FilePath.makeRelative' localBase remotePath@ (returns the remote
-- path unchanged).
--
-- /Forbidden/ (mangle the prefix — guard with 'isRemotePath' first):
--
-- * 'System.FilePath.normalise' — collapses @\/\/@, turning @ssh:\/\/h@
--   into @ssh:\/h@.
-- * 'System.Directory.canonicalizePath', 'System.Directory.makeAbsolute',
--   and friends — treat the string as relative and prepend the cwd\/base.
-- * raw 'System.FilePath.isAbsolute' \/ @isRelative@ — say \"relative\",
--   leading callers to join a base; use 'fsIsAbsolute'.
-- * 'System.FilePath.equalFilePath' — normalises both sides.
-- * @makeRelative remoteBase remotePath@ — never strips the prefix; use
--   'remoteMakeRelative'.
module IDE.Utils.RemotePath
  ( remotePrefix
  , isRemotePath
  , parseRemotePath
  , renderRemotePath
  , remoteHost
  , onLocalPart
  , fsIsAbsolute
  , remoteMakeRelative
  , ProjectInput(..)
  , parseProjectInput
  ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import System.FilePath (isAbsolute, makeRelative)

-- | The scheme prefix marking a path as living on another host.
remotePrefix :: String
remotePrefix = "ssh://"

isRemotePath :: FilePath -> Bool
isRemotePath = (remotePrefix `isPrefixOf`)

-- | @ssh:\/\/h\/a\/b@ → @Just (\"h\", \"\/a\/b\")@.  'Nothing' for local
-- paths and for malformed remote ones (empty host, no absolute local part —
-- note @ssh:\/\/HOST#TARGET@ terminal keys fall in the latter bucket, which
-- is what keeps the namespaces separate).
parseRemotePath :: FilePath -> Maybe (Text, FilePath)
parseRemotePath p = do
  rest <- stripPrefix remotePrefix p
  let (host, local) = break (== '/') rest
  if null host || null local
    then Nothing
    else Just (T.pack host, local)

-- | Inverse of 'parseRemotePath'; the local part must be absolute.
renderRemotePath :: Text -> FilePath -> FilePath
renderRemotePath host local = remotePrefix <> T.unpack host <> local

remoteHost :: FilePath -> Maybe Text
remoteHost = fmap fst . parseRemotePath

-- | Apply a path function to the host-local part of a remote path, or to
-- the whole path when it is local.
onLocalPart :: (FilePath -> FilePath) -> FilePath -> FilePath
onLocalPart f p = case parseRemotePath p of
  Just (host, local) -> renderRemotePath host (f local)
  Nothing            -> f p

-- | 'isAbsolute' that treats remote paths as absolute (they are — just not
-- on this machine).  Use instead of raw 'isAbsolute' wherever the path may
-- be remote, so callers never join a local base onto an @ssh:\/\/@ string.
fsIsAbsolute :: FilePath -> Bool
fsIsAbsolute p = isRemotePath p || isAbsolute p

-- | 'makeRelative' that understands remote paths: when both arguments are
-- remote on the same host it relativises their local parts; otherwise it
-- behaves exactly like 'makeRelative' (which already returns a remote path
-- unchanged against a local base).
remoteMakeRelative :: FilePath -> FilePath -> FilePath
remoteMakeRelative base p =
  case (parseRemotePath base, parseRemotePath p) of
    (Just (bh, bl), Just (ph, pl))
      | bh == ph -> makeRelative bl pl
    _            -> makeRelative base p

-- | A user-supplied project\/file location, before @~@ expansion.
data ProjectInput
  = LocalInput FilePath        -- ^ plain local path (may be relative)
  | RemoteInput Text FilePath  -- ^ host, and a path that is @~@, @~\/x@ or @\/x@
  deriving (Show, Eq)

-- | Parse user input: canonical @ssh:\/\/host\/abs@, scp-style @host:path@
-- (path @~@, @~\/…@ or absolute), or a local path.  Rejections (Left) are
-- reserved for things that LOOK remote but are malformed; strings that
-- cannot be scp-style at all (one-char drive prefixes, hosts containing
-- @\/@ or whitespace, empty path) fall back to 'LocalInput' — they are far
-- more likely local paths with a stray colon.
parseProjectInput :: Text -> Either Text ProjectInput
parseProjectInput t
  | isRemotePath s =
      case parseRemotePath s of
        Just (h, l) -> Right (RemoteInput h l)
        Nothing     -> Left ("Malformed remote path (want ssh://HOST/abs/path): " <> t)
  | (host, ':':path) <- break (== ':') s
  , looksLikeHost host
  , looksLikeRemoteTarget path =
      Right (RemoteInput (T.pack host) path)
  | otherwise = Right (LocalInput s)
  where
    s = T.unpack t
    -- at least 2 chars (rules out Windows-style drive letters), no
    -- separators or whitespace
    looksLikeHost h =
      length h > 1 && not (any (\c -> c == '/' || c == '\\' || isSpace c) h)
    looksLikeRemoteTarget p =
      p == "~" || "~/" `isPrefixOf` p || "/" `isPrefixOf` p
