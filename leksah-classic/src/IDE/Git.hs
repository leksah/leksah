{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The ONE git-execution seam: every widget that shells out to git for a
-- (possibly remote) project directory goes through here, so an
-- @ssh:\/\/HOST\/…@ dir transparently becomes @ssh host git -C dir …@ on the
-- pooled connection (see "IDE.Utils.RemoteExec").  'runGitBatch' runs
-- several git commands in ONE round trip — the file tree's three status
-- calls, the Changes pane's status+diff pass.
module IDE.Git
  ( runGit
  , runGitBatch
  , qualifyPath
  ) where

import Data.Text (Text)
import System.Exit (ExitCode(..))

import IDE.Utils.RemotePath (parseRemotePath, renderRemotePath)

#if defined(ghcjs_HOST_OS)

-- No git in the browser demo; callers already treat failure as "no repo".
runGit :: FilePath -> [Text] -> IO (ExitCode, Text, Text)
runGit _ _ = return (ExitFailure 127, "", "git is not available in the browser demo")

runGitBatch :: FilePath -> [[Text]] -> IO [(ExitCode, Text, Text)]
runGitBatch dir = mapM (runGit dir)

#else

import Control.Exception (SomeException, try)
import qualified Data.Text as T
       (breakOn, drop, intercalate, length, pack, unpack, unwords)
import qualified Data.Text.Encoding as TE (decodeUtf8With)
import qualified Data.Text.Encoding.Error as TEE (lenientDecode)
import System.Log.Logger (debugM)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

import IDE.Utils.RemoteExec (RemoteError, newRunNonce, runSsh, shellQuote)

-- | Run one git command with the given (possibly remote) working directory.
-- Local = @git -C dir args@ exactly as the widgets did before; remote = the
-- same over ssh.  ssh-level failures (host down) come back as
-- @(ExitFailure 255, \"\", message)@ so the callers' existing
-- empty-on-failure fallbacks kick in.  Remote stderr is discarded (the
-- callers here only consume stdout + exit code).
runGit :: FilePath -> [Text] -> IO (ExitCode, Text, Text)
runGit dir args = runGitBatch dir [args] >>= \case
    (r : _) -> return r
    []      -> return (ExitFailure 255, "", "no output")

-- | Run several git commands (same directory) in ONE ssh round trip,
-- returning one (exit, stdout, stderr) triple per command, in order.
-- Locally each command just runs in turn.
runGitBatch :: FilePath -> [[Text]] -> IO [(ExitCode, Text, Text)]
runGitBatch _ [] = return []
runGitBatch dir cmds = case parseRemotePath dir of
    Nothing -> mapM localGit cmds
    Just (host, rdir) -> do
        -- A nonce sentinel separates the commands' outputs (nothing git
        -- prints can collide with it).
        sep <- ("LEKSAH-GIT-" <>) <$> newRunNonce
        let one args = "git -C \"$0\" " <> T.unwords (map shellQuote args)
                       <> " 2>/dev/null; printf '\\n%s %s\\n' " <> shellQuote sep <> " $?"
            script = T.intercalate "; " (map one cmds)
        r <- try $ runSsh host script [T.pack rdir] mempty
        case r of
            Left (e :: RemoteError) -> do
                debugM "leksah" $ "runGitBatch: " <> show e
                return $ replicate (length cmds) (ExitFailure 255, "", T.pack (show e))
            Right (_, out, _) ->
                let frames = parseFrames sep (TE.decodeUtf8With TEE.lenientDecode out)
                in return $ take (length cmds)
                     (frames ++ repeat (ExitFailure 255, "", "missing output"))
  where
    localGit args = do
        r <- try $ readProcessWithExitCode "git"
                     ("-C" : dir : map T.unpack args) ""
        return $ case r of
            Left (e :: SomeException) -> (ExitFailure 255, "", T.pack (show e))
            Right (c, o, e') -> (c, T.pack o, T.pack e')
    -- <output1>\n<sep> <code1>\n<output2>\n<sep> <code2>\n…
    parseFrames sep = go
      where
        marker = "\n" <> sep <> " "
        go t = case T.breakOn marker t of
            (_, "")   -> []
            (pre, rest) ->
                let afterSep = T.drop (T.length marker) rest
                    (codeStr, rest') = T.breakOn "\n" afterSep
                    code = maybe (ExitFailure 255) toExit
                             (readMaybe (T.unpack codeStr))
                in (code, pre, "") : go (T.drop 1 rest')
    toExit :: Int -> ExitCode
    toExit 0 = ExitSuccess
    toExit c = ExitFailure c

#endif

-- | Re-prefix @ssh:\/\/host@ onto an absolute path git printed (e.g.
-- @rev-parse --show-toplevel@ output) so it matches the querying dir's
-- namespace; local dirs pass paths through unchanged.
qualifyPath :: FilePath -> FilePath -> FilePath
qualifyPath dir p = case parseRemotePath dir of
    Just (host, _) | take 1 p == "/" -> renderRemotePath host p
    _ -> p
