{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Grab a screen rectangle and drop its PNG path into a terminal pane.
--
-- Shared by @leksah-cmd grab-region@ ('IDE.Web.CmdServer'), the AI ▸ Grab
-- Region menu command ('IDE.Web.Command'), and the front-end orchestration
-- ('IDE.Web.Main').
--
-- Two capture paths, chosen by whether Screen Recording permission is granted
-- ('screenCaptureAllowed'):
--
--   * granted  → @screencapture -i@ (system crosshair, screen-wide) — captures
--     the real screen, so leksah's transparent/snapped holes come out correct;
--   * missing  → an in-leksah drag overlay + WKWebView @takeSnapshot@ of the
--     selected rect (permission-free; see 'IDE.Web.Main').
--
-- Either way the captured file's path is typed into the target tmux pane with
-- @send-keys -l@ (literal, no Enter) so the pane's program (e.g. a claude
-- session) gets it in its prompt.
module IDE.Web.RegionCapture
  ( grabRegionToTarget
  , regionTmuxTarget
  , screenCaptureAllowed
  , nextRegionFile
  , sendPathToTarget
  , sendTextToTarget
  ) where

import Control.Exception (SomeException, try)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getHomeDirectory, doesFileExist, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

-- | Monotonic counter for temp filenames, so successive captures don't
-- overwrite each other before their paths are used.
{-# NOINLINE regionCounter #-}
regionCounter :: IORef Int
regionCounter = unsafePerformIO (newIORef 0)

-- | A fresh @~/.leksah/region-N.png@ path for the next capture.
nextRegionFile :: IO FilePath
nextRegionFile = do
  home <- getHomeDirectory
  n <- atomicModifyIORef' regionCounter (\i -> (i + 1, i + 1))
  return $ home </> ".leksah" </> ("region-" <> show n <> ".png")

-- | Whether @screencapture@ can actually grab the screen (Screen Recording
-- permission for the app that spawns it).  Probes non-interactively with a 1×1
-- capture — no crosshair, no UI — and checks it produced a file.
screenCaptureAllowed :: IO Bool
screenCaptureAllowed = do
  probe <- (</> ".leksah" </> "region-probe.png") <$> getHomeDirectory
  _ <- try (removeFile probe) :: IO (Either SomeException ())
  (_ec, _out, err) <- readProcessWithExitCode "screencapture"
                        ["-x", "-R", "0,0,1,1", probe] ""
  ok <- doesFileExist probe
  _ <- try (removeFile probe) :: IO (Either SomeException ())
  -- A produced file means granted; the tell-tale of denial is the
  -- "could not create image…" message with no file.
  return (ok && not (blocked err))

-- | screencapture's stderr signature for a missing-permission failure.
blocked :: String -> Bool
blocked e = any (`isInfixOf` e)
  ["could not create image", "not authorized", "not permitted", "permission"]

-- | A @session/window/pane@ path → a tmux target.  A raw tmux target (already
-- containing @:@) passes through; @ssh://@ (remote) is unsupported ('Nothing').
regionTmuxTarget :: Text -> Maybe Text
regionTmuxTarget t
  | "ssh://" `T.isPrefixOf` t = Nothing
  | otherwise = case T.splitOn "/" t of
      [s, w, p]               -> Just (s <> ":" <> w <> "." <> p)
      _ | ":" `T.isInfixOf` t -> Just t
        | otherwise           -> Nothing

-- | Type @file@'s path into @target@'s pane (send-keys -l, no Enter).  'False'
-- if the target is unusable or the send failed.
sendPathToTarget :: Text -> FilePath -> IO Bool
sendPathToTarget target file = sendTextToTarget target (T.pack file <> " ")

-- | Type literal @txt@ into @target@'s pane (send-keys -l, no Enter) so the
-- pane's program (e.g. a claude session) gets it in its prompt.  'False' if the
-- target is unusable or the send failed.
sendTextToTarget :: Text -> Text -> IO Bool
sendTextToTarget target txt = case regionTmuxTarget target of
  Nothing  -> return False
  Just tgt -> do
    r <- try (readProcessWithExitCode "tmux"
                ["-L", "leksah", "send-keys", "-t", T.unpack tgt, "-l", T.unpack txt] "")
           :: IO (Either SomeException (ExitCode, String, String))
    return $ case r of Right (ExitSuccess, _, _) -> True; _ -> False

-- | The screencapture-crosshair path (used when permission IS granted):
-- interactively grab a region and send its PNG path to @target@'s pane.
grabRegionToTarget :: Text -> IO Text
grabRegionToTarget target = case regionTmuxTarget target of
  Nothing -> return $ "grab-region: unsupported target " <> target
                    <> " — use a session/window/pane path (e.g. claude/leksah/0)\
                       \ or a tmux target; remote (ssh://) isn't supported.\n"
  Just tgt -> do
    file <- nextRegionFile
    -- -i interactive (drag a region; Esc cancels), -o no window shadow.
    (_ec, _out, err) <- readProcessWithExitCode "screencapture" ["-i", "-o", file] ""
    exists <- doesFileExist file
    if exists
      then do
        sent <- sendPathToTarget target file
        return $ if sent
          then "Sent " <> T.pack file <> " to " <> target <> ".\n"
          else "grab-region: captured " <> T.pack file <> " but couldn't send to "
               <> tgt <> " (is that pane open on the 'leksah' tmux server?).\n"
      else if blocked err
        then return "grab-region: the screen capture was blocked.  Grant Leksah \
                    \Screen Recording permission (System Settings → Privacy & \
                    \Security → Screen Recording), then relaunch leksah.\n"
        else return "grab-region: cancelled (no region selected).\n"
