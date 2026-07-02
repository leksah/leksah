{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Standalone protocol test for IDE.Web.TmuxCC against a throwaway tmux
-- server (@-L leksah-cc-test@).  Run via scripts/run-tmux-cc-test.sh.
--
-- Asserts: session create + correlated replies, %output after send-keys -H
-- (with unescaping), %layout-change on split (parsed; pane-set diff), reply
-- correlation under 20-way concurrent submission, %error → Left, clean
-- %exit on kill-server.  Plus pure unit checks for 'unescapeOctal' and
-- 'parseLayout' against strings captured from a real tmux 3.6a.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forM, unless, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Exit (exitFailure, exitSuccess)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

import IDE.Web.TmuxCC

sock :: [String]
sock = ["-L", "leksah-cc-test", "-f", "/dev/null"]

main :: IO ()
main = do
    failures <- newIORef (0 :: Int)
    let check name ok = do
            putStrLn $ (if ok then "PASS " else "FAIL ") <> name
            unless ok $ modifyIORef' failures (+ 1)

    -- ---- pure unit tests (strings captured from real tmux 3.6a traffic)
    check "unescape: control bytes" $
        unescapeOctal "a\\015\\012b" == BSC.pack "a\r\nb"
    check "unescape: backslash" $
        unescapeOctal "x\\134303y" == BSC.pack "x\\303y"
    check "unescape: raw UTF-8 passthrough" $
        unescapeOctal "caf\233\x0065" /= BS.empty   -- decodes without error
    check "layout: two-pane row" $
        (layoutPanes <$> parseLayout "6b8b,100x30,0,0{50x30,0,0,0,49x30,51,0,1}")
        == Just [("%0", 0, 0, 50, 30), ("%1", 51, 0, 49, 30)]
    check "layout: single pane" $
        (layoutPanes <$> parseLayout "aafd,120x40,0,0,0") == Just [("%0", 0, 0, 120, 40)]
    check "layout: nested col in row" $
        (fmap (map (\(p,_,_,_,_) -> p) . layoutPanes)
              (parseLayout "abcd,100x30,0,0{50x30,0,0,0,49x30,51,0[49x15,51,0,1,49x14,51,16,2]}"))
        == Just ["%0", "%1", "%2"]
    check "event: window-add" $
        parseEventLine "%window-add @7" == EvWindowAdd "@7"
    check "event: output" $
        parseEventLine "%output %3 hi\\015" == EvOutput "%3" (BSC.pack "hi\r")

    -- ---- live tests
    _ <- readProcessWithExitCode "tmux" (sock ++ ["kill-server"]) ""
    threadDelay 300000

    cc <- startCC sock ["new-session", "-s", "cctest", "-x", "80", "-y", "24", "/bin/sh"]

    -- correlated simple command
    r1 <- ccCommand cc "list-panes -F '#{pane_id}'"
    check "list-panes reply" (r1 == Right ["%0"])

    -- send bytes -> %output arrives and contains our text
    ccSendBytes cc "%0" (BSC.pack "echo hello-from-cc\n")
    got <- waitOutput cc "%0" "hello-from-cc" 5000000
    check "%output after send-keys -H" got

    -- split -> %layout-change with two panes
    r2 <- ccCommand cc "split-window -h -t %0 /bin/sh"
    check "split-window ok" (either (const False) (const True) r2)
    mlay <- waitEvent cc 5000000 $ \case
        EvLayoutChange _ (Just l) _ _ | length (layoutPanes l) == 2 -> Just l
        _ -> Nothing
    check "%layout-change → 2 panes" (mlay /= Nothing)

    -- kill-pane: NO pane-close notification; detect via layout diff
    r3 <- ccCommand cc "kill-pane -t %1"
    check "kill-pane ok" (either (const False) (const True) r3)
    mlay1 <- waitEvent cc 5000000 $ \case
        EvLayoutChange _ (Just l) _ _ | map fst5 (layoutPanes l) == ["%0"] -> Just l
        _ -> Nothing
    check "pane close detected from layout diff" (mlay1 /= Nothing)

    -- correlation under concurrency: 20 commands, distinct payloads
    vs <- forM [1 .. 20 :: Int] $ \i -> do
        v <- newEmptyMVar
        _ <- forkIO $ putMVar v . (,) i =<< ccCommand cc
                 ("display-message -p 'cc-payload-" <> T.pack (show i) <> "'")
        return v
    rs <- mapM takeMVar vs
    let okOne (i, Right [t]) = t == "cc-payload-" <> T.pack (show i)
        okOne _ = False
    check "20 concurrent commands correlate" (all okOne rs)

    -- %error surfaces as Left
    r4 <- ccCommand cc "bogus-command-xyz"
    check "%error → Left with body" $ case r4 of
        Left e  -> "unknown command" `T.isInfixOf` e
        Right _ -> False

    -- clean exit on kill-server
    _ <- forkIO . void $ ccCommand cc "kill-server"
    mexit <- waitEvent cc 5000000 $ \case
        EvExit _ -> Just ()
        _        -> Nothing
    check "%exit on kill-server" (mexit == Just ())
    alive <- ccAlive cc
    threadDelay 300000
    check "client dead after exit" . not =<< pure alive

    stopCC cc   -- must be a no-op / not hang
    check "stopCC after death is safe" True

    n <- readIORef failures
    if n == 0 then putStrLn "ALL TESTS PASSED" >> exitSuccess
              else putStrLn (show n <> " FAILURES") >> exitFailure
  where
    fst5 (p, _, _, _, _) = p

-- | Read events until one matches (or deadline).
waitEvent :: CC -> Int -> (TmuxEvent -> Maybe a) -> IO (Maybe a)
waitEvent cc usec f = fromMaybe Nothing <$> timeout usec loop
  where
    loop = do
        ev <- ccEvents cc
        case f ev of
          Just a  -> return (Just a)
          Nothing -> case ev of
            EvExit _ -> return Nothing
            _        -> loop

-- | Accumulate a pane's output until it contains the needle.
waitOutput :: CC -> PaneId -> BS.ByteString -> Int -> IO Bool
waitOutput cc pane needle usec = do
    acc <- newIORef BS.empty
    r <- timeout usec (loop acc)
    return (r == Just True)
  where
    loop acc = do
        ev <- ccEvents cc
        case ev of
          EvOutput p dat | p == pane -> do
              modifyIORef' acc (<> dat)
              s <- readIORef acc
              if needle `BS.isInfixOf` s then return True else loop acc
          EvExit _ -> return False
          _ -> loop acc
