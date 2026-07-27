{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | @leksah-cmd@: a tiny command line client that talks to a running leksah web
-- UI over the Unix domain socket at @~/.leksah/cmd.sock@ (server side in
-- 'IDE.Web.CmdServer').
--
-- Usage:
--   leksah-cmd restart [--no-rebuild] [--wait]  relaunch via the wrapper
--   leksah-cmd rebuild-self [--no-restart] [--use-cabal]  rebuild in place
--   leksah-cmd wait-ready [SECONDS]    block until the UI answers (default 180)
--   leksah-cmd editor open FILE...     open files in the editor (alias: cm)
--   leksah-cmd project open FILE...    add project files to the workspace
--   leksah-cmd js eval CODE            evaluate JS in the running leksah
--   leksah-cmd js eval -f FILE         evaluate JS read from FILE
--   leksah-cmd js eval -               evaluate JS read from stdin
--   leksah-cmd ping                    print "ok" if the UI is up
--   leksah-cmd help                    show this help
--
-- The wire format: the client's working directory followed by its argv, each
-- field NUL-separated; the client then half-closes its write side and prints the
-- single text reply.  (Sending the cwd lets the server resolve relative paths
-- against the shell that ran @leksah-cmd@, not against leksah's own directory.)
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, catch, try)
import Control.Monad (unless, when, void)

import qualified Data.ByteString as BS
import Data.Char (isSpace, isDigit)
import Data.List (isPrefixOf, isInfixOf, dropWhileEnd, sortBy, stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import System.Directory
       (getCurrentDirectory, getHomeDirectory, doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, stdout, hFlush)
import System.Posix.Process (getProcessID)
import System.Process (readProcess)
import Text.Read (readMaybe)

import Network.Socket
       (Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix),
        socket, connect, close, defaultProtocol,
        ShutdownCmd(ShutdownSend), shutdown)
import Network.Socket.ByteString (recv, sendAll)

-- | The control socket to talk to.  Must match 'IDE.Web.Instance.cmdSocketPath'
-- on the server side — replicated here (not imported) because leksah-cmd is a
-- standalone exe with no leksah-library dependency: @~/.leksah/cmd.sock@ for
-- the default instance, @~/.leksah/cmd-\<port\>.sock@ under a non-default
-- @LEKSAH_PORT@, so `leksah-cmd` reaches the instance sharing its environment.
cmdSocketPath :: IO FilePath
cmdSocketPath = do
  home <- getHomeDirectory
  tag  <- instanceTag
  return $ home </> ".leksah" </> ("cmd" <> tag <> ".sock")

-- | @""@ for the default port (3367), else @-\<port\>@; see 'cmdSocketPath'.
instanceTag :: IO String
instanceTag = do
  m <- lookupEnv "LEKSAH_PORT"
  return $ case m >>= readMaybe of
    Just (p :: Int) | p > 0 && p /= 3367 -> "-" <> show p
    _                                    -> ""

usage :: Text
usage = T.unlines
  [ "leksah-cmd — control a running leksah web UI"
  , ""
  , "Usage:"
  , "  leksah-cmd restart [--no-rebuild] [--wait]"
  , "                                     relaunch via the wrapper (--no-rebuild skips the"
  , "                                     build; --wait blocks until the new UI answers)"
  , "  leksah-cmd rebuild-self [--no-restart] [--use-cabal]"
  , "                                     rebuild via the IDE build system (errors in the UI);"
  , "                                     --use-cabal = failsafe: direct cabal, output streamed"
  , "  leksah-cmd wait-ready [SECONDS]    block until the UI answers a ping (default 180)"
  , "  leksah-cmd editor open FILE...     open files in the editor (alias: cm)"
  , "  leksah-cmd project open FILE...    add project files to the workspace"
  , "  leksah-cmd cc-connect HOST         terminal tab on HOST's tmux (ssh, control mode)"
  , "  leksah-cmd open-browser URL        open the default browser snapped to this pane"
  , "  leksah-cmd js eval CODE            evaluate JS in the running leksah"
  , "  leksah-cmd js eval -f FILE         evaluate JS read from FILE (no shell escaping)"
  , "  leksah-cmd js eval -               evaluate JS read from stdin"
  , "  leksah-cmd hs eval CODE            (leksah.sh --ghci) evaluate Haskell at the ghci"
  , "                                     prompt: suspends the UI, evals, resumes"
  , "  leksah-cmd hs eval -f FILE | -     the same, code from FILE / stdin"
  , "  leksah-cmd ping                    print \"ok\" if the UI is up (silent-ish, exit 0/1)"
  , "  leksah-cmd screenshot FILE         capture the UI to a PNG (wkwebview/webkitgtk)"
  , "  leksah-cmd grab-region [TARGET]    select a screen region; type its PNG path into"
  , "                                     a terminal pane (default: regionCaptureTarget pref;"
  , "                                     TARGET is a session/window/pane path)"
  , "  leksah-cmd help                    show this help"
  ]

main :: IO ()
main = getArgs >>= \case
  []            -> T.putStr usage
  ("help":_)    -> T.putStr usage
  ("--help":_)  -> T.putStr usage
  ("-h":_)      -> T.putStr usage
  -- open-browser also needs the tmux pane it was run in ($TMUX_PANE); pass it
  -- along so leksah can snap the browser to that pane.
  ("open-browser":rest) -> do
    pane <- maybe "" id <$> lookupEnv "TMUX_PANE"
    send ("open-browser" : pane : rest)

  -- ping: exit 0 if the UI answered, 1 otherwise (for scripting).
  ("ping":_) -> pingOnce >>= \ok ->
    if ok then putStrLn "ok" else hPutStrLn stderr "leksah: not responding" >> exitFailure

  -- wait-ready [SECONDS]: poll until the UI answers (used after a relaunch).
  ("wait-ready":rest) -> do
    let secs = maybe 180 id (readMaybe =<< listToMaybeStr rest)
    ok <- waitUiReady secs
    if ok then putStrLn "leksah is ready."
          else hPutStrLn stderr "leksah-cmd: timed out waiting for the UI" >> exitFailure

  -- restart [--wait]: relaunch; with --wait, block until the NEW instance is up
  -- (first wait for the old one to go down, so we don't match it before it exits).
  -- Against a ghci instance (leksah.sh --ghci) a restart is :reload + :main at
  -- the repl prompt (--no-rebuild skips the :reload).
  ("restart":rest) -> instanceMode >>= \case
    Just "ghci" -> ghciReloadRestart (not ("--no-rebuild" `elem` rest))
    _ | "--wait" `elem` rest -> do
          send ("restart" : filter (/= "--wait") rest)
          putStr "Waiting for leksah to relaunch… "; hFlush stdout
          ok <- waitDownThenUp 180
          putStrLn (if ok then "ready." else "TIMED OUT.")
          unless ok exitFailure
      | otherwise -> send ("restart" : rest)

  -- rebuild-self: against a ghci instance this IS the point of --ghci — the
  -- whole rebuild becomes :reload + :main at the repl prompt (seconds).
  ("rebuild-self":rest) -> instanceMode >>= \case
    Just "ghci" -> do
      when ("--no-restart" `elem` rest) $ hPutStrLn stderr
        "leksah-cmd: ghci mode can't reload without restarting — doing :reload + :main."
      ghciReloadRestart True
    _ -> send ("rebuild-self" : rest)

  -- js eval CODE | -f FILE | -   (the last two avoid shell-escaping the JS).
  ("js":"eval":rest) | not (null rest) -> do
    code <- case rest of
      ["-f", file] -> T.readFile file
      ["-"]        -> T.getContents
      parts        -> return (T.unwords (map T.pack parts))
    send' ["js", "eval", T.unpack code]

  -- hs eval CODE | -f FILE | -  : evaluate Haskell at the ghci prompt of a
  -- `leksah.sh --ghci` session.  If the app is running, its run loop is
  -- suspended for the duration (Cocoa needs the process main thread — the UI
  -- freezes, then resumes right after the eval).
  ("hs":"eval":rest) | not (null rest) -> do
    code <- case rest of
      ["-f", file] -> readFile file
      ["-"]        -> getContents
      parts        -> return (unwords parts)
    hsEval code

  args          -> send args

-- | The first element of a list, if any (as a String), for optional args.
listToMaybeStr :: [String] -> Maybe String
listToMaybeStr (x:_) = Just x
listToMaybeStr []    = Nothing

-- | @cwd@ + argv, NUL-separated — the request wire format.
payloadFor :: [String] -> IO BS.ByteString
payloadFor args = do
  cwd <- getCurrentDirectory
  return $ BS.intercalate (BS.singleton 0)
             (map (encodeUtf8 . T.pack) (cwd : args))

-- | Send a command whose argv fields are already exactly as intended (no extra
-- packing), streaming the reply to stdout.  Used for @js eval@ where the code is
-- one field (possibly multi-line).
send' :: [String] -> IO ()
send' args = do
  path <- cmdSocketPath
  exists <- doesFileExist path
  unless exists (noSocket path)
  payload <- payloadFor args
  runClient path payload `catch` \(e :: IOException) -> do
    hPutStrLn stderr $ "leksah-cmd: could not talk to leksah: " <> show e
    exitFailure

send :: [String] -> IO ()
send = send'

noSocket :: FilePath -> IO ()
noSocket path = do
  hPutStrLn stderr $ "leksah-cmd: no control socket at " <> path
                     <> "\n(is leksah running? start it with ./leksah-nix.sh ghc914 wkwebview)"
  exitFailure

runClient :: FilePath -> BS.ByteString -> IO ()
runClient path payload = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix path)
  sendAll sock payload
  shutdown sock ShutdownSend   -- signal EOF so the server starts handling
  let drain = do
        chunk <- recv sock 65536
        if BS.null chunk then return () else BS.hPut stdout chunk >> drain
  drain
  close sock

-- | Try one command and collect its reply; 'Nothing' if we couldn't connect
-- (socket missing or no listener) — used for readiness polling.
tryReply :: [String] -> IO (Maybe Text)
tryReply args = do
  path <- cmdSocketPath
  exists <- doesFileExist path
  if not exists then return Nothing else do
    r <- try (collect path) :: IO (Either IOException BS.ByteString)
    return $ either (const Nothing) (Just . decodeUtf8With lenientDecode) r
  where
    collect path = do
      payload <- payloadFor args
      sock <- socket AF_UNIX Stream defaultProtocol
      connect sock (SockAddrUnix path)
      sendAll sock payload
      shutdown sock ShutdownSend
      let go acc = do
            chunk <- recv sock 65536
            if BS.null chunk then return (BS.concat (reverse acc)) else go (chunk : acc)
      out <- go []
      close sock
      return out

-- | Is the UI up right now?  (A @ping@ that returns "ok".)
pingOnce :: IO Bool
pingOnce = maybe False (T.isInfixOf "ok") <$> tryReply ["ping"]

-- | Poll @ping@ every 0.5 s until it succeeds or @secs@ elapse.
waitUp :: Int -> IO Bool
waitUp secs = go (max 1 (secs * 2))
  where
    go 0 = pingOnce
    go n = pingOnce >>= \case
      True  -> return True
      False -> threadDelay 500000 >> go (n - 1)

-- | Readiness that actually means the UI is up.  A bare @ping@ is not enough
-- after a (re)start: @startCmdServer@ rebinds the control socket during
-- @newIDE@, BEFORE the window's page is built, so ping answers while the DOM is
-- still empty (or, on the ghci reload path, while the recreated window hasn't
-- appeared yet).  So: wait for the socket, THEN poll a window's live JS context
-- until its DOM has actually been built (mirrors leksah.sh --ghci's DOM gate).
-- @js eval@ replies one number per window; any window past the threshold means a
-- real page is up.  No live context replies with no number → keep waiting.
waitUiReady :: Int -> IO Bool
waitUiReady secs = do
  up <- waitUp secs
  if not up then return False else domReady (max 1 (secs * 2))
  where
    domReady :: Int -> IO Bool
    domReady 0 = domOk
    domReady n = domOk >>= \case
      True  -> return True
      False -> threadDelay 500000 >> domReady (n - 1)
    domOk = do
      r <- tryReply ["js", "eval", "document.querySelectorAll('*').length"]
      return $ case r of
        Nothing -> False
        Just t  -> any (> 500) [ n | w <- words (map keepDigit (T.unpack t))
                                   , Just n <- [readMaybe w :: Maybe Int] ]
    keepDigit c = if isDigit c then c else ' '

-- | For @restart --wait@: first wait for the running instance to go DOWN (up to
-- ~20 s), so a ping can't match the old instance before it exits, then wait for
-- the new one to come UP.
waitDownThenUp :: Int -> IO Bool
waitDownThenUp secs = downThen (40 :: Int)
  where
    downThen 0 = waitUp secs   -- never saw it go down; just wait for up
    downThen n = pingOnce >>= \case
      False -> waitUp secs
      True  -> threadDelay 500000 >> downThen (n - 1)

-- ghci-mode driving -----------------------------------------------------------
--
-- `leksah.sh --ghci` runs the app INTERPRETED in a cabal multi-repl inside a
-- tmux pane (socket + pane id in ~/.leksah/ghci-pane…, output piped to
-- ~/.leksah/ghci….log).  The repl prompt is only available while the app's run
-- loop is stopped, so driving it is: ask the instance to stop over the control
-- socket, wait for the prompt in the pane, type at it with send-keys, and read
-- the results from the pipe-pane log — everything before an echoed nonce line
-- is the command's complete output (the ffcabal fence pattern).

-- | "ghci" for a `leksah.sh --ghci` instance, "binary" for a compiled one,
-- 'Nothing' when nothing answers the control socket.
instanceMode :: IO (Maybe String)
instanceMode = tryReply ["mode"] >>= \case
  Just t | "ghci" `T.isPrefixOf` T.strip t -> return (Just "ghci")
         | otherwise                       -> return (Just "binary")
  Nothing -> return Nothing

data GhciPane = GhciPane { gpSock :: String, gpPane :: String }

ghciFile :: String -> IO FilePath
ghciFile base = do
  home <- getHomeDirectory
  tag  <- instanceTag
  return $ home </> ".leksah" </> (base <> tag)

readGhciPane :: IO (Maybe GhciPane)
readGhciPane = do
  p <- ghciFile "ghci-pane"
  exists <- doesFileExist p
  if not exists then return Nothing else do
    ws <- words <$> readFile p
    return $ case ws of
      (sock:pane:_) -> Just (GhciPane sock pane)
      _             -> Nothing

withGhciPane :: (GhciPane -> IO ()) -> IO ()
withGhciPane act = readGhciPane >>= \case
  Just gp -> act gp
  Nothing -> failWith "no ghci session found (~/.leksah/ghci-pane…) — start one with ./leksah.sh --ghci GHCVER"

failWith :: String -> IO a
failWith msg = hPutStrLn stderr ("leksah-cmd: " <> msg) >> exitFailure >> error "unreachable"

tmuxOut :: GhciPane -> [String] -> IO String
tmuxOut gp args = readProcess "tmux" (["-L", gpSock gp] <> args) ""

-- | Type one line at the ghci prompt (literal text, then Enter).
tmuxSendLine :: GhciPane -> String -> IO ()
tmuxSendLine gp s = do
  void $ tmuxOut gp ["send-keys", "-t", gpPane gp, "-l", s]
  void $ tmuxOut gp ["send-keys", "-t", gpPane gp, "Enter"]

-- | The pane's last non-blank visible line ("" if the pane is unreachable).
paneLastLine :: GhciPane -> IO String
paneLastLine gp = do
  r <- try (tmuxOut gp ["capture-pane", "-p", "-t", gpPane gp])
       :: IO (Either SomeException String)
  return $ case r of
    Left _    -> ""
    Right out -> case filter (not . all isSpace) (lines out) of
      [] -> ""
      ls -> last ls

-- | Is this visible line the (idle) ghci prompt?
atPrompt :: String -> Bool
atPrompt l = "ghci>" `T.isSuffixOf` T.pack (dropWhileEnd isSpace l)

-- | Poll the pane until the idle prompt shows (the app has stopped / the load
-- finished), up to @secs@.
waitForPrompt :: GhciPane -> Int -> IO Bool
waitForPrompt gp secs = go (max 1 (secs * 2))
  where
    go :: Int -> IO Bool
    go 0 = atPrompt <$> paneLastLine gp
    go n = paneLastLine gp >>= \l ->
      if atPrompt l then return True else threadDelay 500000 >> go (n - 1)

-- | The pane's RENDERED contents (with scrollback), one clean line each.  We
-- read the rendered screen, not the raw pipe-pane log: haskeline's cursor/
-- keypad control sequences glue a prompt's echoed input to its output on one
-- physical byte-stream line, so the raw log can't be split on our fence
-- markers — but tmux's rendered capture lays each logical line out cleanly.
-- @-J@ rejoins wrapped long lines; @-S -100000@ pulls the whole scrollback
-- (leksah.sh raises the session's history-limit to match).
capturePane :: GhciPane -> IO [String]
capturePane gp = do
  r <- try (tmuxOut gp ["capture-pane", "-p", "-J", "-S", "-100000", "-t", gpPane gp])
       :: IO (Either SomeException String)
  return $ map (filter (/= '\r')) (lines (either (const "") id r))

-- | Output fences bracketing one eval.  We @:!echo@ each marker; because the
-- echoed *command* keeps the quote-split spelling (@BEGIN""_@) while the shell
-- prints the joined form (@BEGIN_@), only the real echoed output matches a
-- marker exactly — the command echo never does.  The pid suffix makes the
-- markers unique per invocation, so a poll never matches a *previous* run's
-- begin/end pair still sitting in the pane scrollback.
data Fence = Fence { fBeginOut, fBeginCmd, fEndOut, fEndCmd :: String }

mkFence :: IO Fence
mkFence = do
  t <- show <$> getProcessID
  return Fence
    { fBeginOut = "LEKSAH_CMD_BEGIN_"     <> t
    , fBeginCmd = "LEKSAH_CMD_BEGIN\"\"_" <> t
    , fEndOut   = "LEKSAH_CMD_END_"       <> t
    , fEndCmd   = "LEKSAH_CMD_END\"\"_"   <> t
    }

-- | The lines strictly between this fence's begin marker and the first end
-- marker after it.  'Nothing' until both are present in that order (the eval is
-- still running / its output hasn't landed yet).
--
-- Markers are matched as SUBSTRINGS, not whole lines.  @hs eval@ only
-- *suspends* the app's run loop (@ghci-stop --keep-windows@); it does NOT stop
-- leksah's background threads (loggers, the resync/heartbeat timers, reflex
-- frame threads), which keep writing to the pane while the eval runs.  A
-- concurrent write can land with no newline before a marker's echoed output, so
-- the marker ends up glued to a log line, e.g.
-- @LEK 08:22:31 [win 1] resync … LEKSAH_CMD_END_1234@.  A whole-line match
-- misses that and 'pollFence' then blocks for its full timeout — the UI stays
-- frozen ("locked up") the whole time.  Infix matching still can't false-match
-- the *command* echo (@:!echo …END""_1234@) because the @END""_@/@END_@
-- quote-split spelling means the command never contains the marker substring;
-- only real marker output does.  The glued boundary lines are excluded (we take
-- the lines strictly between), so a log fragment stuck to a marker is dropped.
fenceWindow :: Fence -> [String] -> Maybe [String]
fenceWindow f ls =
  case reverse [i | (i, l) <- idx, fBeginOut f `isInfixOf` l] of
    []      -> Nothing
    (b : _) -> case [e | (e, l) <- idx, e > b, fEndOut f `isInfixOf` l] of
                 []      -> Nothing
                 (e : _) -> Just (take (e - b - 1) (drop (b + 1) ls))
  where idx = zip [0 :: Int ..] ls

-- | Poll the rendered pane until this fence's begin/end pair appears; return the
-- lines between them (the fenced commands' output).
pollFence :: GhciPane -> Fence -> Int -> IO [String]
pollFence gp f secs = go (max 1 (secs * 4))
  where
    go :: Int -> IO [String]
    go 0 = do
      hPutStrLn stderr "leksah-cmd: timed out waiting for the ghci output fence; partial output follows."
      fromMaybe [] . fenceWindow f <$> capturePane gp
    go n = fenceWindow f <$> capturePane gp >>= \case
      -- The marker appeared, but this frame can catch haskeline mid-redraw: an
      -- unstripped input echo, or a command echo glued/overwritten into the
      -- output line (a wrap-boundary race — intermittent).  Once the END marker
      -- is printed the fenced region is immutable, so re-capture until two
      -- consecutive windows agree (the settled frame is always clean) rather
      -- than trusting a single delayed shot.
      Just w  -> settle w (8 :: Int)
      Nothing -> threadDelay 250000 >> go (n - 1)
    settle prev 0 = return prev
    settle prev k = do
      threadDelay 200000
      cur <- fromMaybe prev . fenceWindow f <$> capturePane gp
      if cur == prev then return cur else settle cur (k - 1)

-- | Type the code at the prompt (multi-line code goes inside @:{@ … @:}@);
-- returns the physical lines sent, so echoes can be filtered from the output.
sendCode :: GhciPane -> String -> IO [String]
sendCode gp code = case lines code of
  [l] -> tmuxSendLine gp l >> return [l]
  lns -> do
    let wrapped = [":{"] <> lns <> [":}"]
    mapM_ (tmuxSendLine gp) wrapped
    return wrapped

-- | Extract command output from the rendered pane lines, stripping the echoes
-- of what we typed.  haskeline (even under TERM=dumb on some GHCs) redraws a
-- prompt so a command echo is GLUED to the previous command's output on one
-- line (e.g. @:!echo …END…65536@).  So don't just drop whole echo lines —
-- strip a leading prompt marker and then a leading typed-command prefix from
-- each line, leaving whatever output was glued after it; lines that are pure
-- output pass through unchanged, and lines that were nothing but an echo become
-- empty and drop out.  @typed@ is every command we sent (code + fences).
--
-- The stripping runs to a FIXPOINT: send-keys' own keystroke echo and ghci's
-- prompt re-echo can both land on one line in either order (seen in the raw
-- byte stream as @[1..40]ghci> [1..40]@ — the typed command, then the prompt,
-- then the command again), so a single prompt-then-command pass leaves a stray
-- @ghci> [1..40]@.  Repeating until nothing more strips clears it.
filterEcho :: [String] -> [String] -> [String]
filterEcho typed = filter (not . null) . map clean
  where
    cmds  = sortBy (flip (comparing length)) typed   -- longest first: exact echo
    -- Only strip command echoes from lines that carry a PROMPT — those are the
    -- echo lines (a prompt, possibly with a glued keystroke echo before it and
    -- trailing output after).  A prompt-less line is pure output: keep it
    -- verbatim, so a result whose printed form equals the typed text (@42@,
    -- @"hi"@) is not mistaken for the echo and eaten.
    clean l | hasPrompt l = trim (fixstrip (dropWhile isSpace l))
            | otherwise   = trim l
    hasPrompt l = any (`isInfixOf` l) ["ghci>", "ghci|"]
    fixstrip s = let s' = dropWhile isSpace (stripCmd (stripPrompt s))
                 in if s' == s then s else fixstrip s'
    stripPrompt s = firstStrip s ["ghci> ", "ghci| ", "ghci>", "ghci|"]
    stripCmd s    = firstStrip s cmds
    firstStrip s xs = fromMaybe s (listToMaybe [ r | x <- xs, Just r <- [stripPrefix x s] ])
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- | @hs eval CODE@: evaluate Haskell at the ghci prompt.  If the app is
-- running, suspend its run loop first (UI freezes) and queue a resume right
-- after the fence, so the UI thaws the moment the eval completes.
hsEval :: String -> IO ()
hsEval code = withGhciPane $ \gp -> do
  f <- mkFence
  lastLn <- paneLastLine gp
  suspended <-
    if atPrompt lastLn
      then return False
      else instanceMode >>= \case
        Just "ghci" -> do
          r <- tryReply ["ghci-stop", "--keep-windows"]
          case r of
            Nothing -> failWith "could not reach the instance to suspend it"
            Just _  -> do
              ok <- waitForPrompt gp 30
              unless ok $ failWith "timed out waiting for the ghci prompt after suspending the app"
              return True
        Just _  -> failWith "the running instance is a compiled binary — hs eval needs a leksah.sh --ghci session"
        Nothing -> failWith "the ghci pane shows no prompt and the control socket isn't answering (still loading, or wedged?)"
  tmuxSendLine gp (":!echo " <> fBeginCmd f)
  sent <- sendCode gp code
  tmuxSendLine gp (":!echo " <> fEndCmd f)
  -- Capture the output while the app is still cleanly suspended (a stable
  -- prompt renders reliably); resume only AFTER, so the resume/fence command
  -- echoes never contaminate the captured window.
  out <- pollFence gp f 600
  when suspended $ tmuxSendLine gp "IDE.Web.MacGlue.resumeApp"
  -- send-keys' keystroke echo can interleave a fence command INTO an output
  -- line at the byte level (raw stream shows @[1,2:!echo …END""_NNN,3,4,…]@ —
  -- inserted, not overwriting, so every real char survives).  Those command
  -- echoes are unique (pid + the @""@ split), so remove them as infixes; then
  -- filterEcho clears own-line echoes and dropLeksahTrace clears LEK lines.
  let fenceCmds = [":!echo " <> fBeginCmd f, ":!echo " <> fEndCmd f]
      noise     = sent <> fenceCmds
      cleaned   = dropLeksahTrace
                . filter (not . null)
                . map (removeInfixes (fenceCmds <> [fBeginCmd f, fEndCmd f]))
                . filterEcho noise
                $ out
  mapM_ putStrLn cleaned

-- | Delete every occurrence of any of @subs@ from a line, then trim.  Used to
-- excise fence command echoes that haskeline interleaved into an output line.
removeInfixes :: [String] -> String -> String
removeInfixes subs = trim . go
  where
    go [] = []
    go s  = case [ drop (length sub) s | sub <- subs, sub `isPrefixOf` s ] of
              (rest : _) -> go rest
              []         -> head s : go (tail s)
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Drop leksah's own @LEK …@ trace/heartbeat lines from an eval's output.
-- @hs eval@ only suspends the run loop (@--keep-windows@), so leksah's
-- diagnostic threads (@logMutation@, @wlog@ — see IDE.Core.State / IDE.Web.Main)
-- keep writing to the pane's stderr and interleave with the eval result.  Those
-- traces are wanted everywhere else (freeze debugging), just not in a result;
-- a @Show@ value never starts @LEK @ + a digit, so this can't eat real output.
dropLeksahTrace :: [String] -> [String]
dropLeksahTrace = filter (not . isTrace)
  where isTrace l = case dropWhile isSpace l of
          ('L':'E':'K':' ':c:_) -> isDigit c
          _                     -> False

-- | The saved @:main@ line (leksah.sh --ghci wrote it with the LEKSAH_ARGS it
-- was given), or a plain @:main@.
readGhciMainLine :: IO String
readGhciMainLine = do
  p <- ghciFile "ghci-main"
  exists <- doesFileExist p
  if not exists then return ":main" else do
    l <- dropWhileEnd isSpace <$> readFile p
    return $ if ":main" `isPrefixOf` l then l else ":main"

-- | The ghci-mode restart: full teardown to the prompt, optionally @:reload@
-- (checking its Ok/Failed verdict), then @:main@ and wait for the UI.
ghciReloadRestart :: Bool -> IO ()
ghciReloadRestart doReload = withGhciPane $ \gp -> do
  lastLn <- paneLastLine gp
  unless (atPrompt lastLn) $ do
    r <- tryReply ["ghci-stop"]
    case r of
      Nothing -> failWith "instance not answering and the ghci pane is not at a prompt"
      Just _  -> return ()
    putStr "Stopping leksah (teardown to the ghci prompt)… " >> hFlush stdout
    ok <- waitForPrompt gp 60
    putStrLn (if ok then "done." else "TIMED OUT.")
    unless ok exitFailure
  when doReload $ do
    f <- mkFence
    tmuxSendLine gp (":!echo " <> fBeginCmd f)
    tmuxSendLine gp ":reload"
    tmuxSendLine gp (":!echo " <> fEndCmd f)
    out <- pollFence gp f 1800
    let lns = filterEcho [":reload", ":!echo " <> fBeginCmd f, ":!echo " <> fEndCmd f] out
    mapM_ putStrLn lns
    case reloadVerdict lns of
      Just True  -> return ()
      Just False -> failWith ":reload FAILED — fix the errors and run rebuild-self again (the prompt is waiting)"
      Nothing    -> hPutStrLn stderr "leksah-cmd: no Ok/Failed verdict found in the :reload output — starting anyway."
  mainLine <- readGhciMainLine
  -- Multi-repl scope: `:main` needs `main` in scope; idempotent, so always.
  tmuxSendLine gp ":module + Main"
  tmuxSendLine gp mainLine
  putStr "Starting leksah (:main)… " >> hFlush stdout
  ok <- waitUiReady 300
  putStrLn (if ok then "ready." else "TIMED OUT waiting for the UI.")
  unless ok exitFailure

-- | The last "Ok, N modules loaded." / "Failed, N modules loaded." line wins
-- (the ffcabal lastVerdict pattern).
reloadVerdict :: [String] -> Maybe Bool
reloadVerdict lns = case [v | l <- lns, Just v <- [verdictOf (dropWhile isSpace l)]] of
  [] -> Nothing
  vs -> Just (last vs)
  where verdictOf l | "Ok," `isPrefixOf` l && "loaded" `T.isInfixOf` T.pack l     = Just True
                    | "Failed," `isPrefixOf` l && "loaded" `T.isInfixOf` T.pack l = Just False
                    | otherwise                                                   = Nothing
