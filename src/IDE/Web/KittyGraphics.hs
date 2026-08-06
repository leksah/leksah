{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Making the kitty graphics protocol usable in leksah's terminal panes.
--
-- xterm.js speaks the protocol itself (@addon-image@, @kittySupport@), so this
-- module is only what a *terminal embedded in tmux, in a browser engine* has to
-- add on top — the two things the addon cannot do for itself:
--
--   * unwrap tmux's DCS passthrough (@ESC P tmux ; … ESC \\@).  In a
--     control-mode pane tmux is only a multiplexer — leksah's xterm IS the
--     terminal — so a program's passthrough arrives verbatim, ESCs still
--     doubled, and xterm drops it as an unknown DCS.  Unwrapping here is what
--     makes ANY passthrough sequence work in a CC pane, images or not (OSC 52
--     clipboard, progress reports, SIXEL, iTerm2 images, kitty);
--   * serve transmissions made BY REFERENCE — @t=s@ (POSIX shared memory) and
--     @t=f@/@t=t@ (a file) — which no browser engine can read, so the addon
--     refuses them (@EINVAL:unsupported transmission medium@).  This is how a
--     client streaming video (an RDP or VNC viewer) avoids base64-ing every
--     frame through the pty.  The filter fetches the bytes through
--     'installImageBytesBridge' and rewrites the command as an equivalent
--     @t=d@, which the addon then handles like any other direct transmission.
--
-- Everything else about kitty graphics — placements, image ids, deletes, cursor
-- movement, the @a=q@ handshake for direct transmission — is the addon's.
--
-- A referenced object is read on leksah's own host, so this covers local panes
-- and not a pane on a remote host (the limitation kitty itself has).  Note also
-- that tmux cannot know how tall a passthrough image is, so its screen model
-- reserves no rows for it: a client should send @C=1@ and move the cursor
-- itself, and must expect the image to be gone after anything that repaints the
-- pane from tmux's model (a resize, a re-attach).
module IDE.Web.KittyGraphics (kittyGraphicsJs, installImageBytesBridge) where

import Data.Text (Text)
import qualified Data.Text as T

import Language.Javascript.JSaddle (JSM)

#if !defined(ghcjs_HOST_OS)
import Control.Lens ((^.))
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, handle)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.List (isPrefixOf)
import Data.Text.Encoding (decodeUtf8)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr (Ptr, castPtr, intPtrToPtr)
import GHCJS.DOM.Types (askJSM)
import Language.Javascript.JSaddle
       (fun, js2, jsg, jss, runJSM, valToNumber, valToText)
import System.Directory (doesFileExist, removeFile)
import System.IO (IOMode(ReadMode), hFileSize, withBinaryFile)
import System.Posix.Files (fileSize, getFdStatus)
import System.Posix.IO (closeFd)
import System.Posix.SharedMem (ShmOpenFlags(..), shmOpen, shmUnlink)
import System.Posix.Types (COff(..), Fd(..))
#endif

kittyGraphicsJs :: Text
kittyGraphicsJs = T.unlines
  [ "window.LeksahKitty = (function(){"
  , "  var ESC = 0x1b;"
  -- Rather than buffer an unterminated sequence forever, give up and let the
  -- bytes through: a broken program cannot wedge the pane.
  , "  var HOLD_LIMIT = 33554432;"
  -- Stay under the image addon's own per-sequence limit for what we rewrite.
  , "  var MAX_DIRECT = 31457280;"
  , "  var st = {};"
  , "  function state(id){"
  , "    var s = st[id];"
  , "    if (!s) s = st[id] = { hold: null, q: [], busy: false };"
  , "    return s;"
  , "  }"
  , "  function drop(id){ delete st[id]; }"

  -- ---------- byte helpers ----------
  , "  function concat(a, b){"
  , "    if (!a || !a.length) return b;"
  , "    if (!b || !b.length) return a;"
  , "    var r = new Uint8Array(a.length + b.length);"
  , "    r.set(a, 0); r.set(b, a.length);"
  , "    return r;"
  , "  }"
  , "  function ascii(bytes){"
  , "    var s = '';"
  , "    for (var i = 0; i < bytes.length; i += 8192)"
  , "      s += String.fromCharCode.apply(null, bytes.subarray(i, i + 8192));"
  , "    return s;"
  , "  }"

  -- ---------- the stream filter ----------
  -- Writes are serialised per terminal: fetching a referenced image is
  -- asynchronous, and the bytes that follow it must not overtake it into xterm.
  -- The queue is that ordering; with nothing pending the path stays synchronous.
  , "  function feed(id, term, bytes){"
  , "    var s = state(id);"
  , "    s.q.push(bytes);"
  , "    if (!s.busy) pump(id, term);"
  , "  }"
  , "  function pump(id, term){"
  , "    var s = state(id);"
  , "    while (s.q.length) {"
  , "      var p = scan(id, term, s.q.shift());"
  , "      if (p && p.then) {"
  , "        s.busy = true;"
  , "        var go = function(){ s.busy = false; pump(id, term); };"
  , "        p.then(go, go);"
  , "        return;"
  , "      }"
  , "    }"
  , "  }"
  , "  function findSt(buf, from){"
  , "    for (var i = from; i + 1 < buf.length; i++)"
  , "      if (buf[i] === ESC && buf[i+1] === 0x5c) return i;"
  , "    return -1;"
  , "  }"
  , "  function isTmux(buf, at){"
  , "    var m = [116,109,117,120,59];"
  , "    for (var i = 0; i < 5; i++) if (buf[at+i] !== m[i]) return false;"
  , "    return true;"
  , "  }"
  -- tmux's passthrough body has every ESC doubled and ends at a lone ESC \.
  , "  function unwrapTmux(buf, from){"
  , "    var n = buf.length, o = new Uint8Array(n - from), k = 0, i = from;"
  , "    while (i < n) {"
  , "      if (buf[i] === ESC) {"
  , "        if (i + 1 >= n) return null;"
  , "        if (buf[i+1] === ESC) { o[k++] = ESC; i += 2; continue; }"
  , "        if (buf[i+1] === 0x5c) return { end: i + 2, bytes: o.subarray(0, k) };"
  , "        o[k++] = ESC; i++; continue;"
  , "      }"
  , "      o[k++] = buf[i++];"
  , "    }"
  , "    return null;"
  , "  }"
  -- The scan splits the stream ONLY where it intercepts something: ordinary
  -- output (which is ESC-dense) reaches xterm as one write per chunk, exactly
  -- as it did before this filter existed.  A sequence straddling two chunks is
  -- held over (terminal output is split at arbitrary byte boundaries); a lone
  -- trailing ESC is held too, since its meaning depends on the next byte.
  , "  function scan(id, term, buf){"
  , "    var s = state(id);"
  , "    if (s.hold && s.hold.length) { buf = concat(s.hold, buf); s.hold = null; }"
  , "    var n = buf.length, i = 0, seg = 0;"
  , "    function emitSeg(end){ if (end > seg) term.write(buf.subarray(seg, end)); }"
  , "    function holdFrom(at){"
  , "      emitSeg(at);"
  , "      var rest = buf.subarray(at);"
  , "      if (rest.length > HOLD_LIMIT) term.write(rest); else s.hold = rest;"
  , "    }"
  , "    function queueRest(at){ if (at < n) s.q.unshift(buf.subarray(at)); }"
  , "    while (i < n) {"
  , "      var e = buf.indexOf(ESC, i);"
  , "      if (e < 0) break;"
  , "      if (e + 1 >= n) { holdFrom(e); return null; }"
  , "      var c = buf[e+1];"
  , "      if (c === 0x5f) {"
  , "        var t = findSt(buf, e + 2);"
  , "        if (t < 0) { holdFrom(e); return null; }"
  , "        var body = buf.subarray(e + 2, t);"
  -- Only a kitty command that names data we can fetch is ours; anything else
  -- (including kitty's own t=d) passes through to the image addon untouched.
  , "        var cmd = body.length && body[0] === 0x47 ? byRef(ascii(body)) : null;"
  , "        if (!cmd) { i = t + 2; continue; }"
  , "        emitSeg(e);"
  , "        seg = i = t + 2;"
  , "        var p = serve(term, cmd);"
  , "        if (p && p.then) { queueRest(i); return p; }"
  , "      } else if (c === 0x50) {"
  , "        if (e + 7 > n) { holdFrom(e); return null; }"
  -- Any other DCS (a sixel, say) is xterm's business, not ours.
  , "        if (!isTmux(buf, e + 2)) { i = e + 2; continue; }"
  , "        var un = unwrapTmux(buf, e + 7);"
  , "        if (!un) { holdFrom(e); return null; }"
  -- The unwrapped bytes are rescanned first (they may be kitty, sixel, OSC 52
  -- ...), then the remainder of this chunk.
  , "        emitSeg(e);"
  , "        queueRest(un.end);"
  , "        s.q.unshift(un.bytes);"
  , "        return null;"
  , "      } else {"
  , "        i = e + 2;"
  , "      }"
  , "    }"
  , "    emitSeg(n);"
  , "    return null;"
  , "  }"

  -- ---------- transmissions by reference ----------
  , "  var NUMKEYS = {i:1,I:1,p:1,s:1,v:1,x:1,y:1,w:1,h:1,c:1,r:1,X:1,Y:1,z:1,m:1,q:1,U:1,f:1,S:1,O:1,C:1,H:1,V:1};"
  , "  function parseCtrl(txt){"
  , "    var k = {}, parts = txt.split(',');"
  , "    for (var j = 0; j < parts.length; j++) {"
  , "      var eq = parts[j].indexOf('=');"
  , "      if (eq <= 0) continue;"
  , "      var key = parts[j].slice(0, eq), val = parts[j].slice(eq + 1);"
  , "      k[key] = NUMKEYS[key] ? parseInt(val, 10) || 0 : val;"
  , "    }"
  , "    return k;"
  , "  }"
  -- Which media leksah can serve for the addon (see 'imageMedia').  Anything
  -- else stays with the addon, whose refusal is the right answer for a client
  -- that queried first.
  , "  function mediaOk(t){"
  , "    if (!window.LeksahImageBytes) return false;"
  , "    return (window.LeksahImageMedia || '').indexOf(t) >= 0;"
  , "  }"
  , "  function byRef(txt){"
  , "    var semi = txt.indexOf(';');"
  , "    var k = parseCtrl(semi < 0 ? txt.slice(1) : txt.slice(1, semi));"
  , "    var t = k.t || 'd';"
  , "    if (t === 'd' || !mediaOk(t)) return null;"
  -- A chunked name, or one to be read at an offset, is not something we serve.
  , "    if (k.m === 1 || k.O) return null;"
  , "    var name;"
  , "    try { name = atob(semi < 0 ? '' : txt.slice(semi + 1)); } catch (e) { return null; }"
  , "    if (!name) return null;"
  , "    return { k: k, t: t, name: name };"
  , "  }"
  -- Answer a query ourselves (the addon would refuse the medium), otherwise
  -- fetch the data and hand the addon the same command as a direct one.
  , "  function serve(term, cmd){"
  , "    var k = cmd.k;"
  , "    if (k.a === 'q') { respond(term, k, 'OK'); return null; }"
  , "    var bpp = k.f === 24 ? 3 : 4;"
  , "    var need = k.S || (k.f === 100 ? 0 : (k.s || 0) * (k.v || 0) * bpp);"
  , "    return fetchBytes(cmd.t, cmd.name, need).then(function(b64){"
  , "      if (b64.length > MAX_DIRECT) { respond(term, k, 'EINVAL:image too large'); return; }"
  , "      term.write(direct(k) + b64 + '\\x1b\\\\');"
  , "    }, function(){ respond(term, k, 'EBADF:cannot read t=' + cmd.t); });"
  , "  }"
  -- The original command with t=d, and without the keys that only described
  -- where the data was (S: how much to read, O: from what offset).
  , "  function direct(k){"
  , "    var out = [];"
  , "    for (var key in k) if (key !== 't' && key !== 'S' && key !== 'O')"
  , "      out.push(key + '=' + k[key]);"
  , "    out.push('t=d');"
  , "    return '\\x1b_G' + out.join(',') + ';';"
  , "  }"
  -- Failures we own have to be reported the way the addon reports its own:
  -- on the pane's INPUT, so the program that asked reads them on stdin.
  -- q=1 mutes OK, q=2 mutes errors as well.
  , "  function respond(term, k, msg){"
  , "    var q = k.q || 0;"
  , "    if (msg === 'OK' ? q >= 1 : q >= 2) return;"
  , "    if (k.a !== 'q' && !k.i && !k.I) return;"
  , "    var idp = k.i ? 'i=' + k.i : (k.I ? 'I=' + k.I : 'i=0');"
  , "    try { term._core.coreService.triggerDataEvent('\\x1b_G' + idp + ';' + msg + '\\x1b\\\\'); }"
  , "    catch (e) {}"
  , "  }"
  -- The Haskell side does the reading and answers with the token it was given.
  , "  var pending = {}, nextTok = 1;"
  , "  function fetchBytes(kind, name, size){"
  , "    return new Promise(function(res, rej){"
  , "      var tok = nextTok++;"
  -- A lost answer would park the pane's output forever, so time it out.
  , "      var timer = setTimeout(function(){"
  , "        if (pending[tok]) { delete pending[tok]; rej(); }"
  , "      }, 5000);"
  , "      pending[tok] = { res: res, rej: rej, timer: timer };"
  , "      try { window.LeksahImageBytes(kind, name, size, tok); }"
  , "      catch (e) { clearTimeout(timer); delete pending[tok]; rej(); }"
  , "    });"
  , "  }"
  , "  function bytesReady(tok, b64){"
  , "    var p = pending[tok];"
  , "    if (!p) return;"
  , "    delete pending[tok];"
  , "    clearTimeout(p.timer);"
  , "    if (b64) p.res(b64); else p.rej();"
  , "  }"
  , "  return { feed: feed, drop: drop, bytesReady: bytesReady };"
  , "})();"
  ]

-- | Installs @window.LeksahImageBytes(kind, name, size, token)@ — the filter's
-- way to ask for the bytes behind a transmission it cannot read itself (@t=s@
-- shared memory, @t=f@/@t=t@ a file).  The answer arrives asynchronously as
-- @LeksahKitty.bytesReady(token, base64)@ (empty on failure), and the read runs
-- on its own thread: one frame of a video-rate stream is megabytes, which must
-- not sit on the thread driving the UI.
installImageBytesBridge :: JSM ()
#if defined(ghcjs_HOST_OS)
-- In the browser there is no shared memory and no local file system to read, so
-- the filter sees no bridge and refuses those media — a client that asks first
-- then falls back to sending the pixels inline.
installImageBytesBridge = return ()
#else
installImageBytesBridge = do
  ctx <- askJSM
  w <- jsg ("window" :: Text)
  void $ w ^. jss ("LeksahImageMedia" :: Text) imageMedia
  void $ w ^. jss ("LeksahImageBytes" :: Text) (fun $ \_ _ args -> case args of
    (kindV : nameV : sizeV : tokV : _) -> do
      kind <- valToText kindV
      name <- valToText nameV
      size <- valToNumber sizeV
      tok  <- valToNumber tokV
      void . liftIO . forkIO $ do
        mb <- readImageBytes kind (T.unpack name) (truncate size)
        (`runJSM` ctx) . void $
          jsg ("LeksahKitty" :: Text) ^. js2 ("bytesReady" :: Text) tok
            (maybe "" (decodeUtf8 . B64.encode) mb)
    _ -> return ())

-- | Which transmission media leksah can serve, as the letters the protocol uses
-- (@window.LeksahImageMedia@): shared memory and both file forms.  Only the
-- @a=q@/refuse decision needs this; a read that fails is reported per command.
imageMedia :: Text
imageMedia = "sft"

-- | An image is at most this big, whatever the client claims: the name and the
-- size both come from the pane, and a bogus pair should fail, not allocate.
maxImageBytes :: Int
maxImageBytes = 64 * 1024 * 1024

readImageBytes :: Text -> FilePath -> Int -> IO (Maybe BS.ByteString)
readImageBytes kind name want = handle onErr $ case kind of
    "s" -> readShm name want
    "f" -> readFilePart name want
    "t" -> do
      bs <- readFilePart name want
      -- A temp-file transmission makes the terminal responsible for deleting
      -- the file — but only somewhere temporary, never an arbitrary path a
      -- pane names.
      when (any (`isPrefixOf` name) tempDirs) $ handle onUnit (removeFile name)
      return bs
    _ -> return Nothing
  where
    tempDirs = ["/tmp/", "/private/tmp/", "/var/tmp/", "/private/var/folders/", "/var/folders/"]
    onErr :: SomeException -> IO (Maybe BS.ByteString)
    onErr _ = return Nothing
    onUnit :: SomeException -> IO ()
    onUnit _ = return ()

readFilePart :: FilePath -> Int -> IO (Maybe BS.ByteString)
readFilePart path want = do
  there <- doesFileExist path
  if not there then return Nothing else
    withBinaryFile path ReadMode $ \h -> do
      have <- fromIntegral <$> hFileSize h
      let len = readLen want have
      if len <= 0 then return Nothing else Just <$> BS.hGet h len

-- | @t=s@: a POSIX shared-memory object, which can only be read through mmap
-- (a plain read of such an fd is unspecified, and fails on macOS).  The
-- protocol makes the terminal responsible for unlinking it afterwards, whether
-- or not the read worked — the object is single-use.
--
-- Opening one read-only never passes a mode, which matters: @unix@'s 'shmOpen'
-- declares the VARIADIC @shm_open(const char*, int, ...)@ as a plain @ccall@
-- with three fixed arguments, so on arm64 (where variadic arguments go on the
-- stack) the mode lands in the wrong place.  That only corrupts a CREATE — the
-- kernel ignores mode without @O_CREAT@ — so this reader is safe, but do not
-- add a create path through it (a probe that did exactly that reported macOS as
-- unable to reopen shm at all, which is false: verified from C, and iTerm2 does
-- it).  Note also that macOS rounds an object's size up to a page, so the size
-- from @fstat@ over-reports; what the client declared wins (see 'readLen').
readShm :: FilePath -> Int -> IO (Maybe BS.ByteString)
readShm name want = do
  let nm = if "/" `isPrefixOf` name then name else '/' : name
      flags = ShmOpenFlags { shmCreate = False, shmExclusive = False
                           , shmReadWrite = False, shmTrunc = False }
  r <- bracket (shmOpen nm flags 0) closeFd $ \fd@(Fd cfd) -> do
         have <- fromIntegral . fileSize <$> getFdStatus fd
         let len = readLen want have
         if len <= 0 then return Nothing else
           bracket (c_mmap nullPtr' (fromIntegral len) protRead mapShared cfd 0)
                   (\p -> when (p /= mapFailed) . void $
                            c_munmap p (fromIntegral len))
                   (\p -> if p == mapFailed then return Nothing
                            else Just <$> BS.packCStringLen (castPtr p, len))
  handle onUnit (shmUnlink nm)
  return r
  where
    onUnit :: SomeException -> IO ()
    onUnit _ = return ()

-- | How much to read: what the client asked for, clamped to what is there and
-- to 'maxImageBytes' (a client that says nothing gets the whole object).
readLen :: Int -> Int -> Int
readLen want have
  | have <= 0 = 0
  | want > 0  = min maxImageBytes (min want have)
  | otherwise = min maxImageBytes have

foreign import ccall unsafe "sys/mman.h mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr ())
foreign import ccall unsafe "sys/mman.h munmap"
  c_munmap :: Ptr () -> CSize -> IO CInt

nullPtr' :: Ptr ()
nullPtr' = intPtrToPtr 0

mapFailed :: Ptr ()
mapFailed = intPtrToPtr (-1)

protRead, mapShared :: CInt
protRead = 1
mapShared = 1
#endif
