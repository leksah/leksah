{-# LANGUAGE OverloadedStrings #-}
-- | Patch the staged leksah.js for the browser: environment + catchable FS
-- errors.
--
-- Two RTS problems, same treatment (exact-text rewrites of the staged
-- artifact):
--
-- 1. Environment.  The RTS hardwires getenv/environ/setenv to `process.env`
--    under h$isNode(); in a browser the environment is always empty, so e.g.
--    HOME is unset and directory's getHomeDirectory falls back to the passwd
--    database (which doesn't exist here either).  Rather than chase every
--    Haskell caller, give the page an environment: the rewrites make the RTS
--    env functions consult (and mutate) `globalThis.leksahDemoEnv` when not
--    running under node.  shims.js defines leksahDemoEnv (HOME etc.) before
--    leksah.js loads.
--
-- 2. Uncatchable FS throws.  The RTS directory functions (mkdir, opendir,
--    readdir, closedir, *utimes) do `throw "h$mkdir unsupported"` in browser
--    mode — a raw JS string throw that Haskell `catch` can NOT intercept, so
--    any thread reaching one dies (createDirectoryIfMissing killed the main
--    thread at boot).  Rewrite each to fail errno-style instead: Haskell then
--    sees an ordinary catchable IOException.  NB h$setErrno only understands
--    the errno names in its lookup table — ENOSYS is not one of them (it
--    would itself throw); EPERM is.
--
-- 3. Out-of-bounds static-table reads.  alex/happy-style table lookups (e.g.
--    Cabal's .cabal-file lexer, hit via parseGenericPackageDescriptionMaybe)
--    read past a table's bounds — sometimes below it — BEFORE their validity
--    check.  Natively that's a harmless read of adjacent static memory; on
--    the JS backend every literal is its own buffer and DataView throws
--    RangeError (which kills the thread uncatchably).  Wrap the DataView of
--    static literals (h$rawStringData) so out-of-range reads return 0
--    instead — matching the native "read garbage, guard afterwards"
--    semantics.  u8[] accesses are already safe in JS (undefined coerces
--    to 0).
--
-- 4. Missing emscripten heap exports.  h$initEmscriptenHeap reads
--    Module.HEAP8/HEAPU8, but the linked-in emscripten module doesn't export
--    them: GHC's JS linker aggregates -sEXPORTED_RUNTIME_METHODS from the js
--    objects' //#OPTIONS:EMCC: pragmas (rts/js/mem.js lists only
--    addFunction,removeFunction,getEmptyTableSlot), and that flag OVERRIDES
--    the settings-level "-sEXPORTED_RUNTIME_METHODS=HEAP8,HEAPU8" (emcc:
--    last -s wins) — so the boot aborts "'HEAP8' was not exported".
--    Redefine the two exports as live getters over the glue's internal view
--    variables (live so they track memory growth).  Proper fix upstream: add
--    HEAP8,HEAPU8 to the mem.js pragma (stable-haskell/ghc).
--
-- Exact-text replacements.  'Strict' asserts each occurs exactly once — if a
-- GHC upgrade changes the RTS shim text this fails loudly instead of silently
-- shipping a broken build.  'Lenient' (for smaller programs like the demo's
-- breakout.js, which link fewer RTS shims — e.g. no emscripten glue at all)
-- lets a pattern be ABSENT, logging a matched/absent table so a genuinely
-- needed patch that stopped matching is still visible in the build log;
-- more than one occurrence is always fatal.
--
-- This is the core of patch-rts.hs (the runghc wrapper, which patches
-- strictly); the site derivation (nix\/website.nix via assemble-site.hs)
-- calls 'run' directly.
module PatchRts (run, Leniency(..)) where

import Control.Monad (foldM, when)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import System.Exit (die)
import Text.Printf (printf)

-- Join exact lines: every replacement is significant down to its whitespace.
tl :: [Text] -> Text
tl = T.intercalate "\n"

replacements :: [(Text, Text)]
replacements =
  [ -- h$getenv: check the page env first when not under node.
    ( tl [ "function h$getenv(name, name_off) {"
         , "    if(h$isNode()) {" ]
    , tl [ "function h$getenv(name, name_off) { /* leksahDemoEnv patch */"
         , "    if(!h$isNode() && globalThis.leksahDemoEnv) {"
         , "        var nD = h$decodeUtf8z(name, name_off);"
         , "        if(typeof globalThis.leksahDemoEnv[nD] !== 'undefined') {"
         , "            { h$ret1 = (0); return (h$encodeUtf8(globalThis.leksahDemoEnv[nD])); };"
         , "        }"
         , "    }"
         , "    if(h$isNode()) {" ] )

    -- h$__hscore_environ (getEnvironment): iterate whichever env exists.
  , ( tl [ "function h$__hscore_environ() {"
         , "    if(h$isNode()) {"
         , "        var env = [], i;"
         , "        for(i in process.env) {"
         , "          var envv = i + '=' + process.env[i];" ]
    , tl [ "function h$__hscore_environ() { /* leksahDemoEnv patch */"
         , "    var envSrc = h$isNode() ? process.env : (globalThis.leksahDemoEnv || null);"
         , "    if(envSrc) {"
         , "        var env = [], i;"
         , "        for(i in envSrc) {"
         , "          var envv = i + '=' + envSrc[i];" ] )

    -- h$setenv
  , ( tl [ "  if(h$isNode()) {"
         , "    if(overwrite || typeof process.env[n] !== 'undefined') process.env[n] = v;"
         , "  }" ]
    , tl [ "  if(h$isNode()) {"
         , "    if(overwrite || typeof process.env[n] !== 'undefined') process.env[n] = v;"
         , "  } else if(globalThis.leksahDemoEnv) {"
         , "    if(overwrite || typeof globalThis.leksahDemoEnv[n] !== 'undefined') globalThis.leksahDemoEnv[n] = v;"
         , "  }" ] )

    -- h$unsetenv
  , ( tl [ "  if(h$isNode()) delete process.env[n];"
         , "  return 0;" ]
    , tl [ "  if(h$isNode()) delete process.env[n];"
         , "  else if(globalThis.leksahDemoEnv) delete globalThis.leksahDemoEnv[n];"
         , "  return 0;" ] )

    -- h$putenv (both arms)
  , ( "    if(h$isNode()) delete process.env[x];"
    , tl [ "    if(h$isNode()) delete process.env[x];"
         , "    else if(globalThis.leksahDemoEnv) delete globalThis.leksahDemoEnv[x];" ] )
  , ( "    if(h$isNode()) process.env[name] = val;"
    , tl [ "    if(h$isNode()) process.env[name] = val;"
         , "    else if(globalThis.leksahDemoEnv) globalThis.leksahDemoEnv[name] = val;" ] )

    -- --- uncatchable browser-mode throws -> errno-style failures ----------
    -- opendir returns a dir handle (NULL + errno on failure).
  , ( tl [ "function h$opendir(path) {"
         , "  if(!h$isNode()) {"
         , "    throw \"h$opendir unsupported\";"
         , "  }" ]
    , tl [ "function h$opendir(path) {"
         , "  if(!h$isNode()) {"
         , "    h$setErrno('EPERM'); { h$ret1 = (0); return (null); };"
         , "  }" ] )
  , ( tl [ "function h$closedir(d,o) {"
         , "  if(!h$isNode()) {"
         , "    throw \"h$closedir unsupported\";"
         , "  }" ]
    , tl [ "function h$closedir(d,o) {"
         , "  if(!h$isNode()) {"
         , "    h$setErrno('EPERM'); return -1;"
         , "  }" ] )
    -- readdir returns a dirent (NULL = end-of-directory / error + errno).
  , ( tl [ "function h$readdir(d,o) {"
         , "  if(!h$isNode()) {"
         , "    throw \"h$readdir unsupported\";"
         , "  }" ]
    , tl [ "function h$readdir(d,o) {"
         , "  if(!h$isNode()) {"
         , "    h$setErrno('EPERM'); { h$ret1 = (0); return (null); };"
         , "  }" ] )
  , ( tl [ "function h$__hscore_readdir(d,o,dst_a,dst_o) {"
         , "  if(!h$isNode()) {"
         , "    throw \"h$readdir unsupported\";"
         , "  }" ]
    , tl [ "function h$__hscore_readdir(d,o,dst_a,dst_o) {"
         , "  if(!h$isNode()) {"
         , "    h$setErrno('EPERM'); return -1;"
         , "  }" ] )
  , ( tl [ "function h$mkdir(path, path_offset, mode) {"
         , "  if (!h$isNode()) {"
         , "    throw \"h$mkdir unsupported\";"
         , "  }" ]
    , tl [ "function h$mkdir(path, path_offset, mode) {"
         , "  if (!h$isNode()) {"
         , "    h$setErrno('EPERM'); return -1;"
         , "  }" ] )
  , ( tl [ "function h$js_futimes(fd,atime,mtime) {"
         , "  if (!h$isNode()) {"
         , "    throw \"h$js_futimes unsupported\";"
         , "  }" ]
    , tl [ "function h$js_futimes(fd,atime,mtime) {"
         , "  if (!h$isNode()) {"
         , "    h$setErrno('EPERM'); return -1;"
         , "  }" ] )
  , ( tl [ "function h$js_utimes(path,path_offset,atime,mtime) {"
         , "  if (!h$isNode()) {"
         , "    throw \"h$js_utimes unsupported\";"
         , "  }" ]
    , tl [ "function h$js_utimes(path,path_offset,atime,mtime) {"
         , "  if (!h$isNode()) {"
         , "    h$setErrno('EPERM'); return -1;"
         , "  }" ] )
  , ( tl [ "function h$js_lutimes(path,path_offset,atime,mtime) {"
         , "  if (!h$isNode()) {"
         , "    throw \"h$js_lutimes unsupported\";"
         , "  }" ]
    , tl [ "function h$js_lutimes(path,path_offset,atime,mtime) {"
         , "  if (!h$isNode()) {"
         , "    h$setErrno('EPERM'); return -1;"
         , "  }" ] )

    -- --- emscripten heap exports (see item 4) ----------------------------
    -- The abort-getters for unexported runtime symbols are installed by this
    -- forEach and are configurable:true, so redefining afterwards is legal.
  , ( "unexportedSymbols.forEach(unexportedRuntimeSymbol);"
    , tl [ "unexportedSymbols.forEach(unexportedRuntimeSymbol);"
         , "Object.defineProperty(Module,'HEAP8',{configurable:true,get:()=>HEAP8});"
         , "Object.defineProperty(Module,'HEAPU8',{configurable:true,get:()=>HEAPU8});" ] )

    -- --- bounds-tolerant DataView for static literals (see item 3) -------
  , ( tl [ "function h$rawStringData(str) {"
         , "    var v = h$newByteArray(str.length+1);"
         , "    var u8 = v.u8;"
         , "    for(var i=0;i<str.length;i++) {"
         , "       u8[i] = str[i];"
         , "    }"
         , "    u8[str.length] = 0;"
         , "    return v;"
         , "}" ]
    , tl [ "function h$rawStringData(str) {"
         , "    var v = h$newByteArray(str.length+1);"
         , "    var u8 = v.u8;"
         , "    for(var i=0;i<str.length;i++) {"
         , "       u8[i] = str[i];"
         , "    }"
         , "    u8[str.length] = 0;"
         , "    var dv = v.dv, n = dv.byteLength;"
         , "    v.dv = {"
         , "      getUint8:   function(o)    { return (o>=0 && o+1<=n) ? dv.getUint8(o)      : 0; },"
         , "      getInt8:    function(o)    { return (o>=0 && o+1<=n) ? dv.getInt8(o)       : 0; },"
         , "      getUint16:  function(o,le) { return (o>=0 && o+2<=n) ? dv.getUint16(o,le)  : 0; },"
         , "      getInt16:   function(o,le) { return (o>=0 && o+2<=n) ? dv.getInt16(o,le)   : 0; },"
         , "      getUint32:  function(o,le) { return (o>=0 && o+4<=n) ? dv.getUint32(o,le)  : 0; },"
         , "      getInt32:   function(o,le) { return (o>=0 && o+4<=n) ? dv.getInt32(o,le)   : 0; },"
         , "      getFloat32: function(o,le) { return (o>=0 && o+4<=n) ? dv.getFloat32(o,le) : 0; },"
         , "      getFloat64: function(o,le) { return (o>=0 && o+8<=n) ? dv.getFloat64(o,le) : 0; },"
         , "      getBigInt64:  function(o,le) { return (o>=0 && o+8<=n) ? dv.getBigInt64(o,le)  : 0n; },"
         , "      getBigUint64: function(o,le) { return (o>=0 && o+8<=n) ? dv.getBigUint64(o,le) : 0n; },"
         , "      setUint8:   dv.setUint8.bind(dv),"
         , "      setInt8:    dv.setInt8.bind(dv),"
         , "      setUint16:  dv.setUint16.bind(dv),"
         , "      setInt16:   dv.setInt16.bind(dv),"
         , "      setUint32:  dv.setUint32.bind(dv),"
         , "      setInt32:   dv.setInt32.bind(dv),"
         , "      setFloat32: dv.setFloat32.bind(dv),"
         , "      setFloat64: dv.setFloat64.bind(dv),"
         , "      setBigInt64:  dv.setBigInt64.bind(dv),"
         , "      setBigUint64: dv.setBigUint64.bind(dv),"
         , "      byteLength: n,"
         , "      buffer: dv.buffer"
         , "    };"
         , "    return v;"
         , "}" ] )
  ]

-- | How to treat a pattern that does not occur in the artifact.
data Leniency = Strict | Lenient deriving (Eq, Show)

-- | @run leniency path@ — apply the rewrites to @path@ in place
-- (idempotent; a site whose NEW text is already present is skipped).
run :: Leniency -> FilePath -> IO ()
run leniency path = do
  src0 <- TE.decodeUtf8 <$> BS.readFile path
  (src, applied, skipped, absent) <-
      foldM step (src0, 0 :: Int, 0 :: Int, 0 :: Int) replacements
  when (applied > 0) $ BS.writeFile path (TE.encodeUtf8 src)
  printf "%s: %d sites patched, %d already patched, %d absent\n"
         path applied skipped absent
  where
    step (src, applied, skipped, absent) (old, new)
      | new `T.isInfixOf` src = pure (src, applied, skipped + 1, absent)  -- already patched (idempotence)
      | otherwise = case (T.count old src, leniency) of
          (1, _) -> pure (T.replace old new src, applied + 1, skipped, absent)
          (0, Lenient) -> do
            printf "  absent: %s…\n" (T.unpack (T.takeWhile (/= '\n') old))
            pure (src, applied, skipped, absent + 1)
          (n, _) -> die $ path ++ ": expected exactly 1 occurrence of:\n"
                     ++ T.unpack old
                     ++ "\nfound " ++ show n
                     ++ " — RTS shim text changed (GHC upgrade?); update PatchRts.hs"
