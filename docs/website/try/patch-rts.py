#!/usr/bin/env python3
"""Patch the staged leksah.js for the browser: environment + catchable FS errors.

Two RTS problems, same treatment (exact-text rewrites of the staged artifact):

1. Environment.  The RTS hardwires getenv/environ/setenv to `process.env`
   under h$isNode(); in a browser the environment is always empty, so e.g.
   HOME is unset and directory's getHomeDirectory falls back to the passwd
   database (which doesn't exist here either).  Rather than chase every
   Haskell caller, give the page an environment: the rewrites make the RTS
   env functions consult (and mutate) `globalThis.leksahDemoEnv` when not
   running under node.  shims.js defines leksahDemoEnv (HOME etc.) before
   leksah.js loads.

2. Uncatchable FS throws.  The RTS directory functions (mkdir, opendir,
   readdir, closedir, *utimes) do `throw "h$mkdir unsupported"` in browser
   mode — a raw JS string throw that Haskell `catch` can NOT intercept, so
   any thread reaching one dies (createDirectoryIfMissing killed the main
   thread at boot).  Rewrite each to fail errno-style instead: Haskell then
   sees an ordinary catchable IOException.  NB h$setErrno only understands
   the errno names in its lookup table — ENOSYS is not one of them (it
   would itself throw); EPERM is.

3. Out-of-bounds static-table reads.  alex/happy-style table lookups (e.g.
   Cabal's .cabal-file lexer, hit via parseGenericPackageDescriptionMaybe)
   read past a table's bounds — sometimes below it — BEFORE their validity
   check.  Natively that's a harmless read of adjacent static memory; on the
   JS backend every literal is its own buffer and DataView throws RangeError
   (which kills the thread uncatchably).  Wrap the DataView of static
   literals (h$rawStringData) so out-of-range reads return 0 instead —
   matching the native "read garbage, guard afterwards" semantics.  u8[]
   accesses are already safe in JS (undefined coerces to 0).

Exact-text replacements, each asserted to occur exactly once — if a GHC
upgrade changes the RTS shim text this fails loudly instead of silently
shipping a broken build.

Usage: patch-rts.py [leksah.js]   (idempotent; skips if already patched)
"""
import sys

PATH = sys.argv[1] if len(sys.argv) > 1 else 'leksah.js'
MARK = '/* leksahDemoEnv patch */'

REPLACEMENTS = [
    # h$getenv: check the page env first when not under node.
    ("""function h$getenv(name, name_off) {
    if(h$isNode()) {""",
     """function h$getenv(name, name_off) { %s
    if(!h$isNode() && globalThis.leksahDemoEnv) {
        var nD = h$decodeUtf8z(name, name_off);
        if(typeof globalThis.leksahDemoEnv[nD] !== 'undefined') {
            { h$ret1 = (0); return (h$encodeUtf8(globalThis.leksahDemoEnv[nD])); };
        }
    }
    if(h$isNode()) {""" % MARK),

    # h$__hscore_environ (getEnvironment): iterate whichever env exists.
    ("""function h$__hscore_environ() {
    if(h$isNode()) {
        var env = [], i;
        for(i in process.env) {
          var envv = i + '=' + process.env[i];""",
     """function h$__hscore_environ() { %s
    var envSrc = h$isNode() ? process.env : (globalThis.leksahDemoEnv || null);
    if(envSrc) {
        var env = [], i;
        for(i in envSrc) {
          var envv = i + '=' + envSrc[i];""" % MARK),

    # h$setenv
    ("""  if(h$isNode()) {
    if(overwrite || typeof process.env[n] !== 'undefined') process.env[n] = v;
  }""",
     """  if(h$isNode()) {
    if(overwrite || typeof process.env[n] !== 'undefined') process.env[n] = v;
  } else if(globalThis.leksahDemoEnv) {
    if(overwrite || typeof globalThis.leksahDemoEnv[n] !== 'undefined') globalThis.leksahDemoEnv[n] = v;
  }"""),

    # h$unsetenv
    ("""  if(h$isNode()) delete process.env[n];
  return 0;""",
     """  if(h$isNode()) delete process.env[n];
  else if(globalThis.leksahDemoEnv) delete globalThis.leksahDemoEnv[n];
  return 0;"""),

    # h$putenv (both arms)
    ("""    if(h$isNode()) delete process.env[x];""",
     """    if(h$isNode()) delete process.env[x];
    else if(globalThis.leksahDemoEnv) delete globalThis.leksahDemoEnv[x];"""),
    ("""    if(h$isNode()) process.env[name] = val;""",
     """    if(h$isNode()) process.env[name] = val;
    else if(globalThis.leksahDemoEnv) globalThis.leksahDemoEnv[name] = val;"""),

    # --- uncatchable browser-mode throws → errno-style failures ----------
    # opendir returns a dir handle (NULL + errno on failure).
    ("""function h$opendir(path) {
  if(!h$isNode()) {
    throw "h$opendir unsupported";
  }""",
     """function h$opendir(path) {
  if(!h$isNode()) {
    h$setErrno('EPERM'); { h$ret1 = (0); return (null); };
  }"""),
    ("""function h$closedir(d,o) {
  if(!h$isNode()) {
    throw "h$closedir unsupported";
  }""",
     """function h$closedir(d,o) {
  if(!h$isNode()) {
    h$setErrno('EPERM'); return -1;
  }"""),
    # readdir returns a dirent (NULL = end-of-directory / error + errno).
    ("""function h$readdir(d,o) {
  if(!h$isNode()) {
    throw "h$readdir unsupported";
  }""",
     """function h$readdir(d,o) {
  if(!h$isNode()) {
    h$setErrno('EPERM'); { h$ret1 = (0); return (null); };
  }"""),
    ("""function h$__hscore_readdir(d,o,dst_a,dst_o) {
  if(!h$isNode()) {
    throw "h$readdir unsupported";
  }""",
     """function h$__hscore_readdir(d,o,dst_a,dst_o) {
  if(!h$isNode()) {
    h$setErrno('EPERM'); return -1;
  }"""),
    ("""function h$mkdir(path, path_offset, mode) {
  if (!h$isNode()) {
    throw "h$mkdir unsupported";
  }""",
     """function h$mkdir(path, path_offset, mode) {
  if (!h$isNode()) {
    h$setErrno('EPERM'); return -1;
  }"""),
    ("""function h$js_futimes(fd,atime,mtime) {
  if (!h$isNode()) {
    throw "h$js_futimes unsupported";
  }""",
     """function h$js_futimes(fd,atime,mtime) {
  if (!h$isNode()) {
    h$setErrno('EPERM'); return -1;
  }"""),
    ("""function h$js_utimes(path,path_offset,atime,mtime) {
  if (!h$isNode()) {
    throw "h$js_utimes unsupported";
  }""",
     """function h$js_utimes(path,path_offset,atime,mtime) {
  if (!h$isNode()) {
    h$setErrno('EPERM'); return -1;
  }"""),
    ("""function h$js_lutimes(path,path_offset,atime,mtime) {
  if (!h$isNode()) {
    throw "h$js_lutimes unsupported";
  }""",
     """function h$js_lutimes(path,path_offset,atime,mtime) {
  if (!h$isNode()) {
    h$setErrno('EPERM'); return -1;
  }"""),

    # --- bounds-tolerant DataView for static literals (see item 3) -------
    ("""function h$rawStringData(str) {
    var v = h$newByteArray(str.length+1);
    var u8 = v.u8;
    for(var i=0;i<str.length;i++) {
       u8[i] = str[i];
    }
    u8[str.length] = 0;
    return v;
}""",
     """function h$rawStringData(str) {
    var v = h$newByteArray(str.length+1);
    var u8 = v.u8;
    for(var i=0;i<str.length;i++) {
       u8[i] = str[i];
    }
    u8[str.length] = 0;
    var dv = v.dv, n = dv.byteLength;
    v.dv = {
      getUint8:   function(o)    { return (o>=0 && o+1<=n) ? dv.getUint8(o)      : 0; },
      getInt8:    function(o)    { return (o>=0 && o+1<=n) ? dv.getInt8(o)       : 0; },
      getUint16:  function(o,le) { return (o>=0 && o+2<=n) ? dv.getUint16(o,le)  : 0; },
      getInt16:   function(o,le) { return (o>=0 && o+2<=n) ? dv.getInt16(o,le)   : 0; },
      getUint32:  function(o,le) { return (o>=0 && o+4<=n) ? dv.getUint32(o,le)  : 0; },
      getInt32:   function(o,le) { return (o>=0 && o+4<=n) ? dv.getInt32(o,le)   : 0; },
      getFloat32: function(o,le) { return (o>=0 && o+4<=n) ? dv.getFloat32(o,le) : 0; },
      getFloat64: function(o,le) { return (o>=0 && o+8<=n) ? dv.getFloat64(o,le) : 0; },
      getBigInt64:  function(o,le) { return (o>=0 && o+8<=n) ? dv.getBigInt64(o,le)  : 0n; },
      getBigUint64: function(o,le) { return (o>=0 && o+8<=n) ? dv.getBigUint64(o,le) : 0n; },
      setUint8:   dv.setUint8.bind(dv),
      setInt8:    dv.setInt8.bind(dv),
      setUint16:  dv.setUint16.bind(dv),
      setInt16:   dv.setInt16.bind(dv),
      setUint32:  dv.setUint32.bind(dv),
      setInt32:   dv.setInt32.bind(dv),
      setFloat32: dv.setFloat32.bind(dv),
      setFloat64: dv.setFloat64.bind(dv),
      setBigInt64:  dv.setBigInt64.bind(dv),
      setBigUint64: dv.setBigUint64.bind(dv),
      byteLength: n,
      buffer: dv.buffer
    };
    return v;
}"""),
]

with open(PATH, encoding='utf-8') as f:
    src = f.read()

applied = skipped = 0
for old, new in REPLACEMENTS:
    if new in src:          # this site already patched (idempotence)
        skipped += 1
        continue
    n = src.count(old)
    if n != 1:
        sys.exit(f'{PATH}: expected exactly 1 occurrence of:\n{old}\nfound {n} — '
                 'RTS shim text changed (GHC upgrade?); update patch-rts.py')
    src = src.replace(old, new)
    applied += 1

if applied:
    with open(PATH, 'w', encoding='utf-8') as f:
        f.write(src)
print(f'{PATH}: {applied} sites patched, {skipped} already patched')
