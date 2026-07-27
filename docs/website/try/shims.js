// Shims for C functions the GHC JS RTS never implemented (found by a static
// scan of leksah.js: every `h$xxx(` call with no `function h$xxx`
// definition).  Two flavours:
//
//   * errno-style failures / trivial values — Haskell sees an ordinary
//     catchable IOException instead of an uncatchable JS ReferenceError
//     (the failure mode that killed the first boots: directory's
//     getHomeDirectory falls back to the passwd database when HOME is
//     unset, and h$geteuid didn't exist);
//
//   * named throws for paths that cannot meaningfully run in a browser
//     (stack-snapshot forensics, profiling CCS, libdw) — if one ever fires
//     the console shows exactly what was reached instead of a bare
//     ReferenceError.
//
// Load this BEFORE leksah.js.

(function () {
  'use strict';
  const g = globalThis;

  // The browser "environment" (read by the RTS env functions after
  // patch-rts.hs rewrites them; see that file).  HOME in particular:
  // without it directory's getHomeDirectory falls back to the passwd
  // database, which doesn't exist here either.
  g.leksahDemoEnv = g.leksahDemoEnv || {
    HOME: '/home/leksah',
    USER: 'leksah',
    LANG: 'en_US.UTF-8',
    PATH: '/usr/bin:/bin',
    TMPDIR: '/tmp',
  };

  // errno helper — leksah.js defines h$setErrno by the time any of these run.
  // NB h$setErrno only knows the errno names in its lookup table (EPERM,
  // EINVAL, ENOENT, …) and THROWS for anything else (e.g. ENOSYS) — stick
  // to names from that table.
  const fail = (name, errno) => function () {
    if (typeof g.h$setErrno === 'function') g.h$setErrno(errno);
    return -1;
  };

  // ---- passwd database / identity -----------------------------------
  g.h$geteuid   = () => 0;
  // getpwuid_r returns an error NUMBER directly (not via errno); any
  // non-zero return makes unix's getUserEntryForID throw a catchable
  // IOException.  2 = ENOENT.
  g.h$getpwuid_r = () => 2;

  // ---- misc POSIX ----------------------------------------------------
  // chdir pretends to succeed: there is no real cwd in a browser, the mock
  // FS (IDE.Web.FS) keys everything by absolute path anyway, and failing
  // with EPERM aborted the workspace load (activatePackage chdirs into the
  // active package's directory).
  g.h$chdir    = () => 0;
  g.h$readlink = fail('readlink', 'EINVAL');
  g.h$sysconf  = fail('sysconf',  'EINVAL');
  g.h$mktime   = fail('mktime',   'EINVAL');   // old-time; revisit if dates misbehave
  g.h$exit     = (code) => { throw new Error('h$exit(' + code + ') called in browser'); };

  // ---- named-throw stubs ---------------------------------------------
  const notInBrowser = (name) => function () {
    throw new Error(name + ': not available in the browser build');
  };
  [
    'h$realloc',
    'h$ghcjsbn_readInteger',
    'h$hs_XXH3_64bits_withSeed_offset',
    // stack snapshot / decode (leksah-cmd stacks forensics; server stubbed)
    'h$advanceStackFrameLocationzh',
    'h$getInfoTableAddrszh',
    'h$getLargeBitmapzh',
    'h$getRetFunLargeBitmapzh',
    'h$getRetFunSmallBitmapzh',
    'h$getSmallBitmapzh',
    'h$getStackClosurezh',
    'h$getStackFieldszh',
    'h$getStackInfoTableAddrzh',
    'h$getUnderflowFrameNextChunkzh',
    'h$getWordzh',
    'h$isArgGenBigRetFunTypezh',
    'h$stg_cloneMyStackzh',
    // profiling / cost centres
    'h$mkCCS', 'h$registerCCS', 'h$setCCS',
    'h$libdwGetBacktrace', 'h$libdwLookupLocation', 'h$libdwPoolTake',
    'h$registerExtensibleRetensionRoot',
    // rts panics
    'h$stg_absentErrorzh', 'h$stg_paniczh',
  ].forEach((n) => { if (typeof g[n] === 'undefined') g[n] = notInBrowser(n); });
})();

// Open the keyboard-shortcuts cheat sheet (⌘/) once the UI is up, so the
// demo greets visitors with the key bindings visible.  ShortcutsKey is
// deliberately excluded from session save/restore ("transient, never
// restore" — IDE.Web.Main), so a canned web-session.json can't do this;
// instead synthesize the ⌘/ keydown the web keymap listens for
// (document-level, reads .keyCode — a synthetic event passes, but keyCode
// must be installed via getter because the KeyboardEvent constructor
// discards it).  Retries until the pane's div.shortcuts exists: a dispatch
// that lands before the keymap widget is wired is simply lost.
(function () {
  'use strict';
  var t0 = Date.now();
  var timer = setInterval(function () {
    if (document.querySelector('.shortcuts') || Date.now() - t0 > 90000) {
      clearInterval(timer);
      return;
    }
    if (!document.querySelector('.workspace')) return; // UI not built yet
    var e = new KeyboardEvent('keydown', { metaKey: true, bubbles: true });
    Object.defineProperty(e, 'keyCode', { get: function () { return 191; } }); // '/'
    document.dispatchEvent(e);
  }, 500);
})();
