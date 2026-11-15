/** @externs */
// Closure Compiler externs for the demo's leksah.js, alongside the GHC RTS
// externs nix/website.nix generates (GHC's own all.externs.js list).  Each
// name here must keep its spelling through ADVANCED_OPTIMIZATIONS because
// something OUTSIDE the compiled file uses it.

// Defined by shims.js (C functions the GHC JS RTS never implemented), and
// h$setErrno, which shims.js calls back into leksah.js.
/** @type {*} */ var h$chdir;
/** @type {*} */ var h$exit;
/** @type {*} */ var h$geteuid;
/** @type {*} */ var h$getpwuid_r;
/** @type {*} */ var h$mktime;
/** @type {*} */ var h$readlink;
/** @type {*} */ var h$setErrno;
/** @type {*} */ var h$sysconf;

// Referenced by leksah.js but defined nowhere: RTS primitives GHC's JS
// backend never implemented, and implicit globals in libraries' JS code.
// Unminified they would throw a ReferenceError only if reached; declared
// here they stay exactly that, instead of failing the compile.
/** @type {*} */ var MK_INTEGER_S;
/** @type {*} */ var a;
/** @type {*} */ var h$eq_thread;
/** @type {*} */ var h$getRTSStats;
/** @type {*} */ var h$getRTSStatsEnabled;
/** @type {*} */ var h$ghcjsbn_readInteger;
/** @type {*} */ var h$realloc;
/** @type {*} */ var h$stg_absentErrorzh;
/** @type {*} */ var h$stg_paniczh;
/** @type {*} */ var h$stg_sendCloneStackMessagezh;
/** @type {*} */ var ls;
/** @type {*} */ var ret;

// Read by the patched RTS env functions (patch-rts.hs) as
// globalThis.leksahDemoEnv; shims.js sets it.
/** @type {*} */
Object.leksahDemoEnv;
