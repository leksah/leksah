# The leksah.github.io site root as a nix derivation (`nix build '.#leksah-website'`): the homepage plus the in-browser demo
# (leksah compiled with the GHC JS backend) and the breakout game it plays in
# a browser pane.  The output is the exact directory to mirror to the
# leksah/leksah.github.io repo (or serve locally:
# `python3 -m http.server -d result`).
#
# The orchestration is Haskell, not bash: docs/website/try/assemble-site.hs
# (boot libraries only) is compiled here with the project's own native GHC —
# no cabal plan involvement — and does the whitelisted copying, the
# page-seed generation (GenXtermCss/GenDemoFiles/GenDemoTerminals) and the
# RTS patching (PatchRts; strict for leksah.js, lenient for the smaller
# breakout.js).  demo-hovers.js is committed, not regenerated: its generator
# needs a live haskell-language-server over the real repo
# (try/gen-demo-hovers.hs, run by hand from the dev shell).
#
# The heavy inputs are the two `javascript-unknown-ghcjs` cross builds.
# Beware: that cross plan has a history of nixpkgs/emscripten trouble under
# `sandbox = true`; the leksah dev machines run the daemon with
# `sandbox = false`.
{ pkgs, src, leksah-js, breakout-js }:
let
  # The project's own native compiler (already in every dev store) — the
  # generators are boot-libraries-only, so no cabal plan is involved.
  ghc = pkgs.buildPackages.haskell-nix.compiler.${pkgs.hixProject.pkg-set.config.compiler.nix-name or "ghc914-sh"};
  # GHC's darwin link step shells out to `otool` (to read the produced binary's
  # load commands) and that lives in cctools, which this runCommand's stdenv
  # does not put on PATH.  Expose ONLY otool: cctools also ships an `ld`/`as`
  # that must not shadow the wrapped ones the cc-wrapper expects.
  otool-only = pkgs.runCommand "otool-only" { } ''
    mkdir -p $out/bin
    ln -s ${pkgs.cctools}/bin/otool $out/bin/otool
  '';
  site-gen = pkgs.runCommand "leksah-site-gen" {
    nativeBuildInputs = [ ghc ]
      ++ pkgs.lib.optional pkgs.stdenv.hostPlatform.isDarwin otool-only;
    buildInputs = [ pkgs.libiconv ];
  } ''
    mkdir -p $out/bin build
    cd build
    cp ${src}/docs/website/try/*.hs .
    # ghc-internal's PrelIOUtils/iconv objects reference iconv_open / iconv /
    # iconv_close / locale_charset, which on darwin live in libiconv rather
    # than in libc, and ghc's settings link `-liconv`.  buildInputs alone does
    # not put it on the link line here, so pass the search path explicitly —
    # without it every Haskell link in this derivation dies with
    # "ld64.lld: error: library not found for -liconv".  (The dev shell has it
    # on NIX_LDFLAGS already, which is why runghc-ing the generators by hand
    # has always worked.)
    ghc --make -O assemble-site.hs -optl-L${pkgs.libiconv}/lib \
      -o $out/bin/assemble-site
  '';
in
pkgs.runCommand "leksah-website" {
  nativeBuildInputs = [ site-gen pkgs.buildPackages.closurecompiler ];
  # Closure Compiler's whole-program pass over a ~140 MB input.
  JAVA_TOOL_OPTIONS = "-Xmx20g -Xss64m";
} ''
  assemble-site \
    --repo ${src} \
    --leksah-js ${leksah-js}/bin/leksah \
    --breakout-js ${breakout-js}/bin/breakout \
    --out $out

  # Shrink leksah.js with Closure Compiler ADVANCED_OPTIMIZATIONS (dead-code
  # removal + renaming): ~139 MB -> ~24 MB raw, 8.8 -> 4.4 MB gzipped.  It
  # has to: GitHub (leksah.github.io) rejects any file over 100 MB, and
  # whitespace/local-name minifiers (esbuild, terser) only reach ~130 MB —
  # the bulk is linked-in code nothing calls, which only ADVANCED removes.
  #
  # ADVANCED renames every global and property it isn't told about, so it
  # gets two externs files:
  #   * GHC's RTS externs — exactly what GHC's JS linker writes as
  #     all.externs.js (rtsExterns in GHC.StgToJS.Linker.Linker; the install
  #     keeps only the executable, so it is regenerated here): the closure
  #     payload fields d1..d16384, the STG properties, and node/emscripten
  #     globals;
  #   * docs/website/try/demo.externs.js — names used from outside leksah.js
  #     (shims.js, the patched env functions) and ones defined nowhere.
  # The input's `#!/usr/bin/env node` line goes first (Closure rejects it).
  {
    printf '/** @externs @suppress {duplicate} */\n'
    printf '// GHCJS RTS externs for closure compiler ADVANCED_OPTIMIZATIONS\n\n'
    for i in $(seq 1 16384); do printf '/** @type {*} */\nObject.d%d;\n' "$i"; done
    for f in m f cc t size i n a r s mv; do printf '/** @type {*} */\nObject.%s;\n' "$f"; done
    cat <<'JS'
/** @type {string} */ var __dirname;
/** @const */ var NodeJS = {};
/** @interface */ NodeJS.Stream = function () {};
/** @template THIS @this {THIS} @return {THIS} */ NodeJS.Stream.prototype.on = function() {};
/** @return {boolean} */ NodeJS.Stream.prototype.write = function() {};
/** @interface */ NodeJS.ProcessVersions = function() {};
/** @type {string} */ NodeJS.ProcessVersions.prototype.node;
/** @interface */ NodeJS.Process = function() {};
/** @type {!NodeJS.Stream} */ NodeJS.Process.prototype.stderr;
/** @type {!NodeJS.Stream} */ NodeJS.Process.prototype.stdin;
/** @type {!NodeJS.Stream} */ NodeJS.Process.prototype.stdout;
/** @type {!NodeJS.ProcessVersions} */ NodeJS.Process.prototype.versions;
/** @return {?} */ NodeJS.Process.prototype.exit = function() {};
/** @type {!Array<string>} */ NodeJS.Process.prototype.argv;
/** @type {!NodeJS.Process} */ var process;
/** @extends {Uint8Array} @constructor */ function Buffer(arg1, encoding) {};
/** @return {!Buffer} */ Buffer.alloc = function() {};
/** @type {*} */ var Module;
/** @type {!Int8Array} */ Module.HEAP8;
/** @type {!Uint8Array} */ Module.HEAPU8;
/** @return {number} */ Module.getEmptyTableSlot = function() {};
/** @return {*} */ Module._free = function() {};
/** @return {*} */ Module._malloc = function() {};
/** @type {*} */ var putstr;
/** @type {*} */ var printErr;
/** @type {*} */ var debug;
JS
  } > rts.externs.js
  chmod u+w $out/try $out/try/leksah.js
  sed '1{/^#!/d}' $out/try/leksah.js > leksah.in.js
  closure-compiler --compilation_level ADVANCED --env BROWSER \
    --language_in ECMASCRIPT_NEXT --language_out ECMASCRIPT_2022 \
    --externs rts.externs.js --externs ${src}/docs/website/try/demo.externs.js \
    --warning_level QUIET \
    --js leksah.in.js --js_output_file $out/try/leksah.js
  echo "leksah.js: $(wc -c < leksah.in.js) -> $(wc -c < $out/try/leksah.js) bytes"
''
