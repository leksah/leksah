# The leksah.org site root as a nix derivation (`nix build
# '.?submodules=1#leksah-website'`): the homepage plus the in-browser demo
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
  nativeBuildInputs = [ site-gen ];
} ''
  assemble-site \
    --repo ${src} \
    --leksah-js ${leksah-js}/bin/leksah \
    --breakout-js ${breakout-js}/bin/breakout \
    --out $out
''
