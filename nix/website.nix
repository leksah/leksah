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
  site-gen = pkgs.runCommand "leksah-site-gen" {
    nativeBuildInputs = [ ghc ];
  } ''
    mkdir -p $out/bin build
    cd build
    cp ${src}/docs/website/try/*.hs .
    ghc --make -O assemble-site.hs -o $out/bin/assemble-site
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
