# leksah-classic

The classic GTK3 front end of the leksah IDE, split out of the main package
(2026-08) so the new web-UI leksah can be relicensed (see `docs/relicensing.md`
in the leksah repository).

**This is a frozen fork.**  Its `src/` tree contains this package's own GPLv2
copies of the modules that used to be shared with the web UI (`IDE.Core.*`,
`IDE.Package`, `IDE.Workspaces`, …).  The main package's versions of those
modules are being rewritten and will drift immediately — do not expect the
two trees to stay in sync, and do not "helpfully" re-share them: the split
exists precisely so they can evolve under different licenses.

**This directory is a self-contained project.**  It has its own
`cabal.project`, `flake.nix`/`flake.lock`, `nix/hix.nix` and `hie.yaml`, and its
own copy of the packages it needs (`vendor/gi-gtkosxapplication`), so it can be
lifted into its own repository unchanged.  Nothing in the parent project refers
to it — build it from *this* directory, not from the repository root:

```
cd leksah-classic
nix develop                                    # haskell.nix dev shell (GTK3 deps)
cabal build leksah-classic:exe:leksah-classic
nix build .#leksah-classic:exe:leksah-classic
```

The flake pins the same compiler this package was built with while it still
lived in the main project's plan (`ghc914-sh`, the stable-haskell fork), and
carries the fork workarounds it needs: the `cabal-doctest` patch for
haskell-gi's custom Setup, the `libyaml-clib -no-rts` flag, and the
`gi-*:setup.Cabal` pins.  Its own frozen bounds (`base <4.20`,
`containers <0.7`) are lifted by the `allow-newer` block in `cabal.project`,
exactly as they were before the split.

License: GPL-2.0-or-later (see `LICENSE`).
