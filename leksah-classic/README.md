# leksah-classic

The classic GTK3 front end of the leksah IDE, split out of the main package
(2026-08) so the new web-UI leksah can be relicensed (see
`../docs/relicensing.md`).

**This is a frozen fork.**  Its `src/` tree contains this package's own GPLv2
copies of the modules that used to be shared with the web UI (`IDE.Core.*`,
`IDE.Package`, `IDE.Workspaces`, …).  The main package's versions of those
modules are being rewritten and will drift immediately — do not expect the
two trees to stay in sync, and do not "helpfully" re-share them: the split
exists precisely so they can evolve under different licenses.

Build and run from the repository root:

```
cabal build leksah-classic
./leksah.sh --classic
```

License: GPL-2.0-or-later (see `LICENSE`).
