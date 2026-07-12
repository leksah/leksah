# Recording leksah demo animations for the website

Short, autoplaying, looping clips of leksah in action — for the homepage hero,
feature blurbs, the README. This produces **MP4 + VP9-WebM + an optimized GIF**
from one scripted browser interaction, all tiny enough to inline on a page.

## TL;DR

```bash
# No leksah needed — proves your machine's record→encode toolchain works:
docs/website/record-demo.sh selftest

# Record the real UI (needs a leksah-warp instance — see "Recording real leksah"):
LEKSAH_URL=http://127.0.0.1:3367/ docs/website/record-demo.sh tour
```

Outputs land in `docs/website/media/<name>.{mp4,webm,gif}`.

## Why this approach

leksah-warp is the **browser** front end (same UI as the macOS/GTK builds,
served at `http://127.0.0.1:3367/`). Because it's a real web page it can be
driven by ordinary browser automation and captured deterministically — no
screen-recording of a positioned window, no OS cursor artifacts, no variable
frame timing.

The chosen pipeline is **Playwright (headless Chromium) → WebM → ffmpeg/gifski**:

- **Deterministic & scriptable.** The same clicks produce the same clip every
  run; re-record after a UI change with one command.
- **Crisp text.** We record at a large CSS viewport (default 1440×900) and
  downscale to ≤1280px with lanczos, so type stays sharp on retina displays.
- **Tiny, web-native output.** An `<video autoplay muted loop playsinline>`
  with MP4 + WebM sources is a few tens of KB (vs. multi-MB GIFs). The GIF is
  there only as a fallback for contexts that can't use `<video>` (some README
  renderers, email).

Recommended embed:

```html
<video autoplay muted loop playsinline width="640" poster="breakout.jpg">
  <source src="breakout.webm" type="video/webm">
  <source src="breakout.mp4"  type="video/mp4">
</video>
```

Alternatives considered and rejected: `ffmpeg -f avfoundation` screen capture
(needs a pixel-positioned window + screen-recording permission, non-repeatable,
captures OS chrome); asciinema (terminal only — can't show the editor/tree/LSP).

## Setup

Nothing to install globally. `record-demo.sh` pulls `nodejs`, `ffmpeg` and
`gifski` via `nix shell` on demand, and caches Playwright's Chromium under
`~/.cache/leksah-rec` (override with `PW_BROWSERS`). The repo stays free of
`node_modules` — the driver is staged into that cache dir at run time.

## Stories

`record-demo.sh <story>`:

| story | needs leksah? | what it shows |
|-------|---------------|---------------|
| `selftest` | no | A bundled stand-in page (`standin.html`) themed as the breakout demo — types out `Breakout.hs`, then runs a canvas breakout game. Proves the toolchain and is a decent placeholder clip. |
| `tour` | yes | A gentle, reversible tour of the live UI: switches the Workspace/Terminals side tabs and the Errors/Log/Grep bottom tabs. |
| `breakout` | yes | Opens `sandpit/breakout`'s source in the editor and runs it in a terminal. Selector names are best-effort — see below. |

## Recording real leksah (`tour` / `breakout`)

leksah-warp serves an **empty bootstrap page** and builds its entire UI live
over a jsaddle WebSocket; native front ends inject `/jsaddle.js` for you, so the
driver injects it itself and waits for the DOM to actually build.

**Use a DEDICATED warp instance for recording — not your working dev instance:**

1. Each automation connection is a *real leksah window*. On a shared instance
   these accumulate and contend the global reflex lock, eventually wedging
   frames (you'll see the driver's watchdog fire). A fresh instance with no
   other windows records cleanly.
2. The `breakout`/`tour` stories click things and open files — you don't want
   that mutating the workspace you're actually working in.

Launch one just for recording (adjust GHC/UI as needed), pointed at the sandpit:

```bash
# in its own tmux/terminal; serves http://127.0.0.1:3367/
./leksah-nix.sh ghc914 warp
# then open the sandpit workspace in it (or start it with that workspace),
# e.g. via the running instance's control socket:
#   bin/ghc914/leksah-cmd project open sandpit/breakout/breakout.cabal
```

Then record:

```bash
LEKSAH_URL=http://127.0.0.1:3367/ docs/website/record-demo.sh breakout
```

If the driver reports *"leksah UI never built"* or the watchdog fires, the
instance is contended or the workspace isn't open — restart a clean warp
instance and retry.

### Tuning the `breakout` storyline

`record-demo.mjs`'s `storyBreakout` clicks the file basename in the tree, then
the Terminals tab, then waits for the app to play. The workspace-tree entries
and the Run control are matched **by visible text**; once the sandpit exists,
open it once in warp and adjust the `clickText(...)` targets (and `OPEN_FILE`)
to match the actual labels. Set `HEADED=1` to watch the browser while you tune.

## Tuning file size / quality

Env knobs on `record-demo.sh`:

- `MAXW` (default 1280) — output width cap. Drop to `960` or `800` for smaller
  files; the display size on the site can be smaller still for extra sharpness.
- `FPS` (default 24) — lower (20/15) shrinks all three formats.
- `W`/`H` (default 1440×900) — the *recording* viewport. Larger = crisper source
  before downscaling, bigger raw file.
- `GIF=0` — skip the GIF (it's by far the largest output).
- MP4 quality is `-crf 23`, WebM `-crf 32`, GIF `--quality 85` — edit
  `record-demo.sh` to trade size vs. fidelity.

## Output

`docs/website/media/<name>.{mp4,webm,gif}` (plus a `.raw.webm` intermediate you
can delete). Reference sample (`selftest`, 1280×800, 24fps, ~8s): **MP4 ≈ 39 KB,
WebM ≈ 40 KB, GIF ≈ 200 KB.**

## Manual steps a human must do

- **Provide/refresh the sandpit workspace** and, once it exists, tune the
  `breakout` selectors as above.
- **Run a dedicated warp instance** for `tour`/`breakout` (the script does not
  launch or manage leksah — by design, so it never disturbs your dev instance).
- **Pick a poster frame** for the `<video>` if you want one:
  `ffmpeg -sseof -1 -i x.mp4 -vframes 1 x-poster.jpg`.
- **Commit the chosen media** into the website repo/asset pipeline (this script
  only writes them to `docs/website/media`).
