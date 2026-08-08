# Debugging web-UI freezes and deadlocks

The web UI (`src/IDE/Web`, front ends `leksah-wkwebview` / `leksah-warp` /
`leksah-webkitgtk`) runs **one reflex network per OS window**, all sharing a
single `IDE` behind an `MVar` (`ideR`). A "freeze" is one window going dead
while the others keep running — not the whole process. This guide covers the
tooling built for diagnosing that, and a step-by-step method.

**Two different failures wear the same face.** Both leave a window that does
not repaint while `leksah-cmd ping` still answers `ok`, so tell them apart
first — the whole method below branches on it:

| | **wedged frame** | **dead window** |
|---|---|---|
| `threads \| grep reflex-frames` | `WindowId N` present, `BlockedOnMVar` | **`WindowId N` absent** |
| heartbeat | ticked, then stopped | usually **never ticked at all** |
| DOM | last good render, stale | blank, or a stale render from before |
| cause | a `performEvent` handler blocking on the frame thread | the window's *initial build* threw or never returned |

The frame thread is labelled by the heartbeat handler, which runs on it, and
`guardedAsyncEvents` is forked only after the build returns — so **a missing
thread means the build, not a frame**. Jump to
[When the build never finished](#when-the-build-never-finished) for that case.

All the live-inspection commands are subcommands of `leksah-cmd` (the control
socket, `src/IDE/Web/CmdServer.hs`); they answer even while a window's reflex
network is wedged, because the command server runs on its own thread.

## The tools

### Heartbeat log — the freeze detector

Each window emits, once per second, to stderr (which the `leksah-nix.sh` loop
tees to `~/.leksah/leksah-nix-wkwebview.log`):

```
LEK 03:21:14.684 [win 0] alive ideVer=12 mvarVer=13 STALE(+1)
```

- `ideVer` — the shared-state version this window has *applied*.
- `mvarVer` — the latest version in the shared `ideR`.
- `STALE(+n)` — this window is `n` versions behind. A **transient** `STALE(+n)`
  that recovers on the next tick is normal (the window is catching up under
  load). A window whose heartbeat line **stops appearing**, or whose `STALE`
  grows without recovering, has **frozen**.

Emitted from the per-window heartbeat `performEvent` in `IDE.Web.Main`
(`tickLossyFromPostBuildTime 1`). Quick check:

```sh
grep "alive ideVer" ~/.leksah/leksah-nix-wkwebview.log | tail -20
grep "alive ideVer" ~/.leksah/leksah-nix-wkwebview.log | tail -40 \
  | grep -oE "\[win [0-9]+\]" | sort -u        # which windows are still ticking
```

### `leksah-cmd threads`

Lists every RTS thread as `ThreadId N  <label>  <status>`. The labelled
threads (set via `labelThread`) are the ones that matter:

- `reflex-frames-WindowId N` — window `N`'s reflex frame thread. **This is the
  one that wedges.** A frozen window = this thread `ThreadBlocked BlockedOnMVar`
  *and* its heartbeat stopped. (Note: an *idle* frame thread is *also*
  `BlockedOnMVar` — waiting for its next event — so the status alone doesn't
  prove a wedge; correlate with the heartbeat.)
  **A window with no `reflex-frames-WindowId N` line at all is a different
  bug** — its build never finished, so the loop was never forked. Count the
  lines against the number of OS windows before reading anything else.
- `resync-notifier-WindowId N` — the per-window cross-window-resync notifier.
- `bridge-drain-*` — the native→reflex request drains (`IDE.Web.WindowBridge`).

```sh
leksah-cmd threads | grep -E "reflex-frames|resync-notifier|bridge-drain"
```

### `leksah-cmd stacks [SUBSTR]`

Clones and decodes the call stack of the labelled long-running threads (or, with
`SUBSTR`, every labelled thread whose label contains it). Output per thread:

```
== ThreadId 1471  reflex-frames-WindowId 1  ThreadBlocked BlockedOnMVar
     <function>  (<module> <srcloc>)
```

**Caveats — this rarely pins a frame-thread block:**

- Source locations need the code built with `-finfo-table-map`
  (`cabal.project.local` sets it for `reflex`, `reflex-dom-core`, `jsaddle`,
  `jsaddle-wkwebview`; nix builds otherwise strip it).
- Even *with* the map, a thread parked in a `forever`/tail-recursive loop's
  `takeMVar` has a **one-frame** stack, so a wedged frame thread typically shows
  only a single module (`Reflex.Dom.Main`) — the same as an *idle* one. Stacks
  are good for confirming *which* labelled thread is blocked, not *where in
  leksah's code*. For that, use **instrumentation** (below).

### `leksah-cmd resync-state`

Per-window occupancy of the cross-window resync signal/ack `MVar`s:

```
WindowId 0 sig=FULL ack=empty
WindowId 1 sig=FULL ack=empty
```

The wedged window shows `sig=FULL ack=empty` — a resync was signalled but its
frame never ran to acknowledge it. Because the notifier holds a process-global
lock across fire→ack (`resyncGlobalLock` in `IDE.Web.WindowBridge`), **every**
window's resync then stalls, so they can all read `sig=FULL`. This is a reliable
*symptom* that a frame is wedged — but the resync stall is a **cascade**, not
the root cause. Don't stop here.

### `leksah-cmd js eval` — the transport-liveness probe

`js eval 'CODE'` runs the JS in **every** live jsaddle context. The key
diagnostic trick:

```sh
leksah-cmd js eval 'Math.random()'
```

- **Two (or N) distinct values** → every window's jsaddle transport is alive and
  servicing round-trips → the freeze is **not** jsaddle and **not** the
  batch serializer; it is a **Haskell-side app-`MVar` block** in the frozen
  window's frame. This is the common case.
- **A missing value / the call hangs** → that context's jsaddle is stuck.
- **All windows unresponsive together** → suspect a *global* stall (e.g. the
  jsaddle batch serializer held by one wedged context — see
  `runJavaScriptWithSerializer`).

### Browser devtools on a wedged wkwebview window

When the Haskell frame wedges, the WKWebView **WebContent process stays alive** —
right-click ▸ Inspect Element and the JS debugger still work. Useful to *confirm
the JS side is healthy*, but it **cannot** observe the Haskell-side `MVar`.

> Pitfall: jsaddle closures retain the `runJSaddleBatch(batch)` scope, so a
> `batch` variable visible in the debugger is usually the **stale batch that
> installed a callback**, not a batch executing now. Don't read a large retained
> `batch` as evidence of a live build.

### `wlog` ENTER/EXIT instrumentation — the reliable way to pin it

`wlog :: MonadIO m => WindowId -> String -> m ()` (`IDE.Web.Main`) writes
`LEK <ts> [win N] <msg>` to the **same** stream as the heartbeats, so markers
interleave with them. Wrap a suspect frame `performEvent` handler:

```haskell
performEvent_ $ ffor someE $ \x -> do
    wlog wid ("ENTER myHandler " <> show x)
    liftIO (theSuspectAction x)
    wlog wid "EXIT myHandler"
```

After a freeze, the frozen window's **last `ENTER` with no matching `EXIT`**,
right where its heartbeat stopped, names the blocking handler. Several handlers
already carry markers (`flipBump`, `flipMirror write`, `wide0Activate`,
`ideD<-resync`, `ENTER/EXIT resync`, `ENTER/EXIT ideAction`, …).

## Step-by-step: debugging a freeze

1. **Confirm and scope it.** Tail the heartbeats; identify which `[win N]`
   stopped ticking while others continue. `leksah-cmd ping` should still return
   `ok` (the socket/command thread is independent of the frozen network).

2. **Categorise the cause** with `leksah-cmd js eval 'Math.random()'`:
   - Distinct values from every context → Haskell-side app-`MVar` block in the
     frozen frame (go on). It is *not* jsaddle/serializer.
   - A context hangs, or all freeze together → a jsaddle/serializer stall;
     investigate the batch transport instead.

3. **Find the wedged thread.** `leksah-cmd threads | grep reflex-frames` → the
   frozen window's `reflex-frames-WindowId N` is `BlockedOnMVar`.
   `leksah-cmd resync-state` will show it `sig=FULL ack=empty` (symptom).
   **If that window has no line here at all, stop** — nothing below applies;
   go to [When the build never finished](#when-the-build-never-finished).

4. **Rule out the usual `MVar`s** before instrumenting:
   - **`ideR`** (the shared `IDE`): if *another* window's heartbeat still
     advances, `ideR` is free — the heartbeat does `readMVar ideR` every second —
     so the block is *not* a held `ideR`, and it is not inside a `modifyIDE`
     (that would hold `ideR` and freeze everyone).
   - **tmux**: `tmux -L leksah list-windows -a` should answer instantly, and
     `ps | grep 'tmux -L leksah'` should show no long-lived stuck client — a
     blocked `readProcessWithExitCode` (e.g. `selectTmuxPane`) parks in
     `waitForProcess`, which reads as `BlockedOnMVar`.
   - **jsaddle**: ruled out in step 2.

5. **Pin the exact handler with instrumentation** (stacks are too shallow):
   - Add `wlog wid "ENTER …"/"EXIT …"` around the candidate frame `performEvent`
     handlers. Good coverage set: the startup path (`ideAction`/`reflectIDE`,
     session `restore`, `pb-initInfo`) and the flip/terminal handlers (the
     `selectTmuxWindow`/`selectTmuxPane` sites, the flip-mirror read).
   - Rebuild in place: `leksah-cmd rebuild-self --use-cabal` (restarts on
     success). Set the status light around it (below).
   - Reproduce. Then:
     ```sh
     grep "\[win N\]" ~/.leksah/leksah-nix-wkwebview.log \
       | grep -iE "ENTER|EXIT" | tail
     ```
     The last unpaired `ENTER` is the blocking call.

6. **Fix.** A `performEvent` handler runs on the frame thread, so it must never
   make an **unbounded blocking call**: no `takeMVar` on an `MVar` that may never
   fill, no blocking `readProcessWithExitCode`, no reply-correlated
   `ccCommand` (see `IDE.Web.TmuxCC`). Move the work off the frame with `forkIO`,
   or bound it with a timeout.

## When the build never finished

No `reflex-frames-WindowId N` thread means `attachImmediateWidget` — which
builds the window's entire widget tree and fires its post-build frame — never
returned, so `guardedAsyncEvents` was never forked. reflex commits the built
DOM only at the end, so that window shows nothing (or whatever a previous run
left behind). Everything else keeps working: the process, the command socket,
and that window's own jsaddle context, which is why `ping` and
`js eval 'Math.random()'` both look healthy.

`IDE.Web.Attach` reports the throwing case as

```
LEK … [win 1] WINDOW BUILD FAILED (the reflex network never built; …): <exception>
```

and it also reaches the Log pane of the windows that *do* work. If the build
**hung** instead of throwing there is no report at all — the missing thread is
the only signal.

### The known cause: two windows building at once

Every window has its own reflex network, but they all share **one Spider
timeline** (`SpiderTimeline Global` — a top-level `unsafePerformIO`
environment in `Reflex.Spider.Internal`: one height bag, one delayed-merge
queue, one propagation depth). reflex takes that timeline's mutex around
event *propagation* (`run`) but not around `runFrame`, which is what
`runHostFrame` / `hold` / `holdDyn` / `buildDynamic` / `subscribeEvent` /
`sample` all go through — i.e. all of building a widget tree. reflex's own
source says as much:

```haskell
runFrame :: … --TODO: This function also needs to hold the mutex
```

Two windows building simultaneously — or one building while another
propagates — therefore interleave writes to that shared state. The window
that loses fails in whichever way the corruption happens to show up:

```
merge: accumRef not yet initialized                    (Spider/Internal.hs:2152)
heightBagRemove: Height 18 not present in bag …        (Spider/Internal.hs:1135)
causality loop detected                                (EventLoopException)
```

…or with no exception at all, just a build that never returns. All four were
observed on four consecutive boots of the same two-window session.

This is why it looks intermittent: a window opened later, by hand, builds
while the others are idle and usually gets away with it. A **multi-window
session restore** attaches them in the same millisecond and hits it reliably.

`IDE.Web.Attach.frameLock` — one process-global `MVar` held around each build
and each frame batch — makes the timeline single-threaded again. It is the
outer lock (reflex's own mutex is only ever taken inside propagation, never
around a build), so there is no lock-order inversion. The cost is that a
blocking frame-thread handler now stalls *every* window rather than its own,
which makes step 6 above matter more, not less.

If you ever see these Spider errors again, suspect something that runs a
reflex frame outside `IDE.Web.Attach` — that is the only place holding the
lock.

## Status light (courtesy while debugging)

The top-right traffic light tells the user when to keep hands off. Drive it over
the socket:

- `leksah-cmd js eval 'leksahRestarting()'` — blue diamond, around a
  rebuild/restart.
- `leksah-cmd js eval 'leksahStatus("red")'` (or `leksahTestStart()`) — while
  interactively testing.
- `leksah-cmd js eval 'leksahStatus("green")'` (or `leksahTestEnd()`) — safe to
  use again.

## Watching for a freeze live

The freeze can be intermittent (a startup/interaction race). To catch it as it
happens, tail the log for the freeze signature, filtering out the always-paired
high-frequency markers (`resync`, `ideAction`) to keep the noise down:

```sh
tail -n0 -f ~/.leksah/leksah-nix-wkwebview.log \
  | grep --line-buffered -iE "ENTER (flipPaneE|selectWinE|selectPaneE|restore|pb-initInfo)|STALE\(\+([3-9]|[0-9]{2})"
```

A window falling several versions behind and going quiet, or a blocking-handler
`ENTER` with no `EXIT`, is the moment to grab `threads` / `resync-state`.
