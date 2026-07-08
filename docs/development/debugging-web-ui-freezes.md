# Debugging web-UI freezes and deadlocks

The web UI (`src/IDE/Web`, front ends `leksah-wkwebview` / `leksah-warp` /
`leksah-webkitgtk`) runs **one reflex network per OS window**, all sharing a
single `IDE` behind an `MVar` (`ideR`). A "freeze" is almost always **one
window's reflex *frame thread* blocked on an `MVar`** while the others keep
running — not the whole process. This guide covers the tooling built for
diagnosing that, and a step-by-step method.

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
