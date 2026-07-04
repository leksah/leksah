# jsaddle-terminal

Run a jsaddle (reflex-dom) app from inside a tmux pane, so IDE panes can be
plain executables that also work in plain old tmux.

## How it works

`Language.Javascript.JSaddle.Terminal.run :: JSM () -> IO ()`

1. **Handshake.** The exe puts its tty in raw mode and emits
   `ESC ] 5799 ; HELLO ; base64(json) BEL` on stdout — invisible on a normal
   terminal, but delivered verbatim to any tmux control-mode client
   (leksah) as `%output`.
2. **Tunnel (leksah answered).** jsaddle `Batch`/`Results` JSON flows over
   the pane's own stdin/stdout: batches as OSC-5799 frames on stdout, and
   results injected into stdin by the IDE via `send-keys -H` as
   `RS TYPE ; base64 LF` lines. The UI renders in an iframe hosted by the
   IDE, whose JS runtime is jsaddle's stock `runBatch` wired to
   `postMessage`. Synchronous callbacks (needed for `preventDefault`) use a
   blocking XHR from the iframe to the **IDE's own local** web server, which
   answers by round-tripping over the pane's stdin/stdout. The machine
   running the exe never needs a reachable socket — it works over
   ssh-reached tmux servers through the IDE's existing connection.
3. **Fallback (nobody answered, ~3s).** The tty is restored and the app is
   served by jsaddle-warp on a free port; a clickable URL (OSC-8 hyperlink)
   is printed.

## Protocol

See `Language.Javascript.JSaddle.Terminal.Protocol` for the frame grammar
and incremental scanners (shared by the exe and the IDE side).

exe → IDE (stdout): `HELLO`, `BATCH`, `SYNCR`, `LOG`, `BYE`
IDE → exe (stdin): `ACK`, `RESULTS`, `SYNC`, `CLOSE`

## Demo

`jsaddle-terminal-demo` — **Breakout**.  The whole game (state, physics,
collisions) runs in Haskell/reflex; only the rendered DOM crosses the tunnel,
and deliberately lightly: a 30 fps tick moves the ball (one element's style
per frame), the paddle updates only while an arrow is held, and a brick's div
is removed the frame it's hit.  Controls: ← → (or A/D) to move, Space (or
click) to launch / restart.  Arrow keys carry no modifier, so the tunnel's
key-forwarding leaves them in the game rather than routing them to leksah's
global shortcuts — while ⌘/⌃ chords still reach leksah.

Debug tracing: set `JSADDLE_TERMINAL_DEBUG=/path/to/log` to append
handshake/dispatch events (stderr is invisible while the tty is raw).

## Known limitations

- **IDE restart mid-session**: the app's jsaddle state cannot be re-adopted
  by a fresh IDE. An app that keeps emitting (e.g. has timers pending)
  is told to `CLOSE` by the new IDE and exits cleanly back to the shell;
  an app that is idle-blocked stays as a silent process — Ctrl-C it
  (handled even in raw mode) and rerun. A future protocol rev could
  re-run the app's `JSM` entry point on re-handshake.
- **`move-pane` across windows** re-keys the IDE-side pane widget; the
  tunnel dies with it (the app can be rerun).
- Stray `RS` (0x1e) bytes typed into a tunnelled pane's stdin could be
  misparsed as frame starts (swallowing bytes to the next newline).
