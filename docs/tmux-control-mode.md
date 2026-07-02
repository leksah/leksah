# tmux control mode (-CC) support for leksah

Status: design + phase-1 client implemented (`src/IDE/Web/TmuxCC.hs`,
tested standalone by `scripts/tmux-cc-test.hs`).  Phase-2 UI wiring sketched
(`src/IDE/Web/Widget/TerminalCC.hs`, unverified).  All protocol claims below
were **verified empirically against tmux 3.6a** (the version in leksah's dev
shell) on a throwaway socket (`-L leksah-cc-test`); transcripts in the
commentary below are real captures.

## 1. Why

Today a leksah terminal is one xterm.js attached to a whole tmux session over
a PTY (`tmux attach-session` in `Widget/Terminal.hs`): tmux paints splits and
borders as characters inside a single screen.  With a control-mode client,
tmux stops painting and instead *tells us what happened*; leksah can render
**each tmux pane as its own xterm.js**, lay panes out with real DOM splits
(like iTerm2's tmux integration), route keystrokes per pane, and replace the
2-second `list-panes` polling (`paneTreeD` in `Web/Main.hs`) with push
notifications.

## 2. The protocol, as measured (tmux 3.6a)

### 2.1 Client invocation

* Use **`-C` (single C)** for programmatic use: `tmux -C -L <socket> attach-session -t <sess>`
  (or `new-session …`).  Plain pipes on stdin/stdout work.
  * `-CC` only adds a `\033P1000p` DCS wrapper for human terminals (iTerm2);
    with a **FIFO/pipe stdin, `-CC` failed** on 3.6a (`tcgetattr failed:
    Operation not supported on socket`) while `-C` works fine.  "tmux -CC
    support" in product terms = a control-mode client; we use `-C`.
* stdin: one tmux command per line.  stdout: the event/reply stream.  Nothing
  interesting arrives on stderr in normal operation.
* On `detach-client` or `kill-server`, the client prints `%exit` and exits 0.

### 2.2 Reply framing and correlation

Every command's reply is bracketed:

```
%begin <epoch> <number> <flags>
<body lines…>
%end   <epoch> <number> <flags>     (success)
%error <epoch> <number> <flags>     (failure; body holds the error text)
```

Measured properties:

* `<number>` increases monotonically per server command; `<flags>` is `1` for
  commands issued by this client, `0` for the **implicit block emitted right
  after attach** (an empty `%begin/%end` pair arrives before anything else —
  a parser must tolerate a reply block with no pending command).
  * **Gotcha (measured)**: the implicit flags=0 block can arrive *after* the
    client has already submitted its first command — FIFO pairing by arrival
    order alone mis-answers that command with the empty block.  Correlate
    only blocks whose flags word has bit 1 set; discard even-flag blocks.
    (This exact race hit the first test run; filtering by flags fixed it.)
* Replies arrive **in the order commands were written** (FIFO).  Correlation
  = pair the n-th completed `%begin…%end/%error` (flags=1) with the n-th
  command sent.  Verified with 20 concurrent `display-message -p` commands
  carrying distinct payloads (see the test).
* Error example (verbatim): body `parse error: unknown command:
  bogus-command-xyz` terminated by `%error …`.

### 2.3 Notifications (asynchronous lines outside %begin blocks)

Observed live on 3.6a:

| line | meaning |
|---|---|
| `%output %<pane> <data>` | pane produced output; `<data>` escaping in §2.4 |
| `%layout-change @<win> <layout> <visible-layout> <flags>` | window layout changed; `<flags>` e.g. `*` (current window) |
| `%window-add @N` | window created in the attached session |
| `%window-close @N` / `%unlinked-window-close @N` | window gone (unlinked = not in this session's current view) |
| `%window-renamed @N <name>` | rename (fires a lot with automatic-rename) |
| `%window-pane-changed @N %P` | active pane of a window changed |
| `%session-changed $N <name>` | this client switched session |
| `%session-window-changed $N @N` | session's current window changed |
| `%sessions-changed` | session list changed (create/kill/rename anywhere on the server) |
| `%session-renamed <name>` | attached session renamed |
| `%pane-mode-changed %P` | pane entered/left a mode (copy-mode) |
| `%exit [reason]` | client is done; stream ends |

**Gotcha (measured): killing a pane emits NO pane-close notification** —
just `%layout-change` (+ `%window-pane-changed`).  Pane lifetime must be
*derived by diffing the pane-id sets of successive layouts*.

**Gotcha (measured): attaching to an existing session replays nothing** —
you get only `%session-changed` (and the implicit empty reply block).  No
`%window-add`, no `%output` backlog.  Initial state sync is the client's
job (§3.3).

With flow control enabled (`refresh-client -f pause-after=N`), `%output` is
replaced by `%extended-output %P <age> … : <data>` plus `%pause %P` /
`%continue %P` events.  We don't enable it in phase 1 (risk noted in §6).

### 2.4 `%output` escaping

Measured on 3.6a: control bytes and backslash arrive octal-escaped, **valid
UTF-8 high bytes arrive raw**:

```
%output %0 e=M-CM-) …          (cat -v; i.e. raw 0xC3 0xA9 for "é")
%output %0 n"\033[?2004l\015\015\012     (ESC, CR, LF as \033 \015 \012)
%output %0 =\134303\1342…                (a *typed* backslash is \134)
```

Decoder rule: `\` followed by exactly three octal digits → that byte; any
other byte → itself.  Keep the result as **ByteString** and hand it to
xterm.js as raw bytes (exactly like today's `window.LeksahTerm.write`
base64→Uint8Array path — xterm does its own stateful UTF-8 decode, so a
UTF-8 sequence split across two `%output` lines is fine).  Older tmux
versions escape ≥0x7F as octal too; the same decoder covers both.

### 2.5 Sending input

* Robust path (used by the client): `send-keys -t %P -H 68 69 0d` — one hex
  byte per argument, no quoting pitfalls, arbitrary bytes.  Chunk long
  pastes (~128 bytes/command) to respect argv limits.
  * Measured: hex bytes must be **separate arguments** (`-H 68 69 0d`), not
    concatenated (`-H 68690d` does nothing useful).
* `send-keys -l '<text>'` works for plain text (UTF-8 round-trips) but needs
  careful quoting of tmux's command-line syntax; `-H` avoids the problem
  entirely.

### 2.6 Resize

The control client has no terminal size; tell tmux one with
`refresh-client -C <W>x<H>` (measured: triggers `%layout-change` re-fitting
windows).  For per-window native layouts, also consider
`set-option -w window-size manual` + `resize-window`.  In leksah, W×H =
the pane area's cell capacity computed from the xterm cell metrics.

### 2.7 Layout strings

`%layout-change`/`#{window_layout}` grammar (measured example:
`6b8b,100x30,0,0{50x30,0,0,0,49x30,51,0,1}`):

```
layout   := csum ',' node
node     := WxH ',' X ',' Y rest
rest     := ',' paneNum            -- leaf: tmux pane id %<paneNum>
          | '{' node (',' node)* '}'   -- left-right split
          | '[' node (',' node)* ']'   -- top-bottom split
```

`csum` is a 4-hex-digit checksum (ignore on read).  The parser in
`TmuxCC.hs` implements exactly this; `layoutPanes` yields
`(paneId, x, y, w, h)` leaves for DOM layout, and diffing successive
layouts' pane sets gives pane create/close.

### 2.8 Initial state sync (on attach)

1. `list-windows -F '#{window_id}\t#{window_name}\t#{window_active}\t#{window_layout}'`
2. `list-panes -s -F '#{pane_id}\t#{window_id}\t#{pane_active}\t#{pane_title}…'`
3. Per pane, replay scrollback+screen into its xterm:
   `capture-pane -t %P -p -e -J -S -<lines>` (`-e` keeps SGR; body arrives in
   the reply block).  Measured: works, though a full-screen capture includes
   trailing blank lines — trim them.  Cost: a few ms per pane; do visible
   panes first, lazy for the rest.

## 3. leksah integration design

### 3.1 One client per session — `IDE.Web.TmuxCC` (phase 1, done)

A UI-free client modelled on `IDE.Utils.Tool`'s ordered-channel discipline
(single reader thread → one `Chan`; single writer lock; MVar-per-command):

```haskell
startCC  :: [String]           -- socket args, e.g. ["-L","leksah"]
         -> [String]           -- attach args, e.g. ["attach-session","-t","$3"]
         -> IO CC
ccEvents  :: CC -> IO TmuxEvent          -- blocking dequeue (drain thread)
ccCommand :: CC -> Text -> IO (Either Text [Text])  -- correlated reply
ccSendBytes :: CC -> PaneId -> ByteString -> IO ()  -- send-keys -H, chunked
ccResize  :: CC -> Int -> Int -> IO ()   -- refresh-client -C WxH
stopCC    :: CC -> IO ()
```

Design points (all lifted from Tool.hs's lessons):
* one reader thread parses stdout lines into either a pending-reply body or
  a `TmuxEvent`; events go to a `Chan` (ordered, like `RawToolOutput`);
* command submission takes a lock around (enqueue reply-MVar, write line) so
  the FIFO pairing can't interleave;
* the implicit flags=0 block (and any reply with an empty pending queue) is
  discarded;
* EOF fails all pending commands and emits `EvExit`, so nothing deadlocks —
  the stdout/stderr *race* problem Tool.hs solves with sentinel handshakes
  does not exist here because control mode multiplexes everything onto one
  ordered stdout stream (that's the whole point of the protocol).

In leksah, `newIDE`/`Main.hs` holds one `CC` per open terminal session
(keyed by `$<session_id>` like today), or — better, later — one `CC` for the
whole server using `session-changed`-free commands with explicit `-t`
targets (control clients receive `%output` only for panes in their attached
session, so phase 2 keeps client-per-session).

### 3.2 Rendering: one xterm.js per pane (phase 2)

* New `Widget/TerminalCC.hs`: for a session's `Dynamic (Map WindowId
  Layout)`, render the current window as nested flex containers mirroring
  the layout tree (`{}` → `flex-direction:row`, `[]` → `column`, flex-grow ∝
  pane w/h), one `div.terminal` + xterm per pane, keyed by PaneId
  (`listWithKey`, panes persist across layout changes — only the containers
  re-shape).
* `%output` events feed `LeksahTerm.write(paneKey, base64)` exactly like
  today's PTY reader thread (reuse `terminalWriteJs` unchanged; key =
  `"%12"` instead of session id).
* xterm `onData` → `ccSendBytes` for that pane; focus tracking per pane
  replaces tmux's own active-pane concept for input, but still
  `select-pane` on click so tmux agrees about the active pane.
* Resize: a ResizeObserver per pane container is *wrong* under control mode
  (tmux owns pane sizes); instead observe the whole window area, compute
  cols×rows from cell metrics, `ccResize`, and let `%layout-change` drive
  the DOM.  User drags on DOM splitters → `resize-pane -t %P -x N -y M`.
* Scrollback: xterm keeps its own once attached; initial fill via
  `capture-pane` (§2.8).

### 3.3 Feeding the existing machinery

`paneTreeD` (Main.hs) is today refreshed by 2 s polling + pokes.  Under CC:
* `%window-add/-close/-renamed`, `%layout-change`, `%session-*` → poke the
  *existing* `fireTermActivity`-style trigger to re-read `listTerminalTree`
  (cheap migration, keeps one source of truth), or
* (endgame) maintain the `Map Text (Text, [TmuxWindow])` incrementally from
  notifications and drop polling entirely.
Phase 2 does the first; the Terminals tree, window tabs, MRU/flipper,
attention badges (`%output` on a non-focused pane ≙ activity; bell needs
`monitor-bell` state fetch or the existing hooks) all keep working.

### 3.4 Preference + fallback

`Prefs.terminalControlMode :: Bool` (default off), Preferences → Terminal
section, exactly the `terminalFileLinks` pattern (Types.hs `Prefs` +
`PrefsFile`, Preferences.hs default/merge/toPrefsFile, Widget/Preferences.hs
checkbox + `wiredLabels`).  `Main.hs` picks `terminalCCWidget` vs
`terminalWidget` per this pref at tab-build time; the PTY path remains the
fallback (and the only path for `leksah-warp` until CC is proven).

### 3.5 Session save/restore

Unchanged: sessions are still keyed by `$<session_id>`; restore attaches a
CC client instead of a PTY client.  `webSessionVersion` bump not required.

## 4. Phased plan (file-level)

1. **Client (done here)**: `src/IDE/Web/TmuxCC.hs` + standalone protocol
   test `scripts/tmux-cc-test.hs` (run: `scripts/run-tmux-cc-test.sh`).
   No project deps beyond boot libs (process/bytestring/text/containers).
2. **Wiring skeleton (sketched here, unverified)**:
   `src/IDE/Web/Widget/TerminalCC.hs` — session widget rendering per-pane
   xterms from CC events; `leksah.cabal` other-modules additions.
3. Preference plumbing (4 small files, see §3.4) + `Main.hs` switch.
4. Initial-sync polish: capture-pane replay, `window-size manual`, DOM
   splitter drags → `resize-pane`.
5. Replace polling with incremental state; attention badges from
   notifications; retire `termActivityJs`/2 s tick when CC is on.

## 5. What was tested vs not

Tested (scripts/tmux-cc-test.hs, green against tmux 3.6a):
* attach/new-session over pipes, implicit block tolerated;
* correlated replies under 20-way concurrent submission;
* `%error` surfaces as `Left` with body text;
* `send-keys -H` → `%output` observed with correct unescaping (CR/LF/ESC,
  backslash, raw UTF-8);
* split-window → `%layout-change` parsed; pane set diff detects the new
  pane; kill-pane detected purely from layout diff;
* clean `%exit` on kill-server; no wedged threads or zombie processes.

Not verified (needs the full leksah build):
* everything in `Widget/TerminalCC.hs` (reflex/jsaddle code paths);
* interaction with leksah's tmux config (status off, remain-on-exit, hooks)
  under a CC client;
* performance under heavy output (vim scrolling, builds) — see §6.

## 6. Risks / open questions

* **Flow control**: without `pause-after`, a firehose pane (yes/cat) can
  flood the client; tmux buffers per client.  Phase 4 should enable
  `refresh-client -f pause-after=…` + handle `%extended-output`/`%pause`/
  `%continue` (age-based drop + `refresh-client -A %P:continue`).
* **Escaping drift across tmux versions**: older tmux octal-escapes ≥0x7F;
  3.6a passes UTF-8 raw.  The decoder handles both, but tests only cover
  3.6a.
* **Multiple leksah windows / warp remote**: one CC client per session per
  leksah instance is fine (tmux supports many control clients), but two
  instances double `%output` streams — same as today's double attach.
* **In-pane modes**: copy-mode inside a CC-rendered pane has no tmux UI
  (control clients get `%pane-mode-changed` but no drawn overlay); leksah
  should rely on xterm.js scrollback instead and disable the tmux prefix
  bindings that enter modes, or accept degraded behaviour.
* **`window-size`**: with mixed clients (a human attached over PTY *and*
  leksah in CC), sizes fight; `window-size latest` (leksah's current conf
  default is unset → latest) is probably right.
