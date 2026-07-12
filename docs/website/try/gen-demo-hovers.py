#!/usr/bin/env python3
"""Generate demo-hovers.js — precomputed LSP hover tooltips for the demo.

This is the demo's stand-in for a live language server, built from REAL
haskell-language-server responses:

  1. Enumerate every hoverable identifier
       * in the static terminal windows (terminals/*.ans): walk each dump
         with the same header/gutter/identifier recognition the runtime
         does in JS (terminalLinksJs, src/IDE/Web/Main.hs — keep the
         regexes in sync!), so every token the demo will underline gets a
         tooltip; and
       * in the packed demo sources (editor hovers): every identifier span
         in each .hs file of demo_manifest.SOURCE_FILES.
  2. Query a real `haskell-language-server --lsp` over the real repo files
     at each position (stdio JSON-RPC; run from the dev shell) and render
     the hover contents the way IDE.LSP.extractHover does.
  3. Emit window.leksahDemoHovers = { demoPath: { line0: [[colStart,
     colEnd, markdown], …] } } — IDE.Web.DemoHovers serves lookups from it.

Failure policy ("find all the tooltips needed"): a terminal-derived
identifier with no HLS hover FAILS the run (listing the misses), except
  * Haskell keywords / pragma words (KEYWORDS below),
  * tokens listed in hover-ignore.txt (curated exceptions),
  * tokens on '+' diff lines whose text doesn't match the real file (the
    fixture marks those as hypothetical edits — the runtime can't hover
    them either, since HLS positions wouldn't line up).
Editor-sweep misses are only counted (a whole file always has spans with
nothing to say).

Responses are cached in .hover-cache.json keyed by file-content hash +
position, so re-runs without source changes skip HLS entirely.
"""
import json
import os
import re
import select
import signal
import sys
import time
import zlib
from pathlib import Path

# NOTE: no `import subprocess` — loading its _posixsubprocess C extension
# (and _hashlib) hangs in the agent environment this script is usually run
# from.  os.posix_spawn + os.pipe cover everything we need.

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[2]
sys.path.insert(0, str(HERE))
from demo_manifest import SOURCE_FILES, HOVER_ROOTS

CACHE_FILE = HERE / '.hover-cache.json'
IGNORE_FILE = HERE / 'hover-ignore.txt'
OUT = HERE / 'demo-hovers.js'

# ---------------------------------------------------------------------------
# Terminal-dump traversal — Python ports of the JS in terminalLinksJs
# (src/IDE/Web/Main.hs).  If those regexes change, change these.
# ---------------------------------------------------------------------------
CSI = re.compile(r'\x1b\[[0-9;:?]*[ -/]*[@-~]')
OSC = re.compile(r'\x1b\][^\x07\x1b]*(\x07|\x1b\\)')
# HDR: headers naming the file a following diff belongs to.
HDR = re.compile(r'(?:Update|Edit|Write|Read)\(([^)]+)\)|^\s*\+\+\+ (?:b/)?(\S+)')
# GUT: indent, right-aligned source line number, 4-column -/+/context field;
# the match length is the 0-based file column of the first code character.
GUT = re.compile(r'^(\s+)(\d+) ([-+ ])  ')
# ID: identifier tokens (optionally module-qualified).
ID = re.compile(r"[A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*")

KEYWORDS = {
    'case', 'class', 'data', 'default', 'deriving', 'do', 'else', 'foreign',
    'if', 'import', 'in', 'infix', 'infixl', 'infixr', 'instance', 'let',
    'module', 'newtype', 'of', 'then', 'type', 'where', 'qualified',
    'hiding', 'as', 'forall', 'mdo', 'rec', 'proc', 'family', 'role',
    'pattern', 'LANGUAGE', 'NOINLINE', 'INLINE', 'INLINABLE', 'UNPACK',
    'e', 'g',  # the "e.g." in comments
}


def visible(s):
    return CSI.sub('', OSC.sub('', s))


def resolve_header(tok):
    """The runtime resolves header paths against the workspace file set;
    here, against the manifest's real paths (same suffix rule)."""
    tok = tok.strip()
    if tok.startswith('./'):
        tok = tok[2:]
    elif tok.startswith(('a/', 'b/')):
        tok = tok[2:]
    for real in SOURCE_FILES:
        if real == tok or real.endswith('/' + tok) or tok.endswith('/' + real):
            return real
    return None


def terminal_targets():
    """(real_file, line0, colStart, colEnd, token, required) per identifier
    on a diff/content line of a terminal dump, with the same coordinates the
    JS sends at runtime.  required=False for hypothetical '+' lines whose
    text doesn't match the real file."""
    targets = []
    for dump in sorted((HERE / 'terminals').glob('[0-9][0-9]-*.ans')):
        lines = [visible(l) for l in dump.read_text().split('\n')]
        governing = None      # real repo path for the current diff block
        real_lines = None
        for t in lines:
            h = HDR.search(t)
            if h:
                tok = (h.group(1) or h.group(2)).strip()
                governing = resolve_header(tok)
                if governing:
                    real_lines = (REPO / governing).read_text().split('\n')
                else:
                    print('  note: %s: header %r not in the demo manifest — '
                          'no hovers for its block (unresolvable at runtime '
                          'too)' % (dump.name, tok))
                continue
            g = GUT.match(t)
            if not (g and governing):
                continue
            line1 = int(g.group(2))
            mark = g.group(3)
            code = t[g.end():]
            in_file = (0 <= line1 - 1 < len(real_lines))
            for m in ID.finditer(code):
                tok = m.group(0)
                if tok in KEYWORDS:
                    continue
                col = m.start()
                matches = (in_file
                           and real_lines[line1 - 1][col:col + len(tok)] == tok)
                if not matches and mark != '+':
                    sys.exit('%s: line %d col %d: %r does not match the real '
                             '%s — stale fixture/capture'
                             % (dump.name, line1, col, tok, governing))
                targets.append((governing, line1 - 1, col, col + len(tok),
                                tok, matches))
    return targets


def editor_targets():
    targets = []
    for real in HOVER_ROOTS:
        text = (REPO / real).read_text()
        for i, line in enumerate(text.split('\n')):
            for m in ID.finditer(line):
                if m.group(0) in KEYWORDS:
                    continue
                targets.append((real, i, m.start(), m.end(), m.group(0), False))
    return targets


# ---------------------------------------------------------------------------
# Minimal LSP client over stdio
# ---------------------------------------------------------------------------
def _which(name):
    for d in os.environ.get('PATH', '').split(os.pathsep):
        p = os.path.join(d, name)
        if os.path.isfile(p) and os.access(p, os.X_OK):
            return p
    sys.exit('%s not on PATH — run from the dev shell' % name)


class _Proc:
    """Popen-alike over os.posix_spawn (see the import note above)."""
    def __init__(self, argv, cwd):
        exe = _which(argv[0])
        in_r, in_w = os.pipe()
        out_r, out_w = os.pipe()
        errlog = os.environ.get('GEN_HOVERS_STDERR', os.devnull)
        err = os.open(errlog, os.O_WRONLY | os.O_CREAT | os.O_APPEND, 0o644)
        cwd0 = os.getcwd()
        os.chdir(cwd)
        try:
            self.pid = os.posix_spawn(exe, argv, os.environ, file_actions=[
                (os.POSIX_SPAWN_DUP2, in_r, 0),
                (os.POSIX_SPAWN_DUP2, out_w, 1),
                (os.POSIX_SPAWN_DUP2, err, 2)])
        finally:
            os.chdir(cwd0)
        for fd in (in_r, out_w, err):
            os.close(fd)
        self.stdin = os.fdopen(in_w, 'wb')
        self.stdout = _Reader(out_r)

    def terminate(self):
        os.kill(self.pid, signal.SIGTERM)


class _Reader:
    """Buffered pipe reader whose reads honour a deadline (a blocked read
    with no deadline could hang the whole run while HLS typechecks
    silently)."""
    def __init__(self, fd):
        self.fd = fd
        self.buf = b''

    def _fill(self, deadline):
        remaining = None if deadline is None else max(0, deadline - time.time())
        r, _, _ = select.select([self.fd], [], [], remaining)
        if not r:
            raise TimeoutError
        chunk = os.read(self.fd, 65536)
        if not chunk:
            raise EOFError
        self.buf += chunk

    def readline(self, deadline=None):
        while b'\n' not in self.buf:
            self._fill(deadline)
        line, self.buf = self.buf.split(b'\n', 1)
        return line + b'\n'

    def read(self, n, deadline=None):
        while len(self.buf) < n:
            self._fill(deadline)
        out, self.buf = self.buf[:n], self.buf[n:]
        return out

    def close(self):
        os.close(self.fd)


class Lsp:
    def __init__(self, root):
        self.root = root
        self.p = _Proc(['haskell-language-server', '--lsp'], cwd=root)
        self.next_id = 0
        self.diagnosed = set()   # uris that have received publishDiagnostics
        uri = 'file://' + str(root)
        r = self.request('initialize', {
            'processId': os.getpid(),
            'rootUri': uri,
            'capabilities': {'window': {'workDoneProgress': True}},
            'workspaceFolders': [{'uri': uri, 'name': Path(root).name}],
        }, timeout=120)
        if r is None:
            sys.exit('HLS at %s: no initialize response' % root)
        self.notify('initialized', {})

    def _send(self, obj):
        body = json.dumps({'jsonrpc': '2.0', **obj}).encode()
        self.p.stdin.write(b'Content-Length: %d\r\n\r\n%s' % (len(body), body))
        self.p.stdin.flush()

    def notify(self, method, params):
        self._send({'method': method, 'params': params})

    def _read(self, deadline):
        headers = {}
        while True:
            line = self.p.stdout.readline(deadline).strip()
            if not line:
                break
            k, _, v = line.partition(b':')
            headers[k.lower()] = v.strip()
        n = int(headers[b'content-length'])
        return json.loads(self.p.stdout.read(n, deadline))

    def _handle(self, msg):
        """Track diagnostics (the readiness signal — ghcide publishes them
        once a file typechecks) and answer server->client requests with the
        boring defaults so HLS doesn't stall."""
        if os.environ.get('GEN_HOVERS_TRACE') and 'method' in msg:
            print('    <- %s' % msg['method'])
        if msg.get('method') == 'textDocument/publishDiagnostics':
            self.diagnosed.add(msg.get('params', {}).get('uri', ''))
        elif 'method' in msg and 'id' in msg:
            if msg['method'] == 'workspace/configuration':
                result = [None] * len(msg['params'].get('items', []))
            elif msg['method'] == 'workspace/applyEdit':
                result = {'applied': False}
            else:
                result = None
            self._send({'id': msg['id'], 'result': result})
        # Other notifications (progress, logs) are dropped.

    def request(self, method, params, timeout=60):
        self.next_id += 1
        rid = self.next_id
        self._send({'id': rid, 'method': method, 'params': params})
        deadline = time.time() + timeout
        self.timed_out = False
        while True:
            try:
                msg = self._read(deadline)
            except TimeoutError:
                self.timed_out = True
                break
            except EOFError:
                sys.exit('HLS at %s died (method %s)' % (self.root, method))
            if msg.get('id') == rid and ('result' in msg or 'error' in msg):
                return msg.get('result')
            self._handle(msg)
        if method == 'initialize':
            sys.exit('HLS at %s: initialize timed out after %ds'
                     % (self.root, timeout))
        print('  warning: HLS at %s: %s timed out after %ds'
              % (self.root, method, timeout))
        return None

    def diagnostics_for(self, path):
        return any(u.endswith(str(path)) for u in self.diagnosed)

    def did_open(self, path):
        self.notify('textDocument/didOpen', {'textDocument': {
            'uri': 'file://' + str(path), 'languageId': 'haskell',
            'version': 1, 'text': Path(path).read_text()}})

    def hover(self, path, line, col, timeout=60):
        return self.request('textDocument/hover', {
            'textDocument': {'uri': 'file://' + str(path)},
            'position': {'line': line, 'character': col}}, timeout)

    def close(self):
        try:
            self.p.stdin.close()
            self.p.terminate()
        except Exception:
            pass


def render_hover(result):
    """IDE.LSP.extractHover's logic: contents may be MarkupContent
    {kind,value}, a MarkedString (string or {language,value}), or a list."""
    if not result:
        return None
    c = result.get('contents')
    if isinstance(c, str):
        text = c
    elif isinstance(c, dict):
        text = c.get('value', '')
    elif isinstance(c, list):
        text = '\n\n'.join(x if isinstance(x, str) else x.get('value', '')
                           for x in c)
    else:
        return None
    text = scrub_paths(text.strip())
    return text or None


def scrub_paths(text):
    """The demo is published: hover markdown must not leak local absolute
    paths ("*Defined at /Users/…*", "file://…" links).  Rewrite paths of
    packed files to their demo names, everything else under the repo to
    /demo, and any remaining home-dir prefix to ~."""
    for real, demo in SOURCE_FILES.items():
        text = text.replace(str(REPO / real), demo)
    text = text.replace(str(REPO), '/demo')
    return text.replace(str(Path.home()), '~')


# ---------------------------------------------------------------------------
def main():
    ignore = set()
    if IGNORE_FILE.exists():
        ignore = {l.strip() for l in IGNORE_FILE.read_text().split('\n')
                  if l.strip() and not l.startswith('#')}

    term = terminal_targets()
    edit = editor_targets()
    # Dedupe by position, letting terminal targets carry their required flag.
    by_pos = {}
    for (f, l, s, e, tok, req) in edit + term:
        key = (f, l, s)
        old = by_pos.get(key)
        by_pos[key] = (f, l, s, e, tok, req or (old[5] if old else False))
    # Required (terminal-derived) positions first, so a stalled cradle for an
    # editor-only file can't block the tooltips the demo terminal needs.
    targets = sorted(by_pos.values(), key=lambda t: (not t[5], t))
    print('%d hover positions (%d from terminal dumps, %d required)'
          % (len(targets), len(term), sum(1 for t in targets if t[5])))

    cache = {}
    if CACHE_FILE.exists():
        # scrub_paths on load too: older caches hold pre-scrub values.
        cache = {k: scrub_paths(v) if isinstance(v, str) else v
                 for k, v in json.loads(CACHE_FILE.read_text()).items()}
    # Cache-buster per file: crc32+length is plenty (not security-relevant,
    # and hashlib's C extension can't load under the agent sandbox).
    hashes = {}
    for real in {t[0] for t in targets}:
        data = (REPO / real).read_bytes()
        hashes[real] = '%08x%x' % (zlib.crc32(data), len(data))

    servers = {}
    opened = set()
    dead = set()    # files whose HLS session never became ready
    # Null hovers are cached too (pragmas, keywords, blank spots — no point
    # re-asking), but ONLY for files that produced at least one real hover
    # this run: a broken session (bad cradle, stalled typecheck) answers null
    # for everything, and caching those would poison every later run.
    ok_files = set()
    pending_nulls = []   # (real, cache-key) held until the file proves alive

    def get_hover(real, line, col):
        if real in dead:
            return None
        ck = '%s:%d:%d' % (hashes[real], line, col)
        if ck in cache:
            return cache[ck]
        root = REPO / HOVER_ROOTS.get(real, '.')
        if root not in servers:
            print('starting haskell-language-server in %s …' % root)
            servers[root] = Lsp(root)
        lsp = servers[root]
        path = REPO / real
        if real not in opened:
            # ghcide computes lazily: nothing typechecks until a request
            # forces it, so the file's FIRST hover doubles as the readiness
            # wait (it blocks until the typecheck finishes) — give it the
            # session-load budget.  Only a TIMEOUT means the session is
            # broken/stalled (skip the file, uncached, so a rerun retries);
            # an ANSWERED null is a legitimate "nothing hoverable here" —
            # a healthy lazy ghcide session publishes no diagnostics either,
            # so absence of diagnostics proves nothing.
            lsp.did_open(path)
            opened.add(real)
            print('first hover of %s (forces its typecheck) …' % real)
            t0 = time.time()
            r = lsp.hover(path, line, col, timeout=900)
            print('  answered in %.0fs' % (time.time() - t0))
            md = render_hover(r)
        else:
            md = render_hover(lsp.hover(path, line, col, timeout=120))
        if md is None and lsp.timed_out:
            print('  warning: hover timed out — skipping the rest of %s'
                  % real)
            dead.add(real)
            return None
        if md is None:
            pending_nulls.append((real, ck))
        else:
            cache[ck] = md
            ok_files.add(real)
        return md

    hovers = {}     # demo path -> {line: [[s, e, md], …]}
    misses = []
    skipped = 0
    try:
        for (real, line, s, e, tok, required) in targets:
            md = get_hover(real, line, s)
            if md is None:
                if required and tok not in ignore:
                    misses.append('%s:%d:%d %s' % (real, line + 1, s, tok))
                else:
                    skipped += 1
                continue
            demo = SOURCE_FILES[real]
            hovers.setdefault(demo, {}).setdefault(str(line), []).append(
                [s, e, md])
    finally:
        for real, ck in pending_nulls:
            if real in ok_files:
                cache[ck] = None
        CACHE_FILE.write_text(json.dumps(cache))
        for lsp in servers.values():
            lsp.close()

    if misses:
        print('\nTOOLTIPS MISSING for terminal identifiers (add to '
              'hover-ignore.txt only if genuinely unhoverable):')
        print('\n'.join('  ' + m for m in misses))
        sys.exit(1)

    n = sum(len(v) for f in hovers.values() for v in f.values())
    OUT.write_text('// Generated by gen-demo-hovers.py — real '
                   'haskell-language-server hover responses.\n'
                   'window.leksahDemoHovers = %s;\n'
                   % json.dumps(hovers, indent=1))
    print('wrote %s (%d spans across %d files; %d spans without hover '
          'skipped)' % (OUT.name, n, len(hovers), skipped))


if __name__ == '__main__':
    main()
