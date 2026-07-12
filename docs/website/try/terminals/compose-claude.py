#!/usr/bin/env python3
"""Compose the demo's Claude Code terminal window (10-claude.ans).

Takes the committed raw-claude-capture.ans — a live `tmux capture-pane -e`
of a real Claude Code session working on this repo, taken at 100 columns —
and splices in the Update(src/IDE/Web/Instance.hs) diff blocks from
docs/development/lsp-hover-terminal-sample.txt (colorized in the genuine
Claude diff style copied from the capture), so the demo terminal carries
hover targets whose gutter line numbers match the real Instance.hs.

The splice point is the live status region (spinner + task list + prompt
box) at the bottom of the capture, so the blocks read as part of the
transcript.  All OSC sequences are stripped: they carry absolute file://
paths and a claude.ai session URL that must not ship in a public demo
page (the visible text is kept — only the invisible link payloads go).

Run from this directory:  python3 compose-claude.py
"""
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
RAW = HERE / 'raw-claude-capture.ans'
FIXTURE = HERE.parents[2] / 'development' / 'lsp-hover-terminal-sample.txt'
OUT = HERE / '10-claude.ans'

CSI = re.compile(r'\x1b\[[0-9;:?]*[ -/]*[@-~]')
OSC = re.compile(r'\x1b\][^\x07\x1b]*(\x07|\x1b\\)')

def visible(s: str) -> str:
    return CSI.sub('', OSC.sub('', s))

raw = RAW.read_text()
lines = OSC.sub('', raw).split('\n')

# The live status region starts at the spinner line ("· Polishing …").
splice_at = next((i for i, l in enumerate(lines)
                  if visible(l).lstrip().startswith('· ')), None)
if splice_at is None:
    sys.exit('compose-claude.py: no spinner line found in raw capture')

# ---------------------------------------------------------------------------
# Colorize the fixture's two Update blocks in the capture's own diff style:
#   header   ⏺ Update(path)            green bullet, bold verb
#   summary  ⎿  Updated …               dim
#   context  <indent>NNN    code        dim number, plain code
#   removal  <indent>NNN -  code        red on dark red, padded to width
#   addition <indent>NNN +  code        green on dark green, padded to width
# Visible columns are preserved exactly — SGR only — so the GUT/ID regexes
# (terminalLinksJs and gen-demo-hovers.py) see the same layout they match
# in a real Claude pane.
# ---------------------------------------------------------------------------
WIDTH = 100
GUT = re.compile(r'^(\s+)(\d+) ([-+ ])  (.*)$')
HDR = re.compile(r'^● Update\((.+)\)$')
SUMMARY = re.compile(r'^  ⎿  (.*)$')

def colorize(line: str) -> str:
    m = HDR.match(line)
    if m:
        return ('\x1b[38;5;114m⏺\x1b[39m \x1b[1mUpdate\x1b[0m(%s)'
                % m.group(1))
    m = SUMMARY.match(line)
    if m:
        return '\x1b[38;5;246m  ⎿  %s\x1b[39m' % m.group(1)
    m = GUT.match(line)
    if m:
        ind, num, mark, code = m.groups()
        if mark == ' ':
            return ('%s\x1b[2m\x1b[38;5;231m%s \x1b[0m\x1b[38;5;231m   %s\x1b[39m'
                    % (ind, num, code))
        fg, bg = ('38;5;77', '48;5;22') if mark == '+' else ('38;5;167', '48;5;52')
        body = '%s %s  %s' % (num, mark, code)
        pad = ' ' * max(0, WIDTH - len(ind) - len(body))
        return ('%s\x1b[%sm\x1b[%sm%s\x1b[38;5;231m%s\x1b[39m\x1b[49m'
                % (ind, fg, bg, body, pad))
    return line

fixture = FIXTURE.read_text().split('\n')
first_hdr = next(i for i, l in enumerate(fixture) if l.startswith('● Update('))
blocks = [colorize(l) for l in fixture[first_hdr:] if l != '']
# Re-insert the blank line between the two blocks.
out_blocks = []
for l in blocks:
    if l.startswith('\x1b[38;5;114m') and out_blocks:
        out_blocks.append('')
    out_blocks.append(l)

lead = ('\x1b[38;5;231m⏺\x1b[39m Reapplying the Instance.hs port-handling '
        'edits:')

composed = (lines[:splice_at]
            + [lead, ''] + out_blocks + ['']
            + lines[splice_at:])
OUT.write_text('\n'.join(composed))
print('wrote %s (%d lines)' % (OUT.name, len(composed)))
