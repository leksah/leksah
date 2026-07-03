// CodeMirror 6, bundled into a single classic script that exposes a small
// high-level editor API as `window.LeksahCM` (so the jsaddle layer can drive it
// without ES-module loading or hand-wiring CM6 state fields).
// Rebuild with `npm run build` (see README.md).

import { EditorState, Compartment, StateField, StateEffect, RangeSet } from "@codemirror/state"
import { EditorView, keymap, lineNumbers, highlightActiveLineGutter,
         highlightActiveLine, drawSelection, dropCursor,
         Decoration, GutterMarker, gutterLineClass } from "@codemirror/view"
import { defaultKeymap, history, historyKeymap, indentWithTab } from "@codemirror/commands"
import { syntaxHighlighting, defaultHighlightStyle, indentOnInput,
         bracketMatching, foldGutter, foldKeymap, StreamLanguage } from "@codemirror/language"
import { haskell } from "@codemirror/legacy-modes/mode/haskell"
import { oneDark } from "@codemirror/theme-one-dark"
import { searchKeymap, highlightSelectionMatches, search,
         SearchCursor, RegExpCursor } from "@codemirror/search"
import { MergeView, unifiedMergeView } from "@codemirror/merge"

// Per-view state we keep outside CM (original text, active diff view, etc.).
const viewState = new WeakMap()

// ---- error/warning/lint marks (replaces CM5 markText) ----------------------

const setMarksEffect = StateEffect.define()

const marksField = StateField.define({
  create() { return Decoration.none },
  update(deco, tr) {
    deco = deco.map(tr.changes)
    for (const e of tr.effects) if (e.is(setMarksEffect)) deco = e.value
    return deco
  },
  provide: f => EditorView.decorations.from(f),
})

// ---- find highlighting (driven by the find bar, no search panel) -----------

const setFindEffect = StateEffect.define()
const findMatchMark  = Decoration.mark({ class: "cm-leksah-find" })
const findActiveMark = Decoration.mark({ class: "cm-leksah-find-active" })

const findField = StateField.define({
  create() { return Decoration.none },
  update(deco, tr) {
    deco = deco.map(tr.changes)
    for (const e of tr.effects) if (e.is(setFindEffect)) deco = e.value
    return deco
  },
  provide: f => EditorView.decorations.from(f),
})

// ---- modified-code (dirty) highlighting against the git original -----------

const setOriginalEffect = StateEffect.define()

class DirtyGutterMarker extends GutterMarker {}
DirtyGutterMarker.prototype.elementClass = "cm-dirty-gutter"
const dirtyMarker = new DirtyGutterMarker()
const dirtyLineDeco = Decoration.line({ class: "cm-dirty-line" })

// 1-based current-document line numbers that are inserted/modified relative to
// `orig` (a simple line LCS; deletions are not marked).  Guarded against huge
// inputs so we never build a giant DP table.
function changedLineNumbers(orig, cur) {
  const a = orig.split("\n"), b = cur.split("\n")
  const n = a.length, m = b.length
  if (n * m > 4000000) return []  // too big to diff cheaply; skip highlighting
  const dp = []
  for (let i = 0; i <= n; i++) dp.push(new Int32Array(m + 1))
  for (let i = n - 1; i >= 0; i--)
    for (let j = m - 1; j >= 0; j--)
      dp[i][j] = a[i] === b[j] ? dp[i + 1][j + 1] + 1 : Math.max(dp[i + 1][j], dp[i][j + 1])
  const changed = []
  let i = 0, j = 0
  while (j < m) {
    if (i < n && a[i] === b[j]) { i++; j++ }
    else if (i < n && dp[i + 1][j] >= dp[i][j + 1]) { i++ }   // deletion from orig
    else { changed.push(j + 1); j++ }                          // insertion/modification
  }
  return changed
}

function buildDirty(doc, orig) {
  if (orig == null) return { deco: Decoration.none, gutter: RangeSet.empty }
  // A brand-new (untracked) file has an empty original: every line is new.
  // (Splitting "" would give [""], a phantom line that mis-aligns the diff and
  // drops the first/last line.)
  let lines
  if (orig.length === 0) {
    lines = []
    for (let i = 1; i <= doc.lines; i++) lines.push(i)
  } else {
    lines = changedLineNumbers(orig, doc.toString())
  }
  const decoRanges = [], gutterRanges = []
  for (const ln of lines) {
    if (ln >= 1 && ln <= doc.lines) {
      const from = doc.line(ln).from
      decoRanges.push(dirtyLineDeco.range(from))
      gutterRanges.push(dirtyMarker.range(from))
    }
  }
  return { deco: Decoration.set(decoRanges, true), gutter: RangeSet.of(gutterRanges, true) }
}

const dirtyField = StateField.define({
  create() { return { orig: null, deco: Decoration.none, gutter: RangeSet.empty } },
  update(value, tr) {
    let orig = value.orig, recompute = tr.docChanged
    for (const e of tr.effects) if (e.is(setOriginalEffect)) { orig = e.value; recompute = true }
    if (!recompute)
      return { orig, deco: value.deco.map(tr.changes), gutter: value.gutter.map(tr.changes) }
    const b = buildDirty(tr.state.doc, orig)
    return { orig, deco: b.deco, gutter: b.gutter }
  },
  provide: f => [
    EditorView.decorations.from(f, v => v.deco),
    gutterLineClass.from(f, v => v.gutter),
  ],
})

// ---- show original side-by-side / inline -----------------------------------
// (The gutter context menu itself is rendered by Reflex; these are the CM6
//  manipulations it calls.)

function showInline(view) {
  const st = viewState.get(view); if (!st) return
  hideSideBySide(view)
  view.dispatch({ effects: st.inlineComp.reconfigure(unifiedMergeView({ original: st.original ?? "" })) })
  st.inline = true
}
function hideInline(view) {
  const st = viewState.get(view); if (!st || !st.inline) return
  view.dispatch({ effects: st.inlineComp.reconfigure([]) })
  st.inline = false
}
function showSideBySide(view) {
  const st = viewState.get(view); if (!st || st.merge) return
  hideInline(view)
  // The position of the first visible line, captured *before* hiding the editor
  // (posAtCoords needs layout).  Use this rather than the cursor: scrolling
  // without clicking leaves the cursor at the top, so the cursor isn't where
  // the user is actually looking.
  const rect = view.scrollDOM.getBoundingClientRect()
  const topPos = view.posAtCoords({ x: rect.left + 1, y: rect.top + 1 }, false)
              ?? view.state.selection.main.head
  // CM6 core sets `.cm-editor { display: flex !important }`, so a plain inline
  // `display = "none"` is overridden — set it with `important` priority.
  view.dom.style.setProperty("display", "none", "important")
  const ro = [gutterMenuLineNumbers(st.onGutterMenu), baseExtensions(),
              EditorView.editable.of(false), EditorState.readOnly.of(true)]
  st.merge = new MergeView({
    parent: st.parent,
    a: { doc: view.state.doc.toString(), extensions: ro },
    b: { doc: st.original ?? "", extensions: ro },
  })
  // Keep the user's place: open the side-by-side view at the same first line.
  requestAnimationFrame(() => {
    if (st.merge) st.merge.a.dispatch({ effects: EditorView.scrollIntoView(topPos, { y: "start" }) })
  })
}
function hideSideBySide(view) {
  const st = viewState.get(view); if (!st || !st.merge) return
  st.merge.destroy(); st.merge = null
  view.dom.style.removeProperty("display")
}
function hideDiff(view) { hideInline(view); hideSideBySide(view) }

// ---- editor construction ---------------------------------------------------

function offsetOf(doc, line, ch) {
  const l = Math.max(1, Math.min(line, doc.lines))
  const lineObj = doc.line(l)
  return Math.min(lineObj.from + Math.max(0, ch), lineObj.to)
}

function baseExtensions() {
  return [
    highlightActiveLineGutter(),
    foldGutter(),
    history(),
    drawSelection(),
    dropCursor(),
    indentOnInput(),
    syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
    bracketMatching(),
    highlightActiveLine(),
    highlightSelectionMatches(),
    // Search state so the find bar can drive find/replace programmatically
    // (setSearchQuery/findNext/replace…); the panel itself stays hidden — we use
    // our own find bar.
    search(),
    keymap.of([
      indentWithTab,
      ...defaultKeymap,
      ...historyKeymap,
      ...foldKeymap,
      ...searchKeymap,
    ]),
    StreamLanguage.define(haskell),
    oneDark,
    EditorView.theme({
      "&": { height: "100%" },
      ".cm-scroller": { fontFamily: "Hasklig, Menlo, monospace" },
      ".cm-leksah-find": { backgroundColor: "rgba(255,200,0,.35)" },
      ".cm-leksah-find-active": { backgroundColor: "rgba(255,140,0,.6)" },
    }),
  ]
}

// `onChange()` fires when the document changes; `onGutterMenu(x, y)` fires on a
// right-click in the line-number gutter (Reflex renders the menu).
// A line-number gutter whose right-click calls `onGutterMenu(x, y)`.
// (Editor-level DOM handlers don't see gutter events — the gutter isn't in the
// content DOM — so the handler must live on the gutter itself.  Used both for
// the main editor and the side-by-side merge panes.)
function gutterMenuLineNumbers(onGutterMenu) {
  return lineNumbers({
    domEventHandlers: {
      contextmenu(v, line, ev) {
        if (onGutterMenu) { ev.preventDefault(); onGutterMenu(ev.clientX, ev.clientY); return true }
        return false
      },
    },
  })
}

function createEditor(parent, doc, onChange, onGutterMenu) {
  const inlineComp = new Compartment()
  const view = new EditorView({
    doc,
    parent,
    extensions: [
      gutterMenuLineNumbers(onGutterMenu),
      baseExtensions(),
      marksField,
      findField,
      dirtyField,
      inlineComp.of([]),
      EditorView.updateListener.of(u => { if (u.docChanged && onChange) onChange() }),
    ],
  })
  viewState.set(view, { parent, original: null, merge: null, inline: false, inlineComp, onGutterMenu })
  // Remember the most recently focused editor so the (shared) find bar knows
  // which pane to act on.  Set on creation too, since a new editor opens focused.
  view.dom.addEventListener("focusin", () => { window.LeksahCM.activeView = view })
  window.LeksahCM.activeView = view
  return view
}

function getDoc(view) { return view.state.doc.toString() }

function setMarks(view, marks) {
  const doc = view.state.doc
  const ranges = []
  for (const m of marks) {
    const from = offsetOf(doc, m.fromLine, m.fromCh)
    const to = offsetOf(doc, m.toLine, m.toCh)
    if (to > from) ranges.push(Decoration.mark({ class: m.cls }).range(from, to))
  }
  ranges.sort((a, b) => a.from - b.from || a.to - b.to)
  view.dispatch({ effects: setMarksEffect.of(Decoration.set(ranges, true)) })
}

function setOriginal(view, orig) {
  const st = viewState.get(view); if (st) st.original = orig
  view.dispatch({ effects: setOriginalEffect.of(orig) })
}

function gotoPos(view, line, ch) {
  const pos = offsetOf(view.state.doc, line, ch)
  view.dispatch({ selection: { anchor: pos }, effects: EditorView.scrollIntoView(pos, { y: "center" }) })
  view.focus()
}

// ---- find / replace (driven by the Reflex find bar) ------------------------
// `flags` is a bitmask: 1 = case-sensitive, 2 = whole word, 4 = regexp.
//
// Find works on whichever pane is active: a CodeMirror editor uses CM's own
// search (it virtualises its text, so a DOM search would miss off-screen
// matches); any other DOM pane (log, trees, grep, errors, …) uses a generic
// DOM find below.  Replace is CM-only.  Terminals render to a WebGL canvas, so
// they have no DOM text to search and are not covered here.

// Track the active pane (the focused `.tab`) and whether it's a CM editor, so
// the find bar can target it and show Replace only for editors.  Focusing the
// find bar itself (not inside a `.tab`) leaves the active pane unchanged.
let activePaneEl = null
let activeIsCM = false
let activeTermSearch = null   // {term, search} when the active pane is a terminal
function onFocusPane(target) {
  if (!target || !target.closest) return
  const tab = target.closest(".tab")
  if (!tab) return
  activePaneEl = tab
  activeIsCM = !!tab.querySelector(".cm-editor")
  // Prefer the terminal the focus is IN (a control-mode tab has one xterm per
  // tmux pane, each with the search addon on its own root element); fall back
  // to the tab's first .terminal (classic tabs register it there).
  const focusTerm = target.closest(".terminal")
  const termDiv = (focusTerm && focusTerm._leksahTermSearch) ? focusTerm
                                                             : tab.querySelector(".terminal")
  activeTermSearch = (!activeIsCM && termDiv && termDiv._leksahTermSearch) || null
  if (window.LeksahCM && window.LeksahCM.onActivePane) window.LeksahCM.onActivePane(activeIsCM)
}

// Attach xterm's SearchAddon to a terminal so the find bar can search it.
// Guarded: if the addon is missing/incompatible, terminal find is just disabled.
function loadTerminalSearch(term, el) {
  try {
    const SA = window.SearchAddon && window.SearchAddon.SearchAddon
    if (!SA || !term) return
    const search = new SA()
    term.loadAddon(search)
    if (el) el._leksahTermSearch = { term, search }
  } catch (e) { /* terminal find unavailable */ }
}
function termSearchOptions(flags) {
  return {
    caseSensitive: (flags & 1) !== 0,
    wholeWord: (flags & 2) !== 0,
    regex: (flags & 4) !== 0,
    decorations: {
      matchBackground: "#806000", activeMatchBackground: "#c89000",
      matchOverviewRuler: "#FFC800", activeMatchColorOverviewRuler: "#FFC800",
    },
  }
}
document.addEventListener("focusin", e => onFocusPane(e.target), true)
document.addEventListener("mousedown", e => onFocusPane(e.target), true)

function cmActiveView() { return activeIsCM ? (window.LeksahCM.activeView || null) : null }

function escapeRe(s) { return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&") }
function findPattern(searchText, flags) {
  let pat = (flags & 4) ? searchText : escapeRe(searchText)   // 4 = regexp
  if (flags & 2) pat = "\\b" + pat + "\\b"                    // 2 = whole word
  return pat
}

// --- CodeMirror find/replace via search cursors (no search panel, and we never
// move focus, so typing in the find box keeps working) ---
function cmCollect(view, searchText, flags) {
  if (!searchText) return []
  const out = []
  try {
    const cur = new RegExpCursor(view.state.doc, findPattern(searchText, flags),
                                 { ignoreCase: (flags & 1) === 0 })
    cur.next()
    while (!cur.done) { out.push({ from: cur.value.from, to: cur.value.to }); cur.next() }
  } catch (e) { /* invalid regexp */ }
  return out
}
function cmHighlight(view, st) {
  const ranges = []
  st.find.matches.forEach((m, i) => {
    if (m.to > m.from) ranges.push((i === st.find.idx ? findActiveMark : findMatchMark).range(m.from, m.to))
  })
  view.dispatch({ effects: setFindEffect.of(Decoration.set(ranges, true)) })
}
function cmFindSet(view, searchText, replace, flags) {
  const st = viewState.get(view); if (!st) return
  const matches = cmCollect(view, searchText, flags)
  st.find = { searchText, replace, flags, matches, idx: -1 }
  cmHighlight(view, st)
  // Reveal (scroll to) the first match at/after the cursor, but don't move the
  // cursor or steal focus — stepping is left to findNext/findPrev.
  if (matches.length) {
    const head = view.state.selection.main.head
    let i = matches.findIndex(m => m.from >= head); if (i < 0) i = 0
    view.dispatch({ effects: EditorView.scrollIntoView(matches[i].from, { y: "center" }) })
  }
}
function cmStep(view, dir) {
  const st = viewState.get(view); if (!st || !st.find) return
  const ms = st.find.matches, n = ms.length; if (!n) return
  let idx = st.find.idx
  if (idx < 0) {
    const head = view.state.selection.main.head
    if (dir > 0) { idx = ms.findIndex(m => m.from >= head); if (idx < 0) idx = 0 }
    else { idx = 0; for (let i = n - 1; i >= 0; i--) if (ms[i].to <= head) { idx = i; break } }
  } else idx = (idx + dir + n) % n
  st.find.idx = idx
  const m = ms[idx]
  // Move the editor selection/scroll to the match, but do NOT focus the editor:
  // focus stays in the find box so the user can keep typing / hitting Enter.
  view.dispatch({ selection: { anchor: m.from, head: m.to },
                  effects: EditorView.scrollIntoView(m.from, { y: "center" }) })
  cmHighlight(view, st)
}
function cmRecount(view) {
  const st = viewState.get(view); if (!st || !st.find) return
  st.find.matches = cmCollect(view, st.find.searchText, st.find.flags)
  if (st.find.idx >= st.find.matches.length) st.find.idx = -1
  cmHighlight(view, st)
}
function cmReplaceNext(view) {
  const st = viewState.get(view); if (!st || !st.find || !st.find.matches.length) return
  const sel = view.state.selection.main
  let i = st.find.matches.findIndex(m => m.from === sel.from && m.to === sel.to)
  if (i < 0) i = st.find.matches.findIndex(m => m.from >= sel.from)
  if (i < 0) i = 0
  const m = st.find.matches[i]
  view.dispatch({ changes: { from: m.from, to: m.to, insert: st.find.replace || "" } })
  cmRecount(view)
  cmStep(view, 1)
}
function cmReplaceAll(view) {
  const st = viewState.get(view); if (!st || !st.find || !st.find.matches.length) return
  const rep = st.find.replace || ""
  view.dispatch({ changes: st.find.matches.map(m => ({ from: m.from, to: m.to, insert: rep })) })
  cmRecount(view)
}

// --- generic DOM find over the active (non-CM, non-terminal) pane ---
// Matches are drawn as absolutely-positioned overlay rectangles computed from
// the ranges' client rects (the CSS Custom Highlight API isn't rendered here).
// Nothing in the pane's DOM is mutated and the document selection is left
// alone, so the find box keeps focus while you type.  The overlay is fixed to
// the viewport and repositioned on scroll/resize.
let domMatches = [], domIdx = -1, domOverlay = null, domListening = false, domRaf = 0, domMO = null
function domOverlayEl() {
  if (!domOverlay) {
    domOverlay = document.createElement("div")
    domOverlay.style.cssText = "position:fixed;inset:0;pointer-events:none;z-index:60"
    document.body.appendChild(domOverlay)
  }
  return domOverlay
}
function domRender() {
  const ov = domOverlayEl(); ov.textContent = ""
  if (!activePaneEl || !domMatches.length) return
  const pane = activePaneEl.getBoundingClientRect()
  domMatches.forEach((r, i) => {
    for (const rect of r.getClientRects()) {
      // Clip to the pane so matches scrolled out of view don't float over others.
      if (rect.bottom < pane.top || rect.top > pane.bottom ||
          rect.right < pane.left || rect.left > pane.right) continue
      const d = document.createElement("div")
      d.style.cssText = "position:absolute;pointer-events:none;left:" + rect.left + "px;top:"
        + rect.top + "px;width:" + rect.width + "px;height:" + rect.height + "px;background:"
        + (i === domIdx ? "rgba(255,140,0,.6)" : "rgba(255,200,0,.35)")
      ov.appendChild(d)
    }
  })
}
function domReposition() {
  if (domRaf) return
  domRaf = requestAnimationFrame(() => { domRaf = 0; domRender() })
}
function domListen(on) {
  if (on && !domListening) {
    document.addEventListener("scroll", domReposition, true)
    window.addEventListener("resize", domReposition)
    // Re-render when the pane's DOM changes (e.g. collapsing a tree node) so
    // highlights for now-hidden/removed matches disappear.
    if (activePaneEl && window.MutationObserver) {
      domMO = new MutationObserver(domReposition)
      domMO.observe(activePaneEl, { childList: true, subtree: true, attributes: true, characterData: true })
    }
    domListening = true
  } else if (!on && domListening) {
    document.removeEventListener("scroll", domReposition, true)
    window.removeEventListener("resize", domReposition)
    if (domMO) { domMO.disconnect(); domMO = null }
    domListening = false
  }
}
function domClear() {
  domMatches = []; domIdx = -1
  if (domOverlay) domOverlay.textContent = ""
  domListen(false)
}
function domFindSet(searchText, flags) {
  domClear()
  if (!activePaneEl || !searchText) return
  let re
  try { re = new RegExp(findPattern(searchText, flags), (flags & 1) ? "g" : "gi") } catch (e) { return }
  const walker = document.createTreeWalker(activePaneEl, NodeFilter.SHOW_TEXT, null)
  let node
  while ((node = walker.nextNode())) {
    const text = node.nodeValue; re.lastIndex = 0
    let m
    while ((m = re.exec(text))) {
      if (m[0].length === 0) { re.lastIndex++; continue }
      const r = document.createRange()
      r.setStart(node, m.index); r.setEnd(node, m.index + m[0].length)
      domMatches.push(r)
    }
  }
  if (!domMatches.length) return
  domListen(true)
  // Reveal the first match, then draw highlights.
  const el = domMatches[0].startContainer.parentElement
  if (el && el.scrollIntoView) el.scrollIntoView({ block: "nearest", inline: "nearest" })
  domRender()
}
function domStep(dir) {
  const n = domMatches.length; if (!n) return
  domIdx = (domIdx + dir + n) % n
  const el = domMatches[domIdx].startContainer.parentElement
  if (el && el.scrollIntoView) el.scrollIntoView({ block: "nearest", inline: "nearest" })
  domRender()
}

// --- dispatch by active pane: CM editor / terminal / other DOM ---
let lastFind = { searchText: "", flags: 0 }
function findSet(searchText, replace, flags) {
  lastFind = { searchText, flags }
  const v = cmActiveView()
  if (v) cmFindSet(v, searchText, replace, flags)
  else if (activeTermSearch) { /* searched on findNext/findPrev */ }
  else domFindSet(searchText, flags)
}
function findNext() {
  const v = cmActiveView()
  if (v) cmStep(v, 1)
  else if (activeTermSearch) activeTermSearch.search.findNext(lastFind.searchText, termSearchOptions(lastFind.flags))
  else domStep(1)
}
function findPrev() {
  const v = cmActiveView()
  if (v) cmStep(v, -1)
  else if (activeTermSearch) activeTermSearch.search.findPrevious(lastFind.searchText, termSearchOptions(lastFind.flags))
  else domStep(-1)
}
function replaceNext() { const v = cmActiveView(); if (v) cmReplaceNext(v) }
function replaceAll() { const v = cmActiveView(); if (v) cmReplaceAll(v) }

window.LeksahCM = {
  EditorState, EditorView, Compartment, MergeView, unifiedMergeView,
  createEditor, getDoc, setMarks, setOriginal, gotoPos,
  showSideBySide, showInline, hideDiff,
  activeView: null, onActivePane: null,
  findSet, findNext, findPrev, replaceNext, replaceAll,
  loadTerminalSearch,
}
