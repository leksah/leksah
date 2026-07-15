// CodeMirror 6, bundled into a single classic script that exposes a small
// high-level editor API as `window.LeksahCM` (so the jsaddle layer can drive it
// without ES-module loading or hand-wiring CM6 state fields).
// Rebuild with `npm run build` (see README.md).

import { EditorState, Compartment, StateField, StateEffect, RangeSet } from "@codemirror/state"
import { EditorView, keymap, lineNumbers, highlightActiveLineGutter,
         highlightActiveLine, drawSelection, dropCursor,
         Decoration, GutterMarker, gutterLineClass, hoverTooltip } from "@codemirror/view"
import { defaultKeymap, history, historyKeymap, indentWithTab } from "@codemirror/commands"
import { syntaxHighlighting, HighlightStyle, indentOnInput,
         bracketMatching, foldGutter, foldKeymap, StreamLanguage } from "@codemirror/language"
import { tags as t } from "@lezer/highlight"
import { haskell } from "@codemirror/legacy-modes/mode/haskell"
import { javascript, json, typescript } from "@codemirror/legacy-modes/mode/javascript"
import { css, sCSS, less } from "@codemirror/legacy-modes/mode/css"
import { yaml } from "@codemirror/legacy-modes/mode/yaml"
import { shell } from "@codemirror/legacy-modes/mode/shell"
import { toml } from "@codemirror/legacy-modes/mode/toml"
import { xml, html } from "@codemirror/legacy-modes/mode/xml"
import { searchKeymap, highlightSelectionMatches, search,
         SearchCursor, RegExpCursor } from "@codemirror/search"
import { MergeView, unifiedMergeView } from "@codemirror/merge"
import { autocompletion, completionKeymap } from "@codemirror/autocomplete"

// Per-view state we keep outside CM (original text, active diff view, etc.).
const viewState = new WeakMap()

// ---- LSP hover (textDocument/hover) ----------------------------------------
// The hover source asks Haskell (per-view `onHover(id, line, ch)`, set via
// `setHoverHandler`) and awaits a Promise that Haskell resolves through
// `resolveHover(id, text)` once the language server replies.  Positions are
// converted to LSP's 0-based line/character here.
let hoverSeq = 0
const hoverResolvers = new Map()

// Markdown-lite -> HTML for hover blurbs (HLS returns markdown: code fences,
// `inline code`, **bold**, and a '---'/'***' rule between the type signature
// and the docs).  Mirrors fmtTip in IDE.Web.Main's terminalLinksJs so editor
// and terminal hovers render identically.  All text is escaped BEFORE any tag
// is introduced, and the only tags emitted are a fixed, attribute-free set, so
// server-supplied hover text can't inject.
function hoverEsc(s) {
  return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
}
function hoverInlineMd(s) {
  return s.replace(/`([^`]+)`/g, (_, c) => "<code>" + c + "</code>")
          .replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>")
          .replace(/__([^_]+)__/g, "<strong>$1</strong>")
}
function renderHoverMd(text) {
  const lines = String(text).split("\n"), out = []
  let code = [], inCode = false
  for (const ln of lines) {
    if (/^\s*```/.test(ln)) {
      if (inCode) { out.push("<pre>" + hoverEsc(code.join("\n")) + "</pre>"); code = []; inCode = false }
      else inCode = true
      continue
    }
    if (inCode) { code.push(ln); continue }
    if (/^\s*([-*_])(\s*\1){2,}\s*$/.test(ln)) { out.push("<hr>"); continue }
    if (/^\s*$/.test(ln)) { out.push("<br>"); continue }
    out.push(hoverInlineMd(hoverEsc(ln)) + "<br>")
  }
  if (inCode && code.length) out.push("<pre>" + hoverEsc(code.join("\n")) + "</pre>")
  // Drop <br>s butting against a block (<pre>/<hr>) so they don't double the gap
  // their own margins already give, and trim a trailing break.
  return out.join("").replace(/<br>(<(?:pre|hr))/g, "$1").replace(/(<\/pre>|<hr>)<br>/g, "$1").replace(/(<br>)+$/, "")
}

const lspHover = hoverTooltip((view, pos) => {
  const st = viewState.get(view)
  if (!st || !st.onHover) return null
  const line = view.state.doc.lineAt(pos)
  const id = ++hoverSeq
  const promise = new Promise(resolve => {
    hoverResolvers.set(id, resolve)
    // Don't leave a hover pending forever if the server never answers.
    setTimeout(() => { if (hoverResolvers.delete(id)) resolve(null) }, 4000)
  })
  st.onHover(id, line.number - 1, pos - line.from)
  return promise.then(text => {
    if (!text) return null
    return { pos, above: true, create() {
      const dom = document.createElement("div")
      dom.className = "cm-leksah-hover"
      dom.innerHTML = renderHoverMd(text)
      return { dom }
    } }
  })
}, { hoverTime: 300 })

// Called from Haskell once the language server replies (empty text => no tip).
function resolveHover(id, text) {
  const r = hoverResolvers.get(id)
  if (r) { hoverResolvers.delete(id); r(text) }
}

// Register the per-view hover callback (a Haskell `fun`).
function setHoverHandler(view, onHover) {
  const st = viewState.get(view)
  if (st) st.onHover = onHover
}

// ---- LSP completion (textDocument/completion) ------------------------------
// Same round-trip shape as hover: the completion source asks Haskell (per-view
// `onComplete(id, line, ch)`, set via `setCompletionHandler`) and awaits a
// Promise resolved by `resolveComplete(id, itemsJson)` — a JSON array of
// `{label, detail, kind, apply}` objects (kind = LSP CompletionItemKind).
let completeSeq = 0
const completeResolvers = new Map()

// LSP CompletionItemKind (1-based) -> CM6 completion `type` (drives the icon).
function completionType(kind) {
  switch (kind) {
    case 2: case 24: return "method"
    case 3: return "function"
    case 4: case 7: case 22: return "class"
    case 5: case 10: return "property"
    case 6: case 12: return "variable"
    case 8: return "interface"
    case 9: return "namespace"
    case 13: case 20: return "enum"
    case 14: return "keyword"
    case 21: case 11: return "constant"
    case 25: return "type"
    default: return "variable"
  }
}

async function lspCompletionSource(context) {
  const view = context.view
  const st = view && viewState.get(view)
  if (!st || !st.onComplete) return null
  // Haskell identifiers: word chars plus ' and . (qualified names).
  const word = context.matchBefore(/[\w'.]*/)
  if (!context.explicit && (!word || word.from === word.to)) return null
  const line = context.state.doc.lineAt(context.pos)
  const id = ++completeSeq
  const items = await new Promise(resolve => {
    completeResolvers.set(id, resolve)
    // Don't leave completion pending forever if the server never answers.
    setTimeout(() => { if (completeResolvers.delete(id)) resolve([]) }, 4000)
    st.onComplete(id, line.number - 1, context.pos - line.from)
  })
  if (!items || !items.length) return null
  return {
    from: word ? word.from : context.pos,
    options: items.map(it => ({
      label: it.label,
      detail: it.detail || undefined,
      type: completionType(it.kind),
      apply: it.apply || it.label,
    })),
    validFor: /^[\w'.]*$/,
  }
}

// Called from Haskell with the language server's reply (JSON array string).
function resolveComplete(id, itemsJson) {
  const r = completeResolvers.get(id)
  if (!r) return
  completeResolvers.delete(id)
  let items = []
  try { items = JSON.parse(itemsJson) } catch (_e) { items = [] }
  r(items)
}

// Register the per-view completion callback (a Haskell `fun`).
function setCompletionHandler(view, onComplete) {
  const st = viewState.get(view)
  if (st) st.onComplete = onComplete
}

// ---- LSP navigation (go-to-definition / find-references) -------------------
// Fire-and-forget (unlike hover/completion there's no JS Promise to resolve —
// the result drives Haskell-side navigation: definition jumps the editor,
// references populate the Grep pane).  Bound to F12 / Shift-F12; also callable
// directly.  Positions are LSP 0-based line/character.
function navAtPos(view, which, pos) {
  const st = viewState.get(view)
  const cb = st && st[which]
  if (!cb) return false
  const line = view.state.doc.lineAt(pos)
  cb(line.number - 1, pos - line.from)
  return true
}

function navAt(view, which) {
  return navAtPos(view, which, view.state.selection.main.head)
}

// Register the per-view navigation callbacks (Haskell `fun`s).
function setNavHandlers(view, onDefinition, onReferences) {
  const st = viewState.get(view)
  if (st) { st.onDefinition = onDefinition; st.onReferences = onReferences }
}

const lspNavKeymap = [
  { key: "F12",       preventDefault: true, run: v => navAt(v, "onDefinition") },
  { key: "Shift-F12", preventDefault: true, run: v => navAt(v, "onReferences") },
]

// Command-click (⌘ on macOS) a symbol to jump to its definition — the same LSP
// go-to-definition as F12, but at the position under the pointer rather than the
// caret.  preventDefault stops the click from also moving the selection / adding
// a multi-cursor.  Meta only (Ctrl-click is the context menu on macOS).
const cmdClickGoto = EditorView.domEventHandlers({
  mousedown(e, view) {
    if (e.metaKey && e.button === 0) {
      const pos = view.posAtCoords({ x: e.clientX, y: e.clientY })
      if (pos != null && navAtPos(view, "onDefinition", pos)) { e.preventDefault(); return true }
    }
    return false
  },
})

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
  const ro = [gutterMenuLineNumbers(st.onGutterMenu), baseExtensions(st.languageExt),
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

// A standalone, read-only side-by-side diff mounted directly into `parent`
// (not tied to an on-disk editor view) — used by the git log viewer to show a
// commit's change to one file.  Old (parent) content on the left, new (commit)
// on the right, both syntax-highlighted for `filePath` and read-only, so it
// looks like the editor's own showSideBySide.  The MergeView handle is stashed
// on the element so destroyDiff / a re-show can tear it down first.
function showDiff(parent, filePath, oldDoc, newDoc) {
  destroyDiff(parent)
  const ro = [baseExtensions(languageForFile(filePath)),
              EditorView.editable.of(false), EditorState.readOnly.of(true)]
  const mv = new MergeView({
    parent,
    a: { doc: oldDoc, extensions: ro },   // old / parent, left
    b: { doc: newDoc, extensions: ro },   // new / commit, right
  })
  parent.__leksahDiff = mv
  return mv
}
function destroyDiff(parent) {
  if (parent && parent.__leksahDiff) { parent.__leksahDiff.destroy(); parent.__leksahDiff = null }
}

// ---- editor construction ---------------------------------------------------

function offsetOf(doc, line, ch) {
  const l = Math.max(1, Math.min(line, doc.lines))
  const lineObj = doc.line(l)
  return Math.min(lineObj.from + Math.max(0, ch), lineObj.to)
}

// ---- GitHub-dark theme (Primer dark palette) -------------------------------
// A close copy of github.com's dark code view so the editor matches GitHub.
// `gh.*` are the Primer dark syntax/UI tokens; the editor chrome is `githubDark`
// and the token highlighting is `githubDarkHighlightStyle`.
const gh = {
  bg:        "#0d1117",  // canvas
  fg:        "#e6edf3",  // default text
  gutterFg:  "#6e7681",  // line numbers
  gutterActiveFg: "#e6edf3",
  activeLine: "rgba(177,186,196,0.06)",
  selection: "rgba(56,139,253,0.35)",   // accent, translucent so text stays legible
  matchBracket: "rgba(56,139,253,0.30)",
  selectionMatch: "rgba(56,139,253,0.20)",
  // syntax
  gray:   "#8b949e",  // comment / meta
  red:    "#ff7b72",  // keyword / storage
  blue:   "#79c0ff",  // number / constant / builtin
  lightblue: "#a5d6ff", // string
  purple: "#d2a8ff",  // entity: type / class / function / definition
  green:  "#7ee787",  // tag
  orange: "#ffa657",  // variable (params)
  coral:  "#ffa198",  // invalid
}

const githubDark = EditorView.theme({
  "&": { color: gh.fg, backgroundColor: gh.bg },
  ".cm-content": { caretColor: gh.fg },
  ".cm-cursor, .cm-dropCursor": { borderLeftColor: gh.fg },
  "&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection":
    { backgroundColor: gh.selection },
  ".cm-activeLine": { backgroundColor: gh.activeLine },
  ".cm-gutters": { backgroundColor: gh.bg, color: gh.gutterFg, border: "none" },
  ".cm-activeLineGutter": { backgroundColor: "transparent", color: gh.gutterActiveFg },
  ".cm-foldPlaceholder": { backgroundColor: "transparent", border: "none", color: gh.gray },
  ".cm-matchingBracket": { backgroundColor: gh.matchBracket, color: "inherit" },
  ".cm-nonmatchingBracket": { backgroundColor: "rgba(248,81,73,0.25)" },
  ".cm-selectionMatch": { backgroundColor: gh.selectionMatch },
}, { dark: true })

// Token colours.  The Haskell legacy mode (a StreamLanguage) tags with:
// keyword, comment, meta, number/integer, string, type→typeName,
// variable→variableName, builtin→variableName.standard, qualifier→modifier.
const githubDarkHighlightStyle = HighlightStyle.define([
  { tag: [t.comment, t.lineComment, t.blockComment, t.docComment], color: gh.gray },
  { tag: [t.keyword, t.moduleKeyword, t.controlKeyword, t.operatorKeyword,
          t.definitionKeyword, t.modifier, t.self, t.null], color: gh.red },
  { tag: [t.string, t.special(t.string), t.character, t.regexp, t.docString], color: gh.lightblue },
  { tag: [t.number, t.integer, t.float, t.bool, t.atom, t.unit], color: gh.blue },
  { tag: [t.typeName, t.className, t.namespace, t.macroName,
          t.function(t.variableName), t.function(t.propertyName),
          t.definition(t.variableName), t.definition(t.propertyName)], color: gh.purple },
  { tag: [t.standard(t.variableName), t.propertyName, t.attributeName,
          t.labelName, t.constant(t.variableName)], color: gh.blue },
  { tag: [t.tagName, t.angleBracket], color: gh.green },
  { tag: [t.special(t.variableName)], color: gh.orange },
  { tag: [t.meta, t.processingInstruction, t.documentMeta], color: gh.gray },
  { tag: [t.link], color: gh.lightblue, textDecoration: "underline" },
  { tag: [t.heading, t.strong], color: gh.blue, fontWeight: "bold" },
  { tag: [t.emphasis], color: gh.blue, fontStyle: "italic" },
  { tag: [t.strikethrough], textDecoration: "line-through" },
  { tag: [t.invalid], color: gh.coral },
  { tag: [t.deleted], color: "#ffdcd7", backgroundColor: "#67060c" },
  { tag: [t.inserted], color: "#aff5b4", backgroundColor: "#033a16" },
])

// Map a file path to a CM6 language extension (or [] for plain text) by its
// extension.  All modes here come from @codemirror/legacy-modes, already
// bundled — no new deps.  NOTE: .nix and .md have no legacy mode and fall back
// to plain text (add a dedicated package if highlighting them is needed).
const LANG_BY_EXT = {
  hs: haskell, lhs: haskell, hsc: haskell,
  js: javascript, mjs: javascript, cjs: javascript, jsx: javascript,
  ts: typescript, tsx: typescript,
  json: json,
  css: css, scss: sCSS, less: less,
  yaml: yaml, yml: yaml,
  sh: shell, bash: shell, zsh: shell,
  toml: toml,
  xml: xml, html: html, htm: html,
}

function languageForFile(path) {
  if (!path) return []
  const m = /\.([^.\/\\]+)$/.exec(path.toLowerCase())
  const parser = m && LANG_BY_EXT[m[1]]
  return parser ? StreamLanguage.define(parser) : []
}

function baseExtensions(languageExt) {
  return [
    highlightActiveLineGutter(),
    foldGutter(),
    history(),
    drawSelection(),
    dropCursor(),
    indentOnInput(),
    syntaxHighlighting(githubDarkHighlightStyle),
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
      ...completionKeymap,
      ...lspNavKeymap,
    ]),
    languageExt ?? [],
    githubDark,
    EditorView.theme({
      "&": { height: "100%" },
      ".cm-scroller": { fontFamily: "Hasklig, Menlo, monospace" },
      ".cm-leksah-find": { backgroundColor: "rgba(255,200,0,.35)" },
      ".cm-leksah-find-active": { backgroundColor: "rgba(255,140,0,.6)" },
      ".cm-tooltip.cm-tooltip-hover": { border: "1px solid #30363d", backgroundColor: "#161b22" },
      ".cm-leksah-hover": { padding: "4px 8px", maxWidth: "600px", lineHeight: "1.4",
                            color: "#e6edf3",
                            fontFamily: "-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif",
                            fontSize: "12px" },
      ".cm-leksah-hover code": { fontFamily: "Hasklig, Menlo, monospace",
                                 backgroundColor: "rgba(255,255,255,0.09)", borderRadius: "3px",
                                 padding: "0 3px", fontSize: "11.5px" },
      ".cm-leksah-hover pre": { margin: "4px 0", padding: "5px 8px",
                                backgroundColor: "rgba(255,255,255,0.06)",
                                border: "1px solid rgba(255,255,255,0.10)", borderRadius: "4px",
                                fontFamily: "Hasklig, Menlo, monospace", fontSize: "11.5px",
                                lineHeight: "1.3", whiteSpace: "pre", overflowX: "auto" },
      ".cm-leksah-hover hr": { border: "none", borderTop: "1px solid rgba(255,255,255,0.16)",
                               margin: "5px 0" },
      ".cm-leksah-hover strong": { color: "#fff", fontWeight: "600" },
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
  // Pick syntax highlighting from the file's extension (data-file is set on the
  // parent element by IDE.Web.Widget.Editor); reused by the side-by-side view.
  const languageExt = languageForFile(parent && parent.getAttribute("data-file"))
  const view = new EditorView({
    doc,
    parent,
    extensions: [
      gutterMenuLineNumbers(onGutterMenu),
      baseExtensions(languageExt),
      marksField,
      findField,
      dirtyField,
      lspHover,
      cmdClickGoto,
      autocompletion({ override: [lspCompletionSource] }),
      inlineComp.of([]),
      EditorView.updateListener.of(u => { if (u.docChanged && onChange) onChange() }),
    ],
  })
  viewState.set(view, { parent, original: null, merge: null, inline: false, inlineComp, languageExt, onGutterMenu, onHover: null, onComplete: null, onDefinition: null, onReferences: null })
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
  showSideBySide, showInline, hideDiff, showDiff, destroyDiff,
  activeView: null, onActivePane: null,
  findSet, findNext, findPrev, replaceNext, replaceAll,
  loadTerminalSearch,
  setHoverHandler, resolveHover,
  setCompletionHandler, resolveComplete,
  setNavHandlers,
}
