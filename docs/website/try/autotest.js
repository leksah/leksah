// Scripted smoke test for the demo page — inert unless the page is opened
// with ?autotest=1 (headless-Chrome console-log verification).  Waits for
// the UI to build, checks the Workspace tree picked up the demo project,
// then opens the sample Main.hs through window.leksahOpenFile (the reflex
// bridge the native Open dialogs use — registered by leksah once the
// network is up) and confirms CodeMirror got the file's contents.  Logs
// are prefixed AUTOTEST: so a console grep isolates them.
(function () {
  'use strict';
  if (!/[?&]autotest=1/.test(location.search)) return;
  var log = function (m) { console.log('AUTOTEST: ' + m); };
  var FILE = '/demo/breakout/app/Main.hs';
  var t0 = Date.now();
  var opened = false, treeChecked = false;

  var timer = setInterval(function () {
    if (Date.now() - t0 > 60000) {
      clearInterval(timer);
      log('TIMEOUT — treeChecked=' + treeChecked + ' opened=' + opened);
      return;
    }
    if (!treeChecked) {
      var ws = document.querySelector('.workspace');
      if (!ws) return;
      var txt = ws.textContent;
      log('workspace tree: ' + ws.querySelectorAll('li').length + ' nodes, '
          + (txt.indexOf('breakout') !== -1 ? 'has' : 'MISSING') + ' breakout');
      treeChecked = true;
      return;
    }
    if (!opened) {
      if (typeof window.leksahOpenFile !== 'function') return;
      log('calling leksahOpenFile(' + FILE + ')');
      window.leksahOpenFile(FILE);
      opened = true;
      return;
    }
    var cm = document.querySelector('.cm-editor .cm-content');
    if (cm && cm.textContent.length > 20) {
      clearInterval(timer);
      log('SUCCESS — editor has ' + cm.textContent.length + ' chars; first line: '
          + (cm.querySelector('.cm-line') || cm).textContent.slice(0, 80));
      // ⌘-held shortcut badges: the pref must be published to the page and a
      // Meta keydown must flip the body class badgesJs keys the overlay on.
      log('badges pref published: ' + window.__leksahShortcutBadges);
      window.dispatchEvent(new KeyboardEvent('keydown',
        { key: 'Meta', metaKey: true, bubbles: true }));
      log('badges class on Meta-down: '
          + document.body.classList.contains('leksah-show-badges'));
      window.dispatchEvent(new KeyboardEvent('keyup',
        { key: 'Meta', bubbles: true }));
      // Browser builds also reveal badges on Ctrl (the flip modifier there).
      window.dispatchEvent(new KeyboardEvent('keydown',
        { key: 'Control', ctrlKey: true, bubbles: true }));
      log('badges class on Control-down: '
          + document.body.classList.contains('leksah-show-badges'));
      window.dispatchEvent(new KeyboardEvent('keyup',
        { key: 'Control', bubbles: true }));
      checkTerminals();
    }
  }, 400);

  // --- Demo terminals + LSP hover stand-in ------------------------------
  var FILE_HS = '/demo/breakout/src/IDE/Web/Instance.hs';
  function checkTerminals() {
    log('demo terminals in page: '
        + (window.leksahDemoTerminals || []).length
        + '; hover files in page: '
        + Object.keys(window.leksahDemoHovers || {}).length);
    // The claude terminal tab should exist and have rendered the canned dump.
    var t0 = Date.now(), n = 0;
    var timer2 = setInterval(function () {
      var reg = window.LeksahTerm && window.LeksahTerm.byId;
      var term = reg && reg['$d0'];
      var buf = term && term.buffer && term.buffer.active;
      if (buf) {
        var txt = '';
        for (var i = 0; i < Math.min(buf.length, 200); i++) {
          var l = buf.getLine(i);
          if (l) txt += l.translateToString(true) + '\n';
        }
        if (txt.indexOf('Update(src/IDE/Web/Instance.hs)') !== -1) {
          clearInterval(timer2);
          log('claude terminal rendered with Update(Instance.hs) block');
          checkHover();
          return;
        }
      }
      if (Date.now() - t0 > 30000) {
        clearInterval(timer2);
        log('TERMINAL TIMEOUT — LeksahTerm ids: '
            + (reg ? Object.keys(reg).join(',') : 'none'));
        checkHover();
      }
    }, 500);
  }

  // Genuine round-trip through the Haskell LSP stand-in: call the hover
  // callback the terminal registered (debugHover), intercept the reply that
  // comes back through LeksahTermLinks.resolveHover, and check we got the
  // precomputed HLS text for unsafePerformIO, at line 36 column 18 of the
  // fixture.  The line is 1-BASED, like the source line numbers a diff prints
  // (that is where the real hover path reads it from, and
  // LSP.requestTerminalHover subtracts the 1); the column is 0-based.
  function checkHover() {
    var L = window.LeksahTermLinks;
    if (!L || !L.debugHover) { log('HOVER: no debugHover seam'); flipCheck(); return; }
    var orig = L.resolveHover, got = null;
    L.resolveHover = function (rid, text) {
      if (rid === 424242) { got = text; L.resolveHover = orig; return; }
      return orig(rid, text);
    };
    L.debugHover(FILE_HS, 36, 18, 424242);
    var t0 = Date.now();
    var timer3 = setInterval(function () {
      if (got !== null) {
        clearInterval(timer3);
        log('terminal hover resolved: '
            + (got ? JSON.stringify(got.slice(0, 80)) : 'EMPTY'));
        flipCheck();
      } else if (Date.now() - t0 > 10000) {
        clearInterval(timer3);
        log('HOVER TIMEOUT — no resolveHover callback');
        flipCheck();
      }
    }, 200);
  }

  // Ctrl+` must flip tabs in the browser build (Cmd+` belongs to the OS).
  function flipCheck() {
    // The flipper cycles the CENTRE notebook's tabs; every area has its own
    // .selected tab, so scope to area-wide0 (unscoped, the first match is the
    // side pane's "Workspace", which never changes).
    function selectedTab() {
      var el = document.querySelector('.area-wide0 .tab-wrap.selected button');
      return el ? el.textContent.trim().slice(0, 30) : '(none)';
    }
    var before = selectedTab();
    var down = new KeyboardEvent('keydown',
      { key: '`', code: 'Backquote', ctrlKey: true, bubbles: true });
    Object.defineProperty(down, 'keyCode', { get: function () { return 192; } });
    document.dispatchEvent(down);
    var up = new KeyboardEvent('keyup', { key: 'Control', bubbles: true });
    Object.defineProperty(up, 'keyCode', { get: function () { return 17; } });
    setTimeout(function () {
      document.dispatchEvent(up);
      setTimeout(function () {
        log('Ctrl+` flip: "' + before + '" -> "' + selectedTab() + '"'
            + (selectedTab() === before ? ' (UNCHANGED)' : ''));
        rendererCheck();
        menubarCheck(function () {
          newTerminalCheck(function () {
            showcaseCheck(function () {
              dragCheck(function () {
                breakoutCheck(function () {
                  agentsCheck(function () { log('ALL CHECKS DONE'); });
                });
              });
            });
          });
        });
      }, 600);
    }, 300);
  }

  // A menu-bar dropdown must be OPAQUE.  It is a popup over content, so a
  // dropped background declaration shows as see-through text soup — which is
  // exactly what Clay's legacy `linear-gradient(top, …)` used to cause (modern
  // engines reject that syntax outright).  Assert the painted colour, not the
  // rule: a rejected declaration still reads back as rgba(0, 0, 0, 0) here.
  // The dropdown is built by `dyn`, i.e. one reflex frame after the click, so
  // poll for it rather than reading straight after dispatching.
  function menubarCheck(done) {
    var li = document.querySelector('.menubar ul li');
    if (!li) { log('MENUBAR: no menu bar in this build'); done(); return; }
    li.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    var t0 = Date.now();
    var timer = setInterval(function () {
      var menu = document.querySelector('.menubar .menu');
      if (menu) {
        clearInterval(timer);
        var cs = getComputedStyle(menu);
        var transparent = /rgba\(0, 0, 0, 0\)|transparent/.test(cs.backgroundColor)
                          && cs.backgroundImage === 'none';
        log('menubar dropdown background: ' + cs.backgroundColor
            + ' image=' + cs.backgroundImage.slice(0, 40)
            + (transparent ? ' — TRANSPARENT (bug)' : ' — opaque'));
        li.dispatchEvent(new MouseEvent('click', { bubbles: true }));  // close
        done();
      } else if (Date.now() - t0 > 5000) {
        clearInterval(timer);
        log('MENUBAR: dropdown did not open');
        done();
      }
    }, 250);
  }

  // A terminal the visitor CREATES has no recording behind it (and no shell in
  // the browser), so it must explain the demo rather than sit there black.
  // Click the Terminals tree's "+" and wait for a terminal whose buffer says so.
  function newTerminalCheck(done) {
    var btn = document.querySelector('button[title="New local session"]');
    if (!btn) { log('NEWTERM: no "New local session" button'); done(); return; }
    function ids() { return Object.keys((window.LeksahTerm || {}).byId || {}); }
    var before = ids();
    btn.click();
    var t0 = Date.now(), clickedTab = false;
    var timer = setInterval(function () {
      // wide0 tab bodies mount on first visibility, so if the new tab did not
      // become the selected one, select it — otherwise there is no xterm to read.
      if (!clickedTab && Date.now() - t0 > 3000) {
        clickedTab = true;
        var tab = Array.prototype.slice.call(
              document.querySelectorAll('.area-wide0 .tab-wrap button'))
            .filter(function (b) { return /^leksah-/.test(b.textContent.trim()); })[0];
        log('NEWTERM selecting new tab: ' + (tab ? tab.textContent.trim() : 'NOT FOUND'));
        if (tab) tab.click();
      }
      var reg = (window.LeksahTerm || {}).byId || {};
      var fresh = ids().filter(function (id) { return before.indexOf(id) === -1; });
      for (var i = 0; i < fresh.length; i++) {
        var buf = reg[fresh[i]].buffer && reg[fresh[i]].buffer.active, txt = '';
        for (var j = 0; buf && j < Math.min(buf.length, 60); j++) {
          var l = buf.getLine(j);
          if (l) txt += l.translateToString(true) + '\n';
        }
        if (/just a demo/.test(txt)) {
          clearInterval(timer);
          // Selected, not merely created: "+" must bring its terminal to the
          // front.  (Reaching here without clickedTab already implies it — a
          // hidden tab body never mounts an xterm — but say so out loud.)
          var sel = document.querySelector('.area-wide0 .tab-wrap.selected button');
          log('new terminal ' + fresh[i] + ' explains the demo: '
              + JSON.stringify(txt.replace(/\s+/g, ' ').trim().slice(0, 100)));
          log('new terminal tab selected: ' + (sel ? sel.textContent.trim() : '(none)')
              + (clickedTab ? ' — but only after the test clicked it (BUG)'
                            : ' — by "+" itself'));
          done();
          return;
        }
      }
      if (Date.now() - t0 > 20000) {
        clearInterval(timer);
        log('NEWTERM TIMEOUT — new ids: ' + (fresh.join(',') || 'none')
            + '; all: ' + ids().join(','));
        done();
      }
    }, 500);
  }

  // The terminal must actually PAINT, not just hold a buffer: with xterm.css
  // missing (the head-rebuild bug this guards against) .xterm-screen falls to
  // static flow far below the pane, xterm's IntersectionObserver reports it
  // off-screen and pauses rendering — buffer checks still pass, screen black.
  // So assert: css applied (screen position:relative), render service not
  // paused, screen inside its host, and visible painted output (DOM-renderer
  // row text, or a sized canvas under WebGL).
  // --- The showcase leksah window (multi-pane split) ---------------------
  // The demo boots with a sessionless leksah window "lw-0": the game's editor
  // beside a browser pane running the compiled game, rendered by the shared
  // sessionlessLwWidget.  Assert the split actually materialized (two leaf
  // rects at different x), that its leaves hold the expected views, and that
  // the ⌘-drag registries the gesture depends on are armed.
  function showcaseCheck(done) {
    var t0 = Date.now();
    var timer = setInterval(function () {
      var lw = document.querySelector('.terminal-cc[data-lw="lw-0"]');
      var leaves = lw ? lw.querySelectorAll('.terminal-cc-leaf') : [];
      if (leaves.length >= 2) {
        clearInterval(timer);
        var lefts = Array.prototype.map.call(leaves, function (l) {
          return l.style.left;
        });
        var split = lefts[0] !== lefts[1];
        var hasEditor = !!lw.querySelector('.cm-editor');
        var hasBrowser = !!lw.querySelector('.browser');
        log('showcase window: ' + leaves.length + ' leaves at ' + lefts.join('/')
            + (split ? '' : ' (NOT SPLIT)')
            + ' editor=' + hasEditor + ' browser=' + hasBrowser
            + ((split && hasEditor && hasBrowser) ? ' — OK' : ' — BROKEN'));
        var geom = (window.__leksahLwGeom || {})['lw-0'];
        log('drag registries: lwGeom subs='
            + (geom && geom.subs ? geom.subs.length : 'MISSING')
            + ' leafDragTabs=' + typeof window.__leksahLeafDragTabs);
        done();
      } else if (Date.now() - t0 > 15000) {
        clearInterval(timer);
        log('SHOWCASE TIMEOUT — .terminal-cc[data-lw=lw-0] '
            + (lw ? 'has ' + leaves.length + ' leaves' : 'not in DOM'));
        done();
      }
    }, 400);
  }

  // Best-effort synthetic ⌘/Ctrl-drag of the showcase's first leaf onto the
  // second (the real gesture: modifier+mousedown, 5px threshold, mouseup).
  // Synthetic-event trust varies, so an unengaged drag logs DRAG-SKIPPED
  // rather than failing; a committed one changes the published geometry.
  function dragCheck(done) {
    var lw = document.querySelector('.terminal-cc[data-lw="lw-0"]');
    var leaves = lw ? lw.querySelectorAll('.terminal-cc-leaf') : [];
    if (leaves.length < 2) { log('DRAG-SKIPPED — no showcase leaves'); done(); return; }
    var before = JSON.stringify((window.__leksahLwGeom || {})['lw-0']);
    var r0 = leaves[0].getBoundingClientRect();
    var r1 = leaves[1].getBoundingClientRect();
    function ev(type, x, y) {
      return new MouseEvent(type, { bubbles: true, cancelable: true,
        clientX: x, clientY: y, metaKey: true, ctrlKey: true, button: 0 });
    }
    var sx = r0.left + r0.width / 2, sy = r0.top + r0.height / 2;
    var tx = r1.left + r1.width / 2, ty = r1.top + r1.height * 0.85;
    document.elementFromPoint(sx, sy).dispatchEvent(ev('mousedown', sx, sy));
    var steps = 6, i = 0;
    var mover = setInterval(function () {
      i++;
      var x = sx + (tx - sx) * i / steps, y = sy + (ty - sy) * i / steps;
      document.dispatchEvent(ev('mousemove', x, y));
      if (i >= steps) {
        clearInterval(mover);
        var engaged = document.querySelector('.leksah')
          && document.querySelector('.leksah').classList.contains('leksah-pane-dragging');
        document.dispatchEvent(ev('mouseup', tx, ty));
        if (!engaged) { log('DRAG-SKIPPED — gesture did not engage'); done(); return; }
        var t0 = Date.now();
        var timer = setInterval(function () {
          var after = JSON.stringify((window.__leksahLwGeom || {})['lw-0']);
          if (after !== before) {
            clearInterval(timer);
            log('synthetic drag committed — lw-0 geometry changed');
            done();
          } else if (Date.now() - t0 > 5000) {
            clearInterval(timer);
            log('DRAG engaged but geometry unchanged (drop may have parked)');
            done();
          }
        }, 300);
      }
    }, 60);
  }

  // --- The breakout game in the showcase's browser pane ------------------
  // Same-origin static page (/try/breakout/), so its document is inspectable.
  // SKIP (not fail) when the page 404s: the app side is testable before the
  // compiled game has been staged next to this page.
  function breakoutCheck(done) {
    var t0 = Date.now();
    var timer = setInterval(function () {
      var f = document.querySelector('.browser .browser-frame');
      var src = f && (f.getAttribute('src') || '');
      if (f && /\/try\/breakout/.test(src)) {
        var body = null;
        try { body = f.contentDocument && f.contentDocument.body; } catch (e) {}
        if (body && body.children.length > 0) {
          clearInterval(timer);
          log('breakout iframe loaded: src=' + src + ' body children='
              + body.children.length);
          done();
          return;
        }
      }
      if (Date.now() - t0 > 10000) {
        clearInterval(timer);
        log(f ? ('BREAKOUT SKIP — iframe src=' + (src || '(unset)')
                 + ' (page missing or empty)')
              : 'BREAKOUT SKIP — no .browser-frame in DOM');
        done();
      }
    }, 400);
  }

  // --- The Agents pane (canned demo forest) ------------------------------
  function agentsCheck(done) {
    var tab = Array.prototype.slice.call(
          document.querySelectorAll('.tab-buttons .tab-wrap'))
        .filter(function (w) { return /^Agents/.test(w.textContent.trim()); })[0];
    var btn = tab && tab.querySelector('button');
    if (!btn) { log('AGENTS: no Agents side tab'); done(); return; }
    btn.click();
    var t0 = Date.now();
    var timer = setInterval(function () {
      var rows = document.querySelectorAll('.agents .agents-node');
      if (rows.length > 0) {
        clearInterval(timer);
        var first = rows[0].querySelector('.agents-title');
        var badges = document.querySelectorAll('.agents .agents-badge').length;
        log('agents pane: ' + rows.length + ' rows, ' + badges + ' badges, first="'
            + (first ? first.textContent.trim() : '?') + '"');
        done();
      } else if (Date.now() - t0 > 8000) {
        clearInterval(timer);
        var pane = document.querySelector('.agents');
        log('AGENTS TIMEOUT — pane ' + (pane
            ? 'present, text: ' + pane.textContent.trim().slice(0, 60)
            : 'not in DOM'));
        done();
      }
    }, 400);
  }

  function rendererCheck() {
    Object.keys((window.LeksahTerm || {}).byId || {}).forEach(function (id) {
      try {
        var t = window.LeksahTerm.byId[id];
        var renderer = t._core._renderService._renderer._value;
        var paused = t._core._renderService._isPaused;
        var el = t.element;
        var screen = el.querySelector('.xterm-screen');
        var hostR = el.getBoundingClientRect(), screenR = screen.getBoundingClientRect();
        var inHost = Math.abs(screenR.y - hostR.y) < hostR.height;
        var canvas = el.querySelector('.xterm-screen canvas');
        var rows = el.querySelector('.xterm-rows');
        var painted = canvas ? (canvas.width > 0 && canvas.height > 0)
                             : !!(rows && rows.textContent.trim().length > 0);
        var ok = getComputedStyle(screen).position === 'relative'
                 && !paused && inHost && painted;
        log('renderer ' + id + ': ' + (ok ? 'OK' : 'BROKEN')
            + ' kind=' + (canvas ? 'webgl' : 'dom')
            + ' cssApplied=' + (getComputedStyle(screen).position === 'relative')
            + ' paused=' + paused + ' screenInHost=' + inHost
            + ' painted=' + painted);
      } catch (e) { log('renderer ' + id + ': probe failed ' + e); }
    });
  }
})();
