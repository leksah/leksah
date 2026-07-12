// record-demo.mjs — drive leksah-warp (or a bundled stand-in) in headless
// Chromium and capture a WebM video of a short scripted interaction.
//
// This is the *capture* half of the pipeline; record-demo.sh handles tool
// provisioning (node/ffmpeg/gifski via nix) and encodes the WebM into
// web-ready MP4 / VP9-WebM / GIF.  You can also run it directly:
//
//   LEKSAH_URL=http://127.0.0.1:3367/ STORY=tour \
//     node record-demo.mjs /path/to/out/raw.webm
//
// Environment:
//   STORY       selftest | tour | breakout        (default: selftest)
//   LEKSAH_URL  warp URL for tour/breakout         (default 127.0.0.1:3367)
//   W, H        recording size in CSS px           (default 1440x900)
//   OPEN_FILE   file to open for the breakout story (default sandpit/breakout/src/Main.hs)
//   HEADED      set to 1 to watch the browser      (default headless)
//
// The final argument is the output .webm path (default ./raw.webm).
//
// Why a stand-in?  leksah-warp serves an empty bootstrap page and builds its
// whole UI live over a jsaddle WebSocket, so a plain browser must inject
// /jsaddle.js itself (native front ends do this for you).  Automation windows
// are real leksah windows and *accumulate* on a shared instance, contending
// the global reflex lock — so `tour`/`breakout` should target a DEDICATED warp
// instance, launched just for the recording (see record-demo.md).  `selftest`
// needs no leksah at all and proves the record→encode toolchain end to end.

import { chromium } from 'playwright';
import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';

const HERE = dirname(fileURLToPath(import.meta.url));
const STORY = process.env.STORY || 'selftest';
const URL = process.env.LEKSAH_URL || 'http://127.0.0.1:3367/';
const W = +(process.env.W || 1440);
const H = +(process.env.H || 900);
const OUT = resolve(process.argv[2] || 'raw.webm');
const HEADED = process.env.HEADED === '1';
const OPEN_FILE = process.env.OPEN_FILE || 'sandpit/breakout/src/Main.hs';

const log = (...a) => console.error('[record]', ...a);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Hard watchdog: a contended leksah window can wedge a frame indefinitely.
const WATCHDOG_MS = +(process.env.WATCHDOG_MS || 90000);
const watchdog = setTimeout(() => {
  log('WATCHDOG: exceeded', WATCHDOG_MS, 'ms — aborting');
  process.exit(4);
}, WATCHDOG_MS);

// Inject a visible fake cursor so clicks read on the recording (Playwright's
// synthetic mouse is invisible).  Exposes window.__cur(x,y).
async function installCursor(p) {
  await p.evaluate(() => {
    const c = document.createElement('div');
    c.id = '__cursor';
    c.style.cssText =
      'position:fixed;z-index:2147483647;width:20px;height:20px;margin:-10px 0 0 -10px;' +
      'border-radius:50%;background:rgba(74,163,255,.30);border:2px solid #4aa3ff;' +
      'box-shadow:0 0 12px rgba(74,163,255,.6);pointer-events:none;left:0;top:0;' +
      'transition:transform .35s cubic-bezier(.4,.1,.2,1)';
    document.body.appendChild(c);
    window.__cur = (x, y) => (c.style.transform = `translate(${x}px,${y}px)`);
    window.__curPulse = () => {
      c.animate(
        [{ transform: c.style.transform + ' scale(1)' },
         { transform: c.style.transform + ' scale(.6)' },
         { transform: c.style.transform + ' scale(1)' }],
        { duration: 260 }
      );
    };
  });
}

async function glideTo(p, x, y) {
  await p.evaluate(([x, y]) => window.__cur && window.__cur(x, y), [x, y]).catch(() => {});
  await p.mouse.move(x, y, { steps: 12 });
  await sleep(420);
}

// Click a visible element by its exact text; glide the cursor there first.
async function clickText(p, text) {
  try {
    const loc = p.getByText(text, { exact: true }).first();
    if (!(await loc.count())) { log('  no target:', text); return false; }
    const box = await loc.boundingBox();
    if (!box) return false;
    const x = box.x + box.width / 2, y = box.y + box.height / 2;
    await glideTo(p, x, y);
    await p.evaluate(() => window.__curPulse && window.__curPulse()).catch(() => {});
    await loc.click({ timeout: 2500 });
    await sleep(800);
    return true;
  } catch (e) { log('  skip', text, '-', e.message); return false; }
}

// Bring up the live leksah UI: navigate, inject /jsaddle.js, wait for the
// reflex network to actually build the DOM (not just the empty shell).
async function bootLeksah(p) {
  await p.goto(URL, { waitUntil: 'load' });
  await p.addScriptTag({ url: '/jsaddle.js' });
  for (let i = 0; i < 60; i++) {
    const n = await p.evaluate(() => document.body.innerHTML.length).catch(() => 0);
    if (n > 60000) { log('leksah UI built, bodyLen=', n); await sleep(1200); return true; }
    await sleep(400);
  }
  throw new Error('leksah UI never built — is a DEDICATED warp instance running & idle at ' + URL + ' ?');
}

// ---- storylines ---------------------------------------------------------

async function storySelftest(p) {
  await p.goto('file://' + resolve(HERE, 'standin.html'), { waitUntil: 'load' });
  await sleep(500);
  await new Promise(async (done) => {
    await p.exposeFunction('__demoDone', done);
    await p.evaluate(() => window.__demo(() => window.__demoDone()));
  });
  await sleep(400);
}

async function storyTour(p) {
  await bootLeksah(p);
  await installCursor(p);
  await sleep(600);
  for (const t of ['Terminals', 'Workspace']) await clickText(p, t);
  for (const t of ['Log', 'Grep', 'Errors']) await clickText(p, t);
  await sleep(700);
}

async function storyBreakout(p) {
  await bootLeksah(p);
  await installCursor(p);
  await sleep(600);
  // Open the example source, then hit Run (▷).  Selector names are best-effort;
  // adjust to match the sandpit workspace once it lands (see record-demo.md).
  await clickText(p, 'Workspace');
  const base = OPEN_FILE.split('/').pop();
  await clickText(p, base);                       // file in the workspace tree
  await sleep(1200);
  await clickText(p, 'Terminals');                // reveal the terminal pane
  await sleep(4000);                              // let the breakout app play
}

// ---- main ---------------------------------------------------------------

const stories = { selftest: storySelftest, tour: storyTour, breakout: storyBreakout };
const story = stories[STORY];
if (!story) { log('unknown STORY:', STORY); process.exit(2); }

log(`story=${STORY} size=${W}x${H} -> ${OUT}`);
const browser = await chromium.launch({ headless: !HEADED, args: ['--force-color-profile=srgb'] });
const ctx = await browser.newContext({
  viewport: { width: W, height: H },
  recordVideo: { dir: dirname(OUT), size: { width: W, height: H } },
});
const page = await ctx.newPage();
page.setDefaultTimeout(4000);

let code = 0;
try {
  await story(page);
} catch (e) {
  log('ERROR:', e.message);
  code = 1;
} finally {
  const vpath = await page.video().path().catch(() => null);
  await ctx.close();                 // finalizes the WebM
  await browser.close();
  clearTimeout(watchdog);
  if (vpath) {
    const { rename } = await import('fs/promises');
    await rename(vpath, OUT).catch(async () => {
      const { copyFile, unlink } = await import('fs/promises');
      await copyFile(vpath, OUT); await unlink(vpath).catch(() => {});
    });
    log('wrote', OUT);
  } else if (!code) { code = 1; log('no video produced'); }
}
process.exit(code);
