// Native macOS menu bar for leksah-wkwebview.
//
// Haskell (IDE.Web.MacMenu) drives this: it builds the menu from the shared
// menu model (leksah_menu_begin / add_menu / add_item) and installs it
// (leksah_menu_install).  When an item is chosen, the target calls back into
// Haskell via the exported `leksah_menu_action` with the item's tag, which
// runs the corresponding Command in the IDE.
//
// Manual retain/release (no ARC): every alloc'd menu/item is released once its
// parent owns it, and leksah_menu_begin releases the previous tree — in ghci
// mode the whole menu is rebuilt on every :main, so an unbalanced alloc here
// leaks one menu tree per reload.

#import <Cocoa/Cocoa.h>
#import <ApplicationServices/ApplicationServices.h>   // accessibility (AXUIElement) for window snapping
#import <AVFoundation/AVFoundation.h>                 // AVSpeechSynthesizer (terminal-bell announcement)
#import <objc/message.h>
#import <objc/runtime.h>
#include <signal.h>
#include <unistd.h>
#include <pthread.h>

// Haskell callbacks.  Registered at runtime by IDE.Web.MacGlue
// (leksah_set_haskell_callbacks) rather than linked as `extern` foreign
// exports: under leksah.sh --ghci this file is loaded as a dylib (GHCi's RTS
// linker can load ObjC objects but never registers their classes with the
// ObjC runtime, so the ObjC must come in through dyld) — and a dylib cannot
// reference RTS-linker-loaded foreign exports by name.  Function pointers
// made with `foreign import ccall "wrapper"` work from both worlds, and
// identically in the ordinary compiled app.
typedef struct {
    void (*menu_action)(int tag);
    void (*open_file)(const char *path);
    void (*open_project)(const char *path);
    void (*unsnap)(const char *key);       // Tmux ▸ Underlay ▸ Unsnap <window>
    void (*open_settings)(void);           // app menu ▸ Settings…
    // Multi-window: attach a jsaddle context to a just-created webview; track
    // the frontmost window; merge a closing window's tabs into another.
    void (*attach_window)(int wid, void *webview);
    void (*window_activated)(int wid);
    void (*window_closing)(int wid);
    void (*color_picked)(const char *hex); // NSColorPanel change ("#rrggbb")
    // A menu item's toggle state, by tag: -1 = not a toggle (leave unchanged),
    // 0 = off, 1 = on.  Queried in validateMenuItem to show a checkmark.
    int  (*toggle_state)(int tag);
    // A live Claude session chosen from the menu-bar status item's menu: show
    // its terminal (by session id).
    void (*claude_activate)(const char *session_id);
} leksah_haskell_callbacks;

static leksah_haskell_callbacks gHs;   // zero-initialised

// The FunPtrs the PREVIOUS registration installed, kept so the Haskell side can
// free them (see MacGlue.setMacCallbacks): each one pins its closure, which
// captures that instance's IDERef — i.e. a whole IDE, reflex network included —
// so a ghci session that never freed them grew by an entire instance per
// reload.  Statics in this dylib, not a Haskell CAF: a :reload resets
// leksah-mac-glue's CAFs (it is a home-package object module) but never reloads
// this dylib, the same reason leksah_take_first_launch lives here.
#define LEKSAH_N_CALLBACKS 11
static void *gPrevCbs[LEKSAH_N_CALLBACKS];
static int   gPrevCbCount = 0;

// Snapshot whatever is currently installed, before it is overwritten.
static void leksah_remember_previous_callbacks(void)
{
    int n = 0;
    void *cur[LEKSAH_N_CALLBACKS] = {
        (void *)gHs.menu_action,      (void *)gHs.open_file,
        (void *)gHs.open_project,     (void *)gHs.unsnap,
        (void *)gHs.open_settings,    (void *)gHs.attach_window,
        (void *)gHs.window_activated, (void *)gHs.window_closing,
        (void *)gHs.color_picked,     (void *)gHs.toggle_state,
        (void *)gHs.claude_activate };
    for (int i = 0; i < LEKSAH_N_CALLBACKS; i++)
        if (cur[i]) gPrevCbs[n++] = cur[i];
    gPrevCbCount = n;
}

// Hand the snapshot to Haskell (which owns freeing it — hs_free_fun_ptr is an
// RTS entry point this dylib cannot link against under ghci) and forget it, so
// each set is freed at most once.  Returns how many were written.
int leksah_take_previous_callbacks(void **out, int max)
{
    int n = gPrevCbCount < max ? gPrevCbCount : max;
    for (int i = 0; i < n; i++) out[i] = gPrevCbs[i];
    gPrevCbCount = 0;
    return n;
}

void leksah_set_haskell_callbacks(
    void (*menu_action)(int),
    void (*open_file)(const char *),
    void (*open_project)(const char *),
    void (*unsnap)(const char *),
    void (*open_settings)(void),
    void (*attach_window)(int, void *),
    void (*window_activated)(int),
    void (*window_closing)(int),
    void (*color_picked)(const char *))
{
    leksah_remember_previous_callbacks();
    gHs.menu_action      = menu_action;
    gHs.open_file        = open_file;
    gHs.open_project     = open_project;
    gHs.unsnap           = unsnap;
    gHs.open_settings    = open_settings;
    gHs.attach_window    = attach_window;
    gHs.window_activated = window_activated;
    gHs.window_closing   = window_closing;
    gHs.color_picked     = color_picked;
}

// Registered separately (additive — keeps leksah_set_haskell_callbacks's ABI
// stable): the callback that reports a menu item's live toggle state.
void leksah_set_toggle_state_callback(int (*toggle_state)(int))
{
    gHs.toggle_state = toggle_state;
}

// Likewise additive: shows the Claude session chosen from the menu-bar status
// item.  Registered AFTER leksah_set_haskell_callbacks, whose
// leksah_remember_previous_callbacks call snapshots the previous set of all
// three registrations for Haskell to free.
void leksah_set_claude_activate_callback(void (*claude_activate)(const char *))
{
    gHs.claude_activate = claude_activate;
}

// The title bar is transparent and the WKWebView fills the whole window, so the
// web toolbar sits in the title-bar strip.  The WKWebView swallows mouse events,
// so the usual "drag the window background" doesn't work and WKWebView has no
// -webkit-app-region support.  We instead watch left-mouse-downs and, when one
// lands in the title-bar strip but not on a traffic-light or a toolbar button,
// start a window drag ourselves.  The toolbar buttons' x-range is measured from
// the DOM (so clicks on them still work).
static const CGFloat kLeksahTitlebarHeight = 28.0;
static double  gToolbarMinX = 1.0e9;   // left edge of the first toolbar button
static double  gToolbarMaxX = -1.0e9;  // right edge of the last toolbar button
static NSWindow *gLeksahWindow = nil;
// Every leksah OS window, keyed by its WindowId (@(wid)) → NSWindow.  wid 0 is
// the first window (created by jsaddle-wkwebview); further windows come from
// leksah_new_window.  Used to raise/address a specific window by id.
static NSMutableDictionary *gWindows = nil;

@interface LeksahMenuTarget : NSObject
- (void)leksahAction:(id)sender;
- (void)leksahRemeasure:(NSTimer *)timer;
- (void)leksahOpenSettings:(id)sender;
@end

static void leksah_measure_toolbar(void);
static void leksah_read_holes(void);

// Whether a terminal tab is on screen (set from Haskell via
// leksah_set_terminal_active).  Items marked terminal-only (the Terminal
// menu's real key equivalents) are disabled otherwise — and a DISABLED item's
// key equivalent is not consumed, so ⌘D / ⌘[ / ⌘⌥arrows still reach the
// editor through the responder chain when no terminal is showing.
static volatile int gTerminalActive = 0;

void leksah_set_terminal_active(int on) {
    gTerminalActive = on;
}

// Whether the active tab, though not a terminal, can CONVERT to a tmux pane
// (an editor / git-log tab with a backing pane — see paneOverlays).  The two
// Split items are marked "splittable": enabled when a terminal is active OR
// a convertible tab is — so ⌘D on an editor tab converts it and splits.
static volatile int gSplitActive = 0;

void leksah_set_split_active(int on) {
    gSplitActive = on;
}

// Set while leksah_close_all_windows runs (ghci-mode teardown): the
// WillClose observers skip leksah_window_closing so the Haskell side's
// merge/quit logic doesn't fire for windows it is closing itself.
static volatile int gTeardownInProgress = 0;

@implementation LeksahMenuTarget
- (void)leksahAction:(id)sender {
    if (gHs.menu_action) gHs.menu_action((int)[(NSMenuItem *)sender tag]);
}
- (BOOL)validateMenuItem:(NSMenuItem *)item {
    if ([@"terminal" isEqual:[item representedObject]])
        return gTerminalActive != 0;
    if ([@"splittable" isEqual:[item representedObject]])
        return gTerminalActive != 0 || gSplitActive != 0;
    // Reflect a toggle command's live state as a checkmark (e.g. Terminal ▸
    // Intercept Ctrl+B, Build ▸ Native/JavaScript/Debug).  -1 = not a toggle.
    if (gHs.toggle_state) {
        int st = gHs.toggle_state((int)[item tag]);
        if (st >= 0)
            [item setState:(st ? NSControlStateValueOn : NSControlStateValueOff)];
    }
    return YES;
}
- (void)leksahRemeasure:(NSTimer *)timer {
    (void)timer;
    leksah_measure_toolbar();
    leksah_read_holes();
}
- (void)leksahOpenSettings:(id)sender {
    (void)sender;
    if (gHs.open_settings) gHs.open_settings();
}
@end

static LeksahMenuTarget *gTarget = nil;
static NSMenu *gMainMenu = nil;
// The Underlay ▸ Unsnap submenu (cached during menu build, populated dynamically
// from the snapped windows).  Declared here so leksah_menu_push_submenu can set
// and seed it; defined/used by the snap code further down.
static NSMenu *gUnsnapMenu = nil;
static void leksah_rebuild_unsnap_menu(void);
// A stack of open menus: gMenuStack[gMenuDepth-1] is the menu items are added to.
// add_menu resets it to a single top-level menu; push/pop_submenu nest within it.
#define LEKSAH_MENU_MAX_DEPTH 16
static NSMenu *gMenuStack[LEKSAH_MENU_MAX_DEPTH];
static int gMenuDepth = 0;

// File ▸ Open Recent: a dynamically-populated submenu of recently opened files.
@interface LeksahRecentTarget : NSObject
- (void)openRecent:(id)sender;
@end
@implementation LeksahRecentTarget
- (void)openRecent:(id)sender {
    NSString *path = [(NSMenuItem *)sender representedObject];
    if (path != nil && gHs.open_file) gHs.open_file([path UTF8String]);
}
@end

static LeksahRecentTarget *gRecentTarget = nil;
static NSMenu *gRecentMenu = nil;

void leksah_menu_begin(void) {
    if (gTarget == nil) gTarget = [[LeksahMenuTarget alloc] init];
    // Rebuild (ghci :main): drop our ref to the old tree (NSApp keeps it alive
    // until leksah_menu_install swaps it) and forget the cached submenus that
    // point into it — they are recreated during this rebuild.
    [gRecentMenu release]; gRecentMenu = nil;
    gUnsnapMenu = nil;
    [gMainMenu release];
    gMainMenu = [[NSMenu alloc] init];
    gMenuDepth = 0;

    // The application (apple-name) menu, so Quit etc. exist.
    NSMenuItem *appItem = [[NSMenuItem alloc] init];
    [gMainMenu addItem:appItem];
    [appItem release];
    NSMenu *appMenu = [[NSMenu alloc] init];
    // Name the About/Quit items from the bundle's CFBundleName (the single
    // source of truth — see Leksah.app in leksah-nix.sh); fall back to the
    // process name for an unbundled run.
    NSString *appName = [[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleName"];
    if (appName.length == 0) appName = [[NSProcessInfo processInfo] processName];
    [appMenu addItemWithTitle:[@"About " stringByAppendingString:appName]
                       action:@selector(orderFrontStandardAboutPanel:)
                keyEquivalent:@""];
    [appMenu addItem:[NSMenuItem separatorItem]];
    // Settings… (⌘,) in its conventional macOS place — the app menu.  It opens
    // the Preferences pane via the reflex bridge (leksah_open_settings).
    if (gTarget == nil) gTarget = [[LeksahMenuTarget alloc] init];
    NSMenuItem *settings = [appMenu addItemWithTitle:@"Settings…"
                       action:@selector(leksahOpenSettings:)
                keyEquivalent:@","];
    [settings setTarget:gTarget];
    [appMenu addItem:[NSMenuItem separatorItem]];
    [appMenu addItemWithTitle:[@"Quit " stringByAppendingString:appName]
                       action:@selector(terminate:)
                keyEquivalent:@"q"];
    [appItem setSubmenu:appMenu];
    [appMenu release];
}

void leksah_menu_add_menu(const char *title) {
    if (gMainMenu == nil) leksah_menu_begin();
    NSString *t = [NSString stringWithUTF8String:title];
    NSMenuItem *item = [[NSMenuItem alloc] init];
    [gMainMenu addItem:item];
    [item release];
    NSMenu *sub = [[NSMenu alloc] initWithTitle:t];
    [item setSubmenu:sub];
    [sub release];
    // Start a fresh top-level menu as the (only) open menu.
    gMenuStack[0] = sub;
    gMenuDepth = 1;
    // The Edit menu gets the standard AppKit editing items.  Their actions
    // (cut:/copy:/paste:/selectAll:) have no target, so the key equivalents
    // (⌘X/⌘C/⌘V/⌘A) travel the responder chain to the focused WKWebView — which
    // is what lets ⌘V paste into the xterm.js terminals (and text fields).
    // Without an item bound to paste:, AppKit never delivers ⌘V at all.  These
    // use system selectors rather than leksah commands, so they don't consume a
    // menu tag; the model's own Edit items (Find) are appended after.
    if ([t isEqualToString:@"Edit"]) {
        [sub addItemWithTitle:@"Undo" action:@selector(undo:) keyEquivalent:@"z"];
        NSMenuItem *redo = [sub addItemWithTitle:@"Redo" action:@selector(redo:)
                                   keyEquivalent:@"z"];
        [redo setKeyEquivalentModifierMask:NSEventModifierFlagCommand | NSEventModifierFlagShift];
        [sub addItem:[NSMenuItem separatorItem]];
        [sub addItemWithTitle:@"Cut"        action:@selector(cut:)       keyEquivalent:@"x"];
        [sub addItemWithTitle:@"Copy"       action:@selector(copy:)      keyEquivalent:@"c"];
        [sub addItemWithTitle:@"Paste"      action:@selector(paste:)     keyEquivalent:@"v"];
        [sub addItemWithTitle:@"Select All" action:@selector(selectAll:) keyEquivalent:@"a"];
        [sub addItem:[NSMenuItem separatorItem]];
    }
}

void leksah_menu_add_item(const char *title, int tag) {
    if (gMenuDepth <= 0) return;
    NSString *t = [NSString stringWithUTF8String:title];
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:t
                                                  action:@selector(leksahAction:)
                                           keyEquivalent:@""];
    [item setTarget:gTarget];
    [item setTag:tag];
    [gMenuStack[gMenuDepth - 1] addItem:item];
    [item release];
}

// Like add_item, but renders `shortcut` in the native key-equivalent column
// (right-aligned, greyed).  These tmux chords (C-b X) aren't single-chord macOS
// key equivalents, so we can't use a real keyEquivalent; instead we set an
// attributed title "desc\tshortcut" with a right tab stop and a grey run.
void leksah_menu_add_item_kv(const char *desc, const char *shortcut, int tag) {
    if (gMenuDepth <= 0) return;
    NSString *d = [NSString stringWithUTF8String:desc];
    NSString *s = [NSString stringWithUTF8String:shortcut];
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:d
                                                  action:@selector(leksahAction:)
                                           keyEquivalent:@""];
    [item setTarget:gTarget];
    [item setTag:tag];
    if ([s length] > 0) {
        NSString *full = [NSString stringWithFormat:@"%@\t%@", d, s];
        NSMutableParagraphStyle *ps = [[NSMutableParagraphStyle alloc] init];
        NSTextTab *tab = [[NSTextTab alloc] initWithType:NSRightTabStopType location:220.0];
        [ps setTabStops:@[tab]];
        NSMutableAttributedString *at =
            [[NSMutableAttributedString alloc] initWithString:full];
        NSRange whole = NSMakeRange(0, [full length]);
        [at addAttribute:NSFontAttributeName value:[NSFont menuFontOfSize:0] range:whole];
        [at addAttribute:NSParagraphStyleAttributeName value:ps range:whole];
        [at addAttribute:NSForegroundColorAttributeName value:[NSColor grayColor]
                    range:NSMakeRange([d length] + 1, [s length])];
        [item setAttributedTitle:at];
        [at release];
        [tab release];
        [ps release];
    }
    [gMenuStack[gMenuDepth - 1] addItem:item];
    [item release];
}

// Like add_item, but with a REAL key equivalent parsed from a spec like
// "cmd+shift+d", "cmd+alt+Up", "cmd+ctrl+=" or "cmd+shift+Enter" (modifiers:
// cmd/super, shift, alt/opt, ctrl; key: a single character or Up/Down/Left/
// Right/Enter).  The item is marked terminal-only: it is enabled (and its key
// equivalent consumed) only while a terminal tab is on screen — see
// validateMenuItem above.
static void leksah_menu_add_item_key_repr(const char *title, const char *spec,
                                          int tag, NSString *repr) {
    if (gMenuDepth <= 0) return;
    NSString *t = [NSString stringWithUTF8String:title];
    NSString *s = [NSString stringWithUTF8String:spec];
    NSUInteger mask = 0;
    NSString *keyPart = @"";
    for (NSString *tok in [s componentsSeparatedByString:@"+"]) {
        if ([tok isEqualToString:@"cmd"] || [tok isEqualToString:@"super"])
            mask |= NSEventModifierFlagCommand;
        else if ([tok isEqualToString:@"shift"])
            mask |= NSEventModifierFlagShift;
        else if ([tok isEqualToString:@"alt"] || [tok isEqualToString:@"opt"])
            mask |= NSEventModifierFlagOption;
        else if ([tok isEqualToString:@"ctrl"])
            mask |= NSEventModifierFlagControl;
        else if ([tok length] > 0)
            keyPart = tok;
        // an empty token ("cmd++") would mean a literal '+': not used today
    }
    unichar kc = 0;
    if ([keyPart isEqualToString:@"Up"])         kc = NSUpArrowFunctionKey;
    else if ([keyPart isEqualToString:@"Down"])  kc = NSDownArrowFunctionKey;
    else if ([keyPart isEqualToString:@"Left"])  kc = NSLeftArrowFunctionKey;
    else if ([keyPart isEqualToString:@"Right"]) kc = NSRightArrowFunctionKey;
    else if ([keyPart isEqualToString:@"Enter"]) kc = '\r';
    else if ([keyPart length] >= 1)              kc = [keyPart characterAtIndex:0];
    NSString *ke = (kc != 0) ? [NSString stringWithCharacters:&kc length:1] : @"";
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:t
                                                  action:@selector(leksahAction:)
                                           keyEquivalent:ke];
    [item setKeyEquivalentModifierMask:mask];
    [item setTarget:gTarget];
    [item setTag:tag];
    [item setRepresentedObject:repr];
    [gMenuStack[gMenuDepth - 1] addItem:item];
    [item release];
}

void leksah_menu_add_item_key(const char *title, const char *spec, int tag) {
    // gate on a terminal being active
    leksah_menu_add_item_key_repr(title, spec, tag, @"terminal");
}

// A Split item: enabled while a terminal OR a convertible (editor/git-log)
// tab is active — see validateMenuItem.
void leksah_menu_add_item_key_splittable(const char *title, const char *spec, int tag) {
    leksah_menu_add_item_key_repr(title, spec, tag, @"splittable");
}

// Like leksah_menu_add_item_key but WITHOUT the terminal gate — an always-enabled
// real key equivalent (the AI menu's Grab Region).  Parses the same spec grammar.
void leksah_menu_add_item_key_global(const char *title, const char *spec, int tag) {
    if (gMenuDepth <= 0) return;
    NSString *t = [NSString stringWithUTF8String:title];
    NSString *s = [NSString stringWithUTF8String:spec];
    NSUInteger mask = 0;
    NSString *keyPart = @"";
    for (NSString *tok in [s componentsSeparatedByString:@"+"]) {
        if ([tok isEqualToString:@"cmd"] || [tok isEqualToString:@"super"])
            mask |= NSEventModifierFlagCommand;
        else if ([tok isEqualToString:@"shift"])
            mask |= NSEventModifierFlagShift;
        else if ([tok isEqualToString:@"alt"] || [tok isEqualToString:@"opt"])
            mask |= NSEventModifierFlagOption;
        else if ([tok isEqualToString:@"ctrl"])
            mask |= NSEventModifierFlagControl;
        else if ([tok length] > 0)
            keyPart = tok;
    }
    unichar kc = 0;
    if ([keyPart isEqualToString:@"Up"])         kc = NSUpArrowFunctionKey;
    else if ([keyPart isEqualToString:@"Down"])  kc = NSDownArrowFunctionKey;
    else if ([keyPart isEqualToString:@"Left"])  kc = NSLeftArrowFunctionKey;
    else if ([keyPart isEqualToString:@"Right"]) kc = NSRightArrowFunctionKey;
    else if ([keyPart isEqualToString:@"Enter"]) kc = '\r';
    else if ([keyPart length] >= 1)              kc = [keyPart characterAtIndex:0];
    NSString *ke = (kc != 0) ? [NSString stringWithCharacters:&kc length:1] : @"";
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:t
                                                  action:@selector(leksahAction:)
                                           keyEquivalent:ke];
    [item setKeyEquivalentModifierMask:mask];
    [item setTarget:gTarget];
    [item setTag:tag];
    [gMenuStack[gMenuDepth - 1] addItem:item];
    [item release];
}

// A separator line in the current menu.
void leksah_menu_add_separator(void) {
    if (gMenuDepth <= 0) return;
    [gMenuStack[gMenuDepth - 1] addItem:[NSMenuItem separatorItem]];
}

// Add a submenu item to the current menu and descend into it, so subsequent
// add_item / push_submenu calls populate the submenu.  Pair with pop_submenu.
void leksah_menu_push_submenu(const char *title) {
    if (gMenuDepth <= 0 || gMenuDepth >= LEKSAH_MENU_MAX_DEPTH) return;
    NSString *t = [NSString stringWithUTF8String:title];
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:t action:NULL keyEquivalent:@""];
    NSMenu *sub = [[NSMenu alloc] initWithTitle:t];
    [item setSubmenu:sub];
    [gMenuStack[gMenuDepth - 1] addItem:item];
    [item release];
    [sub release];
    gMenuStack[gMenuDepth++] = sub;
    // The Underlay ▸ Unsnap submenu is populated dynamically from the snapped
    // windows; cache it and seed it with the current (empty) list.
    if ([t isEqualToString:@"Unsnap"]) { gUnsnapMenu = sub; leksah_rebuild_unsnap_menu(); }
}

// Finish the current submenu and return to its parent.
void leksah_menu_pop_submenu(void) {
    if (gMenuDepth > 1) gMenuDepth--;
}

void leksah_menu_install(void) {
    NSMenu *menu = gMainMenu;
    // The menu bar must be set on the main thread; schedule it so this works
    // whether or not the app's run loop has started yet.
    dispatch_async(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication];
        // The bold application-menu title comes from the bundle's CFBundleName
        // ("Leksah"); nothing to set here (we run from Leksah.app — see
        // leksah-nix.sh).
        [NSApp setMainMenu:menu];
    });
}

// ---------------------------------------------------------------------------
// Title bar: let the web toolbar sit in the window's title bar.
//
// We make the title bar transparent and let the WKWebView content fill the
// whole window (full-size content view), so the web toolbar -- rendered at the
// top of the page with left padding to clear the traffic-light buttons --
// occupies the title-bar strip.  The window is created by jsaddle-wkwebview
// after launch, so retry on the main queue until it exists.
// ---------------------------------------------------------------------------

// SIGUSR1 = "relaunch the dev build": exit with code 2 so leksah-nix.sh's loop
// rebuilds and relaunches (see dev-relaunch.sh).  Done in C because the main
// thread is parked forever inside Cocoa's `[application run]`, so a Haskell
// (RTS) handler can be unreliable here; a C handler runs regardless.  `_exit`
// is async-signal-safe.
static void leksah_on_sigusr1(int sig) {
    (void)sig;
    _exit(2);
}

// Install the handler on the main thread, after the RTS has set up its own, so
// ours wins; unblock SIGUSR1 here so the signal can be delivered to this thread.
static void leksah_install_relaunch_signal(void) {
    signal(SIGUSR1, leksah_on_sigusr1);
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGUSR1);
    pthread_sigmask(SIG_UNBLOCK, &set, NULL);
}

// Find the WKWebView somewhere in a view tree (without importing WebKit).
static id leksah_find_webview(NSView *v) {
    Class wk = NSClassFromString(@"WKWebView");
    if (wk != nil && [v isKindOfClass:wk]) return v;
    for (NSView *sub in [v subviews]) {
        id r = leksah_find_webview(sub);
        if (r != nil) return r;
    }
    return nil;
}

// Snapshot the WKWebView content (or, when useRect, just the given rect in the
// view's coordinate system — CSS px) to a PNG at cpath.  Called off the main
// thread: the snapshot API is async and main-thread-only, so dispatch it to the
// main queue and block on a semaphore until the completion handler has written
// the file.  Returns 1 on success.  WebKit isn't imported, so the config class
// and method are reached dynamically, exactly as evaluateJavaScript is above.
static int leksah_snapshot_impl(const char *cpath, BOOL useRect, NSRect rect) {
    if (cpath == NULL || gLeksahWindow == nil) return 0;
    NSString *path = [NSString stringWithUTF8String:cpath];
    __block BOOL ok = NO;
    dispatch_semaphore_t sem = dispatch_semaphore_create(0);
    dispatch_async(dispatch_get_main_queue(), ^{
        id web = leksah_find_webview([gLeksahWindow contentView]);
        if (web == nil) { dispatch_semaphore_signal(sem); return; }
        Class cfgClass = NSClassFromString(@"WKSnapshotConfiguration");
        id cfg = (cfgClass != nil) ? [[cfgClass alloc] init] : nil;
        if (useRect && cfg != nil) {
            // NB: @catch (...), NOT a typed @catch: any typed catch (even
            // `id`) references an _OBJC_EHTYPE_* symbol via an
            // ARM64_RELOC_POINTER_TO_GOT relocation, which GHCi's RTS linker
            // can't process — and this file must stay loadable in ghci
            // (leksah.sh --ghci).  Same for every @catch in this file.
            @try { [cfg setValue:[NSValue valueWithRect:rect] forKey:@"rect"]; }
            @catch (...) {}
        }
        void (^handler)(id, id) = ^(id image, id error) {
            if (error == nil && [image isKindOfClass:[NSImage class]]) {
                CGImageRef cg = [(NSImage *)image CGImageForProposedRect:NULL
                                                                context:nil hints:nil];
                if (cg != NULL) {
                    NSBitmapImageRep *rep =
                        [[NSBitmapImageRep alloc] initWithCGImage:cg];
                    NSData *png = [rep representationUsingType:NSBitmapImageFileTypePNG
                                                   properties:@{}];
                    ok = (png != nil) && [png writeToFile:path atomically:YES];
                }
            }
            dispatch_semaphore_signal(sem);
        };
        SEL sel = @selector(takeSnapshotWithConfiguration:completionHandler:);
        ((void (*)(id, SEL, id, id))objc_msgSend)(web, sel, cfg, handler);
    });
    dispatch_semaphore_wait(sem, dispatch_time(DISPATCH_TIME_NOW, 10LL * NSEC_PER_SEC));
    return ok ? 1 : 0;
}

// The whole WKWebView content (for `leksah-cmd screenshot`).
int leksah_screenshot(const char *cpath) {
    return leksah_snapshot_impl(cpath, NO, NSZeroRect);
}

// Just the rectangle (x,y,w,h in CSS px) — the permission-free grab-region path.
int leksah_snapshot_rect(const char *cpath, int x, int y, int w, int h) {
    return leksah_snapshot_impl(cpath, YES, NSMakeRect(x, y, w, h));
}

// Ask the page for the x-range covered by the toolbar buttons, so a click there
// isn't turned into a window drag.  Async; the result updates the cached range.
static void leksah_measure_toolbar(void) {
    if (gLeksahWindow == nil) return;
    id web = leksah_find_webview([gLeksahWindow contentView]);
    if (web == nil) return;
    NSString *js =
        @"(function(){var es=document.querySelectorAll('.toolbar .toolbar-item');"
        @"if(!es.length)return '';var lo=1e9,hi=-1e9;"
        @"es.forEach(function(e){var r=e.getBoundingClientRect();"
        @"lo=Math.min(lo,r.left);hi=Math.max(hi,r.right);});return lo+','+hi;})()";
    void (^handler)(id, id) = ^(id result, id error) {
        (void)error;
        if ([result isKindOfClass:[NSString class]] && [(NSString *)result length] > 0) {
            NSArray<NSString *> *parts = [(NSString *)result componentsSeparatedByString:@","];
            if ([parts count] == 2) {
                gToolbarMinX = [parts[0] doubleValue];
                gToolbarMaxX = [parts[1] doubleValue];
            }
        }
    };
    SEL sel = @selector(evaluateJavaScript:completionHandler:);
    ((void (*)(id, SEL, id, id))objc_msgSend)(web, sel, js, handler);
}

// True if a window-base point is over one of the traffic-light buttons.
static BOOL leksah_on_window_button(NSWindow *win, NSPoint pWin) {
    NSWindowButton kinds[3] = { NSWindowCloseButton, NSWindowMiniaturizeButton, NSWindowZoomButton };
    for (int i = 0; i < 3; i++) {
        NSButton *b = [win standardWindowButton:kinds[i]];
        if (b != nil && NSPointInRect(pWin, [b convertRect:[b bounds] toView:nil])) return YES;
    }
    return NO;
}

// The web UI handles its own keyboard shortcuts; jsaddle dispatches key events
// asynchronously, so the web view can't reliably preventDefault in time and
// AppKit beeps about the "unhandled" key.  Swizzle -[NSResponder noResponderFor:]
// to swallow the beep for key events (it's the default impl that calls NSBeep).
static void (*gOrigNoResponderFor)(id, SEL, SEL) = NULL;
static void leksah_noResponderFor(id self, SEL _cmd, SEL eventSelector) {
    if (eventSelector == @selector(keyDown:) || eventSelector == @selector(keyUp:))
        return;  // no beep
    if (gOrigNoResponderFor != NULL) gOrigNoResponderFor(self, _cmd, eventSelector);
}

static void leksah_install_beep_suppression(void) {
    static BOOL installed = NO;
    if (installed) return;
    installed = YES;
    Method m = class_getInstanceMethod([NSResponder class], @selector(noResponderFor:));
    if (m != NULL) {
        gOrigNoResponderFor = (void (*)(id, SEL, SEL))method_getImplementation(m);
        method_setImplementation(m, (IMP)leksah_noResponderFor);
    }
}

// Tell the page whether the Command key is held (flags-changed local monitor).
// The DOM's own Meta keydown/keyup handling (badgesJs) covers keys typed into
// the page, but a CROSS-ORIGIN iframe (a browser pane) swallows key events
// before the parent document can see them — so the ⌘-held navigation hints
// never showed while the embedded page had focus.  AppKit sees every
// flags-changed event regardless of which frame WebKit routed focus to; mirror
// it into the key window's page via leksahCmdHeld (idempotent with the DOM
// path).  WebKit isn't imported: evaluateJavaScript is reached dynamically,
// like the snapshot code above.
static void leksah_install_cmdheld_monitor(void) {
    static BOOL installed = NO;
    if (installed) return;
    installed = YES;
    [NSEvent addLocalMonitorForEventsMatchingMask:NSEventMaskFlagsChanged
        handler:^NSEvent *(NSEvent *e) {
            BOOL held = ([e modifierFlags] & NSEventModifierFlagCommand) != 0;
            NSWindow *w = [NSApp keyWindow];
            if (w == nil || gWindows == nil
                || ![[gWindows allValues] containsObject:w]) return e;
            id web = leksah_find_webview([w contentView]);
            if (web == nil) return e;
            NSString *js = held ? @"window.leksahCmdHeld&&window.leksahCmdHeld(true)"
                                : @"window.leksahCmdHeld&&window.leksahCmdHeld(false)";
            SEL sel = @selector(evaluateJavaScript:completionHandler:);
            ((void (*)(id, SEL, id, id))objc_msgSend)(web, sel, js, nil);
            return e;
        }];
}

// Install a left-mouse-down monitor that drags the window from the title bar.
static void leksah_install_titlebar_drag(void) {
    static BOOL installed = NO;
    if (installed) return;
    installed = YES;
    if (gTarget == nil) gTarget = [[LeksahMenuTarget alloc] init];
    leksah_measure_toolbar();
    // Keep the toolbar x-range up to date (it isn't known until the page loads).
    [NSTimer scheduledTimerWithTimeInterval:0.5
                                     target:gTarget
                                   selector:@selector(leksahRemeasure:)
                                   userInfo:nil
                                    repeats:YES];
    [NSEvent addLocalMonitorForEventsMatchingMask:NSEventMaskLeftMouseDown
        handler:^NSEvent *(NSEvent *e) {
            NSWindow *w = [e window];
            NSView *content = (w != nil) ? [w contentView] : nil;
            // Drag any leksah OS window from its title-bar strip, not just the
            // first one — every window is tracked in gWindows.  (The toolbar
            // x-range is measured from window 0, but the layout is identical in
            // every window, so it applies to all of them.)
            if (content == nil || gWindows == nil
                || ![[gWindows allValues] containsObject:w]) return e;
            NSPoint p = [e locationInWindow];                       // origin bottom-left
            CGFloat yFromTop = NSHeight([content bounds]) - p.y;
            if (yFromTop < 0 || yFromTop > kLeksahTitlebarHeight) return e;  // below the title bar
            if (leksah_on_window_button(w, p)) return e;            // a traffic-light button
            if (p.x >= gToolbarMinX && p.x <= gToolbarMaxX) return e;  // a toolbar button
            [w performWindowDragWithEvent:e];
            return nil;                                              // consume; we handled it
        }];
}

// ---------------------------------------------------------------------------
// Transparent tmux panes: punch see-through, click-through holes in the window.
//
// The web side (window.leksahSetHoles) clips the page where a transparent tmux
// pane is and publishes the hole rectangles (viewport top-left CSS px) as
// window.__leksahHoles.  Here we (1) make the window/web view non-opaque while
// any hole exists, so the clipped-away region shows whatever is behind, and
// (2) toggle the window's ignoresMouseEvents based on whether the cursor is over
// a hole, so clicks/scrolls reach the app behind it.  ignoresMouseEvents is
// window-wide, so a click-through window stops getting its own events -- hence a
// global event monitor (which fires for events headed to other apps) in addition
// to the local one.
// ---------------------------------------------------------------------------

// Hole rects (x, y(top), w, h in CSS px), owned as a malloc'd C array so their
// lifetime doesn't depend on ObjC memory management (this file is manual-retain,
// not ARC — an autoreleased NSArray here would dangle and crash the monitor).
static NSRect *gHoles = NULL;
static NSUInteger gHoleCount = 0;
static BOOL gTransparentEnabled = NO;

static void leksah_apply_transparency(BOOL on) {
    if (gLeksahWindow == nil || on == gTransparentEnabled) return;
    gTransparentEnabled = on;
    id web = leksah_find_webview([gLeksahWindow contentView]);
    // Guard the WKWebView KVC: not every WKWebView exposes a settable
    // "drawsBackground" key, and an NSUnknownKeyException here would crash the
    // app.  Catch it so transparency degrades gracefully instead.
    @try {
        if (on) {
            [gLeksahWindow setOpaque:NO];
            [gLeksahWindow setBackgroundColor:[NSColor clearColor]];
            [gLeksahWindow setAcceptsMouseMovedEvents:YES];
            if (web != nil) [web setValue:@NO forKey:@"drawsBackground"];
        } else {
            [gLeksahWindow setOpaque:YES];
            [gLeksahWindow setBackgroundColor:[NSColor windowBackgroundColor]];
            [gLeksahWindow setIgnoresMouseEvents:NO];
            if (web != nil) [web setValue:@YES forKey:@"drawsBackground"];
        }
    } @catch (...) {   // (...), not NSException*: see the ghci reloc note above
    }
}

// Pass clicks through (or not) depending on whether the cursor is over a hole.
static void leksah_update_clickthrough(void) {
    if (gLeksahWindow == nil || gHoleCount == 0 || gHoles == NULL) return;
    NSView *content = [gLeksahWindow contentView];
    if (content == nil) return;
    NSPoint scr = [NSEvent mouseLocation];                                  // screen, bottom-left
    NSPoint pWin = [gLeksahWindow convertRectFromScreen:NSMakeRect(scr.x, scr.y, 0, 0)].origin;
    CGFloat yFromTop = NSHeight([content bounds]) - pWin.y;                  // -> top-left, like the page
    BOOL inHole = NO;
    for (NSUInteger i = 0; i < gHoleCount; i++) {
        NSRect r = gHoles[i];
        if (pWin.x >= r.origin.x && pWin.x <= r.origin.x + r.size.width &&
            yFromTop >= r.origin.y && yFromTop <= r.origin.y + r.size.height) { inHole = YES; break; }
    }
    [gLeksahWindow setIgnoresMouseEvents:inHole];
}

static void leksah_install_clickthrough_monitors(void) {
    static BOOL installed = NO;
    if (installed) return;
    installed = YES;
    NSEventMask mask = NSEventMaskMouseMoved | NSEventMaskLeftMouseDragged;
    [NSEvent addLocalMonitorForEventsMatchingMask:mask
        handler:^NSEvent *(NSEvent *e) { leksah_update_clickthrough(); return e; }];
    [NSEvent addGlobalMonitorForEventsMatchingMask:mask
        handler:^(NSEvent *e) { (void)e; leksah_update_clickthrough(); }];
}

// ---------------------------------------------------------------------------
// Snap another app's window over a (transparent) pane, via the accessibility
// (AXUIElement) API.  Needs the Accessibility permission; we prompt for it and
// otherwise no-op gracefully.  The web side publishes window.__leksahSnap =
// { rect: <pane viewport rect | null>, armed: <capture next window click> }.
// ---------------------------------------------------------------------------

// Multiple snapped windows, one per pane.  Each entry binds a foreign window
// (owned, +1) to a pane key ("tid:pid").
#define LEKSAH_MAX_SNAP 32
static struct { NSString *key; AXUIElementRef win; } gSnaps[LEKSAH_MAX_SNAP];
static int gSnapCount = 0;
static NSString *gPendingSnapKey = nil;      // pane the next picked/frontmost window binds to
static BOOL gSnapPicking = NO;
static id gSnapClickMonitor = nil;
static int gFrontmostTries = 0;              // ticks spent waiting for the browser

// Target for the dynamic Unsnap menu items: each carries its pane key.
@interface LeksahUnsnapTarget : NSObject
- (void)unsnap:(id)sender;
@end
@implementation LeksahUnsnapTarget
- (void)unsnap:(id)sender {
    NSString *key = [(NSMenuItem *)sender representedObject];
    if (key != nil && gHs.unsnap) gHs.unsnap([key UTF8String]);
}
@end
static LeksahUnsnapTarget *gUnsnapTarget = nil;

static int leksah_snap_index(NSString *key) {
    for (int i = 0; i < gSnapCount; i++)
        if ([gSnaps[i].key isEqualToString:key]) return i;
    return -1;
}

// Bind (own) window `win` to pane `key`, replacing any window already bound there.
static void leksah_snap_set(NSString *key, AXUIElementRef win) {
    if (key == nil) { if (win) CFRelease(win); return; }
    int i = leksah_snap_index(key);
    if (i >= 0) { if (gSnaps[i].win) CFRelease(gSnaps[i].win); gSnaps[i].win = win; }
    else if (gSnapCount < LEKSAH_MAX_SNAP) {
        gSnaps[gSnapCount].key = [key retain]; gSnaps[gSnapCount].win = win; gSnapCount++;
    } else if (win) { CFRelease(win); }
    leksah_rebuild_unsnap_menu();
}

static void leksah_snap_remove_at(int i) {
    if (i < 0 || i >= gSnapCount) return;
    [gSnaps[i].key release];
    if (gSnaps[i].win) CFRelease(gSnaps[i].win);
    for (int j = i + 1; j < gSnapCount; j++) gSnaps[j-1] = gSnaps[j];
    gSnapCount--;
}

// Fire-and-forget JS eval (no completion handler).
static void leksah_eval_js(id web, NSString *js) {
    if (web == nil) return;
    SEL sel = @selector(evaluateJavaScript:completionHandler:);
    ((void (*)(id, SEL, id, id))objc_msgSend)(web, sel, js, (id)nil);
}

// Tell a window's page where its viewport sits on screen, as
// @window.__leksahOrigin = [x, y]@ — the same coordinate space a DOM
// MouseEvent's screenX/screenY use (primary-screen top-left origin, y down,
// CSS px), so page code can convert a screen point to its own clientX/clientY
// by subtracting it.
//
// Why this has to come from here: a WKWebView reports @window.screenX@ as 0 and
// @outerWidth/outerHeight@ as 0 for EVERY leksah window, so a page cannot place
// itself.  It can infer the origin from @screenX - clientX@ of any real mouse
// event, but only for a window the pointer has actually visited — and macOS
// does not deliver mouse-moved to a NON-KEY window's WKWebView, so an untouched
// window learns nothing, and a window MOVED after its last click keeps a stale
// answer.  The cross-OS-window pane drag ('leksahDragPreview' in IDE.Web.Main)
// needs it for every window, always current, so it is pushed from here on
// attach and on every move/resize.
static void leksah_publish_origin(NSWindow *win) {
    if (win == nil) return;
    NSView *content = [win contentView];
    id web = leksah_find_webview(content);
    if (content == nil || web == nil) return;
    // Viewport (0,0) is the content view's TOP-left; in window coords
    // (bottom-left origin) that is (0, height).  Mirrors leksah_viewport_to_ax.
    NSRect scr = [win convertRectToScreen:
                      NSMakeRect(0.0, NSHeight([content bounds]), 0.0, 0.0)];
    CGFloat primaryH = NSHeight([[[NSScreen screens] firstObject] frame]);
    leksah_eval_js(web, [NSString stringWithFormat:
        @"window.__leksahOrigin=[%.1f,%.1f];window.__leksahOriginNative=1;",
        scr.origin.x, primaryH - scr.origin.y]);
}

// Every window's origin — after a screen-configuration change the primary
// screen's height (which the flip above is relative to) can itself change.
static void leksah_publish_all_origins(void) {
    if (gWindows == nil) return;
    for (NSWindow *w in [gWindows allValues]) leksah_publish_origin(w);
}

// A viewport rect (content-view-relative, top-left, CSS px) -> global AX screen
// coords (primary-screen top-left origin, y down).
static CGRect leksah_viewport_to_ax(NSRect vp) {
    NSView *content = [gLeksahWindow contentView];
    CGFloat ch = NSHeight([content bounds]);
    NSRect winRect = NSMakeRect(vp.origin.x, ch - (vp.origin.y + vp.size.height),
                                vp.size.width, vp.size.height);   // window, bottom-left
    NSRect scr = [gLeksahWindow convertRectToScreen:winRect];     // Cocoa screen, bottom-left
    CGFloat primaryH = NSHeight([[[NSScreen screens] firstObject] frame]);
    return CGRectMake(scr.origin.x, primaryH - (scr.origin.y + scr.size.height),
                      scr.size.width, scr.size.height);
}

// Raise every snapped window so it shows through leksah's (transparent) hole
// again after leksah comes forward.  kAXRaiseAction alone only reorders a window
// WITHIN its own app, and a background app's windows all sit below the active
// app's — so a window covered by *another* app stays hidden.  To lift it above
// those other background windows we activate its owning app (raising that app's
// windows), then re-assert leksah on top so it keeps focus with the snapped
// windows layered just beneath it.  The re-activation re-fires DidBecomeMain, so
// a guard breaks the recursion (cleared a beat later, after the notifications
// settle).
static BOOL gRaisingSnaps = NO;
static void leksah_raise_snaps(void) {
    if (gRaisingSnaps || gSnapCount == 0) return;
    gRaisingSnaps = YES;
    BOOL any = NO;
    for (int i = 0; i < gSnapCount; i++) {
        if (gSnaps[i].win == NULL) continue;
        AXUIElementPerformAction(gSnaps[i].win, kAXRaiseAction);   // frontmost in its app
        pid_t pid = 0;
        if (AXUIElementGetPid(gSnaps[i].win, &pid) == kAXErrorSuccess && pid != 0 && pid != getpid()) {
            NSRunningApplication *app =
                [NSRunningApplication runningApplicationWithProcessIdentifier:pid];
            if (app != nil) { [app activateWithOptions:0]; any = YES; }
        }
    }
    if (any) {
        // Foreign activations made leksah resign active; take it back so it stays
        // frontmost with the just-raised windows directly below it.
        [NSApp activateIgnoringOtherApps:YES];
        if (gLeksahWindow != nil) [gLeksahWindow makeKeyAndOrderFront:nil];
    }
    dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(0.25 * NSEC_PER_SEC)),
                   dispatch_get_main_queue(), ^{ gRaisingSnaps = NO; });
}

// Move/resize a bound window to an AX rect.
static void leksah_snap_window_to(AXUIElementRef win, CGRect r) {
    if (win == NULL) return;
    CGPoint pos = r.origin; CGSize size = r.size;
    AXValueRef posV  = AXValueCreate(kAXValueCGPointType, &pos);
    AXValueRef sizeV = AXValueCreate(kAXValueCGSizeType,  &size);
    if (posV)  { AXUIElementSetAttributeValue(win, kAXPositionAttribute, posV);  CFRelease(posV); }
    if (sizeV) { AXUIElementSetAttributeValue(win, kAXSizeAttribute,     sizeV); CFRelease(sizeV); }
}

// Repopulate the Unsnap submenu from the currently-bound windows (by title).
static void leksah_rebuild_unsnap_menu(void) {
    if (gUnsnapMenu == nil) return;
    if (gUnsnapTarget == nil) gUnsnapTarget = [[LeksahUnsnapTarget alloc] init];
    [gUnsnapMenu removeAllItems];
    for (int i = 0; i < gSnapCount; i++) {
        CFStringRef title = NULL;
        AXUIElementCopyAttributeValue(gSnaps[i].win, kAXTitleAttribute, (CFTypeRef *)&title);
        NSString *label = (title != NULL && [(NSString *)title length] > 0)
                            ? (NSString *)title : gSnaps[i].key;
        NSMenuItem *it = [[NSMenuItem alloc] initWithTitle:label
                                                    action:@selector(unsnap:) keyEquivalent:@""];
        [it setTarget:gUnsnapTarget];
        [it setRepresentedObject:gSnaps[i].key];
        [gUnsnapMenu addItem:it];
        [it release];
        if (title != NULL) CFRelease(title);
    }
    if (gSnapCount == 0) {
        NSMenuItem *it = [[NSMenuItem alloc] initWithTitle:@"(none snapped)"
                                                    action:NULL keyEquivalent:@""];
        [it setEnabled:NO];
        [gUnsnapMenu addItem:it];
        [it release];
    }
}

// Prompt for Accessibility if needed, else arm a one-shot global monitor that
// binds the window under the next click in another app.
static void leksah_begin_snap_pick(void) {
    if (gSnapPicking) return;
    if (!AXIsProcessTrusted()) {
        NSDictionary *opts = @{ (id)kAXTrustedCheckOptionPrompt : @YES };
        AXIsProcessTrustedWithOptions((CFDictionaryRef)opts);   // shows the system prompt
        return;   // not granted yet — bail gracefully; the user grants and re-invokes
    }
    gSnapPicking = YES;
    id mon = [NSEvent addGlobalMonitorForEventsMatchingMask:NSEventMaskLeftMouseDown
        handler:^(NSEvent *e) {
            (void)e;
            NSPoint m = [NSEvent mouseLocation];
            CGFloat primaryH = NSHeight([[[NSScreen screens] firstObject] frame]);
            AXUIElementRef sys = AXUIElementCreateSystemWide();
            AXUIElementRef elem = NULL;
            if (sys != NULL &&
                AXUIElementCopyElementAtPosition(sys, (float)m.x, (float)(primaryH - m.y), &elem) == kAXErrorSuccess
                && elem != NULL) {
                pid_t epid = 0;
                AXUIElementGetPid(elem, &epid);
                if (epid != getpid()) {                 // ignore clicks on leksah itself
                    AXUIElementRef win = NULL;
                    if (AXUIElementCopyAttributeValue(elem, kAXWindowAttribute, (CFTypeRef *)&win) == kAXErrorSuccess && win != NULL) {
                        leksah_snap_set(gPendingSnapKey, win);   // Copy returns +1, we own it
                    }
                    if (gSnapClickMonitor != nil) { [NSEvent removeMonitor:gSnapClickMonitor]; [gSnapClickMonitor release]; gSnapClickMonitor = nil; }
                    gSnapPicking = NO;
                }
            }
            if (elem != NULL) CFRelease(elem);
            if (sys != NULL) CFRelease(sys);
        }];
    gSnapClickMonitor = [mon retain];
}

// Private AX SPI: the CoreGraphics window id behind an AX window element.  Lets
// us tell which browser window is new (absent from the pre-`open` snapshot).
extern AXError _AXUIElementGetWindow(AXUIElementRef element, CGWindowID *idOut);

// Snapshot of on-screen window ids, taken when an open-browser snap is armed, so
// the bind can prefer a window that appeared *after* `open <url>` ran — i.e. the
// just-opened one — instead of guessing among pre-existing windows.
static CGWindowID *gSnapWins = NULL;
static int gSnapWinCount = 0;
static int gSnapPhase = 0;   // 0 = no snapshot yet, 1 = snapshot taken, waiting

static void leksah_free_window_snapshot(void) {
    free(gSnapWins);
    gSnapWins = NULL;
    gSnapWinCount = 0;
    gSnapPhase = 0;
}

static void leksah_take_window_snapshot(void) {
    free(gSnapWins);
    gSnapWins = NULL;
    gSnapWinCount = 0;
    CFArrayRef info = CGWindowListCopyWindowInfo(
        kCGWindowListOptionOnScreenOnly | kCGWindowListExcludeDesktopElements,
        kCGNullWindowID);
    if (info == NULL) return;
    CFIndex cnt = CFArrayGetCount(info);
    gSnapWins = (CGWindowID *)malloc(sizeof(CGWindowID) * (cnt > 0 ? (size_t)cnt : 1));
    if (gSnapWins != NULL) {
        for (CFIndex i = 0; i < cnt; i++) {
            CFDictionaryRef d = (CFDictionaryRef)CFArrayGetValueAtIndex(info, i);
            CFNumberRef num = (CFNumberRef)CFDictionaryGetValue(d, kCGWindowNumber);
            int wnum = 0;
            if (num != NULL && CFNumberGetValue(num, kCFNumberIntType, &wnum) && wnum != 0)
                gSnapWins[gSnapWinCount++] = (CGWindowID)wnum;
        }
    }
    CFRelease(info);
}

static BOOL leksah_window_in_snapshot(CGWindowID wid) {
    for (int i = 0; i < gSnapWinCount; i++)
        if (gSnapWins[i] == wid) return YES;
    return NO;
}

// Pick the window of app `pid` to snap onto the pane: prefer one that appeared
// after the snapshot (the just-opened window); else the app's focused window;
// else its main window.  The returned ref is owned (+1) by the caller.
static AXUIElementRef leksah_pick_snap_window(pid_t pid) {
    AXUIElementRef axApp = AXUIElementCreateApplication(pid);
    if (axApp == NULL) return NULL;
    AXUIElementRef chosen = NULL;
    CFArrayRef wins = NULL;
    if (AXUIElementCopyAttributeValue(axApp, kAXWindowsAttribute, (CFTypeRef *)&wins) == kAXErrorSuccess
        && wins != NULL) {
        CFIndex cnt = CFArrayGetCount(wins);
        for (CFIndex i = 0; i < cnt && chosen == NULL; i++) {
            AXUIElementRef w = (AXUIElementRef)CFArrayGetValueAtIndex(wins, i);
            CGWindowID wid = 0;
            if (_AXUIElementGetWindow(w, &wid) == kAXErrorSuccess && wid != 0
                && !leksah_window_in_snapshot(wid))
                chosen = (AXUIElementRef)CFRetain(w);   // a window that wasn't there before
        }
        CFRelease(wins);
    }
    // Fallbacks when nothing is new (e.g. the URL opened as a tab in an existing
    // window): the focused (key) window, then the main window.
    if (chosen == NULL) {
        AXUIElementRef f = NULL;
        if (AXUIElementCopyAttributeValue(axApp, kAXFocusedWindowAttribute, (CFTypeRef *)&f) == kAXErrorSuccess)
            chosen = f;
    }
    if (chosen == NULL) {
        AXUIElementRef m = NULL;
        if (AXUIElementCopyAttributeValue(axApp, kAXMainWindowAttribute, (CFTypeRef *)&m) == kAXErrorSuccess)
            chosen = m;
    }
    CFRelease(axApp);
    return chosen;
}

// Read window.__leksahHoles and refresh the cached rects + window transparency.
// Called on the same timer that remeasures the toolbar.
static void leksah_read_holes(void) {
    if (gLeksahWindow == nil) return;
    id web = leksah_find_webview([gLeksahWindow contentView]);
    if (web == nil) return;
    // The snap rect is recomputed *live* (leksahSnapRect) so the bound window
    // tracks leksah's own move/resize, not just the 0.5s hole refresh.
    NSString *js = @"JSON.stringify({holes: window.__leksahHoles||[], snap: {rects: (window.leksahSnapRects?window.leksahSnapRects():{}), keys: (window.__leksahSnapKeys||[]), armed: (window.__leksahSnap?window.__leksahSnap.armed:false), key: (window.__leksahSnap?window.__leksahSnap.key:null)}})";
    void (^handler)(id, id) = ^(id result, id error) {
        (void)error;
        NSRect *rects = NULL;
        NSUInteger n = 0;
        id snap = nil;
        if ([result isKindOfClass:[NSString class]]) {
            NSData *d = [(NSString *)result dataUsingEncoding:NSUTF8StringEncoding];
            id obj = [NSJSONSerialization JSONObjectWithData:d options:0 error:NULL];
            id arr = [obj isKindOfClass:[NSDictionary class]] ? [obj objectForKey:@"holes"] : nil;
            snap   = [obj isKindOfClass:[NSDictionary class]] ? [obj objectForKey:@"snap"]  : nil;
            if ([arr isKindOfClass:[NSArray class]]) {
                NSUInteger cap = [(NSArray *)arr count];
                if (cap > 0) rects = (NSRect *)malloc(cap * sizeof(NSRect));
                for (id o in (NSArray *)arr) {
                    if (rects == NULL || ![o isKindOfClass:[NSDictionary class]]) continue;
                    rects[n++] = NSMakeRect([[o objectForKey:@"x"] doubleValue],
                                            [[o objectForKey:@"y"] doubleValue],
                                            [[o objectForKey:@"w"] doubleValue],
                                            [[o objectForKey:@"h"] doubleValue]);
                }
            }
        }
        free(gHoles);
        gHoles = rects;
        gHoleCount = n;
        leksah_apply_transparency(n > 0);
        if (n == 0 && gLeksahWindow != nil)
            [gLeksahWindow setIgnoresMouseEvents:NO];
        else
            leksah_update_clickthrough();

        // Window snapping.  armed is the pick mode: "click" (menu) or "frontmost"
        // (open-browser), or absent.
        if ([snap isKindOfClass:[NSDictionary class]]) {
            id rectsV = [snap objectForKey:@"rects"];
            NSDictionary *rects = [rectsV isKindOfClass:[NSDictionary class]] ? (NSDictionary *)rectsV : nil;
            id keyV = [snap objectForKey:@"key"];
            NSString *pkey = [keyV isKindOfClass:[NSString class]] ? (NSString *)keyV : nil;
            // Geometry for the pane being armed for is ready once its key appears
            // in the published rects (a tick or two after arming).
            BOOL haveRect = (pkey != nil && rects != nil
                             && [[rects objectForKey:pkey] isKindOfClass:[NSDictionary class]]);
            id armedV = [snap objectForKey:@"armed"];
            NSString *armed = [armedV isKindOfClass:[NSString class]] ? (NSString *)armedV : nil;
            if ([armed isEqualToString:@"click"]) {
                [gPendingSnapKey release]; gPendingSnapKey = [pkey copy];   // pane this pick binds to
                leksah_begin_snap_pick();   // checks permission / prompts; arms a click monitor
                leksah_eval_js(web, @"if(window.__leksahSnap)window.__leksahSnap.armed=false;");  // one-shot
            } else if ([armed isEqualToString:@"frontmost"]) {
                if (!AXIsProcessTrusted()) {
                    NSDictionary *opts = @{ (id)kAXTrustedCheckOptionPrompt : @YES };
                    AXIsProcessTrustedWithOptions((CFDictionaryRef)opts);
                    leksah_eval_js(web, @"if(window.__leksahSnap)window.__leksahSnap.armed=false;");
                    gFrontmostTries = 0;
                    leksah_free_window_snapshot();
                } else {
                    [gPendingSnapKey release]; gPendingSnapKey = [pkey copy];
                    // First tick after arming: snapshot the on-screen windows, so
                    // once the browser is up we can bind the window `open <url>`
                    // added rather than guessing among its pre-existing windows.
                    if (gSnapPhase == 0) { leksah_take_window_snapshot(); gSnapPhase = 1; gFrontmostTries = 0; }
                    NSRunningApplication *fa = [[NSWorkspace sharedWorkspace] frontmostApplication];
                    // Only bind once the pane's snap rect is published — binding
                    // earlier would be undone instantly by the apply loop below.
                    if (fa != nil && [fa processIdentifier] != getpid() && haveRect) {   // browser up + geometry ready
                        AXUIElementRef w = leksah_pick_snap_window([fa processIdentifier]);
                        if (w != NULL) {
                            leksah_snap_set(gPendingSnapKey, w);
                            leksah_eval_js(web, @"if(window.__leksahSnap)window.__leksahSnap.armed=false;");
                            gFrontmostTries = 0;
                            leksah_free_window_snapshot();
                        } else if (++gFrontmostTries > 12) {
                            leksah_eval_js(web, @"if(window.__leksahSnap)window.__leksahSnap.armed=false;");
                            gFrontmostTries = 0;
                            leksah_free_window_snapshot();
                        }
                    } else if (++gFrontmostTries > 12) {   // ~6s; browser never came up
                        leksah_eval_js(web, @"if(window.__leksahSnap)window.__leksahSnap.armed=false;");
                        gFrontmostTries = 0;
                        leksah_free_window_snapshot();
                    }
                    // else: leksah still frontmost (browser not up yet) — retry next tick
                }
            }
            // For each bound window: unbind it if its pane was unsnapped (its key
            // left the full snapped set); else move it to its live rect if visible
            // (in rects) — a snapped-but-hidden pane keeps its binding but isn't
            // moved (it stays hidden behind the opaque window until re-shown).
            id keysV = [snap objectForKey:@"keys"];
            NSArray *keys = [keysV isKindOfClass:[NSArray class]] ? (NSArray *)keysV : nil;
            BOOL changed = NO;
            for (int i = gSnapCount - 1; i >= 0; i--) {
                // Auto-unsnap a window the user closed: its AX element goes invalid,
                // so drop it the same way the Unsnap menu does — leksah_unsnap also
                // clears the pane's transparency (via the web round-trip), which a
                // local remove wouldn't.
                CFTypeRef role = NULL;
                AXError axerr = AXUIElementCopyAttributeValue(gSnaps[i].win, kAXRoleAttribute, &role);
                if (role != NULL) CFRelease(role);
                if (axerr == kAXErrorInvalidUIElement) {
                    if (gHs.unsnap) gHs.unsnap([gSnaps[i].key UTF8String]);
                    continue;
                }
                if (keys != nil && ![keys containsObject:gSnaps[i].key]) {
                    leksah_snap_remove_at(i);
                    changed = YES;
                    continue;
                }
                id rr = rects ? [rects objectForKey:gSnaps[i].key] : nil;
                if ([rr isKindOfClass:[NSDictionary class]])
                    leksah_snap_window_to(gSnaps[i].win, leksah_viewport_to_ax(NSMakeRect(
                        [[rr objectForKey:@"x"] doubleValue], [[rr objectForKey:@"y"] doubleValue],
                        [[rr objectForKey:@"w"] doubleValue], [[rr objectForKey:@"h"] doubleValue])));
            }
            if (changed) leksah_rebuild_unsnap_menu();
        }
        // Keep the web side's view of the Accessibility grant fresh; the snap
        // only makes a pane transparent when this is true.
        leksah_eval_js(web, AXIsProcessTrusted() ? @"window.__leksahAxTrusted=true;" : @"window.__leksahAxTrusted=false;");
    };
    SEL sel = @selector(evaluateJavaScript:completionHandler:);
    ((void (*)(id, SEL, id, id))objc_msgSend)(web, sel, js, handler);
}

// Plays a macOS system sound when JS posts to the "leksahBeep" script message
// handler (the status light's beep — orange "Claude needs it shortly").  A
// system sound (NSSound) MIXES with any audio already playing on the machine
// and never interrupts it; a Web AudioContext, by contrast, grabbed the audio
// session and silenced other playback, which is why the in-page beep had to be
// disabled.  WebKit isn't linked here, so the handler is registered on the
// webview's userContentController via the runtime and only needs to answer
// -userContentController:didReceiveScriptMessage: (no formal protocol needed).
@interface LeksahBeepHandler : NSObject
@end
@implementation LeksahBeepHandler
- (void)userContentController:(id)ucc didReceiveScriptMessage:(id)message {
    (void)ucc; (void)message;
    // MRC: this file has no -fobjc-arc, so retain the (autoreleased) named sound
    // once and reuse it — else it would dangle after the pool drains mid-play.
    static NSSound *snd = nil;
    if (snd == nil) snd = [[NSSound soundNamed:@"Ping"] retain];
    if (snd != nil) { [snd stop]; [snd play]; }   // stop → rewind so repeats re-trigger
    else NSBeep();                                  // fall back to the alert sound
}
@end

// Speaks text posted to the "leksahSpeak" handler via AVSpeechSynthesizer — the
// terminal-bell announcement ("window <name>, pane <n>").  Like NSSound it plays
// over other audio and never seizes the session.  stopSpeakingAtBoundary first
// so a newer bell interrupts an in-progress announcement (newest alert wins)
// rather than being dropped while the synth is busy.  (Was NSSpeechSynthesizer,
// deprecated in macOS 14.)
@interface LeksahSpeakHandler : NSObject
@end
@implementation LeksahSpeakHandler
- (void)userContentController:(id)ucc didReceiveScriptMessage:(id)message {
    (void)ucc;
    static AVSpeechSynthesizer *synth = nil;
    if (synth == nil) synth = [[AVSpeechSynthesizer alloc] init];
    id body = [message valueForKey:@"body"];          // WKScriptMessage.body (via KVC)
    NSString *text = [body isKindOfClass:[NSString class]]
                       ? (NSString *)body : [body description];
    if (synth != nil && text != nil && [text length] > 0) {
        [synth stopSpeakingAtBoundary:AVSpeechBoundaryImmediate];
        [synth speakUtterance:[AVSpeechUtterance speechUtteranceWithString:text]];
    }
}
@end

// ---------------------------------------------------------------------------
// Native browser panes: a browser pane in the web UI is a REAL WKWebView
// overlaid on the pane's DOM rect (a placeholder div, class .browser-native),
// not an iframe — big sites (X-Frame-Options / CSP frame-ancestors) refuse
// to render in an iframe, but nothing can refuse a real web view.
//
// Geometry/lifecycle is driven from JS: a per-window reporter (Main.hs,
// browserNativeReporterJs) posts {wid, dark, panes:[{bid,x,y,w,h,vis}]} snapshots
// of every .browser-native element to the "leksahBrowserFrame" script
// message handler every ~250ms, and leksah_browser_reconcile creates views
// on first sight (as SUBVIEWS of the window's main webview — CSS px map 1:1
// onto its points), tracks rect/visibility, MOVES a view whose pane element
// shows up in another window (tab dragged across OS windows — the page
// survives), and destroys views whose bid has been absent for a while (the
// pane element left the DOM: tab/leaf closed).  Loads/back/forward/reload
// come from Haskell over leksah_browser_* (IDE.Web.NativeBrowser ops); page
// state flows back by evaluating window.__lkNb[bid]={u,b,f,t} in the host
// window's MAIN webview, where the reflex widget polls it.
// ---------------------------------------------------------------------------
static NSMutableDictionary *gBrowserViews   = nil;  // @(bid) -> WKWebView
static NSMutableDictionary *gBrowserPending = nil;  // @(bid) -> NSString url (load before creation)
static NSMutableDictionary *gBrowserWid     = nil;  // @(bid) -> @(wid) last reporting window
static NSMutableDictionary *gBrowserMiss    = nil;  // @(bid) -> @(consecutive absences)
static NSMutableDictionary *gBrowserZoom    = nil;  // @(bid) -> @(pageZoom last applied)

static void leksah_browser_ensure_dicts(void) {
    if (gBrowserViews   == nil) gBrowserViews   = [[NSMutableDictionary alloc] init];
    if (gBrowserPending == nil) gBrowserPending = [[NSMutableDictionary alloc] init];
    if (gBrowserWid     == nil) gBrowserWid     = [[NSMutableDictionary alloc] init];
    if (gBrowserMiss    == nil) gBrowserMiss    = [[NSMutableDictionary alloc] init];
    if (gBrowserZoom    == nil) gBrowserZoom    = [[NSMutableDictionary alloc] init];
}

// Push a view's navigation state into its host window's MAIN webview (the
// view's superview), where the widget's poll reads it.
static void leksah_browser_push_state(id web) {
    if (web == nil) return;
    NSNumber *bid = nil;
    for (NSNumber *k in gBrowserViews)
        if ([gBrowserViews objectForKey:k] == web) { bid = k; break; }
    if (bid == nil) return;
    NSString *url = @"";
    @try { url = [[web valueForKey:@"URL"] absoluteString] ?: @""; } @catch (...) {}
    NSString *title = @"";
    @try { title = [web valueForKey:@"title"] ?: @""; } @catch (...) {}
    BOOL back = NO, fwd = NO;
    @try { back = [[web valueForKey:@"canGoBack"] boolValue]; } @catch (...) {}
    @try { fwd  = [[web valueForKey:@"canGoForward"] boolValue]; } @catch (...) {}
    NSDictionary *d = @{ @"u": url, @"b": @(back), @"f": @(fwd), @"t": title };
    NSData *j = [NSJSONSerialization dataWithJSONObject:d options:0 error:nil];
    if (j == nil) return;
    NSString *json = [[[NSString alloc] initWithData:j encoding:NSUTF8StringEncoding] autorelease];
    NSView *host = [(NSView *)web superview];
    leksah_eval_js(host,
        [NSString stringWithFormat:@"window.__lkNb=window.__lkNb||{};window.__lkNb[%@]=%@;",
                  bid, json]);
}

// The light/dark mode the reporter last told us about (see
// leksah_browser_reconcile).  Read when building the colour-scheme client
// hint, which — unlike the NSAppearance — has to be attached per navigation.
static BOOL gBrowserDark = YES;

// ⌘-drag pane move: while a drag is in flight the page needs mousemove over
// browser panes too, but the native WKWebViews sit ON TOP of the main
// webview and would swallow them.  leafDragJs posts {drag:true/false} on the
// leksahBrowserFrame handler; in drag mode every browser view's hitTest:
// returns nil, so AppKit's hit-testing falls through to the host (main)
// webview underneath while the pane stays visible.  Views are created as
// LeksahBrowserView, a runtime subclass of WKWebView carrying just that
// override (WKWebView is weak-linked via NSClassFromString everywhere here,
// so the subclass must be built at runtime too).
static BOOL gBrowserDragMode = NO;
static Class gBrowserViewSuper = Nil;

// Which pane a view is (reverse lookup — the dictionaries are tiny).
static NSNumber *leksah_browser_bid_of(id web) {
    if (web == nil || gBrowserViews == nil) return nil;
    for (NSNumber *k in gBrowserViews)
        if ([gBrowserViews objectForKey:k] == web) return k;
    return nil;
}

// Does @v — or anything inside it — hold its window's keyboard focus?  (WebKit
// may park the first responder on an internal subview, so an identity test
// against the WKWebView alone isn't enough.)
static BOOL leksah_view_owns_responder(NSView *v) {
    if (v == nil) return NO;
    NSWindow *w = [v window];
    if (w == nil) return NO;
    id r = [w firstResponder];
    while ([r isKindOfClass:[NSView class]]) {
        if (r == v) return YES;
        r = [(NSView *)r superview];
    }
    return NO;
}

// Hand the keyboard back to the PAGE (the window's main webview) if this
// browser view is holding it.  A view that is hidden or destroyed must not stay
// first responder: AppKit would leave the window with no first responder at all
// and every keystroke would go nowhere — which is what "closing a browser pane
// doesn't leave an active pane" looked like.
static void leksah_browser_release_responder(NSView *v) {
    if (!leksah_view_owns_responder(v)) return;
    NSWindow *w = [v window];
    NSView *host = [w contentView];
    if (host != nil) [w makeFirstResponder:host];
}

// A browser view just took the keyboard (a click in its page, or AppKit moving
// the first responder there): tell the page, so the leksah pane holding the
// view becomes the ACTIVE pane.  Clicks inside a native view never reach the
// DOM, so without this the pane ring, the ⌘-number/flipper MRU and lwFocused
// all stayed on whichever pane was active before.  The page turns this into the
// same synthetic focusin a real click on a pane would have produced
// (browserNativeReporterJs).  Deduplicated per pane over a short window: both
// hooks below can fire for one click, and a second activation is wasted work.
static void leksah_browser_notify_activate(id web) {
    static NSNumber *lastBid = nil;
    static CFAbsoluteTime lastAt = 0;
    NSNumber *bid = leksah_browser_bid_of(web);
    if (bid == nil) return;
    CFAbsoluteTime now = CFAbsoluteTimeGetCurrent();
    if (lastBid != nil && [lastBid isEqual:bid] && now - lastAt < 0.3) return;
    [lastBid release];
    lastBid = [bid retain];
    lastAt = now;
    leksah_eval_js([(NSView *)web superview],
        [NSString stringWithFormat:
            @"window.leksahBrowserActivate&&window.leksahBrowserActivate(%@);", bid]);
}

static NSView *leksah_browser_hittest(id self, SEL _cmd, NSPoint point) {
    if (gBrowserDragMode) return nil;
    struct objc_super sup = { self, gBrowserViewSuper };
    NSView *hit = ((NSView *(*)(struct objc_super *, SEL, NSPoint))objc_msgSendSuper)(
                      &sup, _cmd, point);
    // Fallback activation hook: a hit-test resolving a mouse-DOWN into a view
    // that doesn't hold the keyboard yet means this click is about to activate
    // the pane.  becomeFirstResponder: is the primary hook; this one covers a
    // WebKit that routes focus through an internal view instead (the dedup in
    // notify_activate makes the overlap harmless).
    if (hit != nil && !leksah_view_owns_responder((NSView *)self)) {
        NSEvent *ev = [NSApp currentEvent];
        if (ev != nil && [ev type] == NSEventTypeLeftMouseDown)
            leksah_browser_notify_activate(self);
    }
    return hit;
}

static BOOL leksah_browser_become_first_responder(id self, SEL _cmd) {
    struct objc_super sup = { self, gBrowserViewSuper };
    BOOL ok = ((BOOL (*)(struct objc_super *, SEL))objc_msgSendSuper)(&sup, _cmd);
    if (ok) leksah_browser_notify_activate(self);
    return ok;
}

// The browser-pane view class: LeksahBrowserView (registered once; a ghci
// :reload finds the earlier registration).  Falls back to plain WKWebView —
// no pass-through, everything else intact — if the subclass can't be built.
static Class leksah_browser_view_class(void) {
    static Class cls = Nil;
    if (cls != Nil) return cls;
    Class wk = NSClassFromString(@"WKWebView");
    if (wk == Nil) return Nil;
    Class c = NSClassFromString(@"LeksahBrowserView");
    if (c == Nil) {
        c = objc_allocateClassPair(wk, "LeksahBrowserView", 0);
        if (c != Nil) {
            class_addMethod(c, @selector(hitTest:),
                            (IMP)leksah_browser_hittest, "@@:{CGPoint=dd}");
            class_addMethod(c, @selector(becomeFirstResponder),
                            (IMP)leksah_browser_become_first_responder, "c@:");
            objc_registerClassPair(c);
        }
    }
    if (c == Nil) return wk;
    gBrowserViewSuper = class_getSuperclass(c);
    cls = c;
    return cls;
}

#define LEKSAH_SCHEME_HINT_HEADER @"Sec-CH-Prefers-Color-Scheme"

// Stamp the colour-scheme client hint on a request we are about to load.
//
// The NSAppearance only reaches a page through the @c prefers-color-scheme
// media query; the big sites that theme SERVER-side — Google above all —
// switch on this hint instead.  Measured on www.google.com: identical URL and
// UA, the body background variable comes back @c --xhUGwc:#fff without the
// header and @c #22242a with it.  WebKit ships no UA client hints of its own,
// so a browser pane (like Safari) is served the light page unless we say so.
//
// NB the WebKit SPI route — @c _WKCustomHeaderFields on
// @c WKWebpagePreferences, which WebKit re-applies to every request to the
// main document's registrable domain — does NOT work for this header, and the
// failure is silent.  Verified against a header-dumping server: a plain
// @c X-… field set that way arrives on both the document and its subresources,
// while @c Sec-CH-Prefers-Color-Scheme set the same way is dropped (WebKit
// refuses to let an app forge @c Sec-* headers).  Only headers we put on the
// NSURLRequest ourselves survive — hence the reissue in decidePolicy below.
static NSURLRequest *leksah_with_scheme_hint(NSURLRequest *req) {
    if (req == nil) return req;
    NSMutableURLRequest *m = [[req mutableCopy] autorelease];
    [m setValue:(gBrowserDark ? @"dark" : @"light")
        forHTTPHeaderField:LEKSAH_SCHEME_HINT_HEADER];
    return m;
}

// Should this navigation be cancelled and reissued carrying the hint?  Only
// http(s) GETs of the main frame that don't already have it: a request we
// reissue comes back through here WITH the header, which is what stops the
// recursion.  Back/forward (2) and reload (3) are left alone — reissuing them
// would push a new history entry instead of moving within history, and WebKit
// already replays the original request (headers included) for both.  POSTs are
// left alone too: WKWebView strips the body from a navigation action, so a
// reissue would send an empty form.
static BOOL leksah_should_reissue(id action) {
    @try {
        NSNumber *type = [action valueForKey:@"navigationType"];
        if (type != nil && ([type intValue] == 2 || [type intValue] == 3)) return NO;
        id target = [action valueForKey:@"targetFrame"];
        if (target == nil) return NO;                       // new window/frameless
        NSNumber *isMain = [target valueForKey:@"isMainFrame"];
        if (isMain != nil && ![isMain boolValue]) return NO;
        NSURLRequest *req = [action valueForKey:@"request"];
        if (req == nil) return NO;
        if ([req valueForHTTPHeaderField:LEKSAH_SCHEME_HINT_HEADER] != nil) return NO;
        NSString *method = [req HTTPMethod];
        if (method != nil && ![method isEqualToString:@"GET"]) return NO;
        NSString *scheme = [[[req URL] scheme] lowercaseString];
        return [scheme isEqualToString:@"http"] || [scheme isEqualToString:@"https"];
    } @catch (...) { return NO; }
}

// Navigation + UI delegate for the browser views (OURS — never the main
// webview's, whose UIDelegate belongs to jsaddle).  WebKit calls these via
// respondsToSelector, so no formal protocol/headers are needed.
@interface LeksahBrowserDelegate : NSObject
@end
@implementation LeksahBrowserDelegate
// Carry the colour-scheme hint into navigations the PAGE starts (a link, a
// search box, location=…), not just the ones leksah loads — otherwise a
// server-themed site is dark on the URL we open and light from the first click
// on.  Since only headers on our own NSURLRequest survive (see above), the
// navigation is cancelled and reissued with the header stamped on; the reissued
// one already has it, so it is allowed straight through.  The hint is built
// from gBrowserDark at reissue time, so a mode flip takes effect from the next
// navigation (loaded pages keep their server-rendered theme until reloaded —
// their media-query side still flips instantly with the NSAppearance).
//
// Policies: 0 = WKNavigationActionPolicyCancel, 1 = …Allow (WebKit headers
// aren't imported here — this file reaches WebKit only through
// NSClassFromString/objc_msgSend).
- (void)webView:(id)web decidePolicyForNavigationAction:(id)action
                                           preferences:(id)prefs
                                       decisionHandler:(void (^)(NSInteger, id))decisionHandler {
    if (leksah_should_reissue(action)) {
        @try {
            NSURLRequest *req = leksah_with_scheme_hint([action valueForKey:@"request"]);
            decisionHandler(0, prefs);
            ((void (*)(id, SEL, id))objc_msgSend)(web, @selector(loadRequest:), req);
            return;
        } @catch (...) { /* fall through to a plain allow */ }
    }
    decisionHandler(1, prefs);
}
- (void)webView:(id)web didCommitNavigation:(id)nav {
    (void)nav; leksah_browser_push_state(web);
}
- (void)webView:(id)web didFinishNavigation:(id)nav {
    (void)nav; leksah_browser_push_state(web);
}
// target=_blank / window.open: load in the SAME view instead of a new window.
- (id)webView:(id)web createWebViewWithConfiguration:(id)cfg
        forNavigationAction:(id)action windowFeatures:(id)feat {
    (void)cfg; (void)feat;
    @try {
        id req = [action valueForKey:@"request"];
        if (req != nil)
            ((void (*)(id, SEL, id))objc_msgSend)(web, @selector(loadRequest:), req);
    } @catch (...) {}
    return nil;
}
@end

static LeksahBrowserDelegate *leksah_browser_delegate(void) {
    static LeksahBrowserDelegate *d = nil;
    if (d == nil) d = [[LeksahBrowserDelegate alloc] init];
    return d;
}

// The tail WebKit appends to its base user agent.  A bare WKWebView sends
// "…AppleWebKit/605.1.15 (KHTML, like Gecko)" with NOTHING after it, and sites
// that sniff for Safari read that as an unknown/ancient browser: google.com
// answers a browser pane with its no-JS BASIC page (gbv=2), which among other
// things has no dark styling at all.  Claiming the installed Safari's version
// (read at runtime so it ages with the OS; major.minor, as Safari reports it)
// gets the same markup a real Safari would.
static NSString *leksah_browser_ua_suffix(void) {
    static NSString *cached = nil;
    if (cached != nil) return cached;
    NSString *v = nil;
    @try {
        v = [[[NSBundle bundleWithPath:@"/Applications/Safari.app"] infoDictionary]
                objectForKey:@"CFBundleShortVersionString"];
    } @catch (...) {}
    if (![v isKindOfClass:[NSString class]] || [v length] == 0) v = @"18.0";
    NSArray *parts = [v componentsSeparatedByString:@"."];
    if ([parts count] >= 2)
        v = [NSString stringWithFormat:@"%@.%@", [parts objectAtIndex:0], [parts objectAtIndex:1]];
    cached = [[NSString stringWithFormat:@"Version/%@ Safari/605.1.15", v] retain];
    return cached;
}

static void leksah_browser_load_url(id web, NSString *url) {
    if (web == nil || url == nil || [url length] == 0) return;
    NSURL *u = [NSURL URLWithString:url];
    if (u == nil) return;
    ((void (*)(id, SEL, id))objc_msgSend)(web, @selector(loadRequest:),
        leksah_with_scheme_hint([NSURLRequest requestWithURL:u]));
}

// Give a browser view the light/dark appearance leksah itself is rendering in
// (the reporter forwards the page's prefers-color-scheme with every snapshot).
// Without an EXPLICIT appearance a view merely inherits the system one, so a
// page would report a different colour scheme than the surrounding UI whenever
// the two disagree; setting it also darkens WebKit's own chrome (scrollbars,
// form controls, the pre-load background).  Idempotent — re-applied only when
// the name actually differs, so the 250ms tick doesn't churn the view.
static void leksah_browser_apply_appearance(id web, BOOL dark) {
    if (web == nil) return;
    NSString *want = dark ? NSAppearanceNameDarkAqua : NSAppearanceNameAqua;
    @try {
        NSAppearance *cur = [(NSView *)web appearance];
        if (cur == nil || ![[cur name] isEqualToString:want])
            [(NSView *)web setAppearance:[NSAppearance appearanceNamed:want]];
    } @catch (...) {}
}

// One snapshot from one window's reporter: reconcile that window's views.
static void leksah_browser_reconcile(int wid, NSArray *panes, BOOL dark) {
    leksah_browser_ensure_dicts();
    NSWindow *win = (gWindows != nil) ? [gWindows objectForKey:@(wid)] : nil;
    NSView *host = (win != nil) ? [win contentView] : nil;   // the main webview
    if (host == nil) return;
    gBrowserDark = dark;   // for the per-navigation colour-scheme client hint
    NSMutableSet *seen = [NSMutableSet set];
    for (NSDictionary *p in panes) {
        NSNumber *bid = [p objectForKey:@"bid"];
        if (bid == nil) continue;
        [seen addObject:bid];
        [gBrowserMiss removeObjectForKey:bid];
        [gBrowserWid setObject:@(wid) forKey:bid];
        id web = [gBrowserViews objectForKey:bid];
        if (web == nil) {
            Class cfgClass = NSClassFromString(@"WKWebViewConfiguration");
            Class wkClass  = leksah_browser_view_class();   // WKWebView subclass
            if (cfgClass == Nil || wkClass == Nil) continue;
            id cfg = [[[cfgClass alloc] init] autorelease];
            @try { [[cfg valueForKey:@"preferences"] setValue:@YES forKey:@"developerExtrasEnabled"]; }
            @catch (...) {}
            @try { [cfg setValue:leksah_browser_ua_suffix() forKey:@"applicationNameForUserAgent"]; }
            @catch (...) {}
            web = [((id (*)(id, SEL, NSRect, id))objc_msgSend)(
                      [wkClass alloc], @selector(initWithFrame:configuration:),
                      NSMakeRect(0, 0, 100, 100), cfg) autorelease];
            @try { [web setValue:leksah_browser_delegate() forKey:@"navigationDelegate"]; } @catch (...) {}
            @try { [web setValue:leksah_browser_delegate() forKey:@"UIDelegate"]; } @catch (...) {}
            [(NSView *)web setAutoresizingMask:0];
            [gBrowserViews setObject:web forKey:bid];
            NSString *pending = [gBrowserPending objectForKey:bid];
            if (pending != nil) {
                leksah_browser_load_url(web, pending);
                [gBrowserPending removeObjectForKey:bid];
            }
        }
        leksah_browser_apply_appearance(web, dark);
        if ([(NSView *)web superview] != host) {
            [(NSView *)web removeFromSuperview];
            [host addSubview:(NSView *)web];
        }
        CGFloat x = [[p objectForKey:@"x"] doubleValue], y = [[p objectForKey:@"y"] doubleValue];
        CGFloat w = [[p objectForKey:@"w"] doubleValue], h = [[p objectForKey:@"h"] doubleValue];
        CGFloat H = [host bounds].size.height;
        NSRect fr = [host isFlipped] ? NSMakeRect(x, y, w, h)
                                     : NSMakeRect(x, H - y - h, w, h);
        [(NSView *)web setFrame:fr];
        BOOL vis = [[p objectForKey:@"vis"] boolValue];
        [(NSView *)web setHidden:!vis];
        // A pane you cannot see must not hold the keyboard: the tab was
        // switched away, or an overlay (the flipper!) is up and needs the keys
        // itself.  Model-driven, so it covers every route into that state —
        // DOM focus events can't, since the page often focuses an element that
        // ALREADY had focus and no focusin is fired at all.  Coming back into
        // view re-takes the keyboard from the page side (see the reporter).
        if (!vis) leksah_browser_release_responder((NSView *)web);
        // The pane's page zoom (per-pane font override x the window's page
        // zoom).  A native view is a SIBLING of the page, so the CSS zoom that
        // scales the rest of the UI does not touch its content — this is what
        // makes "everything scales" true for a browser pane as well.
        //
        // Only on CHANGE: this loop runs four times a second for the life of
        // the pane, and telling WebKit its zoom that often would be absurd.
        // pageZoom is macOS 11+, hence KVC in a @try — the same weak-linking
        // style used for the other newer properties here.
        double z = [[p objectForKey:@"z"] doubleValue];
        if (!(z > 0.05 && z < 20)) z = 1.0;
        NSNumber *lastZ = [gBrowserZoom objectForKey:bid];
        if (lastZ == nil || fabs([lastZ doubleValue] - z) > 0.001) {
            @try { [web setValue:@(z) forKey:@"pageZoom"]; } @catch (...) {}
            [gBrowserZoom setObject:@(z) forKey:bid];
        }
    }
    // Panes this window last owned but which no longer report: after ~3s of
    // absence the element has really left the DOM (closed) — destroy.  A tab
    // merely hidden still reports (vis:false); a cross-window drag re-reports
    // from the new window well inside the grace period.
    for (NSNumber *bid in [gBrowserViews allKeys]) {
        if ([seen containsObject:bid]) continue;
        if (![[gBrowserWid objectForKey:bid] isEqual:@(wid)]) continue;
        int miss = [[gBrowserMiss objectForKey:bid] intValue] + 1;
        // The FIRST absence hides the view (and gives the keyboard back) at
        // once, while the destroy below still waits out the grace period: a
        // closed pane whose view keeps painting — over whatever pane took its
        // place — for the whole three seconds is what "closing a browser pane
        // is slow" looked like.  A view that reappears is unhidden by the
        // vis: flag above, so a transient absence (a widget rebuild) only
        // blinks.
        if (miss == 1) {
            id gone = [gBrowserViews objectForKey:bid];
            [(NSView *)gone setHidden:YES];
            leksah_browser_release_responder((NSView *)gone);
        }
        if (miss > 12) {
            id web = [gBrowserViews objectForKey:bid];
            leksah_browser_release_responder((NSView *)web);
            [(NSView *)web removeFromSuperview];
            [gBrowserViews removeObjectForKey:bid];
            [gBrowserWid removeObjectForKey:bid];
            [gBrowserMiss removeObjectForKey:bid];
            [gBrowserZoom removeObjectForKey:bid];
        } else
            [gBrowserMiss setObject:@(miss) forKey:bid];
    }
}

// The page made a browser pane the active pane by a route that ISN'T a click in
// its view (a tab select, the flipper, ⌥-open): give that view the keyboard, or
// the keystrokes would keep going wherever they went before.  Hidden views are
// skipped — a background pane must never take the keyboard.
static void leksah_browser_focus_pane(int bid) {
    leksah_browser_ensure_dicts();
    NSView *web = [gBrowserViews objectForKey:@(bid)];
    if (web == nil || [web isHidden]) return;
    NSWindow *w = [web window];
    if (w != nil && !leksah_view_owns_responder(web)) [w makeFirstResponder:web];
}

// Keyboard focus landed on real DOM in window @wid's page (any pane but a
// browser one): take the first responder back from whichever browser view of
// that window holds it.  Only ever steals from OUR OWN views, so a stray
// release can't disturb the page's own focus.
static void leksah_browser_release_window(int wid) {
    if (gBrowserViews == nil) return;
    NSWindow *win = (gWindows != nil) ? [gWindows objectForKey:@(wid)] : nil;
    NSView *host = (win != nil) ? [win contentView] : nil;
    if (host == nil) return;
    for (NSNumber *bid in gBrowserViews) {
        NSView *v = [gBrowserViews objectForKey:bid];
        if ([v window] == win && leksah_view_owns_responder(v)) {
            [win makeFirstResponder:host];
            return;
        }
    }
}

// Teardown (ghci :reload): drop every browser view before the windows close,
// so nothing keeps their WebContent renderers alive across :main restarts.
static void leksah_browser_teardown(void) {
    if (gBrowserViews == nil) return;
    for (NSNumber *bid in [gBrowserViews allKeys]) {
        leksah_browser_release_responder(
            (NSView *)[gBrowserViews objectForKey:bid]);
        [(NSView *)[gBrowserViews objectForKey:bid] removeFromSuperview];
    }
    [gBrowserViews removeAllObjects];
    [gBrowserPending removeAllObjects];
    [gBrowserWid removeAllObjects];
    [gBrowserMiss removeAllObjects];
}

@interface LeksahBrowserFrameHandler : NSObject
@end
@implementation LeksahBrowserFrameHandler
- (void)userContentController:(id)ucc didReceiveScriptMessage:(id)message {
    (void)ucc;
    id body = [message valueForKey:@"body"];
    if (![body isKindOfClass:[NSDictionary class]]) return;
    // ⌘-drag pane move: {drag:bool} toggles browser-view hit-test
    // pass-through for the duration of the gesture (see gBrowserDragMode).
    id dragVal = [body objectForKey:@"drag"];
    if (dragVal != nil) {
        BOOL on = [dragVal boolValue];
        dispatch_async(dispatch_get_main_queue(), ^{ gBrowserDragMode = on; });
        return;
    }
    // Keyboard hand-over between the page and the native views (the page
    // decides; see the focusin listener in browserNativeReporterJs).
    // {focus:bid} — pane bid is the active pane now, give its view the
    // keyboard; {release:wid} — focus went to real DOM in window wid, take it
    // back from that window's browser views.
    id focusVal = [body objectForKey:@"focus"];
    if (focusVal != nil) {
        int fbid = [focusVal intValue];
        dispatch_async(dispatch_get_main_queue(), ^{ leksah_browser_focus_pane(fbid); });
        return;
    }
    id relVal = [body objectForKey:@"release"];
    if (relVal != nil) {
        int rwid = [relVal intValue];
        dispatch_async(dispatch_get_main_queue(), ^{ leksah_browser_release_window(rwid); });
        return;
    }
    int wid = [[body objectForKey:@"wid"] intValue];
    NSArray *panes = [body objectForKey:@"panes"];
    if (![panes isKindOfClass:[NSArray class]]) return;
    id darkVal = [body objectForKey:@"dark"];
    BOOL dark = (darkVal == nil) ? YES : [darkVal boolValue];   // dark is leksah's default
    dispatch_async(dispatch_get_main_queue(), ^{ leksah_browser_reconcile(wid, panes, dark); });
}
@end

// The Haskell-facing ops (IDE.Web.NativeBrowser): drive pane bid's view.
void leksah_browser_load(int bid, const char *curl) {
    NSString *url = (curl != NULL) ? [NSString stringWithUTF8String:curl] : nil;
    dispatch_async(dispatch_get_main_queue(), ^{
        leksah_browser_ensure_dicts();
        id web = [gBrowserViews objectForKey:@(bid)];
        if (web != nil) leksah_browser_load_url(web, url);
        else if (url != nil) [gBrowserPending setObject:url forKey:@(bid)];
    });
}

static void leksah_browser_send(int bid, SEL sel) {
    dispatch_async(dispatch_get_main_queue(), ^{
        leksah_browser_ensure_dicts();
        id web = [gBrowserViews objectForKey:@(bid)];
        if (web == nil) return;
        @try { ((void (*)(id, SEL))objc_msgSend)(web, sel); } @catch (...) {}
        leksah_browser_push_state(web);
    });
}
void leksah_browser_back(int bid)    { leksah_browser_send(bid, @selector(goBack)); }
void leksah_browser_forward(int bid) { leksah_browser_send(bid, @selector(goForward)); }
void leksah_browser_reload(int bid)  { leksah_browser_send(bid, @selector(reload)); }

// ---------------------------------------------------------------------------
// Menu-bar status item: the state of the Claude Code sessions running RIGHT NOW,
// visible even when leksah is hidden or another app is frontmost — and a menu
// listing those sessions, so one can be brought up from anywhere.
//
// The ICON is the worst state among the live sessions, pushed from Haskell
// (leksah_set_claude_status, from IDE.Web.MacMenu's poll of
// IDE.Web.Claude.claudeLiveBySession).  Same colours as the workspace tree's
// badges, and — for colour-blind accessibility, as with the in-page traffic
// light — a distinct SHAPE per state:
//   red triangle   a session is blocked on an approval prompt (it needs you)
//   amber diamond  a session is working (the agent, or a shell command)
//   green circle   every session is idle, ready for input
//   hollow ring    nothing running (a template image, so AppKit tints it for
//                  the current menu bar rather than shouting in either)
// Beside the icon, for the two states that want you, is HOW MANY sessions are in
// it ("3" next to the triangle = three are blocked): the glyph says what, the
// number says how much of it.  Green and grey carry no number — there is no
// quantity worth reading when everything is idle or nothing is running.
// CLICKING opens the session menu; choosing a session selects its tmux pane and
// brings up its terminal tab (gHs.claude_activate → Claude.showLiveSession).
//
// The in-page coordination traffic light (statusLightJs, posted as
// "leksahStatusItem") no longer drives the icon — this item is about the
// sessions themselves.  It survives as the menu's status line and in the
// tooltip, so "Claude is testing — hands off" is still readable from the menu
// bar while the icon says what the sessions are doing.
//
// Statics live in the dylib, so in ghci mode the item and its last state
// survive :reload.
static NSStatusItem *gStatusItem = nil;
static NSString *gClaudeState = @"none";  // aggregate live-session state
static int       gClaudeCount = 0;        // sessions IN that state; 0 = draw no number
static NSString *gClaudeTip   = nil;      // one-line summary for the tooltip
static NSArray  *gClaudeRows  = nil;      // @[@[state, title, tooltip, session id], …]
static NSString *gCoordState  = @"green"; // the in-page coordination light

@interface LeksahStatusItemTarget : NSObject
- (void)showLeksah:(id)sender;
- (void)claudeSession:(id)sender;
@end
static LeksahStatusItemTarget *gStatusTarget = nil;
@implementation LeksahStatusItemTarget
- (void)showLeksah:(id)sender {
    (void)sender;
    [NSApp activateIgnoringOtherApps:YES];
    if (gLeksahWindow != nil) [gLeksahWindow makeKeyAndOrderFront:nil];
}
- (void)claudeSession:(id)sender {
    // Choosing a session both fronts leksah and shows that session's terminal:
    // the Haskell side selects the tmux pane and opens its tab, which is only
    // useful with the window in front.
    [self showLeksah:sender];
    NSString *sid = [(NSMenuItem *)sender representedObject];
    if (sid != nil && gHs.claude_activate) gHs.claude_activate([sid UTF8String]);
}
@end

// Draw one state's shape at px×px, inset by @o, optionally with the
// agent-coordination ring around it.  flipped:YES so the point lists read in the
// same top-origin coordinates as the in-page clip-path polygons.  "ring" (as a
// shape) is stroked rather than filled, and marked template — but only when
// there is no coordination ring, since a template image would tint that too and
// its colour IS its meaning.
static NSImage *leksah_shape_image(NSString *shape, NSColor *color, CGFloat px,
                                   CGFloat o, NSColor *coordRing) {
    NSImage *img = [NSImage imageWithSize:NSMakeSize(px, px) flipped:YES
                    drawingHandler:^BOOL(NSRect dst) {
        (void)dst;
        CGFloat s = px - 2 * o;
        if (coordRing != nil) {
            // A circle just inside the image edge, around whatever shape follows.
            CGFloat rw = px / 12.0;
            NSBezierPath *r = [NSBezierPath bezierPathWithOvalInRect:
                                  NSMakeRect(rw/2, rw/2, px - rw, px - rw)];
            [coordRing setStroke];
            [r setLineWidth:rw];
            [r stroke];
        }
        NSBezierPath *p;
        if ([shape isEqualToString:@"triangle"]) {                // apex up
            p = [NSBezierPath bezierPath];
            [p moveToPoint:NSMakePoint(o + 0.50*s, o + 0.02*s)];
            [p lineToPoint:NSMakePoint(o + 0.98*s, o + 0.96*s)];
            [p lineToPoint:NSMakePoint(o + 0.02*s, o + 0.96*s)];
            [p closePath];
        } else if ([shape isEqualToString:@"diamond"]) {
            p = [NSBezierPath bezierPath];
            [p moveToPoint:NSMakePoint(o + 0.50*s, o)];
            [p lineToPoint:NSMakePoint(o + s,      o + 0.50*s)];
            [p lineToPoint:NSMakePoint(o + 0.50*s, o + s)];
            [p lineToPoint:NSMakePoint(o,          o + 0.50*s)];
            [p closePath];
        } else if ([shape isEqualToString:@"ring"]) {
            // Inset by half the line width, or the stroke would be clipped by
            // the image edge.
            CGFloat lw = px / 9.0;
            p = [NSBezierPath bezierPathWithOvalInRect:
                    NSMakeRect(o + lw/2, o + lw/2, s - lw, s - lw)];
            [color setStroke];
            [p setLineWidth:lw];
            [p stroke];
            return YES;
        } else {                                                  // circle
            p = [NSBezierPath bezierPathWithOvalInRect:NSMakeRect(o, o, s, s)];
        }
        [color setFill];
        [p fill];
        // Hairline dark edge so the light shapes read on a light menu bar.
        [[NSColor colorWithSRGBRed:0 green:0 blue:0 alpha:0.35] setStroke];
        [p setLineWidth:0.5];
        [p stroke];
        return YES;
    }];
    if ([shape isEqualToString:@"ring"] && coordRing == nil) [img setTemplate:YES];
    return img;
}

// A Claude state's shape+colour: the same colours as the workspace tree's
// session badges (#f85149 / #d29922 / #3fb950), so the menu bar and the tree
// never disagree about what a session is doing.  @coordRing is the
// agent-coordination ring to draw around it (nil for none).
static NSImage *leksah_claude_image(NSString *state, CGFloat px, CGFloat o,
                                    NSColor *coordRing) {
    if ([state isEqualToString:@"waiting"])
        return leksah_shape_image(@"triangle",
            [NSColor colorWithSRGBRed:0.973 green:0.318 blue:0.286 alpha:1], px, o, coordRing);
    if ([state isEqualToString:@"busy"])
        return leksah_shape_image(@"diamond",
            [NSColor colorWithSRGBRed:0.824 green:0.600 blue:0.133 alpha:1], px, o, coordRing);
    if ([state isEqualToString:@"idle"])
        return leksah_shape_image(@"circle",
            [NSColor colorWithSRGBRed:0.247 green:0.725 blue:0.314 alpha:1], px, o, coordRing);
    // Nothing running: a hollow disc, tinted by AppKit unless it carries a ring.
    return leksah_shape_image(@"ring",
        coordRing != nil ? [NSColor colorWithWhite:0.55 alpha:1] : [NSColor labelColor],
        px, o, coordRing);
}

// The agent-coordination ring's colour: an agent has claimed the UI.  nil while
// it's safe — the ring appearing at all is the signal.  Same colours as the
// in-page dot's ring (statusLightJs).
static NSColor *leksah_coord_ring_color(NSString *st) {
    if ([st isEqualToString:@"orange"])
        return [NSColor colorWithSRGBRed:1.000 green:0.584 blue:0.000 alpha:1];
    if ([st isEqualToString:@"red"])
        return [NSColor colorWithSRGBRed:1.000 green:0.231 blue:0.188 alpha:1];
    if ([st isEqualToString:@"blue"])
        return [NSColor colorWithSRGBRed:0.039 green:0.518 blue:1.000 alpha:1];
    return nil;
}

// nil while it is safe — the line, like the ring, exists only to say HANDS OFF.
// Announcing "safe to use" told everyone who isn't modifying leksah something
// they never needed to know, and read as a warning at a glance.
static NSString *leksah_coord_line(NSString *st) {
    if ([st isEqualToString:@"orange"]) return @"Leksah: Claude needs it shortly";
    if ([st isEqualToString:@"red"])    return @"Leksah: Claude is testing — hands off";
    if ([st isEqualToString:@"blue"])   return @"Leksah: Claude is rebuilding/restarting";
    return nil;
}

// Rebuild the item's menu from the pushed rows.  Cheap and only run when the
// payload actually changed (the Haskell poll pushes on change only), so there
// is no need for a menuNeedsUpdate: delegate calling back into Haskell from
// inside the menu-tracking run loop.  MAIN THREAD.
static void leksah_status_rebuild_menu(void) {
    if (gStatusItem == nil) return;
    NSMenu *menu = [[NSMenu alloc] init];
    [menu setAutoenablesItems:NO];      // the informational rows stay disabled
    if ([gClaudeRows count] == 0) {
        NSMenuItem *none = [[NSMenuItem alloc] initWithTitle:@"No Claude sessions running"
                                                     action:NULL keyEquivalent:@""];
        [none setEnabled:NO];
        [menu addItem:none];
        [none release];
    } else {
        for (NSArray *row in gClaudeRows) {
            NSMenuItem *mi = [[NSMenuItem alloc] initWithTitle:[row objectAtIndex:1]
                                  action:@selector(claudeSession:) keyEquivalent:@""];
            [mi setTarget:gStatusTarget];
            [mi setRepresentedObject:[row objectAtIndex:3]];
            [mi setToolTip:[row objectAtIndex:2]];
            [mi setImage:leksah_claude_image([row objectAtIndex:0], 14, 14 * (2.0/18.0), nil)];
            [menu addItem:mi];
            [mi release];
        }
    }
    [menu addItem:[NSMenuItem separatorItem]];
    // Only when an agent has claimed the UI; nothing at all while it's safe.
    NSString *coordLine = leksah_coord_line(gCoordState);
    if (coordLine != nil) {
        NSMenuItem *coord = [[NSMenuItem alloc] initWithTitle:coordLine
                                                      action:NULL keyEquivalent:@""];
        [coord setEnabled:NO];
        [menu addItem:coord];
        [coord release];
    }
    NSMenuItem *show = [[NSMenuItem alloc] initWithTitle:@"Show Leksah"
                            action:@selector(showLeksah:) keyEquivalent:@""];
    [show setTarget:gStatusTarget];
    [menu addItem:show];
    [show release];
    [gStatusItem setMenu:menu];         // a menu, so a click opens it
    [menu release];
}

// Create the item on first use.  MAIN THREAD.
//
// VARIABLE length, not square: the button grows a count beside the icon whenever
// sessions need attention (see leksah_status_refresh), and a square item would
// clip it.  With no count the button is its image plus AppKit's own padding, so
// it still reads as the square icon it was.
static void leksah_status_item_ensure(void) {
    if (gStatusItem != nil) return;
    if (gStatusTarget == nil) gStatusTarget = [[LeksahStatusItemTarget alloc] init];
    gStatusItem = [[[NSStatusBar systemStatusBar]
                      statusItemWithLength:NSVariableStatusItemLength] retain];
}

// Push the current state into the item: icon, count, tooltip and menu.  MAIN
// THREAD.
static void leksah_status_refresh(void) {
    if (gStatusItem == nil) return;
    // The session shape is drawn at 12 in the 18×18 button image (not 14) so the
    // coordination ring has clearance when it appears — a shape that changed size
    // with the ring would read as two different icons.
    [[gStatusItem button] setImage:
        leksah_claude_image(gClaudeState, 18, 3, leksah_coord_ring_color(gCoordState))];
    // HOW MANY are in that state, right of the icon — "3 sessions want you" is
    // the thing you can't read off a single glyph.  Only when the state isn't
    // green: Haskell sends 0 for all-idle and for nothing-running (csCount), so
    // the quiet states stay a bare icon.  A PLAIN title (not attributed) so
    // AppKit keeps colouring it for the current menu bar — light text on a dark
    // one — while setFont still gives us tabular digits that don't jitter as the
    // count changes.
    [[gStatusItem button] setFont:
        [NSFont monospacedDigitSystemFontOfSize:11 weight:NSFontWeightSemibold]];
    [[gStatusItem button] setImagePosition:
        gClaudeCount > 0 ? NSImageLeft : NSImageOnly];
    [[gStatusItem button] setTitle:
        gClaudeCount > 0 ? [NSString stringWithFormat:@"%d", gClaudeCount] : @""];
    NSString *claude = (gClaudeTip != nil && [gClaudeTip length] > 0)
                         ? gClaudeTip : @"Claude: no sessions running";
    // The coordination line only when it is NOT safe (nil = safe): the tooltip is
    // then purely about the sessions.
    NSString *coordLine = leksah_coord_line(gCoordState);
    [[gStatusItem button] setToolTip:
        coordLine != nil ? [NSString stringWithFormat:@"%@\n%@", claude, coordLine]
                         : claude];
    leksah_status_rebuild_menu();
}

// The in-page coordination traffic light changed ("leksahStatusItem").
static void leksah_status_item_set(NSString *state) {
    NSString *st = [((state != nil && [state length] > 0) ? state : @"green") copy];
    dispatch_async(dispatch_get_main_queue(), ^{
        [gCoordState release];
        gCoordState = st;               // takes the copy's reference
        leksah_status_item_ensure();
        leksah_status_refresh();
    });
}

// The live Claude sessions changed (pushed by Haskell's poll, on change only).
// @state is the aggregate ("waiting" / "busy" / "idle" / "none"), @count how many
// sessions are in that state (0 = draw no number: idle/none), @tip a
// one-line summary, and @rows one session per line as
// state \t title \t tooltip \t session-id (the tooltip's own newlines escaped
// as \n by the sender, since they'd otherwise end the row).
void leksah_set_claude_status(const char *state, int count, const char *tip,
                             const char *rows) {
    NSString *st = [[NSString stringWithUTF8String:(state != NULL ? state : "none")] copy];
    NSString *tp = [[NSString stringWithUTF8String:(tip   != NULL ? tip   : "")] copy];
    NSString *rw = [NSString stringWithUTF8String:(rows  != NULL ? rows  : "")];
    NSMutableArray *parsed = [[NSMutableArray alloc] init];
    for (NSString *line in [rw componentsSeparatedByString:@"\n"]) {
        if ([line length] == 0) continue;
        NSArray *f = [line componentsSeparatedByString:@"\t"];
        if ([f count] < 4) continue;
        [parsed addObject:@[ [f objectAtIndex:0], [f objectAtIndex:1],
                             [[f objectAtIndex:2] stringByReplacingOccurrencesOfString:@"\\n"
                                                                           withString:@"\n"],
                             [f objectAtIndex:3] ]];
    }
    dispatch_async(dispatch_get_main_queue(), ^{
        [gClaudeState release]; gClaudeState = st;   // each takes its copy's ref
        [gClaudeTip   release]; gClaudeTip   = tp;
        [gClaudeRows  release]; gClaudeRows  = parsed;
        gClaudeCount = count;
        leksah_status_item_ensure();
        leksah_status_refresh();
    });
}

@interface LeksahStatusHandler : NSObject
@end
@implementation LeksahStatusHandler
- (void)userContentController:(id)ucc didReceiveScriptMessage:(id)message {
    (void)ucc;
    id body = [message valueForKey:@"body"];          // WKScriptMessage.body (via KVC)
    NSString *state = [body isKindOfClass:[NSString class]]
                        ? (NSString *)body : [body description];
    leksah_status_item_set(state);
}
@end

// Register the "leksahBeep" / "leksahSpeak" / "leksahStatusItem" handlers on a
// webview's content controller (once per webview; each window has its own).
// Called from leksah_configure_window so it covers window 0 (created by
// jsaddle's AppDelegate) and every leksah_new_window alike.  Coexists with
// jsaddle's own handlers (different names).
static void leksah_install_beep_handler(id webview) {
    static LeksahBeepHandler *beepHandler = nil;
    static LeksahSpeakHandler *speakHandler = nil;
    static LeksahStatusHandler *statusHandler = nil;
    static LeksahBrowserFrameHandler *browserHandler = nil;
    if (beepHandler == nil)  beepHandler  = [[LeksahBeepHandler alloc] init];
    if (speakHandler == nil) speakHandler = [[LeksahSpeakHandler alloc] init];
    if (statusHandler == nil) statusHandler = [[LeksahStatusHandler alloc] init];
    if (browserHandler == nil) browserHandler = [[LeksahBrowserFrameHandler alloc] init];
    // Show the item (green) as soon as the first window is wired: the JS's
    // initial post can race this handler's install and be swallowed by its
    // try/catch, which would leave the menu bar empty until the first state
    // change.  Seed ONLY while the item doesn't exist yet — a later window
    // install (File ▸ New Window mid-test) must not reset a live red/blue.
    dispatch_async(dispatch_get_main_queue(), ^{
        if (gStatusItem == nil) leksah_status_item_set(@"green");
    });
    if (webview == nil) return;
    @try {
        id cfg = [webview valueForKey:@"configuration"];
        id ucc = [cfg valueForKey:@"userContentController"];
        SEL add = @selector(addScriptMessageHandler:name:);
        ((void (*)(id, SEL, id, id))objc_msgSend)(ucc, add, beepHandler,  @"leksahBeep");
        ((void (*)(id, SEL, id, id))objc_msgSend)(ucc, add, speakHandler, @"leksahSpeak");
        ((void (*)(id, SEL, id, id))objc_msgSend)(ucc, add, statusHandler, @"leksahStatusItem");
        ((void (*)(id, SEL, id, id))objc_msgSend)(ucc, add, browserHandler, @"leksahBrowserFrame");
    } @catch (...) {}   // (...): see the ghci reloc note above
}

// The per-window title-bar configuration shared by the first window and every
// window from leksah_new_window: transparent full-size-content title bar (so the
// web toolbar occupies it), per-window frame autosave, and the become-key /
// will-close observers that drive active-window tracking and close-merge.
// Returns YES if a previously-saved frame was restored (so the caller knows not
// to override it, e.g. by centring a fresh window).
static BOOL leksah_configure_window(NSWindow *win, int wid) {
    if (gWindows == nil) gWindows = [[NSMutableDictionary alloc] init];
    [gWindows setObject:win forKey:@(wid)];
    if (wid == 0) gLeksahWindow = win;
    // Use KVC for the 10.10+ properties so this compiles against the older
    // Cocoa headers in the build environment (the running OS has them).
    [win setValue:@YES forKey:@"titlebarAppearsTransparent"];
    [win setValue:@(1)  forKey:@"titleVisibility"];   // NSWindowTitleHidden
    win.styleMask |= (1 << 15);                        // FullSizeContentView
    win.movableByWindowBackground = YES;
    // Persist each window's position and size across restarts.  Cocoa stores the
    // frame in NSUserDefaults under this name; setFrameUsingName restores it now
    // (if previously saved) and setFrameAutosaveName keeps it saved on changes.
    // wid 0 keeps the historical name so existing saved geometry is preserved.
    NSString *autosave = (wid == 0) ? @"LeksahMainWindow"
                                    : [NSString stringWithFormat:@"LeksahWindow%d", wid];
    // setFrameAutosaveName is a SILENT no-op if another window still owns the
    // name — on a ghci reload the previous generation's window can linger just
    // long enough to steal it, leaving THIS window's moves/resizes unsaved
    // forever.  Evict any foreign owner first (it is a husk on its way out;
    // teardown also clears the name, this is the belt to that brace).
    for (NSWindow *o in [NSApp windows])
        if (o != win && [[o frameAutosaveName] isEqualToString:autosave])
            [o setFrameAutosaveName:@""];
    BOOL restored = [win setFrameUsingName:autosave];
    // A dying window whose content view was torn down can collapse to its
    // minimal frame (~64×64) and autosave that husk.  Never restore such a
    // frame: fall back to a sensible default and report "not restored" so the
    // caller centres the window.
    if (restored && (NSWidth([win frame]) < 400.0 || NSHeight([win frame]) < 300.0)) {
        NSLog(@"leksah: ignoring implausible saved frame for %@ (%.0fx%.0f)",
              autosave, NSWidth([win frame]), NSHeight([win frame]));
        [win setFrame:NSMakeRect(0.0, 500.0, 1200.0, 800.0) display:NO];
        restored = NO;
    }
    [win setFrameAutosaveName:autosave];
    // Frontmost window → active window (routes the bridges + flipper in-place).
    [[NSNotificationCenter defaultCenter] addObserverForName:NSWindowDidBecomeKeyNotification
        object:win queue:[NSOperationQueue mainQueue]
        usingBlock:^(NSNotification *note){ (void)note; if (gHs.window_activated) gHs.window_activated(wid); }];
    // Closing: merge this window's tabs elsewhere (or quit if it was the last),
    // then forget it.
    [[NSNotificationCenter defaultCenter] addObserverForName:NSWindowWillCloseNotification
        object:win queue:[NSOperationQueue mainQueue]
        usingBlock:^(NSNotification *note){
            (void)note;
            if (!gTeardownInProgress && gHs.window_closing) gHs.window_closing(wid);
            [gWindows removeObjectForKey:@(wid)];
        }];
    // Where this window's viewport is on screen — the page cannot work it out
    // (see 'leksah_publish_origin'), and the cross-window pane drag needs it.
    // These two fire continuously through a drag or resize, which is what keeps
    // it correct rather than merely initialised.
    [[NSNotificationCenter defaultCenter] addObserverForName:NSWindowDidMoveNotification
        object:win queue:[NSOperationQueue mainQueue]
        usingBlock:^(NSNotification *note){ (void)note; leksah_publish_origin(win); }];
    [[NSNotificationCenter defaultCenter] addObserverForName:NSWindowDidResizeNotification
        object:win queue:[NSOperationQueue mainQueue]
        usingBlock:^(NSNotification *note){ (void)note; leksah_publish_origin(win); }];
    // Let this window's JS ring the native beep (see LeksahBeepHandler).
    leksah_install_beep_handler(leksah_find_webview([win contentView]));
    // The page may not have loaded yet at attach; a short retry covers boot.
    leksah_publish_origin(win);
    dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(1.5 * NSEC_PER_SEC)),
                   dispatch_get_main_queue(), ^{ leksah_publish_origin(win); });
    return restored;
}

// Create a native window + WKWebView for a freshly-minted WindowId and hand the
// webview to Haskell (leksah_attach_window) so it can attach a jsaddle context.
// WebKit isn't linked into this file, so the WKWebView classes are reached
// dynamically (as elsewhere in this file).
// Webviews created by leksah_new_window carrying its deliberate extra retain
// (see the note there); leksah_close_all_windows releases exactly these.
// Non-retaining (opaque-pointer) hash table — membership only.
static NSHashTable *gOverRetainedWebViews = nil;

void leksah_new_window(int wid) {
    dispatch_async(dispatch_get_main_queue(), ^{
        NSRect contentSize = NSMakeRect(0.0, 500.0, 1000.0, 700.0);
        NSUInteger mask = NSWindowStyleMaskTitled | NSWindowStyleMaskResizable
                        | NSWindowStyleMaskClosable | NSWindowStyleMaskMiniaturizable;
        NSWindow *win = [[NSWindow alloc] initWithContentRect:contentSize
            styleMask:mask backing:NSBackingStoreBuffered defer:YES];
        win.backgroundColor = [NSColor whiteColor];
        // WKWebViewConfiguration with developer extras (matches jsaddle's AppDelegate).
        Class cfgClass = NSClassFromString(@"WKWebViewConfiguration");
        id cfg = [[cfgClass alloc] init];
        @try { [[cfg valueForKey:@"preferences"] setValue:@YES forKey:@"developerExtrasEnabled"]; }
        @catch (...) {}   // (...): see the ghci reloc note above
        Class wkClass = NSClassFromString(@"WKWebView");
        NSRect frame = [[win contentView] frame];
        id web = ((id (*)(id, SEL, NSRect, id))objc_msgSend)(
            [wkClass alloc], @selector(initWithFrame:configuration:), frame, cfg);
        [win setContentView:web];
        // Restore this window's saved position/size; only centre a genuinely new
        // window (no saved frame).  Centring unconditionally would clobber the
        // remembered location on a ghci-mode restart, where even window 0 comes
        // back through this path (see main/wkwebview/Main.hs).
        BOOL restored = leksah_configure_window(win, wid);
        if (!restored) [win center];
        [win makeKeyAndOrderFront:nil];
        // Activate the app.  jsaddle's SYNCHRONOUS callbacks ride a JS
        // prompt("JSaddleSync",…) handled by the WKWebView's UIDelegate — but
        // WebKit SUPPRESSES JS dialogs while the app is not frontmost/active, so
        // the prompt never reaches the handler and the sync round-trip hangs,
        // stalling the reflex DOM build (seen freezing at ~128 elements on a
        // ghci reload, where — unlike a first launch — re-entering [NSApp run]
        // does not re-activate the app).  Activating lets the (invisible) sync
        // prompts through so the build completes.  On File ▸ New Window the app
        // is already active, so this is a harmless no-op there.
        [NSApp activateIgnoringOtherApps:YES];
        if (gHs.attach_window) gHs.attach_window(wid, (void *)web);
        // MRC (this file is compiled without -fobjc-arc).  WKWebView keeps its own
        // copy of the configuration, so that +1 is ours to drop.
        [cfg release];
        // The webview's +1 from alloc/init is deliberately NOT balanced here:
        // it is what keeps the webview alive after its window goes away.
        //
        // gHs.attach_window hands the raw pointer to jsaddle-wkwebview, which
        // stores it and keeps calling -evaluateJavaScript: from its own threads.
        // While that holds, releasing the webview (letting the window's ref be
        // the last one) turns the next evaluateJavaScript into a use-after-free:
        // SIGSEGV in objc_retain under _Block_copy while the dispatch_async
        // block retains the freed webview.  Observed killing the whole ghci
        // process mid-:reload, since teardown races the reflex frame threads
        // still driving JS.
        //
        // So the +1 is balanced at TEARDOWN instead: leksah_close_all_windows
        // releases exactly the webviews recorded here — and it only runs after
        // the Haskell side has invalidated their jsaddle contexts
        // (jsaddleWebViewInvalidate, see IDE.Web.MacMenu's ghci cleanup), so no
        // jsaddle thread touches the pointer again.  That lets the webview
        // dealloc and its WebContent XPC renderer exit — previously one leaked
        // per :main restart.
        if (gOverRetainedWebViews == nil)
            gOverRetainedWebViews = [[NSHashTable hashTableWithOptions:
                NSPointerFunctionsOpaqueMemory | NSPointerFunctionsOpaquePersonality] retain];
        [gOverRetainedWebViews addObject:web];
    });
}

// Bring a specific window to the front (the global flipper's cross-window raise).
void leksah_raise_window(int wid) {
    dispatch_async(dispatch_get_main_queue(), ^{
        NSWindow *win = (gWindows != nil) ? [gWindows objectForKey:@(wid)] : nil;
        if (win != nil) [win makeKeyAndOrderFront:nil];
    });
}

// Raise a window WITHOUT making it key — the flipper's live preview: as the
// highlight moves, the OS window owning the highlighted entry comes to the top
// so you can see where you are about to land.  Key status must NOT move: the
// flipper commits on the modifier KEYUP, and that only reaches the page in the
// KEY window, so stealing key mid-flip would leave the flipper stuck open with
// nothing selected.  (Ordering is independent of key on macOS, so a non-key
// window can sit above the key one — exactly what a preview wants.)
void leksah_order_window_front(int wid) {
    dispatch_async(dispatch_get_main_queue(), ^{
        NSWindow *win = (gWindows != nil) ? [gWindows objectForKey:@(wid)] : nil;
        if (win != nil) [win orderFront:nil];
    });
}

// Close one window from Haskell: the never-empty-window rule (IDE.Web.Main)
// closes a window whose last pane just went, because an empty window shows
// nothing and offers no way back.  -close, not -performClose:, deliberately:
// -performClose: asks the delegate and beeps if it declines, whereas this
// decision has already been taken.  It still posts NSWindowWillClose, so the
// observer above runs leksah_window_closing and the Haskell side does its
// usual merge/unregister — this is exactly the red-button path, minus the
// button.  A no-op if the window is unknown (already closed, or never ours).
void leksah_close_window(int wid) {
    dispatch_async(dispatch_get_main_queue(), ^{
        NSWindow *win = (gWindows != nil) ? [gWindows objectForKey:@(wid)] : nil;
        if (win != nil) [win close];
    });
}

// --- ghci-mode lifecycle (leksah.sh --ghci) --------------------------------
// Under a cabal repl the app must be able to hand control back to the ghci
// prompt and take it again: [NSApp stop:] makes [NSApp run] return (after the
// next event — hence the posted no-op event), and leksah_run_app re-enters
// the loop.  Windows and app state survive a stop; IDE.Web.MacGlue drives
// these.

// First-launch flag for the reload story: 1 the first call in the process, 0
// after.  A C static in THIS dylib — which is preloaded once and never reloaded
// — so it survives a ghci :reload, unlike a Haskell CAF in the leksah-mac-glue
// object module (:reload gives that a fresh instance, resetting it, which
// wrongly routed reloads through the dead-bridge app-launch path).  Lets
// exe:leksah's main pick the runtime new-window path on every reload.
static int gTookFirstLaunch = 0;
int leksah_take_first_launch(void) {
    if (gTookFirstLaunch) return 0;
    gTookFirstLaunch = 1;
    return 1;
}

void leksah_stop_app(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        [NSApp stop:nil];
        NSEvent *e = [NSEvent otherEventWithType:NSEventTypeApplicationDefined
            location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0
            context:nil subtype:0 data1:0 data2:0];
        [NSApp postEvent:e atStart:YES];
    });
}

// Re-enter the run loop.  Must be called on the process main thread (with
// -fno-ghci-sandbox the ghci prompt evaluates there); blocks until the next
// leksah_stop_app.
void leksah_run_app(void) {
    [NSApp run];
}

// Close every leksah window without the merge/quit callbacks (teardown before
// a :reload + fresh :main).  gLeksahWindow is also in gWindows; just nil it.
void leksah_close_all_windows(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        gTeardownInProgress = 1;
        leksah_browser_teardown();
        // The tracked windows, PLUS a defensive sweep: any leksah window (one
        // hosting a WKWebView) still in [NSApp windows] that gWindows never
        // recorded.  Such orphans arise only from abnormal recovery paths (e.g.
        // a window created while state was wedged, or a hand-driven :main), but
        // once orphaned they are invisible to gWindows and so would linger — a
        // dead husk on screen — across every subsequent reload.  Matching on the
        // presence of a WKWebView keeps this to our own windows (not the
        // title-bar helper strips or a stray system panel).  A set dedupes the
        // overlap with gWindows.
        NSMutableSet *toClose = [NSMutableSet set];
        if (gWindows != nil) [toClose addObjectsFromArray:[gWindows allValues]];
        for (NSWindow *w in [NSApp windows])
            if (leksah_find_webview([w contentView]) != nil) [toClose addObject:w];
        for (NSWindow *w in toClose) {
            id web = leksah_find_webview([w contentView]);
            if (web != nil) {
                // Remove the script message handlers.
                // -addScriptMessageHandler: makes WebKit keep an internal strong
                // reference that pins the WKWebView (and thus its WebContent XPC
                // process) alive even after the window closes — the classic
                // WKWebView leak.  We add three (jsaddle / leksahBeep /
                // leksahSpeak), so leaving them on would pin the webview even
                // once its other refs are gone.  Necessary but NOT sufficient:
                // the webview is also deliberately over-retained in
                // leksah_new_window because jsaddle keeps using it — see the long
                // note there for why releasing it crashes the repl.  WebKit is
                // reached dynamically (id).
                @try {
                    id cfg = [web valueForKey:@"configuration"];
                    id ucc = [cfg valueForKey:@"userContentController"];
                    SEL removeAll = @selector(removeAllScriptMessageHandlers);
                    if ([ucc respondsToSelector:removeAll]) {          // macOS 11+
                        ((void (*)(id, SEL))objc_msgSend)(ucc, removeAll);
                    } else {
                        SEL rm = @selector(removeScriptMessageHandlerForName:);
                        for (NSString *nm in @[@"jsaddle", @"leksahBeep", @"leksahSpeak", @"leksahStatusItem", @"leksahBrowserFrame"])
                            ((void (*)(id, SEL, id))objc_msgSend)(ucc, rm, nm);
                    }
                } @catch (...) {}
                @try { ((void (*)(id, SEL))objc_msgSend)(web, @selector(stopLoading)); } @catch (...) {}
                // NB: do NOT nil navigationDelegate / UIDelegate here.  Both are
                // *weak* properties on WKWebView, so clearing them does nothing
                // for the refcount — and jsaddle-wkwebview owns the UIDelegate
                // for its synchronous JS↔Haskell bridge; poking it mid-teardown
                // can wedge jsaddle (a hung reload/teardown).
                //
                // Balance leksah_new_window's deliberate extra retain, for
                // exactly the webviews that carry it (the first-launch window's
                // webview does not — jsaddle's AppDelegate owns that one).  Safe
                // ONLY because the Haskell side invalidated every attached
                // jsaddle context (jsaddleWebViewInvalidate) BEFORE dispatching
                // this teardown block, so no jsaddle thread dereferences the
                // pointer again; any batch already queued on this (main) queue
                // holds its own block-copy retain and stays valid.  This is
                // what lets the webview dealloc and its WebContent XPC renderer
                // exit instead of leaking one per :main restart.
                if (gOverRetainedWebViews != nil
                        && [gOverRetainedWebViews containsObject:web]) {
                    [gOverRetainedWebViews removeObject:web];
                    [web release];
                }
            }
            // Persist the window's LAST GOOD frame, then detach it from frame
            // autosave before closing.  A closing window whose content view
            // dies can collapse to a ~64×64 husk and autosave THAT — and a
            // closed-but-not-yet-deallocated window still owns its autosave
            // name, making the next generation's setFrameAutosaveName a silent
            // no-op.  Saving explicitly (only a plausible frame) and clearing
            // the name closes both holes.
            NSString *nm = [w frameAutosaveName];
            if ([nm length] > 0) {
                if (NSWidth([w frame]) >= 400.0 && NSHeight([w frame]) >= 300.0)
                    [w saveFrameUsingName:nm];
                [w setFrameAutosaveName:@""];
            }
            [w close];
        }
        [gWindows removeAllObjects];
        gLeksahWindow = nil;
        gTeardownInProgress = 0;
    });
}

// jsaddle-wkwebview's AppDelegate answers YES to
// applicationShouldTerminateAfterLastWindowClosed:, which would exit the
// whole process — including the ghci session — when leksah's last window
// closes.  In ghci mode leksah manages quitting explicitly, so rewrite the
// delegate's answer to NO at runtime.
static BOOL leksah_no_auto_terminate(id self, SEL _cmd, id sender) {
    (void)self; (void)_cmd; (void)sender;
    return NO;
}

void leksah_disable_auto_terminate(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        id delegate = [NSApp delegate];
        if (delegate == nil) return;
        Class cls = object_getClass(delegate);
        SEL sel = @selector(applicationShouldTerminateAfterLastWindowClosed:);
        Method m = class_getInstanceMethod(cls, sel);
        if (m != NULL)
            method_setImplementation(m, (IMP)leksah_no_auto_terminate);
        else
            class_addMethod(cls, sel, (IMP)leksah_no_auto_terminate, "c@:@");
    });
}

static void leksah_configure_titlebar(void) {
    // Find leksah's window: the first one hosting a WKWebView.  [NSApp windows]
    // can also hold panels and husks from a previous ghci generation —
    // firstObject once stamped one of those as window 0.
    NSWindow *win = nil;
    for (NSWindow *w in [NSApp windows])
        if ([w contentView] != nil && leksah_find_webview([w contentView]) != nil) {
            win = w;
            break;
        }
    if (win == nil) {
        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(0.1 * NSEC_PER_SEC)),
                       dispatch_get_main_queue(), ^{ leksah_configure_titlebar(); });
        return;
    }
    // Per-window title-bar + autosave + become-key/will-close observers (wid 0)
    // — unless leksah_new_window already configured this window (the ghci
    // reload path): configuring twice would duplicate the become-key and
    // will-close observers.
    if (gWindows == nil || ![[gWindows allValues] containsObject:win])
        leksah_configure_window(win, 0);
    leksah_install_relaunch_signal();
    leksah_install_titlebar_drag();
    leksah_install_beep_suppression();
    leksah_install_cmdheld_monitor();
    leksah_install_clickthrough_monitors();
    // Re-apply the snap immediately when leksah itself moves or resizes (these
    // fire continuously during a drag), so the bound window tracks it smoothly
    // instead of only catching up on the 0.5s hole-refresh timer.
    [[NSNotificationCenter defaultCenter] addObserverForName:NSWindowDidMoveNotification
        object:win queue:[NSOperationQueue mainQueue]
        usingBlock:^(NSNotification *note){ (void)note; leksah_read_holes(); }];
    [[NSNotificationCenter defaultCenter] addObserverForName:NSWindowDidResizeNotification
        object:win queue:[NSOperationQueue mainQueue]
        usingBlock:^(NSNotification *note){ (void)note; leksah_read_holes(); }];
    // When leksah comes to the front, raise its snapped windows too — otherwise a
    // snapped window left behind another app's window stays hidden behind leksah's
    // (now-transparent) hole.  Reposition as well, in case geometry drifted.
    [[NSNotificationCenter defaultCenter] addObserverForName:NSWindowDidBecomeMainNotification
        object:win queue:[NSOperationQueue mainQueue]
        usingBlock:^(NSNotification *note){ (void)note; leksah_raise_snaps(); leksah_read_holes(); }];
    // A display change moves every window in the primary-screen-relative space
    // 'leksah_publish_origin' reports in, without any per-window notification.
    [[NSNotificationCenter defaultCenter]
        addObserverForName:NSApplicationDidChangeScreenParametersNotification
        object:nil queue:[NSOperationQueue mainQueue]
        usingBlock:^(NSNotification *note){ (void)note; leksah_publish_all_origins(); }];
}

void leksah_titlebar_setup(void) {
    dispatch_async(dispatch_get_main_queue(), ^{ leksah_configure_titlebar(); });
}

// Lazily add an "Open Recent" item (with an empty submenu) to the Workspace
// menu (the app's File menu — title kept in sync with MenuModel.hs).
static void leksah_ensure_recent_menu(void) {
    if (gRecentMenu != nil) return;
    if (gRecentTarget == nil) gRecentTarget = [[LeksahRecentTarget alloc] init];
    NSMenuItem *fileItem = (gMainMenu != nil) ? [gMainMenu itemWithTitle:@"Workspace"] : nil;
    if (fileItem == nil || [fileItem submenu] == nil) return;
    gRecentMenu = [[NSMenu alloc] initWithTitle:@"Open Recent"];
    NSMenuItem *recentItem = [[NSMenuItem alloc] initWithTitle:@"Open Recent"
                                                        action:NULL keyEquivalent:@""];
    [recentItem setSubmenu:gRecentMenu];
    [[fileItem submenu] addItem:recentItem];
    [recentItem release];
}

// Replace the Open Recent submenu with the given newline-separated paths (most
// recent first); each item opens its file via leksah_open_file.
void leksah_set_recent_files(const char *paths) {
    NSString *all = [NSString stringWithUTF8String:(paths != NULL ? paths : "")];
    dispatch_async(dispatch_get_main_queue(), ^{
        leksah_ensure_recent_menu();
        if (gRecentMenu == nil) return;
        [gRecentMenu removeAllItems];
        for (NSString *p in [all componentsSeparatedByString:@"\n"]) {
            if ([p length] == 0) continue;
            NSMenuItem *mi = [[NSMenuItem alloc] initWithTitle:[p lastPathComponent]
                                                        action:@selector(openRecent:)
                                                 keyEquivalent:@""];
            [mi setTarget:gRecentTarget];
            [mi setRepresentedObject:p];
            [mi setToolTip:p];
            [gRecentMenu addItem:mi];
            [mi release];
        }
    });
}

// File ▸ Open: show a native open panel and hand the chosen path back to Haskell
// (leksah_open_file), which opens it in an editor -- like the GTK fileOpen.
void leksah_show_open_panel(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        NSOpenPanel *panel = [NSOpenPanel openPanel];
        panel.canChooseFiles = YES;
        panel.canChooseDirectories = NO;
        panel.allowsMultipleSelection = NO;
        void (^done)(NSModalResponse) = ^(NSModalResponse result) {
            if (result == NSModalResponseOK) {
                NSURL *url = [[panel URLs] firstObject];
                if (url != nil && gHs.open_file) gHs.open_file([[url path] UTF8String]);
            }
        };
        if (gLeksahWindow != nil)
            [panel beginSheetModalForWindow:gLeksahWindow completionHandler:done];
        else
            [panel beginWithCompletionHandler:done];
    });
}

// File ▸ Open Project: pick a project file (cabal.project / stack.yaml) and add
// it to the workspace via leksah_open_project -- like the GTK projectOpen.
void leksah_show_open_project_panel(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        NSOpenPanel *panel = [NSOpenPanel openPanel];
        panel.canChooseFiles = YES;
        panel.canChooseDirectories = NO;
        panel.allowsMultipleSelection = NO;
        panel.message = @"Select a project file: cabal.project, stack.yaml, flake.nix, Cargo.toml, or pyproject.toml";
        void (^done)(NSModalResponse) = ^(NSModalResponse result) {
            if (result == NSModalResponseOK) {
                NSURL *url = [[panel URLs] firstObject];
                if (url != nil && gHs.open_project) gHs.open_project([[url path] UTF8String]);
            }
        };
        if (gLeksahWindow != nil)
            [panel beginSheetModalForWindow:gLeksahWindow completionHandler:done];
        else
            [panel beginWithCompletionHandler:done];
    });
}

// File ▸ Open Folder: pick a plain directory and add it to the workspace as a
// directory project (no build file needed).  Hands the chosen folder back via
// the same leksah_open_project callback -- the Haskell side (projectOpenPath)
// treats a directory as a plain-directory project and a file as a project file.
void leksah_show_open_folder_panel(void) {
    dispatch_async(dispatch_get_main_queue(), ^{
        NSOpenPanel *panel = [NSOpenPanel openPanel];
        panel.canChooseFiles = NO;
        panel.canChooseDirectories = YES;
        panel.allowsMultipleSelection = NO;
        panel.message = @"Select a folder to add to the workspace";
        void (^done)(NSModalResponse) = ^(NSModalResponse result) {
            if (result == NSModalResponseOK) {
                NSURL *url = [[panel URLs] firstObject];
                if (url != nil && gHs.open_project) gHs.open_project([[url path] UTF8String]);
            }
        };
        if (gLeksahWindow != nil)
            [panel beginSheetModalForWindow:gLeksahWindow completionHandler:done];
        else
            [panel beginWithCompletionHandler:done];
    });
}

// ---- Colour picker (NSColorPanel) -----------------------------------------
// The web <input type="color"> popover mis-anchors inside our transparent-
// titlebar window (WebKit positions it against the wrong rect), so the
// Preferences colour swatches use the native panel instead: leksah_pick_color
// opens the shared NSColorPanel seeded with the current value, and every
// change while it is open is reported back through leksah_color_picked (a
// Haskell foreign export) as "#rrggbb".

@interface LeksahColorTarget : NSObject
- (void)colorChanged:(id)sender;
@end

@implementation LeksahColorTarget
- (void)colorChanged:(id)sender {
    NSColorPanel *panel = (NSColorPanel *)sender;
    NSColor *c = [panel.color colorUsingColorSpace:[NSColorSpace sRGBColorSpace]];
    if (c == nil) return;
    char hex[8];
    snprintf(hex, sizeof hex, "#%02x%02x%02x",
             (int)lround(c.redComponent   * 255.0),
             (int)lround(c.greenComponent * 255.0),
             (int)lround(c.blueComponent  * 255.0));
    if (gHs.color_picked) gHs.color_picked(hex);
}
@end

static LeksahColorTarget *gColorTarget = nil;

void leksah_pick_color(const char *hexUtf8) {
    NSString *hex = [NSString stringWithUTF8String:hexUtf8 ?: ""];
    dispatch_async(dispatch_get_main_queue(), ^{
        if (gColorTarget == nil) gColorTarget = [[LeksahColorTarget alloc] init];
        NSColorPanel *panel = [NSColorPanel sharedColorPanel];
        [panel setShowsAlpha:NO];
        [panel setContinuous:YES];
        unsigned int rgb = 0;
        if ([hex hasPrefix:@"#"] && hex.length == 7) {
            NSScanner *sc = [NSScanner scannerWithString:[hex substringFromIndex:1]];
            if ([sc scanHexInt:&rgb])
                [panel setColor:[NSColor colorWithSRGBRed:((rgb >> 16) & 0xff) / 255.0
                                                    green:((rgb >>  8) & 0xff) / 255.0
                                                     blue:( rgb        & 0xff) / 255.0
                                                    alpha:1.0]];
        }
        [panel setTarget:gColorTarget];
        [panel setAction:@selector(colorChanged:)];
        [panel makeKeyAndOrderFront:nil];
    });
}
