// Native macOS menu bar for leksah-wkwebview.
//
// Haskell (IDE.Web.MacMenu) drives this: it builds the menu from the shared
// menu model (leksah_menu_begin / add_menu / add_item) and installs it
// (leksah_menu_install).  When an item is chosen, the target calls back into
// Haskell via the exported `leksah_menu_action` with the item's tag, which
// runs the corresponding Command in the IDE.
//
// Menu objects live for the lifetime of the app (no explicit release).

#import <Cocoa/Cocoa.h>
#import <ApplicationServices/ApplicationServices.h>   // accessibility (AXUIElement) for window snapping
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
} leksah_haskell_callbacks;

static leksah_haskell_callbacks gHs;   // zero-initialised

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
    gMainMenu = [[NSMenu alloc] init];
    gMenuDepth = 0;

    // The application (apple-name) menu, so Quit etc. exist.
    NSMenuItem *appItem = [[NSMenuItem alloc] init];
    [gMainMenu addItem:appItem];
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
}

void leksah_menu_add_menu(const char *title) {
    if (gMainMenu == nil) leksah_menu_begin();
    NSString *t = [NSString stringWithUTF8String:title];
    NSMenuItem *item = [[NSMenuItem alloc] init];
    [gMainMenu addItem:item];
    NSMenu *sub = [[NSMenu alloc] initWithTitle:t];
    [item setSubmenu:sub];
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
    }
    [gMenuStack[gMenuDepth - 1] addItem:item];
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
        if (title != NULL) CFRelease(title);
    }
    if (gSnapCount == 0) {
        NSMenuItem *it = [[NSMenuItem alloc] initWithTitle:@"(none snapped)"
                                                    action:NULL keyEquivalent:@""];
        [it setEnabled:NO];
        [gUnsnapMenu addItem:it];
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

// Speaks text posted to the "leksahSpeak" handler via NSSpeechSynthesizer — the
// terminal-bell announcement ("window <name>, pane <n>").  Like NSSound it plays
// over other audio and never seizes the session.  stopSpeaking first so a newer
// bell interrupts an in-progress announcement (newest alert wins) rather than
// being dropped while the synth is busy.
@interface LeksahSpeakHandler : NSObject
@end
@implementation LeksahSpeakHandler
- (void)userContentController:(id)ucc didReceiveScriptMessage:(id)message {
    (void)ucc;
    static NSSpeechSynthesizer *synth = nil;
    if (synth == nil) synth = [[NSSpeechSynthesizer alloc] initWithVoice:nil];
    id body = [message valueForKey:@"body"];          // WKScriptMessage.body (via KVC)
    NSString *text = [body isKindOfClass:[NSString class]]
                       ? (NSString *)body : [body description];
    if (synth != nil && text != nil && [text length] > 0) {
        [synth stopSpeaking];
        [synth startSpeakingString:text];
    }
}
@end

// Register the "leksahBeep" / "leksahSpeak" handlers on a webview's content
// controller (once per webview; each window has its own).  Called from
// leksah_configure_window so it covers window 0 (created by jsaddle's
// AppDelegate) and every leksah_new_window alike.  Coexists with jsaddle's own
// handlers (different names).
static void leksah_install_beep_handler(id webview) {
    static LeksahBeepHandler *beepHandler = nil;
    static LeksahSpeakHandler *speakHandler = nil;
    if (beepHandler == nil)  beepHandler  = [[LeksahBeepHandler alloc] init];
    if (speakHandler == nil) speakHandler = [[LeksahSpeakHandler alloc] init];
    if (webview == nil) return;
    @try {
        id cfg = [webview valueForKey:@"configuration"];
        id ucc = [cfg valueForKey:@"userContentController"];
        SEL add = @selector(addScriptMessageHandler:name:);
        ((void (*)(id, SEL, id, id))objc_msgSend)(ucc, add, beepHandler,  @"leksahBeep");
        ((void (*)(id, SEL, id, id))objc_msgSend)(ucc, add, speakHandler, @"leksahSpeak");
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
    BOOL restored = [win setFrameUsingName:autosave];
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
    // Let this window's JS ring the native beep (see LeksahBeepHandler).
    leksah_install_beep_handler(leksah_find_webview([win contentView]));
    return restored;
}

// Create a native window + WKWebView for a freshly-minted WindowId and hand the
// webview to Haskell (leksah_attach_window) so it can attach a jsaddle context.
// WebKit isn't linked into this file, so the WKWebView classes are reached
// dynamically (as elsewhere in this file).
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
        // back through this path (see src-wkwebview/Main.hs).
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
    });
}

// Bring a specific window to the front (the global flipper's cross-window raise).
void leksah_raise_window(int wid) {
    dispatch_async(dispatch_get_main_queue(), ^{
        NSWindow *win = (gWindows != nil) ? [gWindows objectForKey:@(wid)] : nil;
        if (win != nil) [win makeKeyAndOrderFront:nil];
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
        for (NSWindow *w in toClose) [w close];
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
    NSWindow *win = [[NSApp windows] firstObject];
    if (win == nil || [win contentView] == nil) {
        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(0.1 * NSEC_PER_SEC)),
                       dispatch_get_main_queue(), ^{ leksah_configure_titlebar(); });
        return;
    }
    // Per-window title-bar + autosave + become-key/will-close observers (wid 0).
    leksah_configure_window(win, 0);
    leksah_install_relaunch_signal();
    leksah_install_titlebar_drag();
    leksah_install_beep_suppression();
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
}

void leksah_titlebar_setup(void) {
    dispatch_async(dispatch_get_main_queue(), ^{ leksah_configure_titlebar(); });
}

// Lazily add an "Open Recent" item (with an empty submenu) to the File menu.
static void leksah_ensure_recent_menu(void) {
    if (gRecentMenu != nil) return;
    if (gRecentTarget == nil) gRecentTarget = [[LeksahRecentTarget alloc] init];
    NSMenuItem *fileItem = (gMainMenu != nil) ? [gMainMenu itemWithTitle:@"File"] : nil;
    if (fileItem == nil || [fileItem submenu] == nil) return;
    gRecentMenu = [[NSMenu alloc] initWithTitle:@"Open Recent"];
    NSMenuItem *recentItem = [[NSMenuItem alloc] initWithTitle:@"Open Recent"
                                                        action:NULL keyEquivalent:@""];
    [recentItem setSubmenu:gRecentMenu];
    [[fileItem submenu] addItem:recentItem];
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
        panel.message = @"Select a flake.nix, cabal.project or stack.yaml file";
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
