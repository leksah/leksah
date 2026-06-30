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
#import <objc/message.h>
#import <objc/runtime.h>
#include <signal.h>
#include <unistd.h>
#include <pthread.h>

// Exported from Haskell (foreign export ccall).
extern void leksah_menu_action(int tag);
extern void leksah_open_file(const char *path);
extern void leksah_open_project(const char *path);

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

@interface LeksahMenuTarget : NSObject
- (void)leksahAction:(id)sender;
- (void)leksahRemeasure:(NSTimer *)timer;
@end

static void leksah_measure_toolbar(void);

@implementation LeksahMenuTarget
- (void)leksahAction:(id)sender {
    leksah_menu_action((int)[(NSMenuItem *)sender tag]);
}
- (void)leksahRemeasure:(NSTimer *)timer {
    (void)timer;
    leksah_measure_toolbar();
}
@end

static LeksahMenuTarget *gTarget = nil;
static NSMenu *gMainMenu = nil;
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
    if (path != nil) leksah_open_file([path UTF8String]);
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
    NSString *appName = [[NSProcessInfo processInfo] processName];
    [appMenu addItemWithTitle:[@"About " stringByAppendingString:appName]
                       action:@selector(orderFrontStandardAboutPanel:)
                keyEquivalent:@""];
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
            if (w != gLeksahWindow || content == nil) return e;
            NSPoint p = [e locationInWindow];                       // origin bottom-left
            CGFloat yFromTop = NSHeight([content bounds]) - p.y;
            if (yFromTop < 0 || yFromTop > kLeksahTitlebarHeight) return e;  // below the title bar
            if (leksah_on_window_button(w, p)) return e;            // a traffic-light button
            if (p.x >= gToolbarMinX && p.x <= gToolbarMaxX) return e;  // a toolbar button
            [w performWindowDragWithEvent:e];
            return nil;                                              // consume; we handled it
        }];
}

static void leksah_configure_titlebar(void) {
    NSWindow *win = [[NSApp windows] firstObject];
    if (win == nil || [win contentView] == nil) {
        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(0.1 * NSEC_PER_SEC)),
                       dispatch_get_main_queue(), ^{ leksah_configure_titlebar(); });
        return;
    }
    gLeksahWindow = win;
    // Use KVC for the 10.10+ properties so this compiles against the older
    // Cocoa headers in the build environment (the running OS has them).
    [win setValue:@YES forKey:@"titlebarAppearsTransparent"];
    [win setValue:@(1)  forKey:@"titleVisibility"];   // NSWindowTitleHidden
    win.styleMask |= (1 << 15);                        // FullSizeContentView
    win.movableByWindowBackground = YES;
    // Persist the window's position and size across restarts.  Cocoa stores the
    // frame in NSUserDefaults under this name; setFrameUsingName restores it now
    // (if previously saved) and setFrameAutosaveName keeps it saved on changes.
    [win setFrameUsingName:@"LeksahMainWindow"];
    [win setFrameAutosaveName:@"LeksahMainWindow"];
    leksah_install_relaunch_signal();
    leksah_install_titlebar_drag();
    leksah_install_beep_suppression();
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
                if (url != nil) leksah_open_file([[url path] UTF8String]);
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
                if (url != nil) leksah_open_project([[url path] UTF8String]);
            }
        };
        if (gLeksahWindow != nil)
            [panel beginSheetModalForWindow:gLeksahWindow completionHandler:done];
        else
            [panel beginWithCompletionHandler:done];
    });
}
