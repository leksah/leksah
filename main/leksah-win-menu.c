/* Native Win32 menu bar for leksah-webview2 (see main/IDE/Web/Win32Menu.hs,
 * the Windows sibling of main/leksah-mac-menu.m).
 *
 * The menu is built on the UI thread (Haskell drives the leksah_win_menu_*
 * calls from inside jsaddle-webview2's run' callback, before the message loop
 * starts) and installed with SetMenu on the window jsaddle-webview2 created.
 * WM_COMMAND dispatch works by subclassing that window's wndProc — the shim's
 * own wndProc passes WM_COMMAND through to DefWindowProc.  The subclass must
 * not touch GWLP_USERDATA (the shim stores its JSaddleWV2* there).
 *
 * Calls that can arrive from Haskell (non-UI) threads — open panels, recent
 * files — are marshalled to the UI thread with PostMessage, the same pattern
 * the shim uses for wv2Eval.
 */

#include <windows.h>
#include <string.h>
#include <stdlib.h>

#include "HsFFI.h"

/* Haskell callbacks (foreign exports in IDE.Web.Win32Menu). */
extern void leksah_menu_action(HsInt32 tag);
extern void leksah_open_file(const char *utf8Path);
extern void leksah_open_project(const char *utf8Path);

/* WM_COMMAND id ranges. */
#define LEKSAH_CMD_EXIT      2      /* File > Exit */
#define LEKSAH_RECENT_BASE 500      /* 500..599: Open Recent entries */
#define LEKSAH_RECENT_MAX   64
#define LEKSAH_CMD_BASE   1000      /* 1000+tag: shared-menu-model commands */

/* UI-thread marshalling. */
#define WM_LEKSAH_OPEN_PANEL          (WM_APP + 0x101)
#define WM_LEKSAH_OPEN_PROJECT_PANEL  (WM_APP + 0x102)
#define WM_LEKSAH_SET_RECENT          (WM_APP + 0x103)

static HMENU gMenuBar = NULL;
static HMENU gFileMenu = NULL;    /* first top-level menu (File) */
static HMENU gRecentMenu = NULL;
static HMENU gStack[16];
static int gDepth = 0;

static HWND gWnd = NULL;
static WNDPROC gPrevProc = NULL;

/* Paths behind the Open Recent items, by index. */
static char *gRecent[LEKSAH_RECENT_MAX];
static int gRecentCount = 0;

/* Menu ids of the terminal-gated items (greyed unless a terminal tab is on
 * screen; see leksah_win_set_terminal_active). */
static UINT gGated[512];
static int gGatedCount = 0;
static int gTerminalActive = 0;

static LPWSTR wideFromUtf8(const char *s)
{
    if (!s) return NULL;
    int n = MultiByteToWideChar(CP_UTF8, 0, s, -1, NULL, 0);
    if (n <= 0) return NULL;
    LPWSTR w = (LPWSTR)malloc((size_t)n * sizeof(WCHAR));
    if (w) MultiByteToWideChar(CP_UTF8, 0, s, -1, w, n);
    return w;
}

static char *utf8FromWide(const WCHAR *w)
{
    if (!w) return NULL;
    int n = WideCharToMultiByte(CP_UTF8, 0, w, -1, NULL, 0, NULL, NULL);
    if (n <= 0) return NULL;
    char *s = (char *)malloc((size_t)n);
    if (s) WideCharToMultiByte(CP_UTF8, 0, w, -1, s, n, NULL, NULL);
    return s;
}

/* "label" or "label\taccel" — Win32 renders text after \t right-aligned in
 * the accelerator column. */
static LPWSTR captionFromUtf8(const char *label, const char *accel)
{
    if (!accel || !*accel) return wideFromUtf8(label);
    size_t n = strlen(label) + strlen(accel) + 2;
    char *c = (char *)malloc(n);
    if (!c) return NULL;
    lstrcpyA(c, label);
    lstrcatA(c, "\t");
    lstrcatA(c, accel);
    LPWSTR w = wideFromUtf8(c);
    free(c);
    return w;
}

void leksah_win_menu_begin(void)
{
    gMenuBar = CreateMenu();
    gFileMenu = NULL;
    gDepth = 0;
    gGatedCount = 0;
}

void leksah_win_menu_add_menu(const char *title)
{
    HMENU m = CreatePopupMenu();
    LPWSTR w = wideFromUtf8(title);
    AppendMenuW(gMenuBar, MF_POPUP, (UINT_PTR)m, w ? w : L"?");
    free(w);
    if (!gFileMenu) gFileMenu = m;
    gStack[0] = m;
    gDepth = 1;
}

void leksah_win_menu_add_item(const char *label, const char *accel,
                              int tag, int gated)
{
    UINT id = (UINT)(LEKSAH_CMD_BASE + tag);
    LPWSTR w = captionFromUtf8(label, accel);
    UINT flags = MF_STRING | (gated && !gTerminalActive ? MF_GRAYED : 0);
    AppendMenuW(gStack[gDepth - 1], flags, id, w ? w : L"?");
    free(w);
    if (gated && gGatedCount < (int)(sizeof gGated / sizeof gGated[0]))
        gGated[gGatedCount++] = id;
}

void leksah_win_menu_add_separator(void)
{
    AppendMenuW(gStack[gDepth - 1], MF_SEPARATOR, 0, NULL);
}

void leksah_win_menu_push_submenu(const char *title)
{
    HMENU m = CreatePopupMenu();
    LPWSTR w = wideFromUtf8(title);
    AppendMenuW(gStack[gDepth - 1], MF_POPUP, (UINT_PTR)m, w ? w : L"?");
    free(w);
    if (gDepth < (int)(sizeof gStack / sizeof gStack[0]))
        gStack[gDepth++] = m;
}

void leksah_win_menu_pop_submenu(void)
{
    if (gDepth > 1) gDepth--;
}

void leksah_win_set_terminal_active(int active)
{
    gTerminalActive = active;
    for (int i = 0; i < gGatedCount; i++)
        EnableMenuItem(gMenuBar, gGated[i],
                       MF_BYCOMMAND | (active ? MF_ENABLED : MF_GRAYED));
}

static void showOpenPanel(int project)
{
    WCHAR buf[MAX_PATH];
    buf[0] = 0;
    OPENFILENAMEW ofn;
    memset(&ofn, 0, sizeof ofn);
    ofn.lStructSize = sizeof ofn;
    ofn.hwndOwner = gWnd;
    ofn.lpstrFile = buf;
    ofn.nMaxFile = MAX_PATH;
    ofn.lpstrTitle = project ? L"Open Project" : L"Open File";
    /* NOCHANGEDIR: the dialog must not move leksah's cwd. */
    ofn.Flags = OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR;
    if (GetOpenFileNameW(&ofn)) {
        char *u = utf8FromWide(buf);
        if (u) {
            if (project) leksah_open_project(u); else leksah_open_file(u);
            free(u);
        }
    }
}

/* Rebuild the Open Recent submenu from a newline-separated UTF-8 path list
 * (UI thread; the string is owned here and freed). */
static void setRecentFiles(char *paths)
{
    for (int i = 0; i < gRecentCount; i++) { free(gRecent[i]); gRecent[i] = NULL; }
    gRecentCount = 0;
    while (gRecentMenu && GetMenuItemCount(gRecentMenu) > 0)
        RemoveMenu(gRecentMenu, 0, MF_BYPOSITION);
    if (!gRecentMenu) { free(paths); return; }
    char *p = paths;
    while (p && *p && gRecentCount < LEKSAH_RECENT_MAX) {
        char *nl = strchr(p, '\n');
        if (nl) *nl = 0;
        if (*p) {
            gRecent[gRecentCount] = _strdup(p);
            /* Label with the file name, like the macOS Open Recent menu. */
            const char *name = strrchr(p, '/');
            const char *bs = strrchr(p, '\\');
            if (bs && (!name || bs > name)) name = bs;
            name = name ? name + 1 : p;
            LPWSTR w = wideFromUtf8(name);
            AppendMenuW(gRecentMenu, MF_STRING,
                        (UINT)(LEKSAH_RECENT_BASE + gRecentCount), w ? w : L"?");
            free(w);
            gRecentCount++;
        }
        p = nl ? nl + 1 : NULL;
    }
    free(paths);
}

static LRESULT CALLBACK leksahWndProc(HWND hwnd, UINT msg, WPARAM wp, LPARAM lp)
{
    switch (msg) {
    case WM_COMMAND:
        if (HIWORD(wp) == 0) {  /* menu selection */
            int id = LOWORD(wp);
            if (id == LEKSAH_CMD_EXIT) {
                DestroyWindow(hwnd);
                return 0;
            }
            if (id >= LEKSAH_RECENT_BASE
                && id < LEKSAH_RECENT_BASE + gRecentCount) {
                if (gRecent[id - LEKSAH_RECENT_BASE])
                    leksah_open_file(gRecent[id - LEKSAH_RECENT_BASE]);
                return 0;
            }
            if (id >= LEKSAH_CMD_BASE) {
                leksah_menu_action((HsInt32)(id - LEKSAH_CMD_BASE));
                return 0;
            }
        }
        break;
    case WM_LEKSAH_OPEN_PANEL:         showOpenPanel(0); return 0;
    case WM_LEKSAH_OPEN_PROJECT_PANEL: showOpenPanel(1); return 0;
    case WM_LEKSAH_SET_RECENT:         setRecentFiles((char *)lp); return 0;
    }
    return CallWindowProcW(gPrevProc, hwnd, msg, wp, lp);
}

void leksah_win_menu_install(void *hwnd)
{
    gWnd = (HWND)hwnd;
    /* Windows conventions: Open Recent and Exit at the bottom of File. */
    if (gFileMenu) {
        gRecentMenu = CreatePopupMenu();
        AppendMenuW(gFileMenu, MF_SEPARATOR, 0, NULL);
        AppendMenuW(gFileMenu, MF_POPUP, (UINT_PTR)gRecentMenu, L"Open &Recent");
        AppendMenuW(gFileMenu, MF_SEPARATOR, 0, NULL);
        AppendMenuW(gFileMenu, MF_STRING, LEKSAH_CMD_EXIT, L"E&xit\tAlt+F4");
    }
    SetMenu(gWnd, gMenuBar);
    gPrevProc = (WNDPROC)SetWindowLongPtrW(gWnd, GWLP_WNDPROC,
                                           (LONG_PTR)leksahWndProc);
    DrawMenuBar(gWnd);
}

/* ---- Thread-safe entry points (Haskell threads -> UI thread). ---- */

void leksah_win_show_open_panel(void)
{
    if (gWnd) PostMessageW(gWnd, WM_LEKSAH_OPEN_PANEL, 0, 0);
}

void leksah_win_show_open_project_panel(void)
{
    if (gWnd) PostMessageW(gWnd, WM_LEKSAH_OPEN_PROJECT_PANEL, 0, 0);
}

void leksah_win_set_recent_files(const char *newlineSeparated)
{
    if (!gWnd) return;
    char *copy = _strdup(newlineSeparated ? newlineSeparated : "");
    if (copy && !PostMessageW(gWnd, WM_LEKSAH_SET_RECENT, 0, (LPARAM)copy))
        free(copy);
}
