/* ConPTY shim for leksah's Windows terminals.
 *
 * The web-UI terminal widget (IDE.Web.Widget.Terminal) drives a child shell
 * through a pseudo-terminal: xterm.js keystrokes are written to the PTY input,
 * and a reader thread pumps the PTY output back into xterm.  On POSIX that PTY
 * is posix-pty (forkpty); on Windows there is no forkpty, so we use the Win32
 * Pseudoconsole (ConPTY, Windows 10 1809+) — exactly what node-pty/VS Code do.
 *
 * ConPTY delivers and consumes a VT byte stream on a pair of pipes, which is
 * precisely what xterm.js wants, so IDE.Web.ConPty wraps these functions to
 * present the same read/write/resize interface posix-pty does.  The fiddly
 * STARTUPINFOEX + proc-thread attribute-list dance to attach a child to the
 * pseudoconsole is far cleaner in C than hand-marshalled from Haskell (cf.
 * main/leksah-win-menu.c), so it lives here.
 */

#define _WIN32_WINNT 0x0A00   /* Windows 10: CreatePseudoConsole et al. */
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdlib.h>
#include <string.h>

/* Older mingw-w64 headers may lack the pseudoconsole attribute constant even
 * though kernel32 exports the functions; define it defensively. */
#ifndef PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE
#define PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE 0x00020016
#endif

typedef struct LeksahConPty {
    HPCON  hpc;      /* the pseudoconsole */
    HANDLE hWrite;   /* our end of the input pipe  (keystrokes -> child) */
    HANDLE hRead;    /* our end of the output pipe (child -> xterm)      */
    HANDLE hProc;    /* the child process */
    HANDLE hThread;  /* the child's primary thread */
} LeksahConPty;

static void closeIfSet(HANDLE h)
{
    if (h && h != INVALID_HANDLE_VALUE) CloseHandle(h);
}

/* Spawn @cmdline@ attached to a fresh pseudoconsole of @cols@x@rows@.
 *
 *   cmdline  UTF-16 command line (mutable copy is made for CreateProcessW)
 *   cwd      working directory, or NULL to inherit
 *   env      double-NUL-terminated UTF-16 environment block, or NULL to inherit
 *
 * Returns an opaque handle, or NULL on failure. */
LeksahConPty *leksah_conpty_spawn(const wchar_t *cmdline, const wchar_t *cwd,
                                  const wchar_t *env, SHORT cols, SHORT rows)
{
    HANDLE inRead = NULL, inWrite = NULL, outRead = NULL, outWrite = NULL;
    HPCON  hpc = NULL;
    LPPROC_THREAD_ATTRIBUTE_LIST attrList = NULL;
    STARTUPINFOEXW si;
    PROCESS_INFORMATION pi;
    LeksahConPty *p = NULL;
    wchar_t *cmd = NULL;
    SIZE_T bytes = 0;
    COORD size;

    /* The child reads from inRead and writes to outWrite (handed to the
     * pseudoconsole); we keep inWrite and outRead. */
    if (!CreatePipe(&inRead, &inWrite, NULL, 0)) goto fail;
    if (!CreatePipe(&outRead, &outWrite, NULL, 0)) goto fail;

    size.X = cols;
    size.Y = rows;
    if (FAILED(CreatePseudoConsole(size, inRead, outWrite, 0, &hpc))) goto fail;

    /* Build the STARTUPINFOEX carrying the pseudoconsole attribute. */
    ZeroMemory(&si, sizeof(si));
    si.StartupInfo.cb = sizeof(STARTUPINFOEXW);
    InitializeProcThreadAttributeList(NULL, 1, 0, &bytes);
    attrList = (LPPROC_THREAD_ATTRIBUTE_LIST)HeapAlloc(GetProcessHeap(), 0, bytes);
    if (!attrList) goto fail;
    si.lpAttributeList = attrList;
    if (!InitializeProcThreadAttributeList(attrList, 1, 0, &bytes)) goto fail;
    if (!UpdateProcThreadAttribute(attrList, 0,
            PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE, hpc, sizeof(hpc),
            NULL, NULL)) goto fail;

    /* CreateProcessW may write to its command-line argument, so copy it. */
    {
        size_t n = (wcslen(cmdline) + 1) * sizeof(wchar_t);
        cmd = (wchar_t *)malloc(n);
        if (!cmd) goto fail;
        memcpy(cmd, cmdline, n);
    }

    ZeroMemory(&pi, sizeof(pi));
    if (!CreateProcessW(NULL, cmd, NULL, NULL, FALSE,
                        EXTENDED_STARTUPINFO_PRESENT | CREATE_UNICODE_ENVIRONMENT,
                        (LPVOID)env, cwd, &si.StartupInfo, &pi))
        goto fail;

    /* Success: the pseudoconsole owns its copies of inRead/outWrite; we no
     * longer need ours, nor the attribute list or command-line copy. */
    free(cmd);
    DeleteProcThreadAttributeList(attrList);
    HeapFree(GetProcessHeap(), 0, attrList);
    closeIfSet(inRead);
    closeIfSet(outWrite);

    p = (LeksahConPty *)calloc(1, sizeof(LeksahConPty));
    if (!p) {
        /* Can't return; tear the child down. */
        ClosePseudoConsole(hpc);
        closeIfSet(inWrite);
        closeIfSet(outRead);
        closeIfSet(pi.hThread);
        closeIfSet(pi.hProcess);
        return NULL;
    }
    p->hpc = hpc;
    p->hWrite = inWrite;
    p->hRead = outRead;
    p->hProc = pi.hProcess;
    p->hThread = pi.hThread;
    return p;

fail:
    if (cmd) free(cmd);
    if (attrList) {
        DeleteProcThreadAttributeList(attrList);
        HeapFree(GetProcessHeap(), 0, attrList);
    }
    if (hpc) ClosePseudoConsole(hpc);
    closeIfSet(inRead);
    closeIfSet(inWrite);
    closeIfSet(outRead);
    closeIfSet(outWrite);
    return NULL;
}

/* Blocking read of up to @cap@ bytes.  Returns the count (>0), or <=0 at
 * EOF / on error (the child exited, so the output pipe broke). */
int leksah_conpty_read(LeksahConPty *p, char *buf, int cap)
{
    DWORD n = 0;
    if (!ReadFile(p->hRead, buf, (DWORD)cap, &n, NULL)) return -1;
    return (int)n;
}

/* Write @n@ bytes to the child's input.  Returns bytes written, or -1. */
int leksah_conpty_write(LeksahConPty *p, const char *buf, int n)
{
    DWORD w = 0;
    if (!WriteFile(p->hWrite, buf, (DWORD)n, &w, NULL)) return -1;
    return (int)w;
}

void leksah_conpty_resize(LeksahConPty *p, SHORT cols, SHORT rows)
{
    COORD size;
    size.X = cols;
    size.Y = rows;
    ResizePseudoConsole(p->hpc, size);
}

/* Tear the session down: ClosePseudoConsole terminates the attached child. */
void leksah_conpty_close(LeksahConPty *p)
{
    if (!p) return;
    if (p->hpc) ClosePseudoConsole(p->hpc);
    closeIfSet(p->hWrite);
    closeIfSet(p->hRead);
    closeIfSet(p->hThread);
    closeIfSet(p->hProc);
    free(p);
}
