; Leksah Windows installer (NSIS / MUI2).
;
; Built with makensis on the Linux cross builder (see nix/windows-installer.nix),
; so it is never compiled on a Windows host.  Passed in via -D on the command line:
;   STAGING  - staged install tree: bin\ (leksah.exe + DLLs) and leksah\ (datadir)
;   VERSION  - display version
;   OUTFILE  - output path of the generated Setup.exe
;   ICON     - path to leksah.ico
;   LICENSE  - path to the license text shown on the license page
;
; Install layout (matches leksah's relocatable datadir logic in
; IDE.Core.State.leksahSubDir: the exe must be named leksah.exe, and the data
; dir is <installroot>\leksah):
;   $INSTDIR\bin\leksah.exe   (+ runtime DLLs, WebView2Loader.dll)
;   $INSTDIR\leksah\...        (data, pics, cm6, xterm, fonts, language-specs)

Unicode true
SetCompressor /SOLID lzma

!include "MUI2.nsh"
!include "x64.nsh"
!include "LogicLib.nsh"

Name "Leksah"
OutFile "${OUTFILE}"
InstallDir "$PROGRAMFILES64\Leksah"
InstallDirRegKey HKLM "Software\Leksah" "InstallDir"
RequestExecutionLevel admin

VIProductVersion "0.17.0.0"
VIAddVersionKey "ProductName" "Leksah"
VIAddVersionKey "FileDescription" "Leksah Haskell IDE"
VIAddVersionKey "FileVersion" "${VERSION}"
VIAddVersionKey "ProductVersion" "${VERSION}"
VIAddVersionKey "LegalCopyright" "Leksah authors, GPL"

; --- MUI ---
!define MUI_ABORTWARNING
!define MUI_ICON "${ICON}"
!define MUI_UNICON "${ICON}"
!define MUI_FINISHPAGE_RUN "$INSTDIR\bin\leksah.exe"
!define MUI_FINISHPAGE_RUN_TEXT "Launch Leksah"

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "${LICENSE}"
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

!insertmacro MUI_LANGUAGE "English"

Function .onInit
  ${IfNot} ${RunningX64}
    MessageBox MB_OK|MB_ICONSTOP "Leksah requires 64-bit Windows."
    Abort
  ${EndIf}
  SetRegView 64
FunctionEnd

Section "Leksah" SecMain
  SectionIn RO
  SetOutPath "$INSTDIR"
  File /r "${STAGING}/bin"
  File /r "${STAGING}/leksah"

  WriteRegStr HKLM "Software\Leksah" "InstallDir" "$INSTDIR"

  ; Add/Remove Programs entry
  !define UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\Leksah"
  WriteRegStr HKLM "${UNINST_KEY}" "DisplayName" "Leksah Haskell IDE"
  WriteRegStr HKLM "${UNINST_KEY}" "DisplayVersion" "${VERSION}"
  WriteRegStr HKLM "${UNINST_KEY}" "DisplayIcon" "$INSTDIR\bin\leksah.exe"
  WriteRegStr HKLM "${UNINST_KEY}" "Publisher" "Leksah authors"
  WriteRegStr HKLM "${UNINST_KEY}" "InstallLocation" "$INSTDIR"
  WriteRegStr HKLM "${UNINST_KEY}" "UninstallString" "$INSTDIR\Uninstall.exe"
  WriteRegDWORD HKLM "${UNINST_KEY}" "NoModify" 1
  WriteRegDWORD HKLM "${UNINST_KEY}" "NoRepair" 1

  WriteUninstaller "$INSTDIR\Uninstall.exe"

  ; Shortcuts
  CreateDirectory "$SMPROGRAMS\Leksah"
  CreateShortcut "$SMPROGRAMS\Leksah\Leksah.lnk" "$INSTDIR\bin\leksah.exe" "" "$INSTDIR\bin\leksah.exe" 0
  CreateShortcut "$SMPROGRAMS\Leksah\Uninstall Leksah.lnk" "$INSTDIR\Uninstall.exe"
  CreateShortcut "$DESKTOP\Leksah.lnk" "$INSTDIR\bin\leksah.exe" "" "$INSTDIR\bin\leksah.exe" 0
SectionEnd

; Detect the Microsoft Edge WebView2 Runtime; if absent, offer the download page.
; GUID {F3017226-FE2A-4295-8BDF-00C3A9A7E4C5} is the evergreen runtime.
Section "-WebView2Check"
  ReadRegStr $0 HKLM "SOFTWARE\WOW6432Node\Microsoft\EdgeUpdate\Clients\{F3017226-FE2A-4295-8BDF-00C3A9A7E4C5}" "pv"
  ${If} $0 == ""
    ReadRegStr $0 HKCU "SOFTWARE\Microsoft\EdgeUpdate\Clients\{F3017226-FE2A-4295-8BDF-00C3A9A7E4C5}" "pv"
  ${EndIf}
  ${If} $0 == ""
    MessageBox MB_YESNO|MB_ICONQUESTION \
      "Leksah needs the Microsoft Edge WebView2 Runtime, which was not detected on this PC.$\n$\nOpen the Microsoft download page now? (Download the 'Evergreen Bootstrapper', run it, then start Leksah.)" \
      IDNO skipwv
    ExecShell "open" "https://developer.microsoft.com/microsoft-edge/webview2/"
  skipwv:
  ${EndIf}
SectionEnd

Section "Uninstall"
  Delete "$INSTDIR\Uninstall.exe"
  RMDir /r "$INSTDIR\bin"
  RMDir /r "$INSTDIR\leksah"
  RMDir "$INSTDIR"

  Delete "$SMPROGRAMS\Leksah\Leksah.lnk"
  Delete "$SMPROGRAMS\Leksah\Uninstall Leksah.lnk"
  RMDir "$SMPROGRAMS\Leksah"
  Delete "$DESKTOP\Leksah.lnk"

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Leksah"
  DeleteRegKey HKLM "Software\Leksah"
SectionEnd
