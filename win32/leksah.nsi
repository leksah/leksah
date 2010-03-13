Name "Leksah"

OutFile "Leksah-0.8.0.0.exe"

InstallDir $PROGRAMFILES\Leksah

InstallDirRegKey HKLM "Software\Leksah" "Install_Dir"

; Request application privileges for Windows Vista
RequestExecutionLevel admin

;--------------------------------

; Pages

Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

;--------------------------------

; The stuff to install
Section "Leksah"

  SectionIn RO
  
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  
  ; Put file there
  File "leksah.bat"
  File "leksah-server.bat"
  File "leksah-rebuild-metadata.bat"
  File "leksah.ico"

  File /r "C:\SDKs\GTK\etc"

  SetOutPath $INSTDIR\leksah-server-0.8
  File /r "C:\SDKs\GTK\leksah-server-0.8\data"

  SetOutPath $INSTDIR\leksah-0.8
  File /r "C:\SDKs\GTK\leksah-0.8\*"

  SetOutPath $INSTDIR\etc\gtk-2.0
  File "gtkrc"

  SetOutPath $INSTDIR\bin
  File "C:\SDKs\GTK\bin\leksah.exe"
  File "C:\SDKs\GTK\bin\leksah-server.exe"
  File "C:\SDKs\GTK\bin\leksahecho.exe"
  File "C:\SDKs\GTK\bin\*.dll"
  
  SetOutPath $INSTDIR\share
  File /r "C:\SDKs\GTK\share\gtk-engines"
  File /r "C:\SDKs\GTK\share\themes"
  ; File /r "C:\SDKs\GTK\share\icons"
  File /r "C:\SDKs\GTK\share\gtksourceview-2.0"

  SetOutPath $INSTDIR\lib\gtk-2.0\2.10.0\engines
  File "C:\SDKs\GTK\lib\gtk-2.0\2.10.0\engines\*.dll"

  SetOutPath $INSTDIR\lib\gtk-2.0\2.10.0\loaders
  File "C:\SDKs\GTK\lib\gtk-2.0\2.10.0\loaders\*.dll"

  SetOutPath $INSTDIR\lib\gtk-2.0\modules
  File "C:\SDKs\GTK\lib\gtk-2.0\modules\*.dll"

  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\Leksah "Install_Dir" "$INSTDIR"
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Leksah" "DisplayName" "Leksah"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Leksah" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Leksah" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Leksah" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
  
SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts"

  CreateDirectory "$SMPROGRAMS\Leksah"
  CreateShortCut "$SMPROGRAMS\Leksah\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut "$SMPROGRAMS\Leksah\Server.lnk" "$INSTDIR\leksah-server.bat" "" "$INSTDIR\leksah.ico" 0
  CreateShortCut "$SMPROGRAMS\Leksah\Rebuild Metadata.lnk" "$INSTDIR\leksah-rebuild-metadata.bat" "" "$INSTDIR\leksah.ico" 0
  CreateShortCut "$SMPROGRAMS\Leksah\Leksah.lnk" "$INSTDIR\leksah.bat" "" "$INSTDIR\leksah.ico" 0
  
SectionEnd

;--------------------------------

; Uninstaller

Section "Uninstall"
  
  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Leksah"
  DeleteRegKey HKLM SOFTWARE\Leksah

  ; Remove files and uninstaller
  Delete $INSTDIR\leksah*.bat
  Delete $INSTDIR\leksah.ico
  RMDir /r $INSTDIR\leksah-0.8
  RMDir /r $INSTDIR\leksah-server-0.8
  RMDir /r $INSTDIR\bin
  RMDir /r $INSTDIR\lib
  RMDir /r $INSTDIR\etc
  RMDir /r $INSTDIR\share
  Delete $INSTDIR\uninstall.exe

  ; Remove shortcuts, if any
  Delete "$SMPROGRAMS\Leksah\*.*"

  ; Remove directories used
  RMDir "$SMPROGRAMS\Leksah"
  RMDir "$INSTDIR"

SectionEnd
