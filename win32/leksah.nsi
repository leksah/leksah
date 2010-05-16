Name "Leksah"

OutFile "$%LEKSAH_X_X_X_X%.exe"

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

  File /r "$%GTK_PREFIX%\etc"

  SetOutPath $INSTDIR\leksah
  File /r "$%GTK_PREFIX%\share\$%LEKSAH_X_X%\*"

  SetOutPath $INSTDIR\etc\gtk-2.0
  File "gtkrc"

  SetOutPath $INSTDIR\bin
  File "$%GTK_PREFIX%\bin\leksah.exe"
  File "$%GTK_PREFIX%\bin\leksah-server.exe"
  File "$%GTK_PREFIX%\bin\leksahecho.exe"
  File "$%GTK_PREFIX%\bin\*.dll"
  File /oname=libxml2-2.dll "$%GTK_PREFIX%\bin\libxml2.dll"
  File "$%CURL_PREFIX%\bin\libcurl*.dll"
  File "$%CURL_PREFIX%\bin\msys-z.dll"
  File "$%CURL_PREFIX%\bin\msys-1.0.dll"
  
  SetOutPath $INSTDIR\share
  File /r "$%GTK_PREFIX%\share\gtk-engines"
  File /r "$%GTK_PREFIX%\share\themes"
  ; File /r "$%GTK_PREFIX%\share\icons"
  File /r "$%GTK_PREFIX%\share\gtksourceview-2.0"

  SetOutPath $INSTDIR\lib\gtk-2.0\2.10.0\engines
  File "$%GTK_PREFIX%\lib\gtk-2.0\2.10.0\engines\*.dll"

  ;SetOutPath $INSTDIR\lib\gtk-2.0\2.10.0\loaders
  ;File "$%GTK_PREFIX%\lib\gtk-2.0\2.10.0\loaders\*.dll"

  SetOutPath $INSTDIR\lib\gtk-2.0\modules
  File "$%GTK_PREFIX%\lib\gtk-2.0\modules\*.dll"

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
  RMDir /r $INSTDIR\leksah
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
