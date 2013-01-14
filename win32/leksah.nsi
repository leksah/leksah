Name "Leksah"

OutFile "$%LEKSAH_X_X_X_X_GHC_X_X_X%.exe"

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
  File /r "$%LEKSAH_PREFIX%\$%LEKSAH_X_X_X_X%\data"
  File /r "$%LEKSAH_PREFIX%\$%LEKSAH_X_X_X_X%\language-specs"
  File /r "$%LEKSAH_PREFIX%\$%LEKSAH_X_X_X_X%\pics"
  File "$%LEKSAH_PREFIX%\$%LEKSAH_X_X_X_X%\LICENSE"
  File "$%LEKSAH_PREFIX%\$%LEKSAH_X_X_X_X%\Readme.md"

  SetOutPath $INSTDIR\hlint
  File "$%LEKSAH_PREFIX%\$%HLINT_X_X_X_X%\*"

  SetOutPath $INSTDIR\etc\gtk-2.0
  File "gtkrc"

  SetOutPath $INSTDIR\bin
  File "$%LEKSAH_PREFIX%\bin\leksah.exe"
  File "$%LEKSAH_SERVER_PREFIX%\bin\leksah-server.exe"
  File "$%LEKSAH_SERVER_PREFIX%\bin\leksahecho.exe"
  File "$%GTK_PREFIX%\bin\iconv.dll"
  File "$%GTK_PREFIX%\bin\libatk-1.0-0.dll"
  File "$%GTK_PREFIX%\bin\libcairo-2.dll"
  File "$%GTK_PREFIX%\bin\libcairo-gobject-2.dll"
  File "$%GTK_PREFIX%\bin\libcrypto-10.dll"
  File "$%GTK_PREFIX%\bin\libcurl-4.dll"
  File "$%GTK_PREFIX%\bin\libexpat-1.dll"
  File "$%GTK_PREFIX%\bin\libffi-6.dll"
  File "$%GTK_PREFIX%\bin\libfontconfig-1.dll"
  File "$%GTK_PREFIX%\bin\libfreetype-6.dll"
  File "$%GTK_PREFIX%\bin\libgailutil-3-0.dll"
  File "$%GTK_PREFIX%\bin\libgdk-3-0.dll"
  File "$%GTK_PREFIX%\bin\libgdk_pixbuf-2.0-0.dll"
  File "$%GTK_PREFIX%\bin\libgio-2.0-0.dll"
  File "$%GTK_PREFIX%\bin\libglib-2.0-0.dll"
  File "$%GTK_PREFIX%\bin\libgmodule-2.0-0.dll"
  File "$%GTK_PREFIX%\bin\libgobject-2.0-0.dll"
  File "$%GTK_PREFIX%\bin\libgthread-2.0-0.dll"
  File "$%GTK_PREFIX%\bin\libgtk-3-0.dll"
  File "$%GTK_PREFIX%\bin\libgtksourceview-3.0-0.dll"
  File "$%GTK_PREFIX%\bin\libidn-11.dll"
  File "$%GTK_PREFIX%\bin\libintl-8.dll"
  File "$%GTK_PREFIX%\bin\libpango-1.0-0.dll"
  File "$%GTK_PREFIX%\bin\libpangocairo-1.0-0.dll"
  File "$%GTK_PREFIX%\bin\libpangoft2-1.0-0.dll"
  File "$%GTK_PREFIX%\bin\libpangowin32-1.0-0.dll"
  File "$%GTK_PREFIX%\bin\libpixman-1-0.dll"
  File "$%GTK_PREFIX%\bin\libpng15-15.dll"
  File "$%GTK_PREFIX%\bin\libssh2-1.dll"
  File "$%GTK_PREFIX%\bin\libssl-10.dll"
  File "$%GTK_PREFIX%\bin\libxml2-2.dll"
  File "$%GTK_PREFIX%\bin\zlib1.dll"
  
  SetOutPath $INSTDIR\share
  ;File /r "$%GTK_PREFIX%\share\gtk-engines"
  ;File /r "$%GTK_PREFIX%\share\themes"
  ; File /r "$%GTK_PREFIX%\share\icons"
  File /r "$%GTK_PREFIX%\share\glib-2.0"
  File /r "$%GTK_PREFIX%\share\gtksourceview-3.0"

 ; SetOutPath $INSTDIR\lib\gtk-2.0\2.10.0\engines
 ; File "$%GTK_PREFIX%\lib\gtk-2.0\2.10.0\engines\*.dll"

  ;SetOutPath $INSTDIR\lib\gtk-2.0\modules
  ;File "$%GTK_PREFIX%\lib\gtk-2.0\modules\*.dll"

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
  CreateShortCut "$SMPROGRAMS\Leksah\Leksah.lnk" "$INSTDIR\leksah.bat" "" "$INSTDIR\leksah.ico" 0 SW_SHOWMINIMIZED
  
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
