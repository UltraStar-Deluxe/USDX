; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: Main components
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

; Create Directories:

CreateDirectory $INSTDIR\plugins
CreateDirectory $INSTDIR\covers
CreateDirectory $INSTDIR\songs

${If} ${AtLeastWinVista}

  ; Create folders in appdata for current user
  SetShellVarContext current		
  CreateDirectory $APPDATA\ultrastardx
  CreateDirectory $APPDATA\ultrastardx\screenshots
  CreateDirectory $APPDATA\ultrastardx\playlists

  SetOutPath "$APPDATA\ultrastardx"
  File ..\game\config.ini

  SetOutPath "$INSTDIR"

  CreateShortCut "screenshots.lnk" "$APPDATA\ultrastardx\screenshots"
  CreateShortCut "playlists.lnk" "$APPDATA\ultrastardx\playlists"
  CreateShortCut "config.ini.lnk" "$APPDATA\ultrastardx\config.ini"

  SetShellVarContext all
${EndIf}

; themes, languages, sounds, fonts, visuals dir

SetOutPath "$INSTDIR"

File /r ..\game\themes
File /r ..\game\languages
File /r ..\game\sounds
File /r ..\game\fonts
File /r ..\game\resources
File /r ..\game\visuals

; Root dir:

File .\dependencies\dll\*.dll

File ..\ChangeLog.txt
File ..\ChangeLog.GERMAN.txt
File ..\game\LuaCommands.odt
File ..\README.txt
File .\dependencies\documents\license.txt
File .\dependencies\documents\documentation.pdf

File "..\${exe}.exe"

; Covers dir:

SetOutPath "$INSTDIR\covers"

IfFileExists $INSTDIR\covers\covers.ini +2 0
File ..\game\covers\covers.ini
File ..\game\covers\NoCover.jpg

; Plugins dir:

SetOutPath "$INSTDIR\plugins\"
File "..\game\plugins\*.*"

SetOutPath "$INSTDIR"
