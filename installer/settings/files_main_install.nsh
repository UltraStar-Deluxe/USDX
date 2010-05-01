; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: Main components
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

; Remove old files from previous versions
IfFileExists $INSTDIR\ScoreConverter.exe 0 +2
Delete "$INSTDIR\ScoreConverter.exe"
IfFileExists $INSTDIR\USdx.exe 0 +2
Delete "$INSTDIR\USdx.exe"
IfFileExists $INSTDIR\covers.cache 0 +2
Delete "$INSTDIR\covers.cache"
IfFileExists $INSTDIR\avcodec-51.dll 0 +2
Delete "$INSTDIR\avcodec-51.dll"
IfFileExists $INSTDIR\avformat-50.dll 0 +2
Delete "$INSTDIR\avformat-50.dll"
IfFileExists $INSTDIR\avutil-49.dll 0 +2
Delete "$INSTDIR\avutil-49.dll"
IfFileExists $INSTDIR\bass.dll 0 +2
Delete "$INSTDIR\bass.dll"
IfFileExists $INSTDIR\SDL.dll 0 +2
Delete "$INSTDIR\SDL.dll"
IfFileExists $INSTDIR\sqlite3.dll 0 +2
Delete "$INSTDIR\sqlite3.dll"

RMDir /r "$INSTDIR\Themes"
RMDir /r "$INSTDIR\Skins"
RMDir /r "$INSTDIR\Plugins"
RMDir /r "$INSTDIR\Languages"

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

  SetOutPath "$INSTDIR"

  CreateShortCut "screenshots.lnk" "$APPDATA\ultrastardx\screenshots"
  CreateShortCut "playlists.lnk" "$APPDATA\ultrastardx\playlists"
  CreateShortCut "config.ini.lnk" "$APPDATA\ultrastardx\config.ini"

  SetShellVarContext all
${EndIf}

; themes, languages, sounds, fonts, visuals dir

SetOutPath "$INSTDIR"

File /r /x .svn /x .gitignore ..\game\themes
File /r /x .svn /x .gitignore ..\game\languages
File /r /x .svn /x .gitignore ..\game\sounds
File /r /x .svn /x .gitignore ..\game\fonts
File /r /x .svn /x .gitignore ..\game\resources
File /r /x .svn /x .gitignore ..\game\visuals

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