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
IfFileExists $INSTDIR\bassmidi.dll 0 +2
Delete "$INSTDIR\bassmidi.dll"
IfFileExists $INSTDIR\SDL.dll 0 +2
Delete "$INSTDIR\SDL.dll"
IfFileExists $INSTDIR\sqlite3.dll 0 +2
Delete "$INSTDIR\sqlite3.dll"
IfFileExists $INSTDIR\webs\ultrastares.dll 0 +2
Delete "$INSTDIR\webs\ultrastares.dll"
IfFileExists $INSTDIR\cv210.dll 0 +2
Delete "$INSTDIR\cv210.dll"
IfFileExists $INSTDIR\cxcore210.dll 0 +2
Delete "$INSTDIR\cxcore210.dll"
IfFileExists $INSTDIR\highgui210.dll 0 +2
Delete "$INSTDIR\highgui210.dll"
IfFileExists $INSTDIR\avatar.db 0 +2
Delete "$INSTDIR\avatar.db"

RMDir /r "$INSTDIR\Themes"
RMDir /r "$INSTDIR\Skins"
RMDir /r "$INSTDIR\Plugins"
RMDir /r "$INSTDIR\Languages"
RMDir /r "$INSTDIR\Webs"
RMDir /r "$INSTDIR\Avatars"

; Create Directories:

CreateDirectory $INSTDIR\plugins
CreateDirectory $INSTDIR\covers
CreateDirectory $INSTDIR\songs
CreateDirectory $INSTDIR\avatars

${If} $UseAppData == true

  ; Create folders in appdata for current user
  SetShellVarContext current		
  CreateDirectory $UserDataPath
  CreateDirectory $UserDataPath\screenshots
  CreateDirectory $UserDataPath\playlists

  SetOutPath "$INSTDIR"

  CreateShortCut "screenshots.lnk" "$UserDataPath\screenshots"
  CreateShortCut "playlists.lnk" "$UserDataPath\playlists"
  CreateShortCut "config.ini.lnk" "$ConfigIniPath"

  SetShellVarContext all
${EndIf}

; themes, languages, sounds, fonts, visuals dir

SetOutPath "$INSTDIR"

File /r /x .svn /x .gitignore ..\game\covers
File /r /x .svn /x .gitignore ..\game\themes
File /r /x .svn /x .gitignore ..\game\languages
File /r /x .svn /x .gitignore ..\game\sounds
File /r /x .svn /x .gitignore ..\game\fonts
File /r /x .svn /x .gitignore ..\game\resources
File /r /x .svn /x .gitignore ..\game\visuals
File /r /x .svn /x .gitignore ..\game\webs
File /r /x .svn /x .gitignore ..\game\soundfonts
File /r /x .svn /x .gitignore ..\game\avatars

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

;SetOutPath "$INSTDIR\covers"

;IfFileExists $INSTDIR\covers\covers.ini +2 0
;File ..\game\covers\covers.ini
;File ..\game\covers\NoCover.jpg
;File "..\game\covers\*.*"

; Plugins dir:

SetOutPath "$INSTDIR\plugins\"
File "..\game\plugins\*.*"

SetOutPath "$INSTDIR"