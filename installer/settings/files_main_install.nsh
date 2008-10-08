; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: Main components
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

; Create Directories:

CreateDirectory $INSTDIR\plugins
CreateDirectory $INSTDIR\songs
CreateDirectory $INSTDIR\screenshots
CreateDirectory $INSTDIR\playlists

SetOutPath "$INSTDIR"

; themes, languages, sounds, visuals dir

File /r ..\game\themes
File /r ..\game\languages
File /r ..\game\sounds
File /r ..\installerdependencies\visuals

; Root dir:

File ..\installerdependencies\dll\*.dll


File ..\ChangeLog.txt
File ..\ChangeLog.german.txt
File ..\README.txt
File ..\installerdependencies\documents\documentation.pdf
File ..\installerdependencies\documents\license.txt

File "..\ScoreConverter.exe"
File "..\${exe}.exe"

; Covers dir:

SetOutPath "$INSTDIR\covers"

IfFileExists $INSTDIR\covers\covers.ini +2 0
File ..\game\covers\Covers.ini
File ..\game\covers\NoCover.jpg

; Plugins dir:

SetOutPath "$INSTDIR\Plugins\"
  File "..\Plugins\Blind.dll"
  File "..\Plugins\Duell.dll"
  File "..\Plugins\Hold_The_Line.dll"
  File "..\Plugins\TeamDuell.dll"
  File "..\Plugins\Until5000.dll"

${If} ${AtLeastWinVista}

  SetOutPath "$WINDIR"
  File "..\installerdependencies\plugins\gdf.dll"

${EndIf}

SetOutPath "$INSTDIR"
