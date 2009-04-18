; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Uninstaller: Main components
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~


; Delete created Icons in startmenu

 SetShellVarContext all

 RMDir /r "$SMPROGRAMS\$ICONS_GROUP\"

; Delete created Icon on Desktop

 Delete "$Desktop\Play UltraStar Deluxe.lnk"
 Delete "$Desktop\UltraStar Deluxe spielen.lnk"

; Remove dirs

 RMDir /r "$INSTDIR\plugins"
 RMDir /r "$INSTDIR\themes"
 RMDir /r "$INSTDIR\fonts"
 RMDir /r "$INSTDIR\languages"
 RMDir /r "$INSTDIR\visuals"
 RMDir /r "$INSTDIR\sounds"

; Delete remaining files

 Delete "$INSTDIR\ScoreConverter.exe"
 Delete "$INSTDIR\${exe}.exe"
 Delete "$INSTDIR\Changelog.german.txt"
 Delete "$INSTDIR\Changelog.txt"
 Delete "$INSTDIR\documentation.pdf"
 Delete "$INSTDIR\License.txt"
 Delete "$INSTDIR\config.ini"
 Delete "$INSTDIR\README.txt"
 Delete "$INSTDIR\Error.log"
 Delete "$INSTDIR\covers.cache"
 Delete "$INSTDIR\cover.db"

 Delete "$INSTDIR\avcodec-51.dll"
 Delete "$INSTDIR\avformat-50.dll"
 Delete "$INSTDIR\avutil-49.dll"
 Delete "$INSTDIR\bass.dll"
 Delete "$INSTDIR\glew32.dll"
 Delete "$INSTDIR\jpeg.dll"
 Delete "$INSTDIR\libfreetype-6.dll"
 Delete "$INSTDIR\libpng12-0.dll"
 Delete "$INSTDIR\libprojectM.dll"
 Delete "$INSTDIR\libtiff-3.dll"
 Delete "$INSTDIR\portaudio_x86.dll"
 Delete "$INSTDIR\portmixer.dll"
 Delete "$INSTDIR\projectM-cwrapper.dll"
 Delete "$INSTDIR\SDL.dll"
 Delete "$INSTDIR\SDL_image.dll"
 Delete "$INSTDIR\SDL_ttf.dll"
 Delete "$INSTDIR\sqlite3.dll"
 Delete "$INSTDIR\zlib1.dll"

${If} ${AtLeastWinVista}
 Delete "$WINDIR\gdf.dll"
${EndIf}


 RMDir "$INSTDIR\songs\Dead Smiling Pirates - I 18"
 RMDir "$INSTDIR\songs\Joshua Morin - On The Run"
 RMDir "$INSTDIR\songs\Pornophonique - Space Invaders"
 RMDir "$INSTDIR\songs\Steven Dunston - Northern Star"

 StrCpy $0 "$INSTDIR\songs"
 Call un.DeleteIfEmpty

 Delete "$INSTDIR\covers\NoCover.jpg"

 StrCpy $0 "$INSTDIR\screenshots"
 Call un.DeleteIfEmpty

 StrCpy $0 "$INSTDIR\playlists"
 Call un.DeleteIfEmpty

; Self delete:

 Delete "$INSTDIR\Uninstall.exe"

 StrCpy $0 "$INSTDIR"
 Call un.DeleteIfEmpty