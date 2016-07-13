; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Uninstaller: Main components
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

; Delete created Icons in startmenu

 SetShellVarContext all
 RMDir /r "$SMPROGRAMS\$ICONS_GROUP\"

; Delete created Icon on Desktop

 Delete "$Desktop\Play UltraStar Deluxe WorldParty.lnk"
 Delete "$Desktop\UltraStar Deluxe spielen.lnk"
 Delete "$Desktop\UltraStar Deluxe karaoke.lnk"
 Delete "$Desktop\Graj w UltraStar Deluxe WorldParty.lnk"

; Remove dirs

 RMDir /r "$INSTDIR\plugins"
 RMDir /r "$INSTDIR\themes"
 RMDir /r "$INSTDIR\fonts"
 RMDir /r "$INSTDIR\languages"
 RMDir /r "$INSTDIR\visuals"
 RMDir /r "$INSTDIR\resources"
 RMDir /r "$INSTDIR\sounds"
 RMDir /r "$INSTDIR\webs"
 RMDir /r "$INSTDIR\soundfonts"
 RMDir /r "$INSTDIR\avatars"

; Delete remaining files
 Delete "$INSTDIR\ScoreConverter.exe"
 Delete "$INSTDIR\${exe}.exe"
 Delete "$INSTDIR\ChangeLog.GERMAN.txt"
 Delete "$INSTDIR\ChangeLog.txt"
 Delete "$INSTDIR\LuaCommands.odt"
 Delete "$INSTDIR\documentation.pdf"
 Delete "$INSTDIR\license.txt"
 Delete "$INSTDIR\README.txt"
 Delete "$INSTDIR\screenshots.lnk"
 Delete "$INSTDIR\playlists.lnk"
 Delete "$INSTDIR\config.ini.lnk"

 Delete "$INSTDIR\Error.log"
 Delete "$INSTDIR\Benchmark.log"
 Delete "$INSTDIR\cover.db"
 Delete "$INSTDIR\avatar.db"

 Delete "$INSTDIR\avcodec-52.dll"
 Delete "$INSTDIR\avformat-52.dll"
 Delete "$INSTDIR\avfilter-1.dll"
 Delete "$INSTDIR\avdevice-52.dll"
 Delete "$INSTDIR\avutil-50.dll"
 Delete "$INSTDIR\bass.dll"
 Delete "$INSTDIR\bass_fx.dll"
 Delete "$INSTDIR\bassmidi.dll"
 Delete "$INSTDIR\freetype6.dll"
 Delete "$INSTDIR\glew32.dll"
 Delete "$INSTDIR\jpeg.dll"
 Delete "$INSTDIR\libpng12-0.dll"
 Delete "$INSTDIR\libprojectM.dll"
 Delete "$INSTDIR\libtiff-3.dll"
 Delete "$INSTDIR\lua5.1.dll"
 Delete "$INSTDIR\lua51.dll"
 Delete "$INSTDIR\pcre3.dll"
 Delete "$INSTDIR\portaudio_x86.dll"
 Delete "$INSTDIR\portmixer.dll"
 Delete "$INSTDIR\projectM-cwrapper.dll"
 Delete "$INSTDIR\SDL.dll"
 Delete "$INSTDIR\SDL_image.dll"
 Delete "$INSTDIR\SDL_ttf.dll"
 Delete "$INSTDIR\sqlite3.dll"
 Delete "$INSTDIR\swscale-0.dll"
 Delete "$INSTDIR\zlib1.dll"
 Delete "$INSTDIR\libcurl-3.dll"
 Delete "$INSTDIR\libeay32.dll"
 Delete "$INSTDIR\libidn-11.dll"
 Delete "$INSTDIR\libssl32.dll"
 Delete "$INSTDIR\cv210.dll"
 Delete "$INSTDIR\cxcore210.dll"
 Delete "$INSTDIR\highgui210.dll"

 StrCpy $0 "$INSTDIR\songs"
 Call un.DeleteIfEmpty 

 StrCpy $0 "$INSTDIR\covers"
 Call un.DeleteIfEmpty

 StrCpy $0 "$INSTDIR\screenshots"
 Call un.DeleteIfEmpty

 StrCpy $0 "$INSTDIR\playlists"
 Call un.DeleteIfEmpty

 ; Clean up AppData

 SetShellVarContext current

 Delete "$APPDATA\ultrastardx\Error.log"
 Delete "$APPDATA\ultrastardx\Benchmark.log"
 Delete "$APPDATA\ultrastardx\cover.db"
 Delete "$APPDATA\ultrastardx\avatar.db"
 
 StrCpy $0 "$APPDATA\ultrastardx\covers"
 Call un.DeleteIfEmpty

 StrCpy $0 "$APPDATA\ultrastardx\songs"
 Call un.DeleteIfEmpty

 StrCpy $0 "$APPDATA\ultrastardx\screenshots"
 Call un.DeleteIfEmpty

 StrCpy $0 "$APPDATA\ultrastardx\playlists"
 Call un.DeleteIfEmpty

 StrCpy $0 "$APPDATA\ultrastardx"
 Call un.DeleteIfEmpty

 SetShellVarContext all

; Self delete:

 Delete "$INSTDIR\Uninstall.exe"

 StrCpy $0 "$INSTDIR"
 Call un.DeleteIfEmpty