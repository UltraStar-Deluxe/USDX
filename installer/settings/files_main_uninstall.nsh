; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Uninstaller: Main components
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

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
 Delete "$INSTDIR\${exe}.exe"
 Delete "$INSTDIR\${exeupdate}.exe"
 Delete "$INSTDIR\ScoreConverter.exe"
 Delete "$INSTDIR\ChangeLog.GERMAN.txt"
 Delete "$INSTDIR\ChangeLog.txt"
 Delete "$INSTDIR\LuaCommands.odt"
 Delete "$INSTDIR\documentation.pdf"
 Delete "$INSTDIR\license.txt"
 Delete "$INSTDIR\README.md"
 Delete "$INSTDIR\screenshots.lnk"
 Delete "$INSTDIR\playlists.lnk"
 Delete "$INSTDIR\config.ini.lnk"
 
 ; delete third party licenses
 Delete "$INSTDIR\LICENSE*"

 Delete "$INSTDIR\Error.log"
 Delete "$INSTDIR\Benchmark.log"
 Delete "$INSTDIR\cover.db"
 Delete "$INSTDIR\avatar.db"

 Delete "$INSTDIR\avcodec-56.dll"
 Delete "$INSTDIR\avdevice-56.dll"
 Delete "$INSTDIR\avfilter-5.dll"
 Delete "$INSTDIR\avformat-56.dll"
 Delete "$INSTDIR\avutil-54.dll"
 Delete "$INSTDIR\bass.dll"
 Delete "$INSTDIR\bassmidi.dll"
 Delete "$INSTDIR\bass_fx.dll"
 Delete "$INSTDIR\cv210.dll"
 Delete "$INSTDIR\cxcore210.dll"
 Delete "$INSTDIR\freetype6.dll"
 Delete "$INSTDIR\glew32.dll"
 Delete "$INSTDIR\highgui210.dll"
 Delete "$INSTDIR\libcurl-3.dll"
 Delete "$INSTDIR\libeay32.dll"
 Delete "$INSTDIR\libfreetype6.dll"
 Delete "$INSTDIR\libidn-11.dll"
 Delete "$INSTDIR\libjpeg-9.dll"
 Delete "$INSTDIR\libpng16-16.dll"
 Delete "$INSTDIR\libprojectM.dll"
 Delete "$INSTDIR\libprojectM2.dll"
 Delete "$INSTDIR\libssl32.dll"
 Delete "$INSTDIR\libtiff-5.dll"
 Delete "$INSTDIR\libwebp-4.dll"
 Delete "$INSTDIR\lua5.1.dll"
 Delete "$INSTDIR\opengl32_.dll"
 Delete "$INSTDIR\pcre3.dll"
 Delete "$INSTDIR\portaudio_x86.dll"
 Delete "$INSTDIR\portmixer.dll"
 Delete "$INSTDIR\postproc-53.dll"
 Delete "$INSTDIR\projectM-cwrapper.dll"
 Delete "$INSTDIR\SDL2.dll"
 Delete "$INSTDIR\SDL2_image.dll"
 Delete "$INSTDIR\sqlite3.dll"
 Delete "$INSTDIR\swresample-1.dll"
 Delete "$INSTDIR\swscale-3.dll"
 Delete "$INSTDIR\xinput1_3.dll"
 Delete "$INSTDIR\zlib1.dll"

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

 Delete "$INSTDIR\${exeuninstall}.exe"

 StrCpy $0 "$INSTDIR"
 Call un.DeleteIfEmpty
