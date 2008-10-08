; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: Main
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!include MUI2.nsh
!include WinVer.nsh
!include LogicLib.nsh
!include InstallOptions.nsh

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Variables
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

; Installer Paths:

!define path_settings ".\settings"
!define path_languages ".\languages"
!define path_images "..\installerdependencies\images"
!define path_plugins "..\installerdependencies\plugins"
!define path_gdf "$WINDIR\gdf.dll"

!addPluginDir "${path_plugins}\"

!include "${path_settings}\variables.nsh"
!include "${path_settings}\GameExplorer.nsh"
!include "${path_settings}\functions.nsh"

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Export Settings
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

SetCompress Auto
SetCompressor /SOLID lzma
SetCompressorDictSize 32
SetDatablockOptimize On

XPStyle on

Name "${name} v.${version}"
Brandingtext "${name} v.${version} Installation"
OutFile "ultrastardx-${version}-installer-full.exe"

InstallDir "$PROGRAMFILES\${name}"

; Windows Vista:

RequestExecutionLevel user

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Interface Settings
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

; Icons:

!define MUI_ICON "${path_images}\${img_install}"
!define MUI_UNICON "${path_images}\${img_uninstall}"

; Header and Side Images:

!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "${path_images}\${img_header}"
!define MUI_HEADERIMAGE_UNBITMAP "${path_images}\${img_header}"

!define MUI_WELCOMEFINISHPAGE_BITMAP "${path_images}\${img_side}"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "${path_images}\${img_side}"

; Abort Warnings:

!define MUI_ABORTWARNING
!define MUI_ABORTWARNING_TEXT "$(abort_install)"
!define MUI_ABORTWARNING_CANCEL_DEFAULT

!define MUI_UNABORTWARNING
!define MUI_UNABORTWARNING_TEXT "$(abort_uninstall)"
!define MUI_UNABORTWARNING_CANCEL_DEFAULT

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Pages Installation Routine Settings
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

; Welcome Page:

!define MUI_WELCOMEPAGE_TITLE_3LINES
!define MUI_WELCOMEPAGE_TITLE "$(page_welcome_title)"
!define MUI_WELCOMEPAGE_TEXT "$(page_welcome_txt)"

; License Page:

!define MUI_LICENSEPAGE_RADIOBUTTONS

; Components Page:

!define MUI_COMPONENTSPAGE_SMALLDESC
!define MUI_COMPONENTSPAGE_TEXT_DESCRIPTION_INFO $(page_components_info)

; Finish Pages:

!define MUI_FINISHPAGE_TITLE_3LINES

!define MUI_FINISHPAGE_TEXT_LARGE
!define MUI_FINISHPAGE_TEXT "$(page_finish_txt)"

!define MUI_FINISHPAGE_RUN "$INSTDIR\${exe}.exe"
!define MUI_FINISHPAGE_RUN_NOTCHECKED

!define MUI_FINISHPAGE_LINK "$(page_finish_linktxt)"
!define MUI_FINISHPAGE_LINK_LOCATION "${homepage}"

!define MUI_FINISHPAGE_SHOWREADME
!define MUI_FINISHPAGE_SHOWREADME_TEXT $(page_finish_desktop)
!define MUI_FINISHPAGE_SHOWREADME_FUNCTION CreateDesktopShortCuts

!define MUI_FINISHPAGE_NOAUTOCLOSE
!define MUI_UNFINISHPAGE_NOAUTOCLOSE

!define MUI_FINISHPAGE_NOREBOOTSUPPORT

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Pages Installation Routine
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "${license}"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY

; Start menu page

var ICONS_GROUP
!define MUI_STARTMENUPAGE_NODISABLE
!define MUI_STARTMENUPAGE_DEFAULTFOLDER "${name}"
!define MUI_STARTMENUPAGE_REGISTRY_ROOT "${PRODUCT_UNINST_ROOT_KEY}"
!define MUI_STARTMENUPAGE_REGISTRY_KEY "${PRODUCT_UNINST_KEY}"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "${PRODUCT_STARTMENU_REGVAL}"
!insertmacro MUI_PAGE_STARTMENU Application $ICONS_GROUP

!insertmacro MUI_PAGE_INSTFILES

; USDX Settings Page

Page custom Settings

Function Settings

!insertmacro MUI_HEADER_TEXT " " "$(page_settings_subtitle)"

   !insertmacro INSTALLOPTIONS_DISPLAY "Settings-$LANGUAGE"

; Get all the variables:

var /GLOBAL fullscreen
var /GLOBAL language2
var /GLOBAL resolution
var /GLOBAL tabs
var /GLOBAL animations

  !insertmacro INSTALLOPTIONS_READ $fullscreen "Settings-$LANGUAGE" "Field 6" "State"
  !insertmacro INSTALLOPTIONS_READ $language2 "Settings-$LANGUAGE" "Field 7" "State"
  !insertmacro INSTALLOPTIONS_READ $resolution "Settings-$LANGUAGE" "Field 8" "State"
  !insertmacro INSTALLOPTIONS_READ $tabs "Settings-$LANGUAGE" "Field 9" "State"
  !insertmacro INSTALLOPTIONS_READ $animations "Settings-$LANGUAGE" "Field 10" "State"

; Write all variables to config.ini

FileOpen $0 '$INSTDIR\config.ini' w
FileWrite $0 '[Game]$\r$\n'
FileClose $0

${If} $language2 != ""

${WriteToConfig} "Language=$language2$\r$\n" "$INSTDIR\config.ini"

${EndIf}

${If} $tabs != ""

${WriteToConfig} "Tabs=$tabs$\r$\n" "$INSTDIR\config.ini"

${EndIf}

${WriteToConfig} "[Graphics]$\r$\n" "$INSTDIR\config.ini"

${If} $fullscreen != ""

${WriteToConfig} "FullScreen=$fullscreen$\r$\n" "$INSTDIR\config.ini"

${EndIf}

${If} $resolution != ""

${WriteToConfig} "Resolution=$resolution$\r$\n" "$INSTDIR\config.ini"

${EndIf}

${WriteToConfig} "[Advanced]$\r$\n" "$INSTDIR\config.ini"

; Animations On / Off Tasks

${If} $animations == "Off"

${WriteToConfig} "LoadAnimation=Off$\r$\n" "$INSTDIR\config.ini"

${WriteToConfig} "EffectSing=Off$\r$\n" "$INSTDIR\config.ini"

${WriteToConfig} "ScreenFade=Off$\r$\n" "$INSTDIR\config.ini"

${EndIf}


FunctionEnd ; Settings page End


!insertmacro MUI_PAGE_FINISH

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Pages UnInstallation Routine
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!insertmacro MUI_UNPAGE_WELCOME
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_UNPAGE_FINISH

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Sections Installation Routine
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

;------------------------------------
; MAIN COMPONENTS (Section 1)
;------------------------------------

Section $(name_section1) Section1
	SectionIn RO
	SetOutPath $INSTDIR
	SetOverwrite try

!include "${path_settings}\files_main_install.nsh"


; Create Shortcuts:

SetOutPath "$INSTDIR"

!insertmacro MUI_STARTMENU_WRITE_BEGIN Application

  SetShellVarContext all
  SetOutPath "$INSTDIR"

  CreateDirectory "${name}"
  CreateDirectory "$SMPROGRAMS\$ICONS_GROUP"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_shortcut).lnk" "$INSTDIR\${exe}.exe"
; CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_documentation).lnk" "$INSTDIR\documentation.pdf"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_website).lnk" "http://www.ultrastardeluxe.org/"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_readme).lnk" "$INSTDIR\ReadMe.txt"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_license).lnk" "$INSTDIR\License.txt"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_uninstall).lnk" "$INSTDIR\Uninstall.exe"
  !insertmacro MUI_STARTMENU_WRITE_END

; Vista Game Explorer:

${If} ${AtLeastWinVista}

${GameExplorer_GenerateGUID}
Pop $0

${GameExplorer_AddGame} all "${path_gdf}" $WINDIR $INSTDIR\${exe}.exe $0

CreateDirectory $APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\1
CreateShortcut "$APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\1\Benchmark.lnk" \
  "$INSTDIR\${exe}.exe" "-Benchmark"

CreateDirectory $APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\2
CreateShortcut "$APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\2\Joypad.lnk" \
  "$INSTDIR\${exe}.exe" "-Joypad"

CreateDirectory $APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\3
CreateShortcut "$APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\3\Fullscreen.lnk" \
  "$INSTDIR\${exe}.exe" "-FullScreen"

CreateDirectory $APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\3
CreateShortcut "$APPDATA\Microsoft\Windows\GameExplorer\$0\PlayTasks\3\Dual Screen.lnk" \
  "$INSTDIR\${exe}.exe" "-Screens 2"

CreateDirectory $APPDATA\Microsoft\Windows\GameExplorer\$0\SupportTasks\0
CreateShortcut "$APPDATA\Microsoft\Windows\GameExplorer\$0\SupportTasks\0\Support Forum.lnk" \
  "http://forum.ultrastardeluxe.org"

${EndIf}

; Create Uninstaller:

  WriteUninstaller "$INSTDIR\Uninstall.exe"

  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "${name}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\Uninstall.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "Publisher" "${PRODUCT_PUBLISHER}"

  SetOutPath "$INSTDIR"

SectionEnd

;------------------------------------
; OPTIONAL SONGS (Section 2)
;------------------------------------

SectionGroup $(name_section2) Section2

;
; Dead Smiling Pirates - I 18
; 

Section /o "Dead Smiling Pirates - I 18" g2Section1
;  AddSize 1400
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\Songs\Dead Smiling Pirates - I 18"
   SetOutPath "$INSTDIR\Songs\Dead Smiling Pirates - I 18\"

; Download song:
  NSISdl::download /TIMEOUT=30000 ${download_song1} $TEMP\Song-I-18.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  nsisunz::Unzip "$TEMP\Song-I-18.zip" "$INSTDIR\Songs\Dead Smiling Pirates - I 18\"

  Delete "$TEMP\Song-I-18.zip"

  SetOutPath "$INSTDIR"

SectionEnd

;
; Steven Dunston - Northern Star
; 

Section /o "Joshua Morin - On The Run" g2Section2
;  AddSize 2200
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\Songs\Joshua Morin - On The Run"
   SetOutPath "$INSTDIR\Songs\Joshua Morin - On The Run\"

; Download song:
  NSISdl::download /TIMEOUT=30000 ${download_song3} $TEMP\Song-On-the-run.zip

  Pop $R0 ;Get the return value
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  nsisunz::Unzip "$TEMP\Song-On-the-run.zip" "$INSTDIR\Songs\Joshua Morin - On The Run\"

  Delete "$TEMP\Song-On-the-run.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Pornophonique - Space Invaders" g2Section3
;  AddSize 2200
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\Songs\Pornophonique - Space Invaders"
   SetOutPath "$INSTDIR\Songs\Pornophonique - Space Invaders\"

; Download song:
  NSISdl::download /TIMEOUT=30000 ${download_song3} $TEMP\Song-Space-Invaders.zip

  Pop $R0 ;Get the return value
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  nsisunz::Unzip "$TEMP\Song-Space-Invaders.zip" "$INSTDIR\Songs\Pornophonique - Space Invaders\"

  Delete "$TEMP\Song-Space-Invaders.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Steven Dunston - Northern Star" g2Section4
;  AddSize 1500
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\Songs\Steven Dunston - Northern Star"
   SetOutPath "$INSTDIR\Songs\Steven Dunston - Northern Star\"

; Download song:
  NSISdl::download /TIMEOUT=30000 ${download_song2} $TEMP\Song-Northern-Star.zip

  Pop $R0 ;Get the return value
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  nsisunz::Unzip "$TEMP\Song-Northern-Star.zip" "$INSTDIR\Songs\Steven Dunston - Northern Star\"

  Delete "$TEMP\Song-Northern-Star.zip"

  SetOutPath "$INSTDIR"

SectionEnd

SectionGroupEnd

;------------------------------------
; OPTIONAL THEMES (Section 3)
;------------------------------------

SectionGroup $(name_section3) Section3

 Section /o "Orange" g3Section1
;  AddSize 700

; Download theme orange:
  NSISdl::download /TIMEOUT=30000 ${download_theme1} $TEMP\Theme-Orange.zip

  Pop $R0 ;Get the return value
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  nsisunz::Unzip "$TEMP\Theme-Orange.zip" "$INSTDIR\"

  Delete "$TEMP\Theme-Orange.zip"

  SetOutPath "$INSTDIR"

SectionEnd

 Section /o "Streetlight" g3Section2
;  AddSize 1000

; Download theme Streetlight:
  NSISdl::download /TIMEOUT=30000 ${download_theme2} $TEMP\Theme-Streetlight.zip

  Pop $R0 ;Get the return value
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  nsisunz::Unzip "$TEMP\Theme-Streetlight.zip" "$INSTDIR\"

  Delete "$TEMP\Theme-Streetlight.zip"

  SetOutPath "$INSTDIR"

SectionEnd

 Section /o "Vistar" g3Section3
;   AddSize 1000

; Download theme Vistar:

  NSISdl::download /TIMEOUT=30000 ${download_theme3} $TEMP\Theme-Vistar.zip

  Pop $R0 ;Get the return value
   StrCmp $R0 "success" dlok
     MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  nsisunz::Unzip "$TEMP\Theme-Vistar.zip" "$INSTDIR\"

  Delete "$TEMP\Theme-Vistar.zip"

  SetOutPath "$INSTDIR"

SectionEnd

 Section /o "BlueSensation" g3Section4
;   AddSize 1000

; Download theme BlueSensation:

  NSISdl::download /TIMEOUT=30000 ${download_theme4} $TEMP\Theme-BlueSensation.zip

  Pop $R0 ;Get the return value
   StrCmp $R0 "success" dlok
     MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  nsisunz::Unzip "$TEMP\Theme-BlueSensation.zip" "$INSTDIR\"

  Delete "$TEMP\Theme-BlueSensation.zip"

  SetOutPath "$INSTDIR"

SectionEnd

 Section /o "WiiStar" g3Section5
;   AddSize 1000

; Download theme WiiStar:

  NSISdl::download /TIMEOUT=30000 ${download_theme5} $TEMP\Theme-WiiStar.zip

  Pop $R0 ;Get the return value
   StrCmp $R0 "success" dlok
     MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  nsisunz::Unzip "$TEMP\Theme-WiiStar.zip" "$INSTDIR\"

  Delete "$TEMP\Theme-WiiStar.zip"

  SetOutPath "$INSTDIR"

SectionEnd

 Section /o "iStar" g3Section6
;   AddSize 1000

; Download theme iStar:

  NSISdl::download /TIMEOUT=30000 ${download_theme6} $TEMP\Theme-iStar.zip

  Pop $R0 ;Get the return value
   StrCmp $R0 "success" dlok
     MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  nsisunz::Unzip "$TEMP\Theme-iStar.zip" "$INSTDIR\"

  Delete "$TEMP\Theme-iStar.zip"

  SetOutPath "$INSTDIR"

SectionEnd

SectionGroupEnd

;------------------------------------
; UNINSTALL (Section 4)
;------------------------------------

Section Uninstall

 !insertmacro MUI_STARTMENU_GETFOLDER "Application" $ICONS_GROUP

 !include "${path_settings}\files_main_uninstall.nsh"

 DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"

; Unregister from Windows Vista Game Explorer

${If} ${AtLeastWinVista}

${GameExplorer_RemoveGame} $0

${EndIf}



SectionEnd

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Section Descriptions
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~


!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN

  !insertmacro MUI_DESCRIPTION_TEXT ${Section1} $(DESC_Section1)
  !insertmacro MUI_DESCRIPTION_TEXT ${Section2} $(DESC_Section2)
  !insertmacro MUI_DESCRIPTION_TEXT ${Section3} $(DESC_Section3)

  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section1} $(DESC_g2Section1)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section2} $(DESC_g2Section2)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section3} $(DESC_g2Section3)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section4} $(DESC_g2Section4)

  !insertmacro MUI_DESCRIPTION_TEXT ${g3Section1} $(DESC_g3Section1)
  !insertmacro MUI_DESCRIPTION_TEXT ${g3Section2} $(DESC_g3Section2)
  !insertmacro MUI_DESCRIPTION_TEXT ${g3Section3} $(DESC_g3Section3)
  !insertmacro MUI_DESCRIPTION_TEXT ${g3Section4} $(DESC_g3Section4)
  !insertmacro MUI_DESCRIPTION_TEXT ${g3Section5} $(DESC_g3Section5)
  !insertmacro MUI_DESCRIPTION_TEXT ${g3Section6} $(DESC_g3Section6)

!insertmacro MUI_FUNCTION_DESCRIPTION_END

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Language Support
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "German"

!insertmacro MUI_RESERVEFILE_LANGDLL

!include "${path_languages}\*.nsh"

Function .onInit

var /GLOBAL version
StrCpy $version "1.1a"


   System::Call 'kernel32::CreateMutexA(i 0, i 0, t "USdx Installer.exe") ?e'

  Pop $R0

  StrCmp $R0 0 +3
    MessageBox MB_OK|MB_ICONEXCLAMATION $(oninit_running)
    Abort

  ReadRegStr $R0  HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${name}" 'DisplayVersion'

  ${If} $R0 == $version
	  MessageBox MB_YESNO|MB_ICONEXCLAMATION \
          "${name} v.$R0 $(oninit_alreadyinstalled). $\n$\n $(oninit_installagain)" \
          IDYES done
          Abort
  ${EndIf}

  ReadRegStr $R1 HKLM \
  "Software\Microsoft\Windows\CurrentVersion\Uninstall\${name}" \
  "UninstallString"
  StrCmp $R1 "" done

  ${If} $R0 != $version
	  MessageBox MB_YESNO|MB_ICONEXCLAMATION \
          "${name} v.$R0 $(oninit_alreadyinstalled). $\n$\n $(oninit_updateusdx) v.$R0 -> v.${version}" \
          IDYES done
          Abort
  ${EndIf}

done:

  !insertmacro MUI_LANGDLL_DISPLAY

  !insertmacro INSTALLOPTIONS_EXTRACT_AS ".\settings\settings-1031.ini" "Settings-1031"
  !insertmacro INSTALLOPTIONS_EXTRACT_AS ".\settings\settings-1033.ini" "Settings-1033"

FunctionEnd

Function un.onInit

        ${nsProcess::FindProcess} "USdx.exe" $R0
        StrCmp $R0 0 0 +2
        MessageBox MB_YESNO|MB_ICONEXCLAMATION '$(oninit_closeusdx)' IDYES closeit IDNO end

        closeit:
        ${nsProcess::KillProcess} "USdx.exe" $R0
        goto continue

        end:
        ${nsProcess::Unload}
        Abort

  continue:
  !insertmacro MUI_LANGDLL_DISPLAY

FunctionEnd
