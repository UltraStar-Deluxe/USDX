; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: Main
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!include MUI2.nsh
!include WinVer.nsh
!include LogicLib.nsh
!include InstallOptions.nsh
!include nsDialogs.nsh

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Variables
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

; Installer Paths:

!define path_settings ".\settings"
!define path_languages ".\languages"
!define path_dependencies ".\dependencies"
!define path_images ".\dependencies\images"
!define path_plugins ".\dependencies\plugins"

; MultiLanguage - Show all languages:
!define MUI_LANGDLL_ALLLANGUAGES

!addPluginDir "${path_plugins}\"

!include "${path_settings}\variables.nsh"
!include "${path_settings}\functions.nsh"

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Export Settings
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

SetCompress Auto
SetCompressor lzma
SetCompressorDictSize 32
SetDatablockOptimize On

CRCCheck on

XPStyle on

Name "${name} v.${version}"
Brandingtext "${name} v.${version} Installation"
OutFile "ultrastardx-${version}-installer-full.exe"

InstallDir "$PROGRAMFILES\${name}"
InstallDirRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\UltraStar Deluxe" "InstallDir"

; Windows Vista / Windows 7:

RequestExecutionLevel admin

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Interface Settings
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

; Icons:

!define MUI_ICON "${img_install}"
!define MUI_UNICON "${img_uninstall}"

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

!define MUI_CUSTOMFUNCTION_GUIINIT bgmusic

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
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "${name}"
!insertmacro MUI_PAGE_STARTMENU Application $ICONS_GROUP

!insertmacro MUI_PAGE_INSTFILES

; USDX Settings Page

Page custom Settings

Function Settings

!insertmacro INSTALLOPTIONS_WRITE "Settings-$LANGUAGE" "Field 18" "State" "$INSTDIR\songs"

!insertmacro MUI_HEADER_TEXT " " "$(page_settings_subtitle)"   
!insertmacro INSTALLOPTIONS_DISPLAY "Settings-$LANGUAGE"

; Get all the variables:

Var /GLOBAL LABEL_COMPONENTS

Var /GLOBAL CHECKBOX_COVERS
Var /GLOBAL CB_COVERS_State
Var /GLOBAL CHECKBOX_SCORES
Var /GLOBAL CB_SCORES_State
Var /GLOBAL CHECKBOX_CONFIG
Var /GLOBAL CB_CONFIG_State
Var /GLOBAL CHECKBOX_SCREENSHOTS
Var /GLOBAL CB_SCREENSHOTS_State
Var /GLOBAL CHECKBOX_PLAYLISTS
Var /GLOBAL CB_PLAYLISTS_State
Var /GLOBAL CHECKBOX_SONGS 
Var /GLOBAL CB_SONGS_State

var /GLOBAL fullscreen
var /GLOBAL language2
var /GLOBAL resolution
var /GLOBAL tabs
var /GLOBAL sorting
var /GLOBAL songdir

  !insertmacro INSTALLOPTIONS_READ $fullscreen "Settings-$LANGUAGE" "Field 5" "State"
  !insertmacro INSTALLOPTIONS_READ $language2 "Settings-$LANGUAGE" "Field 6" "State"
  !insertmacro INSTALLOPTIONS_READ $resolution "Settings-$LANGUAGE" "Field 7" "State"
  !insertmacro INSTALLOPTIONS_READ $tabs "Settings-$LANGUAGE" "Field 8" "State"
  !insertmacro INSTALLOPTIONS_READ $sorting "Settings-$LANGUAGE" "Field 15" "State"
  !insertmacro INSTALLOPTIONS_READ $songdir "Settings-$LANGUAGE" "Field 18" "State"

; Write all variables to config.ini

var /GLOBAL path_config
var /GLOBAL path_configini

${If} ${AtLeastWinVista}  
  SetShellVarContext current
  StrCpy $path_config "$APPDATA\ultrastardx"
  SetShellVarContext all
${Else}
  StrCpy $path_config "$INSTDIR"
${EndIf}

StrCpy $path_configini "$path_config\config.ini"

WriteINIStr "$path_configini" "Game" "Language" "$language2"
WriteINIStr "$path_configini" "Game" "Tabs" "$tabs"
WriteINIStr "$path_configini" "Game" "Sorting" "$sorting"

WriteINIStr "$path_configini" "Graphics" "FullScreen" "$fullscreen"
WriteINIStr "$path_configini" "Graphics" "Resolution" "$resolution"

${If} $songdir != "$INSTDIR\songs"
WriteINIStr "$path_configini" "Directories" "SongDir1" "$songdir"
${EndIf}

FunctionEnd ; Settings page End

!insertmacro MUI_PAGE_FINISH

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Pages UnInstallation Routine
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!define MUI_WELCOMEPAGE_TITLE "$(page_un_welcome_title)"
!insertmacro MUI_UNPAGE_WELCOME
!insertmacro MUI_UNPAGE_CONFIRM

UninstPage custom un.AskDelete un.DeleteAll

Function un.AskDelete

nsDialogs::Create /NOUNLOAD 1018

	${NSD_CreateLabel} 0 -195 100% 12u "$(delete_components)"
	Pop $LABEL_COMPONENTS

	${NSD_CreateCheckbox} 0 -175 100% 8u "$(delete_covers)"
	Pop $CHECKBOX_COVERS
	nsDialogs::OnClick /NOUNLOAD $CHECKBOX_COVERS $1

	${NSD_CreateCheckbox} 0 -155 100% 8u "$(delete_config)"
	Pop $CHECKBOX_CONFIG
	nsDialogs::OnClick /NOUNLOAD $CHECKBOX_CONFIG $2

	${NSD_CreateCheckbox} 0 -135 100% 8u "$(delete_highscores)"
	Pop $CHECKBOX_SCORES 
	nsDialogs::OnClick /NOUNLOAD $CHECKBOX_SCORES $3

	${NSD_CreateCheckbox} 0 -115 100% 8u "$(delete_screenshots)"
	Pop $CHECKBOX_SCREENSHOTS 
	nsDialogs::OnClick /NOUNLOAD $CHECKBOX_SCREENSHOTS $4

	${NSD_CreateCheckbox} 0 -95 100% 8u "$(delete_playlists)"
	Pop $CHECKBOX_PLAYLISTS
	nsDialogs::OnClick /NOUNLOAD $CHECKBOX_PLAYLISTS $5

	${NSD_CreateCheckbox} 0 -65 100% 18u "$(delete_songs)"
	Pop $CHECKBOX_SONGS 
	nsDialogs::OnClick /NOUNLOAD $CHECKBOX_SONGS $6


nsDialogs::Show

FunctionEnd

Function un.DeleteAll

${NSD_GetState} $CHECKBOX_COVERS $CB_COVERS_State
${NSD_GetState} $CHECKBOX_CONFIG $CB_CONFIG_State
${NSD_GetState} $CHECKBOX_SCORES $CB_SCORES_State
${NSD_GetState} $CHECKBOX_SCORES $CB_SCREENSHOTS_State
${NSD_GetState} $CHECKBOX_SCORES $CB_PLAYLISTS_State
${NSD_GetState} $CHECKBOX_SONGS  $CB_SONGS_State

${If} $CB_COVERS_State == "1" ; Remove covers
 RMDir /r "$INSTDIR\covers"
 SetShellVarContext current	
 RMDir /r "$APPDATA\ultrastardx\covers"
 SetShellVarContext all
${EndIf}

${If} $CB_CONFIG_State == "1" ; Remove config
 SetShellVarContext current
 Delete "$APPDATA\ultrastardx\config.ini" 
 SetShellVarContext all
 Delete "$INSTDIR\config.ini"
${EndIf}

${If} $CB_SCORES_State == "1" ; Remove highscores
 SetShellVarContext current
 Delete "$APPDATA\ultrastardx\Ultrastar.db" 
 SetShellVarContext all
 Delete "$INSTDIR\Ultrastar.db"
${EndIf}

${If} $CB_SCREENSHOTS_State == "1" ; Remove screenshots
 RMDir /r "$INSTDIR\sreenshots"
 SetShellVarContext current
 RMDir /r "$APPDATA\ultrastardx\screenshots"
 SetShellVarContext all
${EndIf}

${If} $CB_SCREENSHOTS_State == "1" ; Remove playlists
 RMDir /r "$INSTDIR\playlists"
 SetShellVarContext current
 RMDir /r "$APPDATA\ultrastardx\playlists"
 SetShellVarContext all
${EndIf}

${If} $CB_SONGS_State == "1" ; Remove songs
 RMDir /r "$INSTDIR\songs"
 SetShellVarContext current
 RMDir /r "$APPDATA\ultrastardx\songs"
 SetShellVarContext all
${EndIf}


FunctionEnd

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
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_website).lnk" "http://www.ultrastardeluxe.org/"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_songs).lnk" "$INSTDIR\songs"
  CreateShortCut "$SMPROGRAMS\$ICONS_GROUP\$(sm_uninstall).lnk" "$INSTDIR\Uninstall.exe"
!insertmacro MUI_STARTMENU_WRITE_END

; Vista Game Explorer:
; (removed due to incompatibility with Windows 7, needs rewrite)

; Create Uninstaller:

  WriteUninstaller "$INSTDIR\Uninstall.exe"

  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "${name}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayIcon" "$INSTDIR\ultrastardx.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "InstallDir" "$INSTDIR"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\Uninstall.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "Publisher" "${PRODUCT_PUBLISHER}"

  SetOutPath "$INSTDIR"

SectionEnd

;------------------------------------
; OPTIONAL SONGS (Section 2)
;------------------------------------

 !include "${path_settings}\files_opt_songs.nsh"

;------------------------------------
; OPTIONAL THEMES (Section 3)
;------------------------------------

; No additional themes available 
; for current version of ultrastardx

;------------------------------------
; UNINSTALL (Section 4)
;------------------------------------

Section Uninstall

 !insertmacro MUI_STARTMENU_GETFOLDER "Application" $ICONS_GROUP

 !include "${path_settings}\files_main_uninstall.nsh"

 DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"

; Unregister from Windows Vista Game Explorer
; (removed due to incompatibility with Windows 7)

SectionEnd

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Section Descriptions
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~


!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN

  !insertmacro MUI_DESCRIPTION_TEXT ${Section1} $(DESC_Section1)
  !insertmacro MUI_DESCRIPTION_TEXT ${Section2} $(DESC_Section2)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1} $(DESC_Section2_sub1)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub2} $(DESC_Section2_sub2)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub3} $(DESC_Section2_sub3)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub4} $(DESC_Section2_sub4)
; !insertmacro MUI_DESCRIPTION_TEXT ${Section3} $(DESC_Section3) THEMES

  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section1} $(DESC_g2Section1)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section2} $(DESC_g2Section2)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section3} $(DESC_g2Section3)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section4} $(DESC_g2Section4)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section5} $(DESC_g2Section5)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section6} $(DESC_g2Section6)

  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section1} $(DESC_s2_sub1_Section1)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section2} $(DESC_s2_sub1_Section2)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section3} $(DESC_s2_sub1_Section3)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section4} $(DESC_s2_sub1_Section4)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section5} $(DESC_s2_sub1_Section5)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section6} $(DESC_s2_sub1_Section6)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section7} $(DESC_s2_sub1_Section7)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section8} $(DESC_s2_sub1_Section8)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section9} $(DESC_s2_sub1_Section9)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section10} $(DESC_s2_sub1_Section10)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section11} $(DESC_s2_sub1_Section11)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section12} $(DESC_s2_sub1_Section12)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section13} $(DESC_s2_sub1_Section13)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section14} $(DESC_s2_sub1_Section14)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section15} $(DESC_s2_sub1_Section15)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section16} $(DESC_s2_sub1_Section16)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section17} $(DESC_s2_sub1_Section17)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section18} $(DESC_s2_sub1_Section18)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section19} $(DESC_s2_sub1_Section19)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section20} $(DESC_s2_sub1_Section20)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section21} $(DESC_s2_sub1_Section21)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section22} $(DESC_s2_sub1_Section22)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section23} $(DESC_s2_sub1_Section23)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub1_Section24} $(DESC_s2_sub1_Section24)

  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub2_Section1} $(DESC_s2_sub2_Section1)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub2_Section2} $(DESC_s2_sub2_Section2)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub2_Section3} $(DESC_s2_sub2_Section3)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub2_Section4} $(DESC_s2_sub2_Section4)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub2_Section5} $(DESC_s2_sub2_Section5)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub2_Section6} $(DESC_s2_sub2_Section6)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub2_Section7} $(DESC_s2_sub2_Section7)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub2_Section8} $(DESC_s2_sub2_Section8)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub2_Section9} $(DESC_s2_sub2_Section9)

  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub3_Section1} $(DESC_s2_sub3_Section1)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub3_Section2} $(DESC_s2_sub3_Section2)
  !insertmacro MUI_DESCRIPTION_TEXT ${s2_sub3_Section3} $(DESC_s2_sub3_Section3)

!insertmacro MUI_FUNCTION_DESCRIPTION_END

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Language Support
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "German"
!insertmacro MUI_LANGUAGE "Hungarian"

!insertmacro MUI_RESERVEFILE_LANGDLL

!include "${path_languages}\*.nsh"

;!addPluginDir "${path_plugins}\"

Function bgmusic
    File /oname=$PLUGINSDIR\loop.wav .\dependencies\loop.wav
    BGImage::Sound /NOUNLOAD /LOOP $PLUGINSDIR\loop.wav
FunctionEnd

Function .onGUIEnd
    BGImage::Sound /STOP
FunctionEnd

Function .onInit

var /GLOBAL version
StrCpy $version "1.1beta"


  System::Call 'kernel32::CreateMutexA(i 0, i 0, t "USdx Installer.exe") ?e'

  Pop $R0

  StrCmp $R0 0 +3
    MessageBox MB_OK|MB_ICONEXCLAMATION $(oninit_running)
    Abort

  ReadRegStr $R0  HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${name}" 'DisplayVersion'

  ${If} $R0 == $version
	  MessageBox MB_YESNO|MB_ICONEXCLAMATION \
          "${name} v.$R0 $(oninit_alreadyinstalled). $\n$\n $(oninit_installagain)" \
          IDYES continue
          Abort
  ${EndIf}

  ReadRegStr $R1 HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${name}" 'UninstallString'
  StrCmp $R1 "" done
  

  ${If} $R0 != $version
	  MessageBox MB_YESNO|MB_ICONEXCLAMATION \
          "${name} v.$R0 $(oninit_alreadyinstalled). $\n$\n $(oninit_updateusdx) v.$R0 -> v.${version}" \
          IDYES continue
          Abort
  ${EndIf}


continue:
  ReadRegStr $R2 HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${name}" 'UninstallString'
  MessageBox MB_YESNO|MB_ICONEXCLAMATION "$(oninit_uninstall)" IDNO done
  ExecWait '"$R2" _?=$INSTDIR'

done:

  !insertmacro MUI_LANGDLL_DISPLAY

  !insertmacro INSTALLOPTIONS_EXTRACT_AS ".\settings\settings-1031.ini" "Settings-1031"
  !insertmacro INSTALLOPTIONS_EXTRACT_AS ".\settings\settings-1033.ini" "Settings-1033"
  !insertmacro INSTALLOPTIONS_EXTRACT_AS ".\settings\settings-1038.ini" "Settings-1038"

FunctionEnd

Function un.onInit

        ${nsProcess::FindProcess} "USdx.exe" $R0
        StrCmp $R0 0 0 +2
        MessageBox MB_YESNO|MB_ICONEXCLAMATION '$(oninit_closeusdx)' IDYES closeit IDNO end

	closeit:
        ${nsProcess::KillProcess} "USdx.exe" $R0
        goto continue

        ${nsProcess::FindProcess} "ultrastardx.exe" $R0
        StrCmp $R0 0 0 +2
        MessageBox MB_YESNO|MB_ICONEXCLAMATION '$(oninit_closeusdx)' IDYES closeusdx IDNO end

	closeusdx:
	${nsProcess::KillProcess} "ultrastardx.exe" $R0
        goto continue

        end:
        ${nsProcess::Unload}
        Abort

  continue:
  !insertmacro MUI_LANGDLL_DISPLAY

FunctionEnd
