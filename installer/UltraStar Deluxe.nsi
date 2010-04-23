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

Var /GLOBAL CHECKBOX
Var /GLOBAL checkbox_state

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

${WriteToConfig} "[Game]$\r$\n" "$path_configini"
${WriteToConfig} "Language=$language2$\r$\n" "$path_configini"
${WriteToConfig} "Tabs=$tabs$\r$\n" "$path_configini"
${WriteToConfig} "Sorting=$sorting$\r$\n" "$path_configini"

${WriteToConfig} "[Graphics]$\r$\n" "$path_configini"
${WriteToConfig} "FullScreen=$fullscreen$\r$\n" "$path_configini"
${WriteToConfig} "Resolution=$resolution$\r$\n" "$path_configini"

${WriteToConfig} "[Directories]$\r$\n" "$path_configini"
${WriteToConfig} "SongDir2=$songdir$\r$\n" "$path_configini"

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

	${NSD_CreateCheckbox} 0 -150 100% 8u "$(delete_all)"
	Pop $CHECKBOX

	nsDialogs::OnClick /NOUNLOAD $CHECKBOX $0


nsDialogs::Show

FunctionEnd

Function un.DeleteAll

${NSD_GetState} $CHECKBOX $checkbox_state

${If} $checkbox_state == "1"

; Remove settings, songs, highscores, covers

 RMDir /r "$INSTDIR\songs"
 RMDir /r "$INSTDIR\covers"
 Delete "$INSTDIR\config.ini"
 Delete "$INSTDIR\Ultrastar.db"

 SetShellVarContext current	
 RMDir /r "$APPDATA\ultrastardx\songs"
 RMDir /r "$APPDATA\ultrastardx\covers"
 Delete "$APPDATA\ultrastardx\config.ini"
 Delete "$APPDATA\ultrastardx\Ultrastar.db"
 SetShellVarContext all

${Else}

; If checkbox_state = 0


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

SectionGroup $(name_section2) Section2

Section /o "Bodo Wartke - Liebeslied (Love Song)" g2Section1

   AddSize 10342
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_song1} $LOCALAPPDATA\Temp\Song-BodoWartke-LoveSong.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-BodoWartke-LoveSong.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-BodoWartke-LoveSong.zip"

  SetOutPath "$INSTDIR"


SectionEnd

;
; Dead Smiling Pirates - I 18
; 

Section /o "Dead Smiling Pirates - I 18" g2Section2
   AddSize 2816
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\songs\Dead Smiling Pirates - I 18"
   SetOutPath "$INSTDIR\songs\Dead Smiling Pirates - I 18\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_song2} $LOCALAPPDATA\Temp\Song-I-18.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-I-18.zip" "$INSTDIR\songs\Dead Smiling Pirates - I 18\"

  Delete "$LOCALAPPDATA\Temp\Song-I-18.zip"

  SetOutPath "$INSTDIR"

SectionEnd

;
; Jonathan Coulton Songs
; 

SectionGroup $(name_s2_sub1) s2_sub1

Section /o "Monkey Shines" s2_sub1_Section1

   AddSize 1455
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song1} $LOCALAPPDATA\Temp\Song-JC-MS.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-MS.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-MS.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "I Crush Everything" s2_sub1_Section2

   AddSize 7127
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song2} $LOCALAPPDATA\Temp\Song-JC-ICE.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-ICE.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-ICE.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Not About You" s2_sub1_Section3

   AddSize 3492
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song3} $LOCALAPPDATA\Temp\Song-JC-NAY.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-NAY.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-NAY.zip"

  SetOutPath "$INSTDIR"


SectionEnd

Section /o "Mr. Fancy Pants" s2_sub1_Section4

   AddSize 2427
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song4} $LOCALAPPDATA\Temp\Song-JC-MFP.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-MFP.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-MFP.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Big Bad World One" s2_sub1_Section5

   AddSize 4424
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song5} $LOCALAPPDATA\Temp\Song-JC-BBWO.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-BBWO.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-BBWO.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Flickr" s2_sub1_Section6

   AddSize 21607
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song6} $LOCALAPPDATA\Temp\Song-JC-Flickr.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-Flickr.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-Flickr.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "My Beige Bear" s2_sub1_Section7

   AddSize 4926
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song7} $LOCALAPPDATA\Temp\Song-JC-MBB.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-MBB.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-MBB.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "The Future Soon" s2_sub1_Section8

   AddSize 5612
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song8} $LOCALAPPDATA\Temp\Song-JC-TFS.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-TFS.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-TFS.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Ikea" s2_sub1_Section9

   AddSize 4608
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song9} $LOCALAPPDATA\Temp\Song-JC-Ikea.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-Ikea.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-Ikea.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Furry Old Lobster" s2_sub1_Section10

   AddSize 3288
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song10} $LOCALAPPDATA\Temp\Song-JC-FOL.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-FOL.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-FOL.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Code Monkey" s2_sub1_Section11

   AddSize 21402
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song11} $LOCALAPPDATA\Temp\Song-JC-CM.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-CM.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-CM.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "I´m Your Moon" s2_sub1_Section12

   AddSize 4916
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song12} $LOCALAPPDATA\Temp\Song-JC-IYM.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-IYM.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-IYM.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "First Of May" s2_sub1_Section13

   AddSize 6257
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song13} $LOCALAPPDATA\Temp\Song-JC-FOM.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-FOM.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-FOM.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Dance, Soterious Johnson, Dance" s2_sub1_Section14

   AddSize 5929
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song14} $LOCALAPPDATA\Temp\Song-JC-DSJD.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-DSJD.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-DSJD.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "A Talk With George" s2_sub1_Section15

   AddSize 4076
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song15} $LOCALAPPDATA\Temp\Song-JC-ATWG.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-ATWG.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-ATWG.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Creepy Doll" s2_sub1_Section16

   AddSize 66560
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song16} $LOCALAPPDATA\Temp\Song-JC-CD.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-CD.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-CD.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "That Spells DNA" s2_sub1_Section17

   AddSize 4158
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song17} $LOCALAPPDATA\Temp\Song-JC-TSDNA.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-TSDNA.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-TSDNA.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "When You Go" s2_sub1_Section18

   AddSize 5755
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song18} $LOCALAPPDATA\Temp\Song-JC-WYG.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-WYG.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-WYG.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Better" s2_sub1_Section19

   AddSize 4199
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song19} $LOCALAPPDATA\Temp\Song-JC-Better.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-Better.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-Better.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Shop Vac" s2_sub1_Section20

   AddSize 5448
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song20} $LOCALAPPDATA\Temp\Song-JC-SV.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-SV.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-SV.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "I Feel Fantastic" s2_sub1_Section21

   AddSize 3851
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song21} $LOCALAPPDATA\Temp\Song-JC-IFF.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-IFF.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-IFF.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Re: Your Brains" s2_sub1_Section22

   AddSize 7087
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song22} $LOCALAPPDATA\Temp\Song-JC-ReYB.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-ReYB.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-ReYB.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Skullcrusher Mountain" s2_sub1_Section23

   AddSize 6298
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song23} $LOCALAPPDATA\Temp\Song-JC-SCM.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-SCM.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-SCM.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Chiron Beta Prime" s2_sub1_Section24

   AddSize 38298
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song24} $LOCALAPPDATA\Temp\Song-JC-CBP.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-CBP.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-CBP.zip"

  SetOutPath "$INSTDIR"

SectionEnd


SectionGroupEnd

;
; Joshua Morin - On The Run
; 

Section /o "Joshua Morin - On The Run" g2Section3
   AddSize 3881
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\songs\Joshua Morin - On The Run"
   SetOutPath "$INSTDIR\songs\Joshua Morin - On The Run\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_song3} $LOCALAPPDATA\Temp\Song-On-the-run.zip

  Pop $R0 ;Get the return value
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-On-the-run.zip" "$INSTDIR\songs\Joshua Morin - On The Run\"

  Delete "$LOCALAPPDATA\Temp\Song-On-the-run.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Pornophonique - Space Invaders" g2Section4
   AddSize 3646
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\songs\Pornophonique - Space Invaders"
   SetOutPath "$INSTDIR\songs\Pornophonique - Space Invaders\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_song4} $LOCALAPPDATA\Temp\Song-Space-Invaders.zip

  Pop $R0 ;Get the return value
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Space-Invaders.zip" "$INSTDIR\songs\Pornophonique - Space Invaders\"

  Delete "$LOCALAPPDATA\Temp\Song-Space-Invaders.zip"

  SetOutPath "$INSTDIR"

SectionEnd

SectionGroup $(name_s2_sub2) s2_sub2

Section /o "Shearer - 69" s2_sub2_Section1

   AddSize 4557
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song1} $LOCALAPPDATA\Temp\Song-Shearer-69.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-69.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-69.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Shearer - 69 (Karaoke)" s2_sub2_Section2

   AddSize 4772
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song2} $LOCALAPPDATA\Temp\Song-Shearer-69-Kar.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-69-Kar.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-69-Kar.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Shearer - Can't stop it" s2_sub2_Section3

   AddSize 5510
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song3} $LOCALAPPDATA\Temp\Song-Shearer-CSI.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-CSI.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-CSI.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Shearer - Can't stop it (Karaoke)" s2_sub2_Section4

   AddSize 4178
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song4} $LOCALAPPDATA\Temp\Song-Shearer-CSI-Kar.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-CSI-Kar.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-CSI-Kar.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Shearer - In My Hand" s2_sub2_Section5

   AddSize 5960
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song5} $LOCALAPPDATA\Temp\Song-Shearer-IMH.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-IMH.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-IMH.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Shearer - Man Song" s2_sub2_Section6

   AddSize 7270
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song6} $LOCALAPPDATA\Temp\Song-Shearer-MS.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-MS.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-MS.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Shearer - Man Song (Karaoke)" s2_sub2_Section7

   AddSize 5807
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song7} $LOCALAPPDATA\Temp\Song-Shearer-MS-Kar.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-MS-Kar.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-MS-Kar.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Shearer - Stay With Me" s2_sub2_Section8

   AddSize 6400
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song8} $LOCALAPPDATA\Temp\Song-Shearer-SWM.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-SWM.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-SWM.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Shearer - Stay With Me (Karaoke)" s2_sub2_Section9

   AddSize 5417
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song9} $LOCALAPPDATA\Temp\Song-Shearer-SWM-Kar.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:

  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-SWM-Kar.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-SWM-Kar.zip"

  SetOutPath "$INSTDIR"

SectionEnd

SectionGroupEnd

Section /o "Steven Dunston - Northern Star" g2Section5
   AddSize 2427
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\songs\Steven Dunston - Northern Star"
   SetOutPath "$INSTDIR\songs\Steven Dunston - Northern Star\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_song5} $LOCALAPPDATA\Temp\Song-Northern-Star.zip

  Pop $R0 ;Get the return value
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:

  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Northern-Star.zip" "$INSTDIR\songs\Steven Dunston - Northern Star\"

  Delete "$LOCALAPPDATA\Temp\Song-Northern-Star.zip"

  SetOutPath "$INSTDIR"

SectionEnd

SectionGroup $(name_s2_sub3) s2_sub3

Section /o "Wise Guys - Lebendig und kräftig und schärfer" s2_sub3_Section1

   AddSize 4015
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub3_song1} $LOCALAPPDATA\Temp\Song-WiseGuys-LUKUS.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:

  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-WiseGuys-LUKUS.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-WiseGuys-LUKUS.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Wise Guys - Mensch, wo bist du?" s2_sub3_Section2

   AddSize 5335
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub3_song2} $LOCALAPPDATA\Temp\Song-WiseGuys-MWBD.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:

  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-WiseGuys-MWBD.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-WiseGuys-MWBD.zip"

  SetOutPath "$INSTDIR"

SectionEnd

Section /o "Wise Guys - Mensch, wo bist du? (Karaoke)" s2_sub3_Section3

   AddSize 5335
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub3_song3} $LOCALAPPDATA\Temp\Song-WiseGuys-MWBDKar.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:

  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-WiseGuys-MWBDKar.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-WiseGuys-MWBDKar.zip"

  SetOutPath "$INSTDIR"

SectionEnd

SectionGroupEnd

SectionGroupEnd

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
; !insertmacro MUI_DESCRIPTION_TEXT ${Section3} $(DESC_Section3)

  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section1} $(DESC_g2Section1)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section2} $(DESC_g2Section2)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section3} $(DESC_g2Section3)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section4} $(DESC_g2Section4)
  !insertmacro MUI_DESCRIPTION_TEXT ${g2Section5} $(DESC_g2Section5)

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
