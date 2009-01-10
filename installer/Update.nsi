; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: Update
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!include MUI2.nsh
!include WinVer.nsh
!include LogicLib.nsh
!include nsDialogs.nsh

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

Var /GLOBAL CHECKBOX
Var /GLOBAL label_update_information
Var /GLOBAL checkbox_state

XPStyle on

Name "${name} v.${version} - Update"
Brandingtext "${name} v.${version} Update"
OutFile "ultrastardx-update.exe"

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
!define MUI_WELCOMEPAGE_TITLE "$(page_welcome_title_update)"
!define MUI_WELCOMEPAGE_TEXT "$(page_welcome_txt_update)"

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Pages Installation Routine
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!insertmacro MUI_PAGE_WELCOME

; USDX Update Page

Page custom Update Download

Function Update

nsDialogs::Create /NOUNLOAD 1018

	Pop $0
	
	${NSD_CreateCheckbox} 0 -150 100% 8u "$(update_connect)"
	Pop $CHECKBOX
	GetFunctionAddress $0 OnCheckbox
	nsDialogs::OnClick /NOUNLOAD $CHECKBOX $0

	${NSD_CreateLabel} 0 0 100% 30u "$(update_information)"
	Pop $label_update_information

nsDialogs::Show


FunctionEnd ; Update page End

Function Download

${NSD_GetState} $CHECKBOX $checkbox_state

${If} $checkbox_state == "1"

NSISdl::download_quiet /TIMEOUT=30000 http://ultrastardx.svn.sourceforge.net/viewvc/ultrastardx/trunk/installer/version.txt $TEMP\version.txt

FileOpen $4 "$TEMP\version.txt" r
FileRead $4 $1

ReadRegStr $R0  HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${name}" 'DisplayVersion'


${VersionCompare} "$R0" "$1" $R1

MessageBox MB_OK "$R1. R0 = $R0 und R1 = $1"



${Else}



${EndIf}

FunctionEnd

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UPDATE (Section 1)
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

Section $(name_section1) Section1
	SectionIn RO
	SetOutPath $INSTDIR
	SetOverwrite try

SectionEnd

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Language Support
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "German"

!insertmacro MUI_RESERVEFILE_LANGDLL

!include "${path_languages}\*.nsh"

Function .onInit

  !insertmacro MUI_LANGDLL_DISPLAY



FunctionEnd
