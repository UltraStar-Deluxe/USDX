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

Name "${name} - Update"
Brandingtext "${name} Update"
OutFile "ultrastardx-update.exe"

InstallDir "$PROGRAMFILES\${name}"

; Windows Vista / Windows 7:

RequestExecutionLevel admin

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

NSISdl::download /TIMEOUT=50000 http://ultrastardeluxe.xtremeweb-hosting.net/version.txt $TEMP\version.txt

Push 1
Push "$TEMP\version.txt"
 Call ReadFileLine
Pop $1

ReadRegStr $R0  HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${name}" 'DisplayVersion'

${VersionCompare} "$R0" "$1" $R1

${If} $R1 == "0"

messageBox MB_OK|MB_ICONINFORMATION "$(update_check_equal)"

${Else}
	${If} $R1 == "1"

        IfFileExists $TEMP\version.txt FileExists
           SetErrors
           Goto Failed

        FileExists:
	messageBox MB_OK|MB_ICONINFORMATION "$(update_check_newer)"

	${Else}

		${If} $R1 == "2"
		messageBox MB_YESNO|MB_ICONQUESTION \
 		"$(update_check_older)" IDNO +6

		Push 2
		Push "$TEMP\version.txt"
 		Call ReadFileLine
		Pop $2

		ExecShell Open $2

		${Else}

		Failed:
		messageBox MB_YESNO|MB_ICONQUESTION \
 		"$(update_check_failed)" IDNO +2

		ExecShell Open http://www.ultrastardeluxe.org

		${EndIf}
	${EndIf}
${EndIf}
${Else}

; If checkbox_state = 0


${EndIf}

Delete "$TEMP\version.txt"


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
