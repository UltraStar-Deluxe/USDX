; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: Update
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!include MUI2.nsh
!include WordFunc.nsh 
!include StrFunc.nsh
!include WinVer.nsh
!include LogicLib.nsh
!include nsDialogs.nsh
!include UAC.nsh
;!include LangFile.nsh ; included in NSIS installation
!include nsResize.nsh

${StrRep}

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Variables
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

; Installer Paths:

!define path_settings ".\settings"
!define path_languages ".\languages"
!define path_dependencies ".\dependencies"
!define path_images ".\dependencies\images"
!define path_plugins ".\dependencies\plugins"
!define path_gdf "$WINDIR\gdf.dll"

; MultiLanguage - Show all languages:
!define MUI_LANGDLL_ALLLANGUAGES

!addPluginDir "${path_plugins}\"

!include "${path_settings}\variables.nsh"
!include "${path_settings}\functions.nsh"

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Export Settings
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

SetCompress Auto
SetCompressor /SOLID lzma
SetCompressorDictSize 32
SetDatablockOptimize On

Var /GLOBAL CHECKBOX_CHECKONLINE
Var /GLOBAL label_update_information
Var /GLOBAL checkonline_state

Var /GLOBAL online_version ; used in localization, be careful at renaming
Var /GLOBAL installed_version ; used in localization, be careful at renaming

Var /GLOBAL download_version ; used in localization, be careful at renaming
Var /GLOBAL installerexe ; path to downloaded new installer

Var /GLOBAL finish_message ; contains of various message for the finish page
Var /GLOBAL skipdownload ; set if the download should be skipped
Var /GLOBAL gotversion ; set if the version file from the backend server was downloaded
Var /GLOBAL downloaded
Var /GLOBAL failed ; Error. 1=open  2=download  3=invalid_exe  4=skipped

XPStyle on

Name "${name} ${version} - Update"
Brandingtext "${name} ${version} - Update"

!system 'md "dist"'
OutFile "dist\${exeupdate}.exe"

InstallDir "${PRODUCT_PATH}"

; Windows Vista / Windows 7:
; must be "user" for UAC plugin 
RequestExecutionLevel user

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
!define MUI_ABORTWARNING_TEXT "$(abort_update)"
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

; Finish Pages:

!define MUI_FINISHPAGE_TITLE_3LINES

!define MUI_FINISHPAGE_TEXT_LARGE
!define MUI_FINISHPAGE_TEXT "$(page_finish_txt_update)"

!define MUI_FINISHPAGE_LINK "$(page_finish_linktxt)"
!define MUI_FINISHPAGE_LINK_LOCATION "${homepage}"

!define MUI_FINISHPAGE_NOREBOOTSUPPORT

; MUI_FINISHPAGE_RUN is executed as admin by default.
!define MUI_FINISHPAGE_RUN
!define MUI_FINISHPAGE_RUN_FUNCTION RunInstaller

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Pages Installation Routine
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!insertmacro MUI_PAGE_WELCOME

; USDX Update Page

Page custom pageUpdateConfirm pageVersionCheck

Page custom pageSelect

!define MUI_PAGE_CUSTOMFUNCTION_PRE PreDownload
!insertmacro MUI_PAGE_INSTFILES

!define MUI_PAGE_CUSTOMFUNCTION_SHOW FinishShowCallback
!insertmacro MUI_PAGE_FINISH

Function pageUpdateConfirm

	nsDialogs::Create /NOUNLOAD 1018

	Pop $0
	
	${NSD_CreateCheckbox} 0 -150 100% 8u "$(update_connect)"
	Pop $CHECKBOX_CHECKONLINE
	GetFunctionAddress $0 OnCheckbox
	nsDialogs::OnClick /NOUNLOAD $CHECKBOX_CHECKONLINE $0
	${NSD_SetState} $CHECKBOX_CHECKONLINE $checkonline_state
	
	${NSD_CreateLabel} 0 0 100% 30u "$(update_information)"
	Pop $label_update_information
	
	GetDlgItem $R0 $HWNDPARENT 1
	SendMessage $R0 ${WM_SETTEXT} 0 `STR:$(button_next)`
	
	nsDialogs::Show

FunctionEnd ; pageUpdateConfirm page End

Function pageVersionCheck

	; read current installed version
	ReadRegStr $installed_version "${PRODUCT_UNINST_ROOT_KEY}" "${PRODUCT_UNINST_KEY}" 'DisplayVersion'

	${NSD_GetState} $CHECKBOX_CHECKONLINE $checkonline_state	
	${If} $checkonline_state == "1"

		StrCpy $2 "${version_url}"
		StrCpy $3 "${installer_version_path}"
		
		; NSISdl::download/quiet doesn't work for some reason with the provided url and path, it creates an empty file and times out
		;NSISdl::download_quiet /TIMEOUT=5000 "$2" "$3"
		; use inetc for this purpose instead
		inetc::get /SILENT /CONNECTTIMEOUT 5 "$2" "$3"

		; if download failed
		IfFileExists "$3" +3
			SetErrors
			Goto Failed
			   
		Push 1
		Push "$3"
			Call ReadFileLine
		Pop $1
		StrCpy $online_version $1 ; store for late use
		Delete "$3"
		
		; if downloaded file doesn't have a valid version string as first line
		StrCmp $1 "" 0 +3
			SetErrors
			Goto Failed
		
		${VersionCompare} "$installed_version" "$online_version" $R1
		${If} $R1 == "0"
			StrCpy $finish_message "$(update_check_equal)"
			StrCpy $skipdownload "1"
		${ElseIf} $R1 == "1"
			StrCpy $finish_message "$(update_check_newer)"
			StrCpy $skipdownload "1"
		${ElseIf} $R1 == "2"
			StrCpy $finish_message "$(update_check_older)"
			StrCpy $skipdownload "0"				
			StrCpy $gotversion "1"
			
			Return

		NoDownload:
			StrCpy $finish_message "$(update_check_no)"
			StrCpy $skipdownload "1"
		${Else}

		Failed:
			StrCpy $finish_message "$(update_check_failed)"
			StrCpy $failed 1
			StrCpy $skipdownload "1"
		${EndIf}

	${ElseIf} $checkonline_state == "0"
		; TODO: install embedded
		StrCpy $skipdownload ""
		${if} "$installed_version" == ""
			StrCpy $finish_message "$(update_noinstallation_offline)"
		${else}	
			StrCpy $finish_message "$(update_check_offline)"
		${endif}
	${EndIf}

FunctionEnd

Function pageSelect

	${If} $skipdownload == ""
	${OrIf} $skipdownload == "1"
		Abort
	${endif}
	
	nsDialogs::Create /NOUNLOAD 1018

	Pop $0
	GetFunctionAddress $2 SelectVersionClick
		
	${NSD_CreateRadioButton} 20 100 100% 8u "$(update_versions_none)"
	Pop $1
	nsDialogs::SetUserData $1 ""
	nsDialogs::OnClick /NOUNLOAD $1 $2
	
	${NSD_CreateRadioButton} 20 120 100% 8u "${name} $online_version"
	Pop $1
	nsDialogs::SetUserData $1 "$online_version"
	nsDialogs::OnClick /NOUNLOAD $1 $2
	
	; selection first version
	${NSD_SetState} $1 "1" 
	StrCpy $download_version $online_version
	
	; TODO: add aditional available versions
	; ${NSD_CreateRadioButton} 20 140 100% 8u "${name} 1.5.1"
	; Pop $1
	; nsDialogs::SetUserData $1 "1.5.1"
	; nsDialogs::OnClick /NOUNLOAD $1 $2
	
	
	${NSD_CreateLabel} 0 0 100% 50u "$(update_versions_info)$\n$\n$finish_message"
	Pop $label_update_information
	
	GetDlgItem $R0 $HWNDPARENT 1
	SendMessage $R0 ${WM_SETTEXT} 0 `STR:$(button_next)`
	
	nsDialogs::Show

FunctionEnd ; pageSelect page End

Function FinishShowCallback
	
	SendMessage $mui.FinishPage.Text ${WM_SETTEXT} 0 "STR:$finish_message"

	;StrCpy $MUI_NOABORTWARNING 1
	
	${if} $installerexe == ""
		ShowWindow $mui.FinishPage.RUN_TEXT ${SW_HIDE} # hide installer

		${if} $failed <> ""
		${andif} $failed > "0"
			; resize for larger finish messages
			nsResize::Add $mui.FinishPage.Text 0 0 0 40u
		${endif}
	${else}
	
		; replace checkbox label with installer name
		StrCpy $0 "$(MUI_TEXT_FINISH_RUN)"
		${StrRep} $0 $0 "$(^NameDA)" "${Name} $download_version ${installername}"
		SendMessage $mui.FinishPage.RUN_TEXT ${WM_SETTEXT} 0 "STR:$0"

		; disable starting installer, it is required
		EnableWindow $mui.FinishPage.Run 0 ;  0=disabled; 1=enabled
		
	${endif}
FunctionEnd

Function RunInstaller 
	IfFileExists "$installerexe" 0 +3
		UAC::ShellExec "open" "" "$installerexe" "" ""
		Goto End
	
	End:
FunctionEnd

Function OnCheckbox

	Pop $0 # HWND
	GetDlgItem $R0 $HWNDPARENT 1
	
	${NSD_GetState} $CHECKBOX_CHECKONLINE $0
	${If} $0 <> 0
		SendMessage $R0 ${WM_SETTEXT} 0 `STR:$(button_check_update)` ;"STR:Install"
	${Else}
		SendMessage $R0 ${WM_SETTEXT} 0 `STR:$(button_next)` ;^CloseBtn
	${EndIf}

FunctionEnd

Function SelectVersionClick
	Pop $0 # HWND
	nsDialogs::GetUserData $0
	Pop $1
	StrCpy $download_version $1
FunctionEnd

Function ReadFileLine
	Exch $0 ;file
	Exch
	Exch $1 ;line number
	Push $2
	Push $3
	 
	FileOpen $2 $0 r
	StrCpy $3 0
	 
	Loop:
	IntOp $3 $3 + 1
		ClearErrors
		FileRead $2 $0
		IfErrors +2
	StrCmp $3 $1 0 loop
		FileClose $2
	
	Pop $3
	Pop $2
	Pop $1
	Exch $0
FunctionEnd

;!insertmacro MUI_PAGE_INSTFILES
;!insertmacro MUI_PAGE_FINISH

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UPDATE (Section 1)
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

Function PreDownload
	${If} $checkonline_state == "0"
	${OrIf} $skipdownload == "1"
		Abort
	${endif}
FunctionEnd

Section "Download Installer" Section2
	
	${If} $checkonline_state == "0"
	${OrIf} $skipdownload == "1"
		Abort
	${endif}
	
	${If} $download_version == ""
		StrCpy $finish_message "$(update_download_none)"
		Goto End
	${endif}
	
	; ${If} $downloaded == "1"
		; messageBox MB_OK|MB_ICONINFORMATION "abort check2"
		; Abort
	; ${endif}
	
	StrCpy $4 "${installer_exe_path}"
	StrCpy $0 ${update_url}
	${StrRep} $0 $0 ${update_mask_online_version} $1
	${StrRep} $0 $0 ${update_mask_installer_version} $2
	
	NSISdl::download /TRANSLATE2 $(update_download_downloading) $(update_download_connecting) \
		$(update_download_remain_sec) $(update_download_remain_min) \
		$(update_download_remain_hour) $(update_download_remain_secs) \
		$(update_download_remain_mins) $(update_download_remain_hours) \
		$(update_download_progress) \
		/TIMEOUT=60000 "$0" "$4"
		
	;inetc::get /CONNECTTIMEOUT 60 \
		/TRANSLATE $(update_download_downloading) $(update_download_connecting) $(update_download_sec) $(update_download_min) $(update_download_hour) $(update_download_multi) $(update_download_progress) $(update_download_remaining) \
		"$0" "$4"
		
	Pop $R0 ;Get the return value
	
	StrCmp $R0 "cancelled" +1 +4
		StrCpy $finish_message "$(update_download_aborted)"
		StrCpy $failed 4
		Goto Failed
	StrCmp $R0 "cancel" +1 +4
		StrCpy $finish_message "$(update_download_aborted)"
		StrCpy $failed 4
		Goto Failed

	IfFileExists "$4" +5
		SetErrors
		StrCpy $finish_message "$(update_download_failed)"
		StrCpy $failed 2
		Goto Failed

	
	; verify if the downloaded file is an executable file
	; (in cases the download is an html file etc.)
	${FileIsExecutable} $4 $R0
	${if} $R0 == false
		SetErrors
		StrCpy $finish_message "$(update_download_invalid_installer)"
		StrCpy $failed 3
		Goto Failed	
	${endif}
	
	; installer download finished
	StrCpy $installerexe $4
	StrCpy $finish_message "$(update_download_success)"
	Goto End
			
	Failed:
	
	End:
	
SectionEnd

Section $(name_section1) Section1
	${Unless} ${SectionIsSelected} ${Section1}
		Abort
	${EndUnless}
	
	; TODO embedded installation
	
	SectionIn RO
	SetOutPath $INSTDIR
	SetOverwrite try
	
	;WriteUninstaller "$INSTDIR\Test.exe"

SectionEnd

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Section Descriptions
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN

	!insertmacro MUI_DESCRIPTION_TEXT ${Section1} $(DESC_Section1)
	
!insertmacro MUI_FUNCTION_DESCRIPTION_END


; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Language Support
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!include "${path_settings}\languages.nsh"
!insertmacro MUI_RESERVEFILE_LANGDLL

; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Main
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

Function .onInit

	!define PRODUCT_NAME111 "${name} ${version}"

	${UAC.I.Elevate.AdminOnly}
	System::Call 'kernel32::CreateMutexA(i 0, i 0, t "USdx Update.exe") ?e'

	Pop $R0

	StrCmp $R0 0 +3
	MessageBox MB_OK|MB_ICONEXCLAMATION $(oninit_updating)
	Abort
	
	; check online by default
	StrCpy $checkonline_state 1
	
	!insertmacro MUI_LANGDLL_DISPLAY	

FunctionEnd
