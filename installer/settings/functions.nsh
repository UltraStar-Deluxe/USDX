; Checks if the given file is a executable
;  ${FileIsExecutable} "PATH" $ResultVar
Function FileIsExecutable
	!define FileIsExecutable `!insertmacro FileIsExecutableCall`
 
	!macro FileIsExecutableCall _FILE _RESULT
		Push `${_FILE}`
		Call FileIsExecutable
		Pop ${_RESULT}
	!macroend
	
	Exch $0

	FileOpen $1 $0 "r" # Open the file for  reading
	FileSeek $1 0 # Move the file pointer to the right position
	FileReadByte $1 $2 # Read
	FileReadByte $1 $3 # Read
	FileClose $1 # Close the file
	
	# compare exe header
	StrCmp $2 "77" 0 BadHeader
	StrCmp $3 "90" 0 BadHeader
	StrCpy $0 true
	Goto End
	
BadHeader:
	SetErrors
	StrCpy $0 false
	
End:
	Exch $0

FunctionEnd

; Creates Desktop Shortcut(s) if 
; checked on Finish Page

Function CreateDesktopShortCuts
SetOutPath "$INSTDIR"
CreateShortcut "$Desktop\$(sm_shortcut).lnk" "$INSTDIR\${exe}.exe"
FunctionEnd

; Deletes only empty dirs which are
; at the top of the stack.

Function un.DeleteIfEmpty
  FindFirst $R0 $R1 "$0\*.*"
  strcmp $R1 "." 0 NoDelete
   FindNext $R0 $R1
   strcmp $R1 ".." 0 NoDelete
    ClearErrors
    FindNext $R0 $R1
    IfErrors 0 NoDelete
     FindClose $R0
     Sleep 1000
     RMDir "$0"
  NoDelete:
   FindClose $R0
FunctionEnd

; Finds UltraStar Deluxe process
;

!define nsProcess::FindProcess `!insertmacro nsProcess::FindProcess`

!macro nsProcess::FindProcess _FILE _ERR
	nsProcess::_FindProcess /NOUNLOAD `${_FILE}`
	Pop ${_ERR}
!macroend


!define nsProcess::KillProcess `!insertmacro nsProcess::KillProcess`

!macro nsProcess::KillProcess _FILE _ERR
	nsProcess::_KillProcess /NOUNLOAD `${_FILE}`
	Pop ${_ERR}
!macroend


!define nsProcess::Unload `!insertmacro nsProcess::Unload`

!macro nsProcess::Unload
	nsProcess::_Unload
!macroend