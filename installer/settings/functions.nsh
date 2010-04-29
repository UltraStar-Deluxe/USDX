; Creates Desktop Shortcut(s) if 
; checked on Finish Page

Function CreateDesktopShortCuts
SetOutPath "$INSTDIR"
CreateShortcut "$Desktop\$(sm_shortcut).lnk" "$INSTDIR\ultrastardx.exe"
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