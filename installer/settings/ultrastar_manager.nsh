; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: UltraStar Manager
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

Section $(name_section4) Section4
	File ".\tools\UltraStar-Manager-1.8.4-win32-setup.exe"
	ExecWait "$INSTDIR\UltraStar-Manager-1.8.4-win32-setup.exe"
	Delete "$INSTDIR\UltraStar-Manager-1.8.4-win32-setup.exe"
SectionEnd
