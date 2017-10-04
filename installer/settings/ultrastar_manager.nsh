; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: UltraStar Manager
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

Section $(name_section4) Section4
	File ".\tools\UltraStar-Manager-setup.exe"
	ExecWait "$INSTDIR\UltraStar-Manager-setup.exe"
	Delete "$INSTDIR\UltraStar-Manager-setup.exe"
SectionEnd
