; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: UltraStar Creator
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

Section $(name_section5) Section5
	File ".\tools\UltraStar-Creator-1.2.0-win32-setup.exe"
	ExecWait "$INSTDIR\UltraStar-Creator-1.2.0-win32-setup.exe"
	Delete "$INSTDIR\UltraStar-Creator-1.2.0-win32-setup.exe"
SectionEnd
