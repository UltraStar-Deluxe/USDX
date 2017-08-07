; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: UltraStar Creator
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

Section $(name_section5) Section5
	File ".\tools\UltraStar-Creator-setup.exe"
	ExecWait "$INSTDIR\UltraStar-Creator-setup.exe"
	Delete "$INSTDIR\UltraStar-Creator-setup.exe"
SectionEnd
