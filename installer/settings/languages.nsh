; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: Language support
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "German"
!insertmacro MUI_LANGUAGE "Hungarian"
!insertmacro MUI_LANGUAGE "Polish"
!insertmacro MUI_LANGUAGE "Portuguese"
!insertmacro MUI_LANGUAGE "Spanish"

!insertmacro LANGFILE_INCLUDE "${path_languages}\English.nsh"
!insertmacro LANGFILE_INCLUDE_WITHDEFAULT "${path_languages}\German.nsh" "${path_languages}\English.nsh"
!insertmacro LANGFILE_INCLUDE_WITHDEFAULT "${path_languages}\Hungarian.nsh" "${path_languages}\English.nsh"
!insertmacro LANGFILE_INCLUDE_WITHDEFAULT "${path_languages}\Polish.nsh" "${path_languages}\English.nsh"
!insertmacro LANGFILE_INCLUDE_WITHDEFAULT "${path_languages}\Portuguese.nsh" "${path_languages}\English.nsh"
!insertmacro LANGFILE_INCLUDE_WITHDEFAULT "${path_languages}\Spanish.nsh" "${path_languages}\English.nsh"
