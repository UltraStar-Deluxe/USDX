; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer - Version 1.1: Main components
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~


; Delete created Icons in startmenu

 SetShellVarContext all

 Delete "$SMPROGRAMS\$ICONS_GROUP\Uninstall.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Deinstallieren.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Website.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Internetseite.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\UltraStar Deluxe spielen.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Play UltraStar Deluxe.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Readme.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Lies mich.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\Lizenz.lnk"
 Delete "$SMPROGRAMS\$ICONS_GROUP\License.lnk"

; Delete created Icon on Desktop

Delete "$Desktop\Play UltraStar Deluxe.lnk"
Delete "$Desktop\UltraStar Deluxe spielen.lnk"

StrCpy $0 "$SMPROGRAMS\$ICONS_GROUP"
Call un.DeleteIfEmpty

