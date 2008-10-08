; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Un/Installer: Variables
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

; Product Information:

!define version "1.1a" ; Make sure version is also set in onInit


!define name "UltraStar Deluxe"
!define publisher "USDX Team"
!define homepage "http://www.ultrastardeluxe.org"
!define forum "http://forum.ultrastardeluxe.org"

!define exe "USdx"

!define license "license.txt"

; Icons

!define img_install "install.ico"
!define img_uninstall "uninstall.ico"

; Header Images

!define img_header "header.bmp" ; Header image (150x57)
!define img_side "side.bmp" ; Side image (162x314)

; Registry for Start menu entries:

!define PRODUCT_NAME "${name}"
!define PRODUCT_VERSION "${version}"
!define PRODUCT_PUBLISHER "${publisher}"
!define PRODUCT_WEB_SITE "${homepage}"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${name}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"
!define PRODUCT_STARTMENU_REGVAL "NSIS:StartMenuDir"

; Download URLs for Songs and Themes:

!define download_song1 "http://downloads.sourceforge.net/ultrastardx/usdx_song-dead_smiling_pirates_-_i_18.zip"
!define download_song2 "http://downloads.sourceforge.net/ultrastardx/usdx_song-joshua_morin_-_on_the_run.zip"
!define download_song3 "http://downloads.sourceforge.net/ultrastardx/usdx_song-pornophonique_-_space_-_invaders.zip"
!define download_song4 "http://downloads.sourceforge.net/ultrastardx/usdx_song-steven_dunston_-_northern_star.zip"

!define download_theme1 "http://downloads.sourceforge.net/ultrastardx/usdx_skin-orange_by_Skar.zip"
!define download_theme2 "http://downloads.sourceforge.net/ultrastardx/usdx_skin-Streetlight_by_Skar.zip"
!define download_theme3 "http://downloads.sourceforge.net/ultrastardx/usdx_skin-Vistar_by_Skar.zip"
!define download_theme4 "http://downloads.sourceforge.net/ultrastardx/usdx_skin-bluesensationV5_by_Charis.zip"
!define download_theme5 "http://downloads.sourceforge.net/ultrastardx/usdx_skin-WiiStar_by_MasterPhW.zip"
!define download_theme6 "http://downloads.sourceforge.net/ultrastardx/usdx_skin-istar_by_MezzoX.zip"