; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Un/Installer: Variables
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

; Product Information:

; use this block for a dev version
;!define VersionStr "2025.6.0"
;!define FullVersion "2025.6.0+dev" ; semver compatible. see semver.org
;!define Release "Dev"
;!define DevBuild true
;!define ReleaseMeta "+dev"
;!define ReleaseApp " Dev"

; use this block for a release version
!define VersionStr "2025.6.0"
!define FullVersion "2025.6.0" ; semver compatible. see semver.org
!define Release "Stable"
!define DevBuild false
!define ReleaseMeta "+stable"
!define ReleaseApp " Stable"

!define installername " Installer"
!define installerexe "UltraStar.Deluxe_v${VersionStr}${ReleaseMeta}_installer"


; generated
!define version "${VersionStr}${ReleaseApp}"
!define meta StrLower($version)


!define name "UltraStar Deluxe"
!define publisher "USDX Team"
!define homepage "https://usdx.eu"
!define forum "https://github.com/UltraStar-Deluxe/USDX/issues"
!define wiki "https://usdx.eu"

!define exe "ultrastardx"
!define exeuninstall "Uninstall"
!define exeupdate "Update"

!define license ".\dependencies\documents\license.txt"
!define music1 ".\dependencies\loop.wav"
!define music2 "$PLUGINSDIR\loop.wav"

; Installer

!define installer_version_path "$LOCALAPPDATA\Temp\usdxversion"
!define installer_exe_path "$LOCALAPPDATA\Temp\usdxupdate.exe"
!define version_url "http://raw.githubusercontent.com/UltraStar-Deluxe/USDX/release/VERSION"
!define update_url "https://github.com/UltraStar-Deluxe/USDX/releases/download/%VERSION%/UltraStar.Deluxe_v%VERSIONSTRING%-installer.exe"
!define update_mask_online_version "%VERSION%"
!define update_mask_installer_version "%VERSIONSTRING%"

; debug - uncomment for testing
;!define /redef update_url "http://downloads.sourceforge.net/project/ultrastardx/UltraStar%20Deluxe/Version%201.0.1a/ultrastardx-101a-installer-full.exe"
;!define /redef version_url "http://raw.githubusercontent.com/UltraStar-Deluxe/USDX/master/VERSION"

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
!define PRODUCT_PATH "$PROGRAMFILES\${name}"

; Download URLs for Songs and Themes:

; SONGS
;!define download_song1 "http://downloads.sourceforge.net/ultrastardx/usdx_song-bodo_wartke_-_liebeslied.zip"
;!define download_song2 "http://downloads.sourceforge.net/ultrastardx/usdx_song-dead_smiling_pirates_-_i_18.zip"
;!define download_song3 "http://downloads.sourceforge.net/ultrastardx/usdx_song-joshua_morin_-_on_the_run.zip"
;!define download_song4 "http://downloads.sourceforge.net/ultrastardx/usdx_song-pornophonique_-_space_-_invaders.zip"
;!define download_song5 "http://downloads.sourceforge.net/ultrastardx/usdx_song-steven_dunston_-_northern_star.zip"
;!define download_song6 "http://downloads.sourceforge.net/ultrastardx/usdx_song-pornophonique_-_space_invaders_kar.zip"
;
;!define download_sub1_song1 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_monkey_shines.zip"
;!define download_sub1_song2 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_i_crush_everything.zip"
;!define download_sub1_song3 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_not_about_you.zip"
;!define download_sub1_song4 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_mr_fancy_pants.zip"
;!define download_sub1_song5 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_big_bad_world_one.zip"
;!define download_sub1_song6 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_flickr.zip"
;!define download_sub1_song7 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_my_beige_bear.zip"
;!define download_sub1_song8 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_the_future_soon.zip"
;!define download_sub1_song9 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_ikea.zip"
;!define download_sub1_song10 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_furry_old_lobster.zip"
;!define download_sub1_song11 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_code_monkey.zip"
;!define download_sub1_song12 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_im_your_moon.zip"
;!define download_sub1_song13 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_first_of_may.zip"
;!define download_sub1_song14 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_dance_soterios_johnson_dance.zip"
;!define download_sub1_song15 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_a_talk_with_george.zip"
;!define download_sub1_song16 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_creepy_doll.zip"
;!define download_sub1_song17 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_that_spells_dna.zip"
;!define download_sub1_song18 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_when_you_go.zip"
;!define download_sub1_song19 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_better.zip"
;!define download_sub1_song20 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_shop_vac.zip"
;!define download_sub1_song21 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_i_feel_fantastic.zip"
;!define download_sub1_song22 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_re-_your_brains.zip"
;!define download_sub1_song23 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_skullcrusher_mountain.zip"
;!define download_sub1_song24 "http://downloads.sourceforge.net/ultrastardx/usdx_song-jonathan_coulton_-_chiron_beta_prime.zip"
;
;!define download_sub2_song1 "http://downloads.sourceforge.net/ultrastardx/usdx_song-shearer_-_69.zip"
;!define download_sub2_song2 "http://downloads.sourceforge.net/ultrastardx/usdx_song-shearer_-_69_kar.zip"
;!define download_sub2_song3 "http://downloads.sourceforge.net/ultrastardx/usdx_song-shearer_-_cant_stop_it.zip"
;!define download_sub2_song4 "http://downloads.sourceforge.net/ultrastardx/usdx_song-shearer_-_cant_stop_it_kar.zip"
;!define download_sub2_song5 "http://downloads.sourceforge.net/ultrastardx/usdx_song-shearer_-_in_my_hand.zip"
;!define download_sub2_song6 "http://downloads.sourceforge.net/ultrastardx/usdx_song-shearer_-_man_song.zip"
;!define download_sub2_song7 "http://downloads.sourceforge.net/ultrastardx/usdx_song-shearer_-_man_song_kar.zip"
;!define download_sub2_song8 "http://downloads.sourceforge.net/ultrastardx/usdx_song-shearer_-_stay_with_me.zip"
;!define download_sub2_song9 "http://downloads.sourceforge.net/ultrastardx/usdx_song-shearer_-_stay_with_me_kar.zip"
;!define download_sub2_song10 "http://downloads.sourceforge.net/ultrastardx/usdx_song-shearer_-_consequence_of_dawn.zip"
;
;!define download_sub3_song1 "http://downloads.sourceforge.net/ultrastardx/usdx_song-wise_guys_-_lebendig_und_kraeftig_und_schaerfer.zip"
;!define download_sub3_song2 "http://downloads.sourceforge.net/ultrastardx/usdx_song-wise_guys_-_mensch_wo_bist_du.zip"
;!define download_sub3_song3 "http://downloads.sourceforge.net/ultrastardx/usdx_song-wise_guys_-_mensch_wo_bist_du_kar.zip"

; THEMES
; (removed theme section - currently no additional skins available for this usdx version)
