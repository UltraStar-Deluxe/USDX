; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer - Language file: Hungarian
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; Translation by Gergely BOR <borg42+ultrastardx at gmail.com>.

!insertmacro LANGFILE_EXT Hungarian

${LangFileString} abort_install "Biztosan megszakítja a telepítést?"
${LangFileString} abort_uninstall "Biztosan megszakítja a program eltávolítását?"
;TODO ${LangFileString} abort_update "Are you sure to abort the update?"
${LangFileString} oninit_running "A telepítés már folyamatban van."
;TODO ${LangFileString} oninit_updating "An update is already running."
${LangFileString} oninit_installagain "Biztos abban, hogy még egyszer telepíteni szeretné?"
${LangFileString} oninit_alreadyinstalled "már telepítve van"
${LangFileString} oninit_closeusdx "nem eltávolítható, ha még fut! Kívánja a program bezárását?"
${LangFileString} oninit_updateusdx "Kívánja-e a programot frissíteni errõl a helyrõl:"
${LangFileString} oninit_uninstall "Do you want to uninstall the old version? (recommended)"

${LangFileString} update_connect "Új verzió keresése az interneten"
${LangFileString} button_next "Tovább >"
${LangFileString} button_close "Bezárás"
${LangFileString} update_information "Ellenõrizheti, hogy van-e új '${name}'-verzió. Ehhez internetkapcsolat szükséges. Ha van új verzió, az ezt követõen telepíthetõ."
;TODO ${LangFileString} button_check_update "Check"

${LangFileString} delete_components "Also delete the following components:"
${LangFileString} delete_covers "Töröljük a borítókat?"
${LangFileString} delete_highscores "Töröljük a pontszámokat?"
${LangFileString} delete_config "Config?"
${LangFileString} delete_screenshots "Screenshots?"
${LangFileString} delete_playlists "Playlists?"
${LangFileString} delete_songs "Töröljük a dalokat is? WARNING: ALL files within the InstallationDir\songs folder will be removed(!)"

;TODO ${LangFileString} update_noinstallation_online "You have no version installed. The current installer cannot update your version. Check our website ${homepage} for a new version."
;TODO ${LangFileString} update_noinstallation_offline "You have no version installed. The current installer/updater cannot install a version. Check our website ${homepage} for a version."
;TODO ${LangFileString} update_check_offline "Your version $installed_version is up-to-date. The current installer cannot update your version. Visit our project website to get latest news and updates."
${LangFileString} update_check_older "Az Ön gépén telepített verziónál ($installed_version) van már újabb (online_version). Kívánja frissíteni?"
${LangFileString} update_check_equal "Az Ön gépén a legfrissebb verzió ($installed_version) van telepítve. Frissítés nem szükséges."
${LangFileString} update_check_newer "Az Ön gépén telepített verzió ($installed_version) frissebb, mint a jelenlegi kiadás (online_version). Frissítés nem szükséges."
;TODO ${LangFileString} update_check_no "The current updater/installer won't install a version. Check our website ${homepage} for a new version."
${LangFileString} update_check_failed "Az ellenõrzés sikertelen. Kívánja betölteni a program weboldalát, hogy ellenõrizhesse hogy van-e frissítés?"
;TODO ${LangFileString} update_download_success "The download of the new version $online_version succeeded.$\n$\r$\n$\rFinish the update by closing this updater. The new installation will be started right after."
;TODO ${LangFileString} update_download_failed "The download of the new version $online_version failed. The installer could not be downloaded.$\n$\r$\n$\rPlease, visit our website ${homepage} for the new version."
;TODO ${LangFileString} update_download_aborted "The download of the new version $online_version was aborted. Nothing will be updated. Remember, visit our website ${homepage} for latest news and updates."
;TODO ${LangFileString} update_download_invalid_installer "The download of the new version $online_version failed. The downloaded installer was invalid. This can happen if the server/website has some issues, does not exist anymore or is in maintenance mode.$\n$\r$\n$\rPlease, visit our website ${homepage} and download the installer manually."
;TODO ${LangFileString} update_download_none "No version to download selected. The current installer cannot update your version. Check our website ${homepage} for latest news and updates."
;TODO ${LangFileString} update_versions_info "At least one new version of ${name} has been found. Please, select a specific version and choose to update. This version will be downloaded and the installation will be started afterwards."
;TODO ${LangFileString} update_versions_none "None"

;TODO ${LangFileString} update_download_downloading "Downloading %s "
;TODO ${LangFileString} update_download_connecting "Connecting ... "
;TODO ${LangFileString} update_download_sec "sec"
;TODO ${LangFileString} update_download_min "min"
;TODO ${LangFileString} update_download_hour "hour"
;TODO ${LangFileString} update_download_multi "s"
;TODO ${LangFileString} update_download_progress "%dkiB (%d%%) of %dkiB @ %d.%02dkiB/s"
;TODO ${LangFileString} update_download_remaining " (%d %s%s remaining)"
;TODO ${LangFileString} update_download_remain_sec " (1 second remaining)"
;TODO ${LangFileString} update_download_remain_min " (1 minute remaining)"
;TODO ${LangFileString} update_download_remain_hour " (1 hour remaining)"
;TODO ${LangFileString} update_download_remain_secs " (%u seconds remaining)"
;TODO ${LangFileString} update_download_remain_mins " (%u minutes remaining)"
;TODO ${LangFileString} update_download_remain_hours " (%u hours remaining)"
;TODO ${LangFileString} page_finish_txt_update "${name} Update has checked for a new version."

; Welcome Page:

${LangFileString} page_welcome_title_update "Üdvözli Önt az ${name} frissítési varázslója!"
${LangFileString} page_welcome_txt_update "Ez a varázsló végigvezeti Önt az ${name} frissítési folyamatán. Az ${name} egy ingyenes és szabad karaokeprogram, olyan mint a Singstar.$\n$\r$\n$\r${publisher} csapata jó szórakozást kíván!$\n$\r$\n$\rA projekt weboldala:$\n$\r${homepage}$\n$\r$\n$\rTámogatás a fórumunkon:$\n$\r${forum}"
${LangFileString} page_welcome_title "Üdvözli Önt az ${name} telepítési varázslója!"
${LangFileString} page_welcome_txt "Ez a varázsló végigvezeti Önt az ${name} telepítési folyamatán. Az ${name} egy ingyenes és szabad karaokeprogram, olyan mint a Singstar.$\n$\r$\n$\r${publisher} csapata jó szórakozást kíván!$\n$\r$\n$\rA projekt weboldala:$\n$\r${homepage}$\n$\r$\n$\rTámogatás a fórumunkon:$\n$\r${forum}"
${LangFileString} page_un_welcome_title "Üdvözli Önt az ${name} eltávolítási varázslója!"

; Components Page:

${LangFileString} page_components_info "Részletes információért vigye az egeret a komponens fölé"

; Custom Page

${LangFileString} page_settings_subtitle "Adja meg a kedvenc ${name}-beállítását."
;TODO ${LangFileString} page_settings_config_title "${name} Configuration (optional)"
;TODO ${LangFileString} page_settings_config_info "All settings can also be changed in the GUI later."
${LangFileString} page_settings_fullscreen_label "Teljes képernyõs üzemmód:"
;TODO ${LangFileString} page_settings_fullscreen_info "Start game in window or fullscreen?"
${LangFileString} page_settings_language_label "Nyelv:"
;TODO ${LangFileString} page_settings_language_info "Adjust the GUI language."
${LangFileString} page_settings_resolution_label "Felbontás:"
;TODO ${LangFileString} page_settings_resolution_info "Choose screen resolution/window size."
;TODO ${LangFileString} page_settings_tabs_label "Tabs:"
;TODO ${LangFileString} page_settings_tabs_info "Employ a virtual folder structure to show songs?"
;TODO ${LangFileString} page_settings_sorting_label "Sorting:"
;TODO ${LangFileString} page_settings_sorting_info "Select criterion to sort songs."
;TODO ${LangFileString} page_settings_songdir_label "SongDir"
;TODO ${LangFileString} page_settings_songdir_info "Choose additional song directory for ${name}."

; Finish Page:

${LangFileString} page_finish_txt "Az ${name} telepítése sikeresen befejezõdött.$\n$\r$\n$\rHa kíváncsi a legfrissebb hírekre és frissítésekre, kérjük látogassa meg projektünk weboldalát."
${LangFileString} page_finish_linktxt "A projekt weboldala"
${LangFileString} page_finish_desktop "Tegyünk egy parancsikont az asztalra?"

; Start Menu and Shortcuts

${LangFileString} sm_shortcut "${name} karaoke"
${LangFileString} sm_uninstall "Eltávolítás"
${LangFileString} sm_website "Weboldal"
${LangFileString} sm_license "Licensz"
${LangFileString} sm_readme "OlvassEl"
${LangFileString} sm_songs "Dalok"
;TODO ${LangFileString} sm_update "Update"
${LangFileString} sm_documentation "Dokumentáció"

${LangFileString} sc_play "Játék"
${LangFileString} sc_desktop "Tegyünk egy parancsikont az asztalra?"

; Sections and SectionGroups

${LangFileString} name_section1 "Fõ komponensek"
${LangFileString} name_section2 "Dalok"
${LangFileString} name_s2_sub1 "Jonathan Coulton"
${LangFileString} name_s2_sub2 "Shearer"
${LangFileString} name_s2_sub3 "Wise Guys"
${LangFileString} name_s2_sub4 "Pornophonique"

${LangFileString} DESC_Section1 "Azok a fájlok, melyek mindenképpen szükségesek az ${name} futtatásához."
${LangFileString} DESC_Section2 "Kiválaszthatja, hogy milyen dalok legyenek telepítve."
${LangFileString} DESC_Section2_sub1 "Kiválaszthatja, hogy mely Jonathan Coulton-dalok (CC by-nc 3.0) legyenek telepítve."
${LangFileString} DESC_Section2_sub2 "Kiválaszthatja, hogy mely Shearer-dalok (CC by-nc-sa 2.0 / 3.0) legyenek telepítve."
${LangFileString} DESC_Section2_sub3 "Kiválaszthatja, hogy mely Wise Guys-dalok legyenek telepítve."
${LangFileString} DESC_Section2_sub4 "Kiválaszthatja, hogy mely Pornophonique-dalok legyenek telepítve."
${LangFileString} DESC_Section3 "Kiválaszthatja, hogy milyen témák legyenek telepítve. A témák használatával a program grafikai megjelenése változik meg. A témák telepítése nem kötelezõ."

${LangFileString} DESC_g2Section2 "Telepítésre kerül a 'Dead Smiling Pirates - I 18' (CC by-nc-nd 2.5) címu dal."
${LangFileString} DESC_g2Section3 "Telepítésre kerül a 'Joshua Morin - On The Run' (CC by-sa 2.5) címu dal."
${LangFileString} DESC_g2Section4 "Telepítésre kerül a 'Pornophonique - Space Invaders' (CC by-nc-nd 2.0) címu dal."
${LangFileString} DESC_g2Section5 "Telepítésre kerül a 'Steven Dunston - Northern Star' (CC by-nc-sa 2.5) címu dal."
${LangFileString} DESC_g2Section1 "Telepítésre kerül a 'Bodo Wartke - Liebeslied (Love Song)' címu dal."
${LangFileString} DESC_g2Section6 "Telepítésre kerül a 'Pornophonique - Space Invaders (Karaoke)' (CC by-nc-nd 2.0) címu dal."

${LangFileString} DESC_s2_sub1_Section1 "Telepítésre kerül a 'Monkey Shines' címu dal."
${LangFileString} DESC_s2_sub1_Section2 "Telepítésre kerül a 'I Crush Everything' címu dal."
${LangFileString} DESC_s2_sub1_Section3 "Telepítésre kerül a 'Not About You'."
${LangFileString} DESC_s2_sub1_Section4 "Telepítésre kerül a 'Mr. Fancy Pants' címu dal."
${LangFileString} DESC_s2_sub1_Section5 "Telepítésre kerül a 'Big Bad World One' címu dal."
${LangFileString} DESC_s2_sub1_Section6 "Telepítésre kerül a 'Flickr' címu dal (videóval)."
${LangFileString} DESC_s2_sub1_Section7 "Telepítésre kerül a 'My Beige Bear' címu dal."
${LangFileString} DESC_s2_sub1_Section8 "Telepítésre kerül a 'The Future Soon' címu dal."
${LangFileString} DESC_s2_sub1_Section9 "Telepítésre kerül a 'Ikea' címu dal."
${LangFileString} DESC_s2_sub1_Section10 "Telepítésre kerül a 'Furry Old Lobster' címu dal."
${LangFileString} DESC_s2_sub1_Section11 "Telepítésre kerül a 'Code Monkey' címu dal (videóval)."
${LangFileString} DESC_s2_sub1_Section12 "Telepítésre kerül a 'I'm Your Moon' címu dal."
${LangFileString} DESC_s2_sub1_Section13 "Telepítésre kerül a 'First Of May' címu dal."
${LangFileString} DESC_s2_sub1_Section14 "Telepítésre kerül a 'Dance, Soterios Johnson, Dance' címu dal."
${LangFileString} DESC_s2_sub1_Section15 "Telepítésre kerül a 'A Talk With George' címu dal."
${LangFileString} DESC_s2_sub1_Section16 "Telepítésre kerül a 'Creepy Doll' címu dal (videóval)."
${LangFileString} DESC_s2_sub1_Section17 "Telepítésre kerül a 'That Spells DNA' címu dal."
${LangFileString} DESC_s2_sub1_Section18 "Telepítésre kerül a 'When You Go' címu dal."
${LangFileString} DESC_s2_sub1_Section19 "Telepítésre kerül a 'Better' címu dal."
${LangFileString} DESC_s2_sub1_Section20 "Telepítésre kerül a 'Shop Vac' címu dal."
${LangFileString} DESC_s2_sub1_Section21 "Telepítésre kerül a 'I Feel Fantastic' címu dal."
${LangFileString} DESC_s2_sub1_Section22 "Telepítésre kerül a 'Re: Your Brains' címu dal."
${LangFileString} DESC_s2_sub1_Section23 "Telepítésre kerül a 'Skullcrusher Mountain' címu dal."
${LangFileString} DESC_s2_sub1_Section24 "Telepítésre kerül a 'Chiron Beta Prime' címu dal (videóval)."

${LangFileString} DESC_s2_sub2_Section1 "Telepítésre kerül a '69' címu dal."
${LangFileString} DESC_s2_sub2_Section2 "Telepítésre kerül a '69 (Karaoke)' címu dal."
${LangFileString} DESC_s2_sub2_Section3 "Telepítésre kerül a 'Can't stop it' címu dal."
${LangFileString} DESC_s2_sub2_Section4 "Telepítésre kerül a 'Can't stop it (Karaoke)' címu dal."
${LangFileString} DESC_s2_sub2_Section5 "Telepítésre kerül a 'In My Hand' címu dal."
${LangFileString} DESC_s2_sub2_Section6 "Telepítésre kerül a 'Man Song' címu dal."
${LangFileString} DESC_s2_sub2_Section7 "Telepítésre kerül a 'Man Song (Karaoke)' címu dal."
${LangFileString} DESC_s2_sub2_Section8 "Telepítésre kerül a 'Stay with me' címu dal."
${LangFileString} DESC_s2_sub2_Section9 "Telepítésre kerül a 'Stay with me (Karaoke)' címu dal."

${LangFileString} DESC_s2_sub3_Section1 "Telepítésre kerül a 'Lebendig und kräftig und schärfer' címu dal."
${LangFileString} DESC_s2_sub3_Section2 "Telepítésre kerül a 'Mensch, wo bist du?' címu dal."
${LangFileString} DESC_s2_sub3_Section3 "Telepítésre kerül a 'Mensch, wo bist du? (Karaoke)' címu dal."

; Optional Themes
; (not available)
