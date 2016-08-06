; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer - Language file: Polish
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!insertmacro LANGFILE_EXT Polish

;TODO ${LangFileString} error_download_song_info "Error downloading song \'$0' from: $\r$\n$1$\r$\n$\r$\n"
;TODO ${LangFileString} error_download_song_msg "Error downloading song: '$0' from: $\r$\n$1$\r$\n$\r$\nClick OK to continue the installation."
;TODO ${LangFileString} error_download_theme_info "Error downloading theme \'$0' from: $\r$\n$1$\r$\n$\r$\n"
;TODO ${LangFileString} error_download_theme_msg "Error downloading theme: '$0' from: $\r$\n$1$\r$\n$\r$\nClick OK to continue the installation."
;TODO ${LangFileString} error_download_skin_info "Error downloading skin \'$0' from: $\r$\n$1$\r$\n$\r$\n"
;TODO ${LangFileString} error_download_skin_msg "Error downloading skin: '$0' from: $\r$\n$1$\r$\n$\r$\nClick OK to continue the installation."
;TODO ${LangFileString} error_download_plugin_info "Error downloading plugin \'$0' from: $\r$\n$1$\r$\n$\r$\n"
;TODO ${LangFileString} error_download_plugin_msg "Error downloading plugin: '$0' from: $\r$\n$1$\r$\n$\r$\nClick OK to continue the installation."

${LangFileString} abort_install "Czy jesteœ pewien że chcesz zatrzymaæ instalacjê?"
${LangFileString} abort_uninstall "Czy jesteœ pewien że chcesz zatrzymaæ odinstalowywanie?"
;TODO ${LangFileString} abort_update "Are you sure to abort the update?"
${LangFileString} oninit_running "Instajacja jest już uruchomiona."
;TODO ${LangFileString} oninit_updating "An update is already running."
${LangFileString} oninit_installagain "Jesteœ pewien że chcesz zainstalowaæ jeszcze raz?"
${LangFileString} oninit_alreadyinstalled "Jest obecnie zainstalowany"
${LangFileString} oninit_closeusdx "nie może byæ odinstalowany dopóki jest uruchomiony! Czy chcesz zamknąæ go?"
${LangFileString} oninit_updateusdx "Do you want to update the installation from:"
${LangFileString} oninit_uninstall "Czy chcesz odinstalowaæ starą wersjê? (rekomendowane)"

${LangFileString} update_connect "SprawdŸ połączenie z insternetm i sprawdŸ nowe wersje"
${LangFileString} button_next "Dalej >"
${LangFileString} button_close "Zamknij"
${LangFileString} update_information "Możesz sprawdziæ czy jest nowsza wersja '${name}'. Aby to zrobiæ połącz siê z internetm. Jeœli nowa wersja zostanie znaleziona, bêdzie można ją zainstalowaæ."
;TODO ${LangFileString} button_check_update "Check"

${LangFileString} delete_components "Takżê usunąæ nastêpujące składniki:"
${LangFileString} delete_covers "Okładka?"
${LangFileString} delete_highscores "Wyniki?"
${LangFileString} delete_config "Koniguracja?"
${LangFileString} delete_screenshots "Zrzuty ekranów?"
${LangFileString} delete_playlists "Playlisty?"
${LangFileString} delete_songs "Usunąæ piosenki? UWAGA: Wszystkie pliki w katalogu InstallationDir\songs bêdą usuniête(!)"

;TODO ${LangFileString} update_noinstallation_online "You have no version installed. The current installer cannot update your version. Check our website ${homepage} for a new version."
;TODO ${LangFileString} update_noinstallation_offline "You have no version installed. The current installer/updater cannot install a version. Check our website ${homepage} for a version."
;TODO ${LangFileString} update_check_offline "Your version $installed_version is up-to-date. The current installer cannot update your version. Visit our project website to get latest news and updates."
${LangFileString} update_check_older "Twoja werjsa $installed_version jest przestarzała. Nowa wersja online_version ${name} jest już dostêpna. Czy chcesz dokonaæ aktualizacji?"
${LangFileString} update_check_equal "Obecnie zainstalowana wersja $installed_version jest najbardziej aktualna. Nie ma nowszych wersji."
${LangFileString} update_check_newer "Twoja obecnie zainstalowana wersja $installed_version jest nowsza niż $\r$\nobecnie wydana wersja online_version ${name}. Nie ma nowszych aktualizacji."
;TODO ${LangFileString} update_check_no "The current updater/installer won't install a version. Check our website ${homepage} for a new version."
${LangFileString} update_check_failed "Poszukiwanie nowyszch wersji zakoñczyło siê błêdem. Czy chcesz odwiedziæ stronê główną i sprawdziæ rêcznie?"
;TODO ${LangFileString} update_download_success "The download of the new version $online_version succeeded.$\r$\n$\r$\nFinish the update by closing this updater. The new installation will be started right after."
;TODO ${LangFileString} update_download_failed "The download of the new version $online_version failed. The installer could not be downloaded.$\r$\n$\r$\nPlease, visit our website ${homepage} for the new version."
;TODO ${LangFileString} update_download_aborted "The download of the new version $online_version was aborted. Nothing will be updated. Remember, visit our website ${homepage} for latest news and updates."
;TODO ${LangFileString} update_download_invalid_installer "The download of the new version $online_version failed. The downloaded installer was invalid. This can happen if the server/website has some issues, does not exist anymore or is in maintenance mode.$\r$\n$\r$\nPlease, visit our website ${homepage} and download the installer manually."
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

; Welcome Page:

${LangFileString} page_welcome_title_update "Witamy w kreatorze aktualizacji programu ${name}"
${LangFileString} page_welcome_txt_update "Ten kreator przeprowadzi ciê przez proces aktualizacji gry ${name}. ${name} jest bezpłatną i Otwartą grą Karaoke, którą można porównaæ z SingStar'em.$\r$\n$\r$\nZespół ${name} życzy miłej zabawy.$\r$\n$\r$\nStrona projektu:$\n$\r${homepage}$\r$\n$\r$\nForum wsparcia:$\n$\r${forum}"
${LangFileString} page_welcome_title "Witamy w kreatorze instalacji programu ${name}"
${LangFileString} page_welcome_txt "Ten kreator przeprowadzi ciê przez proces instalacji gry ${name}. ${name} jest bezpłatną i Otwartą grą Karaoke, którą można porównaæ z SingStar'em.$\r$\n$\r$\nZespół ${name} życzy miłej zabawy.$\r$\n$\r$\nStrona projektu:$\n$\r${homepage}$\r$\n$\r$\nForum wsparcia:$\n$\r${forum}"
${LangFileString} page_un_welcome_title "Witamy w kreatorze deinstalacji gry ${name}"

; Components Page:

${LangFileString} page_components_info "NajedŸ myszką na komponent, aby zobaczyæ szczegóły"

; Custom Page

${LangFileString} page_settings_subtitle "Wybierz ustawienia dogodne dla Ciebie."
${LangFileString} page_settings_config_title "Konfiguracja ${name} (opcjonalnie)"
${LangFileString} page_settings_config_info "Wszystkie ustawienia można póŸniej zmieniæ w grze."
${LangFileString} page_settings_fullscreen_label "Tryb Pełnoekranowy:"
${LangFileString} page_settings_fullscreen_info "Czy uruchamiaæ grê w oknie czy na pełnym ekranie?"
${LangFileString} page_settings_language_label "Jêzyk:"
${LangFileString} page_settings_language_info "Dostosuj jêzyk GUI."
${LangFileString} page_settings_resolution_label "Rozdzielczoœæ:"
${LangFileString} page_settings_resolution_info "Wybierz rozdzieczoœæ ekrany/okna."
${LangFileString} page_settings_tabs_label "Zakładki:"
${LangFileString} page_settings_tabs_info "Czy chcesz aby piosenki zostały pogrupowane na zakładki?"
${LangFileString} page_settings_sorting_label "Sortowanie:"
${LangFileString} page_settings_sorting_info "Wybierz kryteria sortowania piosenek."
${LangFileString} page_settings_songdir_label "SongDir"
${LangFileString} page_settings_songdir_info "Wybierz katalog w którym znajdują siê piosenki?"

; Finish Page:

${LangFileString} page_finish_txt "${name} został poprawnie zainstalowany na twoim komputerze.$\r$\n$\r$\nOdwiedŸ Naszą stronê aby otrzymaæ najnowsze wiadomoœci i aktualizacjê."
${LangFileString} page_finish_linktxt "Strona Projektu"
${LangFileString} page_finish_desktop "Czy stwórzyæ skrót na Pulpicie?"

;unused
;TODO ${LangFileString} page_finish_txt_update "${name} Update has checked for a new version."

; Start Menu and Shortcuts

${LangFileString} sm_shortcut "Graj w ${name}"
${LangFileString} sm_uninstall "Odinstaluj"
${LangFileString} sm_website "Strona Projektu"
${LangFileString} sm_license "Licencja"
${LangFileString} sm_readme "Readme"
${LangFileString} sm_songs "Piosenki"
;TODO ${LangFileString} sm_update "Update"
${LangFileString} sm_documentation "Dokumentacja"

${LangFileString} sc_play "Graj"
${LangFileString} sc_desktop "Czy stwórzyæ skrót na Pulpicie?"

; Sections and SectionGroups

${LangFileString} name_section1 "Główne komponenty"
${LangFileString} name_section2 "Piosenki"
${LangFileString} name_s2_sub1 "Jonathan Coulton"
${LangFileString} name_s2_sub2 "Shearer"
${LangFileString} name_s2_sub3 "Wise Guys"
${LangFileString} name_s2_sub4 "Pornophonique"

${LangFileString} DESC_Section1 "To są podstawowe pliki potrzebe przez ${name}"
${LangFileString} DESC_Section2 "Możesz wybraæ które piosenki chcesz œciągnąæ (Wymagane połączenie z internetem!)"
${LangFileString} DESC_Section2_sub1 "Możesz wybraæ które piosenki Jonathan Coulton (CC by-nc 3.0) chcesz zainstalowaæ."
${LangFileString} DESC_Section2_sub2 "Możesz wybraæ które piosenki Shearer (CC by-nc-sa 2.0 / 3.0) chcesz zainstalowaæ."
${LangFileString} DESC_Section2_sub3 "Możesz wybraæ które piosenki Wise Guys chcesz zainstalowaæ."
${LangFileString} DESC_Section2_sub4 "Możesz wybraæ które piosenki Pornophonique chcesz zainstalowaæ."

${LangFileString} DESC_g2Section2 "Œciągnij piosenkê 'Dead Smiling Pirates - I 18' (CC by-nc-nd 2.5)."
${LangFileString} DESC_g2Section3 "Œciągnij piosenkê 'Joshua Morin - On The Run' (CC by-sa 2.5)."
${LangFileString} DESC_g2Section4 "Œciągnij piosenkê 'Pornophonique - Space Invaders' (CC by-nc-nd 2.0)."
${LangFileString} DESC_g2Section5 "Œciągnij piosenkê 'Steven Dunston - Northern Star' (CC by-nc-sa 2.5)."
${LangFileString} DESC_g2Section1 "Œciągnij piosenkê 'Bodo Wartke - Liebeslied (Love Song)'."
${LangFileString} DESC_g2Section6 "Œciągnij piosenkê 'Pornophonique - Space Invaders (Karaoke)' (CC by-nc-nd 2.0)."

${LangFileString} DESC_s2_sub1_Section1 "Œciągnij piosenkê 'Monkey Shines'."
${LangFileString} DESC_s2_sub1_Section2 "Œciągnij piosenkê 'I Crush Everything'."
${LangFileString} DESC_s2_sub1_Section3 "Œciągnij piosenkê 'Not About You'."
${LangFileString} DESC_s2_sub1_Section4 "Œciągnij piosenkê 'Mr. Fancy Pants'."
${LangFileString} DESC_s2_sub1_Section5 "Œciągnij piosenkê 'Big Bad World One'."
${LangFileString} DESC_s2_sub1_Section6 "Œciągnij piosenkê 'Flickr [incl. video]'."
${LangFileString} DESC_s2_sub1_Section7 "Œciągnij piosenkê 'My Beige Bear'."
${LangFileString} DESC_s2_sub1_Section8 "Œciągnij piosenkê 'The Future Soon'."
${LangFileString} DESC_s2_sub1_Section9 "Œciągnij piosenkê 'Ikea'."
${LangFileString} DESC_s2_sub1_Section10 "Œciągnij piosenkê 'Furry Old Lobster'."
${LangFileString} DESC_s2_sub1_Section11 "Œciągnij piosenkê 'Code Monkey [incl. video]'."
${LangFileString} DESC_s2_sub1_Section12 "Œciągnij piosenkê 'I´m Your Moon'."
${LangFileString} DESC_s2_sub1_Section13 "Œciągnij piosenkê 'First Of May'."
${LangFileString} DESC_s2_sub1_Section14 "Œciągnij piosenkê 'Dance, Soterios Johnson, Dance'."
${LangFileString} DESC_s2_sub1_Section15 "Œciągnij piosenkê 'A Talk With George'."
${LangFileString} DESC_s2_sub1_Section16 "Œciągnij piosenkê 'Creepy Doll [incl. video]'."
${LangFileString} DESC_s2_sub1_Section17 "Œciągnij piosenkê 'That Spells DNA'."
${LangFileString} DESC_s2_sub1_Section18 "Œciągnij piosenkê 'When You Go'."
${LangFileString} DESC_s2_sub1_Section19 "Œciągnij piosenkê 'Better'."
${LangFileString} DESC_s2_sub1_Section20 "Œciągnij piosenkê 'Shop Vac'."
${LangFileString} DESC_s2_sub1_Section21 "Œciągnij piosenkê 'I Feel Fantastic'."
${LangFileString} DESC_s2_sub1_Section22 "Œciągnij piosenkê 'Re: Your Brains'."
${LangFileString} DESC_s2_sub1_Section23 "Œciągnij piosenkê 'Skullcrusher Mountain'."
${LangFileString} DESC_s2_sub1_Section24 "Œciągnij piosenkê 'Chiron Beta Prime [incl. video]'."

${LangFileString} DESC_s2_sub2_Section1 "Œciągnij piosenkê '69'."
${LangFileString} DESC_s2_sub2_Section2 "Œciągnij piosenkê '69 (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section3 "Œciągnij piosenkê 'Can't stop it'."
${LangFileString} DESC_s2_sub2_Section4 "Œciągnij piosenkê 'Can't stop it (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section5 "Œciągnij piosenkê 'In My Hand'."
${LangFileString} DESC_s2_sub2_Section6 "Œciągnij piosenkê 'Man Song'."
${LangFileString} DESC_s2_sub2_Section7 "Œciągnij piosenkê 'Man Song (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section8 "Œciągnij piosenkê 'Stay with me'."
${LangFileString} DESC_s2_sub2_Section9 "Œciągnij piosenkê 'Stay with me (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section10 "Œciągnij piosenkê 'Consequence Of Dawn'."

${LangFileString} DESC_s2_sub3_Section1 "Œciągnij piosenkê 'Lebendig und kräftig und schärfer'."
${LangFileString} DESC_s2_sub3_Section2 "Œciągnij piosenkê 'Mensch, wo bist du?'."
${LangFileString} DESC_s2_sub3_Section3 "Œciągnij piosenkê 'Mensch, wo bist du? (Karaoke)'."

; Optional Themes
; (not available)
