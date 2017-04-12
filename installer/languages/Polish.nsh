; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer - Language file: Polish
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!insertmacro LANGFILE_EXT Polish

${LangFileString} error_download_song_info "Błąd pobierania utworu \'$0' z: $\r$\n$1$\r$\n$\r$\n"
${LangFileString} error_download_song_msg "Błąd pobierania utwory: '$0' z: $\r$\n$1$\r$\n$\r$\nKliknij OK aby kontynuować instalację."
${LangFileString} error_download_theme_info "Błąd pobierania stylu \'$0' z: $\r$\n$1$\r$\n$\r$\n"
${LangFileString} error_download_theme_msg "Błąd pobierania stylu: '$0' z: $\r$\n$1$\r$\n$\r$\nKliknij OK aby kontynuować instalację."
${LangFileString} error_download_skin_info "Błąd pobierania skórki \'$0' z: $\r$\n$1$\r$\n$\r$\n"
${LangFileString} error_download_skin_msg "Błąd pobierania skórki: '$0' z: $\r$\n$1$\r$\n$\r$\nKliknij OK aby kontynuować instalację."
${LangFileString} error_download_plugin_info "Błąd pobierania pluginu \'$0' z: $\r$\n$1$\r$\n$\r$\n"
${LangFileString} error_download_plugin_msg "Błąd pobierania pluginu: '$0' z: $\r$\n$1$\r$\n$\r$\nKliknij OK aby kontynuować instalację."

${LangFileString} abort_install "Czy jesteś pewien, że chcesz zatrzymać instalację?"
${LangFileString} abort_uninstall "Czy jesteś pewien, że chcesz zatrzymać odinstalowywanie?"
${LangFileString} abort_update "Czy jesteś pewien, że chcesz przerwać aktualizację?"
${LangFileString} oninit_running "Instalacja jest już uruchomiona."
${LangFileString} oninit_updating "Aktualizacja jest już uruchomiona."
${LangFileString} oninit_installagain "Jesteś pewien, że chcesz zainstalować jeszcze raz?"
${LangFileString} oninit_alreadyinstalled "jest już zainstalowany"
${LangFileString} oninit_closeusdx "nie może być odinstalowany dopóki jest uruchomiony! Czy chcesz go zamknąć?"
${LangFileString} oninit_updateusdx "Czy chcesz zaktualizować instalację z:"
${LangFileString} oninit_uninstall "Czy chcesz odinstalować starą wersję? (rekomendowane)"

${LangFileString} update_connect "Sprawdź połączenie z Internetem i sprawdź nowe wersje"
${LangFileString} button_next "Dalej >"
${LangFileString} button_close "Zamknij"
${LangFileString} update_information "Możesz sprawdzić czy jest nowsza wersja '${name}'. Aby to zrobić połącz się z internetem. Jeżeli nowa wersja zostanie znaleziona, będzie można ją zainstalować."
${LangFileString} button_check_update "Sprawdź"

${LangFileString} delete_components "Usuń również następujące składniki:"
${LangFileString} delete_covers "Okładka"
${LangFileString} delete_highscores "Wyniki"
${LangFileString} delete_config "Koniguracja"
${LangFileString} delete_screenshots "Zrzuty ekranów"
${LangFileString} delete_playlists "Playlisty"
${LangFileString} delete_songs "Usunąć piosenki? UWAGA: Wszystkie pliki w katalogu InstallationDir\songs będą usunięte(!)"

${LangFileString} update_noinstallation_online "Nie masz zainstalowanej żadnej wersji. Obecny instalator nie może zaktualizować twojej wersji. Odwiedź naszą stronę ${homepage}, aby pobrać nową wersję."
${LangFileString} update_noinstallation_offline "Nie masz zainstalowanej żadnej wersji. Obecny instalator/aktualizator nie może zainstalować wersji. Odwiedź naszą stronę ${homepage}, aby pobrać wersję."
${LangFileString} update_check_offline "Twoja wersja $installed_version jest aktualna. Obecny instalator nie może zaktualizować twojej wersji. Odwiedź stronę naszego projektu, aby uzyskać najnowsze informacje i aktualizacje."
${LangFileString} update_check_older "Twoja wersja $installed_version jest przestarzała. Nowa wersja online_version ${name} jest już dostępna. Czy chcesz dokonać aktualizacji?"
${LangFileString} update_check_equal "Obecnie zainstalowana wersja $installed_version jest najbardziej aktualna. Nie ma nowszych wersji."
${LangFileString} update_check_newer "Twoja obecnie zainstalowana wersja $installed_version jest nowsza niż $\r$\nobecnie wydana wersja online_version ${name}. Nie ma nowszych aktualizacji."
${LangFileString} update_check_no "Obecny aktualizator/instalator nie zainstaluje wersji. Odwiedź naszą stronę ${homepage}, aby pobrać nową wersję."
${LangFileString} update_check_failed "Poszukiwanie nowyszch wersji zakończyło siê błędem. Czy chcesz odwiedzić stronę główną i sprawdzić ręcznie?"
${LangFileString} update_download_success "Pobieranie nowej wersji $online_version zakończyło się sukcesem.$\r$\n$\r$\nDokończ aktualizację zamykając ten aktualizator. Następnie zostanie uruchomiona nowa instalacja."
${LangFileString} update_download_failed "Pobieranie nowej wersji $online_version nie powiodło się. Instalator nie może zostać pobrany.$\r$\n$\r$\nProszę, odwiedź naszą stronę ${homepage}, aby pobrać nową wersję."
${LangFileString} update_download_aborted "Pobieranie nowej wersji $online_version zostało przerwane. Aktualizacja nie zostanie przeprowadzona. Pamiętaj, odwiedź naszą stronę ${homepage}, aby uzyskać najnowsze informacje i aktualizacje."
${LangFileString} update_download_invalid_installer "Pobieranie nowej wersji $online_version nie powiodło się. Pobrany instalator jest nieprawidłowy. Mogło to nastąpić, jeśli serwer/strona napotkała problemy, już nie istnieje, lub jest w trakcie konserwacji.$\r$\n$\r$\nProszę, odwiedź naszą stronę ${homepage} i pobierz instalator ręcznie."
${LangFileString} update_download_none "Nie wybrano wersji do pobrania. Instalator nie może zaktualizować twojej wersji. Odwiedź naszą stronę ${homepage}, aby uzyskać najnowsze informacje i aktualizacje."
${LangFileString} update_versions_info "Przynajmniej jedna nowa wersja ${name} została znaleziona. Proszę, wybierz konkretną wersję do aktualizacji. Ta wersja zostanie pobrana i następnie zostanie uruchomiona instalacja."
${LangFileString} update_versions_none "Brak"

${LangFileString} update_download_downloading "Pobieranie %s "
${LangFileString} update_download_connecting "Nawiązywanie połączenia ... "
${LangFileString} update_download_sec "sek."
${LangFileString} update_download_min "min."
${LangFileString} update_download_hour "godz."
${LangFileString} update_download_multi "s"
${LangFileString} update_download_progress "%dkiB (%d%%) z %dkiB @ %d.%02dkiB/s"
${LangFileString} update_download_remaining " (%d %s%s do końca)"
${LangFileString} update_download_remain_sec " (1 sekunda do końca)"
${LangFileString} update_download_remain_min " (1 minuta do końca)"
${LangFileString} update_download_remain_hour " (1 godzina do końca)"
${LangFileString} update_download_remain_secs " (%u sekund do końca)"
${LangFileString} update_download_remain_mins " (%u minut do końca)"
${LangFileString} update_download_remain_hours " (%u godzin do końca)"

; Welcome Page:

${LangFileString} page_welcome_title_update "Witamy w kreatorze aktualizacji programu ${name}"
${LangFileString} page_welcome_txt_update "Ten kreator przeprowadzi cię przez proces aktualizacji gry ${name}. ${name} jest bezpłatną i Otwartą grą Karaoke, którą można porównać z SingStar'em.$\r$\n$\r$\nZespół ${name} życzy miłej zabawy.$\r$\n$\r$\nStrona projektu:$\n$\r${homepage}$\r$\n$\r$\nForum wsparcia:$\n$\r${forum}"
${LangFileString} page_welcome_title "Witamy w kreatorze instalacji programu ${name}"
${LangFileString} page_welcome_txt "Ten kreator przeprowadzi cię przez proces instalacji gry ${name}. ${name} jest bezpłatną i Otwartą grą Karaoke, którą można porównać z SingStar'em.$\r$\n$\r$\nZespół ${name} życzy miłej zabawy.$\r$\n$\r$\nStrona projektu:$\n$\r${homepage}$\r$\n$\r$\nForum wsparcia:$\n$\r${forum}"
${LangFileString} page_un_welcome_title "Witamy w kreatorze deinstalacji gry ${name}"

; Components Page:

${LangFileString} page_components_info "Najedź myszką na komponent, aby zobaczyć szczegóły"

; Custom Page

${LangFileString} page_settings_subtitle "Wybierz ustawienia dogodne dla Ciebie."
${LangFileString} page_settings_config_title "Konfiguracja ${name} (opcjonalnie)"
${LangFileString} page_settings_config_info "Wszystkie ustawienia można później zmieniać w grze."
${LangFileString} page_settings_fullscreen_label "Tryb Pełnoekranowy:"
${LangFileString} page_settings_fullscreen_info "Czy uruchamiać grę w oknie czy na pełnym ekranie?"
${LangFileString} page_settings_language_label "Język:"
${LangFileString} page_settings_language_info "Dostosuj język interfejsu graficznego."
${LangFileString} page_settings_resolution_label "Rozdzielczość:"
${LangFileString} page_settings_resolution_info "Wybierz rozdzieczość ekran/okna."
${LangFileString} page_settings_tabs_label "Zakładki:"
${LangFileString} page_settings_tabs_info "Czy chcesz aby piosenki zostały pogrupowane na zakładki?"
${LangFileString} page_settings_sorting_label "Sortowanie:"
${LangFileString} page_settings_sorting_info "Wybierz kryteria sortowania piosenek."
${LangFileString} page_settings_songdir_label "SongDir"
${LangFileString} page_settings_songdir_info "Wybierz katalog w którym znajdują się piosenki?"

; Finish Page:

${LangFileString} page_finish_txt "${name} został poprawnie zainstalowany na twoim komputerze.$\r$\n$\r$\nOdwiedź Naszą stronę aby otrzymać najnowsze wiadomości i aktualizacje."
${LangFileString} page_finish_linktxt "Strona Projektu"
${LangFileString} page_finish_desktop "Czy stwórzyć skrót na Pulpicie?"

;unused
;TODO ${LangFileString} page_finish_txt_update "${name} Update has checked for a new version."

; Start Menu and Shortcuts

${LangFileString} sm_shortcut "Graj w ${name}"
${LangFileString} sm_uninstall "Odinstaluj"
${LangFileString} sm_website "Strona Projektu"
${LangFileString} sm_license "Licencja"
${LangFileString} sm_readme "Readme"
${LangFileString} sm_songs "Piosenki"
${LangFileString} sm_update "Aktualizacja"
${LangFileString} sm_documentation "Dokumentacja"

${LangFileString} sc_play "Graj"
${LangFileString} sc_desktop "Czy stwórzyć skrót na Pulpicie?"

; Sections and SectionGroups

${LangFileString} name_section1 "Główne komponenty"
${LangFileString} name_section2 "Piosenki"
${LangFileString} name_s2_sub1 "Jonathan Coulton"
${LangFileString} name_s2_sub2 "Shearer"
${LangFileString} name_s2_sub3 "Wise Guys"
${LangFileString} name_s2_sub4 "Pornophonique"

${LangFileString} DESC_Section1 "To są podstawowe pliki wymagane przez ${name}"
${LangFileString} DESC_Section2 "Możesz wybrać które piosenki chcesz ściągnąć (Wymagane połączenie z Internetem!)"
${LangFileString} DESC_Section2_sub1 "Możesz wybrać które piosenki Jonathan Coulton (CC by-nc 3.0) chcesz zainstalować."
${LangFileString} DESC_Section2_sub2 "Możesz wybrać które piosenki Shearer (CC by-nc-sa 2.0 / 3.0) chcesz zainstalować."
${LangFileString} DESC_Section2_sub3 "Możesz wybrać które piosenki Wise Guys chcesz zainstalować."
${LangFileString} DESC_Section2_sub4 "Możesz wybrać które piosenki Pornophonique chcesz zainstalować."

${LangFileString} DESC_g2Section2 "Ściągnij piosenkę 'Dead Smiling Pirates - I 18' (CC by-nc-nd 2.5)."
${LangFileString} DESC_g2Section3 "Ściągnij piosenkę 'Joshua Morin - On The Run' (CC by-sa 2.5)."
${LangFileString} DESC_g2Section4 "Ściągnij piosenkę 'Pornophonique - Space Invaders' (CC by-nc-nd 2.0)."
${LangFileString} DESC_g2Section5 "Ściągnij piosenkę 'Steven Dunston - Northern Star' (CC by-nc-sa 2.5)."
${LangFileString} DESC_g2Section1 "Ściągnij piosenkę 'Bodo Wartke - Liebeslied (Love Song)'."
${LangFileString} DESC_g2Section6 "Ściągnij piosenkę 'Pornophonique - Space Invaders (Karaoke)' (CC by-nc-nd 2.0)."

${LangFileString} DESC_s2_sub1_Section1 "Ściągnij piosenkę 'Monkey Shines'."
${LangFileString} DESC_s2_sub1_Section2 "Ściągnij piosenkę 'I Crush Everything'."
${LangFileString} DESC_s2_sub1_Section3 "Ściągnij piosenkę 'Not About You'."
${LangFileString} DESC_s2_sub1_Section4 "Ściągnij piosenkę 'Mr. Fancy Pants'."
${LangFileString} DESC_s2_sub1_Section5 "Ściągnij piosenkę 'Big Bad World One'."
${LangFileString} DESC_s2_sub1_Section6 "Ściągnij piosenkę 'Flickr [incl. video]'."
${LangFileString} DESC_s2_sub1_Section7 "Ściągnij piosenkę 'My Beige Bear'."
${LangFileString} DESC_s2_sub1_Section8 "Ściągnij piosenkę 'The Future Soon'."
${LangFileString} DESC_s2_sub1_Section9 "Ściągnij piosenkę 'Ikea'."
${LangFileString} DESC_s2_sub1_Section10 "Ściągnij piosenkę 'Furry Old Lobster'."
${LangFileString} DESC_s2_sub1_Section11 "Ściągnij piosenkę 'Code Monkey [incl. video]'."
${LangFileString} DESC_s2_sub1_Section12 "Ściągnij piosenkę 'I´m Your Moon'."
${LangFileString} DESC_s2_sub1_Section13 "Ściągnij piosenkę 'First Of May'."
${LangFileString} DESC_s2_sub1_Section14 "Ściągnij piosenkę 'Dance, Soterios Johnson, Dance'."
${LangFileString} DESC_s2_sub1_Section15 "Ściągnij piosenkę 'A Talk With George'."
${LangFileString} DESC_s2_sub1_Section16 "Ściągnij piosenkę 'Creepy Doll [incl. video]'."
${LangFileString} DESC_s2_sub1_Section17 "Ściągnij piosenkę 'That Spells DNA'."
${LangFileString} DESC_s2_sub1_Section18 "Ściągnij piosenkę 'When You Go'."
${LangFileString} DESC_s2_sub1_Section19 "Ściągnij piosenkę 'Better'."
${LangFileString} DESC_s2_sub1_Section20 "Ściągnij piosenkę 'Shop Vac'."
${LangFileString} DESC_s2_sub1_Section21 "Ściągnij piosenkę 'I Feel Fantastic'."
${LangFileString} DESC_s2_sub1_Section22 "Ściągnij piosenkę 'Re: Your Brains'."
${LangFileString} DESC_s2_sub1_Section23 "Ściągnij piosenkę 'Skullcrusher Mountain'."
${LangFileString} DESC_s2_sub1_Section24 "Ściągnij piosenkę 'Chiron Beta Prime [incl. video]'."

${LangFileString} DESC_s2_sub2_Section1 "Ściągnij piosenkę '69'."
${LangFileString} DESC_s2_sub2_Section2 "Ściągnij piosenkę '69 (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section3 "Ściągnij piosenkę 'Can't stop it'."
${LangFileString} DESC_s2_sub2_Section4 "Ściągnij piosenkę 'Can't stop it (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section5 "Ściągnij piosenkę 'In My Hand'."
${LangFileString} DESC_s2_sub2_Section6 "Ściągnij piosenkę 'Man Song'."
${LangFileString} DESC_s2_sub2_Section7 "Ściągnij piosenkę 'Man Song (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section8 "Ściągnij piosenkę 'Stay with me'."
${LangFileString} DESC_s2_sub2_Section9 "Ściągnij piosenkę 'Stay with me (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section10 "Ściągnij piosenkę 'Consequence Of Dawn'."

${LangFileString} DESC_s2_sub3_Section1 "Ściągnij piosenkę 'Lebendig und kräftig und schärfer'."
${LangFileString} DESC_s2_sub3_Section2 "Ściągnij piosenkę 'Mensch, wo bist du?'."
${LangFileString} DESC_s2_sub3_Section3 "Ściągnij piosenkę 'Mensch, wo bist du? (Karaoke)'."

; Optional Themes
; (not available)
