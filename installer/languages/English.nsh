; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer - Language file: English
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!insertmacro LANGFILE_EXT English

${LangFileString} abort_install "Are you sure to abort installation?"
${LangFileString} abort_uninstall "Are you sure to abort uninstallation?"
${LangFileString} abort_update "Are you sure to abort the update?"
${LangFileString} oninit_running "The installer is already running."
${LangFileString} oninit_updating "An update is already running."
${LangFileString} oninit_installagain "Are you sure you want to install it again?"
${LangFileString} oninit_alreadyinstalled "is already installed"
${LangFileString} oninit_closeusdx "cannot be uninstalled while its running! Do you want to close it?"
${LangFileString} oninit_updateusdx "Do you want to update the installation from:"
${LangFileString} oninit_uninstall "Do you want to uninstall the old version? (recommended)"

${LangFileString} update_connect "Establish internet connection and check for new version"
${LangFileString} button_next "Next >"
${LangFileString} button_close "Close"

${LangFileString} delete_components "Also delete the following components:"
${LangFileString} delete_covers "Cover"
${LangFileString} delete_highscores "Highscores"
${LangFileString} delete_config "Config"
${LangFileString} delete_screenshots "Screenshots"
${LangFileString} delete_playlists "Playlists"
${LangFileString} delete_songs "Remove songs? WARNING: ALL files within the InstallationDir\songs folder will be removed(!)"

${LangFileString} update_information "You can check if a new version of '${name}' is available. Thereto an internet connection will be established. If a new version is found, it can be installed afterwards."
${LangFileString} button_check_update "Check"

${LangFileString} update_noinstallation_online "You have no version installed. The current installer cannot update your version. Check our website ${homepage} for a new version."
${LangFileString} update_noinstallation_offline "You have no version installed. The current installer/updater cannot install a version. Check our website ${homepage} for a version."
${LangFileString} update_check_offline "Your version $installed_version is up-to-date. The current installer cannot update your version. Visit our project website to get latest news and updates."
${LangFileString} update_check_older "Your version $installed_version is outdated. The new version $online_version of ${name} is available. Choose your option:"
${LangFileString} update_check_equal "Your currently installed version $installed_version is up-to-date. No update needed."
${LangFileString} update_check_newer "Your installed version $installed_version is newer than the $\n$\rcurrent release version $online_version of ${name}. No update needed."
${LangFileString} update_check_no "The current updater/installer won't install a version. Check our website ${homepage} for a new version."
${LangFileString} update_check_failed "The check for a new version failed. Check our website ${homepage} for a new version and manually download a new version."
${LangFileString} update_download_success "The download of the new version $online_version succeeded.$\n$\r$\n$\rFinish the update by closing this updater. The new installation will be started right after."
${LangFileString} update_download_failed "The download of the new version $online_version failed. The installer could not be downloaded.$\n$\r$\n$\rPlease, visit our website ${homepage} for the new version."
${LangFileString} update_download_aborted "The download of the new version $online_version was aborted. Nothing will be updated. Remember, visit our website ${homepage} for latest news and updates."
${LangFileString} update_download_invalid_installer "The download of the new version $online_version failed. The downloaded installer was invalid. This can happen if the server/website has some issues, does not exist anymore or is in maintenance mode.$\n$\r$\n$\rPlease, visit our website ${homepage} and download the installer manually."
${LangFileString} update_download_none "No version to download selected. The current installer cannot update your version. Check our website ${homepage} for latest news and updates."
${LangFileString} update_versions_info "At least one new version of ${name} has been found. Please, select a specific version and choose to update. This version will be downloaded and the installation will be started afterwards."
${LangFileString} update_versions_none "None"

${LangFileString} update_download_downloading "Downloading %s "
${LangFileString} update_download_connecting "Connecting ... "
${LangFileString} update_download_sec "sec"
${LangFileString} update_download_min "min"
${LangFileString} update_download_hour "hour"
${LangFileString} update_download_multi "s"
${LangFileString} update_download_progress "%dkiB (%d%%) of %dkiB @ %d.%02dkiB/s"
${LangFileString} update_download_remaining " (%d %s%s remaining)"
${LangFileString} update_download_remain_sec " (1 second remaining)"
${LangFileString} update_download_remain_min " (1 minute remaining)"
${LangFileString} update_download_remain_hour " (1 hour remaining)"
${LangFileString} update_download_remain_secs " (%u seconds remaining)"
${LangFileString} update_download_remain_mins " (%u minutes remaining)"
${LangFileString} update_download_remain_hours " (%u hours remaining)"

; Welcome Page:

${LangFileString} page_welcome_title_update "Welcome to the ${name} Update Wizard"
${LangFileString} page_welcome_txt_update "This wizard will guide you through the Update process of ${name}. ${name} is a free open source Karaoke game, which can be compared with Singstar.$\n$\r$\n$\rThe ${publisher} wishes you fun.$\n$\r$\n$\rProject website:$\n$\r${homepage}$\n$\r$\n$\rSupport Forum:$\n$\r${forum}"
${LangFileString} page_welcome_title "Welcome to the ${name} Setup Wizard"
${LangFileString} page_welcome_txt "This wizard will guide you through the Installation of ${name}. ${name} is a free open source Karaoke game, which can be compared with Singstar.$\n$\r$\n$\rThe ${publisher} wishes you fun.$\n$\rProject website: ${homepage}$\n$\rSupport Forum: ${forum}"
${LangFileString} page_un_welcome_title "Welcome to the ${name} uninstall wizard"

; Components Page:

${LangFileString} page_components_info "Hover the component to get details"

; Custom Page

${LangFileString} page_settings_fullscreen "Fullscreen Mode"
${LangFileString} page_settings_subtitle "Specify your favorite settings for ${name}."

; Finish Page:

${LangFileString} page_finish_txt "${name} was installed successfully on your system.$\n$\r$\n$\rVisit our project website to get latest news and updates."
${LangFileString} page_finish_linktxt "Project website"
${LangFileString} page_finish_desktop "Create Desktop Shortcut?"

;unused
${LangFileString} page_finish_txt_update "${name} Update has checked for a new version."


; Start Menu and Shortcuts

${LangFileString} sm_shortcut "Play ${name}"
${LangFileString} sm_uninstall "Uninstall"
${LangFileString} sm_website "Website"
${LangFileString} sm_license "License"
${LangFileString} sm_readme "Readme"
${LangFileString} sm_songs "Songs"
${LangFileString} sm_update "Update"
${LangFileString} sm_documentation "Documentation"

${LangFileString} sc_play "Play"
${LangFileString} sc_desktop "Create Desktop Shortcut"

; Sections and SectionGroups

${LangFileString} name_section1 "Main components"
${LangFileString} name_section2 "Songs"
${LangFileString} name_s2_sub1 "Jonathan Coulton"
${LangFileString} name_s2_sub2 "Shearer"
${LangFileString} name_s2_sub3 "Wise Guys"
${LangFileString} name_s2_sub4 "Pornophonique"

${LangFileString} DESC_Section1 "These are the basic files needed by ${name}"
${LangFileString} DESC_Section2 "You can choose which songs should be downloaded (Requires Internet Connection!)"
${LangFileString} DESC_Section2_sub1 "You can choose which Jonathan Coulton songs (CC by-nc 3.0) should be installed."
${LangFileString} DESC_Section2_sub2 "You can choose which Shearer songs (CC by-nc-sa 2.0 / 3.0) should be installed."
${LangFileString} DESC_Section2_sub3 "You can choose which Wise Guys songs should be installed."
${LangFileString} DESC_Section2_sub4 "You can choose which Pornophonique songs should be installed."

${LangFileString} DESC_g2Section2 "Download the song 'Dead Smiling Pirates - I 18' (CC by-nc-nd 2.5)."
${LangFileString} DESC_g2Section3 "Download the song 'Joshua Morin - On The Run' (CC by-sa 2.5)."
${LangFileString} DESC_g2Section4 "Download the song 'Pornophonique - Space Invaders' (CC by-nc-nd 2.0)."
${LangFileString} DESC_g2Section5 "Download the song 'Steven Dunston - Northern Star' (CC by-nc-sa 2.5)."
${LangFileString} DESC_g2Section1 "Download the song 'Bodo Wartke - Liebeslied (Love Song)'."
${LangFileString} DESC_g2Section6 "Download the song 'Pornophonique - Space Invaders (Karaoke)' (CC by-nc-nd 2.0)."

${LangFileString} DESC_s2_sub1_Section1 "Download the song 'Monkey Shines'."
${LangFileString} DESC_s2_sub1_Section2 "Download the song 'I Crush Everything'."
${LangFileString} DESC_s2_sub1_Section3 "Download the song 'Not About You'."
${LangFileString} DESC_s2_sub1_Section4 "Download the song 'Mr. Fancy Pants'."
${LangFileString} DESC_s2_sub1_Section5 "Download the song 'Big Bad World One'."
${LangFileString} DESC_s2_sub1_Section6 "Download the song 'Flickr [incl. video]'."
${LangFileString} DESC_s2_sub1_Section7 "Download the song 'My Beige Bear'."
${LangFileString} DESC_s2_sub1_Section8 "Download the song 'The Future Soon'."
${LangFileString} DESC_s2_sub1_Section9 "Download the song 'Ikea'."
${LangFileString} DESC_s2_sub1_Section10 "Download the song 'Furry Old Lobster'."
${LangFileString} DESC_s2_sub1_Section11 "Download the song 'Code Monkey [incl. video]'."
${LangFileString} DESC_s2_sub1_Section12 "Download the song 'I´m Your Moon'."
${LangFileString} DESC_s2_sub1_Section13 "Download the song 'First Of May'."
${LangFileString} DESC_s2_sub1_Section14 "Download the song 'Dance, Soterios Johnson, Dance'."
${LangFileString} DESC_s2_sub1_Section15 "Download the song 'A Talk With George'."
${LangFileString} DESC_s2_sub1_Section16 "Download the song 'Creepy Doll [incl. video]'."
${LangFileString} DESC_s2_sub1_Section17 "Download the song 'That Spells DNA'."
${LangFileString} DESC_s2_sub1_Section18 "Download the song 'When You Go'."
${LangFileString} DESC_s2_sub1_Section19 "Download the song 'Better'."
${LangFileString} DESC_s2_sub1_Section20 "Download the song 'Shop Vac'."
${LangFileString} DESC_s2_sub1_Section21 "Download the song 'I Feel Fantastic'."
${LangFileString} DESC_s2_sub1_Section22 "Download the song 'Re: Your Brains'."
${LangFileString} DESC_s2_sub1_Section23 "Download the song 'Skullcrusher Mountain'."
${LangFileString} DESC_s2_sub1_Section24 "Download the song 'Chiron Beta Prime [incl. video]'."

${LangFileString} DESC_s2_sub2_Section1 "Download the song '69'."
${LangFileString} DESC_s2_sub2_Section2 "Download the song '69 (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section3 "Download the song 'Can't stop it'."
${LangFileString} DESC_s2_sub2_Section4 "Download the song 'Can't stop it (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section5 "Download the song 'In My Hand'."
${LangFileString} DESC_s2_sub2_Section6 "Download the song 'Man Song'."
${LangFileString} DESC_s2_sub2_Section7 "Download the song 'Man Song (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section8 "Download the song 'Stay with me'."
${LangFileString} DESC_s2_sub2_Section9 "Download the song 'Stay with me (Karaoke)'."

${LangFileString} DESC_s2_sub3_Section1 "Download the song 'Lebendig und kräftig und schärfer'."
${LangFileString} DESC_s2_sub3_Section2 "Download the song 'Mensch, wo bist du?'."
${LangFileString} DESC_s2_sub3_Section3 "Download the song 'Mensch, wo bist du? (Karaoke)'."

; Optional Themes
; (not available)