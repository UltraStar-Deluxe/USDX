; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer - Language file: Portuguese
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!insertmacro LANGFILE_EXT Portuguese

${LangFileString} abort_install "Tem a certeza que deseja cancelar a instalação?"
${LangFileString} abort_uninstall "Tem a certeza que deseja cancelar a desinstalação?"
${LangFileString} oninit_running "O instalador já está em execução."
${LangFileString} oninit_installagain "Tem certeza de que deseja instalá-lo novamente?"
${LangFileString} oninit_alreadyinstalled "já está instalado"
${LangFileString} oninit_closeusdx "não pode ser desinstalado enquanto está a correr! Deseja fechá-lo?"
${LangFileString} oninit_updateusdx "Você quer atualizar a instalação de:"
${LangFileString} oninit_uninstall "Você quer desinstalar a versão antiga? (recomendado)"

${LangFileString} update_connect "Estabelecer conexão com a internet e verifique se há nova versão"
${LangFileString} button_next "Seguinte >"
${LangFileString} button_close "Fechar"
${LangFileString} update_information "Você pode verificar se uma nova versão do '${name}' está disponível. Uma conexão à internet será estabelecida. Se uma nova versão for encontrada, poderá ser instalada depois."

${LangFileString} delete_components "Além disso, exclua os seguintes componentes:"
${LangFileString} delete_covers "Capas?"
${LangFileString} delete_highscores "Pontuações?"
${LangFileString} delete_config "Configurações?"
${LangFileString} delete_screenshots "Screenshots?"
${LangFileString} delete_playlists "Playlists?"
${LangFileString} delete_songs "Remover canções? ATENÇÃO: TODOS os arquivos dentro da pasta InstallationDir\songs serão removidos(!)"

${LangFileString} update_check_older "A sua versão $installed_version está obsoleta. Uma nova versão (online_version) do ${name} está disponível. Deseja atualizar?"
${LangFileString} update_check_equal "A sua versão atualmente instalada $installed_version está atualizada."
${LangFileString} update_check_newer "A sua versão instalada $installed_version é mais recente que a $\n$\rversão corrente online_version do ${name}. Não necessita de atualização."
${LangFileString} update_check_failed "A verificação de uma nova versão falhou. Você quer visitar o site para verificar manualmente?"

; Welcome Page:

${LangFileString} page_welcome_title_update "Bem-vindo ao assistente de actualização do ${name}"
${LangFileString} page_welcome_txt_update "Este assistente irá guiá-lo através do processo de atualização do ${name}. ${name} é um jogo de Karaoke livre de código aberto, que pode ser comparado com o Singstar.$\n$\r$\n$\rA equipa do ${name} deseja-lhe que se divirta.$\n$\r$\n$\rWebsite do projecto: http://www.ultrastardeluxe.org$\n$\rForúm Suporte: http://forum.ultrastardeluxe.org"
${LangFileString} page_welcome_title "Bem-vindo ao assistente de instalação do ${name}"
${LangFileString} page_welcome_txt "Este assistente irá guiá-lo através do processo de instalação do ${name}. ${name} é um jogo de Karaoke livre de código aberto, que pode ser comparado com o Singstar.$\n$\r$\n$\rA equipa do ${name} deseja-lhe que se divirta.$\n$\r$\n$\rWebsite do projecto: http://www.ultrastardeluxe.org$\n$\rForúm Suporte: http://forum.ultrastardeluxe.org"
${LangFileString} page_un_welcome_title "Bem-vindo ao assistente de desinstalação do ${name}"

; Components Page:

${LangFileString} page_components_info "Passe o componente para obter detalhes"

; Custom Page

${LangFileString} page_settings_subtitle "Especifique suas configurações favoritas para ${name}."
${LangFileString} page_settings_config_title "Configuração ${name} (opcional)"
${LangFileString} page_settings_config_info "Todas as opções podem ser posteriormente alteradas no jogo."
${LangFileString} page_settings_fullscreen_label "Ecrã Completo:"
${LangFileString} page_settings_fullscreen_info "Iniciar jogo em janela ou ecrã completo."
${LangFileString} page_settings_language_label "Idioma:"
${LangFileString} page_settings_language_info "Seleccione idioma."
${LangFileString} page_settings_resolution_label "Resolução:"
${LangFileString} page_settings_resolution_info "Escolha a resolução."
${LangFileString} page_settings_tabs_label "Subpastas:"
${LangFileString} page_settings_tabs_info "Pastas virtuais para mostrar canções."
${LangFileString} page_settings_sorting_label "Ordenação:"
${LangFileString} page_settings_sorting_info "Seleccione o critério de ordenação das canções."
${LangFileString} page_settings_songdir_label "Directoria das canções:"
${LangFileString} page_settings_songdir_info "Escolha diretório adicional para as canções do ${name}."

; Finish Page:

${LangFileString} page_finish_txt "${name} foi instalado com sucesso no seu sistema.$\n$\r$\n$\rVisite o site do projecto para receber as últimas notícias e actualizações."
${LangFileString} page_finish_linktxt "Website"
${LangFileString} page_finish_desktop "Criar atalho no Ambiente de Trabalho"

; Start Menu and Shortcuts

${LangFileString} sm_shortcut "${name}"
${LangFileString} sm_uninstall "Uninstall"
${LangFileString} sm_website "Website"
${LangFileString} sm_license "License"
${LangFileString} sm_readme "Readme"
${LangFileString} sm_songs "Canções"
${LangFileString} sm_documentation "Documentação"

${LangFileString} sc_play "Play"
${LangFileString} sc_desktop "Criar atalho no Ambiente de Trabalho"

; Sections and SectionGroups

${LangFileString} name_section1 "Arquivos principais"
${LangFileString} name_section2 "Canções"
${LangFileString} name_s2_sub1 "Jonathan Coulton"
${LangFileString} name_s2_sub2 "Shearer"
${LangFileString} name_s2_sub3 "Wise Guys"
${LangFileString} name_s2_sub4 "Pornophonique"

${LangFileString} DESC_Section1 "Estes são os arquivos básicos necessários para ${name}"
${LangFileString} DESC_Section2 "Você pode escolher quais as canções que devem ser baixadas (requer conexão à Internet!)"
${LangFileString} DESC_Section2_sub1 "Você pode escolher quais as canções de Jonathan Coulton (CC by-nc 3.0) devem ser instaladas."
${LangFileString} DESC_Section2_sub2 "Você pode escolher quais as canções de Shearer songs (CC by-nc-sa 2.0 / 3.0) devem ser instaladas."
${LangFileString} DESC_Section2_sub3 "Você pode escolher quais as canções de Wise Guys devem ser instaladas."
${LangFileString} DESC_Section2_sub4 "Você pode escolher quais as canções de Pornophonique devem ser instaladas."

${LangFileString} DESC_g2Section2 "Baixar 'Dead Smiling Pirates - I 18' (CC by-nc-nd 2.5)."
${LangFileString} DESC_g2Section3 "Baixar 'Joshua Morin - On The Run' (CC by-sa 2.5)."
${LangFileString} DESC_g2Section4 "Baixar 'Pornophonique - Space Invaders' (CC by-nc-nd 2.0)."
${LangFileString} DESC_g2Section5 "Baixar 'Steven Dunston - Northern Star' (CC by-nc-sa 2.5)."
${LangFileString} DESC_g2Section1 "Baixar 'Bodo Wartke - Liebeslied (Love Song)'."
${LangFileString} DESC_g2Section6 "Baixar 'Pornophonique - Space Invaders (Karaoke)' (CC by-nc-nd 2.0)."

${LangFileString} DESC_s2_sub1_Section1 "Baixar 'Monkey Shines'."
${LangFileString} DESC_s2_sub1_Section2 "Baixar 'I Crush Everything'."
${LangFileString} DESC_s2_sub1_Section3 "Baixar 'Not About You'."
${LangFileString} DESC_s2_sub1_Section4 "Baixar 'Mr. Fancy Pants'."
${LangFileString} DESC_s2_sub1_Section5 "Baixar 'Big Bad World One'."
${LangFileString} DESC_s2_sub1_Section6 "Baixar 'Flickr [incl. video]'."
${LangFileString} DESC_s2_sub1_Section7 "Baixar 'My Beige Bear'."
${LangFileString} DESC_s2_sub1_Section8 "Baixar 'The Future Soon'."
${LangFileString} DESC_s2_sub1_Section9 "Baixar 'Ikea'."
${LangFileString} DESC_s2_sub1_Section10 "Baixar 'Furry Old Lobster'."
${LangFileString} DESC_s2_sub1_Section11 "Baixar 'Code Monkey [incl. video]'."
${LangFileString} DESC_s2_sub1_Section12 "Baixar 'I´m Your Moon'."
${LangFileString} DESC_s2_sub1_Section13 "Baixar 'First Of May'."
${LangFileString} DESC_s2_sub1_Section14 "Baixar 'Dance, Soterios Johnson, Dance'."
${LangFileString} DESC_s2_sub1_Section15 "Baixar 'A Talk With George'."
${LangFileString} DESC_s2_sub1_Section16 "Baixar 'Creepy Doll [incl. video]'."
${LangFileString} DESC_s2_sub1_Section17 "Baixar 'That Spells DNA'."
${LangFileString} DESC_s2_sub1_Section18 "Baixar 'When You Go'."
${LangFileString} DESC_s2_sub1_Section19 "Baixar 'Better'."
${LangFileString} DESC_s2_sub1_Section20 "Baixar 'Shop Vac'."
${LangFileString} DESC_s2_sub1_Section21 "Baixar 'I Feel Fantastic'."
${LangFileString} DESC_s2_sub1_Section22 "Baixar 'Re: Your Brains'."
${LangFileString} DESC_s2_sub1_Section23 "Baixar 'Skullcrusher Mountain'."
${LangFileString} DESC_s2_sub1_Section24 "Baixar 'Chiron Beta Prime [incl. video]'."

${LangFileString} DESC_s2_sub2_Section1 "Baixar '69'."
${LangFileString} DESC_s2_sub2_Section2 "Baixar '69 (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section3 "Baixar 'Can't stop it'."
${LangFileString} DESC_s2_sub2_Section4 "Baixar 'Can't stop it (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section5 "Baixar 'In My Hand'."
${LangFileString} DESC_s2_sub2_Section6 "Baixar 'Man Song'."
${LangFileString} DESC_s2_sub2_Section7 "Baixar 'Man Song (Karaoke)'."
${LangFileString} DESC_s2_sub2_Section8 "Baixar 'Stay with me'."
${LangFileString} DESC_s2_sub2_Section9 "Baixar 'Stay with me (Karaoke)'."

${LangFileString} DESC_s2_sub3_Section1 "Baixar 'Lebendig und kräftig und schärfer'."
${LangFileString} DESC_s2_sub3_Section2 "Baixar 'Mensch, wo bist du?'."
${LangFileString} DESC_s2_sub3_Section3 "Baixar 'Mensch, wo bist du? (Karaoke)'."

; Optional Themes
; (not available)