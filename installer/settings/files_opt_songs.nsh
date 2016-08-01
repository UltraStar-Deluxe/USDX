; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: Download optional songs
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

!macro DownloadSong section Url size Title FileName SubPath
	Section /o "${title}" ${section}

		AddSize ${size}
		SetOverwrite try
		SetOutPath "$INSTDIR\songs\"

		; Download song:
		NSISdl::download /TIMEOUT=50000 ${Url} "$LOCALAPPDATA\Temp\${FileName}"

		Pop $R0
		StrCmp $R0 "success" dlok
			MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
		dlok:
		ZipDLL::extractall "$LOCALAPPDATA\Temp\${FileName}" "$INSTDIR\songs\${SubPath}"

	  Delete "$LOCALAPPDATA\Temp\${FileName}"

	  SetOutPath "$INSTDIR"

	SectionEnd
!macroend

SectionGroup $(name_section2) Section2

;-----------------------------------------------------------------------
; BODO WARTKE
;-----------------------------------------------------------------------
!insertmacro DownloadSong g2Section1 ${download_song1} 10343 "Bodo Wartke - Liebeslied (Love Song)" "Song-BodoWartke-LoveSong.zip" ""

;-----------------------------------------------------------------------
; Dead Smiling Pirates - I 18
;-----------------------------------------------------------------------
!insertmacro DownloadSong g2Section2 ${download_song2} 2816 "Dead Smiling Pirates - I 18" "Song-I-18.zip" "Dead Smiling Pirates - I 18\"

;-----------------------------------------------------------------------
; Jonathan Coulton Songs
;-----------------------------------------------------------------------

SectionGroup $(name_s2_sub1) s2_sub1

!insertmacro DownloadSong s2_sub1_Section1 ${download_sub1_song1} 1455 "Monkey Shines" "Song-JC-MS.zip" ""
!insertmacro DownloadSong s2_sub1_Section2 ${download_sub1_song2} 7127 "I Crush Everything" "Song-JC-ICE.zip" ""
!insertmacro DownloadSong s2_sub1_Section3 ${download_sub1_song3} 3492 "Not About You" "Song-JC-NAY.zip" ""
!insertmacro DownloadSong s2_sub1_Section4 ${download_sub1_song4} 2427 "Mr. Fancy Pants" "Song-JC-MFP.zip" ""
!insertmacro DownloadSong s2_sub1_Section5 ${download_sub1_song5} 4424 "Big Bad World One" "Song-JC-BBWO.zip" ""
!insertmacro DownloadSong s2_sub1_Section6 ${download_sub1_song6} 21607 "Flickr" "Song-JC-Flickr.zip" ""
!insertmacro DownloadSong s2_sub1_Section7 ${download_sub1_song7} 4926 "My Beige Bear" "Song-JC-MBB.zip" ""
!insertmacro DownloadSong s2_sub1_Section8 ${download_sub1_song8} 5612 "The Future Soon" "Song-JC-TFS.zip" ""
!insertmacro DownloadSong s2_sub1_Section9 ${download_sub1_song9} 4608 "Ikea" "Song-JC-Ikea.zip" ""
!insertmacro DownloadSong s2_sub1_Section10 ${download_sub1_song10} 3288 "Furry Old Lobster" "Song-JC-FOL.zip" ""
!insertmacro DownloadSong s2_sub1_Section11 ${download_sub1_song11} 21402 "Code Monkey" "Song-JC-CM.zip" ""
!insertmacro DownloadSong s2_sub1_Section12 ${download_sub1_song12} 4916 "I´m Your Moon" "Song-JC-IYM.zip" ""
!insertmacro DownloadSong s2_sub1_Section13 ${download_sub1_song13} 6257 "First Of May" "Song-JC-FOM.zip" ""
!insertmacro DownloadSong s2_sub1_Section14 ${download_sub1_song14} 5929 "Dance, Soterious Johnson, Dance" "Song-JC-DSJD.zip" ""
!insertmacro DownloadSong s2_sub1_Section15 ${download_sub1_song15} 4076 "A Talk With George" "Song-JC-ATWG.zip" ""
!insertmacro DownloadSong s2_sub1_Section16 ${download_sub1_song16} 66560 "Creepy Doll" "Song-JC-CD.zip" ""
!insertmacro DownloadSong s2_sub1_Section17 ${download_sub1_song17} 4158 "That Spells DNA" "Song-JC-TSDNA.zip" ""
!insertmacro DownloadSong s2_sub1_Section18 ${download_sub1_song18} 5755 "When You Go" "Song-JC-WYG.zip" ""
!insertmacro DownloadSong s2_sub1_Section19 ${download_sub1_song19} 4199 "Better" "Song-JC-Better.zip" ""
!insertmacro DownloadSong s2_sub1_Section20 ${download_sub1_song20} 5448 "Shop Vac" "Song-JC-SV.zip" ""
!insertmacro DownloadSong s2_sub1_Section21 ${download_sub1_song21} 3851 "I Feel Fantastic" "Song-JC-IFF.zip" ""
!insertmacro DownloadSong s2_sub1_Section22 ${download_sub1_song22} 7087 "Re: Your Brains" "Song-JC-ReYB.zip" ""
!insertmacro DownloadSong s2_sub1_Section23 ${download_sub1_song23} 6298 "Skullcrusher Mountain" "Song-JC-SCM.zip" ""
!insertmacro DownloadSong s2_sub1_Section24 ${download_sub1_song24} 38298 "Chiron Beta Prime" "Song-JC-CBP.zip" ""

SectionGroupEnd

;-----------------------------------------------------------------------
; Joshua Morin - On The Run
;-----------------------------------------------------------------------
!insertmacro DownloadSong g2Section3 ${download_song3} 3881 "Joshua Morin - On The Run" "Song-On-the-run.zip" "Joshua Morin - On The Run\"

;-----------------------------------------------------------------------
; Pornophonique
;-----------------------------------------------------------------------
SectionGroup $(name_s2_sub4) s2_sub4

!insertmacro DownloadSong g2Section4 ${download_song4} 3646 "Pornophonique - Space Invaders" "Song-Space-Invaders.zip" "Pornophonique - Space Invaders\"
!insertmacro DownloadSong g2Section6 ${download_song6} 3779 "Pornophonique - Space Invaders (Karaoke)" "Song-SpaceInvadersKAR.zip" ""

SectionGroupEnd
;-----------------------------------------------------------------------
; Shearer
;-----------------------------------------------------------------------
SectionGroup $(name_s2_sub2) s2_sub2

!insertmacro DownloadSong s2_sub2_Section1 ${download_sub2_song1} 4557 "Shearer - 69" "Song-Shearer-69.zip" ""
!insertmacro DownloadSong s2_sub2_Section2 ${download_sub2_song2} 4772 "Shearer - 69 (Karaoke)" "Song-Shearer-69-Kar.zip" ""
!insertmacro DownloadSong s2_sub2_Section3 ${download_sub2_song3} 5510 "Shearer - Can't stop it" "Song-Shearer-CSI.zip" ""
!insertmacro DownloadSong s2_sub2_Section4 ${download_sub2_song4} 4178 "Shearer - Can't stop it (Karaoke)" "Song-Shearer-CSI-Kar.zip" ""
!insertmacro DownloadSong s2_sub2_Section10 ${download_sub2_song10} 6508 "Shearer - Consequence of Dawn" "Song-Shearer-SWM-COD.zip" ""
!insertmacro DownloadSong s2_sub2_Section5 ${download_sub2_song5} 5960 "Shearer - In My Hand" "Song-Shearer-IMH.zip" ""
!insertmacro DownloadSong s2_sub2_Section6 ${download_sub2_song6} 7270 "Shearer - Man Song" "Song-Shearer-MS.zip" ""
!insertmacro DownloadSong s2_sub2_Section7 ${download_sub2_song7} 5807 "Shearer - Man Song (Karaoke)" "Song-Shearer-MS-Kar.zip" ""
!insertmacro DownloadSong s2_sub2_Section8 ${download_sub2_song8} 6400 "Shearer - Stay With Me" "Song-Shearer-SWM.zip" ""
!insertmacro DownloadSong s2_sub2_Section9 ${download_sub2_song9} 5417 "Shearer - Stay With Me (Karaoke)" "Song-Shearer-SWM-Kar.zip" ""

SectionGroupEnd

;-----------------------------------------------------------------------
; Steven Dunston - Northern Star
;-----------------------------------------------------------------------
!insertmacro DownloadSong g2Section5 ${download_song5} 2427 "Steven Dunston - Northern Star" "Song-Northern-Star.zip" "Steven Dunston - Northern Star\"

;-----------------------------------------------------------------------
; Wise Guys
;-----------------------------------------------------------------------
SectionGroup $(name_s2_sub3) s2_sub3

!insertmacro DownloadSong s2_sub3_Section1 ${download_sub3_song1} 4015 "Wise Guys - Lebendig und kräftig und schärfer" "Song-WiseGuys-LUKUS.zip" ""
!insertmacro DownloadSong s2_sub3_Section2 ${download_sub3_song2} 5335 "Wise Guys - Mensch, wo bist du?" "Song-WiseGuys-MWBD.zip" ""
!insertmacro DownloadSong s2_sub3_Section3 ${download_sub3_song3} 5335 "Wise Guys - Mensch, wo bist du? (Karaoke)" "Song-WiseGuys-MWBDKar.zip" ""

SectionGroupEnd

SectionGroupEnd