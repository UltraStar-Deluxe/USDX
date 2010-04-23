; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~
; UltraStar Deluxe Installer: Download optional songs
; ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~ ~+~

SectionGroup $(name_section2) Section2

;-----------------------------------------------------------------------
; BODO WARTKE
;-----------------------------------------------------------------------
Section /o "Bodo Wartke - Liebeslied (Love Song)" g2Section1

   AddSize 10343
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_song1} $LOCALAPPDATA\Temp\Song-BodoWartke-LoveSong.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-BodoWartke-LoveSong.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-BodoWartke-LoveSong.zip"

  SetOutPath "$INSTDIR"

SectionEnd

;-----------------------------------------------------------------------
; Dead Smiling Pirates - I 18
;-----------------------------------------------------------------------

Section /o "Dead Smiling Pirates - I 18" g2Section2
   AddSize 2816
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\songs\Dead Smiling Pirates - I 18"
   SetOutPath "$INSTDIR\songs\Dead Smiling Pirates - I 18\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_song2} $LOCALAPPDATA\Temp\Song-I-18.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-I-18.zip" "$INSTDIR\songs\Dead Smiling Pirates - I 18\"

  Delete "$LOCALAPPDATA\Temp\Song-I-18.zip"

  SetOutPath "$INSTDIR"

SectionEnd

;-----------------------------------------------------------------------
; Jonathan Coulton Songs
;-----------------------------------------------------------------------

SectionGroup $(name_s2_sub1) s2_sub1

Section /o "A Talk With George" s2_sub1_Section15

   AddSize 4076
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song15} $LOCALAPPDATA\Temp\Song-JC-ATWG.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-ATWG.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-ATWG.zip"

  SetOutPath "$INSTDIR"
SectionEnd
;-----------------------------------------------------------------------
Section /o "Better" s2_sub1_Section19

   AddSize 4199
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song19} $LOCALAPPDATA\Temp\Song-JC-Better.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-Better.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-Better.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Monkey Shines" s2_sub1_Section1

   AddSize 1455
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song1} $LOCALAPPDATA\Temp\Song-JC-MS.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-MS.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-MS.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "I Crush Everything" s2_sub1_Section2

   AddSize 7127
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song2} $LOCALAPPDATA\Temp\Song-JC-ICE.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-ICE.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-ICE.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Not About You" s2_sub1_Section3

   AddSize 3492
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song3} $LOCALAPPDATA\Temp\Song-JC-NAY.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-NAY.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-NAY.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Mr. Fancy Pants" s2_sub1_Section4

   AddSize 2427
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song4} $LOCALAPPDATA\Temp\Song-JC-MFP.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-MFP.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-MFP.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Big Bad World One" s2_sub1_Section5

   AddSize 4424
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song5} $LOCALAPPDATA\Temp\Song-JC-BBWO.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-BBWO.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-BBWO.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Flickr" s2_sub1_Section6

   AddSize 21607
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song6} $LOCALAPPDATA\Temp\Song-JC-Flickr.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-Flickr.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-Flickr.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "My Beige Bear" s2_sub1_Section7

   AddSize 4926
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song7} $LOCALAPPDATA\Temp\Song-JC-MBB.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-MBB.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-MBB.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "The Future Soon" s2_sub1_Section8

   AddSize 5612
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song8} $LOCALAPPDATA\Temp\Song-JC-TFS.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-TFS.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-TFS.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Ikea" s2_sub1_Section9

   AddSize 4608
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song9} $LOCALAPPDATA\Temp\Song-JC-Ikea.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-Ikea.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-Ikea.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Furry Old Lobster" s2_sub1_Section10

   AddSize 3288
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song10} $LOCALAPPDATA\Temp\Song-JC-FOL.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-FOL.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-FOL.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Code Monkey" s2_sub1_Section11

   AddSize 21402
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song11} $LOCALAPPDATA\Temp\Song-JC-CM.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-CM.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-CM.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "I´m Your Moon" s2_sub1_Section12

   AddSize 4916
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song12} $LOCALAPPDATA\Temp\Song-JC-IYM.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-IYM.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-IYM.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "First Of May" s2_sub1_Section13

   AddSize 6257
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song13} $LOCALAPPDATA\Temp\Song-JC-FOM.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-FOM.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-FOM.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Dance, Soterious Johnson, Dance" s2_sub1_Section14

   AddSize 5929
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song14} $LOCALAPPDATA\Temp\Song-JC-DSJD.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-DSJD.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-DSJD.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Creepy Doll" s2_sub1_Section16

   AddSize 66560
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song16} $LOCALAPPDATA\Temp\Song-JC-CD.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-CD.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-CD.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "That Spells DNA" s2_sub1_Section17

   AddSize 4158
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song17} $LOCALAPPDATA\Temp\Song-JC-TSDNA.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-TSDNA.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-TSDNA.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "When You Go" s2_sub1_Section18

   AddSize 5755
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song18} $LOCALAPPDATA\Temp\Song-JC-WYG.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-WYG.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-WYG.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Shop Vac" s2_sub1_Section20

   AddSize 5448
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song20} $LOCALAPPDATA\Temp\Song-JC-SV.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-SV.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-SV.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "I Feel Fantastic" s2_sub1_Section21

   AddSize 3851
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song21} $LOCALAPPDATA\Temp\Song-JC-IFF.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-IFF.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-IFF.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Re: Your Brains" s2_sub1_Section22

   AddSize 7087
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song22} $LOCALAPPDATA\Temp\Song-JC-ReYB.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-ReYB.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-ReYB.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Skullcrusher Mountain" s2_sub1_Section23

   AddSize 6298
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song23} $LOCALAPPDATA\Temp\Song-JC-SCM.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-SCM.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-SCM.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Chiron Beta Prime" s2_sub1_Section24

   AddSize 38298
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub1_song24} $LOCALAPPDATA\Temp\Song-JC-CBP.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-JC-CBP.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-JC-CBP.zip"

  SetOutPath "$INSTDIR"

SectionEnd

SectionGroupEnd
;-----------------------------------------------------------------------
; Joshua Morin - On The Run
;-----------------------------------------------------------------------
Section /o "Joshua Morin - On The Run" g2Section3
   AddSize 3881
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\songs\Joshua Morin - On The Run"
   SetOutPath "$INSTDIR\songs\Joshua Morin - On The Run\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_song3} $LOCALAPPDATA\Temp\Song-On-the-run.zip

  Pop $R0 ;Get the return value
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-On-the-run.zip" "$INSTDIR\songs\Joshua Morin - On The Run\"

  Delete "$LOCALAPPDATA\Temp\Song-On-the-run.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
; Pornophonique
;-----------------------------------------------------------------------
SectionGroup $(name_s2_sub4) s2_sub4

Section /o "Pornophonique - Space Invaders" g2Section4
   AddSize 3646
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\songs\Pornophonique - Space Invaders"
   SetOutPath "$INSTDIR\songs\Pornophonique - Space Invaders\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_song4} $LOCALAPPDATA\Temp\Song-Space-Invaders.zip

  Pop $R0 ;Get the return value
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Space-Invaders.zip" "$INSTDIR\songs\Pornophonique - Space Invaders\"

  Delete "$LOCALAPPDATA\Temp\Song-Space-Invaders.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Pornophonique - Space Invaders (Karaoke)" g2Section6
   AddSize 3779
   SetOverwrite try
   SetOutPath "$INSTDIR\songs"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_song6} $LOCALAPPDATA\Temp\Song-SpaceInvadersKAR.zip

  Pop $R0 ;Get the return value
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-SpaceInvadersKAR.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-SpaceInvadersKAR.zip"

  SetOutPath "$INSTDIR"

SectionEnd

SectionGroupEnd
;-----------------------------------------------------------------------
; Shearer
;-----------------------------------------------------------------------
SectionGroup $(name_s2_sub2) s2_sub2

Section /o "Shearer - 69" s2_sub2_Section1

   AddSize 4557
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song1} $LOCALAPPDATA\Temp\Song-Shearer-69.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-69.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-69.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Shearer - 69 (Karaoke)" s2_sub2_Section2

   AddSize 4772
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song2} $LOCALAPPDATA\Temp\Song-Shearer-69-Kar.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-69-Kar.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-69-Kar.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Shearer - Can't stop it" s2_sub2_Section3

   AddSize 5510
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song3} $LOCALAPPDATA\Temp\Song-Shearer-CSI.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-CSI.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-CSI.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Shearer - Can't stop it (Karaoke)" s2_sub2_Section4

   AddSize 4178
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song4} $LOCALAPPDATA\Temp\Song-Shearer-CSI-Kar.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-CSI-Kar.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-CSI-Kar.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Shearer - In My Hand" s2_sub2_Section5

   AddSize 5960
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song5} $LOCALAPPDATA\Temp\Song-Shearer-IMH.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-IMH.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-IMH.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Shearer - Man Song" s2_sub2_Section6

   AddSize 7270
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song6} $LOCALAPPDATA\Temp\Song-Shearer-MS.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-MS.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-MS.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Shearer - Man Song (Karaoke)" s2_sub2_Section7

   AddSize 5807
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song7} $LOCALAPPDATA\Temp\Song-Shearer-MS-Kar.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-MS-Kar.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-MS-Kar.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Shearer - Stay With Me" s2_sub2_Section8

   AddSize 6400
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song8} $LOCALAPPDATA\Temp\Song-Shearer-SWM.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:
  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-SWM.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-SWM.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Shearer - Stay With Me (Karaoke)" s2_sub2_Section9

   AddSize 5417
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub2_song9} $LOCALAPPDATA\Temp\Song-Shearer-SWM-Kar.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:

  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Shearer-SWM-Kar.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-Shearer-SWM-Kar.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
SectionGroupEnd

Section /o "Steven Dunston - Northern Star" g2Section5
   AddSize 2427
   SetOverwrite try
   SetOutPath "$INSTDIR"
   CreateDirectory "$INSTDIR\songs\Steven Dunston - Northern Star"
   SetOutPath "$INSTDIR\songs\Steven Dunston - Northern Star\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_song5} $LOCALAPPDATA\Temp\Song-Northern-Star.zip

  Pop $R0 ;Get the return value
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:

  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-Northern-Star.zip" "$INSTDIR\songs\Steven Dunston - Northern Star\"

  Delete "$LOCALAPPDATA\Temp\Song-Northern-Star.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
; Wise Guys
;-----------------------------------------------------------------------
SectionGroup $(name_s2_sub3) s2_sub3

Section /o "Wise Guys - Lebendig und kräftig und schärfer" s2_sub3_Section1

   AddSize 4015
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub3_song1} $LOCALAPPDATA\Temp\Song-WiseGuys-LUKUS.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:

  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-WiseGuys-LUKUS.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-WiseGuys-LUKUS.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Wise Guys - Mensch, wo bist du?" s2_sub3_Section2

   AddSize 5335
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub3_song2} $LOCALAPPDATA\Temp\Song-WiseGuys-MWBD.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:

  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-WiseGuys-MWBD.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-WiseGuys-MWBD.zip"

  SetOutPath "$INSTDIR"

SectionEnd
;-----------------------------------------------------------------------
Section /o "Wise Guys - Mensch, wo bist du? (Karaoke)" s2_sub3_Section3

   AddSize 5335
   SetOverwrite try
   SetOutPath "$INSTDIR\songs\"

; Download song:
  NSISdl::download /TIMEOUT=50000 ${download_sub3_song3} $LOCALAPPDATA\Temp\Song-WiseGuys-MWBDKar.zip
 
  Pop $R0
    StrCmp $R0 "success" dlok
      MessageBox MB_OK|MB_ICONEXCLAMATION "Download Error, click OK to Continue" /SD IDOK
  dlok:

  ZipDLL::extractall "$LOCALAPPDATA\Temp\Song-WiseGuys-MWBDKar.zip" "$INSTDIR\songs\"

  Delete "$LOCALAPPDATA\Temp\Song-WiseGuys-MWBDKar.zip"

  SetOutPath "$INSTDIR"

SectionEnd

SectionGroupEnd

SectionGroupEnd