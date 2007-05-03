To translate USD to a new language, take the English Language File, or another one that is up to date and Edit the Texts behind the Equal Mark(=).

To port a LanguageFile from Ultrastar 0.5.2 or Higher add the following Texts to the end of the file:

#Main Screen
SING_MENU=Main Menu

SING_MULTI=party
SING_MULTI_DESC=Sing in PartyMode

SING_TOOLS=Tools

SING_STATS=stats
SING_STATS_DESC=View the Statistics

#Advanced Options Screen
SING_OPTIONS_ADVANCED=advanced
SING_OPTIONS_ADVANCED_DESC=advanced options
SING_OPTIONS_ADVANCED_EFFECTSING=Singscreen effects
SING_OPTIONS_ADVANCED_SCREENFADE=Screen Fading
SING_OPTIONS_ADVANCED_LOADANIMATION=Load Animation
SING_OPTIONS_ADVANCED_ASKBEFOREDEL=Savety Questions
SING_OPTIONS_ADVANCED_LINEBONUS=Line Bonus
SING_OPTIONS_ADVANCED_ONSONGCLICK=after SongSelection

#Ratings at the Score Screen
SING_SCORE_TONE_DEAF=Tone Deaf
SING_SCORE_AMATEUR=Amateur
SING_SCORE_RISING_STAR=Rising Star
SING_SCORE_LEAD_SINGER=Lead Singer
SING_SCORE_HIT_ARTIST=Hit Artist
SING_SCORE_SUPERSTAR=Superstar
SING_SCORE_ULTRASTAR=Ultrastar

#Line Bonus PopUps
LINEBONUS_PERFECT=Perfect!
LINEBONUS_BETTER=Cool!
LINEBONUS_GOOD=Good!
LINEBONUS_NORMAL=OK!
LINEBONUS_BAD=Bad!
LINEBONUS_WORST=Ghastly!

#To connect Strigns with, e.g.: He, you and I
IMPLODE_GLUE1=, 
IMPLODE_GLUE2= and 

#Text for the Legend Bar at the bottom
SING_LEGEND_CONTINUE=Continue

#Texts of the Menu that appears when M is Pressed at the SongSelection
SONG_MENU_NAME_MAIN=Song Menu
SONG_MENU_PLAY=Sing
SONG_MENU_EDIT=Edit
SONG_MENU_MODI=Sing a Modi
SONG_MENU_CHANGEPLAYERS=Change Players
SONG_MENU_CANCEL=Cancel

#Playlist Menu
SONG_MENU_NAME_PLAYLIST=Song Menu
SONG_MENU_PLAYLIST_ADD=Add Song
SONG_MENU_PLAYLIST_DEL=Delete Song

SONG_MENU_NAME_PLAYLIST_ADD=Add Song
SONG_MENU_PLAYLIST_ADD_NEW=to new playlist
SONG_MENU_PLAYLIST_ADD_EXISTING=to exiting playlist
SONG_MENU_PLAYLIST_NOEXISTING=No playlist available

SONG_MENU_NAME_PLAYLIST_NEW=New Playlist
SONG_MENU_PLAYLIST_NEW_CREATE=Create
SONG_MENU_PLAYLIST_NEW_UNNAMED=Unnamed

SONG_MENU_NAME_PLAYLIST_DEL=Really Delete?
SONG_MENU_YES=Yes
SONG_MENU_NO=No

SONG_MENU_NAME_PLAYLIST_LOAD=Open Playlist
SONG_MENU_PLAYLIST_LOAD=open

#Menu Party Modus
SONG_MENU_NAME_PARTY=Menu
SONG_MENU_JOKER=Joker

SONG_MENU_NAME_PARTY_JOKER=take Joker

#Texts of the jumpto Window
SONG_JUMPTO_DESC=Jump to Song
SONG_JUMPTO_TYPE_DESC=Search for:
SONG_JUMPTO_TYPE1=All
SONG_JUMPTO_TYPE2=Title
SONG_JUMPTO_TYPE3=Artist
SONG_JUMPTO_SONGSFOUND=%d Song(s) found
SONG_JUMPTO_NOSONGSFOUND=No Song found
SONG_JUMPTO_HELP=Type Text to Search for
SONG_JUMPTO_CATTEXT=Search for: %s

#Texts for Party Mode
PARTY_SCORE_WINS=%s wins!
PARTY_OPTIONS_DESC=Party Mode
PARTY_DIFFICULTY=Difficulty
PARTY_PLAYLIST=Playlist Mode
PARTY_ROUNDS=Rounds
PARTY_TEAMS=Teams
PARTY_TEAMS_PLAYER1=Player Team1
PARTY_TEAMS_PLAYER2=Player Team2
PARTY_TEAMS_PLAYER3=Player Team3
PARTY_SELECT_PLAYER=Enter Playernames!

PARTY_DISMISSED=Dismissed!
PARTY_NOTPLAYEDYET=Not played yet
PARTY_NOBODY=Nobody

#Texts that descripts Plugins or Modis
PLUGIN_HDL_NAME=Hold the Line
PLUGIN_HDL_DESC=Don't get worse than the Pointer at the Rating bar shows you.
PLUGIN_UNTIL5000_NAME=Until 5000
PLUGIN_UNTIL5000_DESC=Who gets 5000 Points first wins the match
PLUGIN_DUELL_NAME=Duell
PLUGIN_DUELL_DESC=Sing a Duell until 10000 Points
PLUGIN_BLIND_NAME=Blind Mode
PLUGIN_BLIND_DESC=Duell without seeing the Notes

#Statistics Screen
#For more Info about the Format Strings look at the SorceCode (UScreenStatMain)
STAT_MAIN_DESC=Statistics

STAT_OVERVIEW_INTRO=%0:s Statistics.  \n Last Reset at %2:.2d.%1:.2d.%3:d
STAT_OVERVIEW_SONG=%0:d Songs(%3:d with Video), whereof %1:d already were played  and %2:d were not played yet.\n The most popular Song is %5:s from %4:s.
STAT_OVERVIEW_PLAYER=Since the last Reset ther were/was %0:d different Player(s).\n The Best Player is %1:s with an average Score of %2:d Points.\n %3:s did the highest Score with %4:d Points.

#Stat Detail Screen
STAT_NEXT=Next Page
STAT_PREV=Previous Page
STAT_REVERSE=Change Order
STAT_PAGE=Seite %0:d of %1:d Pages\n (%2:d of %3:d Entrys)

STAT_DESC_SCORES=HighScores
STAT_DESC_SCORES_REVERSED=LowestScores
STAT_FORMAT_SCORES=%0:s - %1:d  [%2:s] \n (%3:s - %4:s)

STAT_DESC_SINGERS=Best Singers
STAT_DESC_SINGERS_REVERSED=Worst Singers
STAT_FORMAT_SINGERS=%0:s \n Average Score: %1:d

STAT_DESC_SONGS=Most popular Songs
STAT_DESC_SONGS_REVERSED=Least popular Songs
STAT_FORMAT_SONGS=%0:s - %1:s \n %2:dx sung

STAT_DESC_BANDS=Most popular Bands
STAT_DESC_BANDS_REVERSED=Least popular Bands
STAT_FORMAT_BANDS=%0:s \n %1:dx Sung

#Messages for Popup Message Boxes
MSG_QUIT_USDX=Leave UltraStar?
MSG_END_PARTY=End Party Mode?