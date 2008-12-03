.o0 Ultrastar Deluxe in your language 0o.

-----------------------
   Table of Contents
-----------------------
1. Introduction
2. Statistic wildcards
3. Texts to add

-----------------------
1. Introduction:
-----------------------
To translate USD to a new language, take the English language file, or another one that is up to date and edit the texts behind the equal mark(=).

-----------------------
2. Statistic Wild-Cards:
-----------------------
Here are some informations about the wildcards in the language texts for the statistic screens (STAT_...):
Information that will replace the wildcards:

STAT_OVERVIEW_INTRO:
  Format:
    %0:d Ultrastar Version
    %1:d Day of Reset (A1)
    %2:d Month of Reset (A2)
    %3:d Year of Reset (A3)

STAT_OVERVIEW_SONG:
  Format:
    %0:d Count Songs (A1)
    %1:d Count of Sung Songs (A2)
    %2:d Count of UnSung Songs
    %3:d Count of Songs with Video (A3)
    %4:s Name of the most popular Song

STAT_OVERVIEW_PLAYER:
  Format:
    %0:d Count Players (A1)
    %1:s Best Player (Result)
    %2:d Best Players Score
    %3:s Best Score Player (Result2)
    %4:d Best Score

STAT_FORMAT_SCORES:
  Format:
    %0:s Singer
    %1:d Score
    %2:s Difficulty
    %3:s Song Artist   
    %4:s Song Title

STAT_FORMAT_SINGERS:
  Format:
    %0:s Singer
    %1:d Average Score


STAT_FORMAT_SONGS:
  Format:
    %0:s Artist
    %1:s Title
    %2:d Times Sung

STAT_FORMAT_BANDS:
  Format:
    %0:s Artist Name
    %1:d Times Sung

Some further explanations about the wildcards:
%x:[.y]z

Where X is the number of the wildcard,
Y is optional, it is the number of digits for deciaml numbers (Z=d). So, if y is 2 there and the number is only 0 to 9 there will be a zero added in front of the number.
z can be d for numbers and s for texts

For the date thing in STAT_OVERVIEW_INTRO you may use %1:.2d for the day and %2:.2d for the month.

-----------------------
3. Texts to Add:
-----------------------
To port a language file from Ultrastar 0.5.2 or higher add the following texts to the end of the file:

#Main Screen
SING_MENU=Main Menu

SING_MULTI=party
SING_MULTI_DESC=Sing in PartyMode

SING_TOOLS=Tools

SING_STATS=stats
SING_STATS_DESC=View the Statistics

#Sound Options Screen
SING_OPTIONS_SOUND_PREVIEWVOLUME=Preview Volume
SING_OPTIONS_SOUND_PREVIEWFADING=Preview Fading

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
POPUP_PERFECT=perfect!
POPUP_AWESOME=awesome!
POPUP_GREAT=great!
POPUP_GOOD=good!
POPUP_NOTBAD=not bad!
POPUP_BAD=bad!
POPUP_POOR=poor!
POPUP_AWFUL=awful!

#To connect strings with, e.g.: He, you and I
IMPLODE_GLUE1=, 
IMPLODE_GLUE2= and 

#Song Screen Legend
PLAYLIST_CATTEXT=Playlist: %s

#Text for the legend bar at the bottom
SING_LEGEND_CONTINUE=Continue

#Texts of the menu that appears when M is pressed at the song selection
SONG_MENU_NAME_MAIN=Song Menu
SONG_MENU_PLAY=Sing
SONG_MENU_EDIT=Edit
SONG_MENU_MODI=Sing a Modi
SONG_MENU_CHANGEPLAYERS=Change Players
SONG_MENU_CANCEL=Cancel

#Playlist Menu
SONG_MENU_NAME_MAIN=song menu
SONG_MENU_PLAY=Sing
SONG_MENU_CHANGEPLAYERS=Change Players
SONG_MENU_EDIT=Edit
SONG_MENU_MODI=Sing a Modi
SONG_MENU_CANCEL=Cancel

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
SONG_MENU_PLAYLIST_DELCURRENT=delete current Playlist

SONG_MENU_NAME_PLAYLIST_DEL=Delete Playlist?

#Menu Party Modus
SONG_MENU_NAME_PARTY_MAIN=Menu
SONG_MENU_JOKER=Joker

SONG_MENU_NAME_PARTY_JOKER=take Joker

#Texts of the jump to window
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
PARTY_MODE=party mode
PARTY_DIFFICULTY=Difficulty
PARTY_PLAYLIST=Playlist Mode
PARTY_PLAYLIST_ALL=All songs
PARTY_PLAYLIST_CATEGORY=Folder
PARTY_PLAYLIST_PLAYLIST=Playlist
PARTY_ROUNDS=Rounds
PARTY_TEAMS=Teams
PARTY_TEAMS_PLAYER1=Player Team1
PARTY_TEAMS_PLAYER2=Player Team2
PARTY_TEAMS_PLAYER3=Player Team3
PARTY_LEGEND_CONTINUE=continue
PARTY_OPTIONS_DESC=settings for the party-game
PARTY_OPTIONS_WHEREAMI=Party Options
PARTY_PLAYER_DESC=enter player- and teamnames!
PARTY_PLAYER_WHEREAMI=Party Names
PARTY_PLAYER_ENTER_NAME=enter names
PARTY_PLAYER_LEGEND_CONTINUE=start party-game
PARTY_SONG_WHEREAMI=Party Song-Selection 	 
PARTY_SONG_LEGEND_CONTINUE=sing 	 
PARTY_SONG_MENU=party menu
PARTY_ROUND_DESC=next players to the mics
PARTY_ROUND_WHEREAMI=Party Next Round
PARTY_ROUND_LEGEND_CONTINUE=start round
PARTY_SCORE_DESC=score of the last round
PARTY_SCORE_WHEREAMI=Party Points
PARTY_WIN_DESC=winner of the party-game
PARTY_WIN_WHEREAMI=Party Winner
PARTY_WIN_LEGEND_CONTINUE=back to main-menu
PARTY_ROUND=Round
PARTY_ROUND_WINNER=Winner
PARTY_NOTPLAYEDYET=not played yet
PARTY_NOBODY=nobody
NEXT_ROUND=Next round:
PARTY_DISMISSED=Dismissed!
PARTY_SCORE_WINS=%s 
PARTY_SCORE_WINS2=wins!
PARTY_SONG_WHEREAMI=Party Song-Selection
PARTY_SONG_LEGEND_CONTINUE=Party-Menu

#Texts describing Plugins or Modi
PLUGIN_HDL_NAME=Hold the Line
PLUGIN_HDL_DESC=Don't get worse than the pointer at the rating bar shows you.
PLUGIN_UNTIL5000_NAME=Until 5000
PLUGIN_UNTIL5000_DESC=Who gets 5000 points first wins the match.
PLUGIN_DUELL_NAME=Duell
PLUGIN_DUELL_DESC=Sing a duell until 10000 points.
PLUGIN_BLIND_NAME=Blind Mode
PLUGIN_BLIND_DESC=Duell without seeing the notes.
PLUGIN_TEAMDUELL_NAME=Team Duell
PLUGIN_TEAMDUELL_DESC=Pass The Mic!

#Statistics Screen
#For more info about the format strings look at the source code (UScreenStatMain)
STAT_MAIN=Statistics
STAT_MAIN_DESC=General
STAT_MAIN_WHEREAMI=Statistics

STAT_OVERVIEW_INTRO=%0:s Statistics.  \n Last Reset at %2:.2d.%1:.2d.%3:d
STAT_OVERVIEW_SONG=%0:d Songs(%3:d with Video), whereof %1:d already were played  and %2:d were not played yet.\n The most popular Song is %5:s from %4:s.
STAT_OVERVIEW_PLAYER=Since the last Reset there were/was %0:d different Player(s).\n The Best Player is %1:s with an average Score of %2:d Points.\n %3:s did the highest Score with %4:d Points.

#Stat Detail Screen
STAT_DETAIL=Statistics
STAT_DETAIL_WHEREAMI=Detail Statistics

STAT_NEXT=Next Page
STAT_PREV=Previous Page
STAT_REVERSE=Reverse Order
STAT_PAGE=Seite %0:d of %1:d Pages\n (%2:d of %3:d Entrys)

STAT_DESC_SCORES=HighScores
STAT_DESC_SCORES_REVERSED=LowScores
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
MSG_QUESTION_TITLE=Chicken Out
MSG_QUIT_USDX=Really leave\n\nUltraStar?
MSG_END_PARTY=Really end\n\nParty Mode?
ERROR_NO_SONGS=Error: \n No Songs \n loaded
ERROR_NO_PLUGINS=Error: \n No Plugins \n loaded