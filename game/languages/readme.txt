.o0 Ultrastar Deluxe in your language 0o.

-----------------------
   Table of Contents
-----------------------
1. Introduction
2. Translate texts
3. Wildcards

-----------------------
1. Introduction:
-----------------------
To translate USDX to a new language, copy the file Language.new to [Language].ini with [Language]
replaced by the English name of your language (e.g. German.ini for German).
Translate all texts according to section (2) of this file.

-----------------------
2. Translate texts:
-----------------------
1) For each target language there is a translation file named [Language].ini where [Language] is the target
   language.
2) All lines that have not been translated so far are prefixed with ";TODO:" like the following line
     ;TODO: SOME_OPTION=English text here
3) Translate the text if you are familiar with the target language
4) After you have done so remove the ";TODO: " prefix so that it looks like in this German translation:
     SOME_OPTION=Hier der deutsche Text
   IMPORTANT: Do NOT remove the ";TODO: " prefix if you did not translate the text and it is still English.
5) At the end of the file there might be some lines which start with ";INVALID: " or ";UNUSED: ". 
   Invalid lines are not correctly formatted lines and might crash USDX. Unused lines are not used
   anymore by USDX or they have been renamed.
6) If you want you may add your name to the END (not the beginning) of the file:
     ;TRANSLATOR: Mario Translatori
   Note the ";" in front of it.
7) Share your translation with others:
   - Send the translated file to our Patch-Tracker at SourceForge (http://sourceforge.net/tracker/?group_id=191560&atid=937872)
   - or contact us via our IRC channel

-----------------------
3. Wildcards:
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
