# UltraStar Deluxe 1.3 trunk README
[![Build Status](https://travis-ci.org/UltraStar-Deluxe/USDX.svg?branch=master)](https://travis-ci.org/UltraStar-Deluxe/USDX)
```
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                   _______  _________
            ___   /       \/         \
           /   \  \      _/    /\____/\__________
          /   _/  /     / \______      \         \___    _____
         /   |___/      \     \_/       /          \  \  /     \
         \              /\             /   |\       \  \/      /
          \            /  \_ultrastar_/    |/       /         /
           \______www_/              |___deluxe____/          \
                                                 /      /\      \
                                                 \_org_/  \_____/
                                                                    
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
```
Official Project Website: http://sourceforge.net/projects/ultrastardx/

![UltraStar Deluxe Logo](https://github.com/UltraStar-Deluxe/USDX/blob/master/icons/ultrastardx-icon_256.png)
```
 ============================
= 1. About                   =
= 2. Release Notes           =
= 3. Command-Line Parameters =
= 4. Controls                =
 ============================


 ====================
 = 1. About         =
 ====================

UltraStar Deluxe (USDX) is a free and open source karaoke game.  It allows
up to six players to sing along with music using microphones in order to
score points, depending on the pitch of the voice and the rhythm of
singing.

UltraStar Deluxe is a fork of the original UltraStar (developed by corvus5).
Many features have been added like party mode, theme support and support
for more audio and video formats.
The improved stability and code quality of USDX enabled ports to Linux and
Mac OS X.

 ====================
 = 2. Release Notes =
 ====================
- New features contain 6 player on one screen capability, jukebox player, new song selection screen modes (chessboard, carousell, slot machine, Slide, List, Tile), during gameplay you can see on the time bar when there are lyrics to sing, duet mode, 

- To set additional song directories change your config.ini like this:
  [Directories]
  SongDir1=C:\Users\My\Music\MyUSDXSongs
  SongDir2=F:\EvenMoreUSDXSongs
  SongDir...=... (some more directories)

- To take a screenshot press "PrintScreen" key.
  Screenshots are saved in the directory "Screenshots".

- To enable joypad support change config.ini:

  [Controller]
  Joypad=Off

  to

  [Controller]
  Joypad=On

- To enable 2 or 3 player each on 2 screens,
  disable the full screen mode, extend your desktop horizontally 
  and set the resolution to fill one screen.
  Then, in the config.ini set Screens=2

- Press Alt + F[1..12] in NameScreen to save the name of a player
  Press F[1..12] to load the name of a player

- To enable benchmark run the game with -benchmark parameter


 ==============================
 = 3. Command-Line Parameters =
 ==============================
This is currently broken / in development.
```
<!---
Command-line parameters are passed to the game adding it to the path of a
shortcut or starting the game within the console.

The following parameters are possible. They can be joined in any possible way.

- Benchmark         : Create a benchmark.log file with start timings.

- NoLog    	    : Do not create any .log files

- Joypad            : Start with joypad support

- Language [ID]     : Load language [ID] on startup.
                      Example: -Language german

- Songpath [Path]   : Same as config Songpath.
                      Example: -SongPath "C:\Ultrastar Songs"

- ConfigFile [File] : Load configuration file [File] instead of config.ini.
                      The path to the file has to exist.
                      Example: -ConfigFile config.SongCreation.ini

- ScoreFile [File]  : Use [File] instead of Ultrastar.db
                      The path to the file has to exist.
                      Example: -ScoreFile HouseParty.db

- FullScreen        : Start the game in full screen mode

- Depth [16/32]     : Force depth to 16 or 32. Example: -Depth 16

- Resolution [ID]   : Force resolution. Example: -Resolution 800x600

- Screens [1/2]     : Force 1 or 2 screens. Example: -Screens 2

Some Examples:

Start with a resolution of 1024x768, a depth of 32 bit and in full screen
mode:
ultrastar.exe -Resolution 1024x768 -Depth 32 -Fullscreen

Start without logging and with polish language
ultrastar.exe -NoLog -Language polish

Start with a customs configuration file and score database:
ultrastar.exe -ConfigFile C:\Ultrastar\Configs\PartyConfig.ini -ScoreFile C:\Ultrastar\Scores\PartyScores.db
--->

###4. Controls
####Song
####Shortcuts for song selection screen
|Keys | Action|
| :--- | :--- |
| J | open the "Search for a Song" interface |
| [Enter] | confirm song or menu selection |
| [Escape] | go to the previous screen |
| R | select a random song/category |
| [Alt] + [Letter] | jump to a artist with the first letter [Letter] |
| [Alt] + [Shift] + [Letter] | jump to a title with the first letter [Letter] |
| [Spacebar] | when a duet song is selected, switch first and second voice |

####Shortcuts for song editor
|Keys | Action|
| :--- | :--- | 
|arrow right	|select next syllable                                                             |
|arrow left	|select previous syllable                                                         |
|arrow up	|select next sentence                                                             |
|arrow down	|select previous sentence                                                         |
|ctrl + arrow right/left	|move only beginning of note to earlier/later                         |
|Alt + arrow right/left	|move only ending of note to earlier/later                            |
|Shift + arrow up/down	|change pitch of selected note                                        |
|Shift + arrow right/left	|move the note (beginning and ending) to earlier/later            |
|	                                                                                          |
|=	|increase BPM                                                                              |
|-	|decrease BPM                                                                              |
|f	|toggle note freestyle/normal                                                              |
|g	|toggle note golden/normal                                                                 |
|t	|auto-fix timings of sentence switching                                                    |
|v	|play audio + video and follow the lyrics                                                  |
|	                                                                                          |
|Ctrl + z	|undo last change                                                                  |
|s	|save changes                                                                              |
|p	|play current sentence audio                                                               |
|Shift + P	|play current sentence midi                                                        |
|Ctrl + Shift + P	|play current sentence audio and midi                                      |
|	                                                                                          |
|	                                                                                          |
|double click on a note	|split note in two parts on the beat at mouse cursor location          |
|select and drag a note up/down	|change pitch of a note                                        |
|select and drag a note left/right	|move the beginning beat of the note to earlier / later    |
|	                                                                                          |
|4	|copy sentence                                                                             |
|5 |copy sentence	                                                                              |
|	                                                                                          |
|7	|lower video gap                                                                           |
|8 	|increase video gap                                                                        |
|9	|decrease GAP                                                                              |
|0	|increase GAP                                                                              |

####Shortcuts for sing screen
|Keys | Action|
| :--- | :--- |
| s | jump forward to 5 seconds before first singing note |
| [Esc] or [Backspace] | cancel current song |
