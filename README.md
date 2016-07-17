# UltraStar Deluxe 1.3 trunk README

[![Join the chat at https://gitter.im/UltraStar-Deluxe/USDX](https://badges.gitter.im/UltraStar-Deluxe/USDX.svg)](https://gitter.im/UltraStar-Deluxe/USDX?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/UltraStar-Deluxe/USDX.svg?branch=master)](https://travis-ci.org/UltraStar-Deluxe/USDX)
[![SourceForge](https://img.shields.io/sourceforge/dt/ultrastardx.svg?maxAge=86400)](https://sourceforge.net/projects/ultrastardx/files/latest/download)
[![License](https://img.shields.io/badge/license-GPLv2-blue.svg)](LICENSE)

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


###1. About
UltraStar Deluxe (USDX) is a free and open source karaoke game. It allows up to six players to sing along with music using microphones in order to score points, depending on the pitch of the voice and the rhythm of singing.
UltraStar Deluxe is a fork of the original UltraStar (developed by corvus5).
Many features have been added like party mode, theme support and support for more audio and video formats.
The improved stability and code quality of USDX enabled ports to Linux and Mac OS X.

###2. Release Notes
- New features contain 6 player on one screen capability, jukebox player, new song selection screen modes (chessboard, carousell, slot machine, Slide, List, Tile), during gameplay you can see on the time bar when there are lyrics to sing, duet mode, new party modes, support for current versions of Microsoft Windows, 
- To set additional song directories change your config.ini like this:
```
  [Directories]
  SongDir1=C:\Users\My\Music\MyUSDXSongs
  SongDir2=F:\EvenMoreUSDXSongs
  SongDir...=... (some more directories)
```
- To take a screenshot press "PrintScreen" key. Screenshots are saved in the directory "Screenshots".
<!--- - To enable joypad support change config.ini "Joypad=Off" to "Joypad=On"--->
- To enable 2 or 3 player each on 2 screens, disable the full screen mode, extend your desktop horizontally and set the resolution to fill one screen. Then, in the config.ini set Screens=2 and restart the game.
- Press Alt + F[1..12] in NameScreen to save the name of a player, press F[1..12] to load the name of a player
- The primary folder for songs on OS X is "$HOME/Music/UltraStar Deluxe", which is created when UltraStar Deluxe is run for the first time.
- On OS X, by default the "config.ini" file is created in "$HOME/Library/Application Support/UltraStarDeluxe" when UltraStar Deluxe is run for the first time.


###3. Command-Line Parameters
This is currently broken / in development.


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
| [Enter] | confirm song, search or menu selection |
| [Escape] | go to the previous screen |
| D | play selected song as medley |
| E | open selected song in song editor |
| F | add song to medley list |
| K | toggle on/off experimental automatic voice-removal |
| M | open the song menu |
| P | choose a playlist for song selection |
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
| v | switch between video, visualisation and background |
| w | if configured and enabled, show webcam video instead as background |
| t | toggle time displaying between total, remaining and already played time |
| [Tab] | switch visualization / camera mode |
| [Spacebar] | pause / play |
| [Esc] or [Backspace] | cancel current song or end early |
| [F11] | switch on the fly between fullscreen and window mode |

###5. Build and Run
Freepascal 3.0.0 or newer is required to compile UltraStar Deluxe. If you had some older version of fpc installed before, make sure to remove everything of it correctly before trying to install freepascal (otherwise compiling will fail with various weird error messages). Also, using the 3.0-development branch with current fixes is suggested.
If you want to help the project by coding patches, we suggest you to use the [Lazarus 1.6](http://www.lazarus-ide.org/) or newer integrated development environment.
For linking and running the game, the following libraries are also required:
- SDL2, SDL2_gfx, SDL2_mixer, SDL2_image, SDL2_ttf, SDL2_net
- ffmpeg 2.8 or older
- sqlite
- [bass](http://www.un4seen.com/bass.html)
- some fonts like ttf-dejavu and ttf-freefont
- portaudio
- pcre
- lua 5.1 or 5.2 or 5.3
- opencv if you want webcam support
- projectM if you want audio visualisation support

####Compiling using Lazarus
1. Start Lazarus.
2. Choose Project → Open Project … in the menu bar. A file-dialog box will show.
3. Change to the src subdirectory of your USDX working copy (e.g. ultrastardx/src).
  * If you are running Windows, open the ultrastardx-win.lpi project-file.
  * On Unix-like systems use the ultrastardx-unix.lpi file.
4. Now you can compile USDX by choosing the menu entry Run → Build or pressing Ctrl+F9.
8. If you want to compile and/or start USDX directly choose Run → Run or press F9.

####Compiling on linux/bsd using make
1. make sure all required libraries are installed 
  * for current debian / ubuntu: sudo apt-get update && sudo apt-get install git fpc libsdl2-dev libsdl2-image-dev libsdl2-image-2.0-0 libsdl2-2.0-0 libsdl2-mixer-2.0-0 libsdl2-mixer-dev libsdl2-net-2.0-0 libsdl2-net-dev libsdl2-ttf-2.0-0 libsdl2-ttf-dev libsdl2-gfx-1.0-0 libsdl2-gfx-dev ffmpeg libavdevice-dev libsqlite3-0 libsqlite3-dev libpcre3 libpcre3-dev ttf-dejavu ttf-freefont portaudio19-dev lua5.1-dev libpng16-16 libopencv-highgui-dev libprojectm-dev
  * for arch linux there is an aur package called [ultrastardx-git](https://aur.archlinux.org/packages/ultrastardx-git)
2. git clone https://github.com/UltraStar-Deluxe/USDX
2. cd USDX
3. ./configure (or use autoconf)
4. make
5. sudo make install
6. ultrastardx

####Compiling on OS X
- USDX Is built using Homebrew and official FreePascal build
- You can install Homebrew from http://brew.sh
- You can get the FPC build from http://www.freepascal.org/down/i386/macosx.var
- Don't miss XQuartz from http://www.xquartz.org
- Needed brew libraries can be installed using:
  * brew install sdl2 sdl2_gfx sdl2_image sdl2_mixer sdl2_net sdl2_ttf ffmpeg libav portaudio binutils sqlite freetype libpng pcre lua libtiff
- Pass argument --enable-osx-fink or --enable-osx-brew (default) according to the packaging you are using

Feel free to fork this project, modify it to your hearts content and maybe also do pull requests to this repository for additional features, improvements or clean-ups.
