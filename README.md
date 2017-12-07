# UltraStar Deluxe Development README

[![Travis Build Status](https://travis-ci.org/UltraStar-Deluxe/USDX.svg?branch=master)](https://travis-ci.org/UltraStar-Deluxe/USDX)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/jbcwa5cmnmnk4609/branch/master?svg=true)](https://ci.appveyor.com/project/bohning/usdx/branch/master)
[![License](https://img.shields.io/badge/license-GPLv2-blue.svg)](LICENSE)

```
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                   _______  _________
            ___   /       \/         \
           /   \  \      _/    /\____/\__________
          /   _/  /     / \______      \         \___    _____
         /   |___/      \     \_/       /          \  \  /     \
         \              /\             /   |\       \  \/      /
          \            /  \_____/    |/       /         /
           \__________/              |_____________/          \
                                                 /      /\      \
                                                 \_____/  \_____/  .eu
                                                                    
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
```
Official Project Website: https://usdx.eu/

![UltraStar Deluxe Logo](https://github.com/UltraStar-Deluxe/USDX/blob/master/icons/ultrastardx-icon_256.png)


### 1. About
UltraStar Deluxe (USDX) is a free and open source karaoke game. It allows up to six players to sing along with music using microphones in order to score points, depending on the pitch of the voice and the rhythm of singing.
UltraStar Deluxe is a fork of the original UltraStar (developed by corvus5).
Many features have been added like party mode, theme support and support for more audio and video formats.
The improved stability and code quality of USDX enabled ports to Linux and Mac OS X.

### 2. Release Notes
- New features contain 6 player on one screen capability, jukebox player, new song selection screen modes (Chessboard, Carousell, Slot machine, Slide, List, Tile), during gameplay you can see on the time bar when there are lyrics to sing, duet mode, new party modes, support for current versions of Microsoft Windows, 
- To set additional song directories change your config.ini like this:
```ini
  [Directories]
  SongDir1=C:\Users\My\Music\MyUSDXSongs
  SongDir2=F:\EvenMoreUSDXSongs
  SongDir...=... (some more directories)
```
- To take a screenshot press _PrintScreen_ key. Screenshots are saved in the directory `screenshots`.
- To enable joypad support change config.ini `Joypad=Off` to `Joypad=On`
- To enable 2 or 3 player each on 2 screens, disable the full screen mode, extend your desktop horizontally and set the resolution to fill one screen. Then, in the config.ini set `Screens=2` and restart the game.
- Press [Shift] + F[1..12] in _NameScreen_ to save the name of a player (alternative use [Shift] instead of [Alt], keep in mind [Alt]+[F4] quits the game on platforms like Windows), press F[1..12] to load the name of a player
- The primary folder for songs on OS X is `$HOME/Music/UltraStar Deluxe`, which is created when UltraStar Deluxe is run for the first time.
- On OS X, by default the `config.ini` file is created in `$HOME/Library/Application Support/UltraStarDeluxe` when UltraStar Deluxe is run for the first time.

### 3. Command-Line Parameters
_Note: This is currently broken / in development._


Command-line parameters are passed to the game adding it to the path of a
shortcut or starting the game within the console.

The following parameters are possible. They can be joined in any possible way.

- `-Benchmark`         : Create a benchmark.log file with start timings.

- `-NoLog`    	       : Do not create any .log files

- `-Joypad`            : Start with joypad support

- `-Language <ID>`     : Load language [ID] on startup.
                         Example: `-Language german`

- `-Songpath <PATH>`   : Same as config Songpath.
                         Example: `-SongPath "C:\Ultrastar Songs"`

- `-ConfigFile <FILE>` : Load configuration file [File] instead of config.ini.
                         The path to the file has to exist.
                         Example: `-ConfigFile config.SongCreation.ini`

- `-ScoreFile <FILE>`  : Use [File] instead of Ultrastar.db
                         The path to the file has to exist.
                         Example: `-ScoreFile HouseParty.db`

- `-FullScreen`        : Start the game in full screen mode

- `-Depth {16|32}`     : Force depth to 16 or 32. Example: `-Depth 16`

- `-Resolution <ID|RESOLUTION>`   : Force resolution. Either by ID (matching an entry of the possible resolution list)
                                    or custom resolution (with the format of `WIDTHxHEIGHT`).
                                    Example: `-Resolution 800x600`

- `-Screens {1|2}`     : Force 1 or 2 screens. Example: `-Screens 2`

Some Examples:

Start with a resolution of 1024x768, a depth of 32 bit and in full screen mode:  
`ultrastar.exe -Resolution 1024x768 -Depth 32 -Fullscreen`

Start without logging and with polish language  
`ultrastar.exe -NoLog -Language polish`

Start with a customs configuration file and score database:  
`ultrastar.exe -ConfigFile "C:\Ultrastar\Configs\PartyConfig.ini" -ScoreFile "C:\Ultrastar\Scores\PartyScores.db"`

### 4. Controls
#### General
|Keys | Action|
| :--- | :--- |
| [F11] | switch on the fly between windowed fullscreen and window mode |
| [Alt] + [Enter] | switch to real fullscreen (including changing resolution) and window mode |

#### Song
#### Shortcuts for song selection screen
|Keys | Action|
| :--- | :--- |
| J | open the "Search for a Song" interface |
| [Enter] | confirm song, search or menu selection |
| [Escape] | go to the previous screen |
| S | play selected song as medley (if medley tags are set, use these, otherwise use calculated values - can be forced using shift key) |
| D | play up to 5 random songs as medleys (if there are songs available with medley tags set, only use these, otherwise use calculated values - can be forced using shift key) |
| E | open selected song in song editor |
| F | add song to medley list |
| K | toggle on/off experimental automatic voice-removal |
| M | open the song menu |
| P | choose a playlist for song selection |
| R | select a random song/category |
| [Alt] + [_Character_] | jump to artist with the first letter/digit _Character_ \(A to Z, 0 to 9\) |
| [Alt] + [Shift] + [_Character_] | jump to title with the first letter/digit _Character_ \(A to Z, 0 to 9\) |
| [Page down] | jump to first artist with the next letter/digit |
| [Page up] | jump to last artist with the previous letter/digit | 
| [Spacebar] | when a duet song is selected, switch first and second voice |

#### Joypad / Controller
|Axis | Action|
| :--- | :--- |
| D-Pad | Navigation (+switching to non-mouse mode) |
| Left Stick | Mouse (+switching to mouse mode if not already) |
| Right Stick | Navigation |

##### Keyboard mode
|Buttons | Action|
| :--- | :--- |
| A / Button 1 | Simulates [Enter] |
| B / Button 2 | Simulates [Escape] |
| Y / Button 3 | Simulates [M] |
| X / Button 4 | Simulates [R] |

##### Mouse mode
|Buttons | Action|
| :--- | :--- |
| A / Button 1 | Simulates _Left Mouse Button_ |
| B / Button 2 | Simulates _Right Mouse Button_ |
| Left/Right Stick Button | Simulates _Middle Mouse Button_ |
| Start | Simulates [Enter] |
| Select | Simulates [Escape] |

#### Shortcuts for song editor
|Keys | Action|
| :--- | :--- | 
| Arrow Left/Right	| select previous/next syllable                                                             |
| Arrow Down/Up	| select previous/next syllable                                                         |
| [Ctrl] + Arrow Right/Left	| move only beginning of note to earlier/later                         |
| [Alt] + Arrow Right/Left	| move only ending of note to earlier/later                            |
| [Shift] + Arrow Up/Down	| change pitch of selected note                                        |
| [Shift] + Arrow Right/Left	| move the note (beginning and ending) to earlier/later            |
|	                                                                                          |
| =	| increase BPM                                                                              |
| -	| decrease BPM                                                                              |
| F	| toggle note freestyle/normal                                                              |
| G	| toggle note golden/normal                                                                 |
| T	| auto-fix timings of all sentence switching                                                |
| C	| capitalize letter at the beginning of all lines                                           |
| [Shift] + C	    | correct all spaces                                                          |
| V	| play audio + video and follow the lyrics                                                  |
|	                                                                                          |
| [Ctrl] + C	    | copy current sentence                                                       |
| [Ctrl] + V	    | paste current sentence                                                      |
| [Ctrl] + Z	    | **undo last change**                                                        |
| S	| save changes                                                                              |
| R	| reload file without saving                                                                |
| P	| play current sentence audio                                                               |
| [Shift] + P	| play current sentence midi                                                        |
| [Ctrl] + [Shift] + P | play current sentence audio and midi                                      |
|	                                                                                          |
| A | set/clear medley start beat                                                             |
| [Shift] + A | set/clear medley end beat                                                     |
| J | jump to medley start beat                                                               |
| [Shift] + J | jump to medley end beat                                                       |
| [Alt] + J | play medley (starting from medley start, ending at medley end)                  |
|	                                                                                          |
| I | jump to preview start                                                                   |
| [Alt] + I | play audio starting from the preview start                                      |
| [Shift] + I | set/clear preview start at the current note's time                            |
|	                                                                                            |
| [Shift] + D | divide BPM by 2 but keep correct timings                                      |
| [Shift] + M | multiply BPM by 2 but keep correct timings                                    |
|	                                                                                          |
| double click on a note	|split note in two parts on the beat at mouse cursor location          |
| select and drag a note up/down	|change pitch of a note                                        |
| select and drag a note left/right	|move the beginning beat of the note to earlier / later    |
|	                                                                                          |
| [Ctrl] + [Del]	 | delete current note                                                      |
| [+ on numberpad] | Increase tone of all notes by 1                                           |
| [- on numberpad] | Decrease tone of all notes by 1                                           |
| [Shift] + [+ on numberpad] | Increase tone of all notes by octave                                      |
| [Shift] + [- on numberpad] | Decrease tone of all notes by octave                                      |
| [F4]             | enter or leave text edit mode                                            |
| . | moves text to right in current sentence                                               |
|	                                                                                          |
| 4	| copy 4 sentence                                                                             |
| 5 | copy 5 sentence	                                                                              |
|	                                                                                              |
| 7	| lower video gap by 1                                                                      |
| [Shift] + 7	| lower video gap by 10                                                           |
| [Ctrl] + 7	| lower video gap by 100                                                          |
| 8 | increase video gap                                                                        |
| [Shift] + 8	| increase video gap by 10                                                        |
| [Ctrl] + 8	| increase video gap by 100                                                       |
| 9	| decrease GAP                                                                              |
| 0	| increase GAP                                                                              |

#### Shortcuts for sing screen
|Keys | Action|
| :--- | :--- |
| S | jump forward to 5 seconds before first singing note |
| R | restart playback for current song but keep scores for already sung parts |
| V | switch between video, visualisation and background |
| W | if configured and enabled, show webcam video instead as background |
| T | toggle time displaying between total, remaining and already played time |
| [Tab] | switch visualization / camera mode |
| [Spacebar] | pause / play |
| [Esc] or [Backspace] | cancel current song or end early |

### 5. Build and Run
[Freepascal](http://freepascal.org/) 3.0.0 or newer is required to compile UltraStar Deluxe. If you had some older version of fpc installed before, make sure to remove everything of it correctly before trying to install freepascal (otherwise compiling will fail with various weird error messages). Also, using the 3.0-development branch with current fixes is suggested.
If you want to help the project by coding patches, we suggest you to use the [Lazarus 1.6](http://www.lazarus-ide.org/) or newer integrated development environment.
For linking and running the game, the following libraries are also required:
- SDL2, SDL2_image
- ffmpeg 2.8 or older
- sqlite
- [bass](http://www.un4seen.com/bass.html)
- some fonts like ttf-dejavu and ttf-freefont
- portaudio
- lua 5.1 or 5.2 or 5.3
- opencv if you want webcam support
- projectM if you want audio visualisation support

#### Compiling using Lazarus
1. Start Lazarus.
2. Choose Project → Open Project … in the menu bar. A file-dialog box will show.
3. Change to the src subdirectory of your USDX working copy (e.g. ultrastardx/src).
  * If you are running Windows, open the ultrastardx-win.lpi project-file (Preferably use the win32 verison of lazarus, as the included libraries are 32 bit).
  * On Unix-like systems use the ultrastardx-unix.lpi file.
4. Now you can compile USDX by choosing the menu entry Run → Build or pressing Ctrl+F9.
8. If you want to compile and/or start USDX directly choose Run → Run or press F9.

#### Compiling on Linux/BSD using make
1. make sure all required libraries are installed 
  * for current debian / ubuntu: 
    `sudo apt-get update && sudo apt-get install git automake make gcc fpc libsdl2-image-dev libavformat-dev libswscale-dev libsqlite3-dev libfreetype6-dev portaudio19-dev libportmidi-dev liblua5.3-dev libopencv-highgui-dev`
  * if you want to build --with-libprojectM, you also need
    `sudo apt-get install g++ libprojectm-dev`
  * for arch linux there is an aur package called [ultrastardx-git](https://aur.archlinux.org/packages/ultrastardx-git)
2. `git clone https://github.com/UltraStar-Deluxe/USDX`
2. `cd USDX`
4. `./autogen.sh`
5. `./configure` (or use _autoconf_)
6. `make`
7. Play the game, 
   * install the game and start it
     - `sudo make install`
     - `ultrastardx`
   * or start it directly  
     `./game/ultrastardx`

#### Compiling on MacOS (High Sierra)
- USDX is built using _Homebrew_ and official _FreePascal build_ (using its compiler _FPC_)
- To install Homebrew, follow instructions from [brew.sh](http://brew.sh)
- You can get the FPC build from [freepascal.org](http://www.freepascal.org/down/i386/macosx.var) or
  * `brew install fpc`
- Make sure the XCode command line tools are installed.
  * `xcode-select --install`
- Needed brew libraries can be installed using (ffmpeg up to 3.3 is supported):
  * `brew install sdl2 sdl2_image automake portaudio binutils sqlite freetype lua libtiff pkg-config ffmpeg@2.8`
- Clone repo
  * `git clone https://github.com/UltraStar-Deluxe/USDX`
- Generate `configure` file and more
  * `./autogen.sh`
- Make sure that you have your build setup right
  * `./configure`
    * add argument `--enable-osx-fink` or `--enable-osx-brew` (default) according to the packaging you are using
- Now build the UltraStar application
  * `make macosx-standalone-app`
- Run by clicking UltraStarDeluxe in your build folder or
  * `open UltraStarDeluxe.app`

Feel free to fork this project, modify it to your hearts content and maybe also do pull requests to this repository for additional features, improvements or clean-ups.
