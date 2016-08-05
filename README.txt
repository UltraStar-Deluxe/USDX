UltraStar Deluxe 1.3.2 README
----------------------------

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->8 - -
                   _______  _________
            ___   /       \/         \_______
           /   \  \      _/    /\____/       \__________
          /   _/  /     / \______             \         \___    _____
         /   |___/      \     \_/             /          \  \  /     \
         \              /\                   /   |\       \  \/      /
          \            /  \_ultrastardeluxe_/    |/       /         /
           \______www_/                 |____________org_/          \
                                                      /      /\  ~=~ \
                                                      \_____/  \ mog /
                                                                 ~=~
- - 8<- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 ============================
= 1. About                   =
= 2. Release Notes           =
= 3. Command-Line Parameters =
= 4. Controls                =
 ============================

SF.Net Page: http://sourceforge.net/projects/ultrastardx/
Github Repo: https://github.com/UltraStar-Deluxe/USDX/
Wiki:        http://wiki.ultrastardeluxe.org/

 ====================
 = 1. About         =
 ====================

UltraStar Deluxe (USDX) is a free and open source karaoke game. It allows
up to six players to sing along with music using microphones in order to
score points, depending on the pitch of the voice and the rhythm of singing.

UltraStar Deluxe is a fork of the original UltraStar (developed by corvus5).
Many features have been added like party mode, theme support and support for
more audio and video formats.
The improved stability and code quality of USDX enabled ports to Linux and
Mac OS X.


 ====================
 = 2. Release Notes =
 ====================
- New features contain 6 player on one screen capability, jukebox player,
  new song selection screen modes (Chessboard, Carousell, Slot machine, Slide,
  List, Tile), during gameplay you can see on the time bar when there are
  lyrics to sing, duet mode, new party modes, support for current versions of
  Microsoft Windows, 

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

- To enable 4 to 6 player mode, 2 screens are needed for proper usage.
  Disable the full screen mode, extend your desktop horizontally and set
  the resolution to fill one screen.
  Add to Config.ini:
  [Graphics]
  Screens=2

- To enable 2 or 3 player each on 2 screens, disable the full screen mode,
  extend your desktop horizontally and set the resolution to fill one screen.
  Then, in the config.ini set Screens=2 and restart the game.

- Press [Alt] + F[1..12] in NameScreen to save the name of a player
  (alternative use [Shift] instead of [Alt], keep in mind [Alt]+[F4] quits the
  game on platforms like Windows), press F[1..12] to load the name of a player)
- Press F[1..12] to load the name of a player

- The primary folder for songs on OS X is $HOME/Music/UltraStar Deluxe, which
  is created when UltraStar Deluxe is run for the first time.

- On OS X, by default the config.ini file is created in
  $HOME/Library/Application Support/UltraStarDeluxe` when UltraStar Deluxe is
  run for the first time.
  
- To enable benchmark run the game with -benchmark parameter


 ==============================
 = 3. Command-Line Parameters =
 ==============================

Command-line parameters are passed to the game adding it to the path of a
shortcut or starting the game within the console.

The following parameters are possible. They can be joined in any possible way.
(parenthesis are not required; don't write down characters like < > [ ] { } )

- Benchmark         : Create a benchmark.log file with start timings.

- NoLog             : Do not create any .log files

- Joypad            : Start with joypad support

- Language <ID>     : Load language ID on startup.
                      Example: -Language german

- Songpath <PATH>   : Same as config Songpath.
                      Example: -SongPath "C:\Ultrastar Songs"

- ConfigFile <FILE> : Load configuration file FILE instead of config.ini.
                      The path to the file has to exist.
                      Example: -ConfigFile config.SongCreation.ini

- ScoreFile <FILE>  : Use [File] instead of Ultrastar.db
                      The path to the file has to exist.
                      Example: -ScoreFile HouseParty.db

- FullScreen        : Start the game in full screen mode

- Depth {16|32}     : Force depth to 16 or 32. Example: -Depth 16

- Resolution <ID|RESOLUTION>   : Force resolution. Either by ID (matching an
                                 entry of the possible resolution list) or
                                 custom resolution (with the format of
                                 WIDTHxHEIGHT). Example: -Resolution 800x600

- Screens {1|2}     : Force 1 or 2 screens. Example: -Screens 2

Some Examples:

Start with a resolution of 1024x768, a depth of 32 bit and in full screen mode:
ultrastar.exe -Resolution 1024x768 -Depth 32 -Fullscreen

Start without logging and with polish language
ultrastar.exe -NoLog -Language polish

Start with a customs configuration file and score database:
ultrastar.exe -ConfigFile "C:\Ultrastar\Configs\PartyConfig.ini" -ScoreFile "C:\Ultrastar\Scores\PartyScores.db"


 ===============
 = 4. Controls =
 ===============

[F11]             switch on the fly between windowed fullscreen and window mode |
[Alt] + [Enter]   switch to real fullscreen (including changing resolution) and window mode |

[Cursor]   to navigate through the screens.
[Enter]    to confirm
[Escape]   to go to the previous screen.

Songscreen
[J]                           to open the "Search for a Song" interface
[R]                           select a random song/category
[D]                           play selected song as medley
[E]                           open selected song in song editor
[F]                           add song to medley list
[K]                           toggle on/off experimental automatic voice-removal
[M]                           open the song menu
[P]                           choose a playlist for song selection
[Alt] + [Character]           jump to a artist with the first letter/digit Character (A to Z, 0 to 9)
[Alt] + [Shift] + [Character] jump to title with the first letter/digit Character (A to Z, 0 to 9)
[Spacebar]                    when a duet song is selected, switch first and second voice

Joypad / Controller
D-Pad           Navigation (+switching to non-mouse mode)
Left Stick      Mouse (+switching to mouse mode if not already)
Right Stick     Navigation

Keyboard mode
A / Button 1    Simulates [Enter]
B / Button 2    Simulates [Escape]
Y / Button 3    Simulates [M]
X / Button 4    Simulates [R]

Mouse mode
A / Button 1               Simulates Left Mouse Button
B / Button 2               Simulates Right Mouse Button
Left/Right Stick Button    Simulates Middle Mouse Button
Start                      Simulates [Enter]
Select                     Simulates [Escape]

Shortcuts for sing screen
S                       jump forward to 5 seconds before first singing note
V                       switch between video, visualisation and background
W                       if configured and enabled, show webcam video instead as background
T                       toggle time displaying between total, remaining and already played time
[Tab]                   switch visualization / camera mode
[Spacebar]              pause / play
[Esc] or [Backspace]    cancel current song or end early


Editor controls are described in documentation.pdf
