UltraStar Deluxe 1.0 README
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
Wiki:        http://wiki.ultrastardeluxe.org/

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

- To change the path to the song directory add to config.ini:
  [Path]
  Songs=[SongFolder] (e.g. C:\Program Files\Ultrastar\Songs)

- To take a screenshot press "PrintScreen" key.
  Screenshots are saved in the directory "Screenshots".

- To enable joypad support change config.ini:

  [Controller]
  Joypad=Off

  to

  [Controller]
  Joypad=On

- To Enable 4 to 6 player mode 2 screens are needed.
  Disable the full screen mode, extend your desktop horizontally and set
  the resolution to fill one screen.
  Add to Config.ini:
  [Graphics]
  Screens=2

- Press Alt + F[1..12] in NameScreen to save the name of a player
  Press F[1..12] to load the name of a player

- To enable benchmark run the game with -benchmark parameter


 ==============================
 = 3. Command-Line Parameters =
 ==============================

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


 ===============
 = 4. Controls =
 ===============

[J]      to open the "Search for a Song" interface
[Cursor] to navigate through the screens.
[Enter]  to confirm
[Escape] to go to the previous screen.

Songscreen
[R],
  [Shift] + [R],
  [Strg]  + [R]            select a random song/category
[Alt] + [Letter]           jump to a artist with the first letter [Letter]
[Alt] + [Shift] + [Letter] jump to a title with the first letter [Letter]


Editor controls are described in documentation.pdf
