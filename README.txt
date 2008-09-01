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

UltraStar Deluxe (USDX) is a free and open source karaoke game. It allows up to six players to sing along with music using microphones in order to score points, depending on the pitch of the voice and the rhythm of singing. 

UltraStar Deluxe is a fork of the original UltraStar (developed by corvus5). 
Many features have been added like party mode, theme support and
support for more audio and video formats.
The improved stability and code quality of USDX enabled ports to Linux and Mac OS X.

 ====================
 = 2. Release Notes =
 ====================

- To change the path to the song directory add to config.ini:
  [Path]
  Songs=[SongFolder] (e.g. C:\Program Files\Ultrastar\Songs)

- To take a screenshot press "PrintScreen" Key
  Screenshots are saved in the directory "Screenshots".

- To enable joypad support change config.ini:

  [Controller]
  Joypad=Off

  to

  [Controller]
  Joypad=On

- To Enable 4 to 6 Playermode 2 Screens are needed.
  Disable the fullscreen mode, extend your desktop horizontaly, set the
  resolution to fill one screen. 
  Add to Config.ini:
  [Graphics]
  Screens=2

- Press Alt + F[1..12] in NameScreen to Save a Playername
  Press F[1..12] to Load a Playername

- To enable benchmark run the game with -benchmark parameter


 ==============================
 = 3. Command-Line Parameters =
 ==============================

Command-Line Parameters are passed to the game adding it to the Path of a
Shortcut or starting the game within the console.

The following parameters are possible. They can be joined in any possible way.

- Benchmark         : Create a benchmark.log file with start timings.

- NoLog    	        : Do not create any .log files

- Joypad            : Start with Joypad support

- Language [ID]     : Load Language [ID] on startup.
                      Example: -Language german

- Songpath [Path]   : Some as config Songpath.
                      Example: -SongPath "C:\Ultrastar Songs"

- ConfigFile [File] : Load Configfile [File] instead of config.ini. 
                      Path to the file have to exist.
                      Example: -ConfigFile config.SongCreation.ini

- ScoreFile [File]  : Use [File] instead of Ultrastar.db
                      Path to the file have to exist.
                      Example: -ScoreFile HouseParty.db

- FullScreen        : Start the game in Fullscreen Mode

- Depth [16/32]     : Force Depth 16 or 32. Example: -Depth 16

- Resolution [ID]   : Force resolution. Example: -Resolution 800x600

- Screens [1/2]     : Force 1 or 2 Screen Mode. Example: -Screens 2

Some Examples:

Start with Resolution 1024x768 32 Bit Depth and Fullscreen:
ultrastar.exe -Resolution 1024x768 -Depth 32 -Fullscreen

Start without logging and polish Language
ultrastar.exe -NoLog -Language german

Start with custom config File and Score DB:
ultrastar.exe -ConfigFile C:\Ultrastar\Configs\PartyConfig.ini -ScoreFile C:\Ultrastar\Scores\PartyScores.db


 ===============
 = 4. Controls =
 ===============

[J]      to open the "Search for a Song" Interface
[Cursor] to navigate through the screens.
[Enter]  to confirm 
[Escape] to go to the previous screen.

In Songscreen
[R],
  [Shift] + [R],
  [Strg]  + [R]            select a random song/category
[Alt] + [Letter]           jump to a artist with the first letter [Letter]
[Alt] + [Shift] + [Letter] jump to a title with the first letter [Letter]


Editor Controls are described in documentation.pdf
