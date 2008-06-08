Ultrastar Deluxe 1.0 Readme.txt
----------------------------
1. Authors
2. Release Notes
3. Command-Line Parameters
4. Controls
5. License
----------------------------

SF.Net Page: http://sourceforge.net/projects/ultrastardx/

This is just a little introduction, for more information open documentation.pdf

----------------------------
1. Authors
----------------------------
This game was introduced by Corvus5 who has written most of the code by himself.
Basing on the official release 0.5.0 Mota and Whiteshark started to write little patches and modifications, and released this package named ultra-star.dl.am Mod.
This modification was continued at Sourceforge.net by the Ultrastar Deluxe Team:
     Blindy
     Mog
     Mota
     Sawyer
     Whiteshark
And this piece of software is the Result!


----------------------------
2. Release Notes
----------------------------
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
  Disable the fullscreen mode, extend your desktop horizontaly, set the resolution to fill one
  screen. 
  Add to Config.ini:
  [Graphics]
  Screens=2

- Press Alt + F[1..12] in NameScreen to Save a Playername
  Press F[1..12] to Load a Playername

- To enable benchmark run the game with -benchmark parameter


----------------------------
3. Command-Line Parameters
----------------------------
Command-Line Parameters are passed to the game adding it to the Path of a Shortcut or starting the game within the console.

The following parameters are possible. They can be joined in any possible way.

-Benchmark         : Create a benchmark.log file with start timings.
-NoLog    	   : Do not create any .log files
-Joypad            : Start with Joypad support
-Language [ID]     : Load Language [ID] on startup. Example: -Language german

-Songpath [Path]   : Some as config Songpath. Example: -SongPath "C:\Ultrastar Songs"
-ConfigFile [File] : Load Configfile [File] instead of config.ini. Path to the file have to exist. Example: -ConfigFile config.SongCreation.ini
-ScoreFile [File]  : Use [File] instead of Ultrastar.db. Path to the file have to exist. Example: -ScoreFile HouseParty.db

-FullScreen        : Start the game in Fullscreen Mode
-Depth [16/32]     : Force Depth 16 or 32. Example: -Depth 16
-Resolution [ID]   : Force resolution. Example: -Resolution 800x600
-Screens [1/2]     : Force 1 or 2 Screen Mode. Example: -Screens 2

Some Examples:

Start with Resolution 1024x768 32 Bit Depth and Fullscreen:
ultrastar.exe -Resolution 1024x768 -Depth 32 -Fullscreen

Start without logging and polish Language
ultrastar.exe -NoLog -Language polish

Start with custom config File and Score DB:
ultrastar.exe -ConfigFile C:\Ultrastar\Configs\PartyConfig.ini -ScoreFile C:\Ultrastar\Scores\PartyScores.db


----------------------------
4. Controls
----------------------------
Use the Arrowkeys to navigate through the Screens.
Use Enter to select and Escape to go to the previous screen.
In Songscreen you can use R, Shift + R or Strg + R to select a random song/category
Use Alt + [Letter] to jump to a songs artist with the first letter [Letter]
Use Alt + Shift + [Letter] to jump to a song title with the first letter [Letter]
Press J to open the "Search for a Song" Interface

Editor Controls are described in documentation.pdf


----------------------------
5. License
----------------------------
Ultrastar Deluxe is licensed under the terms of the GNU General Public License 2.0
See License.txt for more Information.