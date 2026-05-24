_Note: This is currently broken / in development._


Command-line parameters are passed to the game adding it to the path of a
shortcut or starting the game within the console.

The following parameters are possible. They can be joined in any possible way.

- `-help` : Show help and exit

- `-check-songs` : Check all songs at startup and log any issues

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