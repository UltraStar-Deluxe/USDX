[![Build Status](https://travis-ci.org/UltraStar-Deluxe/USDX.svg?branch=master)](https://travis-ci.org/UltraStar-Deluxe/USDX)
[![License](https://img.shields.io/badge/license-GPLv2-blue.svg)](LICENSE)

#Welcome to UltraStar Deluxe (USDX) development!

The intention of this document is to introduce you to USDX development.

###General information:
- Freepascal 3 and Lazarus 1.6 are strongly suggested for development
- Yes, Lazarus is not a good integrated development environment, but it is the best you get which also supports debugging and trial&error development for objective pascal. After some time you get used to it and you'll start to eventually even like it. Add buttons for compile, run and stop to the toolbar of the code window. Project -> Project Inspector is what shows you the various files of the project. Enable "show directory hierarchy" in the Project Inspector window. General usage is quite similar to eclipse (hold ctrl + click on a word to go to ist declaration; right click menu for jumping to certain places,…)
- Please use CamelCase naming. Classes/Units/Types/Functions should start with a capital letter and constants/values/instances should start with small letters
- You can use Windows or Linux or OS X for development just fine. See readme.md for linux / OS X install instructions.
- As soon as whatever you changed works somewhat and doesn't break much of general gameplay, please commit it to the  git code repository. If you work on bigger parts, please use a separate branch and commit your changes there often, so that your work in progress is visible. No one will judge you on work-in-progress code quality - we don't get paid for this so screw the other people if they complain ;-) .
- There is travis continuous integration set up. It will email project members if build of the master branch or some other branch is broken. This is quite helpful because linux users and other more tech savy users tend to run the most recent code so that they can use all the neat new features. Please keep them happy by not breaking too much of general singstar-like karaoke gameplay on the master branch. 
- If you start working on something that is more then just a small code change, please create an issue in the git repository issue tracker for that and assign it to yourself, so the other developers don't touch it in the mean time.
- Please write somewhat reusable code and split big problems into smaller functions. :)
- If you have questions, feel free to ask them in the irc channel or on the issue tracker.
- Don't let anyone else tell you what to work on. Do what you like and be free!
Have fun and enjoy working on a game that easily can hit 5.000 downloads in a week. This is very rewarding work and you can learn a lot!

###About the engine / used libraries:
- **SDL2** is used as general framework for spawning the window, getting keyboard / other input, and getting microphone and speaker device lists stuff. SDL2 also gets the OpenGL context for the window for us.
- **OpenGL** is used for all the graphics drawing + rendering stuff. Currently, this is mostly based on the horribly old OpenGL 1.x drawing stuff. If you know OpenGL >2.1 / OpenGL ES then please help to implement the required shading stuff and replace the old opengl instructions by the new ones. I already ported to the **dglOpenGL** library, so anything up to OpenGL 4.x should be fully supported and easily usable - just bear in mind there is lots of hardware out there that doesn't do anything newer then OpenGL 3 feature set. Also, if you don't know OpenGL that much but know SDL2 quite good, feel free to switch from directly calling OpenGL to the functions which are provided by SDL2.
- **projectM** is used for visualisation.
- **ffmpeg** is used for most video and audio stuff. On Windows, the bass library is used for playing mp3 files because of licensing issues of mp3 codecs.
- **OpenCV** highgui is used for getting images from the webcam.
- **sqlite** is used for creating and accessing databases to store song scores, already scanned songs, avatar image caches and so on. Remember that changing database setup will require adding an automatic upgrade script or resetting the database file for all users and loosing ist content.


###Where the game starts...:
At the end of the `ultrastardx.dpr` file is the program start. It calls the main function from the `src/base/UMain.pas` file, which does all the initialisation (creating the instances of the screen classes, setting up the window, OpenGL, audio and scanning the song folders for song .txt/.xml files).
After that, the game starts a while() in which it first cleans the screen, shows the last calculated new screen frame, checks for key-presses, calculates the next screen frame and then restarts the while until the user decided to close the game (->break). Everything is recalculated and freshly drawn for every single frame. (if you have some free time and know SDL2/OpenGL, feel free to improve this so that for example static background images are only sent once from system memory to graphics card memory and then drawn from there.) 
The logic code about how to handle key pressed events for each screen can be found in `src/screens/Uscreen[*].pas` (this will later on be split up into models, views and controllers by refactoring. Feel free to help)

###Where to find stuff:
- UMain: main game loop, fps limiter, game initialization, mouse + keyboard handling (which is then passed to the specific game screen UScreen...)
- UGraphic: initializes graphics stuff + much of the game initialization, gets OpenGL handle, sets up SDL window
- UDraw: draws the notes, lyrics, ...
- UScreenEdit + UScreenEditSub: songs editor stuff
- USong: reads + parses single song xml or txt files
- USongs: scans folders for song files to load, provides song filtering, category and sorting
- UIni: reads + writes settings form the config.ini file
- UTheme: loads all the theme files and processes them
- UScreenOptions...: displays the various game screens + handles mouse + keyboard input
