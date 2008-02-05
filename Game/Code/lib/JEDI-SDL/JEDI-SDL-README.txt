This is the based on the SDL ( http://www.libsdl.org ) headers, and has been converted, comments and all, to the Pascal unit called sdl.pas.
Other conversions that have also been done are SDL_Mixer.h, SDL_Net.h, SDL_Image.h, SDL_ttf, SMPEG.h, SDL_sound and the SFont library, 
which are all included in this distribution.

It allows you to access all the functions within the SDL libraries under Windows, Linux and FreeBSD, so you can write cross-platform games or multimedia applications.

Installation Instructions
-------------------------
Windows - We now have a semi-automated setup under Windows ( thanks to David House and the Jedi JCL team ). 
           Once you have extracted the zip file, simply double click on the "JEDISDLWin32Installer.exe" to have the correct paths added to your respective 
           IDEs. All IDEs from Delphi 4 - 7 are supported and it also adds a link to the .CHM help file under the Tools menu.

Linux - Alternatively if you use Linux or want to to manually install the paths, then make sure you read the "Getting Started.html" file ( ideal for those who are new to JEDI-SDL ) and is now included as a guide to help getting everything setup for smooth compilation.

Also included is a guide of how to use Sourceforge using TortoiseCVS under Windows ( Linux guide is under development ).
Both documents can be found in the "documentation" directory.


Release History
---------------
1.0  :   Yeah!! The Official v1.0 Release of JEDI-SDL!!
           JEDI-SDL now updated to SDL v1.2.11, SDL_Image v1.2.5, SDL_Mixer v1.2.7, SDL_Net v1.2.6 & SDL_ttf v2.0.8
           Added Improved FreePascal, TMT Pascal and GnuPascal support as well as maintaining Delphi/Kylix support.
           Fixed Various bugs as pointed out on the JEDI-SDL mailing list.
           Added SDL_GL_STEREO, SDL_GL_MULTISAMPLEBUFFERS, SDL_GL_MULTISAMPLESAMPLES

Now works on MacOS X and a MacOS X disk image is available for download.

// DLL/Shared object functions
function SDL_LoadObject( const sofile : PChar ) : Pointer;

function SDL_LoadFunction( handle : Pointer; const name : PChar ) : Pointer;

procedure SDL_UnloadObject( handle : Pointer );

//Added function to create RWops from const memory: SDL_RWFromConstMem()
function SDL_RWFromConstMem(const mem: Pointer; size: Integer) : PSDL_RWops;

//Added support for environment variables SDL_VIDEO_WINDOW_POS and SDL_VIDEO_CENTERED on Windows

           New Units :
           -----------
           sdl_cpuinfo.pas - ported SDL_cpuinfo.h so Now you can test for Specific CPU types.
           sdlinput.pas    - Input wrapper class
           sdlwindow.pas   - Window wrapper class
           sdltruetypefont.pas - True Type Font wrapper class
           tcputils.pas - SDL_Net utility functions
           sdlweb.pas - SDL_Net Web class
           sdlwebhttp.pas - SDL_Net http protocol wrapper class
           sdlwebftp.pas - SDL_Net ftp protocol wrapper class

           New 2D Demos :
           --------------


           New 3D Demos :
           --------------


           Other New Stuff :
           -----------------



0.5  :   The JEDI-SDL project is now also set up on Sourceforge ( http://sf.net/projects/jedi-sdl/ ) so the latest code is available from there.
           Improved FreePascal support has been added.
           Various bug fixes as pointed out on the JEDI-SDL mailing list.
           SDL_Mixer has been updated to version 1.2.1 and includes an Effects API.
           Demo directories are now split into 2D and 3D related sub-directories.
           There are now both Kylix ( K prefix )  and Delphi ( D prefix ) project groups for all the demos. 
           They can be found in Demos and the 2D and 3D directories.

           New Units
           ---------
           SDLStreams.pas - Chris Bruner has created a wrapper that uses Streams to load BMPs
           SDLUtils.pas - Pascal only version of some Utility functions
           SDLi386Utils.pas - Intel Assembler versions of the SDLUtils.pas functions.
           SDL_ttf.pas - Port of the SDL True Type font support unit.
           SDL_Sound.pas - Port of the SDL Sound library ( untested ).

           New 2D Demos :
           --------------
           Pan and Zoom Demo - How to Pan and Zoom an SDL surface.
           Isometric Demo - I ported my old DelphiX isometric demo over to SDL.
           TestTimer demo - Shows hows how to use AddTimer and RemoveTimer.
           MpegPlayer - I have updated and improved Anders Ohlsson's CLX MPegPlayer and component and it now works 
           and installs into D4, D5, D6, D7, K1, K2 & K3.
           Showfont - Demo to show how to us SDL_ttf.dll
           SmpegPlayer - is a console MPEG player that use smpeg and SDL_Mixer

           New 3D Demos :
           --------------
           DeathTruckTion 1.1 - A slightly updated version of this fully functional 3D network game.
           TerrainDemo - Terrain demo ported from the book "OpenGL Game programming" by Hawkins and Astle.
           TestGL - the standard SDL/OpenGL Test demo. Shows how to mix 2D and 3D rendering using OpenGL.
           glfont - Demo to show how to us SDL_ttf with OpenGL. 
           Particle Engine - Ariel's OpenGL Particle Engine.
           Picking - Phil Freeman's Picking Demo
           Motion Blur - Phil Freeman's Motion Blur Demo
           Dynamic Light - Phil Freeman's Dynamic Light Demo
           Environment Map - Phil Freeman's Environment Map Demo
           GLMovie - is an MPEG Player that uses OpenGL to render the movie.
           NeHe - Quite a few more NeHe demos are now included.

           New Network Demos :
           -------------------
           There are now 3 SDL_Net Server demos and 4 SDL_Client demos as submitted by Dean Ellis.


Beta 4 : The JEDI-SDL home page is now located @ http://www.delphi-jedi.org/Jedi:TEAM_SDL_HOME
         All Demos ( including OpenGL Demos ) now compile under both Kylix and Delphi.
         I have added quite a few more OpenGL examples, we are now up to Nehe tutorial 12.
         All OpenGL demos also show how to handle Window resizing. 
         Included an OpenGL demo called Puntos by Gustavo Maximo.
         Ported Jan Horn's OpenGL MetaBalls and also SkyBox demo to SDL.
         Ported Ilkka Tuomioja's OpenGL Quake 2 Model Viewer/Animator to SDL. 
         NOTE : All OpenGL demos require OpenGL12.pas which can be found at...
         http://www.lischke-online.de/Graphics.html#OpenGL12
         I also fixed a conversion bug to do with SDL_MustLock and also a conversion omission to do with various events.
         Fixed a conversion bug with SDL_CDOpen ( as suggested on the mailing list ).
         Added the GetPixel and PuxPixel functions to the SDLUtils.pas file.
         Jason Farmer has donated SFont, a simple, yet effective Font library he converted for JEDI-SDL.
         It contains 4 Demos show how to best use it.
         Added TUInt8Array and PUIntArray to SDL.pas after suggestions from Matthias Thoma and Eric Grange.
         In the file area of the JEDI-SDL mailing list ( http://groups.yahoo.com/group/JEDI-SDL/files/DTTSrc/ there 
         is a fully functional 3D network game called DeathTruckTion v1.0 written by the TNTeam that makes use of 
         JEDI-SDL and is just too big to include with this distribution but is well worth looking at as it works under Windows and Linux!         
         Gustavo Maxima is working on translating the JEDI-SDL Documentation to Spanish and Portugese.
         The Mouse Demo has now been speeded up considerably and it is very responsive now.
         Dean Ellis will provide steps on how to compile the demos using the Free Pascal compiler.
         Jason Farmer and I are working on a series of Tutorials that should hopefully be out soon.
         David Aclan has donated a SMpeg component that should work under Kylix.
         Róbert Kisnémeth, has been hard at work, and has donated some new demos he has created with a SpriteEngine ( which he also donated ).
         He has also donated a couple of games called BlitzBomber and Oxygene ( which uses the SpriteEngine ) and added a couple of useful 
         functions to SDLUtils.pas.
         The Functions added are SDL_FlipV, SDL_FlipH, SDL_NewPutPixel ( assembler version ), SDL_AddPixel, SDL_SubPixel, SDL_DrawLine, SDL_AddLine,
         SDL_SubLine, SDL_AddSurface, SDL_SubSurface, SDL_MonoSurface & SDL_TexturedSurface.
         He has also donated a Font Blitting class and demo called SDL_MonoFonts which supports alignment like Left, Right and Center.
         He and Thomas are also working on a GUI library.
         Jason Farmer has donated a set of Image Filtering functions which add quite a few interesting effects. Check the SDL_Filter sub-directory for more 
         info.
         Christian Hackbart also donated an OpenGL BlockOut clone.
         

Beta 3 : I have added conversions for SDL_env.h, SDL_Mixer.h and SDL_Net.h while Matthias Thoma has added conversions for SDL_Image.h and SMPEG.h.
         This version is also SDL version 1.2.0 compliant.
         This release also adds demos for the SDL_Image, SDL_Mixer and SDL_Net libraries.
         There are now also some OpenGL demos that make some use of SDL as well as a demo on how to use the Mouse with Clickable regions.
         A conversion bug, that was pointed out by Clem Vasseur, has also been fixed.
         There is now a mailing list that has been set up at http://groups.yahoo.com/group/JEDI-SDL/join/ so we can all learn from each other how to use
         these libraries.
         Demos have not been unified into single .dpr files for each demo, thus showing how you would write a crossplatform game using only 1 .dpr file.
         There is also a documentation directory that is currently in HTML format. All code examples in the documentation have been converted to Object 
         Pascal but are untested.
         I Also fixed a few conversion bugs which I came across while converting the documentation.

Beta 2 : I have added  conversions for SDL_active.h, SDL_thread.h, SDL_mutex.h and 
         SDL_error.h, Matthias Thoma has added Linux Support and JEDI compliancy so these 
         units and examples are now x-platform and x-compiler compilable.
         I also added Tom Jones' SDLUtils.pas file;
         Matthias also cleaned up the 2 new demos and made them work on both Linux and 
         Windows.

Beta 1 : Initial Release;


There are now 5 examples included with this JEDI-SDL distribution. 
1. Is the TestWin application, which is based on the testwin application that comes with the SDL SDK, only my version has a gui front end to the options available and has been compiled under Delphi 4.03. It should be compatible with Delphi 3.0 onwards ( though Delphi 2 compatibility has not been tested ).

2. A Plasma example which was converted from one found on the Demos page of the SDL site.

3. A Voxel terrain following demo, which was converted from one found on the Demos page of the SDL site. This one should be of interest to others as it shows how to handle keyboard events when using SDL.

4. A Mouse handling demo that shows how to use transparency and clickable regions.

5. A Space Invaders style game called Aliens which shows the use of SDL, SDL_Image and SDL_Mixer. This game shows how to handle sound, keyboards and some basic collision detection. It is a conversion of one found on the SDL Demos page.

There are also 14 OpenGL demos that are based on the NeHe tutorials <nehe.gamedev.net>. The other 3 OpenGL demos are Jan Horns' OpenGL demo, A Quake 2 Model viewer that I ported and a Demo by Gustavo Maxima called Puntos.

If writing your own, just make sure that the SDL.pas file is in your projects path for compiling and that the SDL.dll file is in your path when running the compiled app.

Please test these units and report problems to the JEDI-SDL mailing list @ http://groups.yahoo.com/group/JEDI-SDL/ outlining steps under which the error occurred. If you convert any more demos please send them to me so that I can  
include them in the ditribution for others to learn from. 

Also if you are using these Units to write any games 
please let me know about it so that I can post the information to the http://www.DelphiGamer.com site.

The plan is to have this unit JEDI certified at some point so that it can be included on the Delphi and Kylix CDs, so all feedback is greatly welcomed.

Compilers supported     Tested
-------------------     ------
Delphi                   Yes
Kylix                    Yes
FreePascal               Yes
TMT Pascal compiler      Not Yet.
Virtual Pascal           No
Gnu Pascal               No



Credits
-------
Matthias Thoma <ma.thoma@gmx.de> for is endless help with my conversion bugs.
Jason Farmer <jason@cerebral-bicycle.co.uk> for donating the SFont Font Library.
Gustavo Maximo <gmaximo@secretariaplus.com> for the Puntos OpenGL Demo and work he is doing on the documentation
Róbert Kisnémeth <mikrobi@freemail.hu> for his numerous contributions
Chris Bruner <cryst@golden.net> for testing under Kylix
August Logan Bear Jr.<augustjr@columbus.rr.com> for testing under Kylix
Dean Ellis<dean_ellis@yahoo.com> for FreePascal Compiler compatability testing and SDL_Net demos and testing
David House<david@dahsoftware.com> for Windows Insaller and testing.
Romi Kuntsman<romik12345@lycos.co.uk> for helping out on some OpenGL issues.
Everyone on the JEDI-SDL <http://groups.yahoo.com/group/JEDI-SDL/join/> mailing list for their feedback and support.
Everyone on the Delphi-JEDI <http://groups.yahoo.com/group/Delphi-JEDI/join/> mailing for answering my conversion questions.
Tom Jones for inspiring this conversion.

The JEDI-SDL Home page can be found @ http://www.delphi-jedi.org/Jedi:TEAM_SDL_HOME

The JEDI-SDL source code archive can be found @ http://www.sf.net/projects/jedi-sdl/

The JEDI-SDL mailing list can be found @ http://groups.yahoo.com/group/JEDI-SDL/join/

The Latest Stable Release version of the JEDI-SDL.zip file can always be found on the Delphi-JEDI site <http://www.delphi-jedi.org/Jedi:TEAM_SDL_HOME>

The Latest Alpha/Unstable version can always be grabbed from the SourceForge CVS http://sourceforge.net/cvs/?group_id=43805


Sincerely,



Dominique Louis
Delphi Game Developer.
*********************************************************
**  To Do Nothing is to Collaborate with the oppressor **
**  -------------------------------------------------- **
*********************************************************
=========================================================
From . . . . . . . : Dominique Louis
Email. . . . . . . : Dominique@SavageSoftware.com.au
Company. . . . . . : Savage Software Solutions
Delphi Games Site. : http://www.DelphiGamer.com
Delphi JEDI Site . : http://www.delphi-jedi.org
=========================================================

