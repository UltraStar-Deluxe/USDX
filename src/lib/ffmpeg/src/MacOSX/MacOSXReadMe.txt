If you are using fink to install ffmpeg and friends, 
you can skip the rest of this notes.

How to download an build ffmpeg for UltraStar Deluxe on Mac OS X:

1. Open a terminal.

2. cd into the Game/Code/lib/ffmpeg/src/MacOSX directory

3. Run the following command:

svn checkout svn://svn.mplayerhq.hu/ffmpeg/trunk ffmpeg

4. The compile ffmpeg. I made a script for this:

./build_ffmpeg.sh

5. On OS X you have to patch the the dylibs. Run the following 
   script. It patches the dylibs and copies them to the 
   lib/ffmpeg dir:

./copy_and_patch_dylibs.sh

You're done.
