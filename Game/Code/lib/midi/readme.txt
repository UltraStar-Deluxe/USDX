
Midi components
  TMidiFile, TMidiScope
  TMidiIn and TMidiOut of david Churcher are included because they are used in 
  the demo application

Freeware.

100% source code,  demo application.

Included Components/Classes

TMidiFile, read a midifile and have the contents available in memory 
  list of Tracks, track is list of events


TMidiScope, show all activity on a midi device

TMidiIn and TMidiOut of David Churcher are included because they are used 
in the demo application

Midiplayer is a demo application which plays a midifile on a midi output
 it is build fairly simple with the included components. The timer is used
 to time the midievents. The timing is therefor as good as the windows timer.


  The header of midifile,midiscope contains help information on the properties/functions
  The example Midiplayer gives a good idea how to use the components

Installation
	open midiComp.dpk with file/open
	compile and install the package
	make sure that the directory where the files are located is in the library path 	
        (tools/environment options/library)

to run the demo
	open project1.dpr in the demo directory and press run.



history
1.0  18-1-1999 first release

1.1  5-3-1999 update
  added some functions for display purposes
  improved demo to include event viewer
  bpm can be changed

1.2  24-2-2000 update
  added some functions to see the length of a song and ready function to know when playback is ready

for comments/bugs in these components:

Frans Bouwmans
fbouwmans@spiditel.nl

I'm busy building a software music synthesizer, which will be available in source 
to the public. If you are interrested in helping me with certain soundmodules 
(effects, filters, sound generators) just sent me an email.

