-------------------------------------------------------------------------------
                        (: -= BASS_FX v2.4.11.1 =- :)
                             -=================-
            Copyright (c) 2002-2014 JOBnik! [Arthur Aminov, ISRAEL]
                             All rights reserved!
-------------------------------------------------------------------------------

Donate
======
If you like BASS_FX or use it in a commercial/shareware products,
then you may wish to make a donation to support its development (see the docs for info).

Thank you!


Files that you should have found in the BASS_FX "package"
=========================================================
Win32 version
-------------
BASS_FX.TXT      This file
BASS_FX.DLL      The BASS_FX module
BASS_FX.CHM      BASS_FX documentation
File_ID.Diz      BBS description file

x64\
  BASS_FX.DLL    64-bit BASS_FX module

C\               C/C++ APIs and samples
  BASS_FX.H        BASS_FX C/C++ header file
  BASS_FX.LIB      BASS_FX import library
  x64\
    BASS_FX.LIB    64-bit BASS_FX import library
  SAFESEH\
    BASS_FX.LIB    SAFESEH compatible BASS_FX import library

  bpm\             BPM example
    bpm.c
    bpm.h
    bpm.rc

  dsp\             DSP example
    dsp.c
    dsp.h
    dsp.rc

  freeverb\        Freeverb example
    freeverb.c
    freeverb.h
    freeverb.rc

  reverse\         Reverse example
    reverse.c
    reverse.h
    reverse.rc

  tempo\           Tempo example
    tempo.c
    tempo.h
    tempo.rc

  bin\             Precompiled examples
    bpm.exe
    dsp.exe
    reverse.exe
    tempo.exe

Delphi\   Delphi APIs and samples
  BASS_FX.PAS      BASS_FX Delphi APIs Unit

  BPM\     BPM example
    bpm.dfm          main form
    bpm.pas          main unit
    prjBPM.dpr       project file

  DSP\             DSP example
    dsp.dfm
    dsp.pas
    prjDSP.dpr

  Reverse\         Reverse example
    prjRev.dpr
    Reverse.dfm
    Reverse.pas

  Tempo\           Tempo example
    prjTempo.dpr
    tempo.dfm
    tempo.pas

VB\              Visual Basic APIs and samples
  BASS_FX.BAS      BASS_FX VB APIs Module

  BPM\             BPM example
    frmBPM.frm       main form
    modBPM.bas       module with some functions
    prjBPM.vbp       project file

  DSP\             DSP example
    frmDSP.frm
    prjDSP.vbp

  Reverse\         Reverse example
    frmREVERSE.frm
    prjREVERSE.vbp

  Tempo\           Tempo example
    frmTempo.frm
    prjTepmo.vbp


NOTE: To run the sample EXEs, first you'll have to copy BASS_FX.DLL into
      the same directory as the EXEs. You'll also need BASS.DLL which can
      be download from the BASS website.

NOTE: To build the examples, you'll need to copy the BASS API into the
      same directory as the BASS_FX API.


MacOSX version
--------------
BASS_FX.TXT      This file
LIBBASS_FX.DYLIB  The BASS_FX module
BASS_FX.CHM      BASS_FX documentation
BASS_FX.H        BASS_FX C/C++ header file
MAKEFILE         Makefile for all examples
MAKEFILE.IN      Makefile helper macros

reverse\         Reverse example
  reverse.c
  makefile
  reverse.nib

tempo\           Tempo example
  tempo.c
  makefile
  tempo.nib


NOTE: To view the documentation, you will need a CHM viewer, like CHMOX
      which is included in the BASS package.

NOTE: To build the examples, you'll need to copy the BASS API into the
      same directory as the BASS_FX API.


What's the point?
=================
BASS_FX is an extension to the BASS audio library, providing a complete
set of Real-time DSP functions to start developing your own DJ software ;)
Developed to enable the simple and advanced developers/users to 
have in their applications sound effects without knowing any DSP at all!


Requirements
============
BASS 2.4 is required, the BASS_FX module will fail to load if it is not present.

MacOSX version
--------------
OSX 10.3.9 or above is recommended. BASS_FX is compatible with both
PowerPC and Intel Macs.


Using BASS_FX
=============
Win32 version
-------------
To use BASS_FX with Borland C++ Builder, you'll first have to create a
Borland C++ Builder import library for it. This is done by using the
IMPLIB tool that comes with Borland C++ Builder. Simply execute this:

 IMPLIB BASS_FXBCB.LIB BASS_FX.DLL

... and then use BASS_FXBCB.LIB in your projects to import BASS_FX.

To use BASS_FX with LCC-Win32, you'll first have to create a compatible
import library for it. This is done by using the PEDUMP and BUILDLIB
tools that come with LCC-Win32. Run these 2 commands:

 PEDUMP /EXP BASS_FX.LIB > BASS_FXLCC.EXP
 BUILDLIB BASS_FXLCC.EXP BASS_FXLCC.LIB

... and then use BASS_FXLCC.LIB in your projects to import BASS_FX.

TIP: The BASS_FX.CHM file should be put in the same directory as the BASS.CHM
     file, so that the BASS_FX documentation can be accessed from within the
     BASS documentation.

MacOSX version
--------------
A separate  "LIB" file is not required for OSX. Using XCode, you can simply
add the DYLIB file to the project. Or using a makefile, you can build your
programs like this, for example:

 gcc yoursource -L. -lbass -lbass_fx -o yourprog

As with LIBBASS.DYLIB, the LIBBASS_FX.DYLIB file must be put in the same
directory as the executable (it can't just be somewhere in the path). See
the example makefiles.

LIBBASS_FX.DYLIB is a universal binary, with support for both PowerPC and
Intel Macs. If you want PowerPC-only or Intel-only versions, the included
makefile can create them for you, by typing "make ppc" or "make i386".


Latest Version
==============
The latest versions of BASS_FX & BASS can be found at these websites:

        http://www.un4seen.com          (the home of BASS)
        http://www.jobnik.org


Copyright, Disclaimer, and all that other jazz
==============================================
The BASS_FX library is free, so if anyone tries to charge you
for it, kick 'em where it hurts.

This software is provided "as is", without warranty of ANY KIND,
either expressed or implied, including but not limited to the
implied warranties of merchantability and/or fitness for a
particular purpose. The author shall NOT be held liable for
ANY damage to you, your computer, or to anyone or anything else,
that may result from its use, or misuse. Basically, you use it
at YOUR OWN RISK.

Usage of BASS_FX indicates that you agree to the above conditions.

You may freely distribute the BASS_FX package as long as NO FEE is
charged and all the files remain INTACT AND UNMODIFIED.

All trademarks and other registered names contained in the BASS_FX
package are the property of their respective owners.


History
=======
These are the major (and not so major) changes at each version
stage. There are ofcourse bug fixes and other little improvements
made along the way too! To make upgrading simpler, all functions
affected by a change to the BASS_FX interface are listed.

2.4.11.1 - 24/12/2014
---------------------
* BASS_FX:
  * Added support for BASS 2.4.11, BASS_DATA_FIXED flag is used in Android, WinCE and Linux ARM platforms.
  * Documentation is updated.
  * iOS, added "x86_64" simulator architecture.

* Tempo:
  * Fixed "Segmentation fault" on x64 Linux system.

* DSP:
  * Android, fixed "BASS_FX_BFX_ECHO4" effect as reported in this thread:
    http://www.un4seen.com/forum/?topic=13225.msg112373#msg112373

2.4.11 - 02/09/2014
-------------------
* BASS_FX:
   * Fixed an issue on OSX (The Xcode project's "Installation Directory" setting set to "@loader_path" instead of "@executable_path").
   * Tempo/Reverse fixed a thread-safety crash.
   * Tempo/Reverse added CTYPE info for these streams.
   * Android, added "x86" architecture support (some effects are buggy, will be fixed soon).
   * iOS, added "arm64" architecture support.
   * Fixed Delphi/Pascal unit by changing "FLOAT" to "Single".
   * Added C/C++ "freeverb" example.

* Tempo:
   * Updated to latest SoundTouch library version 1.8.0
   * Fixed a crash when using syncs on tempo, as described in this thread: http://www.un4seen.com/forum/?topic=15708.0
   * Added 3 interpolation algorithms to set using BASS_FX_TEMPO_ALGO_XXX flags (BASS_FX_TempoCreate):
      * BASS_FX_TEMPO_ALGO_LINEAR
      * BASS_FX_TEMPO_ALGO_CUBIC   (default)
      * BASS_FX_TEMPO_ALGO_SHANNON

* DSP:
   * Fixed a bug in BASS_FX_BFX_VOLUME_ENV effect with the "bFollow" option on mobile devices,
     as described in this thread http://www.un4seen.com/forum/?topic=15866
   * Added new effects:
     * BASS_FX_BFX_PITCHSHIFT, that uses FFT for its pitch shifting while maintaining duration.
     * BASS_FX_BFX_FREEVERB, a reverb effect.

2.4.10.1 - 05/06/2013
---------------------
* BPM/Beat:
  * Fixed a crash when not using BPMPROGRESSPROC callback
  * Added a check for BPMPROC/BPMBEATPROC callbacks, if not available, returns BASS_ERROR_ILLPARAM error code

2.4.10 - 02/06/2013
-------------------
* BASS_FX:
   * Please see DSP and BPM sections for decprecated effects/functions.
     To not break compatibility with BASS 2.4 version, these effects and functions will still remain in BASS_FX,
     but are removed from documentations and will be totally removed from BASS_FX in the future.
   * BASS_ERROR_FX_NODECODE error code *removed*, will return BASS_ERROR_DECODE instead (Tempo/Reverse/BPM/Beat)
   * BASS_ERROR_FX_BPMINUSE error code *removed*, will return BASS_ERROR_ALREADY instead (BPM/Beat)
   * Tempo and BPM functions updated to latest SoundTouch library version 1.7.1
   * Added BASS_BFX_Linear2dB and BASS_BFX_dB2Linear macros to headers, for convenience.

* Tempo:
  * Multi-channel support is added, but:
     * No SSE optimizations
     * BASS_ATTRIB_TEMPO_OPTION_USE_AA_FILTER is by default set to FALSE on iOS, Android, WinCE and Linux ARM platforms for lower CPU usage
     * Not part of SoundTouch library, sources will be sent to Olli Parviainen after BASS_FX release
  * Sound quality improvements
  * Improved output sound stream duration to match better with ideal duration
  * Fixed BASS_ERROR_UNKNOWN issue with Windows 8 x64, posted in this thread:
    http://www.un4seen.com/forum/?topic=14499.0

* BPM:
  * BASS_FX_BPM_Translate and all of its options, BASS_FX_BPM_TRAN_xxx, are *deprecated*
  * BPMPROCESSPROC *renamed* to BPMPROGRESSPROC
  * BPM example is updated to fit above changes
  * Tuned detection algorithm
  * Fixed detection bug in Android, WinCE & Linux ARM platforms, for returning odd values
  * Fixed percents bug in BPMPROGRESSPROC
  * Changed default min/max BPM window to SoundTouch's 29/200

* Beat:
  * Fixed regression since version 2.4.8 in BASS_FX_BPM_BeatDecodeGet function,
    that would free a "chan" when detection is completed, as described in this thread:
    http://www.un4seen.com/forum/?topic=2181.msg102805#msg102805

* DSP:
  * Ported all effects to Android, WinCE & Linux ARM platforms

  * BASS_FX_BFX_ROTATE:
        added new structure "BASS_BFX_ROTATE" with params:
        "fRate"    - set the rotation rate/speed in Hz between channels
        "lChannel" - multi-channel support, only for even number of channels

  * BASS_FX_BFX_ECHO4:
        added new effect and structure "BASS_BFX_ECHO4" with params:
        "fDryMix"   - unaffected signal mix
        "fWetMix"   - affected signal mix
        "fFeedback" - output signal to feed back into input
        "fDelay"    - delay seconds
        "bStereo"   - even channels are echoed to each other if enabled
        "lChannel"  - multi-channel support

  * BASS_FX_BFX_ECHO        - *deprecated*, use BASS_FX_BFX_ECHO4
  * BASS_FX_BFX_ECHO2       - *deprecated*, use BASS_FX_BFX_ECHO4
  * BASS_FX_BFX_ECHO3       - *deprecated*, use BASS_FX_BFX_ECHO4
  * BASS_FX_BFX_REVERB      - *deprecated*, use BASS_FX_BFX_ECHO4 with fFeedback enabled
  * BASS_FX_BFX_FLANGER     - *deprecated*, use BASS_FX_BFX_CHORUS
  * BASS_FX_BFX_COMPRESSOR  - *deprecated*, use BASS_FX_BFX_COMPRESSOR2
  * BASS_FX_BFX_APF         - *deprecated*, use BASS_FX_BFX_BQF with BASS_BFX_BQF_ALLPASS filter
  * BASS_FX_BFX_LPF         - *deprecated*, use 2x BASS_FX_BFX_BQF with BASS_BFX_BQF_LOWPASS filter and appropriate fQ values

2.4.9 - 16/01/2013
------------------
* BASS_FX:
  * WinCE version introduced (package bass_fx24-ce.zip) currently doesn't include most BASS_FX_BFX_xxx effects.
  * Linux ARM version introduced (package bass_fx24-linux-arm.zip) currently doesn't include most BASS_FX_BFX_xxx effects.

* DSP:
  * Added more effects to Android, WinCE & Linux ARM ports:
     BASS_FX_BFX_PEAKEQ
     BASS_FX_BFX_MIX
     BASS_FX_BFX_VOLUME_ENV

* Tempo and Reverse:
  * According to this request: http://www.un4seen.com/forum/?topic=13910
    Added support for DECODETO option.

* Tempo:
  * iOS, WinCE & Linux ARM: Enabled the BASS_ATTRIB_TEMPO_OPTION_USE_QUICKALGO option on tempo
    streams by default for lower CPU usage. See docs on how to disable it.

* iOS: Added armv7s architecture support.

2.4.8 - 31/07/2012
------------------
* BASS_FX:
  * Android version introduced (package bass_fx24-android.zip)
    currently doesn't include most BASS_FX_BFX_xxx effects.
  * Delphi/Pascal unit: changed "user" param from "DWORD" to "Pointer"

* BPM:
  * Added "user" param to BASS_FX_BPM_DecodeGet and BPMPROCESSPROC (you have to recompile your project).
  * BPM example is updated to fit above changes.
  * According to this request: http://www.un4seen.com/forum/?topic=13319
    Added support for BPM detection from the current position with BASS_FX_BPM_DecodeGet (startSec<0). 
    
* Beat:
  * Couple of little fixes in beat detection.

* Tempo:
  * Android: Enabled the BASS_ATTRIB_TEMPO_OPTION_USE_QUICKALGO option on tempo
    streams by default for lower CPU usage. See docs on how to disable it.

2.4.7.1 - 01/07/2011
--------------------
* BASS_FX:
   * Documentation updated.

* DSP:
   * Fixed a small issue in BASS_FXGetParameters for BASS_FX_BFX_VOLUME effect, as it would change the lChannel value
     when the global volume (lChannel=0) is requested.

* OSX:
   * x86_64 architecture support

2.4.7 - 07/04/2011
------------------
* BASS_FX:
  * Delphi unit updated: BASS_BFX_ENV_NODE = record, changed to BASS_BFX_ENV_NODE = packed record

* DSP:
   * Fixed bug in BASS_FX_BFX_VOLUME_ENV, being applied slightly early when "bFollow" is enabled.
   * BASS_FX_BFX_DAMP made effect parameter setting a bit more convenient. If fGain < 0 then leave the current value.

* iOS:
   * Added armv7 architecture support.
   * Combined the Device armv6/7 architectures and Simulator libraries into a single file.

2.4.6 - 27/07/2010
------------------
* BASS_FX:
   * Tempo and BPM updated to SoundTouch 1.5.1pre
   * iPhone version introduced (package bass_fx24-iphone.zip)
     NOTE: Since iPhone supports only static libraries, it isn't possible for static libraries to declare their dependencies,
           so that needs to be done in the app's project instead, eg. by adding "-lstdc++" in the "Other Linker Flags".
* Tempo:
   * Updated automatic parameter adjustment logic to reduce tone wobbling at large tempo changes.
   * Retired 3DNow! optimization support as 3DNow! is nowadays obsolete and assembler code is nuisance to maintain.

* BPM:
   * Improved BPM detection.
   * Added automatic cutoff threshold adaptation to beat detection routine to better adapt BPM calculation to different types of music.
   * Fixed bug in BPMPROCESSPROC percents, when endSec is greater than stream's length.

* Reverse:
   * Fixed bug with getting the position when using a large output buffer (BASS_CONFIG_BUFFER).
   * Fixed bug in BASS_ChannelGetPosition, could return an incorrect position.

2.4.5 - 18/12/2009
------------------
* DSP: Added new effect, BiQuad filters, BASS_FX_BFX_BQF with filter types:
        BASS_BFX_BQF_LOWPASS
        BASS_BFX_BQF_HIGHPASS
        BASS_BFX_BQF_BANDPASS
        BASS_BFX_BQF_BANDPASS_Q
        BASS_BFX_BQF_NOTCH
        BASS_BFX_BQF_ALLPASS
        BASS_BFX_BQF_PEAKINGEQ
        BASS_BFX_BQF_LOWSHELF
        BASS_BFX_BQF_HIGHSHELF

* Tempo:
   * Tempo processing bypassed when tempo/pitch set to 0
   * Couple of improvements:
     * Added normalization to correlation calculation
     * Heuristic that weights center of the processing window

2.4.4.1 - 29/04/2009
--------------------
* Tempo: Fixed a bug that could cause a stream to end slightly early.

2.4.4 - 28/03/2009
------------------
* BASS_FX:
  * Added: Linux x64 build in bass_fx24-linux.zip package.

* Tempo:
  * Updated to most latest SoundTouch library 1.4.1
  * Improved sound quality by automatic calculation of time stretch algorithm processing
    parameters according to tempo setting.
  * Added new BASS_ATTRIB_TEMPO_OPTION_PREVENT_CLICK, to prevent click when
    samplerate/pitch crosses the default value during processing.
    Default is FALSE as this involves slight sound quality compromise.

* BPM/Beat:
  * Fixed a small bug of internally called SETPOS sync.

2.4.3.1 - 07/01/2009
--------------------
* BASS_FX: Delphi/Pascal unit updated to handle both Windows and Linux
* Tempo: Fixed a small bug in processing with BASS_SAMPLE_LOOP flag

2.4.3 - 24/12/2008
------------------
* BASS_FX:
  * Mixtime POS syncs are now triggered when the specified position is
    rendered in the tempo/reverse stream (rather than when it is decoded from the source).
  * Linux version introduced (package bass_fx24-linux.zip) // examples will follow

* Tempo:
  * Corrected BASS_ATTRIB_TEMPO_FREQ min/max values.

* DSP:
  * Added new volume effect: BASS_FX_BFX_VOLUME_ENV, a volume effect using an envelope.
  * BASS_FX_BFX_APF, BASS_FX_BFX_ECHO2, BASS_FX_BFX_ECHO3: fDelay 6 seconds limit removed.

* BPM:
  * Improved the peak detection algorithm so that it wouldn't incorrectly report too slow
    harmonic beats if they are close to the true base beat.

2.4.2 - 16/08/2008
------------------
* BASS_FX:
  * Some processing functions optimized for speed.

* DSP:
  * Added new compressor effect BASS_FX_BFX_COMPRESSOR2
    For compatibility issues new compressor will replace old one in version 2.5

2.4.1 - 28/06/2008
------------------
* Tempo:
  * Fixed a bug in BASS_ChannelGetPosition, that would return a
    lower position than it should with a decoding tempo stream.

* BPM:
  * Multi-channel support.
  * Fixed a bug in BASS_FX_BPM_DecodeGet, that would return 0 if using
    the same handle and endSec for both Callback and Decode BPM.

2.4.0.2 - 17/04/2008
--------------------
* BPM: fixed another bug in BPMPROC
* Tempo: fixed a bug not allowing changing BASS_ATTRIB_TEMPO_OPTION_xxx

2.4.0.1 - 06/04/2008
--------------------
* BPM: fixed a bug in BPMPROC

2.4 - 02/04/2008
----------------
* BASS_FX:
  * Updated to BASS 2.4
  * More integrated with BASS plug-in system.
  * Added a function BASS_FX_GetVersion
  * BASS_FX_CONFIG_DSP_RESET *removed*
  * Error codes and names changed
  * To be able to link with BASS_FX, you'll have to call BASS_FX_GetVersion function
    (or any other function from BASS_FX.DLL) or load it dynamically using LoadLibrary("bass_fx.dll")
  * Win64 version introduced (package bass_fx24-x64.zip)

* DSP:
  * Effect names, structure names, flags and chain order are changed
     e.g: BASS_FX_DSPFX_PHASER -> BASS_FX_BFX_PHASER
          BASS_FX_DSPPHASER -> BASS_BFX_PHASER
  * All functions integrated with BASS FX functions and the usage is the same
    as with BASS DX8/DMO effects:

     BASS_FX_DSP_Set *removed* (use BASS_ChannelSetFX)
     BASS_FX_DSP_SetParameters *removed* (use BASS_FXSetParameters)
     BASS_FX_DSP_GetParameters *removed* (use BASS_FXGetParameters)
     BASS_FX_DSP_Reset *removed* (use BASS_FXReset)
     BASS_FX_DSP_Remove *removed* (use BASS_ChannelRemoveFX)

  * It is possible now to apply an effect more than once on the same channel.
  * BASS_FX_BFX_SWAP *removed* (use BASS_FX_BFX_MIX)
  * BASS_FX_BFX_S2M:
     Updated to support multi-channel and renamed to BASS_FX_BFX_MIX
     BASS_FX_BFX_MIX supports Swap, remap and mixing channels together.
  * BASS_FX_BFX_PEAKEQ:
    * fFreq param *removed*
      (use 'oldcenter*freq/oldfreq' to update the fCenter after changing the Samplerate)
    * Max fCenter updated from less than 1/3 to 1/2 of info.freq
    * Take a look at DSP example to know how to increase the number of bands

  * BASS_FX_BFX_LPF:
    * fFreq param *removed* (adjust fCutOffFreq param when needed)

* Tempo:
  * Support for all source sync types
  * Funtions integrated with BASS attribute system (BASS_ChannelSet/GetAttribute):
     BASS_FX_TempoSet *removed*
     BASS_FX_TempoGet *removed*
     BASS_FX_TempoSetOption *removed*
     BASS_FX_TempoGetOption *removed*

    * Tempo attributes:
       BASS_ATTRIB_TEMPO
       BASS_ATTRIB_TEMPO_PITCH
       BASS_ATTRIB_TEMPO_FREQ

    * Option attributes:
       BASS_ATTRIB_TEMPO_OPTION_USE_AA_FILTER
       BASS_ATTRIB_TEMPO_OPTION_AA_FILTER_LENGTH
       BASS_ATTRIB_TEMPO_OPTION_USE_QUICKALGO
       BASS_ATTRIB_TEMPO_OPTION_SEQUENCE_MS
       BASS_ATTRIB_TEMPO_OPTION_SEEKWINDOW_MS
       BASS_ATTRIB_TEMPO_OPTION_OVERLAP_MS

* Reverse:
  * Support for all source sync types
  * Funtions integrated with BASS attribute system (BASS_ChannelSet/GetAttribute):
     BASS_FX_ReverseSetDirection *removed*
     BASS_FX_ReverseGetDirection *removed*

    * Reverse attribute:
       BASS_ATTRIB_REVERSE_DIR

* BPM:
  * Seconds changed from "float" to "double"
  * Callback "user" parameters changed to pointers: BASS_FX_BPM_CallbackSet / BPMPROC
  * Translation names changed:
     BASS_FX_BPM_X2       -> BASS_FX_BPM_TRAN_X2
     BASS_FX_BPM_2FREQ    -> BASS_FX_BPM_TRAN_2FREQ
     BASS_FX_BPM_FREQ2    -> BASS_FX_BPM_TRAN_FREQ2
     BASS_FX_BPM_2PERCENT -> BASS_FX_BPM_TRAN_2PERCENT
     BASS_FX_BPM_PERCENT2 -> BASS_FX_BPM_TRAN_PERCENT2

* Beat:
  * Multi-channel support
  * Seconds changed from "float" to "double"
  * "cutofffreq" param renamed to "centerfreq"
  * Callback "user" parameters changed to pointers:
     BASS_FX_BPM_BeatCallbackSet / BPMBEATPROC
     BASS_FX_BPM_BeatDecodeGet / BPMBEATPROC

2.3.0.4 - 30/10/07
------------------
* DSP:
  * Fixed: a bug in BASS_FX_DSPFX_DAMP to avoid trying to amplify silence data
  * Fixed: a bug in BASS_FX_DSPFX_PEAKEQ to check illegal Center Frequencies (fCenter)

* Tempo:
  * Fixed: a bug that would prevent using a BASS_SYNC_MESSAGE sync
  * Fixed: a bug that would prevent triggering a BASS_SYNC_END sync

* Reverse:
  * Fixed: a bug that would prevent using a BASS_SYNC_MESSAGE sync

2.3.0.3 - 08/08/2007
--------------------
* BASS_FX:
  * Fixed: a DEP crashing bug.
  * Added: a Config option, to reset DSPs, BPM/Beat Callbacks when position is set.
           use BASS_FX_CONFIG_DSP_RESET with BASS_Set/GetConfig function.

* DSP:
  * Fixed: bugs in functions:
      BASS_FX_DSP_Set and BASS_FX_DSP_Remove

  * Fixed: bugs in DSP effects:
      BASS_FX_DSPFX_PEAKEQ
      BASS_FX_DSPFX_ECHO
      BASS_FX_DSPFX_ECHO2
      BASS_FX_DSPFX_REVERB
      BASS_FX_DSPFX_VOLUME
      BASS_FX_DSPFX_DAMP

  * Removed: DSPFX.TXT file, please check the docs for DSP effects/DSP Values

* TEMPO:
  * Fixed: a floating-point bug when calling BASS_FX_TempoCreate
  * Fixed: POS SYNCs to be more accurate

* BPM:
  * Fixed: a bug in BASS_FX_BPM_Free and BASS_FX_BPM_BeatFree
           that would sometimes release the source channnel as well
           without using BASS_FX_FREESOURCE flag.

* REVERSE:
  * Fixed: a bug that, if you would set the direction to forward before starting playback,
           the position would keep counting from the end.
  * Updated: When reaching the end of the stream, changing the direction will now
             reset the stream, so that it can be played again.
  * Fixed: POS SYNCs to be more accurate

2.3.0.2 - 09/04/2007
--------------------
* BEAT:
  * Added Beat position detection for decoded streams
     BASS_FX_BPM_BeatDecodeGet

  * Added new functions:
     BASS_FX_BPM_BeatCallbackReset
     BASS_FX_BPM_BeatSetParameters
     BASS_FX_BPM_BeatGetParameters

* REVERSE:
  * Multi-channel support
  * Added new feature to change playing direction from Reverse to Forward and vice-versa
     BASS_FX_ReverseSetDirection
     BASS_FX_ReverseGetDirection

* BPM:
  * Fixed: one more critical bug in BPM functions

* DSP:
  * BASS_FX_DSPFX_PEAKEQ:
     Improved effect and reduced CPU usage
     Fixed: a bug, preventing using fQ if fBandwidth < 0.1f
     Changed: fQ min limit to 0.1f

  * BASS_FX_DSPFX_ECHO3:
     Fixed: a bug in BASS_FX_DSP_GetParameters, that would return a wrong lChannel value

  * Error code: BASS_FX_ERROR_STEREO *removed* (replaced with BASS_ERROR_FORMAT)

* TEMPO:
  * Setting functions name changed:
     BASS_FX_TempoSettingSet -> BASS_FX_TempoSetOption
     BASS_FX_TempoSettingGet -> BASS_FX_TempoGetOption

     BASS_FX_TEMPO_SETTING_xxx -> BASS_FX_TEMPO_OPTION_xxx

2.3.0.1 - 08/06/2006
--------------------
* New in BASS_FX:
  * Added a valid parameters check for all functions.

* BPM:
  * A little improved Beat position trigger.
  * Changing buffer content won't affect the BPM/Beat detection anymore.
  * BASS_Stream/MusicFree will free the callback BPM/Beat as well.

* BASS_FX.CHM:
  * Added a very simple example to BPMBEATPROC callback, showing how to
    count the BPM with just 2 beats.

* DSP:
  * Fixed small bugs in BASS_FX_DSPFX_VOLUME effect.

2.3 - 21/05/2006
----------------
* New in BASS_FX:
  * This version has some API changes.
  * You'll have to recompile your application to use this version.
  * =====
     If you like BASS_FX or use it in a commercial/shareware products,
     then you may wish to make a donation to support its development (see the docs for info).
    =====

* Tempo:
  * Fixed a bug, that wouldn't clear buffers if a source channel isn't seekable.
  * Removed flags:
     BASS_FX_TEMPO_QUICKALGO
     BASS_FX_TEMPO_NO_AAFILTER

    you can set these using a function below, in real-time.

  * Added 2 new functions:
     BASS_FX_TempoSettingSet
     BASS_FX_TempoSettingGet

    with options (check the docs for more info about using them):
     BASS_FX_TEMPO_SETTING_USE_AA_FILTER
     BASS_FX_TEMPO_SETTING_AA_FILTER_LENGTH
     BASS_FX_TEMPO_SETTING_USE_QUICKALGO
     BASS_FX_TEMPO_SETTING_SEQUENCE_MS
     BASS_FX_TEMPO_SETTING_SEEKWINDOW_MS
     BASS_FX_TEMPO_SETTING_OVERLAP_MS

* DSP:
  * Added new struct:
     BASS_FX_DSPSWAP

  * Added multi-channel support and a per channel control with flags/macro:
    each effect with a per channel control has a new "lChannel" param
    (if you won't set the new param, then the effect will be affected on all channels as by default)

     BASS_FX_DSPFX_SWAP  -> it's now possible not only swap, but remap as well.
     BASS_FX_DSPFX_FLANGER
     BASS_FX_DSPFX_VOLUME -> it's now needed to set a global volume, before boosting.
     BASS_FX_DSPFX_PEAKEQ
     BASS_FX_DSPFX_LPF
     BASS_FX_DSPFX_DAMP
     BASS_FX_DSPFX_AUTOWAH
     BASS_FX_DSPFX_ECHO2
     BASS_FX_DSPFX_PHASER
     BASS_FX_DSPFX_ECHO3
     BASS_FX_DSPFX_CHORUS
     BASS_FX_DSPFX_APF
     BASS_FX_DSPFX_COMPRESSOR
     BASS_FX_DSPFX_DISTORTION


  * Channel flags (check the docs for channels order):
     BASS_FX_DSP_CHANALL
     BASS_FX_DSP_CHANNONE
     BASS_FX_DSP_CHAN1 .. BASS_FX_DSP_CHAN8

    * If you have more than 8 channels (7.1), use this macro.
      BASS_FX_DSP_CHANNEL_N(n)

  * Added a DENORMAL check for all effects.
  * BASS_FX_DSP_Reset is updated for all effects.

* BPM:
  * Added Real-Time Beat Position Trigger:
     BASS_FX_BPM_BeatCallbackSet
     BASS_FX_BPM_BeatFree

  * BASS_FX_ERROR_BPMX2 error code *removed* and *replaced* with BASS_ERROR_ALREADY
  * Fixed bugs:
    * Serious memory-leak is fixed using both options.
    * A bug that would free resources before the detecting process is finished.
    * A bug that would still continue to detect previous data even if changing file to
      scan, using a BASS_FX_BPM_BKGRND flag.
    * A bug that would still return BPMs out of MIN/MAX range if using BASS_FX_BPM_MULT2 flag.

2.2.0.1 - 30/11/2005
--------------------
* New in BASS_FX:
  * 8-bit support.
  * Added more DSP effect information to BASS_FX.CHM

* DSP:
  * Automatically free DSP resources when freeing the channel.
  * Multi-channel support started with:
    * BASS_FX_DSPFX_ECHO2
    * BASS_FX_DSPFX_ECHO3
    + more effects will be updated soon! :)
  * Some bugs fixed.

* BPM:
  * Fixed a bug that would return BPMs out of MIN/MAX range if
    using BASS_FX_BPM_MULT2 flag.

* MacOSX:
  * Samples added.

2.2 - 03/10/2005
----------------
* New in BASS_FX:
  * Removed all DSP GPL code.
  * BASS_FX is now fully useable in commercial software, as long as
    credit is given.
  * BASS_FX_GetVersion() *removed* (won't load if BASS 2.2 isn't present)
  * BASS_FX_ERROR_MODLEN *removed* (replaced with BASS_ERROR_NOTAVAIL)
  * BASS_FX_ERROR_16BIT *removed* (no 16-bit only effects are left)
  * Multi_FX example *removed*
  * MacOSX port introduced

* DSP:
  * Removed GPL FX:
    * BASS_FX_DSPFX_FLANGER2 & BASS_FX_DSPFX_CUT
      - because of that the DSP chain is changed!

* Reverse:
  * MOD playback is now supported if using BASS_MUSIC_PRESCAN flag.

* MacOSX examples will follow this week.

2.1.0.2 - 07/05/2005
--------------------
* DSP:
  * Chorus: fixed a bug, that would convert stereo to mono.
  * Low Pass Filter: fixed a bug, that would convert stereo to mono.
  * DynamicAMP: another bug fix, that would sometimes cause a total silence.
  + Added:
    * A new effect: Distortion
    * DSPFX.TXT - a values to use with some effects, to achieve different
                  effect with the same one (not using other effect/s) :)

2.1.0.1 - 22/02/2005
--------------------
* New in BASS_FX:
  + Added:
    * File version info.
    * Documentation file BASS_FX.CHM.

* DSP:
  * Another DynamicAMP bug fix.
  * Added a new effect: Compressor

* BPM:
  * Added: "User" param to Callback BPM functions.

2.1 - 27/12/2004 - Happy New Year ;)
------------------------------------
* New in BASS_FX:
  * No more "alpha/beta" releases! :)
  * Updated to BASS 2.1 add-on APIs, coz of that BASS_FX is not compatible with any 
    previous versions. You'll have to make some changes in your project.
  * Full 32-bit floating-point support.
  * Sync support, "Sync & Tempo" example *removed*
  * A lot of functions/error codes removed and integrated with BASS functions/error codes.
  * New flag: BASS_FX_FREESOURCE if you want BASS_FX to free the source handle as well.
  * New error code: BASS_FX_ERROR_16BIT for Flanger 2.
  * BASS_FX_ErrorGetCode *removed* (use BASS_ErrorGetCode)
  * BASS_FX_Free *removed*

* Tempo:
  * BASS_FX_TempoGetResampledHandle *removed*
  * BASS_FX_TempoStopAndFlush *removed*
  * BASS_FX_TempoFree *removed* (use BASS_StreamFree for music as well)
  * BASS_FX_TempoGetApproxSeconds *removed* (use BASS_FX_TempoGetRateRatio to calculate)
  * BASS_FX_TempoGetApproxPercents *removed* (use BASS_FX_TempoGetRateRatio to calculate)
  + New functions:
     BASS_FX_TempoGetSource (get the source handle when needed)
     BASS_FX_TempoGetRateRatio
  + New in flags:
     BASS_FX_TEMPO_QUICKSEEK *renamed* to BASS_FX_TEMPO_QUICKALGO
     BASS_FX_TEMPO_NO_AAFILTER *added*
  * 3DNow! & SSE support.

* Reverse:
  * BASS_FX_ReverseGetReversedHandle *removed*
  * BASS_FX_ReverseSetPosition *removed* (use BASS_ChannelSetPosition)
  * BASS_FX_ReverseFree *removed* (use BASS_StreamFree)
  * BASS_FX_ReverseCreate: "decode" param *removed* (use BASS_STREAM_DECODE flag)
  * New function: BASS_FX_ReverseGetSource (get the source handle when needed)

* BPM:
  + These functions are combined to one: BASS_FX_BPM_Translate
     * BASS_FX_BPM_X2 *removed*
     * BASS_FX_BPM_Frequency2BPM *removed*
     * BASS_FX_BPM_2Frequency *removed*
     * BASS_FX_BPM_Percents2BPM *removed*
     * BASS_FX_BPM_2Percents *removed*

    + Use these translation options with a function above:
       BASS_FX_BPM_X2
       BASS_FX_BPM_2FREQ
       BASS_FX_BPM_FREQ2
       BASS_FX_BPM_2PERCENT
       BASS_FX_BPM_PERCENT2

* DSP:
  * Flanger 2 still only 16-bit, will return an error if applied to 32-bit.
  * Some DSP effects bug fixed.
  + Echo 2.1 renamed to Echo 3:
    * BASS_FX_DSPFX_ECHO21 -> BASS_FX_DSPFX_ECHO3
    * BASS_FX_DSPECHO21 -> BASS_FX_DSPECHO3
  + New effects added (more will come soon!):
    . Chorus
    . All Pass Filter

2.0 "beta 2" - 28/11/2004
-------------------------
* New in BASS_FX:
  * Updated to BASS 2.1, just before releasing the official BASS_FX 2.1 :)
  * Oops... again BASS_FX is a bit smaller ;)

2.0 "beta 2" - 19/10/2004
-------------------------
* DSP:
  + Updated with 32-bit floating-point support:
    . Auto Wah
    + Dynamic Amplification:
      * Fixed bug that would cause a sound mute if there're ~20+ seconds of silence.
        All parameters changed from "Integer/Long" to "Float/Single" and their names
        now starts with "f: Float/Single".

    + Not updated, yet [only 16-bit support]:
      . Flanger 2.0!

  + Fixed bugs of:
    * "Echo" & "Reverb" effects that would cause a noise clicks and a sound mute.
    * "Peaking EQ" that would cause a crash with Mono files.

* Reverse:
  * 32-bit floating-point support.

* Tempo:
  * Fixed bug that would cause a crash if BASS_FX_Free would be called twice.

* WARN!NG NOTE:
   Not updated effects (Flanger 2/Tempo) must not be used with 32-bit.
   It will crash your program and could make a very annoying noise!!!
   Sometimes could even crash your system until RESET!

2.0 "beta 1" - 07/09/2004
-------------------------
* Really sorry for a long delay with updates!!!

* New in BASS_FX:
  * Now supports Windows 98/98SE without "msvcp60.dll" ~392KB
  * Some DSP effects updated with 32-bit floating-point support.
    The updated effects could be used with 16-bit & 32-bit.

* DSP:
  + Updated with 32-bit floating-point support:
    . Swap Channels
    . Rotate
    . Echo
    . Flanger
    . Volume Amplifier
    . Peaking EQ
    . Reverb
    . Low Pass Filter
    . Volume Cutter
    . Stereo 2 Mono
    . Echo 2.0!
    . Phaser
    . Echo 2.1!

    + Not updated, yet [only 16-bit support]:
      . Flanger 2.0!
      . Dynamic Amplification
      . Auto Wah

  * All parameters that began with "d: Double" changed to "f: Float/Single",
    as it was forgotten with last update.

* BPM:
  * Updated with 32-bit floating-point support.

* WARN!NG NOTE:
   Not updated effects (DSP/Tempo/Reverse) must not be used with 32-bit.
   It will crash your program and could make a very annoying noise!!!
   Sometimes could even crash your system until RESET!

* New in Examples:
  * Added: "Sync & Tempo"

* Removed:
  * BASS_FX_ERROR_BASS20 error code, BASS_FX will show an error message
    if BASS.DLL version is below 2.0 and won't load.

2.0 "alpha" - 4/12/2003
-----------------------
* New in BASS_FX:
  * Updated to BASS 2.0!
  * Version jumpted from '1.2 "beta"' to '2.0 "alpha"',
     means only BASS v2.x is supported!

* DSP:
  * Added *priority* param to BASS_FX_DSP_Set(..) func
  * All *Double* types changed to *Float/Single*

* New in Examples:
  * Added "Multi_FX" C/C++ only.

* VERSION 2.0 (not "alpha") will support:
  * 32-bit floating-point including in: DSP, Tempo, BPM & Reverse.
  * Multi Channel in some DSPs.
  * Planning to release till the end of this month/year :)

1.2 "beta" - 30/06/2003
-----------------------
* New in BASS_FX:
  * Not compatible with any previous BASS_FX versions,
     you'll have to make changes & recompile your
     application to use with this version!

  + Tempo, Pitch Scaling & Samplerate changers (3 at once ;))
    + Functions:
      * BASS_FX_TempoCreate
      * BASS_FX_TempoSet
      * BASS_FX_TempoGet
      * BASS_FX_TempoGetApproxSeconds
      * BASS_FX_TempoGetApproxPercents
      * BASS_FX_TempoGetResampledHandle
      * BASS_FX_TempoStopAndFlush
      * BASS_FX_TempoFree

  + Two BPM Detection options:
    + Option 1 - Get BPM from a Decoding channel:
      + Function
        * BASS_FX_BPM_DecodeGet

    + Option 2 - Get BPM by period of time of any handle - in Real-Time:
      + Functions
        * BASS_FX_BPM_CallbackSet
        * BASS_FX_BPM_CallbackReset

    + Functions to use with both options:
        * BASS_FX_BPM_X2
        * BASS_FX_BPM_Frequency2BPM
        * BASS_FX_BPM_2Frequency
        * BASS_FX_BPM_Percents2BPM
        * BASS_FX_BPM_2Percents
 * BASS_FX_BPM_Free

* New in DSP:
  * All DSP effects names has changed.
  * The index of 1st DSP effect starts from 0 and
    not from 1 as it was before.

  + -= DSP FXs =- added:
      * Dynamic Amplification
      * Stereo 2 Mono
      * Auto Wah
      * Echo v2.0!
      * Phaser
      * Echo v2.1!

  + Equalizer:
    * Added *Q* parameter.
    * Fixed some bugs :)
  + Flanger v2.0!
    * fixed bug (crashed with Mono files)

* New in Functions:
  + Added:
    * BASS_FX_ErrorGetCode
  * Always check for any Function changes.

* Removed all PITCH functions:
  * BASS_FX_PitchCreate
  * BASS_FX_PitchSet
  * BASS_FX_PitchGet
  * BASS_FX_PitchGetResampledHandle
  * BASS_FX_PitchStopAndFlush
  * BASS_FX_PitchFree

* New in Examples:
  * Added: C/C++, Delphi & VB

1.1 - 02/10/2002
----------------
* New in BASS_FX:
  * Now supports - 16/8-Bit Stereo/Mono.
  * Support for Multiple BASS instances.
  * A lot of BUGs fixed =)

* New in DSP:
  + -= DSP FXs =- added:
      * Low Pass Filter
      * Cutter
      * Flanger v2.0!

  + Equalizer: 
    * Algorithm optimized to BiQuad.
    * Added a new parameter *eqBandwidth*

* New in Reverse:
  * Now you can add DX8 effects + change Pitch
    with BASS_FX_Pitch... (check the *Reverse* example).

* New in functions:
  + Added:
    * BASS_FX_DSP_Reset
    * BASS_FX_Free
  * Always check for any Function changes.

* Added:
  * Delphi APIs + Pitch Example.
  * C/C++ Examples + corrected BASS_FX.LIB file ;)

* Switched from MFC to Win32 DLL [MFC42.DLL ~1MB doesn't required]

1.0 - 14/06/2002
----------------
* First release


Credits
=======
* Thanks a lot to - Ian Luck @ www.un4seen.com - for: 
   + BASS - Best Available Sound System!
   + DSP source codes for Echo, Dynamic Amplification, Compressor and Volume Envelope
   + Reverse playback source code
   + SoundTouch algorithms implementation for Tempo/BPM
   + Beat position algorithm fixes
   + 8/16/32-bit support
   + Fixed-point support
   + Multi-channel support
   + Add-on support
   + MacOSX support
   + Android support
* Ian, you're the best programmer in the whole world!


Credits - API/Sample Contributors
=================================
Delphi - Roger Johansson, Alex Graham (bigjim), DJ-Chris
BASS_FX.CHM - Thijs van Vliet


Bug reports, Suggestions, Comments, Enquiries, etc...
=====================================================
If you have any of the aforementioned please check the BASS forum (at
the website)... If you can't find an answer there, you can email:

        bass_fx@jobnik.org


System - Desktop/PC
===================
  ---------------------------------------------------
  BASS_FX.DLL - Windows - developed and tested using:
  ---------------------------------------------------
  System     : Intel Core i7 Haswell 4770 3.9GHz 8MB, 16GB DDR3 1600MHz CL9
               Intel Core i7 860 2.8GHz 8MB, 4GB DDR3 1600MHz CL7
               VMware 10

  OS         : Microsoft Windows:
                x86: 8, 7 Ultimate, Vista Ultimate, XP Pro SP3, 2000 Pro SP4 & 98
                x64: 8.1 Pro, 7 Enterprise/Ultimate, Vista Ultimate SP1, XP Pro SP1

  Sound Card : RealTek HD 7.1 (onboard)

  Compiler   : x86: Microsoft Visual C++ v6.0 SP5 with a Processor Pack
               x64: Microsoft Visual C++ 2005 v8.0

  DirectX    : 11, 10, 9.0c and 7.0
  BASS.DLL   : 2.4.11

  ----------------------------------------------------
  LIBBASS_FX.DYLIB - OSX - developed and tested using:
  ----------------------------------------------------
  System        : VMware 10

  OS            : Apple Macintosh OS X:
                   Intel Mac : 10.9, 10.8.2, 10.5.8 and 10.4.10
                   PowerPC   : 10.3.9 and 10.4

  Compiler      : GCC 4.0.1
  IDE           : XCode 3.1.4
  LIBBASS.DYLIB : 2.4.11

  ---------------------------------------------------
  LIBBASS_FX.SO - Linux - developed and tested using:
  ---------------------------------------------------
  System     : VMware 10

  OS         : Ubuntu Desktop x86 and x64 v8.04

  Compiler   : x86 and x64: GCC 4.2.4 (g++)

  IDE        : Code::Blocks v8.02
  LIBBASS.SO : 2.4.11


System - Mobile/Portable
========================
  ------------------------------------------------
  LIBBASS_FX.A - iOS - developed and tested using:
  ------------------------------------------------
  System    : VMware 10

  OS        : Apple Macintosh OS X: Intel Mac 10.9

  Compiler  : GCC 4.2 / LLVM
  IDE       : XCode 3.1.4 / XCode 5.0.2 for armv7s/arm64 architectures
  LIBBASS.A : 2.4.11

  -----------------------------------------------------
  LIBBASS_FX.SO - Android - developed and tested using:
  -----------------------------------------------------
  System     : Samsung Galaxy S3 GT-I9300
               Samsung Galaxy S2 GT-I9100
               Android Virtual Device

  OS         : Android JB 4.1.1/2
               Android ICS 4.0.3/4
               Android GB 2.3.3

  Compiler   : Android NDK R8: GCC 4.4.3

  IDE        : Eclipse
  LIBBASS.SO : 2.4.11

  -------------------------------------------------
  BASS_FX.DLL - WinCE - developed and tested using:
  -------------------------------------------------
  System   : GPS Device with CPU @ 372MHz
             Pocket PC 2003 SE Emulator

  OS       : Windows CE 5
             Windows Mobile 2003 SE version 4.21.1088

  Compiler : Microsoft Visual C++ 2005 v8.0
  BASS.DLL : 2.4.11

  -------------------------------------------------------
  LIBBASS_FX.SO - Linux ARM - developed and tested using:
  -------------------------------------------------------
  System     : VMware 8

  OS         : Ubuntu Desktop x86 v11.10

  Compiler   : GCC: (crosstool-NG 1.15.2) 4.7.1 20120402 (prerelease)

  IDE        : Code::Blocks v8.02
  LIBBASS.SO : 2.4.11


More Credits ;)
===============
 *  BiQuad filters
(c) Robert Bristow-JohnsonD
 @  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

 *  Peaking Equalizer (BiQuad filter)
    The main source is based on - Manu Webber's - source code.
 @  http://www.un4seen.com/forum/?topic=1246.msg6484#msg6484

 *  Tempo/Pitch/Rate/BPM [SoundTouch v1.8.0]
(c) Copyright (c) 2002-2014 Olli Parviainen
 @  http://www.surina.net/soundtouch
 L  LGPL license

 *  Auto Wah, Chorus, Distortion, Echo (some parts from 1st algorithm) and Phaser
(c) Copyright (c) 2000 Aleksey Smoli
 @  http://st.karelia.ru/~smlalx    (offline)

 *  Freeverb
(c) Copyright (c) 2000 Jezar at Dreampoint
 @  http://www.dreampoint.co.uk
 L  Public domain

 *  Pitch shifting using FFT [smbPitchShift v1.2]
(c) Copyright (c) 1999-2009 Stephan M. Bernsee <smb [AT] dspdimension [DOT] com>
 @  http://www.dspdimension.com/admin/pitch-shifting-using-the-ft/
 L  WOL license

 * BASS_FX is fully useable in commercial software, as long as credit is given.

-----------------------------------------------------------------------------------
* BASS_FX.TXT & File_ID.Diz are better viewed in DOS mode OR with - Courier - font.
-----------------------------------------------------------------------------------