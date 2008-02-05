unit sdl;
{
  $Id: sdl.pas,v 1.17 2005/01/03 18:40:59 savage Exp $

}
{******************************************************************************}
{                                                                              }
{          JEDI-SDL : Pascal units for SDL - Simple DirectMedia Layer          }
{             Conversion of the Simple DirectMedia Layer Headers               }
{                                                                              }
{ Portions created by Sam Lantinga <slouken@devolution.com> are                }
{ Copyright (C) 1997-2004  Sam Lantinga                                        }
{ 5635-34 Springhouse Dr.                                                      }
{ Pleasanton, CA 94588 (USA)                                                   }
{                                                                              }
{ All Rights Reserved.                                                         }
{                                                                              }
{ The original files are : SDL.h                                               }
{                          SDL_main.h                                          }
{                          SDL_types.h                                         }
{                          SDL_rwops.h                                         }
{                          SDL_timer.h                                         }
{                          SDL_audio.h                                         }
{                          SDL_cdrom.h                                         }
{                          SDL_joystick.h                                      }
{                          SDL_mouse.h                                         }
{                          SDL_keyboard.h                                      }
{                          SDL_events.h                                        }
{                          SDL_video.h                                         }
{                          SDL_byteorder.h                                     }
{                          SDL_version.h                                       }
{                          SDL_active.h                                        }
{                          SDL_thread.h                                        }
{                          SDL_mutex .h                                        }
{                          SDL_getenv.h                                        }
{                          SDL_loadso.h                                        }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominqiue Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominqiue Louis are                                      }
{ Copyright (C) 2000 - 2004 Dominqiue Louis.                                   }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Tom Jones <tigertomjones@gmx.de>  His Project inspired this conversion       }
{ Matthias Thoma <ma.thoma@gmx.de>                                             }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   The SDL Runtime libraris on Win32  : SDL.dll on Linux : libSDL.so          }
{   They are available from...                                                 }
{   http://www.libsdl.org .                                                    }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   May      08 2001 - DL : Added Keyboard  State Array ( See demos for how to }
{                           use )                                              }
{                           PKeyStateArr = ^TKeyStateArr;                      }
{                           TKeyStateArr = array[0..65000] of UInt8;           }
{                           As most games will need it.                        }
{                                                                              }
{   April    02 2001 - DL : Added SDL_getenv.h definitions and tested version  }
{                           1.2.0 compatability.                               }
{                                                                              }
{   March    13 2001 - MT : Added Linux compatibility.                         }
{                                                                              }
{   March    10 2001 - MT : Added externalsyms for DEFINES                     }
{                           Changed the license header                         }
{                                                                              }
{   March    09 2001 - MT : Added Kylix Ifdefs/Deleted the uses mmsystem       }
{                                                                              }
{   March    01 2001 - DL : Update conversion of version 1.1.8                 }
{                                                                              }
{   July     22 2001 - DL : Added TUInt8Array and PUIntArray after suggestions }
{                           from Matthias Thoma and Eric Grange.               }
{                                                                              }
{   October  12 2001 - DL : Various changes as suggested by Matthias Thoma and }
{                           David Acklam                                       }
{                                                                              }
{   October  24 2001 - DL : Added FreePascal support as per suggestions from   }
{                           Dean Ellis.                                        }
{                                                                              }
{   October  27 2001 - DL : Added SDL_BUTTON macro                             }
{                                                                              }
{  November  08 2001 - DL : Bug fix as pointed out by Puthoon.                 }
{                                                                              }
{  November  29 2001 - DL : Bug fix of SDL_SetGammaRamp as pointed out by Simon}
{                           Rushton.                                           }
{                                                                              }
{  November  30 2001 - DL : SDL_NOFRAME added as pointed out by Simon Rushton. }
{                                                                              }
{  December  11 2001 - DL : Added $WEAKPACKAGEUNIT ON to facilitate useage in  }
{                           Components                                         }
{                                                                              }
{  January   05 2002 - DL : Added SDL_Swap32 function as suggested by Matthias }
{                           Thoma and also made sure the _getenv from          }
{                           MSVCRT.DLL uses the right calling convention       }
{                                                                              }
{  January   25 2002 - DL : Updated conversion of SDL_AddTimer &               }
{                           SDL_RemoveTimer as per suggestions from Matthias   }
{                           Thoma.                                             }
{                                                                              }
{  January   27 2002 - DL : Commented out exported function putenv and getenv  }
{                           So that developers get used to using SDL_putenv    }
{                           SDL_getenv, as they are more portable              }
{                                                                              }
{  March     05 2002 - DL : Added FreeAnNil procedure for Delphi 4 users.      }
{                                                                              }
{  October   23 2002 - DL : Added Delphi 3 Define of Win32.                    }
{                           If you intend to you Delphi 3...                   }
{                           ( which is officially unsupported ) make sure you  }
{                           remove references to $EXTERNALSYM in this and other}
{                           SDL files.                                         }
{                                                                              }
{ November  29 2002 - DL : Fixed bug in Declaration of SDL_GetRGBA that was    }
{                          pointed out by Todd Lang                            }
{                                                                              }
{   April   03 2003 - DL : Added jedi-sdl.inc include file to support more     }
{                          Pascal compilers. Initial support is now included   }
{                          for GnuPascal, VirtualPascal, TMT and obviously     }
{                          continue support for Delphi Kylix and FreePascal.   }
{                                                                              }
{   April   08 2003 - MK : Aka Mr Kroket - Added Better FPC support            }
{                                                                              }
{   April   24 2003 - DL : under instruction from Alexey Barkovoy, I have added}
{                          better TMT Pascal support and under instruction     }
{                          from Prof. Abimbola Olowofoyeku (The African Chief),}
{                          I have added better Gnu Pascal support              }
{                                                                              }
{   April   30 2003 - DL : under instruction from David Mears AKA              }
{                          Jason Siletto, I have added FPC Linux support.      }
{                          This was compiled with fpc 1.1, so remember to set  }
{                          include file path. ie. -Fi/usr/share/fpcsrc/rtl/*   }
{                                                                              }
{
  $Log: sdl.pas,v $
  Revision 1.17  2005/01/03 18:40:59  savage
  Updated Version number to reflect latest one

  Revision 1.16  2005/01/01 02:02:06  savage
  Updated to v1.2.8

  Revision 1.15  2004/12/24 18:57:11  savage
  forgot to apply Michalis Kamburelis' patch to the implementation section. now fixed

  Revision 1.14  2004/12/23 23:42:18  savage
  Applied Patches supplied by Michalis Kamburelis ( THANKS! ), for greater FreePascal compatability.

  Revision 1.13  2004/09/30 22:31:59  savage
  Updated with slightly different header comments

  Revision 1.12  2004/09/12 21:52:58  savage
  Slight changes to fix some issues with the sdl classes.

  Revision 1.11  2004/08/14 22:54:30  savage
  Updated so that Library name defines are correctly defined for MacOS X.

  Revision 1.10  2004/07/20 23:57:33  savage
  Thanks to Paul Toth for spotting an error in the SDL Audio Convertion structures.
  In TSDL_AudioCVT the filters variable should point to and array of pointers and not what I had there previously.

  Revision 1.9  2004/07/03 22:07:22  savage
  Added Bitwise Manipulation Functions for TSDL_VideoInfo struct.

  Revision 1.8  2004/05/10 14:10:03  savage
  Initial MacOS X support. Fixed defines for MACOS ( Classic ) and DARWIN ( MacOS X ).

  Revision 1.7  2004/04/13 09:32:08  savage
  Changed Shared object names back to just the .so extension to avoid conflicts on various Linux/Unix distros. Therefore developers will need to create Symbolic links to the actual Share Objects if necessary.

  Revision 1.6  2004/04/01 20:53:23  savage
  Changed Linux Shared Object names so they reflect the Symbolic Links that are created when installing the RPMs from the SDL site.

  Revision 1.5  2004/02/22 15:32:10  savage
  SDL_GetEnv Fix so it also works on FPC/Linux. Thanks to Rodrigo for pointing this out.

  Revision 1.4  2004/02/21 23:24:29  savage
  SDL_GetEnv Fix so that it is not define twice for FPC. Thanks to Rene Hugentobler for pointing out this bug,

  Revision 1.3  2004/02/18 22:35:51  savage
  Brought sdl.pas up to 1.2.7 compatability
  Thus...
  Added SDL_GL_STEREO,
      SDL_GL_MULTISAMPLEBUFFERS,
      SDL_GL_MULTISAMPLESAMPLES

  Add DLL/Shared object functions
  function SDL_LoadObject( const sofile : PChar ) : Pointer;

  function SDL_LoadFunction( handle : Pointer; const name : PChar ) : Pointer;

  procedure SDL_UnloadObject( handle : Pointer );

  Added function to create RWops from const memory: SDL_RWFromConstMem()
  function SDL_RWFromConstMem(const mem: Pointer; size: Integer) : PSDL_RWops;

  Ported SDL_cpuinfo.h so Now you can test for Specific CPU types.

  Revision 1.2  2004/02/17 21:37:12  savage
  Tidying up of units

  Revision 1.1  2004/02/05 00:08:20  savage
  Module 1.0 release

}
{******************************************************************************}

{$I jedi-sdl.inc}

{$ALIGN ON}

interface

uses
{$IFDEF __GPC__}
  system,
  gpc;
{$ENDIF}

{$IFDEF WIN32}
  {$IFNDEF __GPC__}
  Windows;
  {$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF FPC}
    {$IFDEF Ver1_0}
    linux,
    {$ELSE}
    pthreads,
    baseunix,
    unix,
    {$ENDIF}
    x,
    xlib;
  {$ELSE}
  Types,
  Libc,
  Xlib;
  {$ENDIF}
{$ENDIF}

{$IFDEF __MACH__}
  GPCMacOSAll;
{$ENDIF}

const
{$IFDEF WIN32}
  SDLLibName = 'SDL.dll';
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF DARWIN}
  SDLLibName = 'libSDL.dylib';
{$ELSE}
  SDLLibName = 'libSDL.so';
{$ENDIF}
{$ENDIF}

{$IFDEF MACOS}
  SDLLibName = 'SDL';
{$ENDIF}

  // SDL.h constants
  SDL_INIT_TIMER = $00000001;
{$EXTERNALSYM SDL_INIT_TIMER}
  SDL_INIT_AUDIO = $00000010;
{$EXTERNALSYM SDL_INIT_AUDIO}
  SDL_INIT_VIDEO = $00000020;
{$EXTERNALSYM SDL_INIT_VIDEO}
  SDL_INIT_CDROM = $00000100;
{$EXTERNALSYM SDL_INIT_CDROM}
  SDL_INIT_JOYSTICK = $00000200;
{$EXTERNALSYM SDL_INIT_JOYSTICK}
  SDL_INIT_NOPARACHUTE = $00100000; // Don't catch fatal signals
{$EXTERNALSYM SDL_INIT_NOPARACHUTE}
  SDL_INIT_EVENTTHREAD = $01000000; // Not supported on all OS's
{$EXTERNALSYM SDL_INIT_EVENTTHREAD}
  SDL_INIT_EVERYTHING = $0000FFFF;
{$EXTERNALSYM SDL_INIT_EVERYTHING}

  // SDL_error.h constants
  ERR_MAX_STRLEN = 128;
{$EXTERNALSYM ERR_MAX_STRLEN}
  ERR_MAX_ARGS = 5;
{$EXTERNALSYM ERR_MAX_ARGS}

  // SDL_types.h constants
  SDL_PRESSED = $01;
{$EXTERNALSYM SDL_PRESSED}
  SDL_RELEASED = $00;
{$EXTERNALSYM SDL_RELEASED}

  // SDL_timer.h constants
  // This is the OS scheduler timeslice, in milliseconds
  SDL_TIMESLICE = 10;
{$EXTERNALSYM SDL_TIMESLICE}
  // This is the maximum resolution of the SDL timer on all platforms
  TIMER_RESOLUTION = 10; // Experimentally determined
{$EXTERNALSYM TIMER_RESOLUTION}

  // SDL_audio.h constants
  AUDIO_U8 = $0008; // Unsigned 8-bit samples
{$EXTERNALSYM AUDIO_U8}
  AUDIO_S8 = $8008; // Signed 8-bit samples
{$EXTERNALSYM AUDIO_S8}
  AUDIO_U16LSB = $0010; // Unsigned 16-bit samples
{$EXTERNALSYM AUDIO_U16LSB}
  AUDIO_S16LSB = $8010; // Signed 16-bit samples
{$EXTERNALSYM AUDIO_S16LSB}
  AUDIO_U16MSB = $1010; // As above, but big-endian byte order
{$EXTERNALSYM AUDIO_U16MSB}
  AUDIO_S16MSB = $9010; // As above, but big-endian byte order
{$EXTERNALSYM AUDIO_S16MSB}
  AUDIO_U16 = AUDIO_U16LSB;
{$EXTERNALSYM AUDIO_U16}
  AUDIO_S16 = AUDIO_S16LSB;
{$EXTERNALSYM AUDIO_S16}


  // SDL_cdrom.h constants
  // The maximum number of CD-ROM tracks on a disk
  SDL_MAX_TRACKS = 99;
{$EXTERNALSYM SDL_MAX_TRACKS}
  // The types of CD-ROM track possible
  SDL_AUDIO_TRACK = $00;
{$EXTERNALSYM SDL_AUDIO_TRACK}
  SDL_DATA_TRACK = $04;
{$EXTERNALSYM SDL_DATA_TRACK}

  // Conversion functions from frames to Minute/Second/Frames and vice versa
  CD_FPS = 75;
{$EXTERNALSYM CD_FPS}
  // SDL_byteorder.h constants
  // The two types of endianness
  SDL_LIL_ENDIAN = 1234;
{$EXTERNALSYM SDL_LIL_ENDIAN}
  SDL_BIG_ENDIAN = 4321;
{$EXTERNALSYM SDL_BIG_ENDIAN}

{$IFDEF IA32}

  SDL_BYTEORDER = SDL_LIL_ENDIAN;
{$EXTERNALSYM SDL_BYTEORDER}
  // Native audio byte ordering
  AUDIO_U16SYS = AUDIO_U16LSB;
{$EXTERNALSYM AUDIO_U16SYS}
  AUDIO_S16SYS = AUDIO_S16LSB;
{$EXTERNALSYM AUDIO_S16SYS}

{$ELSE}

  SDL_BYTEORDER = SDL_BIG_ENDIAN;
{$EXTERNALSYM SDL_BYTEORDER}
  // Native audio byte ordering
  AUDIO_U16SYS = AUDIO_U16MSB;
{$EXTERNALSYM AUDIO_U16SYS}
  AUDIO_S16SYS = AUDIO_S16MSB;
{$EXTERNALSYM AUDIO_S16SYS}

{$ENDIF}


  SDL_MIX_MAXVOLUME = 128;
{$EXTERNALSYM SDL_MIX_MAXVOLUME}

  // SDL_joystick.h constants
  MAX_JOYSTICKS = 2; // only 2 are supported in the multimedia API
{$EXTERNALSYM MAX_JOYSTICKS}
  MAX_AXES = 6; // each joystick can have up to 6 axes
{$EXTERNALSYM MAX_AXES}
  MAX_BUTTONS = 32; // and 32 buttons
{$EXTERNALSYM MAX_BUTTONS}
  AXIS_MIN = -32768; // minimum value for axis coordinate
{$EXTERNALSYM AXIS_MIN}
  AXIS_MAX = 32767; // maximum value for axis coordinate
{$EXTERNALSYM AXIS_MAX}
  JOY_AXIS_THRESHOLD = (((AXIS_MAX) - (AXIS_MIN)) / 100); // 1% motion
{$EXTERNALSYM JOY_AXIS_THRESHOLD}
  //JOY_BUTTON_FLAG(n)        (1<<n)
  // array to hold joystick ID values
  //static UInt        SYS_JoystickID[MAX_JOYSTICKS];
  //static JOYCAPS        SYS_Joystick[MAX_JOYSTICKS];

  { Get the current state of a POV hat on a joystick
    The return value is one of the following positions: }
  SDL_HAT_CENTERED = $00;
{$EXTERNALSYM SDL_HAT_CENTERED}
  SDL_HAT_UP = $01;
{$EXTERNALSYM SDL_HAT_UP}
  SDL_HAT_RIGHT = $02;
{$EXTERNALSYM SDL_HAT_RIGHT}
  SDL_HAT_DOWN = $04;
{$EXTERNALSYM SDL_HAT_DOWN}
  SDL_HAT_LEFT = $08;
{$EXTERNALSYM SDL_HAT_LEFT}
  SDL_HAT_RIGHTUP = SDL_HAT_RIGHT or SDL_HAT_UP;
{$EXTERNALSYM SDL_HAT_RIGHTUP}
  SDL_HAT_RIGHTDOWN = SDL_HAT_RIGHT or SDL_HAT_DOWN;
{$EXTERNALSYM SDL_HAT_RIGHTDOWN}
  SDL_HAT_LEFTUP = SDL_HAT_LEFT or SDL_HAT_UP;
{$EXTERNALSYM SDL_HAT_LEFTUP}
  SDL_HAT_LEFTDOWN = SDL_HAT_LEFT or SDL_HAT_DOWN;
{$EXTERNALSYM SDL_HAT_LEFTDOWN}

  // SDL_verion.h constants
  // Printable format: "%d.%d.%d", MAJOR, MINOR, PATCHLEVEL
  SDL_MAJOR_VERSION = 1;
{$EXTERNALSYM SDL_MAJOR_VERSION}
  SDL_MINOR_VERSION = 2;
{$EXTERNALSYM SDL_MINOR_VERSION}
  SDL_PATCHLEVEL    = 8;
{$EXTERNALSYM SDL_PATCHLEVEL}

  // SDL_events.h constants
  SDL_NOEVENT = 0; // Unused (do not remove)
{$EXTERNALSYM SDL_NOEVENT}
  SDL_ACTIVEEVENT = 1; // Application loses/gains visibility
{$EXTERNALSYM SDL_ACTIVEEVENT}
  SDL_KEYDOWN = 2; // Keys pressed
{$EXTERNALSYM SDL_KEYDOWN}
  SDL_KEYUP = 3; // Keys released
{$EXTERNALSYM SDL_KEYUP}
  SDL_MOUSEMOTION = 4; // Mouse moved
{$EXTERNALSYM SDL_MOUSEMOTION}
  SDL_MOUSEBUTTONDOWN = 5; // Mouse button pressed
{$EXTERNALSYM SDL_MOUSEBUTTONDOWN}
  SDL_MOUSEBUTTONUP = 6; // Mouse button released
{$EXTERNALSYM SDL_MOUSEBUTTONUP}
  SDL_JOYAXISMOTION = 7; // Joystick axis motion
{$EXTERNALSYM SDL_JOYAXISMOTION}
  SDL_JOYBALLMOTION = 8; // Joystick trackball motion
{$EXTERNALSYM SDL_JOYBALLMOTION}
  SDL_JOYHATMOTION = 9; // Joystick hat position change
{$EXTERNALSYM SDL_JOYHATMOTION}
  SDL_JOYBUTTONDOWN = 10; // Joystick button pressed
{$EXTERNALSYM SDL_JOYBUTTONDOWN}
  SDL_JOYBUTTONUP = 11; // Joystick button released
{$EXTERNALSYM SDL_JOYBUTTONUP}
  SDL_QUITEV = 12; // User-requested quit ( Changed due to procedure conflict )
{$EXTERNALSYM SDL_QUIT}
  SDL_SYSWMEVENT = 13; // System specific event
{$EXTERNALSYM SDL_SYSWMEVENT}
  SDL_EVENT_RESERVEDA = 14; // Reserved for future use..
{$EXTERNALSYM SDL_EVENT_RESERVEDA}
  SDL_EVENT_RESERVED = 15; // Reserved for future use..
{$EXTERNALSYM SDL_EVENT_RESERVED}
  SDL_VIDEORESIZE = 16; // User resized video mode
{$EXTERNALSYM SDL_VIDEORESIZE}
  SDL_VIDEOEXPOSE = 17; // Screen needs to be redrawn
{$EXTERNALSYM SDL_VIDEOEXPOSE}
  SDL_EVENT_RESERVED2 = 18; // Reserved for future use..
{$EXTERNALSYM SDL_EVENT_RESERVED2}
  SDL_EVENT_RESERVED3 = 19; // Reserved for future use..
{$EXTERNALSYM SDL_EVENT_RESERVED3}
  SDL_EVENT_RESERVED4 = 20; // Reserved for future use..
{$EXTERNALSYM SDL_EVENT_RESERVED4}
  SDL_EVENT_RESERVED5 = 21; // Reserved for future use..
{$EXTERNALSYM SDL_EVENT_RESERVED5}
  SDL_EVENT_RESERVED6 = 22; // Reserved for future use..
{$EXTERNALSYM SDL_EVENT_RESERVED6}
  SDL_EVENT_RESERVED7 = 23; // Reserved for future use..
{$EXTERNALSYM SDL_EVENT_RESERVED7}
  // Events SDL_USEREVENT through SDL_MAXEVENTS-1 are for your use
  SDL_USEREVENT = 24;
{$EXTERNALSYM SDL_USEREVENT}
  // This last event is only for bounding internal arrays
  // It is the number of bits in the event mask datatype -- UInt32
  SDL_NUMEVENTS = 32;
{$EXTERNALSYM SDL_NUMEVENTS}

  SDL_ALLEVENTS = $FFFFFFFF;
{$EXTERNALSYM SDL_ALLEVENTS}

  SDL_ACTIVEEVENTMASK = 1 shl SDL_ACTIVEEVENT;
{$EXTERNALSYM SDL_ACTIVEEVENTMASK}
  SDL_KEYDOWNMASK = 1 shl SDL_KEYDOWN;
{$EXTERNALSYM SDL_KEYDOWNMASK}
  SDL_KEYUPMASK = 1 shl SDL_KEYUP;
{$EXTERNALSYM SDL_KEYUPMASK}
  SDL_MOUSEMOTIONMASK = 1 shl SDL_MOUSEMOTION;
{$EXTERNALSYM SDL_MOUSEMOTIONMASK}
  SDL_MOUSEBUTTONDOWNMASK = 1 shl SDL_MOUSEBUTTONDOWN;
{$EXTERNALSYM SDL_MOUSEBUTTONDOWNMASK}
  SDL_MOUSEBUTTONUPMASK = 1 shl SDL_MOUSEBUTTONUP;
{$EXTERNALSYM SDL_MOUSEBUTTONUPMASK}
  SDL_MOUSEEVENTMASK = 1 shl SDL_MOUSEMOTION or
    1 shl SDL_MOUSEBUTTONDOWN or
    1 shl SDL_MOUSEBUTTONUP;
{$EXTERNALSYM SDL_MOUSEEVENTMASK}
  SDL_JOYAXISMOTIONMASK = 1 shl SDL_JOYAXISMOTION;
{$EXTERNALSYM SDL_JOYAXISMOTIONMASK}
  SDL_JOYBALLMOTIONMASK = 1 shl SDL_JOYBALLMOTION;
{$EXTERNALSYM SDL_JOYBALLMOTIONMASK}
  SDL_JOYHATMOTIONMASK = 1 shl SDL_JOYHATMOTION;
{$EXTERNALSYM SDL_JOYHATMOTIONMASK}
  SDL_JOYBUTTONDOWNMASK = 1 shl SDL_JOYBUTTONDOWN;
{$EXTERNALSYM SDL_JOYBUTTONDOWNMASK}
  SDL_JOYBUTTONUPMASK = 1 shl SDL_JOYBUTTONUP;
{$EXTERNALSYM SDL_JOYBUTTONUPMASK}
  SDL_JOYEVENTMASK = 1 shl SDL_JOYAXISMOTION or
    1 shl SDL_JOYBALLMOTION or
    1 shl SDL_JOYHATMOTION or
    1 shl SDL_JOYBUTTONDOWN or
    1 shl SDL_JOYBUTTONUP;
{$EXTERNALSYM SDL_JOYEVENTMASK}
  SDL_VIDEORESIZEMASK = 1 shl SDL_VIDEORESIZE;
{$EXTERNALSYM SDL_VIDEORESIZEMASK}
  SDL_QUITMASK = 1 shl SDL_QUITEV;
{$EXTERNALSYM SDL_QUITMASK}
  SDL_SYSWMEVENTMASK = 1 shl SDL_SYSWMEVENT;
{$EXTERNALSYM SDL_SYSWMEVENTMASK}

  { This function allows you to set the state of processing certain events.
    If 'state' is set to SDL_IGNORE, that event will be automatically dropped
    from the event queue and will not event be filtered.
    If 'state' is set to SDL_ENABLE, that event will be processed normally.
    If 'state' is set to SDL_QUERY, SDL_EventState() will return the
    current processing state of the specified event. }

  SDL_QUERY = -1;
{$EXTERNALSYM SDL_QUERY}
  SDL_IGNORE = 0;
{$EXTERNALSYM SDL_IGNORE}
  SDL_DISABLE = 0;
{$EXTERNALSYM SDL_DISABLE}
  SDL_ENABLE = 1;
{$EXTERNALSYM SDL_ENABLE}

  //SDL_keyboard.h constants
  // This is the mask which refers to all hotkey bindings
  SDL_ALL_HOTKEYS = $FFFFFFFF;
{$EXTERNALSYM SDL_ALL_HOTKEYS}

{ Enable/Disable keyboard repeat.  Keyboard repeat defaults to off.
  'delay' is the initial delay in ms between the time when a key is
  pressed, and keyboard repeat begins.
  'interval' is the time in ms between keyboard repeat events. }

  SDL_DEFAULT_REPEAT_DELAY = 500;
{$EXTERNALSYM SDL_DEFAULT_REPEAT_DELAY}
  SDL_DEFAULT_REPEAT_INTERVAL = 30;
{$EXTERNALSYM SDL_DEFAULT_REPEAT_INTERVAL}

  // The keyboard syms have been cleverly chosen to map to ASCII
  SDLK_UNKNOWN = 0;
{$EXTERNALSYM SDLK_UNKNOWN}
  SDLK_FIRST = 0;
{$EXTERNALSYM SDLK_FIRST}
  SDLK_BACKSPACE = 8;
{$EXTERNALSYM SDLK_BACKSPACE}
  SDLK_TAB = 9;
{$EXTERNALSYM SDLK_TAB}
  SDLK_CLEAR = 12;
{$EXTERNALSYM SDLK_CLEAR}
  SDLK_RETURN = 13;
{$EXTERNALSYM SDLK_RETURN}
  SDLK_PAUSE = 19;
{$EXTERNALSYM SDLK_PAUSE}
  SDLK_ESCAPE = 27;
{$EXTERNALSYM SDLK_ESCAPE}
  SDLK_SPACE = 32;
{$EXTERNALSYM SDLK_SPACE}
  SDLK_EXCLAIM = 33;
{$EXTERNALSYM SDLK_EXCLAIM}
  SDLK_QUOTEDBL = 34;
{$EXTERNALSYM SDLK_QUOTEDBL}
  SDLK_HASH = 35;
{$EXTERNALSYM SDLK_HASH}
  SDLK_DOLLAR = 36;
{$EXTERNALSYM SDLK_DOLLAR}
  SDLK_AMPERSAND = 38;
{$EXTERNALSYM SDLK_AMPERSAND}
  SDLK_QUOTE = 39;
{$EXTERNALSYM SDLK_QUOTE}
  SDLK_LEFTPAREN = 40;
{$EXTERNALSYM SDLK_LEFTPAREN}
  SDLK_RIGHTPAREN = 41;
{$EXTERNALSYM SDLK_RIGHTPAREN}
  SDLK_ASTERISK = 42;
{$EXTERNALSYM SDLK_ASTERISK}
  SDLK_PLUS = 43;
{$EXTERNALSYM SDLK_PLUS}
  SDLK_COMMA = 44;
{$EXTERNALSYM SDLK_COMMA}
  SDLK_MINUS = 45;
{$EXTERNALSYM SDLK_MINUS}
  SDLK_PERIOD = 46;
{$EXTERNALSYM SDLK_PERIOD}
  SDLK_SLASH = 47;
{$EXTERNALSYM SDLK_SLASH}
  SDLK_0 = 48;
{$EXTERNALSYM SDLK_0}
  SDLK_1 = 49;
{$EXTERNALSYM SDLK_1}
  SDLK_2 = 50;
{$EXTERNALSYM SDLK_2}
  SDLK_3 = 51;
{$EXTERNALSYM SDLK_3}
  SDLK_4 = 52;
{$EXTERNALSYM SDLK_4}
  SDLK_5 = 53;
{$EXTERNALSYM SDLK_5}
  SDLK_6 = 54;
{$EXTERNALSYM SDLK_6}
  SDLK_7 = 55;
{$EXTERNALSYM SDLK_7}
  SDLK_8 = 56;
{$EXTERNALSYM SDLK_8}
  SDLK_9 = 57;
{$EXTERNALSYM SDLK_9}
  SDLK_COLON = 58;
{$EXTERNALSYM SDLK_COLON}
  SDLK_SEMICOLON = 59;
{$EXTERNALSYM SDLK_SEMICOLON}
  SDLK_LESS = 60;
{$EXTERNALSYM SDLK_LESS}
  SDLK_EQUALS = 61;
{$EXTERNALSYM SDLK_EQUALS}
  SDLK_GREATER = 62;
{$EXTERNALSYM SDLK_GREATER}
  SDLK_QUESTION = 63;
{$EXTERNALSYM SDLK_QUESTION}
  SDLK_AT = 64;
{$EXTERNALSYM SDLK_AT}

  { Skip uppercase letters }

  SDLK_LEFTBRACKET = 91;
{$EXTERNALSYM SDLK_LEFTBRACKET}
  SDLK_BACKSLASH = 92;
{$EXTERNALSYM SDLK_BACKSLASH}
  SDLK_RIGHTBRACKET = 93;
{$EXTERNALSYM SDLK_RIGHTBRACKET}
  SDLK_CARET = 94;
{$EXTERNALSYM SDLK_CARET}
  SDLK_UNDERSCORE = 95;
{$EXTERNALSYM SDLK_UNDERSCORE}
  SDLK_BACKQUOTE = 96;
{$EXTERNALSYM SDLK_BACKQUOTE}
  SDLK_a = 97;
{$EXTERNALSYM SDLK_a}
  SDLK_b = 98;
{$EXTERNALSYM SDLK_b}
  SDLK_c = 99;
{$EXTERNALSYM SDLK_c}
  SDLK_d = 100;
{$EXTERNALSYM SDLK_d}
  SDLK_e = 101;
{$EXTERNALSYM SDLK_e}
  SDLK_f = 102;
{$EXTERNALSYM SDLK_f}
  SDLK_g = 103;
{$EXTERNALSYM SDLK_g}
  SDLK_h = 104;
{$EXTERNALSYM SDLK_h}
  SDLK_i = 105;
{$EXTERNALSYM SDLK_i}
  SDLK_j = 106;
{$EXTERNALSYM SDLK_j}
  SDLK_k = 107;
{$EXTERNALSYM SDLK_k}
  SDLK_l = 108;
{$EXTERNALSYM SDLK_l}
  SDLK_m = 109;
{$EXTERNALSYM SDLK_m}
  SDLK_n = 110;
{$EXTERNALSYM SDLK_n}
  SDLK_o = 111;
{$EXTERNALSYM SDLK_o}
  SDLK_p = 112;
{$EXTERNALSYM SDLK_p}
  SDLK_q = 113;
{$EXTERNALSYM SDLK_q}
  SDLK_r = 114;
{$EXTERNALSYM SDLK_r}
  SDLK_s = 115;
{$EXTERNALSYM SDLK_s}
  SDLK_t = 116;
{$EXTERNALSYM SDLK_t}
  SDLK_u = 117;
{$EXTERNALSYM SDLK_u}
  SDLK_v = 118;
{$EXTERNALSYM SDLK_v}
  SDLK_w = 119;
{$EXTERNALSYM SDLK_w}
  SDLK_x = 120;
{$EXTERNALSYM SDLK_x}
  SDLK_y = 121;
{$EXTERNALSYM SDLK_y}
  SDLK_z = 122;
{$EXTERNALSYM SDLK_z}
  SDLK_DELETE = 127;
{$EXTERNALSYM SDLK_DELETE}
  // End of ASCII mapped keysyms

  // International keyboard syms
  SDLK_WORLD_0 = 160; // 0xA0
{$EXTERNALSYM SDLK_WORLD_0}
  SDLK_WORLD_1 = 161;
{$EXTERNALSYM SDLK_WORLD_1}
  SDLK_WORLD_2 = 162;
{$EXTERNALSYM SDLK_WORLD_2}
  SDLK_WORLD_3 = 163;
{$EXTERNALSYM SDLK_WORLD_3}
  SDLK_WORLD_4 = 164;
{$EXTERNALSYM SDLK_WORLD_4}
  SDLK_WORLD_5 = 165;
{$EXTERNALSYM SDLK_WORLD_5}
  SDLK_WORLD_6 = 166;
{$EXTERNALSYM SDLK_WORLD_6}
  SDLK_WORLD_7 = 167;
{$EXTERNALSYM SDLK_WORLD_7}
  SDLK_WORLD_8 = 168;
{$EXTERNALSYM SDLK_WORLD_8}
  SDLK_WORLD_9 = 169;
{$EXTERNALSYM SDLK_WORLD_9}
  SDLK_WORLD_10 = 170;
{$EXTERNALSYM SDLK_WORLD_10}
  SDLK_WORLD_11 = 171;
{$EXTERNALSYM SDLK_WORLD_11}
  SDLK_WORLD_12 = 172;
{$EXTERNALSYM SDLK_WORLD_12}
  SDLK_WORLD_13 = 173;
{$EXTERNALSYM SDLK_WORLD_13}
  SDLK_WORLD_14 = 174;
{$EXTERNALSYM SDLK_WORLD_14}
  SDLK_WORLD_15 = 175;
{$EXTERNALSYM SDLK_WORLD_15}
  SDLK_WORLD_16 = 176;
{$EXTERNALSYM SDLK_WORLD_16}
  SDLK_WORLD_17 = 177;
{$EXTERNALSYM SDLK_WORLD_17}
  SDLK_WORLD_18 = 178;
{$EXTERNALSYM SDLK_WORLD_18}
  SDLK_WORLD_19 = 179;
{$EXTERNALSYM SDLK_WORLD_19}
  SDLK_WORLD_20 = 180;
{$EXTERNALSYM SDLK_WORLD_20}
  SDLK_WORLD_21 = 181;
{$EXTERNALSYM SDLK_WORLD_21}
  SDLK_WORLD_22 = 182;
{$EXTERNALSYM SDLK_WORLD_22}
  SDLK_WORLD_23 = 183;
{$EXTERNALSYM SDLK_WORLD_23}
  SDLK_WORLD_24 = 184;
{$EXTERNALSYM SDLK_WORLD_24}
  SDLK_WORLD_25 = 185;
{$EXTERNALSYM SDLK_WORLD_25}
  SDLK_WORLD_26 = 186;
{$EXTERNALSYM SDLK_WORLD_26}
  SDLK_WORLD_27 = 187;
{$EXTERNALSYM SDLK_WORLD_27}
  SDLK_WORLD_28 = 188;
{$EXTERNALSYM SDLK_WORLD_28}
  SDLK_WORLD_29 = 189;
{$EXTERNALSYM SDLK_WORLD_29}
  SDLK_WORLD_30 = 190;
{$EXTERNALSYM SDLK_WORLD_30}
  SDLK_WORLD_31 = 191;
{$EXTERNALSYM SDLK_WORLD_31}
  SDLK_WORLD_32 = 192;
{$EXTERNALSYM SDLK_WORLD_32}
  SDLK_WORLD_33 = 193;
{$EXTERNALSYM SDLK_WORLD_33}
  SDLK_WORLD_34 = 194;
{$EXTERNALSYM SDLK_WORLD_34}
  SDLK_WORLD_35 = 195;
{$EXTERNALSYM SDLK_WORLD_35}
  SDLK_WORLD_36 = 196;
{$EXTERNALSYM SDLK_WORLD_36}
  SDLK_WORLD_37 = 197;
{$EXTERNALSYM SDLK_WORLD_37}
  SDLK_WORLD_38 = 198;
{$EXTERNALSYM SDLK_WORLD_38}
  SDLK_WORLD_39 = 199;
{$EXTERNALSYM SDLK_WORLD_39}
  SDLK_WORLD_40 = 200;
{$EXTERNALSYM SDLK_WORLD_40}
  SDLK_WORLD_41 = 201;
{$EXTERNALSYM SDLK_WORLD_41}
  SDLK_WORLD_42 = 202;
{$EXTERNALSYM SDLK_WORLD_42}
  SDLK_WORLD_43 = 203;
{$EXTERNALSYM SDLK_WORLD_43}
  SDLK_WORLD_44 = 204;
{$EXTERNALSYM SDLK_WORLD_44}
  SDLK_WORLD_45 = 205;
{$EXTERNALSYM SDLK_WORLD_45}
  SDLK_WORLD_46 = 206;
{$EXTERNALSYM SDLK_WORLD_46}
  SDLK_WORLD_47 = 207;
{$EXTERNALSYM SDLK_WORLD_47}
  SDLK_WORLD_48 = 208;
{$EXTERNALSYM SDLK_WORLD_48}
  SDLK_WORLD_49 = 209;
{$EXTERNALSYM SDLK_WORLD_49}
  SDLK_WORLD_50 = 210;
{$EXTERNALSYM SDLK_WORLD_50}
  SDLK_WORLD_51 = 211;
{$EXTERNALSYM SDLK_WORLD_51}
  SDLK_WORLD_52 = 212;
{$EXTERNALSYM SDLK_WORLD_52}
  SDLK_WORLD_53 = 213;
{$EXTERNALSYM SDLK_WORLD_53}
  SDLK_WORLD_54 = 214;
{$EXTERNALSYM SDLK_WORLD_54}
  SDLK_WORLD_55 = 215;
{$EXTERNALSYM SDLK_WORLD_55}
  SDLK_WORLD_56 = 216;
{$EXTERNALSYM SDLK_WORLD_56}
  SDLK_WORLD_57 = 217;
{$EXTERNALSYM SDLK_WORLD_57}
  SDLK_WORLD_58 = 218;
{$EXTERNALSYM SDLK_WORLD_58}
  SDLK_WORLD_59 = 219;
{$EXTERNALSYM SDLK_WORLD_59}
  SDLK_WORLD_60 = 220;
{$EXTERNALSYM SDLK_WORLD_60}
  SDLK_WORLD_61 = 221;
{$EXTERNALSYM SDLK_WORLD_61}
  SDLK_WORLD_62 = 222;
{$EXTERNALSYM SDLK_WORLD_62}
  SDLK_WORLD_63 = 223;
{$EXTERNALSYM SDLK_WORLD_63}
  SDLK_WORLD_64 = 224;
{$EXTERNALSYM SDLK_WORLD_64}
  SDLK_WORLD_65 = 225;
{$EXTERNALSYM SDLK_WORLD_65}
  SDLK_WORLD_66 = 226;
{$EXTERNALSYM SDLK_WORLD_66}
  SDLK_WORLD_67 = 227;
{$EXTERNALSYM SDLK_WORLD_67}
  SDLK_WORLD_68 = 228;
{$EXTERNALSYM SDLK_WORLD_68}
  SDLK_WORLD_69 = 229;
{$EXTERNALSYM SDLK_WORLD_69}
  SDLK_WORLD_70 = 230;
{$EXTERNALSYM SDLK_WORLD_70}
  SDLK_WORLD_71 = 231;
{$EXTERNALSYM SDLK_WORLD_71}
  SDLK_WORLD_72 = 232;
{$EXTERNALSYM SDLK_WORLD_72}
  SDLK_WORLD_73 = 233;
{$EXTERNALSYM SDLK_WORLD_73}
  SDLK_WORLD_74 = 234;
{$EXTERNALSYM SDLK_WORLD_74}
  SDLK_WORLD_75 = 235;
{$EXTERNALSYM SDLK_WORLD_75}
  SDLK_WORLD_76 = 236;
{$EXTERNALSYM SDLK_WORLD_76}
  SDLK_WORLD_77 = 237;
{$EXTERNALSYM SDLK_WORLD_77}
  SDLK_WORLD_78 = 238;
{$EXTERNALSYM SDLK_WORLD_78}
  SDLK_WORLD_79 = 239;
{$EXTERNALSYM SDLK_WORLD_79}
  SDLK_WORLD_80 = 240;
{$EXTERNALSYM SDLK_WORLD_80}
  SDLK_WORLD_81 = 241;
{$EXTERNALSYM SDLK_WORLD_81}
  SDLK_WORLD_82 = 242;
{$EXTERNALSYM SDLK_WORLD_82}
  SDLK_WORLD_83 = 243;
{$EXTERNALSYM SDLK_WORLD_83}
  SDLK_WORLD_84 = 244;
{$EXTERNALSYM SDLK_WORLD_84}
  SDLK_WORLD_85 = 245;
{$EXTERNALSYM SDLK_WORLD_85}
  SDLK_WORLD_86 = 246;
{$EXTERNALSYM SDLK_WORLD_86}
  SDLK_WORLD_87 = 247;
{$EXTERNALSYM SDLK_WORLD_87}
  SDLK_WORLD_88 = 248;
{$EXTERNALSYM SDLK_WORLD_88}
  SDLK_WORLD_89 = 249;
{$EXTERNALSYM SDLK_WORLD_89}
  SDLK_WORLD_90 = 250;
{$EXTERNALSYM SDLK_WORLD_90}
  SDLK_WORLD_91 = 251;
{$EXTERNALSYM SDLK_WORLD_91}
  SDLK_WORLD_92 = 252;
{$EXTERNALSYM SDLK_WORLD_92}
  SDLK_WORLD_93 = 253;
{$EXTERNALSYM SDLK_WORLD_93}
  SDLK_WORLD_94 = 254;
{$EXTERNALSYM SDLK_WORLD_94}
  SDLK_WORLD_95 = 255; // 0xFF
{$EXTERNALSYM SDLK_WORLD_95}

  // Numeric keypad
  SDLK_KP0 = 256;
{$EXTERNALSYM SDLK_KP0}
  SDLK_KP1 = 257;
{$EXTERNALSYM SDLK_KP1}
  SDLK_KP2 = 258;
{$EXTERNALSYM SDLK_KP2}
  SDLK_KP3 = 259;
{$EXTERNALSYM SDLK_KP3}
  SDLK_KP4 = 260;
{$EXTERNALSYM SDLK_KP4}
  SDLK_KP5 = 261;
{$EXTERNALSYM SDLK_KP5}
  SDLK_KP6 = 262;
{$EXTERNALSYM SDLK_KP6}
  SDLK_KP7 = 263;
{$EXTERNALSYM SDLK_KP7}
  SDLK_KP8 = 264;
{$EXTERNALSYM SDLK_KP8}
  SDLK_KP9 = 265;
{$EXTERNALSYM SDLK_KP9}
  SDLK_KP_PERIOD = 266;
{$EXTERNALSYM SDLK_KP_PERIOD}
  SDLK_KP_DIVIDE = 267;
{$EXTERNALSYM SDLK_KP_DIVIDE}
  SDLK_KP_MULTIPLY = 268;
{$EXTERNALSYM SDLK_KP_MULTIPLY}
  SDLK_KP_MINUS = 269;
{$EXTERNALSYM SDLK_KP_MINUS}
  SDLK_KP_PLUS = 270;
{$EXTERNALSYM SDLK_KP_PLUS}
  SDLK_KP_ENTER = 271;
{$EXTERNALSYM SDLK_KP_ENTER}
  SDLK_KP_EQUALS = 272;
{$EXTERNALSYM SDLK_KP_EQUALS}

  // Arrows + Home/End pad
  SDLK_UP = 273;
{$EXTERNALSYM SDLK_UP}
  SDLK_DOWN = 274;
{$EXTERNALSYM SDLK_DOWN}
  SDLK_RIGHT = 275;
{$EXTERNALSYM SDLK_RIGHT}
  SDLK_LEFT = 276;
{$EXTERNALSYM SDLK_LEFT}
  SDLK_INSERT = 277;
{$EXTERNALSYM SDLK_INSERT}
  SDLK_HOME = 278;
{$EXTERNALSYM SDLK_HOME}
  SDLK_END = 279;
{$EXTERNALSYM SDLK_END}
  SDLK_PAGEUP = 280;
{$EXTERNALSYM SDLK_PAGEUP}
  SDLK_PAGEDOWN = 281;
{$EXTERNALSYM SDLK_PAGEDOWN}

  // Function keys
  SDLK_F1 = 282;
{$EXTERNALSYM SDLK_F1}
  SDLK_F2 = 283;
{$EXTERNALSYM SDLK_F2}
  SDLK_F3 = 284;
{$EXTERNALSYM SDLK_F3}
  SDLK_F4 = 285;
{$EXTERNALSYM SDLK_F4}
  SDLK_F5 = 286;
{$EXTERNALSYM SDLK_F5}
  SDLK_F6 = 287;
{$EXTERNALSYM SDLK_F6}
  SDLK_F7 = 288;
{$EXTERNALSYM SDLK_F7}
  SDLK_F8 = 289;
{$EXTERNALSYM SDLK_F8}
  SDLK_F9 = 290;
{$EXTERNALSYM SDLK_F9}
  SDLK_F10 = 291;
{$EXTERNALSYM SDLK_F10}
  SDLK_F11 = 292;
{$EXTERNALSYM SDLK_F11}
  SDLK_F12 = 293;
{$EXTERNALSYM SDLK_F12}
  SDLK_F13 = 294;
{$EXTERNALSYM SDLK_F13}
  SDLK_F14 = 295;
{$EXTERNALSYM SDLK_F14}
  SDLK_F15 = 296;
{$EXTERNALSYM SDLK_F15}

  // Key state modifier keys
  SDLK_NUMLOCK = 300;
{$EXTERNALSYM SDLK_NUMLOCK}
  SDLK_CAPSLOCK = 301;
{$EXTERNALSYM SDLK_CAPSLOCK}
  SDLK_SCROLLOCK = 302;
{$EXTERNALSYM SDLK_SCROLLOCK}
  SDLK_RSHIFT = 303;
{$EXTERNALSYM SDLK_RSHIFT}
  SDLK_LSHIFT = 304;
{$EXTERNALSYM SDLK_LSHIFT}
  SDLK_RCTRL = 305;
{$EXTERNALSYM SDLK_RCTRL}
  SDLK_LCTRL = 306;
{$EXTERNALSYM SDLK_LCTRL}
  SDLK_RALT = 307;
{$EXTERNALSYM SDLK_RALT}
  SDLK_LALT = 308;
{$EXTERNALSYM SDLK_LALT}
  SDLK_RMETA = 309;
{$EXTERNALSYM SDLK_RMETA}
  SDLK_LMETA = 310;
{$EXTERNALSYM SDLK_LMETA}
  SDLK_LSUPER = 311; // Left "Windows" key
{$EXTERNALSYM SDLK_LSUPER}
  SDLK_RSUPER = 312; // Right "Windows" key
{$EXTERNALSYM SDLK_RSUPER}
  SDLK_MODE = 313; // "Alt Gr" key
{$EXTERNALSYM SDLK_MODE}
  SDLK_COMPOSE = 314; // Multi-key compose key
{$EXTERNALSYM SDLK_COMPOSE}

  // Miscellaneous function keys
  SDLK_HELP = 315;
{$EXTERNALSYM SDLK_HELP}
  SDLK_PRINT = 316;
{$EXTERNALSYM SDLK_PRINT}
  SDLK_SYSREQ = 317;
{$EXTERNALSYM SDLK_SYSREQ}
  SDLK_BREAK = 318;
{$EXTERNALSYM SDLK_BREAK}
  SDLK_MENU = 319;
{$EXTERNALSYM SDLK_MENU}
  SDLK_POWER = 320; // Power Macintosh power key
{$EXTERNALSYM SDLK_POWER}
  SDLK_EURO = 321; // Some european keyboards
{$EXTERNALSYM SDLK_EURO}

  // Enumeration of valid key mods (possibly OR'd together)
  KMOD_NONE = $0000;
{$EXTERNALSYM KMOD_NONE}
  KMOD_LSHIFT = $0001;
{$EXTERNALSYM KMOD_LSHIFT}
  KMOD_RSHIFT = $0002;
{$EXTERNALSYM KMOD_RSHIFT}
  KMOD_LCTRL = $0040;
{$EXTERNALSYM KMOD_LCTRL}
  KMOD_RCTRL = $0080;
{$EXTERNALSYM KMOD_RCTRL}
  KMOD_LALT = $0100;
{$EXTERNALSYM KMOD_LALT}
  KMOD_RALT = $0200;
{$EXTERNALSYM KMOD_RALT}
  KMOD_LMETA = $0400;
{$EXTERNALSYM KMOD_LMETA}
  KMOD_RMETA = $0800;
{$EXTERNALSYM KMOD_RMETA}
  KMOD_NUM = $1000;
{$EXTERNALSYM KMOD_NUM}
  KMOD_CAPS = $2000;
{$EXTERNALSYM KMOD_CAPS}
  KMOD_MODE = 44000;
{$EXTERNALSYM KMOD_MODE}
  KMOD_RESERVED = $8000;
{$EXTERNALSYM KMOD_RESERVED}

  KMOD_CTRL = (KMOD_LCTRL or KMOD_RCTRL);
{$EXTERNALSYM KMOD_CTRL}
  KMOD_SHIFT = (KMOD_LSHIFT or KMOD_RSHIFT);
{$EXTERNALSYM KMOD_SHIFT}
  KMOD_ALT = (KMOD_LALT or KMOD_RALT);
{$EXTERNALSYM KMOD_ALT}
  KMOD_META = (KMOD_LMETA or KMOD_RMETA);
{$EXTERNALSYM KMOD_META}

  //SDL_video.h constants
  // Transparency definitions: These define alpha as the opacity of a surface */
  SDL_ALPHA_OPAQUE = 255;
{$EXTERNALSYM SDL_ALPHA_OPAQUE}
  SDL_ALPHA_TRANSPARENT = 0;
{$EXTERNALSYM SDL_ALPHA_TRANSPARENT}

  // These are the currently supported flags for the SDL_surface
  // Available for SDL_CreateRGBSurface() or SDL_SetVideoMode()
  SDL_SWSURFACE = $00000000; // Surface is in system memory
{$EXTERNALSYM SDL_SWSURFACE}
  SDL_HWSURFACE = $00000001; // Surface is in video memory
{$EXTERNALSYM SDL_HWSURFACE}
  SDL_ASYNCBLIT = $00000004; // Use asynchronous blits if possible
{$EXTERNALSYM SDL_ASYNCBLIT}
  // Available for SDL_SetVideoMode()
  SDL_ANYFORMAT = $10000000; // Allow any video depth/pixel-format
{$EXTERNALSYM SDL_ANYFORMAT}
  SDL_HWPALETTE = $20000000; // Surface has exclusive palette
{$EXTERNALSYM SDL_HWPALETTE}
  SDL_DOUBLEBUF = $40000000; // Set up double-buffered video mode
{$EXTERNALSYM SDL_DOUBLEBUF}
  SDL_FULLSCREEN = $80000000; // Surface is a full screen display
{$EXTERNALSYM SDL_FULLSCREEN}
  SDL_OPENGL = $00000002; // Create an OpenGL rendering context
{$EXTERNALSYM SDL_OPENGL}
  SDL_OPENGLBLIT = $00000002; // Create an OpenGL rendering context
{$EXTERNALSYM SDL_OPENGLBLIT}
  SDL_RESIZABLE = $00000010; // This video mode may be resized
{$EXTERNALSYM SDL_RESIZABLE}
  SDL_NOFRAME = $00000020; // No window caption or edge frame
{$EXTERNALSYM SDL_NOFRAME}
  // Used internally (read-only)
  SDL_HWACCEL = $00000100; // Blit uses hardware acceleration
{$EXTERNALSYM SDL_HWACCEL}
  SDL_SRCCOLORKEY = $00001000; // Blit uses a source color key
{$EXTERNALSYM SDL_SRCCOLORKEY}
  SDL_RLEACCELOK = $00002000; // Private flag
{$EXTERNALSYM SDL_RLEACCELOK}
  SDL_RLEACCEL = $00004000; // Colorkey blit is RLE accelerated
{$EXTERNALSYM SDL_RLEACCEL}
  SDL_SRCALPHA = $00010000; // Blit uses source alpha blending
{$EXTERNALSYM SDL_SRCALPHA}
  SDL_SRCCLIPPING = $00100000; // Blit uses source clipping
{$EXTERNALSYM SDL_SRCCLIPPING}
  SDL_PREALLOC = $01000000; // Surface uses preallocated memory
{$EXTERNALSYM SDL_PREALLOC}

  { The most common video overlay formats.
    For an explanation of these pixel formats, see:
    http://www.webartz.com/fourcc/indexyuv.htm

   For information on the relationship between color spaces, see:
   http://www.neuro.sfc.keio.ac.jp/~aly/polygon/info/color-space-faq.html }

  SDL_YV12_OVERLAY = $32315659; // Planar mode: Y + V + U  (3 planes)
{$EXTERNALSYM SDL_YV12_OVERLAY}
  SDL_IYUV_OVERLAY = $56555949; // Planar mode: Y + U + V  (3 planes)
{$EXTERNALSYM SDL_IYUV_OVERLAY}
  SDL_YUY2_OVERLAY = $32595559; // Packed mode: Y0+U0+Y1+V0 (1 plane)
{$EXTERNALSYM SDL_YUY2_OVERLAY}
  SDL_UYVY_OVERLAY = $59565955; // Packed mode: U0+Y0+V0+Y1 (1 plane)
{$EXTERNALSYM SDL_UYVY_OVERLAY}
  SDL_YVYU_OVERLAY = $55595659; // Packed mode: Y0+V0+Y1+U0 (1 plane)
{$EXTERNALSYM SDL_YVYU_OVERLAY}

  // flags for SDL_SetPalette()
  SDL_LOGPAL = $01;
{$EXTERNALSYM SDL_LOGPAL}
  SDL_PHYSPAL = $02;
{$EXTERNALSYM SDL_PHYSPAL}

  //SDL_mouse.h constants
  { Used as a mask when testing buttons in buttonstate
    Button 1:	Left mouse button
    Button 2:	Middle mouse button
    Button 3:	Right mouse button
    Button 4:	Mouse Wheel Up
    Button 5:	Mouse Wheel Down
  }
  SDL_BUTTON_LEFT = 1;
{$EXTERNALSYM SDL_BUTTON_LEFT}
  SDL_BUTTON_MIDDLE = 2;
{$EXTERNALSYM SDL_BUTTON_MIDDLE}
  SDL_BUTTON_RIGHT = 3;
{$EXTERNALSYM SDL_BUTTON_RIGHT}
  SDL_BUTTON_WHEELUP = 4;
{$EXTERNALSYM SDL_BUTTON_WHEELUP}
  SDL_BUTTON_WHEELDOWN = 5;
{$EXTERNALSYM SDL_BUTTON_WHEELDOWN}
  SDL_BUTTON_LMASK = SDL_PRESSED shl (SDL_BUTTON_LEFT - 1);
{$EXTERNALSYM SDL_BUTTON_LMASK}
  SDL_BUTTON_MMASK = SDL_PRESSED shl (SDL_BUTTON_MIDDLE - 1);
{$EXTERNALSYM SDL_BUTTON_MMASK}
  SDL_BUTTON_RMask = SDL_PRESSED shl (SDL_BUTTON_RIGHT - 1);
{$EXTERNALSYM SDL_BUTTON_RMask}

  // SDL_active.h constants
  // The available application states
  SDL_APPMOUSEFOCUS = $01; // The app has mouse coverage
{$EXTERNALSYM SDL_APPMOUSEFOCUS}
  SDL_APPINPUTFOCUS = $02; // The app has input focus
{$EXTERNALSYM SDL_APPINPUTFOCUS}
  SDL_APPACTIVE = $04; // The application is active
{$EXTERNALSYM SDL_APPACTIVE}

  // SDL_mutex.h constants
  // Synchronization functions which can time out return this value
  //  they time out.

  SDL_MUTEX_TIMEDOUT = 1;
{$EXTERNALSYM SDL_MUTEX_TIMEDOUT}

  // This is the timeout value which corresponds to never time out
  SDL_MUTEX_MAXWAIT = not Cardinal(0);
{$EXTERNALSYM SDL_MUTEX_MAXWAIT}

  {TSDL_GrabMode = (
    SDL_GRAB_QUERY,
    SDL_GRAB_OFF,
    SDL_GRAB_ON,
    SDL_GRAB_FULLSCREEN	); // Used internally}
  SDL_GRAB_QUERY = -1;
  SDL_GRAB_OFF   = 0;
  SDL_GRAB_ON    = 1;
  //SDL_GRAB_FULLSCREEN // Used internally

type
  THandle = Cardinal;
  //SDL_types.h types
  // Basic data types

  SDL_Bool  = (SDL_FALSE, SDL_TRUE);
  TSDL_Bool = SDL_Bool;

  PUInt8Array = ^TUInt8Array;
  PUInt8 = ^UInt8;
  UInt8 = Byte;
{$EXTERNALSYM UInt8}
  TUInt8Array = array [0..MAXINT shr 1] of UInt8;

  PUInt16 = ^UInt16;
  UInt16 = word;
{$EXTERNALSYM UInt16}

  PSInt16 = ^SInt16;
  SInt16 = smallint;
{$EXTERNALSYM SInt16}

  PUInt32 = ^UInt32;
  UInt32 = Cardinal;
{$EXTERNALSYM UInt32}

  SInt32 = Integer;
{$EXTERNALSYM SInt32}

  PInt = ^Integer;

  PShortInt = ^ShortInt;

  PUInt64 = ^UInt64;
  UInt64 = record
    hi: UInt32;
    lo: UInt32;
  end;
{$EXTERNALSYM UInt64}

  PSInt64 = ^SInt64;
  SInt64 = record
    hi: UInt32;
    lo: UInt32;
  end;
{$EXTERNALSYM SInt64}

  TSDL_GrabMode = Integer;

  // SDL_error.h types
  TSDL_errorcode = (
    SDL_ENOMEM,
    SDL_EFREAD,
    SDL_EFWRITE,
    SDL_EFSEEK,
    SDL_LASTERROR);

  SDL_errorcode = TSDL_errorcode;
{$EXTERNALSYM SDL_errorcode}

  TArg = record
    case Byte of
      0: (value_ptr: Pointer);
      (* #if 0 means: never
      1 :  ( value_c : Byte );
      *)
      2: (value_i: Integer);
      3: (value_f: double);
      4: (buf: array[0..ERR_MAX_STRLEN - 1] of Byte);
  end;

  PSDL_error = ^TSDL_error;
  TSDL_error = record
    { This is a numeric value corresponding to the current error }
    error: Integer;

    { This is a key used to index into a language hashtable containing
       internationalized versions of the SDL error messages.  If the key
       is not in the hashtable, or no hashtable is available, the key is
       used directly as an error message format string. }
    key: array[0..ERR_MAX_STRLEN - 1] of Byte;

    { These are the arguments for the error functions }
    argc: Integer;
    args: array[0..ERR_MAX_ARGS - 1] of TArg;
  end;

  // SDL_rwops.h types
  // This is the read/write operation structure -- very basic
  // some helper types to handle the unions
  // "packed" is only guessed

  TStdio = record
    autoclose: Integer;
   // FILE * is only defined in Kylix so we use a simple Pointer
    fp: Pointer;
  end;

  TMem = record
    base: PUInt8;
    here: PUInt8;
    stop: PUInt8;
  end;

  TUnknown = record
    data1: Pointer;
  end;

  // first declare the pointer type
  PSDL_RWops = ^TSDL_RWops;
  // now the pointer to function types
  {$IFNDEF __GPC__}
  TSeek = function( context: PSDL_RWops; offset: Integer; whence: Integer ): Integer; cdecl;
  TRead = function( context: PSDL_RWops; Ptr: Pointer; size: Integer; maxnum : Integer ): Integer;  cdecl;
  TWrite = function( context: PSDL_RWops; Ptr: Pointer; size: Integer; num: Integer ): Integer; cdecl;
  TClose = function( context: PSDL_RWops ): Integer; cdecl;
  {$ELSE}
  TSeek = function( context: PSDL_RWops; offset: Integer; whence: Integer ): Integer;
  TRead = function( context: PSDL_RWops; Ptr: Pointer; size: Integer; maxnum : Integer ): Integer;
  TWrite = function( context: PSDL_RWops; Ptr: Pointer; size: Integer; num: Integer ): Integer;
  TClose = function( context: PSDL_RWops ): Integer;
  {$ENDIF}
  // the variant record itself
  TSDL_RWops = record
    seek: TSeek;
    read: TRead;
    write: TWrite;
    close: TClose;
    // a keyword as name is not allowed
    type_: UInt32;
    // be warned! structure alignment may arise at this point
    case Integer of
      0: (stdio: TStdio);
      1: (mem: TMem);
      2: (unknown: TUnknown);
  end;

  SDL_RWops = TSDL_RWops;
{$EXTERNALSYM SDL_RWops}


  // SDL_timer.h types
  // Function prototype for the timer callback function
  {$IFNDEF __GPC__}
  TSDL_TimerCallback = function( interval: UInt32 ): UInt32; cdecl;
  {$ELSE}
  TSDL_TimerCallback = function( interval: UInt32 ): UInt32;
  {$ENDIF}

 { New timer API, supports multiple timers
   Written by Stephane Peter <megastep@lokigames.com> }

 { Function prototype for the new timer callback function.
   The callback function is passed the current timer interval and returns
   the next timer interval.  If the returned value is the same as the one
   passed in, the periodic alarm continues, otherwise a new alarm is
   scheduled.  If the callback returns 0, the periodic alarm is cancelled. }
  {$IFNDEF __GPC__}
  TSDL_NewTimerCallback = function( interval: UInt32; param: Pointer ): UInt32; cdecl;
  {$ELSE}
  TSDL_NewTimerCallback = function( interval: UInt32; param: Pointer ): UInt32;
  {$ENDIF}
  
  // Definition of the timer ID type
  PSDL_TimerID = ^TSDL_TimerID;
  TSDL_TimerID = record
    interval: UInt32;
    callback: TSDL_NewTimerCallback;
    param: Pointer;
    last_alarm: UInt32;
    next: PSDL_TimerID;
  end;

  {$IFNDEF __GPC__}
  TSDL_AudioSpecCallback = procedure( userdata: Pointer; stream: PUInt8; len: Integer ); cdecl;
  {$ELSE}
  TSDL_AudioSpecCallback = procedure( userdata: Pointer; stream: PUInt8; len: Integer );
  {$ENDIF}

  // SDL_audio.h types
  // The calculated values in this structure are calculated by SDL_OpenAudio()
  PSDL_AudioSpec = ^TSDL_AudioSpec;
  TSDL_AudioSpec = record
    freq: Integer; // DSP frequency -- samples per second
    format: UInt16; // Audio data format
    channels: UInt8; // Number of channels: 1 mono, 2 stereo
    silence: UInt8; // Audio buffer silence value (calculated)
    samples: UInt16; // Audio buffer size in samples
    padding: UInt16; // Necessary for some compile environments
    size: UInt32; // Audio buffer size in bytes (calculated)
    { This function is called when the audio device needs more data.
      'stream' is a pointer to the audio data buffer
      'len' is the length of that buffer in bytes.
      Once the callback returns, the buffer will no longer be valid.
      Stereo samples are stored in a LRLRLR ordering.}
    callback: TSDL_AudioSpecCallback;
    userdata: Pointer;
  end;

  // A structure to hold a set of audio conversion filters and buffers
  PSDL_AudioCVT = ^TSDL_AudioCVT;

  PSDL_AudioCVTFilter = ^TSDL_AudioCVTFilter;
  TSDL_AudioCVTFilter = record
    cvt: PSDL_AudioCVT;
    format: UInt16;
  end;

  PSDL_AudioCVTFilterArray = ^TSDL_AudioCVTFilterArray;
  TSDL_AudioCVTFilterArray = array[0..9] of PSDL_AudioCVTFilter;

  TSDL_AudioCVT = record
    needed: Integer; // Set to 1 if conversion possible
    src_format: UInt16; // Source audio format
    dst_format: UInt16; // Target audio format
    rate_incr: double; // Rate conversion increment
    buf: PUInt8; // Buffer to hold entire audio data
    len: Integer; // Length of original audio buffer
    len_cvt: Integer; // Length of converted audio buffer
    len_mult: Integer; // buffer must be len*len_mult big
    len_ratio: double; // Given len, final size is len*len_ratio
    filters: TSDL_AudioCVTFilterArray;
    filter_index: Integer; // Current audio conversion function
  end;

  TSDL_Audiostatus = (
    SDL_AUDIO_STOPPED,
    SDL_AUDIO_PLAYING,
    SDL_AUDIO_PAUSED
    );

  // SDL_cdrom.h types
  TSDL_CDStatus = (
    CD_ERROR,
    CD_TRAYEMPTY,
    CD_STOPPED,
    CD_PLAYING,
    CD_PAUSED );

  PSDL_CDTrack = ^TSDL_CDTrack;
  TSDL_CDTrack = record
    id: UInt8; // Track number
    type_: UInt8; // Data or audio track
    unused: UInt16;
    length: UInt32; // Length, in frames, of this track
    offset: UInt32; // Offset, in frames, from start of disk
  end;

  // This structure is only current as of the last call to SDL_CDStatus()
  PSDL_CD = ^TSDL_CD;
  TSDL_CD = record
    id: Integer; // Private drive identifier
    status: TSDL_CDStatus; // Current drive status

    // The rest of this structure is only valid if there's a CD in drive
    numtracks: Integer; // Number of tracks on disk
    cur_track: Integer; // Current track position
    cur_frame: Integer; // Current frame offset within current track
    track: array[0..SDL_MAX_TRACKS] of TSDL_CDTrack;
  end;

  //SDL_joystick.h types
  PTransAxis = ^TTransAxis;
  TTransAxis = record
    offset: Integer;
    scale: single;
  end;

  // The private structure used to keep track of a joystick
  PJoystick_hwdata = ^TJoystick_hwdata;
  TJoystick_hwdata = record
    // joystick ID
    id: Integer;
    // values used to translate device-specific coordinates into  SDL-standard ranges
    transaxis: array[0..5] of TTransAxis;
  end;

  PBallDelta = ^TBallDelta;
  TBallDelta = record
    dx: Integer;
    dy: Integer;
  end; // Current ball motion deltas

  // The SDL joystick structure
  PSDL_Joystick = ^TSDL_Joystick;
  TSDL_Joystick = record
    index: UInt8; // Device index
    name: PChar; // Joystick name - system dependent

    naxes: Integer; // Number of axis controls on the joystick
    axes: PUInt16; // Current axis states

    nhats: Integer; // Number of hats on the joystick
    hats: PUInt8; // Current hat states

    nballs: Integer; // Number of trackballs on the joystick
    balls: PBallDelta; // Current ball motion deltas

    nbuttons: Integer; // Number of buttons on the joystick
    buttons: PUInt8; // Current button states

    hwdata: PJoystick_hwdata; // Driver dependent information

    ref_count: Integer; // Reference count for multiple opens
  end;

  // SDL_verion.h types
  PSDL_version = ^TSDL_version;
  TSDL_version = record
    major: UInt8;
    minor: UInt8;
    patch: UInt8;
  end;

  // SDL_keyboard.h types
  TSDLKey = LongWord;

  TSDLMod = LongWord;

  PSDL_KeySym = ^TSDL_KeySym;
  TSDL_KeySym = record
    scancode: UInt8; // hardware specific scancode
    sym: TSDLKey; // SDL virtual keysym
    modifier: TSDLMod; // current key modifiers
    unicode: UInt16; // translated character
  end;

  // SDL_events.h types
  {Checks the event queue for messages and optionally returns them.
   If 'action' is SDL_ADDEVENT, up to 'numevents' events will be added to
   the back of the event queue.
   If 'action' is SDL_PEEKEVENT, up to 'numevents' events at the front
   of the event queue, matching 'mask', will be returned and will not
   be removed from the queue.
   If 'action' is SDL_GETEVENT, up to 'numevents' events at the front
   of the event queue, matching 'mask', will be returned and will be
   removed from the queue.
   This function returns the number of events actually stored, or -1
   if there was an error.  This function is thread-safe. }

  TSDL_EventAction = (SDL_ADDEVENT, SDL_PEEKEVENT, SDL_GETEVENT);

  // Application visibility event structure
  TSDL_ActiveEvent = record
    type_: UInt8; // SDL_ACTIVEEVENT
    gain: UInt8; // Whether given states were gained or lost (1/0)
    state: UInt8; // A mask of the focus states
  end;

  // Keyboard event structure
  TSDL_KeyboardEvent = record
    type_: UInt8; // SDL_KEYDOWN or SDL_KEYUP
    which: UInt8; // The keyboard device index
    state: UInt8; // SDL_PRESSED or SDL_RELEASED
    keysym: TSDL_KeySym;
  end;

  // Mouse motion event structure
  TSDL_MouseMotionEvent = record
    type_: UInt8; // SDL_MOUSEMOTION
    which: UInt8; // The mouse device index
    state: UInt8; // The current button state
    x, y: UInt16; // The X/Y coordinates of the mouse
    xrel: SInt16; // The relative motion in the X direction
    yrel: SInt16; // The relative motion in the Y direction
  end;

  // Mouse button event structure
  TSDL_MouseButtonEvent = record
    type_: UInt8;  // SDL_MOUSEBUTTONDOWN or SDL_MOUSEBUTTONUP
    which: UInt8;  // The mouse device index
    button: UInt8; // The mouse button index
    state: UInt8;  // SDL_PRESSED or SDL_RELEASED
    x: UInt16;     // The X coordinates of the mouse at press time
    y: UInt16;     // The Y coordinates of the mouse at press time
  end;

  // Joystick axis motion event structure
  TSDL_JoyAxisEvent = record
    type_: UInt8; // SDL_JOYAXISMOTION
    which: UInt8; // The joystick device index
    axis: UInt8; // The joystick axis index
    value: SInt16; // The axis value (range: -32768 to 32767)
  end;

  // Joystick trackball motion event structure
  TSDL_JoyBallEvent = record
    type_: UInt8; // SDL_JOYAVBALLMOTION
    which: UInt8; // The joystick device index
    ball: UInt8; // The joystick trackball index
    xrel: SInt16; // The relative motion in the X direction
    yrel: SInt16; // The relative motion in the Y direction
  end;

  // Joystick hat position change event structure
  TSDL_JoyHatEvent = record
    type_: UInt8; // SDL_JOYHATMOTION */
    which: UInt8; // The joystick device index */
    hat: UInt8; // The joystick hat index */
    value: UInt8; { The hat position value:
                    8   1   2
                    7   0   3
                    6   5   4

                    Note that zero means the POV is centered. }

  end;

  // Joystick button event structure
  TSDL_JoyButtonEvent = record
    type_: UInt8; // SDL_JOYBUTTONDOWN or SDL_JOYBUTTONUP
    which: UInt8; // The joystick device index
    button: UInt8; // The joystick button index
    state: UInt8; // SDL_PRESSED or SDL_RELEASED
  end;

  { The "window resized" event
    When you get this event, you are responsible for setting a new video
    mode with the new width and height. }
  TSDL_ResizeEvent = record
    type_: UInt8; // SDL_VIDEORESIZE
    w: Integer; // New width
    h: Integer; // New height
  end;

  // The "quit requested" event
  PSDL_QuitEvent = ^TSDL_QuitEvent;
  TSDL_QuitEvent = record
    type_: UInt8;
  end;

  // A user-defined event type
  PSDL_UserEvent = ^TSDL_UserEvent;
  TSDL_UserEvent = record
    type_: UInt8; // SDL_USEREVENT through SDL_NUMEVENTS-1
    code: Integer; // User defined event code */
    data1: Pointer; // User defined data pointer */
    data2: Pointer; // User defined data pointer */
  end;

  // The "screen redraw" event
  PSDL_ExposeEvent = ^TSDL_ExposeEvent;
  TSDL_ExposeEvent = record
    type_ : Uint8;        // SDL_VIDEOEXPOSE
  end;

 {$IFDEF Unix}
 //These are the various supported subsystems under UNIX
  TSDL_SysWm = ( SDL_SYSWM_X11 ) ;
 {$ENDIF}

// The windows custom event structure
{$IFDEF Win32}
  PSDL_SysWMmsg = ^TSDL_SysWMmsg;
  TSDL_SysWMmsg = record
    version: TSDL_version;
    h_wnd: HWND; // The window for the message
    msg: UInt; // The type of message
    w_Param: WPARAM; // WORD message parameter
    lParam: LPARAM; // LONG message parameter
  end;
{$ELSE}

{$IFDEF Unix}
{ The Linux custom event structure }
  PSDL_SysWMmsg = ^TSDL_SysWMmsg;
  TSDL_SysWMmsg = record
    version : TSDL_version;
    subsystem : TSDL_SysWm;
    {$IFDEF FPC}
    event : TXEvent;
    {$ELSE}
    event : XEvent;
    {$ENDIF}
  end;
{$ELSE}
{ The generic custom event structure }
  PSDL_SysWMmsg = ^TSDL_SysWMmsg;
  TSDL_SysWMmsg = record
    version: TSDL_version;
    data: Integer;
  end;
{$ENDIF}

{$ENDIF}

// The Windows custom window manager information structure
{$IFDEF Win32}
  PSDL_SysWMinfo = ^TSDL_SysWMinfo;
  TSDL_SysWMinfo = record
    version : TSDL_version;
    window : HWnd;	// The display window
  end;
{$ELSE}

// The Linux custom window manager information structure
{$IFDEF Unix}
  TX11 = record
    display : PDisplay;	// The X11 display
    window : TWindow ;		// The X11 display window */
    {* These locking functions should be called around
       any X11 functions using the display variable.
       They lock the event thread, so should not be
       called around event functions or from event filters.
     *}
    lock_func : Pointer;
    unlock_func : Pointer;

    // Introduced in SDL 1.0.2
    fswindow : TWindow ;	// The X11 fullscreen window */
    wmwindow : TWindow ;	// The X11 managed input window */
  end;

  PSDL_SysWMinfo = ^TSDL_SysWMinfo;
  TSDL_SysWMinfo = record
     version : TSDL_version ;
     subsystem : TSDL_SysWm;
     X11 : TX11;
  end;
{$ELSE}
  // The generic custom window manager information structure
  PSDL_SysWMinfo = ^TSDL_SysWMinfo;
  TSDL_SysWMinfo = record
    version : TSDL_version ;
    data : integer;
  end;
{$ENDIF}

{$ENDIF}

  PSDL_SysWMEvent = ^TSDL_SysWMEvent;
  TSDL_SysWMEvent = record
    type_: UInt8;
    msg: PSDL_SysWMmsg;
  end;

  PSDL_Event = ^TSDL_Event;
  TSDL_Event = record
    case UInt8 of
      SDL_NOEVENT: (type_: byte);
      SDL_ACTIVEEVENT: (active: TSDL_ActiveEvent);
      SDL_KEYDOWN, SDL_KEYUP: (key: TSDL_KeyboardEvent);
      SDL_MOUSEMOTION: (motion: TSDL_MouseMotionEvent);
      SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP: (button: TSDL_MouseButtonEvent );
      SDL_JOYAXISMOTION: (jaxis: TSDL_JoyAxisEvent );
      SDL_JOYBALLMOTION: (jball: TSDL_JoyBallEvent );
      SDL_JOYHATMOTION: (jhat: TSDL_JoyHatEvent );
      SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP: (jbutton: TSDL_JoyButtonEvent );
      SDL_VIDEORESIZE: (resize: TSDL_ResizeEvent );
      SDL_QUITEV: (quit: TSDL_QuitEvent );
      SDL_USEREVENT : ( user : TSDL_UserEvent );
      SDL_SYSWMEVENT: (syswm: TSDL_SysWMEvent );
  end;


{ This function sets up a filter to process all events before they
  change internal state and are posted to the internal event queue.

  The filter is protypted as: }
  {$IFNDEF __GPC__}
  TSDL_EventFilter = function( event : PSDL_Event ): Integer; cdecl;
  {$ELSE}
  TSDL_EventFilter = function( event : PSDL_Event ): Integer;
  {$ENDIF}

  // SDL_video.h types
  // Useful data types
  PPSDL_Rect = ^PSDL_Rect;
  PSDL_Rect = ^TSDL_Rect;
  TSDL_Rect = record
    x, y: SInt16;
    w, h: UInt16;
  end;

  SDL_Rect = TSDL_Rect;
{$EXTERNALSYM SDL_Rect}

  PSDL_Color = ^TSDL_Color;
  TSDL_Color = record
    r: UInt8;
    g: UInt8;
    b: UInt8;
    unused: UInt8;
  end;

  PSDL_ColorArray = ^TSDL_ColorArray;
  TSDL_ColorArray = array[0..65000] of TSDL_Color;

  PSDL_Palette = ^TSDL_Palette;
  TSDL_Palette = record
    ncolors: Integer;
    colors: PSDL_ColorArray;
  end;

  // Everything in the pixel format structure is read-only
  PSDL_PixelFormat = ^TSDL_PixelFormat;
  TSDL_PixelFormat = record
    palette: PSDL_Palette;
    BitsPerPixel: UInt8;
    BytesPerPixel: UInt8;
    Rloss: UInt8;
    Gloss: UInt8;
    Bloss: UInt8;
    Aloss: UInt8;
    Rshift: UInt8;
    Gshift: UInt8;
    Bshift: UInt8;
    Ashift: UInt8;
    RMask: UInt32;
    GMask: UInt32;
    BMask: UInt32;
    AMask: UInt32;
    colorkey: UInt32; // RGB color key information
    alpha: UInt8; // Alpha value information (per-surface alpha)
  end;

{$IFDEF WIN32}
  {PPrivate_hwdata = ^TPrivate_hwdata;
  TPrivate_hwdata = record
    dd_surface : IDIRECTDRAWSURFACE3;
    dd_writebuf : IDIRECTDRAWSURFACE3;
  end;}
  {ELSE}
{$ENDIF}

  // The structure passed to the low level blit functions
  PSDL_BlitInfo = ^TSDL_BlitInfo;
  TSDL_BlitInfo = record
    s_pixels: PUInt8;
    s_width: Integer;
    s_height: Integer;
    s_skip: Integer;
    d_pixels: PUInt8;
    d_width: Integer;
    d_height: Integer;
    d_skip: Integer;
    aux_data: Pointer;
    src: PSDL_PixelFormat;
    table: PUInt8;
    dst: PSDL_PixelFormat;
  end;

  // typedef for private surface blitting functions
  PSDL_Surface = ^TSDL_Surface;

  {$IFNDEF __GPC__}
  TSDL_Blit = function( src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect ): Integer; cdecl;
  {$ELSE}
  TSDL_Blit = function( src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect ): Integer;
  {$ENDIF}

  // The type definition for the low level blit functions
  //TSDL_LoBlit = procedure( info : PSDL_BlitInfo ); cdecl;

  // This is the private info structure for software accelerated blits
  {PPrivate_swaccel = ^TPrivate_swaccel;
  TPrivate_swaccel = record
    blit : TSDL_LoBlit;
    aux_data : Pointer;
  end;}

  // Blit mapping definition
  {PSDL_BlitMap = ^TSDL_BlitMap;
  TSDL_BlitMap = record
    dst : PSDL_Surface;
    identity : Integer;
    table : PUInt8;
    hw_blit : TSDL_Blit;
    sw_blit : TSDL_Blit;
    hw_data : PPrivate_hwaccel;
    sw_data : PPrivate_swaccel;

    // the version count matches the destination; mismatch indicates an invalid mapping
    format_version : Cardinal;
  end;}

  TSDL_Surface = record
    flags: UInt32; // Read-only
    format: PSDL_PixelFormat; // Read-only
    w, h: Integer; // Read-only
    pitch: UInt16; // Read-only
    pixels: Pointer; // Read-write
    offset: Integer; // Private
    hwdata: Pointer; //TPrivate_hwdata;  Hardware-specific surface info

    // clipping information:
    clip_rect: TSDL_Rect; // Read-only
    unused1: UInt32; // for binary compatibility
    // Allow recursive locks
    locked: UInt32; // Private
    // info for fast blit mapping to other surfaces
    Blitmap: Pointer; // PSDL_BlitMap; //   Private
    // format version, bumped at every change to invalidate blit maps
    format_version: Cardinal; // Private
    refcount: Integer;
  end;

  // Useful for determining the video hardware capabilities
  PSDL_VideoInfo = ^TSDL_VideoInfo;
  TSDL_VideoInfo = record
    hw_available: UInt8; // Hardware and WindowManager flags in first 2 bits ( see below )
    {hw_available: 1; // Can you create hardware surfaces
    wm_available: 1; // Can you talk to a window manager?
    UnusedBits1: 6;}
    blit_hw: UInt8; // Blit Hardware flags. See below for which bits do what
    {UnusedBits2: 1;
    blit_hw: 1; // Flag:UInt32  Accelerated blits HW --> HW
    blit_hw_CC: 1; // Flag:UInt32  Accelerated blits with Colorkey
    blit_hw_A: 1; // Flag:UInt32  Accelerated blits with Alpha
    blit_sw: 1; // Flag:UInt32  Accelerated blits SW --> HW
    blit_sw_CC: 1; // Flag:UInt32  Accelerated blits with Colorkey
    blit_sw_A: 1; // Flag:UInt32  Accelerated blits with Alpha
    blit_fill: 1; // Flag:UInt32  Accelerated color fill}
    UnusedBits3: UInt8; // Unused at this point
    video_mem: UInt32; // The total amount of video memory (in K)
    vfmt: PSDL_PixelFormat; // Value: The format of the video surface
  end;

  // The YUV hardware video overlay
  PSDL_Overlay = ^TSDL_Overlay;
  TSDL_Overlay = record
    format: UInt32; // Overlay format
    w, h: Integer; // Width and height of overlay
    planes: Integer; // Number of planes in the overlay. Usually either 1 or 3
    pitches: PUInt16;
      // An array of pitches, one for each plane. Pitch is the length of a row in bytes.
    pixels: PUInt8;
      // An array of pointers to teh data of each plane. The overlay should be locked before these pointers are used.
    hw_overlay: UInt32;
      // This will be set to 1 if the overlay is hardware accelerated.
  end;

  // Public enumeration for setting the OpenGL window attributes.
  TSDL_GLAttr = (
    SDL_GL_RED_SIZE,
    SDL_GL_GREEN_SIZE,
    SDL_GL_BLUE_SIZE,
    SDL_GL_ALPHA_SIZE,
    SDL_GL_BUFFER_SIZE,
    SDL_GL_DOUBLEBUFFER,
    SDL_GL_DEPTH_SIZE,
    SDL_GL_STENCIL_SIZE,
    SDL_GL_ACCUM_RED_SIZE,
    SDL_GL_ACCUM_GREEN_SIZE,
    SDL_GL_ACCUM_BLUE_SIZE,
    SDL_GL_ACCUM_ALPHA_SIZE,
    SDL_GL_STEREO,
    SDL_GL_MULTISAMPLEBUFFERS,
    SDL_GL_MULTISAMPLESAMPLES);



  PSDL_Cursor = ^TSDL_Cursor;
  TSDL_Cursor = record
    area: TSDL_Rect; // The area of the mouse cursor
    hot_x, hot_y: SInt16; // The "tip" of the cursor
    data: PUInt8; // B/W cursor data
    mask: PUInt8; // B/W cursor mask
    save: array[1..2] of PUInt8; // Place to save cursor area
    wm_cursor: Pointer; // Window-manager cursor
  end;

// SDL_mutex.h types

{$IFDEF WIN32}
  PSDL_Mutex = ^TSDL_Mutex;
  TSDL_Mutex = record
    id: THANDLE;
  end;
{$ENDIF}

{$IFDEF Unix}
  PSDL_Mutex = ^TSDL_Mutex;
  TSDL_mutex = record
    id: pthread_mutex_t;
{$IFDEF PTHREAD_NO_RECURSIVE_MUTEX}
    recursive: Integer;
    owner: pthread_t;
{$ENDIF}
  end;
{$ENDIF}

{$IFDEF __MACH__}
  {$define USE_NAMED_SEMAPHORES}
  // Broken sem_getvalue() in MacOS X Public Beta */
  {$define BROKEN_SEMGETVALUE}
{$ENDIF}

PSDL_semaphore = ^TSDL_semaphore;
{$IFDEF WIN32}
  // Win32 or Machintosh
  TSDL_semaphore = record
    id: THANDLE;
    count: UInt32;
  end;
{$ELSE}
  {$IFDEF FPC}
  // This should be semaphore.h 
  __sem_lock_t = {packed} record { Not in header file - anonymous }
    status: Longint;
    spinlock: Integer;
  end;

  sem_t = {packed} record
    __sem_lock: __sem_lock_t;
    __sem_value: Integer;
    __sem_waiting: longint ; {_pthread_queue;}
  end;
  {$ENDIF}
  
  TSDL_semaphore = record
    sem: Pointer; //PSem_t;
  {$IFNDEF USE_NAMED_SEMAPHORES}
    sem_data: Sem_t;
  {$ENDIF}

  {$IFDEF BROKEN_SEMGETVALUE}
    { This is a little hack for MacOS X -
      It's not thread-safe, but it's better than nothing }
    sem_value: Integer;
  {$ENDIF}
  end;
{$ENDIF}

  PSDL_Sem = ^TSDL_Sem;
  TSDL_Sem = TSDL_Semaphore;

  PSDL_Cond = ^TSDL_Cond;
  TSDL_Cond = record
{$IFDEF Unix}
    cond: pthread_cond_t;
{$ELSE}
    // Generic Cond structure
    lock: PSDL_mutex;
    waiting: Integer;
    signals: Integer;
    wait_sem: PSDL_Sem;
    wait_done: PSDL_Sem;
{$ENDIF}
  end;

  // SDL_thread.h types
{$IFDEF WIN32}
  TSYS_ThreadHandle = THandle;
{$ENDIF}

{$IFDEF Unix}
  TSYS_ThreadHandle = pthread_t;
{$ENDIF}

  { This is the system-independent thread info structure }
  PSDL_Thread = ^TSDL_Thread;
  TSDL_Thread = record
    threadid: UInt32;
    handle: TSYS_ThreadHandle;
    status: Integer;
    errbuf: TSDL_Error;
    data: Pointer;
  end;

  // Helper Types

  // Keyboard  State Array ( See demos for how to use )
  PKeyStateArr = ^TKeyStateArr;
  TKeyStateArr = array[0..65000] of UInt8;

  // Types required so we don't need to use Windows.pas
  PInteger = ^Integer;
  PByte = ^Byte;
  PWord = ^Word;
  PLongWord = ^Longword;

  // General arrays
  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;

  PWordArray = ^TWordArray;
  TWordArray = array[0..16383] of Word;

  PPoint = ^TPoint;
  TPoint = record
    x: Longint;
    y: Longint;
  end;

  PRect = ^TRect;
  TRect = record
    case Integer of
      0: (Left, Top, Right, Bottom: Integer);
      1: (TopLeft, BottomRight: TPoint);
  end;

  { Generic procedure pointer }
  TProcedure = procedure;

{------------------------------------------------------------------------------}
{ initialization                                                               }
{------------------------------------------------------------------------------}

{ This function loads the SDL dynamically linked library and initializes
  the subsystems specified by 'flags' (and those satisfying dependencies)
  Unless the SDL_INIT_NOPARACHUTE flag is set, it will install cleanup
  signal handlers for some commonly ignored fatal signals (like SIGSEGV) }

function SDL_Init( flags : UInt32 ) : Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_Init'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_Init}

// This function initializes specific SDL subsystems
function SDL_InitSubSystem( flags : UInt32 ) : Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_InitSubSystem'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_InitSubSystem}

// This function cleans up specific SDL subsystems
procedure SDL_QuitSubSystem( flags : UInt32 );
cdecl; external {$IFDEF __GPC__}name 'SDL_QuitSubSystem'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_QuitSubSystem}

{ This function returns mask of the specified subsystems which have
  been initialized.
  If 'flags' is 0, it returns a mask of all initialized subsystems. }

function SDL_WasInit( flags : UInt32 ): UInt32;
cdecl; external {$IFDEF __GPC__}name 'SDL_WasInit'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_WasInit}

{ This function cleans up all initialized subsystems and unloads the
  dynamically linked library.  You should call it upon all exit conditions. }
procedure SDL_Quit;
cdecl; external {$IFDEF __GPC__}name 'SDL_Quit'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_Quit}

{$IFDEF WIN32}
// This should be called from your WinMain() function, if any
function SDL_RegisterApp(name: PChar; style: UInt32; h_Inst: Pointer): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_RegisterApp'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_RegisterApp}
{$ENDIF}

{$IFDEF __MACH__}
// This should be called from your main() function, if any
procedure SDL_InitQuickDraw( the_qd: QDGlobals );
cdecl; external {$IFDEF __GPC__}name 'SDL_InitQuickDraw'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_InitQuickDraw}
{$ENDIF}


{------------------------------------------------------------------------------}
{ types }
{------------------------------------------------------------------------------}
// The number of elements in a table
function SDL_TableSize( table: PChar ): Integer;
{$EXTERNALSYM SDL_TABLESIZE}


{------------------------------------------------------------------------------}
{ error-handling }
{------------------------------------------------------------------------------}
// Public functions
function SDL_GetError: PChar;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetError'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetError}
procedure SDL_SetError(fmt: PChar);
cdecl; external {$IFDEF __GPC__}name 'SDL_SetError'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SetError}
procedure SDL_ClearError;
cdecl; external {$IFDEF __GPC__}name 'SDL_ClearError'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_ClearError}

{$IFNDEF WIN32}
procedure SDL_Error(Code: TSDL_errorcode);
cdecl; external {$IFDEF __GPC__}name 'SDL_Error'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_Error}
{$ENDIF}

// Private error message function - used internally
procedure SDL_OutOfMemory;

{------------------------------------------------------------------------------}
{ io handling                                                                  }
{------------------------------------------------------------------------------}
// Functions to create SDL_RWops structures from various data sources

function SDL_RWFromFile(filename, mode: PChar): PSDL_RWops;
cdecl; external {$IFDEF __GPC__}name 'SDL_RWFromFile'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_RWFromFile}
procedure SDL_FreeRW(area: PSDL_RWops);
cdecl; external {$IFDEF __GPC__}name 'SDL_FreeRW'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_FreeRW}

//fp is FILE *fp ???
function SDL_RWFromFP(fp: Pointer; autoclose: Integer): PSDL_RWops;
cdecl; external {$IFDEF __GPC__}name 'SDL_RWFromFP'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_RWFromFP}
function SDL_RWFromMem(mem: Pointer; size: Integer): PSDL_RWops;
cdecl; external {$IFDEF __GPC__}name 'SDL_RWFromMem'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_RWFromMem}
function SDL_RWFromConstMem(const mem: Pointer; size: Integer) : PSDL_RWops;
cdecl; external {$IFDEF __GPC__}name 'SDL_RWFromConstMem'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_RWFromConstMem}
function SDL_AllocRW: PSDL_RWops;
cdecl; external {$IFDEF __GPC__}name 'SDL_AllocRW'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_AllocRW}

function SDL_RWSeek(context: PSDL_RWops; offset: Integer; whence: Integer) : Integer;
{$EXTERNALSYM SDL_RWSeek}
function SDL_RWTell(context: PSDL_RWops): Integer;
{$EXTERNALSYM SDL_RWTell}
function SDL_RWRead(context: PSDL_RWops; ptr: Pointer; size: Integer; n : Integer): Integer;
{$EXTERNALSYM SDL_RWRead}
function SDL_RWWrite(context: PSDL_RWops; ptr: Pointer; size: Integer; n : Integer): Integer;
{$EXTERNALSYM SDL_RWWrite}
function SDL_RWClose(context: PSDL_RWops): Integer;
{$EXTERNALSYM SDL_RWClose}

{------------------------------------------------------------------------------}
{ time-handling                                                                }
{------------------------------------------------------------------------------}

{ Get the number of milliseconds since the SDL library initialization. }
{ Note that this value wraps if the program runs for more than ~49 days. }
function SDL_GetTicks: UInt32;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetTicks'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetTicks}

// Wait a specified number of milliseconds before returning
procedure SDL_Delay(msec: UInt32);
cdecl; external {$IFDEF __GPC__}name 'SDL_Delay'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_Delay}

{ Add a new timer to the pool of timers already running. }
{ Returns a timer ID, or NULL when an error occurs.      }
function SDL_AddTimer(interval: UInt32; callback: TSDL_NewTimerCallback; param : Pointer): PSDL_TimerID;
cdecl; external {$IFDEF __GPC__}name 'SDL_AddTimer'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_AddTimer}

{ Remove one of the multiple timers knowing its ID. }
{ Returns a boolean value indicating success. }
function SDL_RemoveTimer(t: PSDL_TimerID): TSDL_Bool;
cdecl; external {$IFDEF __GPC__}name 'SDL_RemoveTimer'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_RemoveTimer}

function SDL_SetTimer(interval: UInt32; callback: TSDL_TimerCallback): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_SetTimer'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SetTimer}

{------------------------------------------------------------------------------}
{ audio-routines                                                               }
{------------------------------------------------------------------------------}

{ These functions are used internally, and should not be used unless you
  have a specific need to specify the audio driver you want to use.
  You should normally use SDL_Init() or SDL_InitSubSystem(). }

function SDL_AudioInit(driver_name: PChar): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_AudioInit'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_AudioInit}
procedure SDL_AudioQuit;
cdecl; external {$IFDEF __GPC__}name 'SDL_AudioQuit'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_AudioQuit}

{ This function fills the given character buffer with the name of the
  current audio driver, and returns a Pointer to it if the audio driver has
  been initialized.  It returns NULL if no driver has been initialized. }

function SDL_AudioDriverName(namebuf: PChar; maxlen: Integer): PChar;
cdecl; external {$IFDEF __GPC__}name 'SDL_AudioDriverName'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_AudioDriverName}

{ This function opens the audio device with the desired parameters, and
  returns 0 if successful, placing the actual hardware parameters in the
  structure pointed to by 'obtained'.  If 'obtained' is NULL, the audio
  data passed to the callback function will be guaranteed to be in the
  requested format, and will be automatically converted to the hardware
  audio format if necessary.  This function returns -1 if it failed
  to open the audio device, or couldn't set up the audio thread.

  When filling in the desired audio spec structure,
   'desired->freq' should be the desired audio frequency in samples-per-second.
   'desired->format' should be the desired audio format.
   'desired->samples' is the desired size of the audio buffer, in samples.
      This number should be a power of two, and may be adjusted by the audio
      driver to a value more suitable for the hardware.  Good values seem to
      range between 512 and 8096 inclusive, depending on the application and
      CPU speed.  Smaller values yield faster response time, but can lead
      to underflow if the application is doing heavy processing and cannot
      fill the audio buffer in time.  A stereo sample consists of both right
      and left channels in LR ordering.
      Note that the number of samples is directly related to time by the
      following formula:  ms = (samples*1000)/freq
   'desired->size' is the size in bytes of the audio buffer, and is
      calculated by SDL_OpenAudio().
   'desired->silence' is the value used to set the buffer to silence,
      and is calculated by SDL_OpenAudio().
   'desired->callback' should be set to a function that will be called
      when the audio device is ready for more data.  It is passed a pointer
      to the audio buffer, and the length in bytes of the audio buffer.
      This function usually runs in a separate thread, and so you should
      protect data structures that it accesses by calling SDL_LockAudio()
      and SDL_UnlockAudio() in your code.
   'desired->userdata' is passed as the first parameter to your callback
      function.

  The audio device starts out playing silence when it's opened, and should
  be enabled for playing by calling SDL_PauseAudio(0) when you are ready
  for your audio callback function to be called.  Since the audio driver
  may modify the requested size of the audio buffer, you should allocate
  any local mixing buffers after you open the audio device. }

function SDL_OpenAudio(desired, obtained: PSDL_AudioSpec): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_OpenAudio'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_OpenAudio}

{ Get the current audio state: }
function SDL_GetAudioStatus: TSDL_Audiostatus;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetAudioStatus'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetAudioStatus}

{ This function pauses and unpauses the audio callback processing.
  It should be called with a parameter of 0 after opening the audio
  device to start playing sound.  This is so you can safely initialize
  data for your callback function after opening the audio device.
  Silence will be written to the audio device during the pause. }

procedure SDL_PauseAudio(pause_on: Integer);
cdecl; external {$IFDEF __GPC__}name 'SDL_PauseAudio'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_PauseAudio}

{ This function loads a WAVE from the data source, automatically freeing
  that source if 'freesrc' is non-zero.  For example, to load a WAVE file,
  you could do:
  SDL_LoadWAV_RW(SDL_RWFromFile("sample.wav", "rb"), 1, ...);

  If this function succeeds, it returns the given SDL_AudioSpec,
  filled with the audio data format of the wave data, and sets
  'audio_buf' to a malloc()'d buffer containing the audio data,
  and sets 'audio_len' to the length of that audio buffer, in bytes.
  You need to free the audio buffer with SDL_FreeWAV() when you are
  done with it.

  This function returns NULL and sets the SDL error message if the
  wave file cannot be opened, uses an unknown data format, or is
  corrupt.  Currently raw and MS-ADPCM WAVE files are supported. }

function SDL_LoadWAV_RW(src: PSDL_RWops; freesrc: Integer; spec:
  PSDL_AudioSpec; audio_buf: PUInt8; audiolen: PUInt32): PSDL_AudioSpec;
cdecl; external {$IFDEF __GPC__}name 'SDL_LoadWAV_RW'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_LoadWAV_RW}

// Compatibility convenience function -- loads a WAV from a file
function SDL_LoadWAV(filename: PChar; spec: PSDL_AudioSpec; audio_buf:
  PUInt8; audiolen: PUInt32): PSDL_AudioSpec;
{$EXTERNALSYM SDL_LoadWAV}

{ This function frees data previously allocated with SDL_LoadWAV_RW() }

procedure SDL_FreeWAV(audio_buf: PUInt8);
cdecl; external {$IFDEF __GPC__}name 'SDL_FreeWAV'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_FreeWAV}

{ This function takes a source format and rate and a destination format
  and rate, and initializes the 'cvt' structure with information needed
  by SDL_ConvertAudio() to convert a buffer of audio data from one format
  to the other.
  This function returns 0, or -1 if there was an error. }
function SDL_BuildAudioCVT(cvt: PSDL_AudioCVT; src_format: UInt16;
  src_channels: UInt8; src_rate: Integer; dst_format: UInt16; dst_channels: UInt8;
  dst_rate: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_BuildAudioCVT'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_BuildAudioCVT}

{ Once you have initialized the 'cvt' structure using SDL_BuildAudioCVT(),
  created an audio buffer cvt->buf, and filled it with cvt->len bytes of
  audio data in the source format, this function will convert it in-place
  to the desired format.
  The data conversion may expand the size of the audio data, so the buffer
  cvt->buf should be allocated after the cvt structure is initialized by
  SDL_BuildAudioCVT(), and should be cvt->len*cvt->len_mult bytes long. }
function SDL_ConvertAudio(cvt: PSDL_AudioCVT): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_ConvertAudio'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_ConvertAudio}

{ This takes two audio buffers of the playing audio format and mixes
  them, performing addition, volume adjustment, and overflow clipping.
  The volume ranges from 0 - 128, and should be set to SDL_MIX_MAXVOLUME
  for full audio volume.  Note this does not change hardware volume.
  This is provided for convenience -- you can mix your own audio data. }

procedure SDL_MixAudio(dst, src: PUInt8; len: UInt32; volume: Integer);
cdecl; external {$IFDEF __GPC__}name 'SDL_MixAudio'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_MixAudio}

{ The lock manipulated by these functions protects the callback function.
  During a LockAudio/UnlockAudio pair, you can be guaranteed that the
  callback function is not running.  Do not call these from the callback
  function or you will cause deadlock. }
procedure SDL_LockAudio;
cdecl; external {$IFDEF __GPC__}name 'SDL_LockAudio'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_LockAudio}
procedure SDL_UnlockAudio;
cdecl; external {$IFDEF __GPC__}name 'SDL_UnlockAudio'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_UnlockAudio}

{ This function shuts down audio processing and closes the audio device. }

procedure SDL_CloseAudio;
cdecl; external {$IFDEF __GPC__}name 'SDL_CloseAudio'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CloseAudio}

{------------------------------------------------------------------------------}
{ CD-routines }
{------------------------------------------------------------------------------}

{ Returns the number of CD-ROM drives on the system, or -1 if
  SDL_Init() has not been called with the SDL_INIT_CDROM flag. }

function SDL_CDNumDrives: Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_CDNumDrives'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CDNumDrives}

{ Returns a human-readable, system-dependent identifier for the CD-ROM.
   Example:
   "/dev/cdrom"
   "E:"
   "/dev/disk/ide/1/master" }

function SDL_CDName(drive: Integer): PChar;
cdecl; external {$IFDEF __GPC__}name 'SDL_CDName'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CDName}

{ Opens a CD-ROM drive for access.  It returns a drive handle on success,
  or NULL if the drive was invalid or busy.  This newly opened CD-ROM
  becomes the default CD used when other CD functions are passed a NULL
  CD-ROM handle.
  Drives are numbered starting with 0.  Drive 0 is the system default CD-ROM. }

function SDL_CDOpen(drive: Integer): PSDL_CD;
cdecl; external {$IFDEF __GPC__}name 'SDL_CDOpen'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CDOpen}

{ This function returns the current status of the given drive.
  If the drive has a CD in it, the table of contents of the CD and current
  play position of the CD will be stored in the SDL_CD structure. }

function SDL_CDStatus(cdrom: PSDL_CD): TSDL_CDStatus;
cdecl; external {$IFDEF __GPC__}name 'SDL_CDStatus'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CDStatus}

{  Play the given CD starting at 'start_track' and 'start_frame' for 'ntracks'
   tracks and 'nframes' frames.  If both 'ntrack' and 'nframe' are 0, play
   until the end of the CD.  This function will skip data tracks.
   This function should only be called after calling SDL_CDStatus() to
   get track information about the CD.

   For example:
   // Play entire CD:
  if ( CD_INDRIVE(SDL_CDStatus(cdrom)) ) then
    SDL_CDPlayTracks(cdrom, 0, 0, 0, 0);
   // Play last track:
   if ( CD_INDRIVE(SDL_CDStatus(cdrom)) ) then
   begin
    SDL_CDPlayTracks(cdrom, cdrom->numtracks-1, 0, 0, 0);
   end;

   // Play first and second track and 10 seconds of third track:
   if ( CD_INDRIVE(SDL_CDStatus(cdrom)) )
    SDL_CDPlayTracks(cdrom, 0, 0, 2, 10);

   This function returns 0, or -1 if there was an error. }

function SDL_CDPlayTracks(cdrom: PSDL_CD; start_track: Integer; start_frame:
  Integer; ntracks: Integer; nframes: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_CDPlayTracks'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CDPlayTracks}


{  Play the given CD starting at 'start' frame for 'length' frames.
   It returns 0, or -1 if there was an error. }

function SDL_CDPlay(cdrom: PSDL_CD; start: Integer; length: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_CDPlay'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CDPlay}

// Pause play -- returns 0, or -1 on error
function SDL_CDPause(cdrom: PSDL_CD): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_CDPause'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CDPause}

// Resume play -- returns 0, or -1 on error
function SDL_CDResume(cdrom: PSDL_CD): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_CDResume'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CDResume}

// Stop play -- returns 0, or -1 on error
function SDL_CDStop(cdrom: PSDL_CD): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_CDStop'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CDStop}

// Eject CD-ROM -- returns 0, or -1 on error
function SDL_CDEject(cdrom: PSDL_CD): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_CDEject'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CDEject}

// Closes the handle for the CD-ROM drive
procedure SDL_CDClose(cdrom: PSDL_CD);
cdecl; external {$IFDEF __GPC__}name 'SDL_CDClose'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CDClose}

// Given a status, returns true if there's a disk in the drive
function SDL_CDInDrive( status : TSDL_CDStatus ) : LongBool;
{$EXTERNALSYM SDL_CDInDrive}

// Conversion functions from frames to Minute/Second/Frames and vice versa
procedure FRAMES_TO_MSF(frames: Integer; var M: Integer; var S: Integer; var
  F: Integer);
{$EXTERNALSYM FRAMES_TO_MSF}
function MSF_TO_FRAMES(M: Integer; S: Integer; F: Integer): Integer;
{$EXTERNALSYM MSF_TO_FRAMES}

{------------------------------------------------------------------------------}
{ JoyStick-routines                                                            }
{------------------------------------------------------------------------------}

{ Count the number of joysticks attached to the system }
function SDL_NumJoysticks: Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_NumJoysticks'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_NumJoysticks}

{ Get the implementation dependent name of a joystick.
  This can be called before any joysticks are opened.
  If no name can be found, this function returns NULL. }
function SDL_JoystickName(index: Integer): PChar;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickName'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickName}

{ Open a joystick for use - the index passed as an argument refers to
  the N'th joystick on the system.  This index is the value which will
  identify this joystick in future joystick events.

  This function returns a joystick identifier, or NULL if an error occurred. }
function SDL_JoystickOpen(index: Integer): PSDL_Joystick;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickOpen'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickOpen}

{ Returns 1 if the joystick has been opened, or 0 if it has not. }
function SDL_JoystickOpened(index: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickOpened'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickOpened}

{ Get the device index of an opened joystick. }
function SDL_JoystickIndex(joystick: PSDL_Joystick): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickIndex'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickIndex}

{ Get the number of general axis controls on a joystick }
function SDL_JoystickNumAxes(joystick: PSDL_Joystick): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickNumAxes'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickNumAxes}

{ Get the number of trackballs on a joystick
  Joystick trackballs have only relative motion events associated
  with them and their state cannot be polled. }
function SDL_JoystickNumBalls(joystick: PSDL_Joystick): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickNumBalls'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickNumBalls}


{ Get the number of POV hats on a joystick }
function SDL_JoystickNumHats(joystick: PSDL_Joystick): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickNumHats'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickNumHats}

{ Get the number of buttons on a joystick }
function SDL_JoystickNumButtons(joystick: PSDL_Joystick): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickNumButtons'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickNumButtons}

{ Update the current state of the open joysticks.
  This is called automatically by the event loop if any joystick
  events are enabled. }

procedure SDL_JoystickUpdate;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickUpdate'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickUpdate;}

{ Enable/disable joystick event polling.
  If joystick events are disabled, you must call SDL_JoystickUpdate()
  yourself and check the state of the joystick when you want joystick
  information.
  The state can be one of SDL_QUERY, SDL_ENABLE or SDL_IGNORE. }

function SDL_JoystickEventState(state: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickEventState'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickEventState}

{ Get the current state of an axis control on a joystick
  The state is a value ranging from -32768 to 32767.
  The axis indices start at index 0. }

function SDL_JoystickGetAxis(joystick: PSDL_Joystick; axis: Integer) : SInt16;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickGetAxis'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickGetAxis}

{ The hat indices start at index 0. }

function SDL_JoystickGetHat(joystick: PSDL_Joystick; hat: Integer): UInt8;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickGetHat'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickGetHat}

{ Get the ball axis change since the last poll
  This returns 0, or -1 if you passed it invalid parameters.
  The ball indices start at index 0. }

function SDL_JoystickGetBall(joystick: PSDL_Joystick; ball: Integer; var dx: Integer; var dy: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickGetBall'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickGetBall}

{ Get the current state of a button on a joystick
  The button indices start at index 0. }
function SDL_JoystickGetButton( joystick: PSDL_Joystick; Button: Integer): UInt8;
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickGetButton'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickGetButton}

{ Close a joystick previously opened with SDL_JoystickOpen() }
procedure SDL_JoystickClose(joystick: PSDL_Joystick);
cdecl; external {$IFDEF __GPC__}name 'SDL_JoystickClose'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_JoystickClose}

{------------------------------------------------------------------------------}
{ event-handling }
{------------------------------------------------------------------------------}

{ Pumps the event loop, gathering events from the input devices.
  This function updates the event queue and internal input device state.
  This should only be run in the thread that sets the video mode. }

procedure SDL_PumpEvents;
cdecl; external {$IFDEF __GPC__}name 'SDL_PumpEvents'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_PumpEvents;}

{ Checks the event queue for messages and optionally returns them.
  If 'action' is SDL_ADDEVENT, up to 'numevents' events will be added to
  the back of the event queue.
  If 'action' is SDL_PEEKEVENT, up to 'numevents' events at the front
  of the event queue, matching 'mask', will be returned and will not
  be removed from the queue.
  If 'action' is SDL_GETEVENT, up to 'numevents' events at the front
  of the event queue, matching 'mask', will be returned and will be
  removed from the queue.
  This function returns the number of events actually stored, or -1
  if there was an error.  This function is thread-safe. }

function SDL_PeepEvents(events: PSDL_Event; numevents: Integer; action: TSDL_eventaction; mask: UInt32): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_PeepEvents'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_PeepEvents}

{ Polls for currently pending events, and returns 1 if there are any pending
   events, or 0 if there are none available.  If 'event' is not NULL, the next
   event is removed from the queue and stored in that area. }

function SDL_PollEvent(event: PSDL_Event): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_PollEvent'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_PollEvent}

{  Waits indefinitely for the next available event, returning 1, or 0 if there
   was an error while waiting for events.  If 'event' is not NULL, the next
   event is removed from the queue and stored in that area. }

function SDL_WaitEvent(event: PSDL_Event): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_WaitEvent'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_WaitEvent}

function SDL_PushEvent( event : PSDL_Event ) : Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_PushEvent'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_PushEvent}

{ If the filter returns 1, then the event will be added to the internal queue.
  If it returns 0, then the event will be dropped from the queue, but the
  internal state will still be updated.  This allows selective filtering of
  dynamically arriving events.

  WARNING:  Be very careful of what you do in the event filter function, as
            it may run in a different thread!

  There is one caveat when dealing with the SDL_QUITEVENT event type.  The
  event filter is only called when the window manager desires to close the
  application window.  If the event filter returns 1, then the window will
  be closed, otherwise the window will remain open if possible.
  If the quit event is generated by an interrupt signal, it will bypass the
  internal queue and be delivered to the application at the next event poll. }
procedure SDL_SetEventFilter( filter : TSDL_EventFilter );
cdecl; external {$IFDEF __GPC__}name 'SDL_SetEventFilter'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SetEventFilter}

{ Return the current event filter - can be used to "chain" filters.
  If there is no event filter set, this function returns NULL. }

function SDL_GetEventFilter: TSDL_EventFilter;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetEventFilter'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetEventFilter}

{ This function allows you to set the state of processing certain events.
  If 'state' is set to SDL_IGNORE, that event will be automatically dropped
  from the event queue and will not event be filtered.
  If 'state' is set to SDL_ENABLE, that event will be processed normally.
  If 'state' is set to SDL_QUERY, SDL_EventState() will return the
  current processing state of the specified event. }

function SDL_EventState(type_: UInt8; state: Integer): UInt8;
cdecl; external {$IFDEF __GPC__}name 'SDL_EventState'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_EventState}

{------------------------------------------------------------------------------}
{ Version Routines }
{------------------------------------------------------------------------------}

{ This macro can be used to fill a version structure with the compile-time
  version of the SDL library. }
procedure SDL_VERSION(var X: TSDL_Version);
{$EXTERNALSYM SDL_VERSION}

{ This macro turns the version numbers into a numeric value:
   (1,2,3) -> (1203)
   This assumes that there will never be more than 100 patchlevels }

function SDL_VERSIONNUM(X, Y, Z: Integer): Integer;
{$EXTERNALSYM SDL_VERSIONNUM}

// This is the version number macro for the current SDL version
function SDL_COMPILEDVERSION: Integer;
{$EXTERNALSYM SDL_COMPILEDVERSION}

// This macro will evaluate to true if compiled with SDL at least X.Y.Z
function SDL_VERSION_ATLEAST(X: Integer; Y: Integer; Z: Integer) : LongBool;
{$EXTERNALSYM SDL_VERSION_ATLEAST}

{ This function gets the version of the dynamically linked SDL library.
  it should NOT be used to fill a version structure, instead you should
  use the SDL_Version() macro. }

function SDL_Linked_Version: PSDL_version;
cdecl; external {$IFDEF __GPC__}name 'SDL_Linked_Version'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_Linked_Version}

{------------------------------------------------------------------------------}
{ video                                                                        }
{------------------------------------------------------------------------------}

{ These functions are used internally, and should not be used unless you
  have a specific need to specify the video driver you want to use.
  You should normally use SDL_Init() or SDL_InitSubSystem().

  SDL_VideoInit() initializes the video subsystem -- sets up a connection
  to the window manager, etc, and determines the current video mode and
  pixel format, but does not initialize a window or graphics mode.
  Note that event handling is activated by this routine.

  If you use both sound and video in your application, you need to call
  SDL_Init() before opening the sound device, otherwise under Win32 DirectX,
  you won't be able to set full-screen display modes. }

function SDL_VideoInit(driver_name: PChar; flags: UInt32): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_VideoInit'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_VideoInit}
procedure SDL_VideoQuit;
cdecl; external {$IFDEF __GPC__}name 'SDL_VideoQuit'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_VideoQuit}

{ This function fills the given character buffer with the name of the
  video driver, and returns a pointer to it if the video driver has
  been initialized.  It returns NULL if no driver has been initialized. }

function SDL_VideoDriverName(namebuf: PChar; maxlen: Integer): PChar;
cdecl; external {$IFDEF __GPC__}name 'SDL_VideoDriverName'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_VideoDriverName}

{ This function returns a pointer to the current display surface.
  If SDL is doing format conversion on the display surface, this
  function returns the publicly visible surface, not the real video
  surface. }

function SDL_GetVideoSurface: PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetVideoSurface'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetVideoSurface}

{ This function returns a read-only pointer to information about the
  video hardware.  If this is called before SDL_SetVideoMode(), the 'vfmt'
  member of the returned structure will contain the pixel format of the
  "best" video mode. }
function SDL_GetVideoInfo: PSDL_VideoInfo;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetVideoInfo'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetVideoInfo}

{ Check to see if a particular video mode is supported.
  It returns 0 if the requested mode is not supported under any bit depth,
  or returns the bits-per-pixel of the closest available mode with the
  given width and height.  If this bits-per-pixel is different from the
  one used when setting the video mode, SDL_SetVideoMode() will succeed,
  but will emulate the requested bits-per-pixel with a shadow surface.

  The arguments to SDL_VideoModeOK() are the same ones you would pass to
  SDL_SetVideoMode() }

function SDL_VideoModeOK(width, height, bpp: Integer; flags: UInt32): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_VideoModeOK'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_VideoModeOK}

{ Return a pointer to an array of available screen dimensions for the
  given format and video flags, sorted largest to smallest.  Returns
  NULL if there are no dimensions available for a particular format,
  or (SDL_Rect **)-1 if any dimension is okay for the given format.

  if 'format' is NULL, the mode list will be for the format given
  by SDL_GetVideoInfo( ) - > vfmt }

function SDL_ListModes(format: PSDL_PixelFormat; flags: UInt32): PPSDL_Rect;
cdecl; external {$IFDEF __GPC__}name 'SDL_ListModes'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_ListModes}


{ Set up a video mode with the specified width, height and bits-per-pixel.

  If 'bpp' is 0, it is treated as the current display bits per pixel.

  If SDL_ANYFORMAT is set in 'flags', the SDL library will try to set the
  requested bits-per-pixel, but will return whatever video pixel format is
  available.  The default is to emulate the requested pixel format if it
  is not natively available.

  If SDL_HWSURFACE is set in 'flags', the video surface will be placed in
  video memory, if possible, and you may have to call SDL_LockSurface()
  in order to access the raw framebuffer.  Otherwise, the video surface
  will be created in system memory.

  If SDL_ASYNCBLIT is set in 'flags', SDL will try to perform rectangle
  updates asynchronously, but you must always lock before accessing pixels.
  SDL will wait for updates to complete before returning from the lock.

  If SDL_HWPALETTE is set in 'flags', the SDL library will guarantee
  that the colors set by SDL_SetColors() will be the colors you get.
  Otherwise, in 8-bit mode, SDL_SetColors() may not be able to set all
  of the colors exactly the way they are requested, and you should look
  at the video surface structure to determine the actual palette.
  If SDL cannot guarantee that the colors you request can be set,
  i.e. if the colormap is shared, then the video surface may be created
  under emulation in system memory, overriding the SDL_HWSURFACE flag.

  If SDL_FULLSCREEN is set in 'flags', the SDL library will try to set
  a fullscreen video mode.  The default is to create a windowed mode
  if the current graphics system has a window manager.
  If the SDL library is able to set a fullscreen video mode, this flag
  will be set in the surface that is returned.

  If SDL_DOUBLEBUF is set in 'flags', the SDL library will try to set up
  two surfaces in video memory and swap between them when you call
  SDL_Flip().  This is usually slower than the normal single-buffering
  scheme, but prevents "tearing" artifacts caused by modifying video
  memory while the monitor is refreshing.  It should only be used by
  applications that redraw the entire screen on every update.

  This function returns the video framebuffer surface, or NULL if it fails. }

function SDL_SetVideoMode(width, height, bpp: Integer; flags: UInt32): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'SDL_SetVideoMode'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SetVideoMode}


{ Makes sure the given list of rectangles is updated on the given screen.
  If 'x', 'y', 'w' and 'h' are all 0, SDL_UpdateRect will update the entire
  screen.
  These functions should not be called while 'screen' is locked. }

procedure SDL_UpdateRects(screen: PSDL_Surface; numrects: Integer; rects: PSDL_Rect);
cdecl; external {$IFDEF __GPC__}name 'SDL_UpdateRects'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_UpdateRects}
procedure SDL_UpdateRect(screen: PSDL_Surface; x, y: SInt32; w, h: UInt32);
cdecl; external {$IFDEF __GPC__}name 'SDL_UpdateRect'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_UpdateRect}


{ On hardware that supports double-buffering, this function sets up a flip
  and returns.  The hardware will wait for vertical retrace, and then swap
  video buffers before the next video surface blit or lock will return.
  On hardware that doesn not support double-buffering, this is equivalent
  to calling SDL_UpdateRect(screen, 0, 0, 0, 0);
  The SDL_DOUBLEBUF flag must have been passed to SDL_SetVideoMode() when
  setting the video mode for this function to perform hardware flipping.
  This function returns 0 if successful, or -1 if there was an error.}

function SDL_Flip(screen: PSDL_Surface): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_Flip'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_Flip}

{ Set the gamma correction for each of the color channels.
  The gamma values range (approximately) between 0.1 and 10.0

  If this function isn't supported directly by the hardware, it will
  be emulated using gamma ramps, if available.  If successful, this
  function returns 0, otherwise it returns -1. }

function SDL_SetGamma(redgamma: single; greengamma: single; bluegamma: single ): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_SetGamma'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SetGamma}

{ Set the gamma translation table for the red, green, and blue channels
  of the video hardware.  Each table is an array of 256 16-bit quantities,
  representing a mapping between the input and output for that channel.
  The input is the index into the array, and the output is the 16-bit
  gamma value at that index, scaled to the output color precision.

  You may pass NULL for any of the channels to leave it unchanged.
  If the call succeeds, it will return 0.  If the display driver or
  hardware does not support gamma translation, or otherwise fails,
  this function will return -1. }

function SDL_SetGammaRamp( redtable: PUInt16; greentable: PUInt16; bluetable: PUInt16): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_SetGammaRamp'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SetGammaRamp}

{ Retrieve the current values of the gamma translation tables.

  You must pass in valid pointers to arrays of 256 16-bit quantities.
  Any of the pointers may be NULL to ignore that channel.
  If the call succeeds, it will return 0.  If the display driver or
  hardware does not support gamma translation, or otherwise fails,
  this function will return -1. }

function SDL_GetGammaRamp( redtable: PUInt16; greentable: PUInt16; bluetable: PUInt16): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetGammaRamp'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetGammaRamp}

{ Sets a portion of the colormap for the given 8-bit surface.  If 'surface'
  is not a palettized surface, this function does nothing, returning 0.
  If all of the colors were set as passed to SDL_SetColors(), it will
  return 1.  If not all the color entries were set exactly as given,
  it will return 0, and you should look at the surface palette to
  determine the actual color palette.

  When 'surface' is the surface associated with the current display, the
  display colormap will be updated with the requested colors.  If
  SDL_HWPALETTE was set in SDL_SetVideoMode() flags, SDL_SetColors()
  will always return 1, and the palette is guaranteed to be set the way
  you desire, even if the window colormap has to be warped or run under
  emulation. }


function SDL_SetColors(surface: PSDL_Surface; colors: PSDL_Color; firstcolor : Integer; ncolors: Integer) : Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_SetColors'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SetColors}

{ Sets a portion of the colormap for a given 8-bit surface.
  'flags' is one or both of:
  SDL_LOGPAL  -- set logical palette, which controls how blits are mapped
                 to/from the surface,
  SDL_PHYSPAL -- set physical palette, which controls how pixels look on
                 the screen
  Only screens have physical palettes. Separate change of physical/logical
  palettes is only possible if the screen has SDL_HWPALETTE set.

  The return value is 1 if all colours could be set as requested, and 0
  otherwise.

  SDL_SetColors() is equivalent to calling this function with
  flags = (SDL_LOGPAL or SDL_PHYSPAL). }

function SDL_SetPalette(surface: PSDL_Surface; flags: Integer; colors: PSDL_Color; firstcolor: Integer; ncolors: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_SetPalette'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SetPalette}

{ Maps an RGB triple to an opaque pixel value for a given pixel format }
function SDL_MapRGB(format: PSDL_PixelFormat; r: UInt8; g: UInt8; b: UInt8) : UInt32;
cdecl; external {$IFDEF __GPC__}name 'SDL_MapRGB'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_MapRGB}

{ Maps an RGBA quadruple to a pixel value for a given pixel format }
function SDL_MapRGBA(format: PSDL_PixelFormat; r: UInt8; g: UInt8; b: UInt8; a: UInt8): UInt32;
cdecl; external {$IFDEF __GPC__}name 'SDL_MapRGBA'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_MapRGBA}

{ Maps a pixel value into the RGB components for a given pixel format }
procedure SDL_GetRGB(pixel: UInt32; fmt: PSDL_PixelFormat; r: PUInt8; g: PUInt8; b: PUInt8);
cdecl; external {$IFDEF __GPC__}name 'SDL_GetRGB'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetRGB}

{ Maps a pixel value into the RGBA components for a given pixel format }
procedure SDL_GetRGBA(pixel: UInt32; fmt: PSDL_PixelFormat; r: PUInt8; g: PUInt8; b: PUInt8; a: PUInt8);
cdecl; external {$IFDEF __GPC__}name 'SDL_GetRGBA'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetRGBA}

{ Allocate and free an RGB surface (must be called after SDL_SetVideoMode)
  If the depth is 4 or 8 bits, an empty palette is allocated for the surface.
  If the depth is greater than 8 bits, the pixel format is set using the
  flags '[RGB]mask'.
  If the function runs out of memory, it will return NULL.

  The 'flags' tell what kind of surface to create.
  SDL_SWSURFACE means that the surface should be created in system memory.
  SDL_HWSURFACE means that the surface should be created in video memory,
  with the same format as the display surface.  This is useful for surfaces
  that will not change much, to take advantage of hardware acceleration
  when being blitted to the display surface.
  SDL_ASYNCBLIT means that SDL will try to perform asynchronous blits with
  this surface, but you must always lock it before accessing the pixels.
  SDL will wait for current blits to finish before returning from the lock.
  SDL_SRCCOLORKEY indicates that the surface will be used for colorkey blits.
  If the hardware supports acceleration of colorkey blits between
  two surfaces in video memory, SDL will try to place the surface in
  video memory. If this isn't possible or if there is no hardware
  acceleration available, the surface will be placed in system memory.
  SDL_SRCALPHA means that the surface will be used for alpha blits and
  if the hardware supports hardware acceleration of alpha blits between
  two surfaces in video memory, to place the surface in video memory
  if possible, otherwise it will be placed in system memory.
  If the surface is created in video memory, blits will be _much_ faster,
  but the surface format must be identical to the video surface format,
  and the only way to access the pixels member of the surface is to use
  the SDL_LockSurface() and SDL_UnlockSurface() calls.
  If the requested surface actually resides in video memory, SDL_HWSURFACE
  will be set in the flags member of the returned surface.  If for some
  reason the surface could not be placed in video memory, it will not have
  the SDL_HWSURFACE flag set, and will be created in system memory instead. }

function SDL_AllocSurface(flags: UInt32; width, height, depth: Integer;
  RMask, GMask, BMask, AMask: UInt32): PSDL_Surface;
{$EXTERNALSYM SDL_AllocSurface}

function SDL_CreateRGBSurface(flags: UInt32; width, height, depth: Integer; RMask, GMask, BMask, AMask: UInt32): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'SDL_CreateRGBSurface'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CreateRGBSurface}

function SDL_CreateRGBSurfaceFrom(pixels: Pointer; width, height, depth, pitch
  : Integer; RMask, GMask, BMask, AMask: UInt32): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'SDL_CreateRGBSurfaceFrom'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CreateRGBSurfaceFrom}

procedure SDL_FreeSurface(surface: PSDL_Surface);
cdecl; external {$IFDEF __GPC__}name 'SDL_FreeSurface'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_FreeSurface}

function SDL_MustLock(Surface: PSDL_Surface): Boolean;
{$EXTERNALSYM SDL_MustLock}
{ SDL_LockSurface() sets up a surface for directly accessing the pixels.
  Between calls to SDL_LockSurface()/SDL_UnlockSurface(), you can write
  to and read from 'surface->pixels', using the pixel format stored in
  'surface->format'.  Once you are done accessing the surface, you should
  use SDL_UnlockSurface() to release it.

  Not all surfaces require locking.  If SDL_MUSTLOCK(surface) evaluates
  to 0, then you can read and write to the surface at any time, and the
  pixel format of the surface will not change.  In particular, if the
  SDL_HWSURFACE flag is not given when calling SDL_SetVideoMode(), you
  will not need to lock the display surface before accessing it.

  No operating system or library calls should be made between lock/unlock
  pairs, as critical system locks may be held during this time.

  SDL_LockSurface() returns 0, or -1 if the surface couldn't be locked. }
function SDL_LockSurface(surface: PSDL_Surface): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_LockSurface'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_LockSurface}

procedure SDL_UnlockSurface(surface: PSDL_Surface);
cdecl; external {$IFDEF __GPC__}name 'SDL_UnlockSurface'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_UnlockSurface}

{ Load a surface from a seekable SDL data source (memory or file.)
  If 'freesrc' is non-zero, the source will be closed after being read.
  Returns the new surface, or NULL if there was an error.
  The new surface should be freed with SDL_FreeSurface(). }
function SDL_LoadBMP_RW(src: PSDL_RWops; freesrc: Integer): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'SDL_LoadBMP_RW'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_LoadBMP_RW}

// Convenience macro -- load a surface from a file
function SDL_LoadBMP(filename: PChar): PSDL_Surface;
{$EXTERNALSYM SDL_LoadBMP}

{ Save a surface to a seekable SDL data source (memory or file.)
  If 'freedst' is non-zero, the source will be closed after being written.
  Returns 0 if successful or -1 if there was an error. }

function SDL_SaveBMP_RW(surface: PSDL_Surface; dst: PSDL_RWops; freedst: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_SaveBMP_RW'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SaveBMP_RW}

// Convenience macro -- save a surface to a file
function SDL_SaveBMP(surface: PSDL_Surface; filename: PChar): Integer;
{$EXTERNALSYM SDL_SaveBMP}

{ Sets the color key (transparent pixel) in a blittable surface.
  If 'flag' is SDL_SRCCOLORKEY (optionally OR'd with SDL_RLEACCEL),
  'key' will be the transparent pixel in the source image of a blit.
  SDL_RLEACCEL requests RLE acceleration for the surface if present,
  and removes RLE acceleration if absent.
  If 'flag' is 0, this function clears any current color key.
  This function returns 0, or -1 if there was an error. }

function SDL_SetColorKey(surface: PSDL_Surface; flag, key: UInt32) : Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_SetColorKey'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SetColorKey}

{ This function sets the alpha value for the entire surface, as opposed to
  using the alpha component of each pixel. This value measures the range
  of transparency of the surface, 0 being completely transparent to 255
  being completely opaque. An 'alpha' value of 255 causes blits to be
  opaque, the source pixels copied to the destination (the default). Note
  that per-surface alpha can be combined with colorkey transparency.

  If 'flag' is 0, alpha blending is disabled for the surface.
  If 'flag' is SDL_SRCALPHA, alpha blending is enabled for the surface.
  OR:ing the flag with SDL_RLEACCEL requests RLE acceleration for the
  surface; if SDL_RLEACCEL is not specified, the RLE accel will be removed. }


function SDL_SetAlpha(surface: PSDL_Surface; flag: UInt32; alpha: UInt8): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_SetAlpha'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SetAlpha}

{ Sets the clipping rectangle for the destination surface in a blit.

  If the clip rectangle is NULL, clipping will be disabled.
  If the clip rectangle doesn't intersect the surface, the function will
  return SDL_FALSE and blits will be completely clipped.  Otherwise the
  function returns SDL_TRUE and blits to the surface will be clipped to
  the intersection of the surface area and the clipping rectangle.

  Note that blits are automatically clipped to the edges of the source
  and destination surfaces. }
procedure SDL_SetClipRect(surface: PSDL_Surface; rect: PSDL_Rect); cdecl;
external {$IFDEF __GPC__}name 'SDL_SetClipRect'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SetClipRect}

{ Gets the clipping rectangle for the destination surface in a blit.
  'rect' must be a pointer to a valid rectangle which will be filled
  with the correct values. }
procedure SDL_GetClipRect(surface: PSDL_Surface; rect: PSDL_Rect); cdecl;
external {$IFDEF __GPC__}name 'SDL_GetClipRect'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetClipRect}

{ Creates a new surface of the specified format, and then copies and maps
  the given surface to it so the blit of the converted surface will be as
  fast as possible.  If this function fails, it returns NULL.

  The 'flags' parameter is passed to SDL_CreateRGBSurface() and has those
  semantics.  You can also pass SDL_RLEACCEL in the flags parameter and
  SDL will try to RLE accelerate colorkey and alpha blits in the resulting
  surface.

  This function is used internally by SDL_DisplayFormat(). }

function SDL_ConvertSurface(src: PSDL_Surface; fmt: PSDL_PixelFormat; flags: UInt32): PSDL_Surface;
cdecl; external {$IFDEF __GPC__}name 'SDL_ConvertSurface'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_ConvertSurface}

{
  This performs a fast blit from the source surface to the destination
  surface.  It assumes that the source and destination rectangles are
  the same size.  If either 'srcrect' or 'dstrect' are NULL, the entire
  surface (src or dst) is copied.  The final blit rectangles are saved
  in 'srcrect' and 'dstrect' after all clipping is performed.
  If the blit is successful, it returns 0, otherwise it returns -1.
 
  The blit function should not be called on a locked surface.
 
  The blit semantics for surfaces with and without alpha and colorkey
  are defined as follows:
 
  RGBA->RGB:
      SDL_SRCALPHA set:
   alpha-blend (using alpha-channel).
   SDL_SRCCOLORKEY ignored.
      SDL_SRCALPHA not set:
   copy RGB.
   if SDL_SRCCOLORKEY set, only copy the pixels matching the
   RGB values of the source colour key, ignoring alpha in the
   comparison.
 
  RGB->RGBA:
      SDL_SRCALPHA set:
   alpha-blend (using the source per-surface alpha value);
   set destination alpha to opaque.
      SDL_SRCALPHA not set:
   copy RGB, set destination alpha to opaque.
      both:
   if SDL_SRCCOLORKEY set, only copy the pixels matching the
   source colour key.
 
  RGBA->RGBA:
      SDL_SRCALPHA set:
   alpha-blend (using the source alpha channel) the RGB values;
   leave destination alpha untouched. [Note: is this correct?]
   SDL_SRCCOLORKEY ignored.
      SDL_SRCALPHA not set:
   copy all of RGBA to the destination.
   if SDL_SRCCOLORKEY set, only copy the pixels matching the
   RGB values of the source colour key, ignoring alpha in the
   comparison.
 
  RGB->RGB:
      SDL_SRCALPHA set:
   alpha-blend (using the source per-surface alpha value).
      SDL_SRCALPHA not set:
   copy RGB.
      both:
   if SDL_SRCCOLORKEY set, only copy the pixels matching the
   source colour key.
 
  If either of the surfaces were in video memory, and the blit returns -2,
  the video memory was lost, so it should be reloaded with artwork and
  re-blitted:
  while ( SDL_BlitSurface(image, imgrect, screen, dstrect) = -2 ) do
  begin
  while ( SDL_LockSurface(image) < 0 ) do
   Sleep(10);
  -- Write image pixels to image->pixels --
  SDL_UnlockSurface(image);
 end;

  This happens under DirectX 5.0 when the system switches away from your
  fullscreen application.  The lock will also fail until you have access
  to the video memory again. }

{ You should call SDL_BlitSurface() unless you know exactly how SDL
   blitting works internally and how to use the other blit functions. }

function SDL_BlitSurface(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): Integer;
{$EXTERNALSYM SDL_BlitSurface}

{  This is the public blit function, SDL_BlitSurface(), and it performs
   rectangle validation and clipping before passing it to SDL_LowerBlit() }
function SDL_UpperBlit(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_UpperBlit'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_UpperBlit}

{ This is a semi-private blit function and it performs low-level surface
  blitting only. }
function SDL_LowerBlit(src: PSDL_Surface; srcrect: PSDL_Rect; dst: PSDL_Surface; dstrect: PSDL_Rect): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_LowerBlit'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_LowerBlit}

{ This function performs a fast fill of the given rectangle with 'color'
  The given rectangle is clipped to the destination surface clip area
  and the final fill rectangle is saved in the passed in pointer.
  If 'dstrect' is NULL, the whole surface will be filled with 'color'
  The color should be a pixel of the format used by the surface, and
  can be generated by the SDL_MapRGB() function.
  This function returns 0 on success, or -1 on error. }

function SDL_FillRect(dst: PSDL_Surface; dstrect: PSDL_Rect; color: UInt32) : Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_FillRect'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_FillRect}

{ This function takes a surface and copies it to a new surface of the
  pixel format and colors of the video framebuffer, suitable for fast
  blitting onto the display surface.  It calls SDL_ConvertSurface()

  If you want to take advantage of hardware colorkey or alpha blit
  acceleration, you should set the colorkey and alpha value before
  calling this function.

  If the conversion fails or runs out of memory, it returns NULL }

function SDL_DisplayFormat(surface: PSDL_Surface): PSDL_Surface; cdecl;
external {$IFDEF __GPC__}name 'SDL_DisplayFormat'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_DisplayFormat}

{ This function takes a surface and copies it to a new surface of the
  pixel format and colors of the video framebuffer (if possible),
  suitable for fast alpha blitting onto the display surface.
  The new surface will always have an alpha channel.

  If you want to take advantage of hardware colorkey or alpha blit
  acceleration, you should set the colorkey and alpha value before
  calling this function.

  If the conversion fails or runs out of memory, it returns NULL }


function SDL_DisplayFormatAlpha(surface: PSDL_Surface): PSDL_Surface; cdecl;
external {$IFDEF __GPC__}name 'SDL_DisplayFormatAlpha'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_DisplayFormatAlpha}

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
//* YUV video surface overlay functions                                       */
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

{ This function creates a video output overlay
  Calling the returned surface an overlay is something of a misnomer because
  the contents of the display surface underneath the area where the overlay
  is shown is undefined - it may be overwritten with the converted YUV data. }

function SDL_CreateYUVOverlay(width: Integer; height: Integer; format: UInt32; display: PSDL_Surface): PSDL_Overlay;
cdecl; external {$IFDEF __GPC__}name 'SDL_CreateYUVOverlay'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CreateYUVOverlay}

// Lock an overlay for direct access, and unlock it when you are done
function SDL_LockYUVOverlay(Overlay: PSDL_Overlay): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_LockYUVOverlay'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_LockYUVOverlay}

procedure SDL_UnlockYUVOverlay(Overlay: PSDL_Overlay); cdecl;
external {$IFDEF __GPC__}name 'SDL_UnlockYUVOverlay'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_UnlockYUVOverlay}


{ Blit a video overlay to the display surface.
  The contents of the video surface underneath the blit destination are
  not defined.
  The width and height of the destination rectangle may be different from
  that of the overlay, but currently only 2x scaling is supported. }

function SDL_DisplayYUVOverlay(Overlay: PSDL_Overlay; dstrect: PSDL_Rect) : Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_DisplayYUVOverlay'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_DisplayYUVOverlay}

// Free a video overlay
procedure SDL_FreeYUVOverlay(Overlay: PSDL_Overlay);
cdecl; external {$IFDEF __GPC__}name 'SDL_FreeYUVOverlay'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_FreeYUVOverlay}

{------------------------------------------------------------------------------}
{ OpenGL Routines                                                              }
{------------------------------------------------------------------------------}

{ Dynamically load a GL driver, if SDL is built with dynamic GL.

  SDL links normally with the OpenGL library on your system by default,
  but you can compile it to dynamically load the GL driver at runtime.
  If you do this, you need to retrieve all of the GL functions used in
  your program from the dynamic library using SDL_GL_GetProcAddress().

  This is disabled in default builds of SDL. }


function SDL_GL_LoadLibrary(filename: PChar): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_GL_LoadLibrary'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GL_LoadLibrary}

{ Get the address of a GL function (for extension functions) }
function SDL_GL_GetProcAddress(procname: PChar) : Pointer;
cdecl; external {$IFDEF __GPC__}name 'SDL_GL_GetProcAddress'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GL_GetProcAddress}

{ Set an attribute of the OpenGL subsystem before intialization. }
function SDL_GL_SetAttribute(attr: TSDL_GLAttr; value: Integer) : Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_GL_SetAttribute'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GL_SetAttribute}

{ Get an attribute of the OpenGL subsystem from the windowing
  interface, such as glX. This is of course different from getting
  the values from SDL's internal OpenGL subsystem, which only
  stores the values you request before initialization.

  Developers should track the values they pass into SDL_GL_SetAttribute
  themselves if they want to retrieve these values. }

function SDL_GL_GetAttribute(attr: TSDL_GLAttr; var value: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_GL_GetAttribute'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GL_GetAttribute}

{ Swap the OpenGL buffers, if double-buffering is supported. }

procedure SDL_GL_SwapBuffers;
cdecl; external {$IFDEF __GPC__}name 'SDL_GL_SwapBuffers'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GL_SwapBuffers;}

{ Internal functions that should not be called unless you have read
  and understood the source code for these functions. }

procedure SDL_GL_UpdateRects(numrects: Integer; rects: PSDL_Rect);
cdecl; external {$IFDEF __GPC__}name 'SDL_GL_UpdateRects'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GL_UpdateRects}
procedure SDL_GL_Lock;
cdecl; external {$IFDEF __GPC__}name 'SDL_GL_Lock'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GL_Lock;}
procedure SDL_GL_Unlock;
cdecl; external {$IFDEF __GPC__}name 'SDL_GL_Unlock'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GL_Unlock;}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* These functions allow interaction with the window manager, if any.        *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ Sets/Gets the title and icon text of the display window }
procedure SDL_WM_GetCaption(var title : PChar; var icon : PChar);
cdecl; external {$IFDEF __GPC__}name 'SDL_WM_GetCaption'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_WM_GetCaption}
procedure SDL_WM_SetCaption( const title : PChar; const icon : PChar);
cdecl; external {$IFDEF __GPC__}name 'SDL_WM_SetCaption'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_WM_SetCaption}

{ Sets the icon for the display window.
  This function must be called before the first call to SDL_SetVideoMode().
  It takes an icon surface, and a mask in MSB format.
  If 'mask' is NULL, the entire icon surface will be used as the icon. }
procedure SDL_WM_SetIcon(icon: PSDL_Surface; mask: UInt8);
cdecl; external {$IFDEF __GPC__}name 'SDL_WM_SetIcon'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_WM_SetIcon}

{ This function iconifies the window, and returns 1 if it succeeded.
  If the function succeeds, it generates an SDL_APPACTIVE loss event.
  This function is a noop and returns 0 in non-windowed environments. }

function SDL_WM_IconifyWindow: Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_WM_IconifyWindow'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_WM_IconifyWindow}

{ Toggle fullscreen mode without changing the contents of the screen.
  If the display surface does not require locking before accessing
  the pixel information, then the memory pointers will not change.

  If this function was able to toggle fullscreen mode (change from
  running in a window to fullscreen, or vice-versa), it will return 1.
  If it is not implemented, or fails, it returns 0.

  The next call to SDL_SetVideoMode() will set the mode fullscreen
  attribute based on the flags parameter - if SDL_FULLSCREEN is not
  set, then the display will be windowed by default where supported.

  This is currently only implemented in the X11 video driver. }

function SDL_WM_ToggleFullScreen(surface: PSDL_Surface): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_WM_ToggleFullScreen'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_WM_ToggleFullScreen}

{ Grabbing means that the mouse is confined to the application window,
  and nearly all keyboard input is passed directly to the application,
  and not interpreted by a window manager, if any. }

function SDL_WM_GrabInput(mode: TSDL_GrabMode): TSDL_GrabMode;
cdecl; external {$IFDEF __GPC__}name 'SDL_WM_GrabInput'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_WM_GrabInput}

{------------------------------------------------------------------------------}
{ mouse-routines }
{------------------------------------------------------------------------------}

{ Retrieve the current state of the mouse.
  The current button state is returned as a button bitmask, which can
  be tested using the SDL_BUTTON(X) macros, and x and y are set to the
  current mouse cursor position.  You can pass NULL for either x or y. }

function SDL_GetMouseState(var x: Integer; var y: Integer): UInt8;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetMouseState'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetMouseState}

{ Retrieve the current state of the mouse.
  The current button state is returned as a button bitmask, which can
  be tested using the SDL_BUTTON(X) macros, and x and y are set to the
  mouse deltas since the last call to SDL_GetRelativeMouseState(). }
function SDL_GetRelativeMouseState(var x: Integer; var y: Integer): UInt8;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetRelativeMouseState'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetRelativeMouseState}

{ Set the position of the mouse cursor (generates a mouse motion event) }
procedure SDL_WarpMouse(x, y: UInt16);
cdecl; external {$IFDEF __GPC__}name 'SDL_WarpMouse'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_WarpMouse}

{ Create a cursor using the specified data and mask (in MSB format).
  The cursor width must be a multiple of 8 bits.

  The cursor is created in black and white according to the following:
  data  mask    resulting pixel on screen
   0     1       White
   1     1       Black
   0     0       Transparent
   1     0       Inverted color if possible, black if not.

  Cursors created with this function must be freed with SDL_FreeCursor(). }
function SDL_CreateCursor(data, mask: PUInt8; w, h, hot_x, hot_y: Integer): PSDL_Cursor;
cdecl; external {$IFDEF __GPC__}name 'SDL_CreateCursor'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CreateCursor}

{ Set the currently active cursor to the specified one.
  If the cursor is currently visible, the change will be immediately
  represented on the display. }
procedure SDL_SetCursor(cursor: PSDL_Cursor);
cdecl; external {$IFDEF __GPC__}name 'SDL_SetCursor'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SetCursor}

{ Returns the currently active cursor. }
function SDL_GetCursor: PSDL_Cursor;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetCursor'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetCursor}

{ Deallocates a cursor created with SDL_CreateCursor(). }
procedure SDL_FreeCursor(cursor: PSDL_Cursor);
cdecl; external {$IFDEF __GPC__}name 'SDL_FreeCursor'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_FreeCursor}

{ Toggle whether or not the cursor is shown on the screen.
  The cursor start off displayed, but can be turned off.
  SDL_ShowCursor() returns 1 if the cursor was being displayed
  before the call, or 0 if it was not.  You can query the current
  state by passing a 'toggle' value of -1. }
function SDL_ShowCursor(toggle: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_ShowCursor'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_ShowCursor}

function SDL_BUTTON( Button : Integer ) : Integer;

{------------------------------------------------------------------------------}
{ Keyboard-routines                                                            }
{------------------------------------------------------------------------------}

{ Enable/Disable UNICODE translation of keyboard input.
  This translation has some overhead, so translation defaults off.
  If 'enable' is 1, translation is enabled.
  If 'enable' is 0, translation is disabled.
  If 'enable' is -1, the translation state is not changed.
  It returns the previous state of keyboard translation. }
function SDL_EnableUNICODE(enable: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_EnableUNICODE'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_EnableUNICODE}

{ If 'delay' is set to 0, keyboard repeat is disabled. }
function SDL_EnableKeyRepeat(delay: Integer; interval: Integer): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_EnableKeyRepeat'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_EnableKeyRepeat}

{ Get a snapshot of the current state of the keyboard.
  Returns an array of keystates, indexed by the SDLK_* syms.
  Used:

  UInt8 *keystate = SDL_GetKeyState(NULL);
  if ( keystate[SDLK_RETURN] ) ... <RETURN> is pressed }

function SDL_GetKeyState(numkeys: PInt): PUInt8;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetKeyState'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetKeyState}

{ Get the current key modifier state }
function SDL_GetModState: TSDLMod;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetModState'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetModState}

{ Set the current key modifier state
  This does not change the keyboard state, only the key modifier flags. }
procedure SDL_SetModState(modstate: TSDLMod);
cdecl; external {$IFDEF __GPC__}name 'SDL_SetModState'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SetModState}

{ Get the name of an SDL virtual keysym }
function SDL_GetKeyName(key: TSDLKey): PChar;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetKeyName'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetKeyName}

{------------------------------------------------------------------------------}
{ Active Routines                                                              }
{------------------------------------------------------------------------------}

{ This function returns the current state of the application, which is a
  bitwise combination of SDL_APPMOUSEFOCUS, SDL_APPINPUTFOCUS, and
  SDL_APPACTIVE.  If SDL_APPACTIVE is set, then the user is able to
  see your application, otherwise it has been iconified or disabled. }

function SDL_GetAppState: UInt8;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetAppState'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetAppState}


{ Mutex functions }

{ Create a mutex, initialized unlocked }

function SDL_CreateMutex: PSDL_Mutex;
cdecl; external {$IFDEF __GPC__}name 'SDL_CreateMutex'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CreateMutex}

{ Lock the mutex  (Returns 0, or -1 on error) }

 function SDL_mutexP(mutex: PSDL_mutex): Integer;
 cdecl; external {$IFDEF __GPC__}name 'SDL_mutexP'{$ELSE} SDLLibName{$ENDIF __GPC__};
{ $ EXTERNALSYM SDL_mutexP}

function SDL_LockMutex(mutex: PSDL_mutex): Integer;
{$EXTERNALSYM SDL_LockMutex}

{ Unlock the mutex  (Returns 0, or -1 on error) }
function SDL_mutexV(mutex: PSDL_mutex): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_mutexV'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_mutexV}

function SDL_UnlockMutex(mutex: PSDL_mutex): Integer;
{$EXTERNALSYM SDL_UnlockMutex}

{ Destroy a mutex }
procedure SDL_DestroyMutex(mutex: PSDL_mutex);
cdecl; external {$IFDEF __GPC__}name 'SDL_DestroyMutex'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_DestroyMutex}

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ Semaphore functions                                           }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ Create a semaphore, initialized with value, returns NULL on failure. }
function SDL_CreateSemaphore(initial_value: UInt32): PSDL_Sem;
cdecl; external {$IFDEF __GPC__}name 'SDL_CreateSemaphore'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CreateSemaphore}


{ Destroy a semaphore }
procedure SDL_DestroySemaphore(sem: PSDL_sem);
cdecl; external {$IFDEF __GPC__}name 'SDL_DestroySemaphore'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_DestroySemaphore}

{ This function suspends the calling thread until the semaphore pointed
  to by sem has a positive count. It then atomically decreases the semaphore
  count. }

function SDL_SemWait(sem: PSDL_sem): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_SemWait'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SemWait}

{ Non-blocking variant of SDL_SemWait(), returns 0 if the wait succeeds,
   SDL_MUTEX_TIMEDOUT if the wait would block, and -1 on error. }

function SDL_SemTryWait(sem: PSDL_sem): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_SemTryWait'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SemTryWait}

{ Variant of SDL_SemWait() with a timeout in milliseconds, returns 0 if
   the wait succeeds, SDL_MUTEX_TIMEDOUT if the wait does not succeed in
   the allotted time, and -1 on error.
   On some platforms this function is implemented by looping with a delay
   of 1 ms, and so should be avoided if possible. }

function SDL_SemWaitTimeout(sem: PSDL_sem; ms: UInt32): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_SemWaitTimeout'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SemTryWait}

{ Atomically increases the semaphore's count (not blocking), returns 0,
   or -1 on error. }

function SDL_SemPost(sem: PSDL_sem): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_SemPost'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SemTryWait}

{ Returns the current count of the semaphore }

function SDL_SemValue(sem: PSDL_sem): UInt32;
cdecl; external {$IFDEF __GPC__}name 'SDL_SemValue'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_SemValue}

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ Condition variable functions                                  }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ Create a condition variable }
function SDL_CreateCond: PSDL_Cond;
cdecl; external {$IFDEF __GPC__}name 'SDL_CreateCond'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CreateCond}

{ Destroy a condition variable }
procedure SDL_DestroyCond(cond: PSDL_Cond);
cdecl; external {$IFDEF __GPC__}name 'SDL_DestroyCond'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_DestroyCond}

{ Restart one of the threads that are waiting on the condition variable,
   returns 0 or -1 on error. }

function SDL_CondSignal(cond: PSDL_cond): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_CondSignal'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CondSignal}

{ Restart all threads that are waiting on the condition variable,
  returns 0 or -1 on error. }

function SDL_CondBroadcast(cond: PSDL_cond): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_CondBroadcast'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CondBroadcast}


{ Wait on the condition variable, unlocking the provided mutex.
  The mutex must be locked before entering this function!
  Returns 0 when it is signaled, or -1 on error. }

function SDL_CondWait(cond: PSDL_cond; mut: PSDL_mutex): Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_CondWait'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CondWait}

{ Waits for at most 'ms' milliseconds, and returns 0 if the condition
  variable is signaled, SDL_MUTEX_TIMEDOUT if the condition is not
  signaled in the allotted time, and -1 on error.
  On some platforms this function is implemented by looping with a delay
  of 1 ms, and so should be avoided if possible. }

function SDL_CondWaitTimeout(cond: PSDL_cond; mut: PSDL_mutex; ms: UInt32) : Integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_CondWaitTimeout'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CondWaitTimeout}

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ Condition variable functions                                  }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{ Create a thread }
function SDL_CreateThread(fn: PInt; data: Pointer): PSDL_Thread;
cdecl; external {$IFDEF __GPC__}name 'SDL_CreateThread'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_CreateThread}

{ Get the 32-bit thread identifier for the current thread }
function SDL_ThreadID: UInt32;
cdecl; external {$IFDEF __GPC__}name 'SDL_ThreadID'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_ThreadID}

{ Get the 32-bit thread identifier for the specified thread,
  equivalent to SDL_ThreadID() if the specified thread is NULL. }
function SDL_GetThreadID(thread: PSDL_Thread): UInt32;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetThreadID'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetThreadID}

{ Wait for a thread to finish.
  The return code for the thread function is placed in the area
  pointed to by 'status', if 'status' is not NULL. }

procedure SDL_WaitThread(thread: PSDL_Thread; var status: Integer);
cdecl; external {$IFDEF __GPC__}name 'SDL_WaitThread'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_WaitThread}

{ Forcefully kill a thread without worrying about its state }
procedure SDL_KillThread(thread: PSDL_Thread);
cdecl; external {$IFDEF __GPC__}name 'SDL_KillThread'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_KillThread}

{------------------------------------------------------------------------------}
{ Get Environment Routines                                                     }
{------------------------------------------------------------------------------}
{$IFDEF WIN32}
function _putenv( const variable : Pchar ): integer;
cdecl;
{$ENDIF}

{$IFDEF Unix}
{$IFDEF FPC}
function _putenv( const variable : Pchar ): integer;
cdecl; external 'libc.so' name 'putenv';
{$ENDIF}
{$ENDIF}

{ Put a variable of the form "name=value" into the environment }
//function SDL_putenv(const variable: PChar): integer; cdecl; external {$IFDEF __GPC__}name 'SDL_Init'{$ELSE} SDLLibName{$ENDIF __GPC__}SDLLibName name '';
function SDL_putenv(const variable: PChar): integer;
{$EXTERNALSYM SDL_putenv}

// The following function has been commented out to encourage developers to use
// SDL_putenv as it it more portable
//function putenv(const variable: PChar): integer;
//{$EXTERNALSYM putenv}

{$IFDEF WIN32}
{$IFNDEF __GPC__}
function getenv( const name : Pchar ): PChar; cdecl;
{$ENDIF}
{$ENDIF}

{* Retrieve a variable named "name" from the environment }
//function SDL_getenv(const name: PChar): PChar; cdecl; external {$IFDEF __GPC__}name 'SDL_Init'{$ELSE} SDLLibName{$ENDIF __GPC__}SDLLibName name '';
function SDL_getenv(const name: PChar): PChar;
{$EXTERNALSYM SDL_getenv}

// The following function has been commented out to encourage developers to use
// SDL_getenv as it it more portable
//function getenv(const name: PChar): PChar;
//{$EXTERNALSYM getenv}

{*
 * This function gives you custom hooks into the window manager information.
 * It fills the structure pointed to by 'info' with custom information and
 * returns 1 if the function is implemented.  If it's not implemented, or
 * the version member of the 'info' structure is invalid, it returns 0.
 *}
function SDL_GetWMInfo(info : PSDL_SysWMinfo) : integer;
cdecl; external {$IFDEF __GPC__}name 'SDL_GetWMInfo'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_GetWMInfo}

{------------------------------------------------------------------------------}

//SDL_loadso.h
{* This function dynamically loads a shared object and returns a pointer
 * to the object handle (or NULL if there was an error).
 * The 'sofile' parameter is a system dependent name of the object file.
 *}
function SDL_LoadObject( const sofile : PChar ) : Pointer;
cdecl; external {$IFDEF __GPC__}name 'SDL_LoadObject'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_LoadObject}

{* Given an object handle, this function looks up the address of the
 * named function in the shared object and returns it.  This address
 * is no longer valid after calling SDL_UnloadObject().
 *}
function SDL_LoadFunction( handle : Pointer; const name : PChar ) : Pointer;
cdecl; external {$IFDEF __GPC__}name 'SDL_LoadFunction'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_LoadFunction}

{* Unload a shared object from memory *}
procedure SDL_UnloadObject( handle : Pointer );
cdecl; external {$IFDEF __GPC__}name 'SDL_UnloadObject'{$ELSE} SDLLibName{$ENDIF __GPC__};
{$EXTERNALSYM SDL_UnloadObject}



{------------------------------------------------------------------------------}

function SDL_Swap32(D: Uint32): Uint32;
{$EXTERNALSYM SDL_Swap32}

{ FreeAndNil frees the given TObject instance and sets the variable reference
  to nil.  Be careful to only pass TObjects to this routine. }
procedure FreeAndNil(var Obj);

{ Exit procedure handling }

{ AddExitProc adds the given procedure to the run-time library's exit
  procedure list. When an application terminates, its exit procedures are
  executed in reverse order of definition, i.e. the last procedure passed
  to AddExitProc is the first one to get executed upon termination. }
procedure AddExitProc(Proc: TProcedure);

// Bitwise Checking functions
function IsBitOn( value : integer; bit : Byte ) : boolean;

function TurnBitOn( value : integer; bit : Byte ) : integer;

function TurnBitOff( value : integer; bit : Byte ) : integer;

implementation

{$IFDEF __GPC__}
  {$L 'sdl'}  { link sdl.dll.a or libsdl.so or libsdl.a }
{$ENDIF}

function SDL_TABLESIZE(table: PChar): Integer;
begin
  Result := SizeOf(table) div SizeOf(table[0]);
end;

procedure SDL_OutOfMemory;
begin
  {$IFNDEF WIN32}
  SDL_Error(SDL_ENOMEM);
  {$ENDIF}
end;

function SDL_RWSeek(context: PSDL_RWops; offset: Integer; whence: Integer) : Integer;
begin
  Result := context^.seek(context, offset, whence);
end;

function SDL_RWTell(context: PSDL_RWops): Integer;
begin
  Result := context^.seek(context, 0, 1);
end;

function SDL_RWRead(context: PSDL_RWops; ptr: Pointer; size: Integer; n: Integer): Integer;
begin
  Result := context^.read(context, ptr, size, n);
end;

function SDL_RWWrite(context: PSDL_RWops; ptr: Pointer; size: Integer; n: Integer): Integer;
begin
  Result := context^.write(context, ptr, size, n);
end;

function SDL_RWClose(context: PSDL_RWops): Integer;
begin
  Result := context^.close(context);
end;

function SDL_LoadWAV(filename: PChar; spec: PSDL_AudioSpec; audio_buf: PUInt8; audiolen: PUInt32): PSDL_AudioSpec;
begin
  Result := SDL_LoadWAV_RW(SDL_RWFromFile(filename, 'rb'), 1, spec, audio_buf, audiolen);
end;

function SDL_CDInDrive( status : TSDL_CDStatus ): LongBool;
begin
  Result := ord( status ) > ord( CD_ERROR );
end;

procedure FRAMES_TO_MSF(frames: Integer; var M: Integer; var S: Integer; var
  F: Integer);
var
  value: Integer;
begin
  value := frames;
  F := value mod CD_FPS;
  value := value div CD_FPS;
  S := value mod 60;
  value := value div 60;
  M := value;
end;

function MSF_TO_FRAMES(M: Integer; S: Integer; F: Integer): Integer;
begin
  Result := M * 60 * CD_FPS + S * CD_FPS + F;
end;

procedure SDL_VERSION(var X: TSDL_Version);
begin
  X.major := SDL_MAJOR_VERSION;
  X.minor := SDL_MINOR_VERSION;
  X.patch := SDL_PATCHLEVEL;
end;

function SDL_VERSIONNUM(X, Y, Z: Integer): Integer;
begin
  Result := X * 1000 + Y * 100 + Z;
end;

function SDL_COMPILEDVERSION: Integer;
begin
  Result := SDL_VERSIONNUM(SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_PATCHLEVEL
    );
end;

function SDL_VERSION_ATLEAST(X, Y, Z: Integer): LongBool;
begin
  Result := (SDL_COMPILEDVERSION >= SDL_VERSIONNUM(X, Y, Z));
end;

function SDL_LoadBMP(filename: PChar): PSDL_Surface;
begin
  Result := SDL_LoadBMP_RW(SDL_RWFromFile(filename, 'rb'), 1);
end;

function SDL_SaveBMP(surface: PSDL_Surface; filename: PChar): Integer;
begin
  Result := SDL_SaveBMP_RW(surface, SDL_RWFromFile(filename, 'wb'), 1);
end;

function SDL_BlitSurface(src: PSDL_Surface; srcrect: PSDL_Rect; dst:
  PSDL_Surface;
  dstrect: PSDL_Rect): Integer;
begin
  Result := SDL_UpperBlit(src, srcrect, dst, dstrect);
end;

function SDL_AllocSurface(flags: UInt32; width, height, depth: Integer;
  RMask, GMask, BMask, AMask: UInt32): PSDL_Surface;
begin
  Result := SDL_CreateRGBSurface(flags, width, height, depth, RMask, GMask,
    BMask, AMask);
end;

function SDL_MustLock(Surface: PSDL_Surface): Boolean;
begin
  Result := ( ( surface^.offset <> 0 ) or
           ( ( surface^.flags and ( SDL_HWSURFACE or SDL_ASYNCBLIT or SDL_RLEACCEL ) ) <> 0 ) );
end;

function SDL_LockMutex(mutex: PSDL_mutex): Integer;
begin
  Result := SDL_mutexP(mutex);
end;

function SDL_UnlockMutex(mutex: PSDL_mutex): Integer;
begin
  Result := SDL_mutexV(mutex);
end;

{$IFDEF WIN32}
function _putenv( const variable : Pchar ): Integer;
cdecl; external {$IFDEF __GPC__}name '_putenv'{$ELSE} 'MSVCRT.DLL'{$ENDIF __GPC__};
{$ENDIF}


function SDL_putenv(const variable: PChar): Integer;
begin
  {$IFDEF WIN32}
  Result := _putenv(variable);
  {$ENDIF}

  {$IFDEF UNIX}
  {$IFDEF FPC}
  Result := _putenv(variable);
  {$ELSE}
  Result := libc.putenv(variable);
  {$ENDIF}
  {$ENDIF}
end;

{$IFDEF WIN32}
{$IFNDEF __GPC__}
function getenv( const name : Pchar ): PChar;
cdecl; external {$IFDEF __GPC__}name 'getenv'{$ELSE} 'MSVCRT.DLL'{$ENDIF};
{$ENDIF}
{$ENDIF}

function SDL_getenv(const name: PChar): PChar;
begin
  {$IFDEF WIN32}

  {$IFDEF __GPC__}
  Result := getenv( string( name ) );
  {$ELSE}
  Result := getenv( name );
  {$ENDIF}

  {$ELSE}

  {$IFDEF UNIX}

  {$IFDEF FPC}
  Result := fpgetenv(name);
  {$ELSE}
  Result := libc.getenv(name);  
  {$ENDIF}

  {$ENDIF}

  {$ENDIF}
end;

function SDL_BUTTON( Button : Integer ) : Integer;
begin
  Result := SDL_PRESSED shl ( Button - 1 );
end;

function SDL_Swap32(D: Uint32): Uint32;
begin
  Result := ((D shl 24) or ((D shl 8) and $00FF0000) or ((D shr 8) and $0000FF00) or (D shr 24));
end;

procedure FreeAndNil(var Obj);
{$IFNDEF __GPC__}
{$IFNDEF __TMT__}
var
  Temp: TObject;
{$ENDIF}
{$ENDIF}
begin
{$IFNDEF __GPC__}
{$IFNDEF __TMT__}
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
{$ENDIF}
{$ENDIF}
end;

{ Exit procedure handling }
type
  PExitProcInfo = ^TExitProcInfo;
  TExitProcInfo = record
    Next: PExitProcInfo;
    SaveExit: Pointer;
    Proc: TProcedure;
  end;

var
  ExitProcList: PExitProcInfo = nil;

procedure DoExitProc;
var
  P: PExitProcInfo;
  Proc: TProcedure;
begin
  P := ExitProcList;
  ExitProcList := P^.Next;
  ExitProc := P^.SaveExit;
  Proc := P^.Proc;
  Dispose(P);
  Proc;
end;

procedure AddExitProc(Proc: TProcedure);
var
  P: PExitProcInfo;
begin
  New(P);
  P^.Next := ExitProcList;
  P^.SaveExit := ExitProc;
  P^.Proc := Proc;
  ExitProcList := P;
  ExitProc := @DoExitProc;
end;

function IsBitOn( value : integer; bit : Byte ) : boolean;
begin
  result := ( ( value and ( 1 shl bit ) ) <> 0 );
end;

function TurnBitOn( value : integer; bit : Byte ) : integer;
begin
  result := ( value or ( 1 shl bit ) );
end;

function TurnBitOff( value : integer; bit : Byte ) : integer;
begin
  result := ( value and not ( 1 shl bit ) );
end;

end.


