{
  BASSMIDI 2.4 Delphi unit
  Copyright (c) 2006-2011 Un4seen Developments Ltd.

  See the BASSMIDI.CHM file for more detailed documentation
}

unit BassMIDI;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses Bass;

const
  // Additional config options
  BASS_CONFIG_MIDI_COMPACT   = $10400;
  BASS_CONFIG_MIDI_VOICES    = $10401;
  BASS_CONFIG_MIDI_AUTOFONT  = $10402;

  // Additional BASS_SetConfigPtr options
  BASS_CONFIG_MIDI_DEFFONT   = $10403;

  // Additional sync types
  BASS_SYNC_MIDI_MARKER      = $10000;
  BASS_SYNC_MIDI_CUE         = $10001;
  BASS_SYNC_MIDI_LYRIC       = $10002;
  BASS_SYNC_MIDI_TEXT        = $10003;
  BASS_SYNC_MIDI_EVENT       = $10004;
  BASS_SYNC_MIDI_TICK        = $10005;
  BASS_SYNC_MIDI_TIMESIG     = $10006;
  BASS_SYNC_MIDI_KEYSIG      = $10007;

  // Additional BASS_MIDI_StreamCreateFile/etc flags
  BASS_MIDI_DECAYEND         = $1000;
  BASS_MIDI_NOFX             = $2000;
  BASS_MIDI_DECAYSEEK        = $4000;

  // Marker types
  BASS_MIDI_MARK_MARKER      = 0; // marker events
  BASS_MIDI_MARK_CUE         = 1; // cue events
  BASS_MIDI_MARK_LYRIC       = 2; // lyric events
  BASS_MIDI_MARK_TEXT        = 3; // text events
  BASS_MIDI_MARK_TIMESIG     = 4; // time signature
  BASS_MIDI_MARK_KEYSIG      = 5; // key signature

  // MIDI events
  MIDI_EVENT_NOTE            = 1;
  MIDI_EVENT_PROGRAM         = 2;
  MIDI_EVENT_CHANPRES        = 3;
  MIDI_EVENT_PITCH           = 4;
  MIDI_EVENT_PITCHRANGE      = 5;
  MIDI_EVENT_DRUMS           = 6;
  MIDI_EVENT_FINETUNE        = 7;
  MIDI_EVENT_COARSETUNE      = 8;
  MIDI_EVENT_MASTERVOL       = 9;
  MIDI_EVENT_BANK            = 10;
  MIDI_EVENT_MODULATION      = 11;
  MIDI_EVENT_VOLUME          = 12;
  MIDI_EVENT_PAN             = 13;
  MIDI_EVENT_EXPRESSION      = 14;
  MIDI_EVENT_SUSTAIN         = 15;
  MIDI_EVENT_SOUNDOFF        = 16;
  MIDI_EVENT_RESET           = 17;
  MIDI_EVENT_NOTESOFF        = 18;
  MIDI_EVENT_PORTAMENTO      = 19;
  MIDI_EVENT_PORTATIME       = 20;
  MIDI_EVENT_PORTANOTE       = 21;
  MIDI_EVENT_MODE            = 22;
  MIDI_EVENT_REVERB          = 23;
  MIDI_EVENT_CHORUS          = 24;
  MIDI_EVENT_CUTOFF          = 25;
  MIDI_EVENT_RESONANCE       = 26;
  MIDI_EVENT_RELEASE         = 27;
  MIDI_EVENT_ATTACK          = 28;
  MIDI_EVENT_REVERB_MACRO    = 30;
  MIDI_EVENT_CHORUS_MACRO    = 31;
  MIDI_EVENT_REVERB_TIME     = 32;
  MIDI_EVENT_REVERB_DELAY    = 33;
  MIDI_EVENT_REVERB_LOCUTOFF = 34;
  MIDI_EVENT_REVERB_HICUTOFF = 35;
  MIDI_EVENT_REVERB_LEVEL    = 36;
  MIDI_EVENT_CHORUS_DELAY    = 37;
  MIDI_EVENT_CHORUS_DEPTH    = 38;
  MIDI_EVENT_CHORUS_RATE     = 39;
  MIDI_EVENT_CHORUS_FEEDBACK = 40;
  MIDI_EVENT_CHORUS_LEVEL    = 41;
  MIDI_EVENT_CHORUS_REVERB   = 42;
  MIDI_EVENT_DRUM_FINETUNE   = 50;
  MIDI_EVENT_DRUM_COARSETUNE = 51;
  MIDI_EVENT_DRUM_PAN        = 52;
  MIDI_EVENT_DRUM_REVERB     = 53;
  MIDI_EVENT_DRUM_CHORUS     = 54;
  MIDI_EVENT_DRUM_CUTOFF     = 55;
  MIDI_EVENT_DRUM_RESONANCE  = 56;
  MIDI_EVENT_DRUM_LEVEL      = 57;
  MIDI_EVENT_SOFT            = 60;
  MIDI_EVENT_SYSTEM          = 61;
  MIDI_EVENT_TEMPO           = 62;
  MIDI_EVENT_SCALETUNING     = 63;
  MIDI_EVENT_MIXLEVEL        = $10000;
  MIDI_EVENT_TRANSPOSE       = $10001;
  MIDI_EVENT_SYSTEMEX        = $10002;

  MIDI_SYSTEM_DEFAULT        = 0;
  MIDI_SYSTEM_GM1            = 1;
  MIDI_SYSTEM_GM2            = 2;
  MIDI_SYSTEM_XG             = 3;
  MIDI_SYSTEM_GS             = 4;

  // BASS_MIDI_StreamEvents modes
  BASS_MIDI_EVENTS_STRUCT    = 0; // BASS_MIDI_EVENT structures
  BASS_MIDI_EVENTS_RAW       = $10000; // raw MIDI event data
  BASS_MIDI_EVENTS_SYNC      = $1000000; // FLAG: trigger event syncs

  // BASS_CHANNELINFO type
  BASS_CTYPE_STREAM_MIDI     = $10d00;

  // Additional attributes
  BASS_ATTRIB_MIDI_PPQN      = $12000;
  BASS_ATTRIB_MIDI_CPU       = $12001;
  BASS_ATTRIB_MIDI_CHANS     = $12002;
  BASS_ATTRIB_MIDI_VOICES    = $12003;
  BASS_ATTRIB_MIDI_TRACK_VOL = $12100; // + track #

  // Additional tag type
  BASS_TAG_MIDI_TRACK        = $11000; // + track #, track text : array of null-terminated ANSI strings

  // BASS_ChannelGetLength/GetPosition/SetPosition mode
  BASS_POS_MIDI_TICK         = 2; // tick position


type
  HSOUNDFONT = DWORD;   // soundfont handle

  BASS_MIDI_FONT = record
    font: HSOUNDFONT;   // soundfont
    preset: LongInt;    // preset number (-1=all)
    bank: Longint;
  end;

  BASS_MIDI_FONTINFO = record
    name: PAnsiChar;
    copyright: PAnsiChar;
    comment: PAnsiChar;
    presets: DWORD;     // number of presets/instruments
    samsize: DWORD;     // total size (in bytes) of the sample data
    samload: DWORD;     // amount of sample data currently loaded
    samtype: DWORD;     // sample format (CTYPE) if packed
  end;

  BASS_MIDI_MARK = record
    track: DWORD;       // track containing marker
    pos: DWORD;         // marker position
    text: PAnsiChar;    // marker text
  end;

  BASS_MIDI_EVENT = record
    event: DWORD;       // MIDI_EVENT_xxx
    param: DWORD;
    chan: DWORD;
    tick: DWORD;        // event position (ticks)
    pos: DWORD;         // event position (bytes)
  end;

  BASS_MIDI_DEVICEINFO = record
	name: PAnsiChar;	// description
    id: DWORD;
	flags: DWORD;
  end;

  // callback function types
  MIDIINPROC = procedure(device: DWORD; time: Double; buffer: Pointer; length: DWORD; user: Pointer); stdcall;
  {
    User MIDI input callback function.
    device : MIDI input device
    time   : Timestamp
    buffer : Buffer containing MIDI data
    length : Number of bytes of data
    user   : The 'user' parameter value given when calling BASS_MIDI_InInit
  }


const
  bassmididll = 'bassmidi.dll';

function BASS_MIDI_StreamCreate(channels,flags,freq:DWORD): HSTREAM; stdcall; external bassmididll;
function BASS_MIDI_StreamCreateFile(mem:BOOL; fl:pointer; offset,length:QWORD; flags,freq:DWORD): HSTREAM; stdcall; external bassmididll;
function BASS_MIDI_StreamCreateURL(URL:PAnsiChar; offset:DWORD; flags:DWORD; proc:DOWNLOADPROC; user:Pointer; freq:DWORD): HSTREAM; stdcall; external bassmididll;
function BASS_MIDI_StreamCreateFileUser(system,flags:DWORD; var procs:BASS_FILEPROCS; user:Pointer; freq:DWORD): HSTREAM; stdcall; external bassmididll;
function BASS_MIDI_StreamGetMark(handle:HSTREAM; type_,index:DWORD; var mark:BASS_MIDI_MARK): BOOL; stdcall; external bassmididll;
function BASS_MIDI_StreamSetFonts(handle:HSTREAM; var fonts:BASS_MIDI_FONT; count:DWORD): BOOL; stdcall; external bassmididll;
function BASS_MIDI_StreamGetFonts(handle:HSTREAM; var fonts:BASS_MIDI_FONT; count:DWORD): DWORD; stdcall; external bassmididll;
function BASS_MIDI_StreamLoadSamples(handle:HSTREAM): BOOL; stdcall; external bassmididll;
function BASS_MIDI_StreamEvent(handle:HSTREAM; chan,event,param:DWORD): BOOL; stdcall; external bassmididll;
function BASS_MIDI_StreamEvents(handle:HSTREAM; mode:DWORD; events:Pointer; length:DWORD): DWORD; stdcall; external bassmididll;
function BASS_MIDI_StreamGetEvent(handle:HSTREAM; chan,event:DWORD): DWORD; stdcall; external bassmididll;
function BASS_MIDI_StreamGetEvents(handle:HSTREAM; chan,filter:DWORD; var events:BASS_MIDI_EVENT): DWORD; stdcall; external bassmididll;
function BASS_MIDI_StreamGetChannel(handle:HSTREAM; chan:DWORD): HSTREAM; stdcall; external bassmididll;

function BASS_MIDI_FontInit(fname:PChar; flags:DWORD): HSOUNDFONT; stdcall; external bassmididll;
function BASS_MIDI_FontFree(handle:HSOUNDFONT): BOOL; stdcall; external bassmididll;
function BASS_MIDI_FontGetInfo(handle:HSOUNDFONT; var info:BASS_MIDI_FONTINFO): BOOL; stdcall; external bassmididll;
function BASS_MIDI_FontGetPreset(handle:HSOUNDFONT; preset,bank:LongInt): PAnsiChar; stdcall; external bassmididll;
function BASS_MIDI_FontLoad(handle:HSOUNDFONT; preset,bank:LongInt): BOOL; stdcall; external bassmididll;
function BASS_MIDI_FontCompact(handle:HSOUNDFONT): BOOL; stdcall; external bassmididll;
function BASS_MIDI_FontPack(handle:HSOUNDFONT; outfile,encoder:PChar; flags:DWORD): BOOL; stdcall; external bassmididll;
function BASS_MIDI_FontUnpack(handle:HSOUNDFONT; outfile:PChar; flags:DWORD): BOOL; stdcall; external bassmididll;
function BASS_MIDI_FontSetVolume(handle:HSOUNDFONT; volume:Single): BOOL; stdcall; external bassmididll;
function BASS_MIDI_FontGetVolume(handle:HSOUNDFONT): Single; stdcall; external bassmididll;

function BASS_MIDI_InGetDeviceInfo(device:DWORD; var info: BASS_MIDI_DEVICEINFO): BOOL; stdcall; external bassmididll;
function BASS_MIDI_InInit(device:DWORD; proc:MIDIINPROC; user:Pointer): BOOL; stdcall; external bassmididll;
function BASS_MIDI_InFree(device:DWORD): BOOL; stdcall; external bassmididll;
function BASS_MIDI_InStart(device:DWORD): BOOL; stdcall; external bassmididll;
function BASS_MIDI_InStop(device:DWORD): BOOL; stdcall; external bassmididll;

implementation

end.
