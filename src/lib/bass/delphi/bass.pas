{
  BASS 2.4 Delphi unit
  Copyright (c) 1999-2008 Un4seen Developments Ltd.

  See the BASS.CHM file for more detailed documentation

  How to install
  --------------
  Copy BASS.PAS to the \LIB subdirectory of your Delphi path or your project dir
}

unit Bass;

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$PACKRECORDS C}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$DEFINE DLL_STDCALL}
{$ELSE}
  {$DEFINE DLL_CDECL}
{$ENDIF}

// IMPORTANT: define BASS_242 when switching to 2.4.2(.1) as
// BASS_RECORDINFO.driver was removed.
// Otherwise BASS_RECORDINFO.freq will point to a wrong location.
{$UNDEF BASS_242}


{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

const
  BASSVERSION = $204;             // API version
  BASSVERSIONTEXT = '2.4';

  // Use these to test for error from functions that return a DWORD or QWORD
  DW_ERROR = Cardinal(-1); // -1 (DWORD)
  QW_ERROR = Int64(-1);    // -1 (QWORD)

  // Error codes returned by BASS_ErrorGetCode()
  BASS_OK                 = 0;    // all is OK
  BASS_ERROR_MEM          = 1;    // memory error
  BASS_ERROR_FILEOPEN     = 2;    // can't open the file
  BASS_ERROR_DRIVER       = 3;    // can't find a free sound driver
  BASS_ERROR_BUFLOST      = 4;    // the sample buffer was lost
  BASS_ERROR_HANDLE       = 5;    // invalid handle
  BASS_ERROR_FORMAT       = 6;    // unsupported sample format
  BASS_ERROR_POSITION     = 7;    // invalid position
  BASS_ERROR_INIT         = 8;    // BASS_Init has not been successfully called
  BASS_ERROR_START        = 9;    // BASS_Start has not been successfully called
  BASS_ERROR_ALREADY      = 14;   // already initialized/paused/whatever
  BASS_ERROR_NOCHAN       = 18;   // can't get a free channel
  BASS_ERROR_ILLTYPE      = 19;   // an illegal type was specified
  BASS_ERROR_ILLPARAM     = 20;   // an illegal parameter was specified
  BASS_ERROR_NO3D         = 21;   // no 3D support
  BASS_ERROR_NOEAX        = 22;   // no EAX support
  BASS_ERROR_DEVICE       = 23;   // illegal device number
  BASS_ERROR_NOPLAY       = 24;   // not playing
  BASS_ERROR_FREQ         = 25;   // illegal sample rate
  BASS_ERROR_NOTFILE      = 27;   // the stream is not a file stream
  BASS_ERROR_NOHW         = 29;   // no hardware voices available
  BASS_ERROR_EMPTY        = 31;   // the MOD music has no sequence data
  BASS_ERROR_NONET        = 32;   // no internet connection could be opened
  BASS_ERROR_CREATE       = 33;   // couldn't create the file
  BASS_ERROR_NOFX         = 34;   // effects are not enabled
  BASS_ERROR_NOTAVAIL     = 37;   // requested data is not available
  BASS_ERROR_DECODE       = 38;   // the channel is a "decoding channel"
  BASS_ERROR_DX           = 39;   // a sufficient DirectX version is not installed
  BASS_ERROR_TIMEOUT      = 40;   // connection timedout
  BASS_ERROR_FILEFORM     = 41;   // unsupported file format
  BASS_ERROR_SPEAKER      = 42;   // unavailable speaker
  BASS_ERROR_VERSION      = 43;   // invalid BASS version (used by add-ons)
  BASS_ERROR_CODEC        = 44;   // codec is not available/supported
  BASS_ERROR_ENDED        = 45;   // the channel/file has ended
  BASS_ERROR_UNKNOWN      = -1;   // some other mystery problem

  // BASS_SetConfig options
  BASS_CONFIG_BUFFER        = 0;
  BASS_CONFIG_UPDATEPERIOD  = 1;
  BASS_CONFIG_GVOL_SAMPLE   = 4;
  BASS_CONFIG_GVOL_STREAM   = 5;
  BASS_CONFIG_GVOL_MUSIC    = 6;
  BASS_CONFIG_CURVE_VOL     = 7;
  BASS_CONFIG_CURVE_PAN     = 8;
  BASS_CONFIG_FLOATDSP      = 9;
  BASS_CONFIG_3DALGORITHM   = 10;
  BASS_CONFIG_NET_TIMEOUT   = 11;
  BASS_CONFIG_NET_BUFFER    = 12;
  BASS_CONFIG_PAUSE_NOPLAY  = 13;
  BASS_CONFIG_NET_PREBUF    = 15;
  BASS_CONFIG_NET_PASSIVE   = 18;
  BASS_CONFIG_REC_BUFFER    = 19;
  BASS_CONFIG_NET_PLAYLIST  = 21;
  BASS_CONFIG_MUSIC_VIRTUAL = 22;
  BASS_CONFIG_VERIFY        = 23;
  BASS_CONFIG_UPDATETHREADS = 24;

  // BASS_SetConfigPtr options
  BASS_CONFIG_NET_AGENT     = 16;
  BASS_CONFIG_NET_PROXY     = 17;

  // Initialization flags
  BASS_DEVICE_8BITS       = 1;    // use 8 bit resolution, else 16 bit
  BASS_DEVICE_MONO        = 2;    // use mono, else stereo
  BASS_DEVICE_3D          = 4;    // enable 3D functionality
  BASS_DEVICE_LATENCY     = 256;  // calculate device latency (BASS_INFO struct)
  BASS_DEVICE_CPSPEAKERS  = 1024; // detect speakers via Windows control panel
  BASS_DEVICE_SPEAKERS    = 2048; // force enabling of speaker assignment
  BASS_DEVICE_NOSPEAKER   = 4096; // ignore speaker arrangement

  // DirectSound interfaces (for use with BASS_GetDSoundObject)
  BASS_OBJECT_DS          = 1;   // IDirectSound
  BASS_OBJECT_DS3DL       = 2;   // IDirectSound3DListener

  // BASS_DEVICEINFO flags
  BASS_DEVICE_ENABLED     = 1;
  BASS_DEVICE_DEFAULT     = 2;
  BASS_DEVICE_INIT        = 4;

  // BASS_INFO flags (from DSOUND.H)
  DSCAPS_CONTINUOUSRATE   = $00000010;     // supports all sample rates between min/maxrate
  DSCAPS_EMULDRIVER       = $00000020;     // device does NOT have hardware DirectSound support
  DSCAPS_CERTIFIED        = $00000040;     // device driver has been certified by Microsoft
  DSCAPS_SECONDARYMONO    = $00000100;     // mono
  DSCAPS_SECONDARYSTEREO  = $00000200;     // stereo
  DSCAPS_SECONDARY8BIT    = $00000400;     // 8 bit
  DSCAPS_SECONDARY16BIT   = $00000800;     // 16 bit

  // BASS_RECORDINFO flags (from DSOUND.H)
  DSCCAPS_EMULDRIVER = DSCAPS_EMULDRIVER;  // device does NOT have hardware DirectSound recording support
  DSCCAPS_CERTIFIED = DSCAPS_CERTIFIED;    // device driver has been certified by Microsoft

  // defines for formats field of BASS_RECORDINFO (from MMSYSTEM.H)
  WAVE_FORMAT_1M08       = $00000001;      // 11.025 kHz, Mono,   8-bit
  WAVE_FORMAT_1S08       = $00000002;      // 11.025 kHz, Stereo, 8-bit
  WAVE_FORMAT_1M16       = $00000004;      // 11.025 kHz, Mono,   16-bit
  WAVE_FORMAT_1S16       = $00000008;      // 11.025 kHz, Stereo, 16-bit
  WAVE_FORMAT_2M08       = $00000010;      // 22.05  kHz, Mono,   8-bit
  WAVE_FORMAT_2S08       = $00000020;      // 22.05  kHz, Stereo, 8-bit
  WAVE_FORMAT_2M16       = $00000040;      // 22.05  kHz, Mono,   16-bit
  WAVE_FORMAT_2S16       = $00000080;      // 22.05  kHz, Stereo, 16-bit
  WAVE_FORMAT_4M08       = $00000100;      // 44.1   kHz, Mono,   8-bit
  WAVE_FORMAT_4S08       = $00000200;      // 44.1   kHz, Stereo, 8-bit
  WAVE_FORMAT_4M16       = $00000400;      // 44.1   kHz, Mono,   16-bit
  WAVE_FORMAT_4S16       = $00000800;      // 44.1   kHz, Stereo, 16-bit

  BASS_SAMPLE_8BITS       = 1;   // 8 bit
  BASS_SAMPLE_FLOAT       = 256; // 32-bit floating-point
  BASS_SAMPLE_MONO        = 2;   // mono
  BASS_SAMPLE_LOOP        = 4;   // looped
  BASS_SAMPLE_3D          = 8;   // 3D functionality
  BASS_SAMPLE_SOFTWARE    = 16;  // not using hardware mixing
  BASS_SAMPLE_MUTEMAX     = 32;  // mute at max distance (3D only)
  BASS_SAMPLE_VAM         = 64;  // DX7 voice allocation & management
  BASS_SAMPLE_FX          = 128; // old implementation of DX8 effects
  BASS_SAMPLE_OVER_VOL    = $10000; // override lowest volume
  BASS_SAMPLE_OVER_POS    = $20000; // override longest playing
  BASS_SAMPLE_OVER_DIST   = $30000; // override furthest from listener (3D only)

  BASS_STREAM_PRESCAN     = $20000; // enable pin-point seeking/length (MP3/MP2/MP1)
  BASS_MP3_SETPOS         = BASS_STREAM_PRESCAN;
  BASS_STREAM_AUTOFREE	  = $40000; // automatically free the stream when it stop/ends
  BASS_STREAM_RESTRATE	  = $80000; // restrict the download rate of internet file streams
  BASS_STREAM_BLOCK       = $100000;// download/play internet file stream in small blocks
  BASS_STREAM_DECODE      = $200000;// don't play the stream, only decode (BASS_ChannelGetData)
  BASS_STREAM_STATUS      = $800000;// give server status info (HTTP/ICY tags) in DOWNLOADPROC

  BASS_MUSIC_FLOAT        = BASS_SAMPLE_FLOAT;
  BASS_MUSIC_MONO         = BASS_SAMPLE_MONO;
  BASS_MUSIC_LOOP         = BASS_SAMPLE_LOOP;
  BASS_MUSIC_3D           = BASS_SAMPLE_3D;
  BASS_MUSIC_FX           = BASS_SAMPLE_FX;
  BASS_MUSIC_AUTOFREE     = BASS_STREAM_AUTOFREE;
  BASS_MUSIC_DECODE       = BASS_STREAM_DECODE;
  BASS_MUSIC_PRESCAN      = BASS_STREAM_PRESCAN; // calculate playback length
  BASS_MUSIC_CALCLEN      = BASS_MUSIC_PRESCAN;
  BASS_MUSIC_RAMP         = $200;  // normal ramping
  BASS_MUSIC_RAMPS        = $400;  // sensitive ramping
  BASS_MUSIC_SURROUND     = $800;  // surround sound
  BASS_MUSIC_SURROUND2    = $1000; // surround sound (mode 2)
  BASS_MUSIC_FT2MOD       = $2000; // play .MOD as FastTracker 2 does
  BASS_MUSIC_PT1MOD       = $4000; // play .MOD as ProTracker 1 does
  BASS_MUSIC_NONINTER     = $10000; // non-interpolated sample mixing
  BASS_MUSIC_SINCINTER    = $800000; // sinc interpolated sample mixing
  BASS_MUSIC_POSRESET     = $8000; // stop all notes when moving position
  BASS_MUSIC_POSRESETEX   = $400000; // stop all notes and reset bmp/etc when moving position
  BASS_MUSIC_STOPBACK     = $80000; // stop the music on a backwards jump effect
  BASS_MUSIC_NOSAMPLE     = $100000; // don't load the samples

  // Speaker assignment flags
  BASS_SPEAKER_FRONT      = $1000000;  // front speakers
  BASS_SPEAKER_REAR       = $2000000;  // rear/side speakers
  BASS_SPEAKER_CENLFE     = $3000000;  // center & LFE speakers (5.1)
  BASS_SPEAKER_REAR2      = $4000000;  // rear center speakers (7.1)
  BASS_SPEAKER_LEFT       = $10000000; // modifier: left
  BASS_SPEAKER_RIGHT      = $20000000; // modifier: right
  BASS_SPEAKER_FRONTLEFT  = BASS_SPEAKER_FRONT or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_FRONTRIGHT = BASS_SPEAKER_FRONT or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_REARLEFT   = BASS_SPEAKER_REAR or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_REARRIGHT  = BASS_SPEAKER_REAR or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_CENTER     = BASS_SPEAKER_CENLFE or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_LFE        = BASS_SPEAKER_CENLFE or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_REAR2LEFT  = BASS_SPEAKER_REAR2 or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_REAR2RIGHT = BASS_SPEAKER_REAR2 or BASS_SPEAKER_RIGHT;

  BASS_UNICODE            = $80000000;

  BASS_RECORD_PAUSE       = $8000; // start recording paused

  // DX7 voice allocation & management flags
  BASS_VAM_HARDWARE       = 1;
  BASS_VAM_SOFTWARE       = 2;
  BASS_VAM_TERM_TIME      = 4;
  BASS_VAM_TERM_DIST      = 8;
  BASS_VAM_TERM_PRIO      = 16;

  // BASS_CHANNELINFO types
  BASS_CTYPE_SAMPLE       = 1;
  BASS_CTYPE_RECORD       = 2;
  BASS_CTYPE_STREAM       = $10000;
  BASS_CTYPE_STREAM_OGG   = $10002;
  BASS_CTYPE_STREAM_MP1   = $10003;
  BASS_CTYPE_STREAM_MP2   = $10004;
  BASS_CTYPE_STREAM_MP3   = $10005;
  BASS_CTYPE_STREAM_AIFF  = $10006;
  BASS_CTYPE_STREAM_WAV   = $40000; // WAVE flag, LOWORD=codec
  BASS_CTYPE_STREAM_WAV_PCM = $50001;
  BASS_CTYPE_STREAM_WAV_FLOAT = $50003;
  BASS_CTYPE_MUSIC_MOD    = $20000;
  BASS_CTYPE_MUSIC_MTM    = $20001;
  BASS_CTYPE_MUSIC_S3M    = $20002;
  BASS_CTYPE_MUSIC_XM     = $20003;
  BASS_CTYPE_MUSIC_IT     = $20004;
  BASS_CTYPE_MUSIC_MO3    = $00100; // MO3 flag

  // 3D channel modes
  BASS_3DMODE_NORMAL      = 0; // normal 3D processing
  BASS_3DMODE_RELATIVE    = 1; // position is relative to the listener
  BASS_3DMODE_OFF         = 2; // no 3D processing

  // software 3D mixing algorithms (used with BASS_CONFIG_3DALGORITHM)
  BASS_3DALG_DEFAULT      = 0;
  BASS_3DALG_OFF          = 1;
  BASS_3DALG_FULL         = 2;
  BASS_3DALG_LIGHT        = 3;

{$IFDEF MSWINDOWS}
  // EAX environments, use with BASS_SetEAXParameters
  EAX_ENVIRONMENT_GENERIC           = 0;
  EAX_ENVIRONMENT_PADDEDCELL        = 1;
  EAX_ENVIRONMENT_ROOM              = 2;
  EAX_ENVIRONMENT_BATHROOM          = 3;
  EAX_ENVIRONMENT_LIVINGROOM        = 4;
  EAX_ENVIRONMENT_STONEROOM         = 5;
  EAX_ENVIRONMENT_AUDITORIUM        = 6;
  EAX_ENVIRONMENT_CONCERTHALL       = 7;
  EAX_ENVIRONMENT_CAVE              = 8;
  EAX_ENVIRONMENT_ARENA             = 9;
  EAX_ENVIRONMENT_HANGAR            = 10;
  EAX_ENVIRONMENT_CARPETEDHALLWAY   = 11;
  EAX_ENVIRONMENT_HALLWAY           = 12;
  EAX_ENVIRONMENT_STONECORRIDOR     = 13;
  EAX_ENVIRONMENT_ALLEY             = 14;
  EAX_ENVIRONMENT_FOREST            = 15;
  EAX_ENVIRONMENT_CITY              = 16;
  EAX_ENVIRONMENT_MOUNTAINS         = 17;
  EAX_ENVIRONMENT_QUARRY            = 18;
  EAX_ENVIRONMENT_PLAIN             = 19;
  EAX_ENVIRONMENT_PARKINGLOT        = 20;
  EAX_ENVIRONMENT_SEWERPIPE         = 21;
  EAX_ENVIRONMENT_UNDERWATER        = 22;
  EAX_ENVIRONMENT_DRUGGED           = 23;
  EAX_ENVIRONMENT_DIZZY             = 24;
  EAX_ENVIRONMENT_PSYCHOTIC         = 25;
  // total number of environments
  EAX_ENVIRONMENT_COUNT             = 26;
{$ENDIF}

  BASS_STREAMPROC_END = $80000000; // end of user stream flag


  // BASS_StreamCreateFileUser file systems
  STREAMFILE_NOBUFFER     = 0;
  STREAMFILE_BUFFER       = 1;
  STREAMFILE_BUFFERPUSH   = 2;

  // BASS_StreamPutFileData options
  BASS_FILEDATA_END       = 0; // end & close the file

  // BASS_StreamGetFilePosition modes
  BASS_FILEPOS_CURRENT    = 0;
  BASS_FILEPOS_DECODE     = BASS_FILEPOS_CURRENT;
  BASS_FILEPOS_DOWNLOAD   = 1;
  BASS_FILEPOS_END        = 2;
  BASS_FILEPOS_START      = 3;
  BASS_FILEPOS_CONNECTED  = 4;
  BASS_FILEPOS_BUFFER     = 5;

  // BASS_ChannelSetSync types
  BASS_SYNC_POS           = 0;
  BASS_SYNC_END           = 2;
  BASS_SYNC_META          = 4;
  BASS_SYNC_SLIDE         = 5;
  BASS_SYNC_STALL         = 6;
  BASS_SYNC_DOWNLOAD      = 7;
  BASS_SYNC_FREE          = 8;
  BASS_SYNC_SETPOS        = 11;
  BASS_SYNC_MUSICPOS      = 10;
  BASS_SYNC_MUSICINST     = 1;
  BASS_SYNC_MUSICFX       = 3;
  BASS_SYNC_OGG_CHANGE    = 12;
  BASS_SYNC_MIXTIME       = $40000000; // FLAG: sync at mixtime, else at playtime
  BASS_SYNC_ONETIME       = $80000000; // FLAG: sync only once, else continuously

  // BASS_ChannelIsActive return values
  BASS_ACTIVE_STOPPED = 0;
  BASS_ACTIVE_PLAYING = 1;
  BASS_ACTIVE_STALLED = 2;
  BASS_ACTIVE_PAUSED  = 3;

  // Channel attributes
  BASS_ATTRIB_FREQ                  = 1;
  BASS_ATTRIB_VOL                   = 2;
  BASS_ATTRIB_PAN                   = 3;
  BASS_ATTRIB_EAXMIX                = 4;
  BASS_ATTRIB_MUSIC_AMPLIFY         = $100;
  BASS_ATTRIB_MUSIC_PANSEP          = $101;
  BASS_ATTRIB_MUSIC_PSCALER         = $102;
  BASS_ATTRIB_MUSIC_BPM             = $103;
  BASS_ATTRIB_MUSIC_SPEED           = $104;
  BASS_ATTRIB_MUSIC_VOL_GLOBAL      = $105;
  BASS_ATTRIB_MUSIC_VOL_CHAN        = $200; // + channel #
  BASS_ATTRIB_MUSIC_VOL_INST        = $300; // + instrument #

  // BASS_ChannelGetData flags
  BASS_DATA_AVAILABLE = 0;        // query how much data is buffered
  BASS_DATA_FLOAT     = $40000000; // flag: return floating-point sample data
  BASS_DATA_FFT256    = $80000000; // 256 sample FFT
  BASS_DATA_FFT512    = $80000001; // 512 FFT
  BASS_DATA_FFT1024   = $80000002; // 1024 FFT
  BASS_DATA_FFT2048   = $80000003; // 2048 FFT
  BASS_DATA_FFT4096   = $80000004; // 4096 FFT
  BASS_DATA_FFT8192   = $80000005; // 8192 FFT
  BASS_DATA_FFT_INDIVIDUAL = $10; // FFT flag: FFT for each channel, else all combined
  BASS_DATA_FFT_NOWINDOW = $20;   // FFT flag: no Hanning window

  // BASS_ChannelGetTags types : what's returned
  BASS_TAG_ID3        = 0; // ID3v1 tags : TAG_ID3 structure
  BASS_TAG_ID3V2      = 1; // ID3v2 tags : variable length block
  BASS_TAG_OGG        = 2; // OGG comments : series of null-terminated UTF-8 strings
  BASS_TAG_HTTP       = 3; // HTTP headers : series of null-terminated ANSI strings
  BASS_TAG_ICY        = 4; // ICY headers : series of null-terminated ANSI strings
  BASS_TAG_META       = 5; // ICY metadata : ANSI string
  BASS_TAG_VENDOR     = 9; // OGG encoder : UTF-8 string
  BASS_TAG_LYRICS3    = 10; // Lyric3v2 tag : ASCII string
  BASS_TAG_RIFF_INFO  = $100; // RIFF "INFO" tags : series of null-terminated ANSI strings
  BASS_TAG_RIFF_BEXT  = $101; // RIFF/BWF Broadcast Audio Extension tags : TAG_BEXT structure
  BASS_TAG_MUSIC_NAME = $10000;	// MOD music name : ANSI string
  BASS_TAG_MUSIC_MESSAGE = $10001; // MOD message : ANSI string
  BASS_TAG_MUSIC_INST = $10100;	// + instrument #, MOD instrument name : ANSI string
  BASS_TAG_MUSIC_SAMPLE = $10300; // + sample #, MOD sample name : ANSI string

  // BASS_ChannelGetLength/GetPosition/SetPosition modes
  BASS_POS_BYTE           = 0; // byte position
  BASS_POS_MUSIC_ORDER    = 1; // order.row position, MAKELONG(order,row)

  // BASS_RecordSetInput flags
  BASS_INPUT_OFF    = $10000;
  BASS_INPUT_ON     = $20000;

  BASS_INPUT_TYPE_MASK    = $FF000000;
  BASS_INPUT_TYPE_UNDEF   = $00000000;
  BASS_INPUT_TYPE_DIGITAL = $01000000;
  BASS_INPUT_TYPE_LINE    = $02000000;
  BASS_INPUT_TYPE_MIC     = $03000000;
  BASS_INPUT_TYPE_SYNTH   = $04000000;
  BASS_INPUT_TYPE_CD      = $05000000;
  BASS_INPUT_TYPE_PHONE   = $06000000;
  BASS_INPUT_TYPE_SPEAKER = $07000000;
  BASS_INPUT_TYPE_WAVE    = $08000000;
  BASS_INPUT_TYPE_AUX     = $09000000;
  BASS_INPUT_TYPE_ANALOG  = $0A000000;

  BASS_FX_DX8_CHORUS	  = 0;
  BASS_FX_DX8_COMPRESSOR  = 1;
  BASS_FX_DX8_DISTORTION  = 2;
  BASS_FX_DX8_ECHO        = 3;
  BASS_FX_DX8_FLANGER     = 4;
  BASS_FX_DX8_GARGLE      = 5;
  BASS_FX_DX8_I3DL2REVERB = 6;
  BASS_FX_DX8_PARAMEQ     = 7;
  BASS_FX_DX8_REVERB      = 8;

  BASS_DX8_PHASE_NEG_180 = 0;
  BASS_DX8_PHASE_NEG_90  = 1;
  BASS_DX8_PHASE_ZERO    = 2;
  BASS_DX8_PHASE_90      = 3;
  BASS_DX8_PHASE_180     = 4;

type
  DWORD = cardinal;
  BOOL = LongBool;
  FLOAT = Single;
  QWORD = int64;        // 64-bit (replace "int64" with "comp" if using Delphi 3)

  HMUSIC = DWORD;       // MOD music handle
  HSAMPLE = DWORD;      // sample handle
  HCHANNEL = DWORD;     // playing sample's channel handle
  HSTREAM = DWORD;      // sample stream handle
  HRECORD = DWORD;      // recording handle
  HSYNC = DWORD;        // synchronizer handle
  HDSP = DWORD;         // DSP handle
  HFX = DWORD;          // DX8 effect handle
  HPLUGIN = DWORD;      // Plugin handle

  // Device info structure
  BASS_DEVICEINFO = record
    name: PAnsiChar;    // description
    driver: PAnsiChar;  // driver
    flags: DWORD;
  end;

  BASS_INFO = record
    flags: DWORD;       // device capabilities (DSCAPS_xxx flags)
    hwsize: DWORD;      // size of total device hardware memory
    hwfree: DWORD;      // size of free device hardware memory
    freesam: DWORD;     // number of free sample slots in the hardware
    free3d: DWORD;      // number of free 3D sample slots in the hardware
    minrate: DWORD;     // min sample rate supported by the hardware
    maxrate: DWORD;     // max sample rate supported by the hardware
    eax: BOOL;          // device supports EAX? (always FALSE if BASS_DEVICE_3D was not used)
    minbuf: DWORD;      // recommended minimum buffer length in ms (requires BASS_DEVICE_LATENCY)
    dsver: DWORD;       // DirectSound version
    latency: DWORD;     // delay (in ms) before start of playback (requires BASS_DEVICE_LATENCY)
    initflags: DWORD;   // BASS_Init "flags" parameter
    speakers: DWORD;    // number of speakers available
    freq: DWORD;        // current output rate (OSX only)
  end;

  // Recording device info structure
  BASS_RECORDINFO = record
    flags: DWORD;       // device capabilities (DSCCAPS_xxx flags)
    formats: DWORD;     // supported standard formats (WAVE_FORMAT_xxx flags)
    inputs: DWORD;      // number of inputs
    singlein: BOOL;     // only 1 input can be set at a time
    {$IFNDEF BASS_242}
    driver: PChar;      // driver
    {$ENDIF}
    freq: DWORD;        // current input rate (OSX only)
  end;

  // Sample info structure
  BASS_SAMPLE = record
    freq: DWORD;        // default playback rate
    volume: FLOAT;      // default volume (0-100)
    pan: FLOAT;         // default pan (-100=left, 0=middle, 100=right)
    flags: DWORD;       // BASS_SAMPLE_xxx flags
    length: DWORD;      // length (in samples, not bytes)
    max: DWORD;         // maximum simultaneous playbacks
    origres: DWORD;     // original resolution
    chans: DWORD;       // number of channels
    mingap: DWORD;      // minimum gap (ms) between creating channels
    mode3d: DWORD;      // BASS_3DMODE_xxx mode
    mindist: FLOAT;     // minimum distance
    maxdist: FLOAT;     // maximum distance
    iangle: DWORD;      // angle of inside projection cone
    oangle: DWORD;      // angle of outside projection cone
    outvol: FLOAT;      // delta-volume outside the projection cone
    vam: DWORD;         // voice allocation/management flags (BASS_VAM_xxx)
    priority: DWORD;    // priority (0=lowest, $ffffffff=highest)
  end;

  // Channel info structure
  BASS_CHANNELINFO = record
    freq: DWORD;        // default playback rate
    chans: DWORD;       // channels
    flags: DWORD;       // BASS_SAMPLE/STREAM/MUSIC/SPEAKER flags
    ctype: DWORD;       // type of channel
    origres: DWORD;     // original resolution
    plugin: HPLUGIN;    // plugin
    sample: HSAMPLE;    // sample
    filename: PAnsiChar; // filename
  end;

  BASS_PLUGINFORM = record
    ctype: DWORD;       // channel type
    name: PAnsiChar;    // format description
    exts: PAnsiChar;    // file extension filter (*.ext1;*.ext2;etc...)
  end;
  PBASS_PLUGINFORMS = ^TBASS_PLUGINFORMS;
  TBASS_PLUGINFORMS = array[0..maxInt div sizeOf(BASS_PLUGINFORM) - 1] of BASS_PLUGINFORM;

  BASS_PLUGININFO = record
    version: DWORD;             // version (same form as BASS_GetVersion)
    formatc: DWORD;             // number of formats
    formats: PBASS_PLUGINFORMS; // the array of formats
  end;
  PBASS_PLUGININFO = ^BASS_PLUGININFO;

  // 3D vector (for 3D positions/velocities/orientations)
  BASS_3DVECTOR = record
    x: FLOAT;           // +=right, -=left
    y: FLOAT;           // +=up, -=down
    z: FLOAT;           // +=front, -=behind
  end;

  // User file stream callback functions
  FILECLOSEPROC = procedure(user: Pointer); {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}
  FILELENPROC = function(user: Pointer): QWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}
  FILEREADPROC = function(buffer: Pointer; length: DWORD; user: Pointer): DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}
  FILESEEKPROC = function(offset: QWORD; user: Pointer): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}

  BASS_FILEPROCS = record
    close: FILECLOSEPROC;
    length: FILELENPROC;
    read: FILEREADPROC;
    seek: FILESEEKPROC;
  end;

  // ID3v1 tag structure
  TAG_ID3 = record
    id: Array[0..2] of AnsiChar;
    title: Array[0..29] of AnsiChar;
    artist: Array[0..29] of AnsiChar;
    album: Array[0..29] of AnsiChar;
    year: Array[0..3] of AnsiChar;
    comment: Array[0..29] of AnsiChar;
    genre: Byte;
  end;

  // BWF Broadcast Audio Extension tag structure
  TAG_BEXT = record
    Description: Array[0..255] of AnsiChar;     // description
    Originator: Array[0..31] of AnsiChar;       // name of the originator
    OriginatorReference: Array[0..31] of AnsiChar; // reference of the originator
    OriginationDate: Array[0..9] of AnsiChar;   // date of creation (yyyy-mm-dd)
    OriginationTime: Array[0..7] of AnsiChar;   // time of creation (hh-mm-ss)
    TimeReference: QWORD;                       // first sample count since midnight (little-endian)
    Version: Word;                              // BWF version (little-endian)
    UMID: Array[0..63] of Byte;                 // SMPTE UMID
    Reserved: Array[0..189] of Byte;
    CodingHistory: Array of AnsiChar;           // history
  end;

  BASS_DX8_CHORUS = record
    fWetDryMix: FLOAT;
    fDepth: FLOAT;
    fFeedback: FLOAT;
    fFrequency: FLOAT;
    lWaveform: DWORD;   // 0=triangle, 1=sine
    fDelay: FLOAT;
    lPhase: DWORD;      // BASS_DX8_PHASE_xxx
  end;

  BASS_DX8_COMPRESSOR = record
    fGain: FLOAT;
    fAttack: FLOAT;
    fRelease: FLOAT;
    fThreshold: FLOAT;
    fRatio: FLOAT;
    fPredelay: FLOAT;
  end;

  BASS_DX8_DISTORTION = record
    fGain: FLOAT;
    fEdge: FLOAT;
    fPostEQCenterFrequency: FLOAT;
    fPostEQBandwidth: FLOAT;
    fPreLowpassCutoff: FLOAT;
  end;

  BASS_DX8_ECHO = record
    fWetDryMix: FLOAT;
    fFeedback: FLOAT;
    fLeftDelay: FLOAT;
    fRightDelay: FLOAT;
    lPanDelay: BOOL;
  end;

  BASS_DX8_FLANGER = record
    fWetDryMix: FLOAT;
    fDepth: FLOAT;
    fFeedback: FLOAT;
    fFrequency: FLOAT;
    lWaveform: DWORD;   // 0=triangle, 1=sine
    fDelay: FLOAT;
    lPhase: DWORD;      // BASS_DX8_PHASE_xxx
  end;

  BASS_DX8_GARGLE = record
    dwRateHz: DWORD;               // Rate of modulation in hz
    dwWaveShape: DWORD;            // 0=triangle, 1=square
  end;

  BASS_DX8_I3DL2REVERB = record
    lRoom: Longint;                // [-10000, 0]      default: -1000 mB
    lRoomHF: Longint;              // [-10000, 0]      default: 0 mB
    flRoomRolloffFactor: FLOAT;    // [0.0, 10.0]      default: 0.0
    flDecayTime: FLOAT;            // [0.1, 20.0]      default: 1.49s
    flDecayHFRatio: FLOAT;         // [0.1, 2.0]       default: 0.83
    lReflections: Longint;         // [-10000, 1000]   default: -2602 mB
    flReflectionsDelay: FLOAT;     // [0.0, 0.3]       default: 0.007 s
    lReverb: Longint;              // [-10000, 2000]   default: 200 mB
    flReverbDelay: FLOAT;          // [0.0, 0.1]       default: 0.011 s
    flDiffusion: FLOAT;            // [0.0, 100.0]     default: 100.0 %
    flDensity: FLOAT;              // [0.0, 100.0]     default: 100.0 %
    flHFReference: FLOAT;          // [20.0, 20000.0]  default: 5000.0 Hz
  end;

  BASS_DX8_PARAMEQ = record
    fCenter: FLOAT;
    fBandwidth: FLOAT;
    fGain: FLOAT;
  end;

  BASS_DX8_REVERB = record
    fInGain: FLOAT;                // [-96.0,0.0]            default: 0.0 dB
    fReverbMix: FLOAT;             // [-96.0,0.0]            default: 0.0 db
    fReverbTime: FLOAT;            // [0.001,3000.0]         default: 1000.0 ms
    fHighFreqRTRatio: FLOAT;       // [0.001,0.999]          default: 0.001
  end;

  // callback function types
  STREAMPROC = function(handle: HSTREAM; buffer: Pointer; length: DWORD; user: Pointer): DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}
  {
    User stream callback function. NOTE: A stream function should obviously be as
    quick as possible, other streams (and MOD musics) can't be mixed until
    it's finished.
    handle : The stream that needs writing
    buffer : Buffer to write the samples in
    length : Number of bytes to write
    user   : The 'user' parameter value given when calling BASS_StreamCreate
    RETURN : Number of bytes written. Set the BASS_STREAMPROC_END flag to end
             the stream.
  }

const
  // special STREAMPROCs
  STREAMPROC_DUMMY {: STREAMPROC} = Pointer(0);  // "dummy" stream
  STREAMPROC_PUSH  {: STREAMPROC} = Pointer(-1); // push stream

type

  DOWNLOADPROC = procedure(buffer: Pointer; length: DWORD; user: Pointer); {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}
  {
    Internet stream download callback function.
    buffer : Buffer containing the downloaded data... NULL=end of download
    length : Number of bytes in the buffer
    user   : The 'user' parameter value given when calling BASS_StreamCreateURL
  }

  SYNCPROC = procedure(handle: HSYNC; channel, data: DWORD; user: Pointer); {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}
  {
    Sync callback function. NOTE: a sync callback function should be very
    quick as other syncs cannot be processed until it has finished. If the
    sync is a "mixtime" sync, then other streams and MOD musics can not be
    mixed until it's finished either.
    handle : The sync that has occured
    channel: Channel that the sync occured in
    data   : Additional data associated with the sync's occurance
    user   : The 'user' parameter given when calling BASS_ChannelSetSync
  }

  DSPPROC = procedure(handle: HDSP; channel: DWORD; buffer: Pointer; length: DWORD; user: Pointer); {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}
  {
    DSP callback function. NOTE: A DSP function should obviously be as quick
    as possible... other DSP functions, streams and MOD musics can not be
    processed until it's finished.
    handle : The DSP handle
    channel: Channel that the DSP is being applied to
    buffer : Buffer to apply the DSP to
    length : Number of bytes in the buffer
    user   : The 'user' parameter given when calling BASS_ChannelSetDSP
  }

  RECORDPROC = function(handle: HRECORD; buffer: Pointer; length: DWORD; user: Pointer): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}
  {
    Recording callback function.
    handle : The recording handle
    buffer : Buffer containing the recorded sample data
    length : Number of bytes
    user   : The 'user' parameter value given when calling BASS_RecordStart
    RETURN : TRUE = continue recording, FALSE = stop
  }


// Functions
const
{$IFDEF MSWINDOWS}
  bassdll = 'bass.dll';
{$ENDIF}
{$IFDEF DARWIN}
  bassdll = 'libbass.dylib';
  {$linklib libbass}
{$ENDIF}

function BASS_SetConfig(option, value: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_GetConfig(option: DWORD): DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_SetConfigPtr(option: DWORD; value: Pointer): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_GetConfigPtr(option: DWORD): Pointer; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_GetVersion: DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ErrorGetCode: Integer; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_GetDeviceInfo(device: DWORD; var info: BASS_DEVICEINFO): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
{$IFDEF MSWINDOWS}
function BASS_Init(device: Integer; freq, flags: DWORD; win: HWND; clsid: PGUID): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
{$ELSE}
function BASS_Init(device: Integer; freq, flags: DWORD; win: Pointer; clsid: Pointer): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
{$ENDIF}
function BASS_SetDevice(device: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_GetDevice: DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_Free: BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
{$IFDEF MSWINDOWS}
function BASS_GetDSoundObject(obj: DWORD): Pointer; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
{$ENDIF}
function BASS_GetInfo(var info: BASS_INFO): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_Update(length: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_GetCPU: FLOAT; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_Start: BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_Stop: BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_Pause: BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_SetVolume(volume: FLOAT): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_GetVolume: FLOAT; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;

function BASS_PluginLoad(filename: PAnsiChar; flags: DWORD): HPLUGIN; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_PluginFree(handle: HPLUGIN): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_PluginGetInfo(handle: HPLUGIN): PBASS_PLUGININFO; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;

function BASS_Set3DFactors(distf, rollf, doppf: FLOAT): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_Get3DFactors(var distf, rollf, doppf: FLOAT): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_Set3DPosition(var pos, vel, front, top: BASS_3DVECTOR): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_Get3DPosition(var pos, vel, front, top: BASS_3DVECTOR): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
procedure BASS_Apply3D; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
{$IFDEF MSWINDOWS}
function BASS_SetEAXParameters(env: Integer; vol, decay, damp: FLOAT): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_GetEAXParameters(var env: DWORD; var vol, decay, damp: FLOAT): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
{$ENDIF}

function BASS_MusicLoad(mem: BOOL; f: Pointer; offset: QWORD; length, flags, freq: DWORD): HMUSIC; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_MusicFree(handle: HMUSIC): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;

function BASS_SampleLoad(mem: BOOL; f: Pointer; offset: QWORD; length, max, flags: DWORD): HSAMPLE; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_SampleCreate(length, freq, chans, max, flags: DWORD): HSAMPLE; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_SampleFree(handle: HSAMPLE): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_SampleSetData(handle: HSAMPLE; buffer: Pointer): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_SampleGetData(handle: HSAMPLE; buffer: Pointer): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_SampleGetInfo(handle: HSAMPLE; var info: BASS_SAMPLE): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_SampleSetInfo(handle: HSAMPLE; var info: BASS_SAMPLE): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_SampleGetChannel(handle: HSAMPLE; onlynew: BOOL): HCHANNEL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_SampleGetChannels(handle: HSAMPLE; channels: Pointer): DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_SampleStop(handle: HSAMPLE): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;

function BASS_StreamCreate(freq, chans, flags: DWORD; proc: STREAMPROC; user: Pointer): HSTREAM; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_StreamCreateFile(mem: BOOL; f: Pointer; offset, length: QWORD; flags: DWORD): HSTREAM; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_StreamCreateURL(url: PAnsiChar; offset: DWORD; flags: DWORD; proc: DOWNLOADPROC; user: Pointer):HSTREAM; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_StreamCreateFileUser(system, flags: DWORD; var procs: BASS_FILEPROCS; user: Pointer): HSTREAM; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_StreamFree(handle: HSTREAM): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_StreamGetFilePosition(handle: HSTREAM; mode: DWORD): QWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_StreamPutData(handle: HSTREAM; buffer: Pointer; length: DWORD): DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_StreamPutFileData(handle: HSTREAM; buffer: Pointer; length: DWORD): DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;

function BASS_RecordGetDeviceInfo(device: DWORD; var info: BASS_DEVICEINFO): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_RecordInit(device: Integer):BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_RecordSetDevice(device: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_RecordGetDevice: DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_RecordFree: BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_RecordGetInfo(var info: BASS_RECORDINFO): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_RecordGetInputName(input: Integer): PAnsiChar; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_RecordSetInput(input: Integer; flags: DWORD; volume: FLOAT): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_RecordGetInput(input: Integer; var volume: FLOAT): DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_RecordStart(freq, chans, flags: DWORD; proc: RECORDPROC; user: Pointer): HRECORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;

function BASS_ChannelBytes2Seconds(handle: DWORD; pos: QWORD): Double; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}external bassdll;
function BASS_ChannelSeconds2Bytes(handle: DWORD; pos: Double): QWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}external bassdll;
function BASS_ChannelGetDevice(handle: DWORD): DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelSetDevice(handle, device: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelIsActive(handle: DWORD): DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}external bassdll;
function BASS_ChannelGetInfo(handle: DWORD; var info: BASS_CHANNELINFO):BOOL;{$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}external bassdll;
function BASS_ChannelGetTags(handle: HSTREAM; tags: DWORD): PAnsiChar; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelFlags(handle, flags, mask: DWORD): DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelUpdate(handle, length: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelLock(handle: DWORD; lock: BOOL): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelPlay(handle: DWORD; restart: BOOL): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelStop(handle: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelPause(handle: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelSetAttribute(handle, attrib: DWORD; value: FLOAT): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelGetAttribute(handle, attrib: DWORD; var value: FLOAT): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelSlideAttribute(handle, attrib: DWORD; value: FLOAT; time: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelIsSliding(handle, attrib: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF}external bassdll;
function BASS_ChannelSet3DAttributes(handle: DWORD; mode: Integer; min, max: FLOAT; iangle, oangle, outvol: Integer): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelGet3DAttributes(handle: DWORD; var mode: DWORD; var min, max: FLOAT; var iangle, oangle, outvol: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelSet3DPosition(handle: DWORD; var pos, orient, vel: BASS_3DVECTOR): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelGet3DPosition(handle: DWORD; var pos, orient, vel: BASS_3DVECTOR): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelGetLength(handle, mode: DWORD): QWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelSetPosition(handle: DWORD; pos: QWORD; mode: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelGetPosition(handle, mode: DWORD): QWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelGetLevel(handle: DWORD): DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelGetData(handle: DWORD; buffer: Pointer; length: DWORD): DWORD; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelSetSync(handle: DWORD; type_: DWORD; param: QWORD; proc: SYNCPROC; user: Pointer): HSYNC; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelRemoveSync(handle: DWORD; sync: HSYNC): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelSetDSP(handle: DWORD; proc: DSPPROC; user: Pointer; priority: Integer): HDSP; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelRemoveDSP(handle: DWORD; dsp: HDSP): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelSetLink(handle, chan: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelRemoveLink(handle, chan: DWORD): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelSetFX(handle, type_: DWORD; priority: Integer): HFX; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_ChannelRemoveFX(handle: DWORD; fx: HFX): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;

function BASS_FXSetParameters(handle: HFX; par: Pointer): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_FXGetParameters(handle: HFX; par: Pointer): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;
function BASS_FXReset(handle: HFX): BOOL; {$IFDEF DLL_STDCALL}stdcall;{$ENDIF}{$IFDEF DLL_CDECL}cdecl;{$ENDIF} external bassdll;


function BASS_SPEAKER_N(n: DWORD): DWORD;
{$IFDEF MSWINDOWS}
function BASS_SetEAXPreset(env: Integer): BOOL;
{
  This function is defined in the implementation part of this unit.
  It is not part of BASS.DLL but an extra function which makes it easier
  to set the predefined EAX environments.
  env    : a EAX_ENVIRONMENT_xxx constant
}
{$ENDIF}

implementation

function BASS_SPEAKER_N(n: DWORD): DWORD;
begin
  Result := n shl 24;
end;

{$IFDEF MSWINDOWS}
function BASS_SetEAXPreset(env: Integer): BOOL;
begin
  case (env) of
    EAX_ENVIRONMENT_GENERIC:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_GENERIC, 0.5, 1.493, 0.5);
    EAX_ENVIRONMENT_PADDEDCELL:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PADDEDCELL, 0.25, 0.1, 0);
    EAX_ENVIRONMENT_ROOM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_ROOM, 0.417, 0.4, 0.666);
    EAX_ENVIRONMENT_BATHROOM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_BATHROOM, 0.653, 1.499, 0.166);
    EAX_ENVIRONMENT_LIVINGROOM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_LIVINGROOM, 0.208, 0.478, 0);
    EAX_ENVIRONMENT_STONEROOM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_STONEROOM, 0.5, 2.309, 0.888);
    EAX_ENVIRONMENT_AUDITORIUM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_AUDITORIUM, 0.403, 4.279, 0.5);
    EAX_ENVIRONMENT_CONCERTHALL:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CONCERTHALL, 0.5, 3.961, 0.5);
    EAX_ENVIRONMENT_CAVE:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CAVE, 0.5, 2.886, 1.304);
    EAX_ENVIRONMENT_ARENA:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_ARENA, 0.361, 7.284, 0.332);
    EAX_ENVIRONMENT_HANGAR:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_HANGAR, 0.5, 10.0, 0.3);
    EAX_ENVIRONMENT_CARPETEDHALLWAY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CARPETEDHALLWAY, 0.153, 0.259, 2.0);
    EAX_ENVIRONMENT_HALLWAY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_HALLWAY, 0.361, 1.493, 0);
    EAX_ENVIRONMENT_STONECORRIDOR:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_STONECORRIDOR, 0.444, 2.697, 0.638);
    EAX_ENVIRONMENT_ALLEY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_ALLEY, 0.25, 1.752, 0.776);
    EAX_ENVIRONMENT_FOREST:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_FOREST, 0.111, 3.145, 0.472);
    EAX_ENVIRONMENT_CITY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CITY, 0.111, 2.767, 0.224);
    EAX_ENVIRONMENT_MOUNTAINS:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_MOUNTAINS, 0.194, 7.841, 0.472);
    EAX_ENVIRONMENT_QUARRY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_QUARRY, 1, 1.499, 0.5);
    EAX_ENVIRONMENT_PLAIN:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PLAIN, 0.097, 2.767, 0.224);
    EAX_ENVIRONMENT_PARKINGLOT:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PARKINGLOT, 0.208, 1.652, 1.5);
    EAX_ENVIRONMENT_SEWERPIPE:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_SEWERPIPE, 0.652, 2.886, 0.25);
    EAX_ENVIRONMENT_UNDERWATER:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_UNDERWATER, 1, 1.499, 0);
    EAX_ENVIRONMENT_DRUGGED:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_DRUGGED, 0.875, 8.392, 1.388);
    EAX_ENVIRONMENT_DIZZY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_DIZZY, 0.139, 17.234, 0.666);
    EAX_ENVIRONMENT_PSYCHOTIC:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PSYCHOTIC, 0.486, 7.563, 0.806);
    else
      Result := FALSE;
  end;
end;
{$ENDIF}

end.
// END OF FILE /////////////////////////////////////////////////////////////////

