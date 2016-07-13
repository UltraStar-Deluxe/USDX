{=============================================================================
  BASS_FX 2.4 - Copyright (c) 2002-2014 (: JOBnik! :) [Arthur Aminov, ISRAEL]
                                                      [http://www.jobnik.org]

         bugs/suggestions/questions:
           forum  : http://www.un4seen.com/forum/?board=1
                    http://www.jobnik.org/smforum
           e-mail : bass_fx@jobnik.org
        --------------------------------------------------

  BASS_FX unit is based on BASS_FX 1.1 unit:
  ------------------------------------------
  (c) 2002 Roger Johansson. w1dg3r@yahoo.com

  NOTE: This unit will work only with BASS_FX version 2.4.11.1
        Check www.un4seen.com or www.jobnik.org for any later versions.

  * Requires BASS 2.4 (available at http://www.un4seen.com)

  How to install:
  ---------------
  Copy BASS_FX.PAS & BASS.PAS to the \LIB subdirectory of your Delphi path
  or your project dir.
=============================================================================}

unit BASS_FX;

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$PACKRECORDS C}
{$ENDIF}

uses Bass, Math;    // Math is used for BASS_BFX_Linear2dB/dB2Linear functions below

const
	// BASS_CHANNELINFO types
	BASS_CTYPE_STREAM_TEMPO = $1f200;
	BASS_CTYPE_STREAM_REVERSE = $1f201;

	// Tempo / Reverse / BPM / Beat flag
	BASS_FX_FREESOURCE = $10000;		// Free the source handle as well?

	{$IFDEF MSWINDOWS}
		bass_fxdll = 'bass_fx.dll';
	{$ELSE}
		bass_fxdll = 'libbass_fx.so';
	{$ENDIF}

// BASS_FX Version
function BASS_FX_GetVersion(): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;

{=============================================================================
	DSP (Digital Signal Processing)
=============================================================================}

{
	Multi-channel order of each channel is as follows:
	 3 channels       left-front, right-front, center.
	 4 channels       left-front, right-front, left-rear/side, right-rear/side.
	 5 channels       left-front, right-front, center, left-rear/side, right-rear/side.
	 6 channels (5.1) left-front, right-front, center, LFE, left-rear/side, right-rear/side.
	 8 channels (7.1) left-front, right-front, center, LFE, left-rear/side, right-rear/side, left-rear center, right-rear center.
}

const
	// DSP channels flags
	BASS_BFX_CHANALL  = -1;				// all channels at once (as by default)
	BASS_BFX_CHANNONE = 0;				// disable an effect for all channels
	BASS_BFX_CHAN1    = 1;				// left-front channel
	BASS_BFX_CHAN2    = 2;				// right-front channel
	BASS_BFX_CHAN3    = 4;				// see above info
	BASS_BFX_CHAN4    = 8;				// see above info
	BASS_BFX_CHAN5    = 16;				// see above info
	BASS_BFX_CHAN6    = 32;				// see above info
	BASS_BFX_CHAN7    = 64;				// see above info
	BASS_BFX_CHAN8    = 128;			// see above info

// if you have more than 8 channels (7.1), use this function below
function BASS_BFX_CHANNEL_N(n: DWORD): DWORD;

// DSP effects
const
	BASS_FX_BFX_ROTATE      = $10000;    // A channels volume ping-pong		/ multi channel
	BASS_FX_BFX_ECHO        = $10001;    // Echo							/ 2 channels max	(deprecated)
	BASS_FX_BFX_FLANGER     = $10002;    // Flanger							/ multi channel		(deprecated)
	BASS_FX_BFX_VOLUME      = $10003;    // Volume							/ multi channel
	BASS_FX_BFX_PEAKEQ      = $10004;    // Peaking Equalizer				/ multi channel
	BASS_FX_BFX_REVERB      = $10005;    // Reverb							/ 2 channels max	(deprecated)
	BASS_FX_BFX_LPF         = $10006;    // Low Pass Filter 24dB			/ multi channel		(deprecated)
	BASS_FX_BFX_MIX         = $10007;    // Swap, remap and mix channels	/ multi channel
	BASS_FX_BFX_DAMP        = $10008;    // Dynamic Amplification			/ multi channel
	BASS_FX_BFX_AUTOWAH     = $10009;    // Auto Wah						/ multi channel
	BASS_FX_BFX_ECHO2       = $1000A;    // Echo 2							/ multi channel		(deprecated)
	BASS_FX_BFX_PHASER      = $1000B;    // Phaser							/ multi channel
	BASS_FX_BFX_ECHO3       = $1000C;    // Echo 3							/ multi channel		(deprecated)
	BASS_FX_BFX_CHORUS      = $1000D;    // Chorus/Flanger					/ multi channel
	BASS_FX_BFX_APF         = $1000E;    // All Pass Filter					/ multi channel		(deprecated)
	BASS_FX_BFX_COMPRESSOR  = $1000F;    // Compressor						/ multi channel		(deprecated)
	BASS_FX_BFX_DISTORTION  = $10010;    // Distortion						/ multi channel
	BASS_FX_BFX_COMPRESSOR2 = $10011;    // Compressor 2					/ multi channel
	BASS_FX_BFX_VOLUME_ENV  = $10012;    // Volume envelope					/ multi channel
	BASS_FX_BFX_BQF         = $10013;    // BiQuad filters					/ multi channel
	BASS_FX_BFX_ECHO4       = $10014;    // Echo 4							/ multi channel
	BASS_FX_BFX_PITCHSHIFT	= $10015;    // Pitch shift using FFT			/ multi channel		(not available on mobile)
	BASS_FX_BFX_FREEVERB	= $10016;    // Reverb using "Freeverb" algo	/ multi channel

{
    Deprecated effects in 2.4.10 version:
	------------------------------------
	BASS_FX_BFX_ECHO		-> use BASS_FX_BFX_ECHO4
	BASS_FX_BFX_ECHO2		-> use BASS_FX_BFX_ECHO4
	BASS_FX_BFX_ECHO3		-> use BASS_FX_BFX_ECHO4
	BASS_FX_BFX_REVERB		-> use BASS_FX_BFX_FREEVERB
	BASS_FX_BFX_FLANGER		-> use BASS_FX_BFX_CHORUS
	BASS_FX_BFX_COMPRESSOR	-> use BASS_FX_BFX_COMPRESSOR2
	BASS_FX_BFX_APF			-> use BASS_FX_BFX_BQF with BASS_BFX_BQF_ALLPASS filter
	BASS_FX_BFX_LPF			-> use 2x BASS_FX_BFX_BQF with BASS_BFX_BQF_LOWPASS filter and appropriate fQ values
}

const
	// BiQuad Filters
	BASS_BFX_BQF_LOWPASS    = 0;
	BASS_BFX_BQF_HIGHPASS   = 1;
	BASS_BFX_BQF_BANDPASS   = 2;	// constant 0 dB peak gain
	BASS_BFX_BQF_BANDPASS_Q = 3; 	// constant skirt gain, peak gain = Q
	BASS_BFX_BQF_NOTCH      = 4;
	BASS_BFX_BQF_ALLPASS    = 5;
	BASS_BFX_BQF_PEAKINGEQ  = 6;
	BASS_BFX_BQF_LOWSHELF   = 7;
	BASS_BFX_BQF_HIGHSHELF  = 8;

	// Freeverb
	BASS_BFX_FREEVERB_MODE_FREEZE = 1;

type
	// Rotate
	BASS_BFX_ROTATE = record
		fRate: Single;					// rotation rate/speed in Hz (A negative rate can be used for reverse direction)
		lChannel: Integer;				// BASS_BFX_CHANxxx flag/s (supported only even number of channels)
	end;

	// Echo (deprecated)
	BASS_BFX_ECHO = record
		fLevel: Single;					// [0....1....n] linear
		lDelay: Integer;              	// [1200..30000]
	end;

	// Flanger (deprecated)
	BASS_BFX_FLANGER = record
		fWetDry: Single;    	           	// [0....1....n] linear
		fSpeed: Single;                	// [0......0.09]
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Volume
	BASS_BFX_VOLUME = record
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s or 0 for global volume control
		fVolume: Single;               	// [0....1....n] linear
	end;

	// Peaking Equalizer
	BASS_BFX_PEAKEQ = record
		lBand: Integer;               	// [0...............n] more bands means more memory & cpu usage
		fBandwidth: Single;            	// [0.1...........<10] in octaves - fQ is not in use (Bandwidth has a priority over fQ)
		fQ: Single;                    	// [0...............1] the EE kinda definition (linear) (if Bandwidth is not in use)
		fCenter: Single;               	// [1Hz..<info.freq/2] in Hz
		fGain: Single;                 	// [-15dB...0...+15dB] in dB (can be above/below these limits)
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Reverb (deprecated)
	BASS_BFX_REVERB = record
		fLevel: Single;                	// [0....1....n] linear
		lDelay: Integer;              	// [1200..10000]
	end;

	// Low Pass Filter (deprecated)
	BASS_BFX_LPF = record
		fResonance: Single;            	// [0.01............10]
		fCutOffFreq: Single;           	// [1Hz....info.freq/2] cutoff frequency
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Swap, remap and mix
	PTlChannel = ^TlChannel;
	TlChannel = array[0..maxInt div sizeOf(DWORD) - 1] of DWORD;
	BASS_BFX_MIX = record
		lChannel: PTlChannel;         	// a pointer to an array of channels to mix using BASS_BFX_CHANxxx flag/s (lChannel[0] is left channel...)
	end;

	// Dynamic Amplification
	BASS_BFX_DAMP = record
		fTarget: Single;               	// target volume level                      [0<......1] linear
		fQuiet: Single;                	// quiet  volume level                      [0.......1] linear
		fRate: Single;                 	// amp adjustment rate                      [0.......1] linear
		fGain: Single;                 	// amplification level                      [0...1...n] linear
		fDelay: Single;                	// delay in seconds before increasing level [0.......n] linear
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Auto Wah
	BASS_BFX_AUTOWAH = record
		fDryMix: Single;               	// dry (unaffected) signal mix              [-2......2]
		fWetMix: Single;               	// wet (affected) signal mix                [-2......2]
		fFeedback: Single;             	// output signal to feed back into input	[-1......1]
		fRate: Single;                 	// rate of sweep in cycles per second       [0<....<10]
		fRange: Single;                	// sweep range in octaves                   [0<....<10]
		fFreq: Single;                 	// base frequency of sweep Hz               [0<...1000]
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Echo 2 (deprecated)
	BASS_BFX_ECHO2 = record
		fDryMix: Single;               	// dry (unaffected) signal mix              [-2......2]
		fWetMix: Single;               	// wet (affected) signal mix                [-2......2]
		fFeedback: Single;             	// output signal to feed back into input	[-1......1]
		fDelay: Single;                	// delay sec                                [0<......n]
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Phaser
	BASS_BFX_PHASER = record
		fDryMix: Single;               	// dry (unaffected) signal mix              [-2......2]
		fWetMix: Single;               	// wet (affected) signal mix                [-2......2]
		fFeedback: Single;             	// output signal to feed back into input	[-1......1]
		fRate: Single;                 	// rate of sweep in cycles per second       [0<....<10]
		fRange: Single;                	// sweep range in octaves                   [0<....<10]
		fFreq: Single;                 	// base frequency of sweep                  [0<...1000]
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Echo 3 (deprecated)
	BASS_BFX_ECHO3 = record
		fDryMix: Single;               	// dry (unaffected) signal mix              [-2......2]
		fWetMix: Single;               	// wet (affected) signal mix                [-2......2]
		fDelay: Single;                	// delay sec                                [0<......n]
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Chorus/Flanger
	BASS_BFX_CHORUS = record
		fDryMix: Single;               	// dry (unaffected) signal mix              [-2......2]
		fWetMix: Single;               	// wet (affected) signal mix                [-2......2]
		fFeedback: Single;             	// output signal to feed back into input	[-1......1]
		fMinSweep: Single;             	// minimal delay ms                         [0<...6000]
		fMaxSweep: Single;             	// maximum delay ms                         [0<...6000]
		fRate: Single;                 	// rate ms/s                                [0<...1000]
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// All Pass Filter (deprecated)
	BASS_BFX_APF = record
		fGain: Single;                 	// reverberation time                       [-1=<..<=1]
		fDelay: Single;                	// delay sec                                [0<....<=n]
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Compressor (deprecated)
	BASS_BFX_COMPRESSOR = record
		fThreshold: Single;            	// compressor threshold                     [0<=...<=1]
		fAttacktime: Single;           	// attack time ms                           [0<.<=1000]
		fReleasetime: Single;          	// release time ms                          [0<.<=5000]
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Distortion
	BASS_BFX_DISTORTION = record
		fDrive: Single;                	// distortion drive                         [0<=...<=5]
		fDryMix: Single;               	// dry (unaffected) signal mix              [-5<=..<=5]
		fWetMix: Single;               	// wet (affected) signal mix                [-5<=..<=5]
		fFeedback: Single;             	// output signal to feed back into input	[-1<=..<=1]
		fVolume: Single;               	// distortion volume                        [0=<...<=2]
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Compressor 2
	BASS_BFX_COMPRESSOR2 = record
		fGain: Single;                 	// output gain of signal after compression  [-60....60] in dB
		fThreshold: Single;            	// point at which compression begins        [-60.....0] in dB
		fRatio: Single;                	// compression ratio                        [1.......n]
		fAttack: Single;               	// attack time in ms                        [0.01.1000]
		fRelease: Single;              	// release time in ms                       [0.01.5000]
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Volume envelope
	BASS_BFX_ENV_NODE = packed record
		pos: Double;                  	// node position in seconds (1st envelope node must be at position 0)
		val: Single;                   	// node value
	end;
	PBASS_BFX_ENV_NODES = ^TBASS_BFX_ENV_NODES;
	TBASS_BFX_ENV_NODES = array[0..maxInt div sizeOf(BASS_BFX_ENV_NODE) - 1] of BASS_BFX_ENV_NODE;

	BASS_BFX_VOLUME_ENV = record
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
		lNodeCount: Integer;          	// number of nodes
		pNodes: PBASS_BFX_ENV_NODES;  	// the nodes
		bFollow: BOOL;                	// follow source position
	end;

	// BiQuad Filters
	BASS_BFX_BQF = record 
		lFilter: Integer;             	// BASS_BFX_BQF_xxx filter types
		fCenter: Single;               	// [1Hz..<info.freq/2] Cutoff (central) frequency in Hz
		fGain: Single;                 	// [-15dB...0...+15dB] Used only for PEAKINGEQ and Shelving filters in dB (can be above/below these limits)
		fBandwidth: Single;            	// [0.1...........<10] Bandwidth in octaves (fQ is not in use (fBandwidth has a priority over fQ))
										//                     (between -3 dB frequencies for BANDPASS and NOTCH or between midpoint
										//                     (fGgain/2) gain frequencies for PEAKINGEQ)
		fQ: Single;                    	// [0.1.............1] The EE kinda definition (linear) (if fBandwidth is not in use)
		fS: Single;                    	// [0.1.............1] A "shelf slope" parameter (linear) (used only with Shelving filters)
										//                     when fS = 1, the shelf slope is as steep as you can get it and remain monotonically
										//                     increasing or decreasing gain with frequency.
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Echo 4
	BASS_BFX_ECHO4 = record
		fDryMix: Single;               	// dry (unaffected) signal mix              [-2.......2]
		fWetMix: Single;               	// wet (affected) signal mix                [-2.......2]
		fFeedback: Single;             	// output signal to feed back into input	[-1.......1]
		fDelay: Single;                	// delay sec                                [0<.......n]
		bStereo: BOOL;					// echo adjoining channels to each other	[TRUE/FALSE]
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Pitch shift (not available on mobile)
	BASS_BFX_PITCHSHIFT = record
		fPitchShift: Single;            // A factor value which is between 0.5 (one octave down) and 2 (one octave up) (1 won't change the pitch) [1 default]
										// (fSemitones is not in use, fPitchShift has a priority over fSemitones)
		fSemitones: Single;             // Semitones (0 won't change the pitch) [0 default]
		lFFTsize: Integer;             	// Defines the FFT frame size used for the processing. Typical values are 1024, 2048 and 4096 [2048 default]
										// It may be any value <= 8192 but it MUST be a power of 2
		lOsamp: Integer;                // Is the STFT oversampling factor which also determines the overlap between adjacent STFT frames [8 default]
										// It should at least be 4 for moderate scaling ratios. A value of 32 is recommended for best quality (better quality = higher CPU usage)
		lChannel: Integer;            	// BASS_BFX_CHANxxx flag/s
	end;

	// Freeverb
	BASS_BFX_FREEVERB = record
		fDryMix: Single;                // dry (unaffected) signal mix				[0........1], def. 0
		fWetMix: Single;                // wet (affected) signal mix				[0........3], def. 1.0f
		fRoomSize: Single;              // room size								[0........1], def. 0.5f
		fDamp: Single;                  // damping									[0........1], def. 0.5f
		fWidth: Single;                 // stereo width								[0........1], def. 1
		lMode: DWORD;                   // 0 or BASS_BFX_FREEVERB_MODE_FREEZE, def. 0 (no freeze)
		lChannel: Integer;              // BASS_BFX_CHANxxx flag/s
	end;

{=============================================================================
	set dsp fx			- BASS_ChannelSetFX
	remove dsp fx		- BASS_ChannelRemoveFX
	set parameters		- BASS_FXSetParameters
	retrieve parameters - BASS_FXGetParameters
	reset the state		- BASS_FXReset
=============================================================================}

{=============================================================================
	Tempo, Pitch scaling and Sample rate changers
=============================================================================}

// NOTE: Enable Tempo supported flags in BASS_FX_TempoCreate and the others to source handle.

const
	// tempo attributes (BASS_ChannelSet/GetAttribute)
	BASS_ATTRIB_TEMPO       = $10000;
	BASS_ATTRIB_TEMPO_PITCH = $10001;
	BASS_ATTRIB_TEMPO_FREQ  = $10002;

	// tempo attributes options
	BASS_ATTRIB_TEMPO_OPTION_USE_AA_FILTER    = $10010;    // TRUE (default) / FALSE (default for multi-channel on mobile devices for lower CPU usage)
	BASS_ATTRIB_TEMPO_OPTION_AA_FILTER_LENGTH = $10011;    // 32 default (8 .. 128 taps)
	BASS_ATTRIB_TEMPO_OPTION_USE_QUICKALGO    = $10012;    // TRUE (default on mobile devices for loswer CPU usage) / FALSE (default)
	BASS_ATTRIB_TEMPO_OPTION_SEQUENCE_MS      = $10013;    // 82 default, 0 = automatic
	BASS_ATTRIB_TEMPO_OPTION_SEEKWINDOW_MS    = $10014;    // 28 default, 0 = automatic
	BASS_ATTRIB_TEMPO_OPTION_OVERLAP_MS       = $10015;    // 8  default
	BASS_ATTRIB_TEMPO_OPTION_PREVENT_CLICK    = $10016;    // TRUE / FALSE (default)

	// tempo algorithm flags
	BASS_FX_TEMPO_ALGO_LINEAR  = $200;
	BASS_FX_TEMPO_ALGO_CUBIC   = $400;                     // default
	BASS_FX_TEMPO_ALGO_SHANNON = $800;

function BASS_FX_TempoCreate(chan, flags: DWORD): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;
function BASS_FX_TempoGetSource(chan: HSTREAM): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;
function BASS_FX_TempoGetRateRatio(chan: HSTREAM): Single; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;

{=============================================================================
	Reverse playback
=============================================================================}

// NOTES: 1. MODs won't load without BASS_MUSIC_PRESCAN flag.
//        2. Enable Reverse supported flags in BASS_FX_ReverseCreate and the others to source handle.

const
	// reverse attribute (BASS_ChannelSet/GetAttribute)
	BASS_ATTRIB_REVERSE_DIR = $11000;

	// playback directions
	BASS_FX_RVS_REVERSE = -1;
	BASS_FX_RVS_FORWARD = 1;

function BASS_FX_ReverseCreate(chan: DWORD; dec_block: Single; flags: DWORD): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;
function BASS_FX_ReverseGetSource(chan: HSTREAM): DWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;

{=============================================================================
	BPM (Beats Per Minute)
=============================================================================}

const
	// bpm flags
	BASS_FX_BPM_BKGRND = 1;            // if in use, then you can do other processing while detection's in progress. Available only in Windows platforms (BPM/Beat)
	BASS_FX_BPM_MULT2  = 2;            // if in use, then will auto multiply bpm by 2 (if BPM < minBPM*2)

	// translation options (deprecated)
	BASS_FX_BPM_TRAN_X2       = 0;     // multiply the original BPM value by 2 (may be called only once & will change the original BPM as well!)
	BASS_FX_BPM_TRAN_2FREQ    = 1;     // BPM value to Frequency
	BASS_FX_BPM_TRAN_FREQ2    = 2;     // Frequency to BPM value
	BASS_FX_BPM_TRAN_2PERCENT = 3;     // BPM value to Percents
	BASS_FX_BPM_TRAN_PERCENT2 = 4;     // Percents to BPM value

type
	BPMPROC = procedure(chan: DWORD; bpm: Single; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
	BPMPROGRESSPROC = procedure(chan: DWORD; percent: Single; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
	BPMPROCESSPROC = BPMPROGRESSPROC;	// back-compatibility

function BASS_FX_BPM_DecodeGet(chan: DWORD; startSec, endSec: Double; minMaxBPM, flags: DWORD; proc: BPMPROGRESSPROC; user: Pointer): Single; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;
function BASS_FX_BPM_CallbackSet(handle: DWORD; proc: BPMPROC; period: Double; minMaxBPM, flags: DWORD; user: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;
function BASS_FX_BPM_CallbackReset(handle: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;
function BASS_FX_BPM_Translate(handle: DWORD; val2tran: Single; trans: DWORD): Single; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;	// deprecated
function BASS_FX_BPM_Free(handle: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;

{=============================================================================
	Beat position trigger
=============================================================================}

type
	BPMBEATPROC = procedure(chan: DWORD; beatpos: Double; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

function BASS_FX_BPM_BeatCallbackSet(handle: DWORD; proc: BPMBEATPROC; user: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;
function BASS_FX_BPM_BeatCallbackReset(handle: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;
function BASS_FX_BPM_BeatDecodeGet(chan: DWORD; startSec, endSec: Double; flags: DWORD; proc: BPMBEATPROC; user: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;
function BASS_FX_BPM_BeatSetParameters(handle: DWORD; bandwidth, centerfreq, beat_rtime: Single): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;
function BASS_FX_BPM_BeatGetParameters(handle: DWORD; var bandwidth, centerfreq, beat_rtime: Single): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;
function BASS_FX_BPM_BeatFree(handle: DWORD): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; external bass_fxdll;

implementation

{=============================================================================
	Macros
=============================================================================}

// if you have more than 8 channels (7.1), use this function
function BASS_BFX_CHANNEL_N(n: DWORD): DWORD;
begin
	Result := 1 shl (n-1);
end;

// translate linear level to logarithmic dB
function BASS_BFX_Linear2dB(level: Double): Double;
begin
	Result := 20*log10(level);
end;

// translate logarithmic dB level to linear
function BASS_BFX_dB2Linear(dB: Double): Double;
begin
	Result := Power(10,(dB)/20);
end;

end.
