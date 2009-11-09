{ $Header: /MidiComp/MIDICONS.PAS 2     10/06/97 7:33 Davec $ }

{ Written by David Churcher <dchurcher@cix.compulink.co.uk>,
  released to the public domain. }


{ MIDI Constants }
unit Midicons;

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+} // use long strings
{$ENDIF}

uses Messages;

const
	MIDI_ALLNOTESOFF     = $7B;
	MIDI_NOTEON          = $90;
	MIDI_NOTEOFF         = $80;
	MIDI_KEYAFTERTOUCH   = $a0;
	MIDI_CONTROLCHANGE   = $b0;
	MIDI_PROGRAMCHANGE   = $c0;
	MIDI_CHANAFTERTOUCH  = $d0;
	MIDI_PITCHBEND       = $e0;
	MIDI_SYSTEMMESSAGE   = $f0;
	MIDI_BEGINSYSEX      = $f0;
	MIDI_MTCQUARTERFRAME = $f1;
	MIDI_SONGPOSPTR      = $f2;
	MIDI_SONGSELECT      = $f3;
	MIDI_ENDSYSEX        = $F7;
	MIDI_TIMINGCLOCK     = $F8;
	MIDI_START           = $FA;
	MIDI_CONTINUE        = $FB;
	MIDI_STOP            = $FC;
	MIDI_ACTIVESENSING   = $FE;
	MIDI_SYSTEMRESET     = $FF;

	MIM_OVERFLOW         = WM_USER;	{ Input buffer overflow }
	MOM_PLAYBACK_DONE    = WM_USER+1; { Timed playback complete }


implementation

end.
