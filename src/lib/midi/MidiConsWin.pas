{ $Header: /MidiComp/MidiCons.pas 2     10/06/97 7:33 Davec $ }

{ Written by David Churcher <dchurcher@cix.compulink.co.uk>,
  released to the public domain. }

{ Windows Midi Constants }
unit MidiConsWin;

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+} // use long strings
{$ENDIF}

{$IFNDEF FPC}
uses
  Messages;
{$ENDIF}

const
{$IFDEF FPC}
  WM_USER              = $400;        { standard WM_USER value }
{$ENDIF}

  MIM_OVERFLOW         = WM_USER;     { Input buffer overflow }
  MOM_PLAYBACK_DONE    = WM_USER + 1; { Timed playback complete }

implementation

end.
