{ $Header: /MidiComp/MidiDefs.pas 2     10/06/97 7:33 Davec $ }

{ Written by David Churcher <dchurcher@cix.compulink.co.uk>,
  released to the public domain. }


{ Common definitions used by DELPHMID.DPR and the Midi components.
  This must be a separate unit to prevent large chunks of the VCL being
  linked into the DLL. }
unit Mididefs;

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+} // use long strings
{$ENDIF}

uses
  Windows,
  MMsystem,
  CircBuf;

type

  {-------------------------------------------------------------------}
  { This is the information about the control that must be accessed by
    the Midi input callback function in the DLL at interrupt time }
  PMidiCtlInfo = ^TMidiCtlInfo;
  TMidiCtlInfo = record
    hMem: THandle;        { Memory handle for this record }
    PBuffer: PCircularBuffer; { Pointer to the Midi input data buffer }
    hWindow: HWnd;          { Control's window handle }
    SysexOnly: boolean;     { Only process System Exclusive input }
  end;

  { Information for the output timer callback function, also required at
    interrupt time. }
  PMidiOutTimerInfo = ^TMidiOutTimerInfo;
  TMidiOutTimerInfo = record
    hMem: THandle;        { Memory handle for this record }
    PBuffer: PCircularBuffer; { Pointer to Midi output data buffer }
    hWindow: HWnd;        { Control's window handle }
    TimeToNextEvent: dword; { Delay to next event after timer set }
    MidiHandle: HMidiOut;   { Midi handle to send output to
                  (copy of component's FMidiHandle property) }
    PeriodMin: word;      { Multimedia timer minimum period supported }
    PeriodMax: word;      { Multimedia timer maximum period supported }
    TimerId: word;        { Multimedia timer ID of current event }
  end;

implementation

end.
