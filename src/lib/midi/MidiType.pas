{ $Header: /MidiComp/MidiType.pas 2     10/06/97 7:33 Davec $ } 

{ Written by David Churcher <dchurcher@cix.compulink.co.uk>,
  released to the public domain. }

unit MidiType;

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+} // use long strings
{$ENDIF}

uses
  Classes,
  Windows,
  Messages,
  MMSystem,
  MidiDefs,
  CircBuf;

type
  {-------------------------------------------------------------------}
  { A MIDI input/output event }
  TMyMidiEvent = class(TPersistent)
  public
    MidiMessage: byte;      { MIDI message status byte }
    Data1: byte;            { MIDI message data 1 byte }
    Data2: byte;            { MIDI message data 2 byte }
    Time: dword;            { Time in ms since midiInOpen }
    SysexLength: word;      { Length of sysex data (0 if none) }
    Sysex: Pchar;           { Pointer to sysex data buffer }
    destructor Destroy; override;   { Frees sysex data buffer if nec. }
  end;
  PMyMidiEvent = ^TMyMidiEvent;

  {-------------------------------------------------------------------}
  { Encapsulates the MIDIHDR with its memory handle and sysex buffer }
  PMyMidiHdr = ^TMyMidiHdr;
  TMyMidiHdr = class(TObject)
  public
    hdrHandle: THandle;
    hdrPointer: PMIDIHDR;
    sysexHandle: THandle;
    sysexPointer: pointer;
    constructor Create(BufferSize: word);
    destructor Destroy; override;
  end;

implementation

{-------------------------------------------------------------------}
{ Free any sysex buffer associated with the event }
destructor TMyMidiEvent.Destroy;
begin
  if (Sysex <> nil) then
    Freemem(Sysex, SysexLength);

  inherited Destroy;
end;

{-------------------------------------------------------------------}
{ Allocate memory for the sysex header and buffer }
constructor TMyMidiHdr.Create(BufferSize: word);
begin
  inherited Create;

  if BufferSize > 0 then
  begin
    hdrPointer   := GlobalSharedLockedAlloc(sizeof(TMIDIHDR), hdrHandle);
    sysexPointer := GlobalSharedLockedAlloc(BufferSize, sysexHandle);

    hdrPointer^.lpData := sysexPointer;
    hdrPointer^.dwBufferLength := BufferSize;
  end;
end;

{-------------------------------------------------------------------}
destructor TMyMidiHdr.Destroy;
begin
  GlobalSharedLockedFree(hdrHandle, hdrPointer);
  GlobalSharedLockedFree(sysexHandle, sysexPointer);
  inherited Destroy;
end;

end.
