{ $Header: /MidiComp/DelphiMcb.pas 2     10/06/97 7:33 Davec $ }

{Midi callback for Delphi, was DLL for Delphi 1}

unit DelphiMcb;

{ These segment options required for the Midi callback functions }
{$IFNDEF FPC}
{$C PRELOAD FIXED PERMANENT}
{$ENDIF}

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+} // use long strings
{$ENDIF}

uses
  Windows,
  MMsystem,
  CircBuf,
  MidiDefs,
  MidiCons;

procedure MidiHandler(
      hMidiIn: HMidiIn;
      wMsg: uint;
      dwInstance: dword;
      dwParam1: dword;
      dwParam2: dword); stdcall; export;

function CircbufPutEvent(PBuffer: PCircularBuffer; PTheEvent: PMidiBufferItem): boolean; stdcall; export;

implementation

{ Add an event to the circular input buffer. }
function CircbufPutEvent(PBuffer: PCircularBuffer; PTheEvent: PMidiBufferItem): boolean; stdcall;
begin
  if PBuffer^.EventCount < PBuffer^.Capacity then
  begin
    inc(Pbuffer^.EventCount);

    { Todo: better way of copying this record }
    with PBuffer^.PNextput^ do
    begin
      Timestamp := PTheEvent^.Timestamp;
      Data := PTheEvent^.Data;
      Sysex := PTheEvent^.Sysex;
    end;

    { Move to next put location, with wrap }
    inc(Pbuffer^.PNextPut);
    if PBuffer^.PNextPut = PBuffer^.PEnd then
      PBuffer^.PNextPut := PBuffer^.PStart;

    CircbufPutEvent := true;
  end
  else
    CircbufPutEvent := false;
end;

{ This is the callback function specified when the Midi device was opened
  by MidiInOpen. It's called at interrupt time when Midi input is seen
  by the Midi device driver(s). See the docs for MidiInOpen for restrictions
  on the Windows functions that can be called in this interrupt. }
procedure MidiHandler(
      hMidiIn: HMidiIn;
      wMsg: dword;
      dwInstance: dword;
      dwParam1: dword;
      dwParam2: dword); stdcall;
var
  thisEvent: TMidiBufferItem;
  thisCtlInfo: PMidiCtlInfo;
  thisBuffer: PCircularBuffer;
begin
  case wMsg of

    mim_Open: {nothing};

    mim_Error: {TODO: handle (message to trigger exception?) };

    mim_Data, mim_Longdata, mim_Longerror:
      { Note: mim_Longerror included because there's a bug in the Maui
      input driver that sends MIM_LONGERROR for subsequent buffers when
      the input buffer is smaller than the sysex block being received }

    begin
      { TODO: Make filtered messages customisable, I'm sure someone wants to
      do something with MTC! }
      if (dwParam1 <> MIDI_ACTIVESENSING) and
              (dwParam1 <> MIDI_TIMINGCLOCK) then
      begin

        { The device driver passes us the instance data pointer we
        specified for MidiInOpen. Use this to get the buffer address
        and window handle for the Midi control }
        thisCtlInfo := PMidiCtlInfo(dwInstance);
        thisBuffer := thisCtlInfo^.PBuffer;

        { Screen out short messages if we've been asked to }
        if ((wMsg <> mim_Data) or (thisCtlInfo^.SysexOnly = false))
          and (thisCtlInfo <> nil) and (thisBuffer <> nil) then
        begin
          with thisEvent do
          begin
            timestamp := dwParam2;
            if (wMsg = mim_Longdata) or (wMsg = mim_Longerror) then
            begin
              data := 0;
              sysex := PMidiHdr(dwParam1);
            end
            else
            begin
              data := dwParam1;
              sysex := nil;
            end;
          end;
          if CircbufPutEvent(thisBuffer, @thisEvent) then
            { Send a message to the control to say input's arrived }
            PostMessage(thisCtlInfo^.hWindow, mim_Data, 0, 0)
          else
            { Buffer overflow }
            PostMessage(thisCtlInfo^.hWindow, mim_Overflow, 0, 0);
        end;
      end;
    end;

    mom_Done: { Sysex output complete, dwParam1 is pointer to MIDIHDR }
    begin
      { Notify the control that its sysex output is finished.
        The control should call MidiOutUnprepareHeader before freeing the buffer }
      PostMessage(PMidiCtlInfo(dwInstance)^.hWindow, mom_Done, 0, dwParam1);
    end;

  end;  { Case }
end;

end.
