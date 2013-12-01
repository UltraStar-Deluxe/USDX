{ $Header: /MidiComp/CircBuf.pas 2     10/06/97 7:33 Davec $ }

{ Written by David Churcher <dchurcher@cix.compulink.co.uk>,
  released to the public domain. }


{ A First-In First-Out circular buffer.
  Port of circbuf.c from Microsoft's Windows Midi monitor example.
  I did do a version of this as an object (see Rev 1.1) but it was getting too 
  complicated and I couldn't see any real benefits to it so I dumped it 
  for an ordinary memory buffer with pointers. 

  This unit is a bit C-like, everything is done with pointers and extensive
  use is made of the undocumented feature of the inc() function that 
  increments pointers by the size of the object pointed to.
  All of this could probably be done using Pascal array notation with
  range-checking turned off, but I'm not sure it's worth it.
}

unit CircBuf;

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+} // use long strings
{$ENDIF}

uses
  Windows,
  MMSystem;

type
  { Midi input event }
  TMidiBufferItem = record
    timestamp: dword; { Timestamp in milliseconds after midiInStart }
    data: dword;      { Midi message received }
    sysex: PMidiHdr;  { Pointer to sysex MIDIHDR, nil if not sysex }
  end;
  PMidiBufferItem = ^TMidiBufferItem;

  { Midi input buffer }
  TCircularBuffer = record
    RecordHandle: HGLOBAL;     { Windows memory handle for this record }
    BufferHandle: HGLOBAL;     { Windows memory handle for the buffer }
    pStart: PMidiBufferItem;   { ptr to start of buffer }
    pEnd: PMidiBufferItem;     { ptr to end of buffer }
    pNextPut: PMidiBufferItem; { next location to fill }
    pNextGet: PMidiBufferItem; { next location to empty }
    Error: word;               { error code from MMSYSTEM functions }
    Capacity: word;            { buffer size (in TMidiBufferItems) }
    EventCount: word;          { Number of events in buffer }
  end;

  PCircularBuffer = ^TCircularBuffer;

function GlobalSharedLockedAlloc(Capacity: word; var hMem: HGLOBAL): pointer;
procedure GlobalSharedLockedFree(hMem: HGLOBAL; ptr: pointer);

function CircbufAlloc(Capacity: word): PCircularBuffer;
procedure CircbufFree(PBuffer: PCircularBuffer);
function CircbufRemoveEvent(PBuffer: PCircularBuffer): boolean;
function CircbufReadEvent(PBuffer: PCircularBuffer; PEvent: PMidiBufferItem): boolean;
{ Note: The PutEvent function is in the DLL }

implementation

{ Allocates in global shared memory, returns pointer and handle }
function GlobalSharedLockedAlloc(Capacity: word; var hMem: HGLOBAL): pointer;
var
  ptr: pointer;
begin
  { Allocate the buffer memory }
  hMem := GlobalAlloc(GMEM_SHARE Or GMEM_MOVEABLE Or GMEM_ZEROINIT, Capacity);

  if hMem = 0 then
    ptr := nil
  else
  begin
    ptr := GlobalLock(hMem);
    if ptr = nil then
      GlobalFree(hMem);
  end;

  GlobalSharedLockedAlloc := Ptr;
end;

procedure GlobalSharedLockedFree(hMem: HGLOBAL; ptr: pointer);
begin
  if hMem <> 0 then
  begin
    GlobalUnlock(hMem);
    GlobalFree(hMem);
  end;
end;

function CircbufAlloc(Capacity: word): PCircularBuffer;
var
  NewCircularBuffer: PCircularBuffer;
  NewMidiBuffer: PMidiBufferItem;
  hMem: HGLOBAL;
begin
  { TODO: Validate circbuf size, <64K }
  NewCircularBuffer :=
    GlobalSharedLockedAlloc(Sizeof(TCircularBuffer), hMem);
  if NewCircularBuffer <> nil then
  begin
    NewCircularBuffer^.RecordHandle := hMem;
    NewMidiBuffer :=
      GlobalSharedLockedAlloc(Capacity * Sizeof(TMidiBufferItem), hMem);
    if NewMidiBuffer = nil then
    begin
      { TODO: Exception here? }
      GlobalSharedLockedFree(NewCircularBuffer^.RecordHandle,
                      NewCircularBuffer);
      NewCircularBuffer := nil;
    end
    else
    begin
      NewCircularBuffer^.pStart := NewMidiBuffer;
      { Point to item at end of buffer }
      NewCircularBuffer^.pEnd := NewMidiBuffer;
      inc(NewCircularBuffer^.pEnd, Capacity);
      { Start off the get and put pointers in the same position. These
        will get out of sync as the interrupts start rolling in }
      NewCircularBuffer^.pNextPut := NewMidiBuffer;
      NewCircularBuffer^.pNextGet := NewMidiBuffer;
      NewCircularBuffer^.Error := 0;
      NewCircularBuffer^.Capacity := Capacity;
      NewCircularBuffer^.EventCount := 0;
    end;
  end;
  CircbufAlloc := NewCircularBuffer;
end;

procedure CircbufFree(pBuffer: PCircularBuffer);
begin
  if pBuffer <> nil then
  begin
    GlobalSharedLockedFree(pBuffer^.BufferHandle, pBuffer^.pStart);
    GlobalSharedLockedFree(pBuffer^.RecordHandle, pBuffer);
  end;
end;

{ Reads first event in queue without removing it.
  Returns true if successful, False if no events in queue }
function CircbufReadEvent(PBuffer: PCircularBuffer; PEvent: PMidiBufferItem): boolean;
var
  PCurrentEvent: PMidiBufferItem;
begin
  if PBuffer^.EventCount <= 0 then
    CircbufReadEvent := false
  else
  begin
    PCurrentEvent := PBuffer^.PNextget;

    { Copy the object from the "tail" of the buffer to the caller's object }
    PEvent^.Timestamp := PCurrentEvent^.Timestamp;
    PEvent^.Data := PCurrentEvent^.Data;
    PEvent^.Sysex := PCurrentEvent^.Sysex;
    CircbufReadEvent := true;
  end;
end;

{ Remove current event from the queue }
function CircbufRemoveEvent(PBuffer: PCircularBuffer): boolean;
begin
  if PBuffer^.EventCount > 0 then
  begin
    dec(Pbuffer^.EventCount);

    { Advance the buffer pointer, with wrap }
    inc(Pbuffer^.PNextGet);
    if PBuffer^.PNextGet = PBuffer^.PEnd then
      PBuffer^.PNextGet := PBuffer^.PStart;

    CircbufRemoveEvent := true;
  end
  else
    CircbufRemoveEvent := false;
end;

end.
