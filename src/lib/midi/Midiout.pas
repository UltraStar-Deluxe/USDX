{ $Header: /MidiComp/MidiOut.pas 2     10/06/97 7:33 Davec $ }

{ Written by David Churcher <dchurcher@cix.compulink.co.uk>,
  released to the public domain. }

{ Thanks very much to Fred Kohler for the Technology code. }

unit MidiOut;

{
  MIDI Output component.

  Properties:
   DeviceID: 	Windows numeric device ID for the MIDI output device.
 Between 0 and (midioutGetNumDevs-1), or MIDI_MAPPER (-1).
    Special value MIDI_MAPPER specifies output to the Windows MIDI mapper
 Read-only while device is open, exception if changed while open

 MIDIHandle:	The output handle to the MIDI device.
 0 when device is not open
 Read-only, runtime-only

 ProductName: Name of the output device product that corresponds to the
 DeviceID property (e.g. 'MPU 401 out').
 You can write to this while the device is closed to select a particular
 output device by name (the DeviceID property will change to match).
 Exception if this property is changed while the device is open.

 Numdevs: Number of MIDI output devices installed on the system. This
 is the value returned by midiOutGetNumDevs. It's included for
 completeness.

    Technology: Type of technology used by the MIDI device. You can set this
    property to one of the values listed for OutportTech (below) and the component
    will find an appropriate MIDI device. For example:
     MidiOutput.Technology := opt_FMSynth;
    will set MidiInput.DeviceID to the MIDI device ID of the FM synth, if one
    is installed. If no such device is available an exception is raised,
    see MidiOutput.SetTechnology.

 See the MIDIOUTCAPS entry in MMSYSTEM.HLP for descriptions of the
 following properties:
  DriverVersion
  Voices
  Notes
  ChannelMask
  Support

 Error: The error code for the last MMSYSTEM error. See the MMSYSERR_
 entries in MMSYSTEM.INT for possible values.

  Methods:
 Open: Open MIDI device specified by DeviceID property for output

 Close: Close device

 PutMidiEvent(Event:TMyMidiEvent): Output a note or sysex message to the
 device. This method takes a TMyMidiEvent object and transmits it.
 Notes:
   1. If the object contains a sysex event the OnMidiOutput event will
    be triggered when the sysex transmission is complete.
   2. You can queue up multiple blocks of system exclusive data for
    transmission by chucking them at this method; they will be
   transmitted as quickly as the device can manage.
   3. This method will not free the TMyMidiEvent object, the caller
    must do that. Any sysex data in the TMyMidiEvent is copied before
   transmission so you can free the TMyMidiEvent immediately after
   calling PutMidiEvent, even if output has not yet finished.

 PutShort(MidiMessage: Byte; Data1: Byte; Data2: Byte): Output a short
 MIDI message. Handy when you can't be bothered to build a TMyMidiEvent.
 If the message you're sending doesn't use Data1 or Data2, set them to 0.

 PutLong(TheSysex: Pointer; msgLength: Word): Output sysex data.
  SysexPointer: Pointer to sysex data to send
  msgLength: Length of sysex data.
 This is handy when you don't have a TMyMidiEvent.

 SetVolume(Left: Word, Right: Word): Set the volume of the
 left and right channels on the output device (only on internal devices?).
 0xFFFF is maximum volume. If the device doesn't support separate
 left/right volume control, the value of the Left parameter will be used.
 Check the Support property to see whether the device supports volume
 control. See also other notes on volume control under midiOutSetVolume()
 in MMSYSTEM.HLP.

  Events:
 OnMidiOutput: Procedure called when output of a system exclusive block
 is completed.

  Notes:
   I haven't implemented any methods for midiOutCachePatches and
  midiOutCacheDrumpatches, mainly 'cause I don't have any way of testing
  them. Does anyone really use these?
}

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+} // use long strings
{$ENDIF}

uses
  SysUtils,
  Windows,
  Messages,
  Classes,
  MMSystem,
  {$IFDEF FPC}
  WinAllocation,
  {$ENDIF}
  Circbuf,
  MidiType,
  MidiDefs,
  Delphmcb;

{$IFDEF FPC}
type TmidioutCaps = MIDIOUTCAPS;
{$ENDIF}

type
  midioutputState = (mosOpen, mosClosed);
  EmidioutputError = class(Exception);

 { These are the equivalent of constants prefixed with mod_
   as defined in MMSystem. See SetTechnology }
  OutPortTech = (
    opt_None, { none }
    opt_MidiPort, { output port }
    opt_Synth, { generic internal synth }
    opt_SQSynth, { square wave internal synth }
    opt_FMSynth, { FM internal synth }
    opt_Mapper); { MIDI mapper }
  TechNameMap = array[OutPortTech] of string[18];


const
  TechName: TechNameMap = (
    'None', 'MIDI Port', 'Generic Synth', 'Square Wave Synth',
    'FM Synth', 'MIDI Mapper');

{-------------------------------------------------------------------}
type
  TMidiOutput = class(TComponent)
  protected
    Handle: THandle; { Window handle used for callback notification }
    FDeviceID: Cardinal; { MIDI device ID }
    FMIDIHandle: Hmidiout; { Handle to output device }
    FState: midioutputState; { Current device state }
    PCtlInfo: PMidiCtlInfo; { Pointer to control info for DLL }

    PBuffer: PCircularBuffer; { Output queue for PutTimedEvent, set by Open }

    FError: Word; { Last MMSYSTEM error }

 { Stuff from midioutCAPS }
    FDriverVersion: MMVERSION; { Driver version from midioutGetDevCaps }
    FProductName: string; { product name }
    FTechnology: OutPortTech; { Type of MIDI output device }
    FVoices: Word; { Number of voices (internal synth) }
    FNotes: Word; { Number of notes (internal synth) }
    FChannelMask: Word; { Bit set for each MIDI channels that the
          device responds to (internal synth) }
    FSupport: DWORD; { Technology supported (volume control,
          patch caching etc. }
    FNumdevs: Word; { Number of MIDI output devices on system }


    FOnMIDIOutput: TNotifyEvent; { Sysex output finished }

    procedure MidiOutput(var Message: TMessage);
    procedure SetDeviceID(DeviceID: Cardinal);
    procedure SetProductName(NewProductName: string);
    procedure SetTechnology(NewTechnology: OutPortTech);
    function midioutErrorString(WError: Word): string;

  public
 { Properties }
    property MIDIHandle: Hmidiout read FMIDIHandle;
    property DriverVersion: MMVERSION { Driver version from midioutGetDevCaps }
    read FDriverVersion;
    property Technology: OutPortTech { Type of MIDI output device }
    read FTechnology
      write SetTechnology
      default opt_Synth;
    property Voices: Word { Number of voices (internal synth) }
    read FVoices;
    property Notes: Word { Number of notes (internal synth) }
    read FNotes;
    property ChannelMask: Word { Bit set for each MIDI channels that the }
    read FChannelMask; { device responds to (internal synth) }
    property Support: DWORD { Technology supported (volume control, }
    read FSupport; { patch caching etc. }
    property Error: Word read FError;
    property Numdevs: Word read FNumdevs;

 { Methods }
    function Open: Boolean; virtual;
    function Close: Boolean; virtual;
    procedure PutMidiEvent(theEvent: TMyMidiEvent); virtual;
    procedure PutShort(MidiMessage: Byte; Data1: Byte; Data2: Byte); virtual;
    procedure PutLong(TheSysex: Pointer; msgLength: Word); virtual;
    procedure SetVolume(Left: Word; Right: Word);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

   { Some functions to decode and classify incoming messages would be nice }

  published
 { TODO: Property editor with dropdown list of product names }
    property ProductName: string read FProductName write SetProductName;

    property DeviceID: Cardinal read FDeviceID write SetDeviceID default 0;
 { TODO: midiOutGetVolume? Or two properties for Left and Right volume?
   Is it worth it??
     midiOutMessage?? Does anyone use this? }

 { Events }
    property Onmidioutput: TNotifyEvent
      read FOnmidioutput
      write FOnmidioutput;
  end;

procedure Register;

{-------------------------------------------------------------------}
implementation

(* Not used in Delphi 3

{ This is the callback procedure in the external DLL.
  It's used when midioutOpen is called by the Open method.
  There are special requirements and restrictions for this callback
  procedure (see midioutOpen in MMSYSTEM.HLP) so it's impractical to
  make it an object method }
{$IFDEF WIN32}
function midiHandler(
    hMidiIn: HMidiIn;
    wMsg: UINT;
    dwInstance: DWORD;
    dwParam1: DWORD;
    dwParam2: DWORD): Boolean; stdcall; external 'DELMID32.DLL';
{$ELSE}
function midiHandler(
    hMidiIn: HMidiIn;
    wMsg: Word;
    dwInstance: DWORD;
    dwParam1: DWORD;
    dwParam2: DWORD): Boolean; far; external 'DELPHMID.DLL';
{$ENDIF}
*)

{-------------------------------------------------------------------}

constructor Tmidioutput.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := mosClosed;
  FNumdevs := midiOutGetNumDevs;

 { Create the window for callback notification }
  if not (csDesigning in ComponentState) then
  begin
    Handle := AllocateHwnd(MidiOutput);
  end;

end;

{-------------------------------------------------------------------}

destructor Tmidioutput.Destroy;
begin
  if FState = mosOpen then
    Close;
  if (PCtlInfo <> nil) then
    GlobalSharedLockedFree(PCtlinfo^.hMem, PCtlInfo);
  DeallocateHwnd(Handle);
  inherited Destroy;
end;

{-------------------------------------------------------------------}
{ Convert the numeric return code from an MMSYSTEM function to a string
  using midioutGetErrorText. TODO: These errors aren't very helpful
  (e.g. "an invalid parameter was passed to a system function") so
  some proper error strings would be nice. }


function Tmidioutput.midioutErrorString(WError: Word): string;
var
  errorDesc: PChar;
begin
  errorDesc := nil;
  try
    errorDesc := StrAlloc(MAXERRORLENGTH);
    if midioutGetErrorText(WError, errorDesc, MAXERRORLENGTH) = 0 then
      result := StrPas(errorDesc)
    else
      result := 'Specified error number is out of range';
  finally
    if errorDesc <> nil then StrDispose(errorDesc);
  end;
end;

{-------------------------------------------------------------------}
{ Set the output device ID and change the other properties to match }

procedure Tmidioutput.SetDeviceID(DeviceID: Cardinal);
var
  midioutCaps: TmidioutCaps;
begin
  if FState = mosOpen then
    raise EmidioutputError.Create('Change to DeviceID while device was open')
  else
    if (DeviceID >= midioutGetNumDevs) and (DeviceID <> MIDI_MAPPER) then
      raise EmidioutputError.Create('Invalid device ID')
    else
    begin
      FDeviceID := DeviceID;

   { Set the name and other midioutCAPS properties to match the ID }
      FError :=
        midioutGetDevCaps(DeviceID, @midioutCaps, sizeof(TmidioutCaps));
      if Ferror > 0 then
        raise EmidioutputError.Create(midioutErrorString(FError));

      with midiOutCaps do
      begin
        FProductName := StrPas(szPname);
        FDriverVersion := vDriverVersion;
        FTechnology := OutPortTech(wTechnology);
        FVoices := wVoices;
        FNotes := wNotes;
        FChannelMask := wChannelMask;
        FSupport := dwSupport;
      end;

    end;
end;

{-------------------------------------------------------------------}
{ Set the product name property and put the matching output device number
  in FDeviceID.
  This is handy if you want to save a configured output/output device
  by device name instead of device number, because device numbers may
  change if users install or remove MIDI devices.
  Exception if output device with matching name not found,
  or if output device is open }

procedure Tmidioutput.SetProductName(NewProductName: string);
var
  midioutCaps: TmidioutCaps;
  testDeviceID: Integer;
  testProductName: string;
begin
  if FState = mosOpen then
    raise EmidioutputError.Create('Change to ProductName while device was open')
  else
  { Don't set the name if the component is reading properties because
  the saved Productname will be from the machine the application was compiled
  on, which may not be the same for the corresponding DeviceID on the user's
  machine. The FProductname property will still be set by SetDeviceID }
    if not (csLoading in ComponentState) then
    begin
    { Loop uses -1 to test for MIDI_MAPPER as well }
      for testDeviceID := -1 to (midioutGetNumDevs - 1) do
      begin
        FError :=
          midioutGetDevCaps(testDeviceID, @midioutCaps, sizeof(TmidioutCaps));
        if Ferror > 0 then
          raise EmidioutputError.Create(midioutErrorString(FError));
        testProductName := StrPas(midioutCaps.szPname);
        if testProductName = NewProductName then
        begin
          FProductName := NewProductName;
          Break;
        end;
      end;
      if FProductName <> NewProductName then
        raise EmidioutputError.Create('MIDI output Device ' +
          NewProductName + ' not installed')
      else
        SetDeviceID(testDeviceID);
    end;
end;

{-------------------------------------------------------------------}
{ Set the output technology property and put the matching output device
 number in FDeviceID.
  This is handy, for example, if you want to be able to switch between a
  sound card and a MIDI port }

procedure TMidiOutput.SetTechnology(NewTechnology: OutPortTech);
var
  midiOutCaps: TMidiOutCaps;
  testDeviceID: Integer;
  testTechnology: OutPortTech;
begin
  if FState = mosOpen then
    raise EMidiOutputError.Create(
      'Change to Product Technology while device was open')
  else
  begin
       { Loop uses -1 to test for MIDI_MAPPER as well }
    for testDeviceID := -1 to (midiOutGetNumDevs - 1) do
    begin
      FError :=
        midiOutGetDevCaps(testDeviceID,
        @midiOutCaps, sizeof(TMidiOutCaps));
      if Ferror > 0 then
        raise EMidiOutputError.Create(MidiOutErrorString(FError));
      testTechnology := OutPortTech(midiOutCaps.wTechnology);
      if testTechnology = NewTechnology then
      begin
        FTechnology := NewTechnology;
        Break;
      end;
    end;
    if FTechnology <> NewTechnology then
      raise EMidiOutputError.Create('MIDI output technology ' +
        TechName[NewTechnology] + ' not installed')
    else
      SetDeviceID(testDeviceID);
  end;
end;

{-------------------------------------------------------------------}

function Tmidioutput.Open: Boolean;
var
  hMem: THandle;
begin
  Result := False;
  try
  { Create the control info for the DLL }
    if (PCtlInfo = nil) then
    begin
      PCtlInfo := GlobalSharedLockedAlloc(Sizeof(TMidiCtlInfo), hMem);
      PctlInfo^.hMem := hMem;
    end;

    Pctlinfo^.hWindow := Handle; { Control's window handle }

    FError := midioutOpen(@FMidiHandle, FDeviceId,
      DWORD(@midiHandler),
      DWORD(PCtlInfo),
      CALLBACK_FUNCTION);
{                FError := midioutOpen(@FMidiHandle, FDeviceId,
      Handle,
      DWORD(PCtlInfo),
      CALLBACK_WINDOW); }
    if (FError <> 0) then
   { TODO: use CreateFmtHelp to add MIDI device name/ID to message }
      raise EmidioutputError.Create(midioutErrorString(FError))
    else
    begin
      Result := True;
      FState := mosOpen;
    end;

  except
    if PCtlInfo <> nil then
    begin
      GlobalSharedLockedFree(PCtlInfo^.hMem, PCtlInfo);
      PCtlInfo := nil;
    end;
  end;

end;

{-------------------------------------------------------------------}

procedure TMidiOutput.PutShort(MidiMessage: Byte; Data1: Byte; Data2: Byte);
var
  thisMsg: DWORD;
begin
  thisMsg := DWORD(MidiMessage) or
    (DWORD(Data1) shl 8) or
    (DWORD(Data2) shl 16);

  FError := midiOutShortMsg(FMidiHandle, thisMsg);
  if Ferror > 0 then
    raise EmidioutputError.Create(midioutErrorString(FError));
end;

{-------------------------------------------------------------------}

procedure TMidiOutput.PutLong(TheSysex: Pointer; msgLength: Word);
{ Notes: This works asynchronously; you send your sysex output by
calling this function, which returns immediately. When the MIDI device
driver has finished sending the data the MidiOutPut function in this
component is called, which will in turn call the OnMidiOutput method
if the component user has defined one. }
{ TODO: Combine common functions with PutTimedLong into subroutine }

var
  MyMidiHdr: TMyMidiHdr;
begin
 { Initialize the header and allocate buffer memory }
  MyMidiHdr := TMyMidiHdr.Create(msgLength);

 { Copy the data over to the MidiHdr buffer
   We can't just use the caller's PChar because the buffer memory
   has to be global, shareable, and locked. }
  StrMove(MyMidiHdr.SysexPointer, TheSysex, msgLength);

 { Store the MyMidiHdr address in the header so we can find it again quickly
      (see the MidiOutput proc) }
  MyMidiHdr.hdrPointer^.dwUser := DWORD(MyMidiHdr);

 { Get MMSYSTEM's blessing for this header }
  FError := midiOutPrepareHeader(FMidiHandle, MyMidiHdr.hdrPointer,
    sizeof(TMIDIHDR));
  if Ferror > 0 then
    raise EMidiOutputError.Create(MidiOutErrorString(FError));

 { Send it }
  FError := midiOutLongMsg(FMidiHandle, MyMidiHdr.hdrPointer,
    sizeof(TMIDIHDR));
  if Ferror > 0 then
    raise EMidiOutputError.Create(MidiOutErrorString(FError));

end;

{-------------------------------------------------------------------}

procedure Tmidioutput.PutMidiEvent(theEvent: TMyMidiEvent);
begin
  if FState <> mosOpen then
    raise EMidiOutputError.Create('MIDI Output device not open');

  with theEvent do
  begin
    if Sysex = nil then
    begin
      PutShort(MidiMessage, Data1, Data2)
    end
    else
      PutLong(Sysex, SysexLength);
  end;
end;

{-------------------------------------------------------------------}

function Tmidioutput.Close: Boolean;
begin
  Result := False;
  if FState = mosOpen then
  begin

        { Note this sends a lot of fast control change messages which some synths can't handle.
          TODO: Make this optional. }
{		FError := midioutReset(FMidiHandle);
  if Ferror <> 0 then
   raise EMidiOutputError.Create(MidiOutErrorString(FError)); }

    FError := midioutClose(FMidiHandle);
    if Ferror <> 0 then
      raise EMidiOutputError.Create(MidiOutErrorString(FError))
    else
      Result := True;
  end;

  FMidiHandle := 0;
  FState := mosClosed;

end;

{-------------------------------------------------------------------}

procedure TMidiOutput.SetVolume(Left: Word; Right: Word);
var
  dwVolume: DWORD;
begin
  dwVolume := (DWORD(Left) shl 16) or Right;
  FError := midiOutSetVolume(DeviceID, dwVolume);
  if Ferror <> 0 then
    raise EMidiOutputError.Create(MidiOutErrorString(FError));
end;

{-------------------------------------------------------------------}

procedure Tmidioutput.midioutput(var Message: TMessage);
{ Triggered when sysex output from PutLong is complete }
var
  MyMidiHdr: TMyMidiHdr;
  thisHdr: PMidiHdr;
begin
  if Message.Msg = Mom_Done then
  begin
  { Find the MIDIHDR we used for the output. Message.lParam is its address }
    thisHdr := PMidiHdr(Message.lParam);

  { Remove it from the output device }
    midiOutUnprepareHeader(FMidiHandle, thisHdr, sizeof(TMIDIHDR));

  { Get the address of the MyMidiHdr object containing this MIDIHDR structure.
   We stored this address in the PutLong procedure }
    MyMidiHdr := TMyMidiHdr(thisHdr^.dwUser);

  { Header and copy of sysex data no longer required since output is complete }
    MyMidiHdr.Free;

  { Call the user's event handler if any }
    if Assigned(FOnmidioutput) then
      FOnmidioutput(Self);
  end;
 { TODO: Case for MOM_PLAYBACK_DONE }
end;

{-------------------------------------------------------------------}

procedure Register;
begin
  RegisterComponents('Synth', [Tmidioutput]);
end;

end.

