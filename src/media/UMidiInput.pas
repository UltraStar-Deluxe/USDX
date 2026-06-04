{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *}

unit UMidiInput;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$I switches.inc}

uses
  SysUtils;

type
  TMidiNoteProc = procedure (Note : Byte) of object;

procedure OpenMidiIn  (CB : TMidiNoteProc);
procedure CloseMidiIn;

implementation

uses
  Dynlibs,
  ULog,
  UMain;

const
  MidiInputDeviceID = 0;

  MMSYSERR_NOERROR = 0;
  MAXERRORLENGTH   = 256;
  MAXPNAMELEN      = 32;

  CALLBACK_FUNCTION = $00030000;
  MIM_DATA          = $03C3;

type
  MMRESULT = Cardinal;
  HMIDIIn = PtrUInt;
  PHMIDIIn = ^HMIDIIn;

  TMidiInCaps = packed record
    wMid: Word;
    wPid: Word;
    vDriverVersion: Cardinal;
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;
    dwSupport: LongWord;
  end;
  PMidiInCaps = ^TMidiInCaps;

  TMidiInGetNumDevs = function: Cardinal; stdcall;
  TMidiInGetDevCaps = function(uDeviceID: PtrUInt; lpMidiInCaps: PMidiInCaps;
    cbMidiInCaps: Cardinal): MMRESULT; stdcall;
  TMidiInOpen = function(lphMidiIn: PHMIDIIn; uDeviceID: Cardinal;
    dwCallback: PtrUInt; dwInstance: PtrUInt; fdwOpen: LongWord): MMRESULT; stdcall;
  TMidiInStart = function(hMidiIn: HMIDIIn): MMRESULT; stdcall;
  TMidiInStop = function(hMidiIn: HMIDIIn): MMRESULT; stdcall;
  TMidiInReset = function(hMidiIn: HMIDIIn): MMRESULT; stdcall;
  TMidiInClose = function(hMidiIn: HMIDIIn): MMRESULT; stdcall;
  TMidiInGetErrorText = function(mmrError: MMRESULT; pszText: PAnsiChar;
    cchText: Cardinal): MMRESULT; stdcall;

var
  InHandle: HMIDIIn = 0;
  OnNote: TMidiNoteProc;
  WinMMHandle: TLibHandle = 0;

  MidiInGetNumDevs: TMidiInGetNumDevs = nil;
  MidiInGetDevCaps: TMidiInGetDevCaps = nil;
  MidiInOpen: TMidiInOpen = nil;
  MidiInStart: TMidiInStart = nil;
  MidiInStop: TMidiInStop = nil;
  MidiInReset: TMidiInReset = nil;
  MidiInClose: TMidiInClose = nil;
  MidiInGetErrorText: TMidiInGetErrorText = nil;

procedure ClearWinMMFunctions;
begin
  MidiInGetNumDevs := nil;
  MidiInGetDevCaps := nil;
  MidiInOpen := nil;
  MidiInStart := nil;
  MidiInStop := nil;
  MidiInReset := nil;
  MidiInClose := nil;
  MidiInGetErrorText := nil;
end;

function LoadWinMM: boolean;
begin
  if WinMMHandle <> 0 then
    Exit(true);

  WinMMHandle := LoadLibrary('winmm.dll');
  if WinMMHandle = 0 then
    Exit(false);

  MidiInGetNumDevs := TMidiInGetNumDevs(GetProcedureAddress(WinMMHandle, 'midiInGetNumDevs'));
  MidiInGetDevCaps := TMidiInGetDevCaps(GetProcedureAddress(WinMMHandle, 'midiInGetDevCapsA'));
  MidiInOpen := TMidiInOpen(GetProcedureAddress(WinMMHandle, 'midiInOpen'));
  MidiInStart := TMidiInStart(GetProcedureAddress(WinMMHandle, 'midiInStart'));
  MidiInStop := TMidiInStop(GetProcedureAddress(WinMMHandle, 'midiInStop'));
  MidiInReset := TMidiInReset(GetProcedureAddress(WinMMHandle, 'midiInReset'));
  MidiInClose := TMidiInClose(GetProcedureAddress(WinMMHandle, 'midiInClose'));
  MidiInGetErrorText := TMidiInGetErrorText(GetProcedureAddress(WinMMHandle, 'midiInGetErrorTextA'));

  Result :=
    Assigned(MidiInGetNumDevs) and
    Assigned(MidiInGetDevCaps) and
    Assigned(MidiInOpen) and
    Assigned(MidiInStart) and
    Assigned(MidiInStop) and
    Assigned(MidiInReset) and
    Assigned(MidiInClose) and
    Assigned(MidiInGetErrorText);

  if not Result then
  begin
    Log.LogError('Could not load WinMM MIDI input functions.', 'UMidiInput.LoadWinMM');
    UnloadLibrary(WinMMHandle);
    WinMMHandle := 0;
    ClearWinMMFunctions;
  end;
end;

procedure UnloadWinMM;
begin
  if WinMMHandle <> 0 then
  begin
    UnloadLibrary(WinMMHandle);
    WinMMHandle := 0;
  end;
  ClearWinMMFunctions;
end;

function MidiInErrorString(Error: MMRESULT): string;
var
  ErrorText: array[0..MAXERRORLENGTH - 1] of AnsiChar;
begin
  FillChar(ErrorText, SizeOf(ErrorText), 0);
  if Assigned(MidiInGetErrorText) and
     (MidiInGetErrorText(Error, @ErrorText[0], SizeOf(ErrorText)) = MMSYSERR_NOERROR) then
    Result := string(PAnsiChar(@ErrorText[0]))
  else
    Result := 'MIDI input error ' + IntToStr(Error);
end;

procedure ListDevices;
var
  i, n: Integer;
  caps: TMidiInCaps;
begin
  if not Assigned(MidiInGetNumDevs) or not Assigned(MidiInGetDevCaps) then
    Exit;

  n := MidiInGetNumDevs();
  Log.LogInfo('---------- MIDI INPUT DEVICES ----------','UMidiInput.ListDevices');
  for i := 0 to n-1 do
  begin
    FillChar(caps, SizeOf(caps), 0);
    if MidiInGetDevCaps(i, @caps, SizeOf(caps)) = MMSYSERR_NOERROR then
      Log.LogInfo(Format('ID %d : %s', [i, string(PAnsiChar(@caps.szPname[0]))]),
        'UMidiInput.ListDevices');
  end;
end;

procedure DeliverMidiNote(Data: Pointer);
var
  Note: Byte;
begin
  Note := Byte(PtrUInt(Data) and $FF);
  if Assigned(OnNote) then
    OnNote(Note);
end;

procedure MidiInCallback(hMidiIn: HMIDIIn; wMsg: Cardinal; dwInstance: PtrUInt;
  dwParam1: PtrUInt; dwParam2: PtrUInt); stdcall;
var
  ShortMessage: LongWord;
  Status: Byte;
  Note: Byte;
  Velocity: Byte;
begin
  if wMsg <> MIM_DATA then
    Exit;

  ShortMessage := LongWord(dwParam1);
  Status := ShortMessage and $FF;
  Note := (ShortMessage shr 8) and $FF;
  Velocity := (ShortMessage shr 16) and $FF;

  if ((Status and $F0) = $90) and (Velocity <> 0) then
    MainThreadExec(@DeliverMidiNote, Pointer(PtrUInt(Note)));
end;

procedure OpenMidiIn(CB : TMidiNoteProc);
var
  caps: TMidiInCaps;
  numDevs: Integer;
  Error: MMRESULT;
begin
  if InHandle <> 0 then
    Exit;

  if not LoadWinMM then
    Exit;

  OnNote := CB;

  ListDevices;

  numDevs := MidiInGetNumDevs();
  if numDevs = 0 then
  begin
    Log.LogWarn('No MIDI input devices found. MIDI-IN will not be opened.','UMidiInput.OpenMidiIn');
    OnNote := nil;
    UnloadWinMM;
    Exit;
  end;

  Error := MidiInOpen(@InHandle, MidiInputDeviceID, PtrUInt(@MidiInCallback), 0, CALLBACK_FUNCTION);
  if Error <> MMSYSERR_NOERROR then
  begin
    Log.LogError('Could not open MIDI-IN ID 0: ' + MidiInErrorString(Error), 'UMidiInput.OpenMidiIn');
    InHandle := 0;
    OnNote := nil;
    UnloadWinMM;
    Exit;
  end;

  Error := MidiInStart(InHandle);
  if Error <> MMSYSERR_NOERROR then
  begin
    Log.LogError('Could not start MIDI-IN ID 0: ' + MidiInErrorString(Error), 'UMidiInput.OpenMidiIn');
    MidiInClose(InHandle);
    InHandle := 0;
    OnNote := nil;
    UnloadWinMM;
    Exit;
  end;

  FillChar(caps, SizeOf(caps), 0);
  if MidiInGetDevCaps(MidiInputDeviceID, @caps, SizeOf(caps)) = MMSYSERR_NOERROR then
    Log.LogInfo(Format('Opened MIDI-IN ID %d (%s)',
      [MidiInputDeviceID, string(PAnsiChar(@caps.szPname[0]))]), 'UMidiInput.OpenMidiIn')
  else
    Log.LogInfo(Format('Opened MIDI-IN ID %d', [MidiInputDeviceID]), 'UMidiInput.OpenMidiIn');
end;

procedure CloseMidiIn;
begin
  OnNote := nil;
  if InHandle <> 0 then
  begin
    Log.LogInfo('Closing MIDI-IN','UMidiInput.CloseMidiIn');
    MidiInStop(InHandle);
    MidiInReset(InHandle);
    MidiInClose(InHandle);
    InHandle := 0;
  end;
  UnloadWinMM;
end;

end.
