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
  Classes, Windows, MMSystem, SysUtils,
  MidiIn, MidiType, ULog;

type
  TMidiNoteProc = procedure (Note : Byte) of object;

procedure OpenMidiIn  (CB : TMidiNoteProc);
procedure CloseMidiIn;

implementation

var
  InDev   : TMidiInput  = nil;
  OnNote  : TMidiNoteProc;

type
  TMidiInBridge = class(TObject)
    procedure HandleInput(Sender: TObject);
  end;

var
  Bridge : TMidiInBridge;

procedure TMidiInBridge.HandleInput(Sender: TObject);
var Ev : TMyMidiEvent;
begin
  repeat
    Ev := InDev.GetMidiEvent;
    if Ev = nil then Break;
    try
      if (Ev.MidiMessage and $F0 = $90) and (Ev.Data2 <> 0) then
        if Assigned(OnNote) then
          OnNote(Ev.Data1);   // pass note number
    finally
      Ev.Free;
    end;
  until False;
end;

procedure ListDevices;
var i,n:Integer; caps: TMidiInCaps;
begin
  n := midiInGetNumDevs;
  Log.LogInfo('---------- MIDI INPUT DEVICES ----------','UMidiInput.ListDevices');
  for i := 0 to n-1 do
  begin
    midiInGetDevCaps(i,@caps,SizeOf(caps));
    Log.LogInfo(Format('ID %d : %s',[i,caps.szPname]),'UMidiInput.ListDevices');
  end;
end;

procedure OpenMidiIn(CB : TMidiNoteProc);
var caps: TMidiInCaps;
    dev : Integer;
begin
  if Assigned(InDev) then Exit;

  ListDevices;

  dev := 0;
  InDev := TMidiInput.Create(nil);
  InDev.DeviceID := dev;
  InDev.Open;
  midiInGetDevCaps(dev,@caps,SizeOf(caps));
  Log.LogInfo(Format('Opened MIDI-IN ID %d (%s)',[dev,caps.szPname]),'UMidiInput.OpenMidiIn');

  if Bridge=nil then Bridge := TMidiInBridge.Create;
  InDev.OnMidiInput := Bridge.HandleInput;
  InDev.Start;

  OnNote := CB;
end;

procedure CloseMidiIn;
begin
  if Assigned(InDev) then
  begin
    Log.LogInfo('Closing MIDI-IN','UMidiInput.CloseMidiIn');
    InDev.Stop;
    InDev.Close;
    InDev.Free;
    InDev := nil;
  end;
  FreeAndNil(Bridge);
end;

end.
