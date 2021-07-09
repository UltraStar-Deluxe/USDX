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

unit UKeyboardRecording;



interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  UCommon,
  UUnicodeUtils,
  UMusic,
  UIni;

// Class for recording keyboard events relevant
//for beat note playing with keyboard keys
type
  TKeyboardRecorder = class
    public
    statePressed: array of Boolean; // Array of the length IKeyPlayLetters holding whether this keyboard key was pressed down
    keysRecorded: array of char; // Actually used for comparison, this is the uppercase of whatever IKeyPlayLetters is
    beatRecorded: array of real; // Timing in beats when the key pressed event was detected (so slightly after actually pressing it)
    constructor Create;
    function ParseInput(PressedKey: cardinal; CharCode: UCS4Char;
      PressedDown: boolean): boolean;
    function keyboardPressedForPlayer(playerIndex: integer; resetRegistry: boolean): boolean;
  end;

// create the singleton if necessary
procedure createKeyboardRecorder();


// global singleton variable to hold the keystroke information for keyboard playing
var KeyBoardRecorder : TKeyboardRecorder;



implementation

uses
  UNote;



procedure createKeyboardRecorder();
begin
   if KeyBoardRecorder = nil then
      KeyBoardRecorder := TKeyboardRecorder.Create();
end;

constructor TKeyboardRecorder.Create;
var
  count: integer;
begin
   // for each of possible keys wether it was pressed
   setLength(statePressed, high(IKeyPlayLetters)+1);
   // the keys (letters) recorded, upper case to be independent of caps or shift
   setLength(keysRecorded, high(IKeyPlayLetters)+1);
   // For reference, the nominal beat at which it was recorded (not directly used at present)
   setLength(beatRecorded, high(IKeyPlayLetters)+1);
   for count:= 0 to high(IKeyPlayLetters) do
   begin
       statePressed[count]:=false;
       keysRecorded[count]:=UpCase(IKeyPlayLetters[count])[1];
       beatRecorded[count]:=-1;
   end;

end;




function TKeyboardRecorder.ParseInput(PressedKey: cardinal; CharCode: UCS4Char;
      PressedDown: boolean): boolean;
var
  count: integer;
begin
   for count:= 0 to high(IKeyPlayLetters) do
   begin

      if UCS4UpperCase(CharCode)=Ord(keysRecorded[count]) then
         if PressedDown then
            if not statePressed[count] then // Newly detected, set time as well
               statePressed[count] :=true;
               beatRecorded[count] :=LyricsState.MidBeatD;
   end;


   Result:=true;
end;

// Poll whether the letter associated with a given player was pressed.
// This also resets the registry for this event so that it is not counted multiple
// times
function TKeyboardRecorder.keyboardPressedForPlayer(playerIndex: integer; resetRegistry: boolean): boolean;

begin

     Result := statePressed[Ini.PlayerKeys[playerIndex]];
     if resetRegistry then
          statePressed[Ini.PlayerKeys[playerIndex]]:=false;

end;

end.
