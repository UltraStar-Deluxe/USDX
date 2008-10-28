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

unit UJoystick;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SDL;

type
  TJoyButton = record
    State:      integer;
    Enabled:    boolean;
    Type_:      byte;
    Sym:        cardinal;
  end;

  TJoyHatState = record
    State:      Boolean;
    LastTick:   Cardinal;
    Enabled:    boolean;
    Type_:      byte;
    Sym:        cardinal;
  end;

  TJoyUnit = record
    Button:   array[0..15] of TJoyButton;
    HatState: Array[0..3]  of TJoyHatState;
  end;

  TJoy = class
    constructor Create;
    procedure Update;
  end;

var
  Joy:        TJoy;
  JoyUnit:    TJoyUnit;
  SDL_Joy:    PSDL_Joystick;
  JoyEvent:   TSDL_Event;

implementation

uses SysUtils,
     ULog;

constructor TJoy.Create;
var
  B: integer;
  //N: integer;
begin
  inherited;

  //Old Corvus5 Method
  {// joystick support
  SDL_JoystickEventState(SDL_IGNORE);
  SDL_InitSubSystem(SDL_INIT_JOYSTICK);
  if SDL_NumJoysticks <> 1 then
    Log.LogStatus('Joystick count <> 1', 'TJoy.Create');

  SDL_Joy := SDL_JoystickOpen(0);
  if SDL_Joy = nil then
    Log.LogError('SDL_JoystickOpen failed', 'TJoy.Create');

  if SDL_JoystickNumButtons(SDL_Joy) <> 16 then
    Log.LogStatus('Joystick button count <> 16', 'TJoy.Create');

//  SDL_JoystickEventState(SDL_ENABLE);
  // Events don't work - thay hang the whole application with SDL_JoystickEventState(SDL_ENABLE)

  // clear states
  for B := 0 to 15 do
    JoyUnit.Button[B].State := 1;

  // mapping
  JoyUnit.Button[1].Enabled := true;
  JoyUnit.Button[1].Type_ := SDL_KEYDOWN;
  JoyUnit.Button[1].Sym := SDLK_RETURN;
  JoyUnit.Button[2].Enabled := true;
  JoyUnit.Button[2].Type_ := SDL_KEYDOWN;
  JoyUnit.Button[2].Sym := SDLK_ESCAPE;

  JoyUnit.Button[12].Enabled := true;
  JoyUnit.Button[12].Type_ := SDL_KEYDOWN;
  JoyUnit.Button[12].Sym := SDLK_LEFT;
  JoyUnit.Button[13].Enabled := true;
  JoyUnit.Button[13].Type_ := SDL_KEYDOWN;
  JoyUnit.Button[13].Sym := SDLK_DOWN;
  JoyUnit.Button[14].Enabled := true;
  JoyUnit.Button[14].Type_ := SDL_KEYDOWN;
  JoyUnit.Button[14].Sym := SDLK_RIGHT;
  JoyUnit.Button[15].Enabled := true;
  JoyUnit.Button[15].Type_ := SDL_KEYDOWN;
  JoyUnit.Button[15].Sym := SDLK_UP;
  }
  //New Sarutas method
  SDL_JoystickEventState(SDL_IGNORE);
  SDL_InitSubSystem(SDL_INIT_JOYSTICK);
  if SDL_NumJoysticks < 1 then
  begin
    Log.LogError('No Joystick found');
    exit;
  end;


  SDL_Joy := SDL_JoystickOpen(0);
  if SDL_Joy = nil then
  begin
    Log.LogError('Could not Init Joystick');
    exit;
  end;
  //N := SDL_JoystickNumButtons(SDL_Joy);
  //if N < 6 then Log.LogStatus('Joystick button count < 6', 'TJoy.Create');

  for B := 0 to 5 do begin
    JoyUnit.Button[B].Enabled := true;
    JoyUnit.Button[B].State := 1;
    JoyUnit.Button[B].Type_ := SDL_KEYDOWN;
  end;

  JoyUnit.Button[0].Sym := SDLK_Return;
  JoyUnit.Button[1].Sym := SDLK_Escape;
  JoyUnit.Button[2].Sym := SDLK_M;
  JoyUnit.Button[3].Sym := SDLK_R;

  JoyUnit.Button[4].Sym := SDLK_RETURN;
  JoyUnit.Button[5].Sym := SDLK_ESCAPE;

  //Set HatState
  for B := 0 to 3 do begin
    JoyUnit.HatState[B].Enabled := true;
    JoyUnit.HatState[B].State := False;
    JoyUnit.HatState[B].Type_ := SDL_KEYDOWN;
  end;

  JoyUnit.HatState[0].Sym := SDLK_UP;
  JoyUnit.HatState[1].Sym := SDLK_RIGHT;
  JoyUnit.HatState[2].Sym := SDLK_DOWN;
  JoyUnit.HatState[3].Sym := SDLK_LEFT;
end;

procedure TJoy.Update;
var
  B:      integer;
  State:  UInt8;
  Tick:   Cardinal;
  Axes:   Smallint;
begin
  SDL_JoystickUpdate;

  //Manage Buttons
  for B := 0 to 15 do begin
    if (JoyUnit.Button[B].Enabled) and (JoyUnit.Button[B].State <> SDL_JoystickGetButton(SDL_Joy, B)) and (JoyUnit.Button[B].State = 0) then begin
      JoyEvent.type_ := JoyUnit.Button[B].Type_;
      JoyEvent.key.keysym.sym := JoyUnit.Button[B].Sym;
      SDL_PushEvent(@JoyEvent);
    end;
  end;


  for B := 0 to 15 do begin
    JoyUnit.Button[B].State := SDL_JoystickGetButton(SDL_Joy, B);
  end;

  //Get Tick
  Tick := SDL_GetTicks();

  //Get CoolieHat
  if (SDL_JoystickNumHats(SDL_Joy)>=1) then
    State := SDL_JoystickGetHat(SDL_Joy, 0)
  else
    State := 0;

  //Get Axis  
  if (SDL_JoystickNumAxes(SDL_Joy)>=2) then
  begin
    //Down - Up (X- Axis)
    Axes := SDL_JoystickGetAxis(SDL_Joy, 1);
    If Axes >= 15000 then
      State := State or SDL_HAT_Down
    Else If Axes <= -15000 then
      State := State or SDL_HAT_UP;

    //Left - Right (Y- Axis)
    Axes := SDL_JoystickGetAxis(SDL_Joy, 0);
    If Axes >= 15000 then
      State := State or SDL_HAT_Right
    Else If Axes <= -15000 then
      State := State or SDL_HAT_Left;
  end;

  //Manage Hat and joystick Events
  if (SDL_JoystickNumHats(SDL_Joy)>=1) OR (SDL_JoystickNumAxes(SDL_Joy)>=2) then
  begin

    //Up Button
    If (JoyUnit.HatState[0].Enabled) and ((SDL_HAT_UP AND State) = SDL_HAT_UP) then
    begin //IF Button is newly Pressed or if he is Pressed longer than 500 msecs
      if (JoyUnit.HatState[0].State = False) OR (JoyUnit.HatState[0].Lasttick < Tick) then
      begin
        //Set Tick and State
        if JoyUnit.HatState[0].State then
          JoyUnit.HatState[0].Lasttick := Tick + 200
        else
          JoyUnit.HatState[0].Lasttick := Tick + 500;

        JoyUnit.HatState[0].State := True;

        JoyEvent.type_ := JoyUnit.HatState[0].Type_;
        JoyEvent.key.keysym.sym := JoyUnit.HatState[0].Sym;
        SDL_PushEvent(@JoyEvent);
      end;
    end
    else
      JoyUnit.HatState[0].State := False;

    //Right Button
    If (JoyUnit.HatState[1].Enabled) and ((SDL_HAT_RIGHT AND State) = SDL_HAT_RIGHT) then
    begin //IF Button is newly Pressed or if he is Pressed longer than 500 msecs
      if (JoyUnit.HatState[1].State = False) OR (JoyUnit.HatState[1].Lasttick < Tick) then
      begin
        //Set Tick and State
        if JoyUnit.HatState[1].State then
          JoyUnit.HatState[1].Lasttick := Tick + 200
        else
          JoyUnit.HatState[1].Lasttick := Tick + 500;

        JoyUnit.HatState[1].State := True;
        
        JoyEvent.type_ := JoyUnit.HatState[1].Type_;
        JoyEvent.key.keysym.sym := JoyUnit.HatState[1].Sym;
        SDL_PushEvent(@JoyEvent);
      end;
    end
    else
      JoyUnit.HatState[1].State := False;

    //Down button
    If (JoyUnit.HatState[2].Enabled) and ((SDL_HAT_DOWN AND State) = SDL_HAT_DOWN) then
    begin //IF Button is newly Pressed or if he is Pressed longer than 230 msecs
      if (JoyUnit.HatState[2].State = False) OR (JoyUnit.HatState[2].Lasttick < Tick) then
      begin
        //Set Tick and State
        if JoyUnit.HatState[2].State then
          JoyUnit.HatState[2].Lasttick := Tick + 200
        else
          JoyUnit.HatState[2].Lasttick := Tick + 500;

        JoyUnit.HatState[2].State := True;
        
        JoyEvent.type_ := JoyUnit.HatState[2].Type_;
        JoyEvent.key.keysym.sym := JoyUnit.HatState[2].Sym;
        SDL_PushEvent(@JoyEvent);
      end;
    end
    else
      JoyUnit.HatState[2].State := False;

    //Left Button
    If (JoyUnit.HatState[3].Enabled) and ((SDL_HAT_LEFT AND State) = SDL_HAT_LEFT) then
    begin //IF Button is newly Pressed or if he is Pressed longer than 230 msecs
      if (JoyUnit.HatState[3].State = False) OR (JoyUnit.HatState[3].Lasttick < Tick) then
      begin
        //Set Tick and State
        if JoyUnit.HatState[3].State then
          JoyUnit.HatState[3].Lasttick := Tick + 200
        else
          JoyUnit.HatState[3].Lasttick := Tick + 500;

        JoyUnit.HatState[3].State := True;
        
        JoyEvent.type_ := JoyUnit.HatState[3].Type_;
        JoyEvent.key.keysym.sym := JoyUnit.HatState[3].Sym;
        SDL_PushEvent(@JoyEvent);
      end;
    end
    else
      JoyUnit.HatState[3].State := False;
  end;

end;

end.
