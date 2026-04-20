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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UJoystick.pas $
 * $Id: UJoystick.pas 1485 2008-10-28 20:16:05Z tobigun $
 *}

unit UJoystick;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  strutils,
  typinfo, // for GetEnumName
  fgl, // TGFMap
  math,
  SDL2;

const

  JOYSTICK_MOUSE_REPEAT       = 1;
  JOYSTICK_MOUSE_DEADZONE     = 0.01;
  JOYSTICK_MOUSE_DEFAULTSPEED = 550;

  JOYSTICK_AXIS_PRESSED_THRESHOLD = 0.2;
  JOYSTICK_AXIS_RELEASED_THRESHOLD = 0.7;
  JOYSTICK_AXIS_REPEAT_THRESHOLD = 0.8;
  JOYSTICK_AXIS_REPEAT_TIME = 150; // milliseconds to ignore repeated input
  JOYSTICK_AXIS_MAX_RANGE = 32767; // SDL ranges -32768 to 32767

  { Missing SDL header constants }

  SDL_HAT_CENTERED  = $0000;
  SDL_HAT_UP        = $0001;
  SDL_HAT_RIGHT     = $0002;
  SDL_HAT_DOWN      = $0004;
  SDL_HAT_LEFT      = $0008;
  SDL_HAT_RIGHTUP   = SDL_HAT_RIGHT or SDL_HAT_UP;
  SDL_HAT_RIGHTDOWN = SDL_HAT_RIGHT or SDL_HAT_DOWN;
  SDL_HAT_LEFTUP    = SDL_HAT_LEFT or SDL_HAT_UP;
  SDL_HAT_LEFTDOWN  = SDL_HAT_LEFT or SDL_HAT_DOWN;

  SDL_PRESSED = 1;
  SDL_RELEASED = 0;

  { custom SDL mapping for Axis-mapping }

  // Axis mapping names for legacy Joystick to button/key mapping
  SDL_HAT_AXIS_Center = 0;
  SDL_HAT_AXIS_X_pos = 1;
  SDL_HAT_AXIS_X_neg = 2;
  SDL_HAT_AXIS_X = 3;  // unused
  SDL_HAT_AXIS_Y_pos = 4;
  SDL_HAT_AXIS_Y_neg = 8;
  SDL_HAT_AXIS_Y = 12; // unused
  SDL_HAT_AXIS_X_pos_Y_pos = SDL_HAT_AXIS_X_pos or SDL_HAT_AXIS_Y_pos; // 5  Top Right
  SDL_HAT_AXIS_X_pos_Y_neg = SDL_HAT_AXIS_X_pos or SDL_HAT_AXIS_Y_neg; // 9  Bottom Right
  SDL_HAT_AXIS_Y_pos_X_neg = SDL_HAT_AXIS_Y_pos or SDL_HAT_AXIS_X_neg; // 6  Bottom Left
  SDL_HAT_AXIS_Y_neg_X_neg = SDL_HAT_AXIS_Y_neg or SDL_HAT_AXIS_X_neg; // 10 Top Left

type
  TJoyButtonState = ( bsReleased, bsPressed );
  TJoyControllerType = ( ctJoystick, ctGameController );

  TJoyMouse = record
    X, Y: integer;
    DeltaX, DeltaY: double;
    Time: LongInt;
    IsSet: boolean;
  end;

  PControllerDPadState = ^TControllerDPadState;
  TControllerDPadState = record
    X: boolean;
    Y: boolean;

    RawX: integer;
    RawY: integer;
  end;

  PControllerAxisState = ^TControllerAxisState;
  TControllerAxisState = record
    Perc: double;

    Time: Cardinal; // used to prevent additional 'repeated' press when moving Axis into one side fast enough
    Pressed: boolean;
    Repeat_: boolean;
    WasRepeat: boolean;
  end;

  TControllerDPadIDStateMap = TFPGMap<integer,TControllerDPadState>;
  TControllerAxisIDStateMap = TFPGMap<integer,TControllerAxisState>;

  TJoyController = class
    private
      _DeviceId: integer;
      _Name: string;
      _Enabled: boolean;

      LastMouseState: TJoyMouse;
      MouseRepeatThread: PSDL_Thread;
      MouseRepeatThreadFlag: boolean;
      MouseMode: boolean;

    protected

      _Type: TJoyControllerType;
      DPadStates: TControllerDPadIDStateMap;
      AxesStates: TControllerAxisIDStateMap;

    private
      function GetDeviceId(): integer;
      function GetInstanceId(): integer;
      function GetName(): string;

      function GetControllerType(): TJoyControllerType;
      function GetControllerAxesCount(): integer; virtual;
      function GetControllerHatsCount(): integer; virtual;
      function GetControllerButtonCount(): integer; virtual;

      function SimulateKeyboard(Key: TSDL_KeyCode; Pressed: boolean; NoMouseOverride: boolean = false): boolean; virtual;

      procedure SimulateMouseSend();
      function SimulateMouse(ButtonId: byte; Pressed: boolean): boolean; virtual; overload;
      function SimulateMouse(Axis: byte; Delta: real): boolean; virtual; overload;

    public
      constructor Create(DeviceId: integer; Name: string = ''); virtual;
      destructor Destroy(); override;

      property Name: string read GetName;
      property DeviceId: integer read GetDeviceId;
      property InstanceId: integer read GetInstanceId;

      function IsEnabled(): boolean;
      procedure SetEnabled(Enable: boolean);
      procedure Enable;
      procedure Disable;

      function GetJoystick(): Pointer; virtual; abstract;
      function ShouldIgnoreLegacy(): boolean; virtual;

      // actions
      function HandleControllerDPad(ControllerId: integer; PadId: integer; X, Y: integer): boolean;
      function HandleControllerMotion(ControllerId: integer; Axis: integer; Perc: double; Time: Cardinal): boolean;
      function HandleControllerButton(ButtonId: integer; State: TJoyButtonState): boolean;

      procedure SwitchMouseModeFor(ButtonId: integer; State: TJoyButtonState); virtual; abstract;

      // mapping
      function TranslateAxisToKey(Axis: integer; Direction: integer; out Key: TSDL_KeyCode): boolean; virtual;
      function TranslateAxisToMouseAxis(Axis: integer; Direction: integer; out MouseAxis: byte): boolean; virtual;
      function TranslateDPadToKey(Hat: integer; Axis: integer; Positive: boolean; out Key: TSDL_KeyCode): boolean; virtual;
      function TranslateButtonToKey(ButtonId: integer; State: TJoyButtonState; out Key: TSDL_KeyCode): boolean; virtual;
      function TranslateButtonToMouse(ButtonId: integer; State: TJoyButtonState; out Button: integer): boolean; virtual;

      property ControllerType: TJoyControllerType read GetControllerType;
      property Enabled: boolean read IsEnabled;

      property AxesCount: integer read GetControllerAxesCount;
      property HatsCount: integer read GetControllerHatsCount;
      property ButtonCount: integer read GetControllerButtonCount;

      // TODO: properly track/receive mouse coords in order to use relative mouse
      procedure OnMouseMove(X, Y: integer);

  end;

  TControllerIDMap = TFPGMap<integer,TJoyController>;

  TJoyControllerJoyStick = class(TJoyController)
    private
      JoyStick: PSDL_Joystick;

    public
      constructor Create(DeviceId: integer; JS: PSDL_Joystick; Name: string = ''); overload;
      destructor Destroy(); override;

      function GetJoystick(): Pointer; override;

      // mapping
      function TranslateAxisToKey(Axis: integer; Direction: integer; out Key: TSDL_KeyCode): boolean; override;
      function TranslateAxisToMouseAxis(Axis: integer; Direction: integer; out MouseAxis: byte): boolean; override;
      function TranslateDPadToKey(Hat: integer; Axis: integer; Positive: boolean; out Key: TSDL_KeyCode): boolean;  override;
      function TranslateButtonToKey(ButtonId: integer; State: TJoyButtonState; out Key: TSDL_KeyCode): boolean; override;
      function TranslateButtonToMouse(ButtonId: integer; State: TJoyButtonState; out Button: integer): boolean; override;

      procedure SwitchMouseModeFor(ButtonId: integer; State: TJoyButtonState); override;
  end;
  TJoyControllerGameController = class(TJoyController)
    private
      GameController: PSDL_GameController;

    public
      constructor Create(DeviceId: integer; GC: PSDL_GameController; Name: string = ''); overload;
      destructor Destroy(); override;

      function GetJoystick(): Pointer; override;
      function ShouldIgnoreLegacy(): boolean; override;

      // mapping
      function TranslateAxisToKey(Axis: integer; Direction: integer; out Key: TSDL_KeyCode): boolean; override;
      function TranslateAxisToMouseAxis(Axis: integer; Direction: integer; out MouseAxis: byte): boolean; override;
      function TranslateDPadToKey(Hat: integer; Axis: integer; Positive: boolean; out Key: TSDL_KeyCode): boolean; override;
      function TranslateButtonToKey(ButtonId: integer; State: TJoyButtonState; out Key: TSDL_KeyCode): boolean; override;
      function TranslateButtonToMouse(ButtonId: integer; State: TJoyButtonState; out Button: integer): boolean; override;

      procedure SwitchMouseModeFor(ButtonId: integer; State: TJoyButtonState); override;
  end;

  TJoy = class

    private
      Controllers: TControllerIDMap;

    public
      constructor Create;
      destructor Destroy; override;

    private

      function AddController(DeviceId: integer; out Error: string): boolean;
      function RemoveController(InstanceId: integer): boolean;
      function GetControllerByDeviceId(Id: integer; out Controller: TJoyController): boolean;
      function GetControllerByInstanceId(Id: integer; out Controller: TJoyController): boolean;
      function HasControllerByDeviceId(Id: integer): boolean;
      function HasControllerByInstanceId(Id: integer): boolean;

    public

      // events
      { A new Game controller has been inserted into the system }
      procedure OnControllerAdded(DeviceId: integer);

      { An opened Game controller has been removed }
      procedure OnControllerRemoved(InstanceId: integer);

      { The controller mapping was updated }
      procedure OnControllerRemapped(InstanceId: integer);

      { Game controller DPad motion }
      procedure OnControllerDPad(id: integer; PadId: integer; X, Y: integer; Legacy: boolean = false);

      { Game controller axis motion }
      procedure OnControllerMotion(id: integer; Axis: integer; Perc: double; Time: Cardinal; Legacy: boolean = false);

      { Game controller button pressed/released }
      procedure OnControllerButton(id: integer; ButtonId: integer; State: TJoyButtonState; Legacy: boolean = false);

      { called when real mouse moved }
      procedure OnMouseMove(X, Y: integer);

  end;

var
  Joy:        TJoy;

procedure InitializeJoystick;
procedure FinalizeJoyStick;
function HasJoyStick: boolean;
function ShouldSimulateJoystickKeyInput(Event: TSDL_event): boolean;

procedure OnJoystickPollEvent(Event: TSDL_event);

function ifthen(val:boolean;const iftrue:TJoyButtonState; const iffalse:TJoyButtonState = bsReleased): TJoyButtonState; overload;
function ifthen(val:boolean;const iftrue:TSDL_KeyCode; const iffalse:TSDL_KeyCode = 0): TSDL_KeyCode; overload;

function MouseRepeatHandlerFunc(Data: Pointer): integer; cdecl;// forward;

implementation

uses SysUtils,
     ULog;

procedure InitializeJoystick;
begin
  if assigned(Joy) then Exit;
  Log.LogStatus('Initialize Joystick', 'Initialization');
  Joy := TJoy.Create;
end;

procedure FinalizeJoyStick;
begin
  if assigned(Joy) then
  begin
    Log.LogStatus('Uninitialize Joystick', 'Finalization');
    Joy.Destroy;
    Joy := nil;
  end;
end;

function HasJoyStick: boolean;
begin
  Result := assigned(Joy);
end;

procedure OnJoystickPollEvent(Event: TSDL_event);
begin
  // sanity check
  if (Joy = nil) then Exit;

  // the LogDebug statements here are commented out because this is extremely spammy if a joystick/controller is used
  case Event.type_ of
  SDL_JOYAXISMOTION:
    with Event.jaxis do
    begin
      // Log.LogDebug(Format('JOYAXISMOTION [%d] Axis:%d  Value:%d  Time:%d', [which, axis, value, timestamp]), 'TJoy.Polling');
      Joy.OnControllerMotion(which, axis, EnsureRange((1.0*value) / JOYSTICK_AXIS_MAX_RANGE, -1.0, 1.0), timestamp, true);
    end;

  SDL_JOYHATMOTION:
    with Event.jhat do
    begin
      // Log.LogDebug(Format('JOYHATMOTION [%d] Pad:%d  Value:%d  Time:%d', [which, hat, value, timestamp]), 'TJoy.Polling');
      Joy.OnControllerDPad(which, hat, ifthen(value and SDL_HAT_LEFT <> 0, -1, ifthen(value and SDL_HAT_RIGHT <> 0, 1, 0)),
                                       ifthen(value and SDL_HAT_DOWN <> 0, -1, ifthen(value and SDL_HAT_UP <> 0, 1, 0)),
                                       true);
    end;
  SDL_JOYBUTTONUP, SDL_JOYBUTTONDOWN:
    with Event.jbutton do
    begin
      // Log.LogDebug(Format('JOYBUTTON [%d] Button:%d  State:%d  Type:%d  Time:%d', [which, button, state, type_, timestamp]), 'TJoy.Polling');
      Joy.OnControllerButton(which, button, ifthen(state = SDL_PRESSED, bsPressed, bsReleased), true);
    end;

  SDL_CONTROLLERDEVICEADDED:
    begin
      // Log.LogDebug(Format('CONTROLLERDEVICEADDED [DeviceID=%d]', [Event.cdevice.which]), 'TJoy.Polling');
      Joy.OnControllerAdded(Event.cdevice.which);
    end;
  SDL_CONTROLLERDEVICEREMOVED:
    begin
      // Log.LogDebug(Format('CONTROLLERDEVICEREMOVED [InstanceID=%d]', [Event.cdevice.which]), 'TJoy.Polling');
      Joy.OnControllerRemoved(Event.cdevice.which);
    end;
  SDL_CONTROLLERDEVICEREMAPPED:
    begin
      // Log.LogDebug(Format('CONTROLLERDEVICEREMAPPED [InstanceID=%d]', [Event.cdevice.which]), 'TJoy.Polling');
      Joy.OnControllerRemapped(Event.cdevice.which);
    end;

  SDL_CONTROLLERAXISMOTION:
    with Event.caxis do
    begin
      // Log.LogDebug(Format('CONTROLLERAXISMOTION [%d] Axis:%d  Value:%d  Time:%d', [which, axis, value, timestamp]), 'TJoy.Polling');
      Joy.OnControllerMotion(which, axis, EnsureRange((1.0*value) / JOYSTICK_AXIS_MAX_RANGE, -1.0, 1.0), timestamp);
    end;
  SDL_CONTROLLERBUTTONDOWN:
    with Event.cbutton do
    begin
      // Log.LogDebug(Format('CONTROLLERBUTTONDOWN [%d] Button:%d  State:%d  Type:%d  Time:%d', [which, button, state, type_, timestamp]), 'TJoy.Polling');
      Joy.OnControllerButton(which, button, ifthen(state = SDL_PRESSED, bsPressed, bsReleased));
    end;
  SDL_CONTROLLERBUTTONUP:
    with Event.cbutton do
    begin
      // Log.LogDebug(Format('CONTROLLERBUTTONUP [%d] Button:%d  State:%d  Type:%d  Time:%d', [which, button, state, type_, timestamp]), 'TJoy.Polling');
      Joy.OnControllerButton(which, button, ifthen(state = SDL_PRESSED, bsPressed, bsReleased));
    end;
  SDL_JOYDEVICEADDED:
    begin
      Joy := TJoy.Create;
    end;
  end; // case
end;

function MouseRepeatHandlerFunc(Data: Pointer): integer; cdecl;
var
  JoyC: TJoyController;
begin
  JoyC := TJoyController(Data);
  while (JoyC <> nil) and (JoyC.MouseRepeatThreadFlag) do
  begin
    JoyC.SimulateMouseSend();
    Sleep(JOYSTICK_MOUSE_REPEAT);
  end;

  Result := 0;
end;

constructor TJoy.Create;
var
  Controller, LoopController: TJoyController;
  Error: string;

  N: integer;
  I: integer;
begin
  inherited;

  Controllers := TControllerIDMap.Create;
  Controllers.Sorted := true;
  Controller := nil;

  SDL_InitSubSystem( SDL_INIT_JOYSTICK or SDL_INIT_GAMECONTROLLER );
  N := SDL_NumJoysticks;
  Log.LogStatus(Format('Joystick count: %d', [N]), 'TJoy.Create');
  for i := 0 to n-1 do
  begin
    if not AddController(i, Error) then
    begin
      Log.LogError(Error, 'TJoy.Create');
    end;
  end;

  if Controllers.Count < 1 then
  begin
    Log.LogStatus('No GameController/Joystick connected.', 'TJoy.Create');
    Exit;
  end;

  // if only 1 controller, just activate it
  if Controllers.Count = 1 then
  begin
    Controller := Controllers.Data[0];
    Log.LogStatus(Format('Using controller: %s', [Controller.Name]), 'TJoy.Create');
  end;


  if not assigned(Controller) then
  begin
    // try finding game controller with best button count
    for i := 0 to Controllers.Count -1 do
    begin
      LoopController := Controllers.Data[i];
      if (LoopController.ControllerType = ctGameController) and ((not assigned(Controller)) or (LoopController.ButtonCount > Controller.ButtonCount)) then begin
        Controller := LoopController;
      end;
    end;

    if assigned(Controller) then begin
      Log.LogStatus(Format('Using game controller: %s', [Controller.Name]), 'TJoy.Create');
    end;
  end;

  // fallback using best legacy joystick
  if not assigned(Controller) then
  begin
    // try finding joystick with best button count
    for i := 0 to Controllers.Count -1 do
    begin
      LoopController := Controllers.Data[i];
      if (LoopController.ControllerType = ctJoystick) and ((not assigned(Controller)) or (LoopController.ButtonCount > Controller.ButtonCount)) then begin
        Controller := LoopController;
      end;
    end;

    if assigned(Controller) then begin
      Log.LogStatus(Format('Using legacy Joystick: %s', [Controller.Name]), 'TJoy.Create');
    end;
  end;

  if not assigned(Controller) then
  begin
    Log.LogWarn('Unable to find a suited controller', 'TJoy.Create');
    Exit;
  end;

  Log.LogStatus(Format('Enable input for controller %s [ID=%d]', [Controller.Name, Controller.DeviceId]), 'TJoy.Create');
  Controller.Enable;
end;

destructor TJoy.Destroy;
var
  i, index: integer;
begin
  inherited;

  if Controllers <> nil then
  begin

    for i := 0 to Controllers.Count - 1 do
      if Controllers.Find(Controllers.Keys[i], index) and assigned(Controllers.Data[index]) then
        Controllers.Data[index].Destroy();

    Controllers.Clear;
    Controllers.Free;
    Controllers := nil;
  end;
end;

function TJoy.AddController(DeviceId: integer; out Error: string): boolean;
var
  JoyStick:    PSDL_Joystick;
  GameController:    PSDL_GameController;
  Controller: TJoyController;
  s: string;
begin
  Result := true;
  if SDL_IsGameController(DeviceId) = SDL_TRUE then
  begin
    s := SDL_GameControllerNameForIndex(DeviceId);
    Log.LogStatus(Format('Is a game controller: %s [ID=%d]', [s, DeviceId]), 'TJoy.AddController');
    GameController := SDL_GameControllerOpen(DeviceId);
    if GameController = nil then
    begin
      Error := 'SDL_GameControllerOpen failed';
      Result := false;
    end
    else
    begin
      Controller := TJoyControllerGameController.Create(DeviceId, GameController, s);
      Controllers.Add(Controller.InstanceId, Controller);
    end;
  end
  else
  begin
    s := SDL_JoystickNameForIndex(DeviceId);
    Log.LogStatus(Format('Is a Joystick: %s [ID=%d]', [s, DeviceId]), 'TJoy.AddController');
    JoyStick := SDL_JoystickOpen(DeviceId);
    if JoyStick = nil then
    begin
      Error := 'SDL_JoystickOpen failed';
      Result := false;
    end
    else
    begin
      Controller := TJoyControllerJoyStick.Create(DeviceId, JoyStick, s);
      Controllers.Add(Controller.InstanceId, TJoyControllerJoyStick.Create(DeviceId, JoyStick, s));
    end;
  end;

  if Controller <> nil then
  begin
    Log.LogStatus(Format('Controller@%d  %d Buttons  %d Axes  %d Hats/DPads', [DeviceId, Controller.ButtonCount, Controller.AxesCount, Controller.HatsCount]), 'TJoy.AddController');
  end;
end;

function TJoy.RemoveController(InstanceId: integer): boolean;
var Controller: TJoyController;
begin
  Result := false;
  if GetControllerByInstanceId(InstanceId, Controller) then
  begin
    Controllers.Remove(Controller.DeviceId);
    Controller.Destroy;
    Result := true;
  end
  else Log.LogError(Format('Unable to disconnect controller with Instance ID: %d', [InstanceId]), 'TJoy.RemoveController');
end;

function TJoy.GetControllerByInstanceId(Id: integer; out Controller: TJoyController): boolean;
var
  index: integer;
begin
  Result := false;
  if Controllers.Find(Id, index) then
  begin
    Controller := Controllers.Data[index];
    Result := true;
  end;
end;

function TJoy.GetControllerByDeviceId(Id: integer; out Controller: TJoyController): boolean;
var
  i: integer;
begin
  Result := false;

  for i := 0 to Controllers.Count - 1 do
  begin
    if (Controllers.Data[i] <> nil) and (Id = Controllers.Data[i].DeviceId) then
    begin
      Controller := Controllers.Data[i];
      Result := true;
      Exit;
    end;
  end;
end;

function TJoy.HasControllerByInstanceId(Id: integer): boolean;
var Controller: TJoyController;
begin
  Result := GetControllerByInstanceId(Id, Controller);
end;

function TJoy.HasControllerByDeviceId(Id: integer): boolean;
var Controller: TJoyController;
begin
  Result := GetControllerByDeviceId(Id, Controller);
end;

procedure TJoy.OnControllerAdded(DeviceId: integer);
var Error: string;
begin
  if not HasControllerByDeviceId(DeviceId) then
  begin
    if not AddController(DeviceId, Error) then Log.LogError(Error, 'TJoy.Connect');
  end;
end;

procedure TJoy.OnControllerRemoved(InstanceId: integer);
begin
  RemoveController(InstanceId);
end;

procedure TJoy.OnControllerRemapped(InstanceId: integer);
begin
  // TODO: TJoy.OnControllerRemapped. Not implemented
end;

procedure TJoy.OnControllerDPad(id: integer; PadId: integer; X, Y: integer; Legacy: boolean);
var
  Controller: TJoyController;
begin

  // ignore unknown or disabled input
  if not GetControllerByInstanceId(id, Controller) or not Controller.IsEnabled() then Exit;

  // ignore lecacy events for given controller
  if Legacy and Controller.ShouldIgnoreLegacy() then Exit;

  Log.LogInfo(Format('DPad   [%d] PadId:%d  X: %d  Y: %d', [id, PadId, X, Y]), 'TJoy.Input');
  if not Controller.HandleControllerDPad(id, PadId, X, Y) then
  begin
    Log.LogWarn(Format('Unable to handle DPad input of ''%s'' [ID:%d]', [Controller.Name, id]), 'TJoy.Input');
  end;
end;

procedure TJoy.OnControllerMotion(id: integer; Axis: integer; Perc: double; Time: Cardinal; Legacy: boolean);
var
  Controller: TJoyController;
begin

  // ignore unknown or disabled input
  if not GetControllerByInstanceId(id, Controller) or not Controller.IsEnabled() then Exit;

  // ignore lecacy events for given controller
  if Legacy and Controller.ShouldIgnoreLegacy() then Exit;

  Log.LogInfo(Format('Motion [%d] Axis:%d  Perc: %0.3f', [id, Axis, Perc]), 'TJoy.Input');
  if not Controller.HandleControllerMotion(id, Axis, Perc, Time) then
  begin
    Log.LogWarn(Format('Unable to handle axis input of ''%s'' [ID:%d]', [Controller.Name, id]), 'TJoy.Input');
  end;
end;

procedure TJoy.OnControllerButton(id: integer; ButtonId: integer; State: TJoyButtonState; Legacy: boolean);
var
  Controller: TJoyController;
begin

  // ignore unknown or disabled input
  if not GetControllerByInstanceId(id, Controller) or not Controller.IsEnabled() then Exit;

  // ignore lecacy events for given controller
  if Legacy and Controller.ShouldIgnoreLegacy() then Exit;

  Log.LogInfo(Format('Button [%d] Button: %d  State: %s', [id, ButtonId, GetEnumName(TypeInfo(TJoyButtonState), Ord(State))]), 'TJoy.Input');
  if not Controller.HandleControllerButton(ButtonId, State) then
  begin
    Log.LogDebug(Format('Unable to handle button input of ''%s'' [ID:%d]', [Controller.Name, id]), 'TJoy.Input');
  end;
end;

// workaround for relative mouse
// TODO: implement simulating mouse properly (access to current cached mouse coors)
procedure TJoy.OnMouseMove(X, Y: integer);
var
  i, index: integer;
begin
  for i := 0 to Controllers.Count - 1 do
    if Controllers.Find(Controllers.Keys[i], index) and Controllers.Data[index].IsEnabled() then
      Controllers.Data[index].OnMouseMove(X, Y);
end;


{ Controllers }
{ ~~~~~~~~~~~~~~~~~~~~~~~}

constructor TJoyController.Create(DeviceId: integer; Name: string);
begin
  //inherited;

  DPadStates := TControllerDPadIDStateMap.Create;
  DPadStates.Sorted := true;
  AxesStates := TControllerAxisIDStateMap.Create;
  AxesStates.Sorted := true;

  MouseMode := false;

  self._DeviceId := DeviceId;
  self._Name := Name;
end;

destructor TJoyController.Destroy();
begin
  inherited;

  MouseRepeatThread := nil;

  DPadStates.Free;
  AxesStates.Free;
end;

function TJoyController.GetDeviceId(): integer;
begin
  Result := _DeviceId;
end;
function TJoyController.GetInstanceId(): integer;
begin
  Result := SDL_JoystickInstanceID(GetJoystick());
end;
function TJoyController.GetName(): string;
begin
  Result := _Name;
end;

function TJoyController.IsEnabled(): boolean;
begin
  Result := _Enabled;
end;
procedure TJoyController.Enable;
begin
  SetEnabled(true);
end;
procedure TJoyController.Disable;
begin
  SetEnabled(false);
end;

procedure TJoyController.SetEnabled(Enable: boolean);
begin
  _Enabled := Enable;
end;

function TJoyController.GetControllerType(): TJoyControllerType;
begin
  Result := _Type
end;

function TJoyController.ShouldIgnoreLegacy(): boolean;
begin
  Result := false;
end;

// TODO: Move to Joystick manager
function TJoyController.SimulateKeyboard(Key: TSDL_KeyCode; Pressed: boolean; NoMouseOverride: boolean): boolean;
var
  JoyEvent: TSDL_Event;
begin
  Result := true;

  Log.LogInfo(Format('Simulate Key: %s  Pressed: %d', [SDL_GetScancodeName(SDL_GetScancodeFromKey(Key)), integer(Pressed)]), 'TJoy.Simulate');

  // switch back to non-mouse mode (alias keyboard simulation)
  if not NoMouseOverride then MouseMode := false;

  JoyEvent := Default(TSDL_Event);
  JoyEvent.type_ := ifthen(Pressed, SDL_KEYDOWN, SDL_KEYUP);
  JoyEvent.key.keysym.sym := Key;
  JoyEvent.key.keysym.scancode := SDL_GetScancodeFromKey(Key);

  // set un-defined unicode char to distinguish keyboard simulated events from real input events as workaround. Check UMain.CheckEvents and ShouldSimulateJoystickKeyInput
  JoyEvent.key.keysym.unicode := SizeOf(Uint32);

  SDL_PushEvent(@JoyEvent);
end;

function ShouldSimulateJoystickKeyInput(Event: TSDL_event): boolean;
begin
  Result := HasJoyStick() and (Event.key.keysym.unicode = SizeOf(Uint32));
end;

// TODO: Move to Joystick manager
procedure TJoyController.SimulateMouseSend();
var
  MouseEvent: TSDL_Event;
  DiffTime: integer;
  Speed: real;
begin

  // directly set mouse which switches button simulation for mouse
  MouseMode := true;

  DiffTime := SDL_GetTicks() - LastMouseState.Time;
  Speed := (DiffTime / 1000.0) * JOYSTICK_MOUSE_DEFAULTSPEED;

  MouseEvent.type_ := SDL_MOUSEMOTION;
  MouseEvent.button.x := LastMouseState.X + Round(LastMouseState.DeltaX * Speed);
  MouseEvent.button.y := LastMouseState.Y + Round(LastMouseState.DeltaY * Speed);
  SDL_PushEvent(@MouseEvent);
end;

// TODO: Move to Joystick manager
function TJoyController.SimulateMouse(ButtonId: byte; Pressed: boolean): boolean;
var
  JoyEvent: TSDL_Event;
begin
  Result := true;

  JoyEvent := Default(TSDL_Event);
  JoyEvent.type_ := ifthen(Pressed, SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP);
  JoyEvent.button.button := ButtonId;

  // current SDL2 event handling requires having a valid position when sending mouse button presses
  // TODO: clean up mouse position emulation once SDL2 input handling is handling mouse button and motion input separately
  JoyEvent.button.X := LastMouseState.X;
  JoyEvent.button.Y := LastMouseState.Y;

  SDL_PushEvent(@JoyEvent);
end;

// TODO: Move to Joystick manager
function TJoyController.SimulateMouse(Axis: byte; Delta: real): boolean;
var
  mouseX, mouseY: integer;
begin
  Result := true;

  if not LastMouseState.IsSet then
  begin
    SDL_GetMouseState(@mouseX, @mouseY);
    LastMouseState.X := mousex;
    LastMouseState.Y := mousey;
    LastMouseState.Time := SDL_GetTicks();
  end;

  if Axis = 0 then LastMouseState.DeltaX := Delta
  else LastMouseState.DeltaY := Delta;

  SimulateMouseSend();

  // check whether we should stop the thread or re-create it, in order to send repeated simulated mouse events
  if (abs(LastMouseState.DeltaX) < JOYSTICK_MOUSE_DEADZONE) and (abs(LastMouseState.DeltaY) < JOYSTICK_MOUSE_DEADZONE) then
  begin
    if assigned(MouseRepeatThread) then
    begin
      MouseRepeatThreadFlag := false;
      MouseRepeatThread := nil;
    end;
  end
  else if not assigned(MouseRepeatThread) then
  begin
    MouseRepeatThreadFlag := true;
    MouseRepeatThread := SDL_CreateThread(@MouseRepeatHandlerFunc, nil, Self);
  end;
end;

function TJoyController.HandleControllerDPad(ControllerId: integer; PadId: integer; X, Y: integer): boolean;
var
  Index: integer;
  State: TControllerDPadState;
  Key: TSDL_KeyCode;
begin
  Result := false;

  State := Default(TControllerDPadState);
  if not DPadStates.Find(PadId, index) then DPadStates.Add(PadId, State);
  State := DPadStates.Data[index];

  if (X <> 0) xor State.X then
  begin
    if TranslateDPadToKey(PadId, ifthen(State.RawX + X > 0, SDL_HAT_AXIS_X_pos, SDL_HAT_AXIS_X_neg), X <> 0, Key) then
      SimulateKeyboard(Key, X <> 0);
  end;
  if (Y <> 0) xor State.Y then
  begin
    if TranslateDPadToKey(PadId, ifthen(State.RawY + Y > 0, SDL_HAT_AXIS_Y_pos, SDL_HAT_AXIS_Y_neg), Y <> 0, Key) then
      SimulateKeyboard(Key, Y <> 0);
  end;

  State.X := X <> 0;
  State.Y := Y <> 0;
  State.RawX := ifthen(State.X, X, 0);
  State.RawY := ifthen(State.Y, Y, 0);
  DPadStates.Data[index] := State;
  Result := true;
end;

function TJoyController.HandleControllerMotion(ControllerId: integer; Axis: integer; Perc: double; Time: Cardinal): boolean;
var
  Index: integer;
  State: TControllerAxisState;
  MouseAxis: Byte;
  Key: TSDL_KeyCode;
begin
  Result := false;

  State := Default(TControllerAxisState);
  if not AxesStates.Find(Axis, index) then AxesStates.Add(Axis, State);
  State := AxesStates.Data[index];

  if TranslateAxisToMouseAxis(Axis, Sign(Perc), MouseAxis) then begin
    SimulateMouse(MouseAxis, Perc);
  end
  else if TranslateAxisToKey(Axis, Sign(Perc), Key) then
  begin
    if not State.Pressed and not State.Repeat_ then
    begin
      if (abs(Perc) > JOYSTICK_AXIS_PRESSED_THRESHOLD) then
      begin
        State.Pressed := true;
        State.Time := Time;
        SimulateKeyboard(Key, true);
      end;
    end
    else if not State.Repeat_ then
    begin
      if (abs(Perc) < JOYSTICK_AXIS_PRESSED_THRESHOLD) then begin
        State.Pressed := false;
        if not State.WasRepeat then SimulateKeyboard(Key, false);
        State.WasRepeat := false;
      end else if (abs(Perc) > JOYSTICK_AXIS_REPEAT_THRESHOLD) then begin
        State.Repeat_ := true;

        if (Time - State.Time > JOYSTICK_AXIS_REPEAT_TIME) then begin
          State.Pressed := true;
          SimulateKeyboard(Key, true);
        end;
      end;
    end else if {State.Pressed and} (abs(Perc) < JOYSTICK_AXIS_RELEASED_THRESHOLD) then
    begin
      State.Repeat_ := false;
      State.WasRepeat := true;
      if State.Pressed then SimulateKeyboard(Key, false);
    end;
  end;

  State.Perc := Perc;
  AxesStates.Data[index] := State;
  Result := true;
end;

function TJoyController.HandleControllerButton(ButtonId: integer; State: TJoyButtonState): boolean;
var
  Key: TSDL_KeyCode;
  Button: integer;
begin
  Result := false;

  SwitchMouseModeFor(ButtonId, State);
  if MouseMode then
  begin
    if TranslateButtonToMouse(ButtonId, State, Button) then begin
      Result := SimulateMouse(Button, State = bsPressed);
    end else if TranslateButtonToKey(ButtonId, State, Key) then begin
      Result := SimulateKeyboard(Key, State = bsPressed, true);
    end;
  end else if TranslateButtonToKey(ButtonId, State, Key) then begin
    Result := SimulateKeyboard(Key, State = bsPressed);
  end;
end;

function TJoyController.GetControllerButtonCount(): integer;
begin
  Result := SDL_JoystickNumButtons(GetJoystick());
end;

function TJoyController.GetControllerAxesCount(): integer;
begin
  Result := SDL_JoystickNumAxes(GetJoystick());
end;

function TJoyController.GetControllerHatsCount(): integer;
begin
  Result := SDL_JoystickNumHats(GetJoystick());
end;

function TJoyController.TranslateAxisToKey(Axis: integer; Direction: integer; out Key: TSDL_KeyCode): boolean;
begin
  Result := false;
end;

function TJoyController.TranslateAxisToMouseAxis(Axis: integer; Direction: integer; out MouseAxis: byte): boolean;
begin
  Result := false;
end;

function TJoyController.TranslateDPadToKey(Hat: integer; Axis: integer; Positive: boolean; out Key: TSDL_KeyCode): boolean;
begin
  Result := false;
end;
function TJoyController.TranslateButtonToKey(ButtonId: integer; State: TJoyButtonState; out Key: TSDL_KeyCode): boolean;
begin
  Result := false;
end;
function TJoyController.TranslateButtonToMouse(ButtonId: integer; State: TJoyButtonState; out Button: integer): boolean;
begin
  Result := false;
end;

procedure TJoyController.OnMouseMove(X, Y: integer);
begin
  LastMouseState.X := X;
  LastMouseState.Y := Y;
  //LastMouseState.DeltaX := 0.0;
  //LastMouseState.DeltaY := 0.0;
  LastMouseState.Time := SDL_GetTicks();
  LastMouseState.IsSet := true;
end;

constructor TJoyControllerJoyStick.Create(DeviceId: integer; JS: PSDL_Joystick; Name: string);
begin
  inherited
  Create(DeviceId, Name);

  _Type := ctJoystick;
  self.JoyStick := JS;
end;

destructor TJoyControllerJoyStick.Destroy();
begin
  inherited;

  if assigned(JoyStick) then
  begin
    SDL_JoystickClose(JoyStick);
    JoyStick := nil;
  end;
end;

function TJoyControllerJoyStick.GetJoystick(): Pointer;
begin
  Result := JoyStick;
end;

constructor TJoyControllerGameController.Create(DeviceId: integer; GC: PSDL_GameController; Name: string);
begin
  inherited
  Create(DeviceId, Name);

  _Type := ctGameController;
  self.GameController := GC;
end;

destructor TJoyControllerGameController.Destroy();
begin
  inherited;

  if assigned(GameController) then
  begin
    SDL_GameControllerClose(GameController);
    GameController := nil;
  end;
end;

function TJoyControllerGameController.GetJoystick(): Pointer;
begin
  Result := SDL_GameControllerGetJoystick(GameController);
end;

function TJoyControllerGameController.ShouldIgnoreLegacy(): boolean;
begin
  Result := true;
end;


{ Helper }
{ ~~~~~~~~~~~~~~~~~~~~~~~}

function ifthen(val:boolean;const iftrue:TJoyButtonState; const iffalse:TJoyButtonState = bsReleased): TJoyButtonState;
begin
  if val then result:=iftrue else result:=iffalse;
end;

function ifthen(val:boolean;const iftrue:TSDL_KeyCode; const iffalse:TSDL_KeyCode = 0): TSDL_KeyCode;
begin
  if val then result:=iftrue else result:=iffalse;
end;

{ Key mapping }
{ ~~~~~~~~~~~~~~~~~~~~~~~}

function TJoyControllerJoyStick.TranslateAxisToKey(Axis: integer; Direction: integer; out Key: TSDL_KeyCode): boolean;
begin
  Result := false;
end;

function TJoyControllerJoyStick.TranslateAxisToMouseAxis(Axis: integer; Direction: integer; out MouseAxis: byte): boolean;
begin
  Result := true;
  case Axis of
    0, 1: MouseAxis := Axis;
    otherwise Result := false;
  end;
end;

function TJoyControllerJoyStick.TranslateDPadToKey(Hat: integer; Axis: integer; Positive: boolean; out Key: TSDL_KeyCode): boolean;
begin
  Result := false;

  if Hat = 0 then
  begin
    case Axis of
      SDL_HAT_AXIS_X_pos: Key := SDLK_RIGHT;
      SDL_HAT_AXIS_X_neg: Key := SDLK_LEFT;
      SDL_HAT_AXIS_Y_pos: Key := SDLK_UP;
      SDL_HAT_AXIS_Y_neg: Key := SDLK_DOWN;
      otherwise Exit;
    end;
    Result := true;
  end
  else if Hat = 1 then
  begin
    case Axis of
      SDL_HAT_AXIS_X_pos: Key := SDLK_KP_6;
      SDL_HAT_AXIS_X_neg: Key := SDLK_KP_4;
      SDL_HAT_AXIS_Y_pos: Key := SDLK_KP_8;
      SDL_HAT_AXIS_Y_neg: Key := SDLK_KP_2;
      SDL_HAT_AXIS_X_pos_Y_neg: Key := SDLK_KP_3;
      SDL_HAT_AXIS_X_pos_Y_pos: Key := SDLK_KP_9;
      SDL_HAT_AXIS_Y_neg_X_neg: Key := SDLK_KP_7;
      SDL_HAT_AXIS_Y_pos_X_neg: Key := SDLK_KP_1;

      // TODO: consider invalid buttons presses as NumPad5?
      //SDL_HAT_AXIS_X, SDL_HAT_AXIS_Y: Key := SDLK_KP_5;
      otherwise Exit;
    end;
    Result := true;
  end;
end;

function TJoyControllerJoyStick.TranslateButtonToMouse(ButtonId: integer; State: TJoyButtonState; out Button: integer): boolean;
begin
  Result := true;

  case ButtonId of
    0: Button := SDL_BUTTON_LEFT;
    1: Button := SDL_BUTTON_RIGHT;

    // TODO: Mouse wheel

    otherwise Result := false;
  end;
end;

function TJoyControllerJoyStick.TranslateButtonToKey(ButtonId: integer; State: TJoyButtonState; out Key: TSDL_KeyCode): boolean;
begin
  Result := true;

  // default configuration with 4 buttons
  case ButtonId of
    0: Key := SDLK_RETURN;
    1: Key := SDLK_ESCAPE;
    2: Key := SDLK_m;
    3: Key := SDLK_r;
    otherwise Result := false;
  end;
end;

procedure TJoyControllerJoyStick.SwitchMouseModeFor(ButtonId: integer; State: TJoyButtonState);
begin
  // no need to do anything in here.
  // whether we should switching Mosue mode is handled in DPad tracking, see SimulateKeyboard SimulateMouse
  // this method is required for controllers sending the digital pad buttons as Button presses (and not as Hat motion)
end;

function TJoyControllerGameController.TranslateAxisToKey(Axis: integer; Direction: integer; out Key: TSDL_KeyCode): boolean;
begin

  Result := false;

  if Direction > 0 then
  begin
    case Axis of
      SDL_CONTROLLER_AXIS_RIGHTX: Key := SDLK_RIGHT;
      SDL_CONTROLLER_AXIS_RIGHTY: Key := SDLK_UP;
      otherwise Exit;
    end;
    Result := true;
  end

  else if Direction < 0 then
  begin
    case Axis of
      SDL_CONTROLLER_AXIS_RIGHTX: Key := SDLK_LEFT;
      SDL_CONTROLLER_AXIS_RIGHTY: Key := SDLK_DOWN;
      otherwise Exit;
    end;
    Result := true;
  end;
end;

function TJoyControllerGameController.TranslateAxisToMouseAxis(Axis: integer; Direction: integer; out MouseAxis: byte): boolean;
begin
  Result := false;

  case Axis of
    SDL_CONTROLLER_AXIS_LEFTX: MouseAxis := 0;
    SDL_CONTROLLER_AXIS_LEFTY: MouseAxis := 1;
    otherwise Exit;
  end;
  Result := true;
end;

function TJoyControllerGameController.TranslateDPadToKey(Hat: integer; Axis: integer; Positive: boolean; out Key: TSDL_KeyCode): boolean;
begin
  Result := false;
end;

function TJoyControllerGameController.TranslateButtonToMouse(ButtonId: integer; State: TJoyButtonState; out Button: integer): boolean;
begin
  Result := true;

  case ButtonId of
    SDL_CONTROLLER_BUTTON_A: Button := SDL_BUTTON_LEFT;
    SDL_CONTROLLER_BUTTON_B: Button := SDL_BUTTON_RIGHT;
    SDL_CONTROLLER_BUTTON_LEFTSTICK,
    SDL_CONTROLLER_BUTTON_RIGHTSTICK: Button := SDL_BUTTON_MIDDLE;

    // TODO: Mouse wheel
    // Button := SDL_BUTTON_WHEELUP

    otherwise Result := false;
  end;
end;

function TJoyControllerGameController.TranslateButtonToKey(ButtonId: integer; State: TJoyButtonState; out Key: TSDL_KeyCode): boolean;
begin

  Result := true;
  case ButtonId of
    SDL_CONTROLLER_BUTTON_DPAD_UP: Key := SDLK_UP;
    SDL_CONTROLLER_BUTTON_DPAD_DOWN: Key := SDLK_DOWN;
    SDL_CONTROLLER_BUTTON_DPAD_LEFT: Key := SDLK_LEFT;
    SDL_CONTROLLER_BUTTON_DPAD_RIGHT: Key := SDLK_RIGHT;
    otherwise Result := false;
  end;

  // for mouse mode switching
  if Result then
  begin
    MouseMode := false;
    Exit;
  end;

  Result := true;
  if MouseMode then
  begin
    case ButtonId of
      SDL_CONTROLLER_BUTTON_START: Key := SDLK_RETURN;
      SDL_CONTROLLER_BUTTON_BACK: Key := SDLK_ESCAPE;
      otherwise Result := false;
    end;
  end
  else
  begin
    case ButtonId of
      SDL_CONTROLLER_BUTTON_A: Key := SDLK_RETURN;
      SDL_CONTROLLER_BUTTON_B: Key := SDLK_ESCAPE;
      SDL_CONTROLLER_BUTTON_Y: Key := SDLK_m;
      SDL_CONTROLLER_BUTTON_X: Key := SDLK_r;
      otherwise Result := false;
    end;
  end;
end;

procedure TJoyControllerGameController.SwitchMouseModeFor(ButtonId: integer; State: TJoyButtonState);
begin
  if MouseMode then
  begin
    case ButtonId of
      SDL_CONTROLLER_BUTTON_DPAD_UP,
      SDL_CONTROLLER_BUTTON_DPAD_DOWN,
      SDL_CONTROLLER_BUTTON_DPAD_LEFT,
      SDL_CONTROLLER_BUTTON_DPAD_RIGHT:
        MouseMode := false;
    end;
  end;
end;

end.
