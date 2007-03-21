unit UJoystick;

interface

uses SDL;

type
  TJoyButton = record
    State:      integer;
    Enabled:    boolean;
    Type_:      byte;
    Sym:        cardinal;
  end;

  TJoyUnit = record
    Button:   array[0..15] of TJoyButton;
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

uses SysUtils;

constructor TJoy.Create;
var
  B, N:    integer;
begin
  //Old Corvus5 Method
  {// joystick support
  SDL_JoystickEventState(SDL_IGNORE);
  SDL_InitSubSystem(SDL_INIT_JOYSTICK);
  if SDL_NumJoysticks <> 1 then beep;

  SDL_Joy := SDL_JoystickOpen(0);
  if SDL_Joy = nil then beep;

  if SDL_JoystickNumButtons(SDL_Joy) <> 16 then beep;

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
  if SDL_NumJoysticks < 1 then beep;

  SDL_Joy := SDL_JoystickOpen(0);
  if SDL_Joy = nil then beep;

  N := SDL_JoystickNumButtons(SDL_Joy);
  if N < 6 then beep;

  for B := 0 to 5 do begin
    JoyUnit.Button[B].Enabled := true;
    JoyUnit.Button[B].State := 1;
    JoyUnit.Button[B].Type_ := SDL_KEYDOWN;
  end;

  JoyUnit.Button[0].Sym := SDLK_UP;
  JoyUnit.Button[1].Sym := SDLK_RIGHT;
  JoyUnit.Button[2].Sym := SDLK_DOWN;
  JoyUnit.Button[3].Sym := SDLK_LEFT;

  JoyUnit.Button[4].Sym := SDLK_RETURN;
  JoyUnit.Button[5].Sym := SDLK_ESCAPE;
end;

procedure TJoy.Update;
var
  B:    integer;
begin
  SDL_JoystickUpdate;

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

end;

end.
