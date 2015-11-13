unit sdlinput;
{
  $Id: sdlinput.pas,v 1.9 2007/08/22 21:18:43 savage Exp $
  
}
{******************************************************************************}
{                                                                              }
{          JEDI-SDL : Pascal units for SDL - Simple DirectMedia Layer          }
{                     SDL Input Wrapper                                        }
{                                                                              }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominique Louis are                                      }
{ Copyright (C) 2003 - 2100 Dominique Louis.                                   }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{   SDL Mouse, Keyboard and Joystick wrapper                                   }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.dll on Windows platforms                                               }
{   libSDL-1.1.so.0 on Linux platform                                          }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{ March      12     2003 - DL : Initial creation                               }
{                                                                              }
{ February   02     2004 - DL : Added Custom Cursor Support to the Mouse class }
{
  $Log: sdlinput.pas,v $
  Revision 1.9  2007/08/22 21:18:43  savage
  Thanks to Dean for his MouseDelta patch.

  Revision 1.8  2005/08/03 18:57:32  savage
  Various updates and additions. Mainly to handle OpenGL 3D Window support and better cursor support for the mouse class

  Revision 1.7  2004/09/30 22:32:04  savage
  Updated with slightly different header comments

  Revision 1.6  2004/09/12 21:52:58  savage
  Slight changes to fix some issues with the sdl classes.

  Revision 1.5  2004/05/10 21:11:49  savage
  changes required to help get SoAoS off the ground.

  Revision 1.4  2004/05/03 22:38:40  savage
  Added the ability to enable or disable certain inputs @ runtime. Basically it just does not call UpdateInput if Enabled = false.
  Can also disable and enable input devices via the InputManager.

  Revision 1.3  2004/04/28 21:27:01  savage
  Updated Joystick code and event handlers. Needs testing...

  Revision 1.2  2004/02/14 22:36:29  savage
  Fixed inconsistencies of using LoadLibrary and LoadModule.
  Now all units make use of LoadModule rather than LoadLibrary and other dynamic proc procedures.

  Revision 1.1  2004/02/05 00:08:20  savage
  Module 1.0 release


}
{******************************************************************************}

interface

{$i jedi-sdl.inc}

uses
  Classes,
  sdl;

type
  TSDLInputType = ( itJoystick , itKeyBoard, itMouse );
  TSDLInputTypes = set of TSDLInputType;

  TSDLCustomInput = class( TObject )
  private
    FEnabled: Boolean;
  public
    constructor Create;
    function UpdateInput( event: TSDL_EVENT ) : Boolean; virtual; abstract;
    property Enabled : Boolean read FEnabled write FEnabled;
  end;

  TSDLJoyAxisMoveEvent =  procedure ( Which: UInt8; Axis: UInt8; Value: SInt16 ) {$IFNDEF NOT_OO}of object{$ENDIF};
  TSDLJoyBallMoveEvent =  procedure ( Which: UInt8; Ball: UInt8; RelativePos: TPoint ) {$IFNDEF NOT_OO}of object{$ENDIF};
  TSDLJoyHatMoveEvent =  procedure ( Which: UInt8; Hat: UInt8; Value: SInt16 ) {$IFNDEF NOT_OO}of object{$ENDIF};
  TSDLJoyButtonEvent =  procedure ( Which: UInt8; Button: UInt8; State: SInt16 ) {$IFNDEF NOT_OO}of object{$ENDIF};


  TSDLJoyStick = class( TSDLCustomInput )
  private
    FJoystick : PSDL_Joystick;
    FJoystickIndex : Integer;
    FJoyAxisMoveEvent : TSDLJoyAxisMoveEvent;
    FJoyBallMoveEvent : TSDLJoyBallMoveEvent;
    FJoyHatMoveEvent : TSDLJoyHatMoveEvent;
    FJoyButtonDownEvent : TSDLJoyButtonEvent;
    FJoyButtonUpEvent : TSDLJoyButtonEvent;
    procedure DoAxisMove( Event : TSDL_Event );
    procedure DoBallMove( Event : TSDL_Event );
    procedure DoHatMove( Event : TSDL_Event );
    procedure DoButtonDown( Event : TSDL_Event );
    procedure DoButtonUp( Event : TSDL_Event );
    function GetName: PChar;
    function GetNumAxes: integer;
    function GetNumBalls: integer;
    function GetNumButtons: integer;
    function GetNumHats: integer;
  public
    constructor Create( Index : Integer );
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function UpdateInput( Event: TSDL_EVENT ) : Boolean; override;
    property Name : PChar read GetName;
    property NumAxes : integer read GetNumAxes;
    property NumBalls : integer read GetNumBalls;
    property NumButtons : integer read GetNumButtons;
    property NumHats : integer read GetNumHats;
    property OnAxisMove : TSDLJoyAxisMoveEvent read FJoyAxisMoveEvent write FJoyAxisMoveEvent;
    property OnBallMove : TSDLJoyBallMoveEvent read FJoyBallMoveEvent write FJoyBallMoveEvent;
    property OnHatMove : TSDLJoyHatMoveEvent read FJoyHatMoveEvent write FJoyHatMoveEvent;
    property OnButtonDown : TSDLJoyButtonEvent read FJoyButtonDownEvent write FJoyButtonDownEvent;
    property OnButtonUp : TSDLJoyButtonEvent read FJoyButtonUpEvent write FJoyButtonUpEvent;
  end;

  TSDLJoySticks = class( TObject )
  private
    FNumOfJoySticks: Integer;
    FJoyStickList : TList;
    function GetJoyStick(Index: integer): TSDLJoyStick;
    procedure SetJoyStick(Index: integer; const Value: TSDLJoyStick);
  public
    constructor Create;
    destructor Destroy; override;
    function UpdateInput( event: TSDL_EVENT ) : Boolean;
    property NumOfJoySticks : Integer read FNumOfJoySticks write FNumOfJoySticks;
    property JoySticks[ Index : integer ] : TSDLJoyStick read GetJoyStick write SetJoyStick;
  end;

  TSDLKeyBoardEvent =  procedure ( var Key: TSDLKey; Shift: TSDLMod; unicode : UInt16 ) {$IFNDEF NOT_OO}of object{$ENDIF};

  TSDLKeyBoard = class( TSDLCustomInput )
  private
    FKeys : PKeyStateArr;
    FOnKeyUp: TSDLKeyBoardEvent;
    FOnKeyDown: TSDLKeyBoardEvent;
    procedure DoKeyDown( keysym : PSDL_keysym );
    procedure DoKeyUp( keysym : PSDL_keysym );
  public
    function IsKeyDown( Key : TSDLKey ) : Boolean;
    function IsKeyUp( Key : TSDLKey ) : Boolean;
    function UpdateInput( event: TSDL_EVENT ) : Boolean; override;
    property Keys : PKeyStateArr read FKeys write FKeys;
    property OnKeyDown : TSDLKeyBoardEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp : TSDLKeyBoardEvent read FOnKeyUp write FOnKeyUp;
  end;

  TSDLMouseButtonEvent =  procedure ( Button : Integer; Shift: TSDLMod; MousePos : TPoint ) {$IFNDEF NOT_OO}of object{$ENDIF};
  TSDLMouseMoveEvent =  procedure ( Shift: TSDLMod; CurrentPos : TPoint; RelativePos : TPoint ) {$IFNDEF NOT_OO}of object{$ENDIF};
  TSDLMouseWheelEvent =  procedure ( WheelDelta : Integer; Shift: TSDLMod; MousePos : TPoint ) {$IFNDEF NOT_OO}of object{$ENDIF};

  TSDLCustomCursor = class( TObject )
  private
    FFileName : string;
    FHotPoint: TPoint;
    procedure SetFileName(const aValue: string );
    function ScanForChar( str : string; ch : Char; startPos : Integer; lookFor : Boolean ) : Integer;
  public
    constructor Create( const aFileName : string; aHotPoint: TPoint );
    procedure LoadFromFile( const aFileName : string ); virtual; abstract;
    procedure LoadFromStream( aStream : TStream ); virtual; abstract;
    procedure Show; virtual; abstract;
    property FileName : string read FFileName write SetFileName;
    property HotPoint : TPoint read FHotPoint write FHotPoint;
  end;

  TSDLXPMCursor = class( TSDLCustomCursor )
  private
    FCursor : PSDL_Cursor;
    procedure FreeCursor;
  public
    destructor Destroy; override;
    procedure LoadFromFile( const aFileName : string ); override;
    procedure LoadFromStream( aStream : TStream ); override;
    procedure Show; override;
  end;

  TSDLCursorList = class( TStringList )
  protected
    function GetObject( aIndex : Integer ): TSDLCustomCursor; reintroduce;
    procedure PutObject( aIndex : Integer; AObject : TSDLCustomCursor); reintroduce;
  public
    constructor Create;
   function AddCursor(const aName : string; aObject : TSDLCustomCursor): Integer; virtual;
  end;

  TSDLMouse = class( TSDLCustomInput )
  private
    FDragging : Boolean;
    FMousePos : TPoint;
    FOnMouseUp: TSDLMouseButtonEvent;
    FOnMouseDown: TSDLMouseButtonEvent;
    FOnMouseMove: TSDLMouseMoveEvent;
    FOnMouseWheel: TSDLMouseWheelEvent;
    FCursorList : TSDLCursorList; // Cursor Pointer
    procedure DoMouseMove( Event: TSDL_Event );
    procedure DoMouseDown( Event: TSDL_Event );
    procedure DoMouseUp( Event: TSDL_Event );
    procedure DoMouseWheelScroll( Event: TSDL_Event );
    function GetMousePosition: TPoint;
    procedure SetMousePosition(const Value: TPoint);
    function GetMouseDelta: TPoint;
  public
    destructor Destroy; override;
    function UpdateInput( event: TSDL_EVENT ) : Boolean; override;
    function MouseIsDown( Button : Integer ) : Boolean;
    function MouseIsUp( Button : Integer ) : Boolean;
    procedure ShowCursor;
    procedure HideCursor;
    property OnMouseDown : TSDLMouseButtonEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp : TSDLMouseButtonEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseMove : TSDLMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseWheel : TSDLMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property MousePosition : TPoint read GetMousePosition write SetMousePosition;
    property MouseDelta: TPoint read GetMouseDelta;
    property Cursors : TSDLCursorList read FCursorList write FCursorList;
  end;

  TSDLInputManager = class( TObject )
  private
    FKeyBoard : TSDLKeyBoard;
    FMouse : TSDLMouse;
    FJoystick : TSDLJoysticks;
  public
    constructor Create( InitInputs : TSDLInputTypes );
    destructor Destroy; override;
    procedure Disable( InitInputs : TSDLInputTypes; JoyStickNumber : Integer = 0 );
    procedure Enable( InitInputs : TSDLInputTypes; JoyStickNumber : Integer = 0 );
    function UpdateInputs( event: TSDL_EVENT ) : Boolean;
    property KeyBoard : TSDLKeyBoard read FKeyBoard write FKeyBoard;
    property Mouse : TSDLMouse read FMouse write FMouse;
    property JoyStick : TSDLJoysticks read FJoyStick write FJoyStick;
  end;

implementation

uses
  SysUtils;

{ TSDLCustomInput }
constructor TSDLCustomInput.Create;
begin
  inherited;
  FEnabled := true;
end;

{ TSDLJoysticks }
constructor TSDLJoysticks.Create;
var
  i : integer;
begin
  inherited;
  if ( SDL_WasInit( SDL_INIT_JOYSTICK ) = 0 ) then
    SDL_InitSubSystem( SDL_INIT_JOYSTICK );
  FNumOfJoySticks := SDL_NumJoysticks;
  FJoyStickList := TList.Create;
  for i := 0 to FNumOfJoySticks - 1 do
  begin
    FJoyStickList.Add( TSDLJoyStick.Create( i )  );
  end;
end;

destructor TSDLJoysticks.Destroy;
var
  i : integer;
begin
  if FJoyStickList.Count > 0 then
  begin
    for i := 0 to FJoyStickList.Count - 1 do
    begin
      TSDLJoyStick( FJoyStickList.Items[i] ).Free;
    end;
  end;
  SDL_QuitSubSystem( SDL_INIT_JOYSTICK );
  inherited;
end;

function TSDLJoySticks.GetJoyStick(Index: integer): TSDLJoyStick;
begin
  Result := TSDLJoyStick( FJoyStickList[ Index ] );
end;

procedure TSDLJoySticks.SetJoyStick(Index: integer;
  const Value: TSDLJoyStick);
begin
  FJoyStickList[ Index ] :=  @Value;
end;

function TSDLJoysticks.UpdateInput(event: TSDL_EVENT): Boolean;
var
  i : integer;
begin
  result := false;
  if FJoyStickList.Count > 0 then
  begin
    for i := 0 to FJoyStickList.Count - 1 do
    begin
      TSDLJoyStick( FJoyStickList.Items[i] ).UpdateInput( event );
    end;
  end;
end;

{ TSDLKeyBoard }
procedure TSDLKeyBoard.DoKeyDown(keysym: PSDL_keysym);
begin
  if Assigned( FOnKeyDown ) then
    FOnKeyDown( keysym.sym , keysym.modifier, keysym.unicode );
end;

procedure TSDLKeyBoard.DoKeyUp(keysym: PSDL_keysym);
begin
  if Assigned( FOnKeyUp ) then
    FOnKeyUp( keysym.sym , keysym.modifier, keysym.unicode );
end;

function TSDLKeyBoard.IsKeyDown( Key: TSDLKey ): Boolean;
begin
  SDL_PumpEvents;

  // Populate Keys array
  FKeys := PKeyStateArr( SDL_GetKeyState( nil ) );
  Result := ( FKeys[Key] = SDL_PRESSED );
end;

function TSDLKeyBoard.IsKeyUp( Key: TSDLKey ): Boolean;
begin
  SDL_PumpEvents;

  // Populate Keys array
  FKeys := PKeyStateArr( SDL_GetKeyState( nil ) );
  Result := ( FKeys[Key] = SDL_RELEASED );
end;

function TSDLKeyBoard.UpdateInput(event: TSDL_EVENT): Boolean;
begin
  result := false;
  if ( FEnabled ) then
  begin
    case event.type_ of
      SDL_KEYDOWN :
      begin
        // handle key presses
        DoKeyDown( @event.key.keysym );
        result := true;
      end;

      SDL_KEYUP :
      begin
        // handle key releases
        DoKeyUp( @event.key.keysym );
        result := true;
      end;
    end;
  end;
end;

{ TSDLMouse }
destructor TSDLMouse.Destroy;
begin

  inherited;
end;

procedure TSDLMouse.DoMouseDown( Event: TSDL_Event );
var
  CurrentPos : TPoint;
begin
  FDragging := true;
  if Assigned( FOnMouseDown ) then
  begin
    CurrentPos.x := event.button.x;
    CurrentPos.y := event.button.y;
    FOnMouseDown( event.button.button, SDL_GetModState, CurrentPos );
  end;
end;

procedure TSDLMouse.DoMouseMove( Event: TSDL_Event );
var
  CurrentPos, RelativePos : TPoint;
begin
  if Assigned( FOnMouseMove ) then
  begin
    CurrentPos.x := event.motion.x;
    CurrentPos.y := event.motion.y;
    RelativePos.x := event.motion.xrel;
    RelativePos.y := event.motion.yrel;
    FOnMouseMove( SDL_GetModState, CurrentPos, RelativePos );
  end;
end;

procedure TSDLMouse.DoMouseUp( event: TSDL_EVENT );
var
  Point : TPoint;
begin
  FDragging := false;
  if Assigned( FOnMouseUp ) then
  begin
    Point.x := event.button.x;
    Point.y := event.button.y;
    FOnMouseUp( event.button.button, SDL_GetModState, Point );
  end;
end;

procedure TSDLMouse.DoMouseWheelScroll( event: TSDL_EVENT );
var
  Point : TPoint;
begin
  if Assigned( FOnMouseWheel ) then
  begin
    Point.x := event.button.x;
    Point.y := event.button.y;
    if ( event.button.button = SDL_BUTTON_WHEELUP ) then
      FOnMouseWheel( SDL_BUTTON_WHEELUP, SDL_GetModState, Point )
    else
      FOnMouseWheel( SDL_BUTTON_WHEELDOWN, SDL_GetModState, Point );
  end;
end;

function TSDLMouse.GetMouseDelta: TPoint;
begin
  SDL_PumpEvents;
  
  SDL_GetRelativeMouseState( Result.X, Result.Y );
end;

function TSDLMouse.GetMousePosition: TPoint;
begin
  SDL_PumpEvents;

  SDL_GetMouseState( FMousePos.X, FMousePos.Y );
  Result := FMousePos;
end;

procedure TSDLMouse.HideCursor;
begin
  SDL_ShowCursor( SDL_DISABLE  );
end;

function TSDLMouse.MouseIsDown(Button: Integer): Boolean;
begin
  SDL_PumpEvents;

  Result := ( SDL_GetMouseState( FMousePos.X, FMousePos.Y ) and SDL_BUTTON( Button ) = 0 );
end;

function TSDLMouse.MouseIsUp(Button: Integer): Boolean;
begin
  SDL_PumpEvents;

  Result := not ( SDL_GetMouseState( FMousePos.X, FMousePos.Y ) and SDL_BUTTON( Button ) = 0 );
end;

procedure TSDLMouse.SetMousePosition(const Value: TPoint);
begin
  SDL_WarpMouse( Value.x, Value.y );
end;

procedure TSDLMouse.ShowCursor;
begin
  SDL_ShowCursor( SDL_ENABLE  );
end;

function TSDLMouse.UpdateInput(event: TSDL_EVENT): Boolean;
begin
  result := false;
  if ( FEnabled ) then
  begin
    case event.type_ of
      SDL_MOUSEMOTION :
      begin
        // handle Mouse Move
        DoMouseMove( event );
      end;

      SDL_MOUSEBUTTONDOWN :
      begin
        // handle Mouse Down
        if ( event.button.button = SDL_BUTTON_WHEELUP )
        or ( event.button.button = SDL_BUTTON_WHEELDOWN ) then
          DoMouseWheelScroll( event )
        else
          DoMouseDown( event );
      end;

      SDL_MOUSEBUTTONUP :
      begin
        // handle Mouse Up
        if ( event.button.button = SDL_BUTTON_WHEELUP )
        or ( event.button.button = SDL_BUTTON_WHEELDOWN ) then
          DoMouseWheelScroll( event )
        else
          DoMouseUp( event );
      end;
    end;
  end;
end;

{ TSDLInputManager }
constructor TSDLInputManager.Create(InitInputs: TSDLInputTypes);
begin
  inherited Create;
  if itJoystick in InitInputs then
    FJoystick := TSDLJoysticks.Create;

  if itKeyBoard in InitInputs then
    FKeyBoard := TSDLKeyBoard.Create;
    
  if itMouse in InitInputs then
    FMouse := TSDLMouse.Create;
end;

destructor TSDLInputManager.Destroy;
begin
  if FJoystick <> nil then
    FreeAndNil( FJoystick );
  if FKeyBoard <> nil then
    FreeAndNil( FKeyBoard );
  if FMouse <> nil then
    FreeAndNil( FMouse );
  inherited;
end;

procedure TSDLInputManager.Disable( InitInputs : TSDLInputTypes;  JoyStickNumber : Integer );
begin
  if itJoystick in InitInputs then
    FJoystick.JoySticks[ JoyStickNumber ].Enabled := false;

  if itKeyBoard in InitInputs then
    FKeyBoard.Enabled := false;

  if itMouse in InitInputs then
    FMouse.Enabled := false;
end;

procedure TSDLInputManager.Enable( InitInputs: TSDLInputTypes; JoyStickNumber: Integer );
begin
  if itJoystick in InitInputs then
    FJoystick.JoySticks[ JoyStickNumber ].Enabled := true;

  if itKeyBoard in InitInputs then
    FKeyBoard.Enabled := true;
    
  if itMouse in InitInputs then
    FMouse.Enabled := true;
end;

function TSDLInputManager.UpdateInputs( event: TSDL_EVENT ): Boolean;
begin
  Result := false;
  if ( FJoystick <> nil ) then
   Result := FJoystick.UpdateInput( event );
  if ( FKeyBoard <> nil ) then
   Result := FKeyBoard.UpdateInput( event );
  if ( FMouse <> nil ) then
   Result := FMouse.UpdateInput( event );
end;

{ TSDLJoyStick }
procedure TSDLJoyStick.Close;
begin
  SDL_JoystickClose( @FJoystick );
end;

constructor TSDLJoyStick.Create( Index : Integer );
begin
  inherited Create;
  FJoystick := nil;
  FJoystickIndex := Index;
end;

destructor TSDLJoyStick.Destroy;
begin
  if FJoystick <> nil then
    Close;
  inherited;
end;

procedure TSDLJoyStick.DoAxisMove(Event: TSDL_Event);
begin
  if Assigned( FJoyAxisMoveEvent ) then
  begin
    FJoyAxisMoveEvent( Event.jaxis.which, Event.jaxis.axis, Event.jaxis.value );
  end
end;

procedure TSDLJoyStick.DoBallMove(Event: TSDL_Event);
var
  BallPoint : TPoint;
begin
  if Assigned( FJoyBallMoveEvent ) then
  begin
    BallPoint.x := Event.jball.xrel;
    BallPoint.y := Event.jball.yrel;
    FJoyBallMoveEvent( Event.jball.which, Event.jball.ball, BallPoint );
  end;
end;

procedure TSDLJoyStick.DoButtonDown(Event: TSDL_Event);
begin
  if Assigned( FJoyButtonDownEvent ) then
  begin
    if ( Event.jbutton.state = SDL_PRESSED ) then
      FJoyButtonDownEvent( Event.jbutton.which, Event.jbutton.button, Event.jbutton.state );
  end;
end;

procedure TSDLJoyStick.DoButtonUp(Event: TSDL_Event);
begin
  if Assigned( FJoyButtonUpEvent ) then
  begin
    if ( Event.jbutton.state = SDL_RELEASED ) then
      FJoyButtonUpEvent( Event.jbutton.which, Event.jbutton.button, Event.jbutton.state );
  end
end;

procedure TSDLJoyStick.DoHatMove(Event: TSDL_Event);
begin
  if Assigned( FJoyHatMoveEvent ) then
  begin
    FJoyHatMoveEvent( Event.jhat.which, Event.jhat.hat, Event.jhat.value );
  end;
end;

function TSDLJoyStick.GetName: PChar;
begin
  result := FJoystick.name;
end;

function TSDLJoyStick.GetNumAxes: integer;
begin
  result := FJoystick.naxes;
end;

function TSDLJoyStick.GetNumBalls: integer;
begin
  result := FJoystick.nballs;
end;

function TSDLJoyStick.GetNumButtons: integer;
begin
  result := FJoystick.nbuttons;
end;

function TSDLJoyStick.GetNumHats: integer;
begin
  result := FJoystick.nhats;
end;

procedure TSDLJoyStick.Open;
begin
  FJoystick := SDL_JoyStickOpen( FJoystickIndex );
end;

function TSDLJoyStick.UpdateInput(Event: TSDL_EVENT): Boolean;
begin
  Result := false;
  
  if ( FEnabled ) then
  begin
    case event.type_ of
      SDL_JOYAXISMOTION :
      begin
        DoAxisMove( Event );
      end;

      SDL_JOYBALLMOTION :
      begin
        DoBallMove( Event );
      end;

      SDL_JOYHATMOTION :
      begin
        DoHatMove( Event );
      end;

      SDL_JOYBUTTONDOWN :
      begin
        DoButtonDown( Event );
      end;

      SDL_JOYBUTTONUP :
      begin
        DoButtonUp( Event );
      end;
    end;
  end;
end;

{ TSDLCustomCursor }

constructor TSDLCustomCursor.Create(const aFileName: string;  aHotPoint: TPoint);
begin
  inherited Create;
  FHotPoint := aHotPoint;
  LoadFromFile( aFileName );
end;

function TSDLCustomCursor.ScanForChar(str: string; ch: Char;
  startPos: Integer; lookFor: Boolean): Integer;
begin
  Result := -1;
  while ( ( ( str[ startPos ] = ch ) <> lookFor ) and ( startPos < Length( str ) ) ) do
    inc( startPos );
  if startPos <> Length( str ) then
    Result := startPos;
end;

procedure TSDLCustomCursor.SetFileName(const aValue: string);
begin
  LoadFromFile( aValue );
end;

{ TSDLXPMCursor }

destructor TSDLXPMCursor.Destroy;
begin
  FreeCursor;
  inherited;
end;

procedure TSDLXPMCursor.FreeCursor;
begin
  if FCursor <> nil then
  begin
    SDL_FreeCursor( FCursor );
    FFileName := ''; 
  end;
end;

procedure TSDLXPMCursor.LoadFromFile(const aFileName: string);
var
  xpmFile : Textfile;
  step : Integer;
  holdPos : Integer;
  counter : Integer;
  dimensions : array[ 1..3 ] of Integer;
  clr, clrNone, clrBlack, clrWhite : Char;
  data, mask : array of UInt8;
  i, col : Integer;
  LineString : string;
begin
  FreeCursor;
  AssignFile( xpmFile, aFileName );
  Reset( xpmFile );
  step := 0;
  i := -1;
  clrBlack := 'X';
  clrWhite := ',';
  clrNone := ' ';
  counter := 0;
  while not ( eof( xpmFile ) ) do
  begin
    Readln( xpmFile, LineString );
    // scan for strings
    if LineString[ 1 ] = '"' then
    begin
      case step of
        0 : // Get dimensions  (should be width height number-of-colors ???)
          begin
            HoldPos := 2;
            counter := ScanForChar( LineString, ' ', HoldPos, False );
            counter := ScanForChar( LineString, ' ', counter, True );
            dimensions[ 1 ] := StrToInt( Copy( LineString, HoldPos, counter - HoldPos ) );
            counter := ScanForChar( LineString, ' ', counter, False );
            holdPos := counter;
            counter := ScanForChar( LineString, ' ', counter, True );
            dimensions[ 2 ] := StrToInt( Copy( LineString, holdPos, counter - HoldPos ) );
            counter := ScanForChar( LineString, ' ', counter, False );
            holdPos := counter;
            counter := ScanForChar( LineString, ' ', counter, True );
            dimensions[ 3 ] := StrToInt( Copy( LineString, holdPos, counter - HoldPos ) );
            step := 1;
            SetLength( data, ( dimensions[ 1 ] * dimensions[ 2 ] ) div 8 );
            SetLength( mask, ( dimensions[ 1 ] * dimensions[ 2 ] ) div 8 );
            //Log.LogStatus( 'Length = ' + IntToStr( ( dimensions[ 1 ] * dimensions[ 2 ] ) div 8 ), 'LoadCursorFromFile' );
          end;
        1 : // get the symbols for transparent, black and white
          begin
            // get the symbol for the color
            clr := LineString[ 2 ];
            // look for the 'c' symbol
            counter := ScanForChar( LineString, 'c', 3, True );
            inc( counter );
            counter := ScanForChar( LineString, ' ', counter, False );
            if LowerCase( Copy( LineString, counter, 4 ) ) = 'none' then
            begin
              clrNone := clr;
            end;
            if LowerCase( Copy( LineString, counter, 7 ) ) = '#ffffff' then
            begin
              clrWhite := clr;
            end;
            if LowerCase( Copy( LineString, counter, 7 ) ) = '#000000' then
            begin
              clrBlack := clr;
            end;
            dec( dimensions[ 3 ] );
            if dimensions[ 3 ] = 0 then
            begin
              step := 2;
              counter := 0;
            end;
          end;
        2 : // get cursor information -- modified from the SDL
          // documentation of SDL_CreateCursor.
          begin
            for col := 1 to dimensions[1] do
            begin
              if ( ( col mod 8 ) <> 1 ) then
              begin
                data[ i ] := data[ i ] shl 1;
                mask[ i ] := mask[ i ] shl 1;
              end
              else
              begin
                inc( i );
                data[ i ] := 0;
                mask[ i ] := 0;
              end;
              if LineString[ col ] = clrWhite then
              begin
                mask[ i ] := mask[ i ] or $01;
              end
              else if LineString[ col ] = clrBlack then
              begin
                data[ i ] := data[ i ] or $01;
                mask[ i ] := mask[ i ] or $01;
              end
              else if LineString[ col + 1 ] = clrNone then
              begin
                //
              end;
            end;
            inc(counter);
            if counter = dimensions[2] then
              step := 4;
          end;
      end;
    end;
  end;
  CloseFile( xpmFile );
  FCursor := SDL_CreateCursor( PUInt8( data ), PUInt8( mask ), dimensions[ 1 ], dimensions[ 2 ], FHotPoint.x, FHotPoint.y );
end;

procedure TSDLXPMCursor.LoadFromStream(aStream: TStream);
begin
  inherited;

end;

procedure TSDLXPMCursor.Show;
begin
  inherited;
  SDL_SetCursor( FCursor );
end;

{ TSDLCursorList }
function TSDLCursorList.AddCursor(const aName : string; aObject : TSDLCustomCursor): Integer;
begin
  result := inherited AddObject( aName, aObject );
end;

constructor TSDLCursorList.Create;
begin
  inherited;
  Duplicates := dupIgnore;
end;

function TSDLCursorList.GetObject(aIndex: Integer): TSDLCustomCursor;
begin
  result := TSDLCustomCursor( inherited GetObject( aIndex ) );
end;

procedure TSDLCursorList.PutObject(aIndex: Integer; aObject: TSDLCustomCursor);
begin
  inherited PutObject( aIndex, aObject );
end;

end.
