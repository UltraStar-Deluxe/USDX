unit sdlgameinterface;
{
  $Id: sdlgameinterface.pas,v 1.3 2004/10/17 18:41:49 savage Exp $
  
}
{******************************************************************************}
{                                                                              }
{          JEDI-SDL : Pascal units for SDL - Simple DirectMedia Layer          }
{                    Game Interface Base class                                 }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominqiue Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominqiue Louis are                                      }
{ Copyright (C) 2000 - 2001 Dominqiue Louis.                                   }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{                                                                              }
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
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   The SDL Runtime libraris on Win32  : SDL.dll on Linux : libSDL.so          }
{   They are available from...                                                 }
{   http://www.libsdl.org .                                                    }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   September   23 2004 - DL : Initial Creation                                }
{
  $Log: sdlgameinterface.pas,v $
  Revision 1.3  2004/10/17 18:41:49  savage
  Slight Change to allow Reseting of Input Event handlers

  Revision 1.2  2004/09/30 22:35:47  savage
  Changes, enhancements and additions as required to get SoAoS working.


}
{******************************************************************************}

interface

uses
  sdl,
  sdlwindow;

type
  TGameInterfaceClass = class of TGameInterface;

  TGameInterface = class( TObject )
  private
    FNextGameInterface : TGameInterfaceClass;
  protected
    Dragging : Boolean;
    Loaded : Boolean;
    procedure FreeSurfaces; virtual;
    procedure Render; virtual; abstract;
    procedure Close; virtual;
    procedure Update( aElapsedTime : single ); virtual; 
    procedure MouseDown( Button : Integer; Shift: TSDLMod; MousePos : TPoint ); virtual;
    procedure MouseMove( Shift: TSDLMod; CurrentPos : TPoint; RelativePos : TPoint ); virtual;
    procedure MouseUp( Button : Integer; Shift: TSDLMod; MousePos : TPoint ); virtual;
    procedure MouseWheelScroll( WheelDelta : Integer; Shift: TSDLMod; MousePos : TPoint ); virtual;
    procedure KeyDown( var Key: TSDLKey; Shift: TSDLMod; unicode : UInt16 ); virtual;
  public
    MainWindow : TSDL2DWindow;
    procedure ResetInputManager;
    procedure LoadSurfaces; virtual;
    function PointIsInRect( Point : TPoint; x, y, x1, y1 : integer ) : Boolean;
    constructor Create( const aMainWindow : TSDL2DWindow );
    destructor Destroy; override;
    property NextGameInterface : TGameInterfaceClass read FNextGameInterface write FNextGameInterface;
  end;

implementation

{ TGameInterface }
procedure TGameInterface.Close;
begin
  FNextGameInterface := nil;
end;

constructor TGameInterface.Create( const aMainWindow : TSDL2DWindow );
begin
  inherited Create;
  MainWindow := aMainWindow;
  FNextGameInterface := TGameInterface;
  ResetInputManager;
end;

destructor TGameInterface.Destroy;
begin
  if Loaded then
    FreeSurfaces;
  inherited;
end;

procedure TGameInterface.FreeSurfaces;
begin
  Loaded := False;
end;

procedure TGameInterface.KeyDown(var Key: TSDLKey; Shift: TSDLMod; unicode: UInt16);
begin

end;

procedure TGameInterface.LoadSurfaces;
begin
  Loaded := True;
end;

procedure TGameInterface.MouseDown(Button: Integer; Shift: TSDLMod; MousePos: TPoint);
begin
  Dragging := True;
end;

procedure TGameInterface.MouseMove(Shift: TSDLMod; CurrentPos, RelativePos: TPoint);
begin

end;

procedure TGameInterface.MouseUp(Button: Integer; Shift: TSDLMod; MousePos: TPoint);
begin
  Dragging := True;
end;

procedure TGameInterface.MouseWheelScroll(WheelDelta: Integer; Shift: TSDLMod; MousePos: TPoint);
begin

end;

function TGameInterface.PointIsInRect( Point : TPoint; x, y, x1, y1: integer ): Boolean;
begin
  if ( Point.x >= x )
  and ( Point.y >= y )
  and ( Point.x <= x1 )
  and ( Point.y <= y1 ) then
    result := true
  else
    result := false;
end;

procedure TGameInterface.ResetInputManager;
begin
  MainWindow.InputManager.Mouse.OnMouseDown := MouseDown;
  MainWindow.InputManager.Mouse.OnMouseMove := MouseMove;
  MainWindow.InputManager.Mouse.OnMouseUp := MouseUp;
  MainWindow.InputManager.Mouse.OnMouseWheel := MouseWheelScroll;
  MainWindow.InputManager.KeyBoard.OnKeyDown := KeyDown;
  MainWindow.OnRender := Render;
  MainWindow.OnClose := Close;
  MainWindow.OnUpdate := Update;
end;

procedure TGameInterface.Update(aElapsedTime: single);
begin

end;

end.
