unit sdlwindow;
{
  $Id: sdlwindow.pas,v 1.7 2004/09/30 22:35:47 savage Exp $
  
}
{******************************************************************************}
{                                                                              }
{          JEDI-SDL : Pascal units for SDL - Simple DirectMedia Layer          }
{                SDL Window Wrapper                                            }
{                                                                              }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominique Louis are                                      }
{ Copyright (C) 2004 - 2100 Dominique Louis.                                   }
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
{   SDL Window Wrapper                                                         }
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
{ January    31     2003 - DL : Initial creation                               }
{                                                                              }
{
  $Log: sdlwindow.pas,v $
  Revision 1.7  2004/09/30 22:35:47  savage
  Changes, enhancements and additions as required to get SoAoS working.

  Revision 1.6  2004/09/12 21:52:58  savage
  Slight changes to fix some issues with the sdl classes.

  Revision 1.5  2004/05/10 21:11:49  savage
  changes required to help get SoAoS off the ground.

  Revision 1.4  2004/05/01 14:59:27  savage
  Updated code

  Revision 1.3  2004/04/23 10:45:28  savage
  Changes made by Dean Ellis to work more modularly.

  Revision 1.2  2004/03/31 10:06:41  savage
  Changed so that it now compiles, but is untested.

  Revision 1.1  2004/02/05 00:08:20  savage
  Module 1.0 release
  
}
{******************************************************************************}

interface

{$i jedi-sdl.inc}

uses
  Classes,
  sdl,
  sdlinput,
  sdlticks;

type
  TSDLNotifyEvent =  procedure {$IFNDEF NOT_OO}of object{$ENDIF};
  TSDLUpdateEvent =  procedure( aElapsedTime : single ) {$IFNDEF NOT_OO}of object{$ENDIF};
  TSDLResizeEvent =  procedure( aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 ) {$IFNDEF NOT_OO}of object{$ENDIF};
  TSDLUserEvent =  procedure( aType : UInt8; aCode : integer; aData1 : Pointer; aData2 : Pointer ) {$IFNDEF NOT_OO}of object{$ENDIF};
  TSDLActiveEvent = procedure( aGain: UInt8; aState: UInt8 ) {$IFNDEF NOT_OO}of object{$ENDIF};

  TSDLBaseWindow = class( TObject )
  private
    FDisplaySurface : PSDL_Surface;
    FVideoFlags : Uint32;
    FOnDestroy: TSDLNotifyEvent;
    FOnCreate: TSDLNotifyEvent;
    FOnShow: TSDLNotifyEvent;
    FOnResize: TSDLResizeEvent;
    FOnUpdate: TSDLUpdateEvent;
    FOnRender: TSDLNotifyEvent;
    FOnClose: TSDLNotifyEvent;
    FLoaded: Boolean;
    FRendering: Boolean;
    FHeight: integer;
    FBitDepth: integer;
    FWidth: integer;
    FInputManager: TSDLInputManager;
    FCaptionText : PChar;
    FIconName : PChar;
    FOnActive: TSDLActiveEvent;
    FOnQuit: TSDLNotifyEvent;
    FOnExpose: TSDLNotifyEvent;
    FOnUser: TSDLUserEvent;
    FTimer : TSDLTicks;
  protected
    procedure DoActive( aGain: UInt8; aState: UInt8 );
    procedure DoCreate;
    procedure DoClose;
    procedure DoDestroy;
    procedure DoUpdate( aElapsedTime : single );
    procedure DoQuit;
    procedure DoRender;
    procedure DoResize( aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 );
    procedure DoShow;
    procedure DoUser( aType : UInt8; aCode : integer; aData1 : Pointer; aData2 : Pointer );
    procedure DoExpose;
    procedure Render; virtual;
    procedure Update( aElapsedTime : single ); virtual;
    procedure InitialiseObjects; virtual;
    procedure RestoreObjects; virtual;
    procedure DeleteObjects; virtual;
    function Flip : integer; virtual;
    property OnActive : TSDLActiveEvent read FOnActive write FOnActive;
    property OnClose: TSDLNotifyEvent read FOnClose write FOnClose;
    property OnDestroy : TSDLNotifyEvent read FOnDestroy write FOnDestroy;
    property OnCreate : TSDLNotifyEvent read FOnCreate write FOnCreate;
    property OnUpdate: TSDLUpdateEvent read FOnUpdate write FOnUpdate;
    property OnQuit : TSDLNotifyEvent read FOnQuit write FOnQuit;
    property OnResize : TSDLResizeEvent read FOnResize write FOnResize;
    property OnRender: TSDLNotifyEvent read FOnRender write FOnRender;
    property OnShow : TSDLNotifyEvent read FOnShow write FOnShow;
    property OnUser : TSDLUserEvent read FOnUser write FOnUser;
    property OnExpose : TSDLNotifyEvent read FOnExpose write FOnExpose;
    property DisplaySurface: PSDL_Surface read FDisplaySurface;
  public
    property InputManager : TSDLInputManager read FInputManager;
    property Loaded : Boolean read FLoaded;
    property Width : integer read FWidth;
    property Height : integer read FHeight;
    property BitDepth : integer read FBitDepth;
    property Rendering : Boolean read FRendering write FRendering;
    procedure SetCaption( const aCaptionText : string; const aIconName : string );
    procedure GetCaption( var aCaptionText : string; var aIconName : string );
    procedure SetIcon( aIcon : PSDL_Surface; aMask: UInt8 );
    procedure ActivateVideoMode;
    constructor Create( aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 ); virtual;
    destructor Destroy; override;
    procedure InitialiseEnvironment;
    function Show : Boolean; virtual;
  end;

  TSDL2DWindow = class( TSDLBaseWindow )
  public
    constructor Create( aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 = SDL_DOUBLEBUF or SDL_SWSURFACE); override;
    procedure Render; override;
    procedure Update( aElapsedTime : single ); override;
    procedure InitialiseObjects; override;
    procedure RestoreObjects; override;
    procedure DeleteObjects; override;
    function Flip : integer; override;
    property OnCreate;
    property OnDestroy;
    property OnClose;
    property OnShow;
    property OnResize;
    property OnRender;
    property OnUpdate;
    property DisplaySurface;
  end;

  TSDL3DWindow = class( TSDLBaseWindow )
  public
    constructor Create( aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 = SDL_OPENGL or SDL_DOUBLEBUF); override;
    function Flip : integer; override;
    procedure Render; override;
    procedure Update( aElapsedTime : single ); override;
    procedure InitialiseObjects; override;
    procedure RestoreObjects; override;
    procedure DeleteObjects; override;
    property OnCreate;
    property OnDestroy;
    property OnClose;
    property OnShow;
    property OnResize;
    property OnRender;
    property OnUpdate;
    property DisplaySurface;
  end;



implementation

uses
  logger,
  SysUtils;

{ TSDLBaseWindow }
procedure TSDLBaseWindow.ActivateVideoMode;
begin
  FDisplaySurface := SDL_SetVideoMode( FWidth, FHeight, FBitDepth, FVideoFlags);
  if (FDisplaySurface = nil) then
  begin
    Log.LogError( Format('Could not set video mode: %s', [SDL_GetError]), 'Main');
    exit;
  end;
  
  SetCaption( 'Made with JEDI-SDL', 'JEDI-SDL Icon' );
end;

constructor TSDLBaseWindow.Create( aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 );
begin
  inherited Create;
  SDL_Init(SDL_INIT_EVERYTHING);
  FInputManager := TSDLInputManager.Create( [ itJoystick, itKeyBoard, itMouse ]);
  FTimer := TSDLTicks.Create;

  FWidth := aWidth;
  FHeight := aHeight;
  FBitDepth := aBitDepth;
  FVideoFlags := aVideoFlags;

  DoCreate;
end;

procedure TSDLBaseWindow.DeleteObjects;
begin
  FLoaded := False;
end;

destructor TSDLBaseWindow.Destroy;
begin
  DoDestroy;
  if FLoaded then
    DeleteObjects;
  if FInputManager <> nil then
    FreeAndNil( FInputManager );
  if FTimer <> nil then
    FreeAndNil( FTimer );
  if FDisplaySurface <> nil then
    SDL_FreeSurface( FDisplaySurface );
  inherited Destroy;
  SDL_Quit;
end;

procedure TSDLBaseWindow.DoActive(aGain, aState: UInt8);
begin
  if Assigned( FOnActive ) then
  begin
    FOnActive( aGain, aState );
  end;
end;

procedure TSDLBaseWindow.DoClose;
begin
  if Assigned( FOnClose ) then
  begin
    FOnClose;
  end;
end;

procedure TSDLBaseWindow.DoCreate;
begin
  if Assigned( FOnCreate ) then
  begin
    FOnCreate;
  end;
end;

procedure TSDLBaseWindow.DoDestroy;
begin
  if Assigned( FOnDestroy ) then
  begin
    FOnDestroy;
  end;
end;

procedure TSDLBaseWindow.DoExpose;
begin
  if Assigned( FOnExpose ) then
  begin
    FOnExpose;
  end;
end;

procedure TSDLBaseWindow.DoUpdate( aElapsedTime : single );
begin
  if Assigned( FOnUpdate ) then
  begin
    FOnUpdate( aElapsedTime );
  end;
end;

procedure TSDLBaseWindow.DoQuit;
begin
  FRendering := false;
  if Assigned( FOnQuit ) then
  begin
    FOnQuit;
  end;
end;

procedure TSDLBaseWindow.DoRender;
begin
  if Assigned(  FOnRender ) then
  begin
    FOnRender;
  end;
end;

procedure TSDLBaseWindow.DoResize( aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 );
begin
  // resize to the new size
  SDL_FreeSurface(FDisplaySurface);
  FWidth := aWidth;
  FHeight := aHeight;
  FBitDepth := aBitDepth;
  FVideoFlags := aVideoFlags;
  FDisplaySurface := SDL_SetVideoMode(aWidth, aHeight, aBitDepth, aVideoFlags);
  if Assigned( FOnResize ) then
  begin
    FOnResize( aWidth, aHeight, aBitDepth, aVideoFlags );
  end;
end;

procedure TSDLBaseWindow.DoShow;
begin
  if Assigned( FOnShow ) then
  begin
    FOnShow;
  end;
end;

procedure TSDLBaseWindow.DoUser(aType: UInt8; aCode: integer; aData1, aData2: Pointer);
begin
  if Assigned(  FOnUser ) then
  begin
    FOnUser( aType, aCode, aData1, aData2 );
  end;
end;

function TSDLBaseWindow.Flip : integer;
begin
  result := 0;
end;

procedure TSDLBaseWindow.GetCaption( var aCaptionText : string; var aIconName : string );
begin
  aCaptionText := string( FCaptionText );
  aIconName := string( FIconName );
end;

procedure TSDLBaseWindow.InitialiseEnvironment;
begin
  InitialiseObjects;
  RestoreObjects;
end;

procedure TSDLBaseWindow.InitialiseObjects;
begin
  FLoaded := True;
end;

procedure TSDLBaseWindow.Update( aElapsedTime : single );
begin
  DoUpdate( aElapsedTime );
end;

procedure TSDLBaseWindow.Render;
begin
  DoRender;
end;

procedure TSDLBaseWindow.RestoreObjects;
begin
  FLoaded := false;
end;

procedure TSDLBaseWindow.SetCaption( const aCaptionText : string; const aIconName : string );
begin
  if FCaptionText <> aCaptionText then
  begin
    FCaptionText := PChar( aCaptionText );
    FIconName := PChar( aIconName );
    SDL_WM_SetCaption( FCaptionText, FIconName );
  end;
end;

procedure TSDLBaseWindow.SetIcon(aIcon: PSDL_Surface; aMask: UInt8);
begin
  SDL_WM_SetIcon( aIcon, aMask );
end;

function TSDLBaseWindow.Show : Boolean;
var
  eBaseWindowEvent : TSDL_Event;
begin
  DoShow;

  FTimer.Init;

  FRendering := true;
  // repeat until we are told not to render
  while FRendering do
  begin
    // wait for an event
    while SDL_PollEvent( @eBaseWindowEvent ) > 0 do
    begin

      // check for a quit event
      case eBaseWindowEvent.type_ of
        SDL_ACTIVEEVENT :
        begin
          DoActive( eBaseWindowEvent.active.gain, eBaseWindowEvent.active.state );
        end;

        SDL_QUITEV :
        begin
          DoQuit;
          DoClose;
        end;

        SDL_USEREVENT :
        begin
          DoUser( eBaseWindowEvent.user.type_, eBaseWindowEvent.user.code, eBaseWindowEvent.user.data1, eBaseWindowEvent.user.data2 );
        end;

        SDL_VIDEOEXPOSE :
        begin
          DoExpose;
        end;

        SDL_VIDEORESIZE :
        begin
          DoResize( eBaseWindowEvent.resize.w, eBaseWindowEvent.resize.h, FDisplaySurface.format.BitsPerPixel, FVideoflags );
        end;


      end;
      InputManager.UpdateInputs( eBaseWindowEvent );
    end;
    // Prepare the Next Frame
    Update( FTimer.GetElapsedSeconds );
    // Display the Next Frame
    Render;
    // Flip the surfaces
    Flip;
  end;

  Result := FRendering;
end;

{ TSDL2DWindow }

constructor TSDL2DWindow.Create(aWidth, aHeight, aBitDepth: integer; aVideoFlags: Uint32);
begin
  // make sure double buffer is always included in the video flags
  inherited Create(aWidth,aHeight, aBitDepth, aVideoFlags or SDL_DOUBLEBUF);
end;

procedure TSDL2DWindow.DeleteObjects;
begin
  inherited;

end;

function TSDL2DWindow.Flip: integer;
begin
  // let's show the back buffer
  result := SDL_Flip( FDisplaySurface );
end;

procedure TSDL2DWindow.InitialiseObjects;
begin
  inherited;

end;

procedure TSDL2DWindow.Update( aElapsedTime : single );
begin
  inherited;

end;

procedure TSDL2DWindow.Render;
begin
  inherited;

end;

procedure TSDL2DWindow.RestoreObjects;
begin
  inherited;

end;

{ TSDL3DWindow }

constructor TSDL3DWindow.Create(aWidth,
  aHeight, aBitDepth: integer; aVideoFlags: Uint32);
begin
  // make sure opengl is always included in the video flags
  inherited Create(aWidth,aHeight, aBitDepth, aVideoFlags or SDL_OPENGL or SDL_DOUBLEBUF);
end;

procedure TSDL3DWindow.DeleteObjects;
begin
  inherited;

end;

function TSDL3DWindow.Flip : integer;
begin
  SDL_GL_SwapBuffers;
  result := 0;
end;

procedure TSDL3DWindow.InitialiseObjects;
begin
  inherited;

end;

procedure TSDL3DWindow.Update( aElapsedTime : single );
begin
  inherited;

end;

procedure TSDL3DWindow.Render;
begin
  inherited;

end;

procedure TSDL3DWindow.RestoreObjects;
begin
  inherited;

end;

end.
