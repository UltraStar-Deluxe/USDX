unit sdlticks;
{
  $Id: sdlticks.pas,v 1.1 2004/09/30 22:35:47 savage Exp $
  
}
{******************************************************************************}
{                                                                              }
{          JEDI-SDL : Pascal units for SDL - Simple DirectMedia Layer          }
{                   SDL GetTicks Class Wrapper                                 }
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
{                                                                              }
{   September   23 2004 - DL : Initial Creation                                }
{
  $Log: sdlticks.pas,v $
  Revision 1.1  2004/09/30 22:35:47  savage
  Changes, enhancements and additions as required to get SoAoS working.


}
{******************************************************************************}

interface

uses
  sdl;


type
  TSDLTicks = class
  private
    m_startTime : Int64;
    m_ticksPerSecond : Int64;
    s_lastTime : Int64;

  public
    constructor Create;
    destructor Destroy; override;          // destructor

    {*****************************************************************************
     Init

     If the hi-res timer is present, the tick rate is stored and the function
     returns true. Otherwise, the function returns false, and the timer should
     not be used.
    *****************************************************************************}
    function Init : boolean;

    function GetElapsedSeconds( elapsedFrames : Cardinal = 1 ) : single;

    {***************************************************************************
     GetFPS

     Returns the average frames per second over elapsedFrames, which defaults to
     one. If this is not called every frame, the client should track the number
     of frames itself, and reset the value after this is called.
    ***************************************************************************}
    function GetFPS( elapsedFrames : Cardinal  = 1 )  : single;

    {***************************************************************************
     LockFPS

     Used to lock the frame rate to a set amount. This will block until enough
     time has passed to ensure that the fps won't go over the requested amount.
     Note that this can only keep the fps from going above the specified level;
     it can still drop below it. It is assumed that if used, this function will
     be called every frame. The value returned is the instantaneous fps, which
     will be <= targetFPS.
    ***************************************************************************}
    function LockFPS( targetFPS : Byte ) : single;
  end;

implementation

{ TSDLTicks }
constructor TSDLTicks.Create;
begin

end;

destructor TSDLTicks.Destroy;
begin

  inherited;
end;

function TSDLTicks.GetElapsedSeconds( elapsedFrames: Cardinal ): single;
var
  currentTime : Int64;
begin
  // s_lastTime := m_startTime;

  currentTime := SDL_GetTicks;
  //QueryPerformanceCounter( currentTime );

  result :=  (currentTime - s_lastTime) / m_ticksPerSecond;

  // reset the timer
  s_lastTime := currentTime;
end;

function TSDLTicks.GetFPS( elapsedFrames: Cardinal ): single;
var
  currentTime : integer;
  fps : single;
begin
  // s_lastTime := m_startTime;

  currentTime := SDL_GetTicks;

  fps := elapsedFrames * m_ticksPerSecond / ( currentTime - s_lastTime);

  // reset the timer
  s_lastTime := currentTime;

  result := fps;
end;

function TSDLTicks.Init: boolean;
begin
  m_startTime := SDL_GetTicks;
  s_lastTime := m_startTime;
  m_ticksPerSecond := 1000;
  result := true;
end;

function TSDLTicks.LockFPS(targetFPS: Byte): single;
var
  currentTime : integer;
  fps : single;
begin
  if (targetFPS = 0) then
    targetFPS := 1;

  s_lastTime := m_startTime;

  // delay to maintain a constant frame rate
  repeat
    currentTime := SDL_GetTicks;
    fps := m_ticksPerSecond / (currentTime - s_lastTime);
  until (fps > targetFPS);

  // reset the timer
  s_lastTime := m_startTime;

  result := fps;
end;

end.
 