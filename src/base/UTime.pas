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

unit UTime;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

type
  TTime = class
    public
      constructor Create;
      function GetTime(): real;
  end;

  TRelativeTimerState = (rtsStopped, rtsWait, rtsPaused, rtsRunning);

  TRelativeTimer = class
    private
      AbsoluteTime: int64;      // system-clock reference time for calculation of CurrentTime
      RelativeTime: real;
      TriggerMode: boolean;
      State: TRelativeTimerState;
    public
      constructor Create();
      procedure Start(WaitForTrigger: boolean = false);
      procedure Pause();
      procedure Stop();
      function GetTime(): real;
      procedure SetTime(Time: real);
      function GetState(): TRelativeTimerState;
  end;

  TSyncSource = class
    function GetClock(): real; virtual; abstract;
  end;

procedure CountSkipTimeSet;
procedure CountSkipTime;
procedure CountMidTime;

var
  USTime:       TTime;
  VideoBGTimer: TRelativeTimer;

  TimeNew:     int64;
  TimeOld:     int64;
  TimeSkip:    real;
  TimeMid:     real;
  TimeMidTemp: int64;

implementation

uses
  sdl,
  UCommon;
  
const
  cSDLCorrectionRatio = 1000;

(*
BEST Option now ( after discussion with whiteshark ) seems to be to use SDL
timer functions...

SDL_delay
SDL_GetTicks
http://www.gamedev.net/community/forums/topic.asp?topic_id=466145&whichpage=1%EE%8D%B7
*)


procedure CountSkipTimeSet;
begin
  TimeNew := SDL_GetTicks();
end;

procedure CountSkipTime;
begin
  TimeOld  := TimeNew;
  TimeNew  := SDL_GetTicks();
  TimeSkip := (TimeNew-TimeOld) / cSDLCorrectionRatio;
end;

procedure CountMidTime;
begin
  TimeMidTemp := SDL_GetTicks();
  TimeMid     := (TimeMidTemp - TimeNew) / cSDLCorrectionRatio;
end;

{**
 * TTime
 **}

constructor TTime.Create;
begin
  inherited;
  CountSkipTimeSet;
end;

function TTime.GetTime: real;
begin
  Result := SDL_GetTicks() / cSDLCorrectionRatio;
end;

{**
 * TRelativeTimer
 **}

(**
 * Creates a new relative timer.
 * A relative timer works like a stop-watch. It can be paused and
 * resumed afterwards, continuing with the counter it had when it was paused.
 *)
constructor TRelativeTimer.Create();
begin
  State := rtsStopped;
  AbsoluteTime := 0;
  RelativeTime := 0;
end;

(**
 * Starts the timer.
 * If WaitForTrigger is false the timer will be started immediately.
 * If WaitForTrigger is true the timer will be started when a trigger event
 * occurs. A trigger event is a call of one of the Get-/SetTime() methods.
 * In addition the timer can be started by calling this method again with
 * WaitForTrigger set to false.
 *)
procedure TRelativeTimer.Start(WaitForTrigger: boolean = false);
begin
  case (State) of
    rtsStopped, rtsPaused: begin
      if (WaitForTrigger) then
      begin
        State := rtsWait;
      end
      else
      begin
        State := rtsRunning;
        AbsoluteTime := SDL_GetTicks();
      end;
    end;

    rtsWait: begin
      if (not WaitForTrigger) then
      begin
        State := rtsRunning;
        AbsoluteTime := SDL_GetTicks();
        RelativeTime := 0;
      end;
    end;
  end;
end;

(**
 * Pauses the timer and leaves the counter untouched.
 *)
procedure TRelativeTimer.Pause();
begin
  if (State = rtsRunning) then
  begin
    // Important: GetTime() must be called in running state
    RelativeTime := GetTime();
    State := rtsPaused;
  end;
end;

(**
 * Stops the timer and sets its counter to 0.
 *)
procedure TRelativeTimer.Stop();
begin
  if (State <> rtsStopped) then
  begin
    State := rtsStopped;
    RelativeTime := 0;
  end;
end;

(**
 * Returns the current counter of the timer.
 * If WaitForTrigger was true in Start() the timer will be started
 * if it was not already running.
 *)
function TRelativeTimer.GetTime(): real;
begin
  case (State) of
    rtsStopped, rtsPaused:
      Result := RelativeTime;
    rtsRunning:
      Result := RelativeTime + (SDL_GetTicks() - AbsoluteTime) / cSDLCorrectionRatio;
    rtsWait: begin
      // start triggered
      State := rtsRunning;
      AbsoluteTime := SDL_GetTicks();
      Result := RelativeTime;
    end;
  end;
end;

(**
 * Sets the counter of the timer.
 * If WaitForTrigger was true in Start() the timer will be started
 * if it was not already running.
 *)
procedure TRelativeTimer.SetTime(Time: real);
begin
  RelativeTime := Time;
  AbsoluteTime := SDL_GetTicks();
  // start triggered
  if (State = rtsWait) then
    State := rtsRunning;
end;

function TRelativeTimer.GetState(): TRelativeTimerState;
begin
  Result := State;
end;

end.
