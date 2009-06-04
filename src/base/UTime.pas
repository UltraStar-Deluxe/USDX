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

  TRelativeTimer = class
    private
      AbsoluteTime: int64;      // system-clock reference time for calculation of CurrentTime
      RelativeTimeOffset: real;
      Paused: boolean;
      TriggerMode: boolean;
    public
      constructor Create(TriggerMode: boolean = false);
      procedure Pause();
      procedure Resume();
      function GetTime(): real;
      function GetAndResetTime(): real;
      procedure SetTime(Time: real; Trigger: boolean = true);
      procedure Reset();
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

(*
 * creates a new timer.
 * if triggermode is false (default), the timer
 * will immediately begin with counting.
 * if triggermode is true, it will wait until get/settime() or pause() is called
 * for the first time.
 *)
constructor TRelativeTimer.Create(TriggerMode: boolean);
begin
  inherited Create();
  Self.TriggerMode := TriggerMode;
  Reset();
  Paused := false;
end;

procedure TRelativeTimer.Pause();
begin
  RelativeTimeOffset := GetTime();
  Paused := true;
end;

procedure TRelativeTimer.Resume();
begin
  AbsoluteTime := SDL_GetTicks();
  Paused := false;
end;

(*
 * Returns the counter of the timer.
 * If in TriggerMode it will return 0 and start the counter on the first call.
 *)
function TRelativeTimer.GetTime: real;
begin
  // initialize absolute time on first call in triggered mode
  if (TriggerMode and (AbsoluteTime = 0)) then
  begin
    AbsoluteTime := SDL_GetTicks();
    Result := RelativeTimeOffset;
    Exit;
  end;

  if Paused then
    Result := RelativeTimeOffset
  else
    Result := RelativeTimeOffset + (SDL_GetTicks() - AbsoluteTime) / cSDLCorrectionRatio;
end;

(*
 * Returns the counter of the timer and resets the counter to 0 afterwards.
 * Note: In TriggerMode the counter will not be stopped as with Reset().
 *)
function TRelativeTimer.GetAndResetTime(): real;
begin
  Result := GetTime();
  SetTime(0);
end;

(*
 * Sets the timer to the given time. This will trigger in TriggerMode if
 * Trigger is set to true. Otherwise the counter's state will not change.
 *)
procedure TRelativeTimer.SetTime(Time: real; Trigger: boolean);
begin
  RelativeTimeOffset := Time;
  if ((not TriggerMode) or Trigger) then
    AbsoluteTime := SDL_GetTicks();
end;

(*
 * Resets the counter of the timer to 0.
 * If in TriggerMode the timer will not start counting until it is triggered again.
 *)
procedure TRelativeTimer.Reset();
begin
  RelativeTimeOffset := 0;
  if (TriggerMode) then
    AbsoluteTime := 0
  else
    AbsoluteTime := SDL_GetTicks();
end;

end.
