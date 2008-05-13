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
    public
      constructor Create;
      procedure Pause();
      procedure Resume();
      function GetTime(): real;
      procedure SetTime(Time: real);
  end;

procedure CountSkipTimeSet;
procedure CountSkipTime;
procedure CountMidTime;

var
  USTime      : TTime;
  VideoBGTimer: TRelativeTimer;

  TimeNew     : int64;
  TimeOld     : int64;
  TimeSkip    : real;
  TimeMid     : real;
  TimeMidTemp : int64;

implementation

uses
  sdl,
  ucommon;
  
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
  TimeNew     := SDL_GetTicks();
end;

procedure CountSkipTime;
begin
  TimeOld     := TimeNew;
  TimeNew     := SDL_GetTicks();
  TimeSkip    := (TimeNew-TimeOld) / cSDLCorrectionRatio;
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

constructor TRelativeTimer.Create;
begin
  inherited;
  RelativeTimeOffset := 0;
  AbsoluteTime := SDL_GetTicks();
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

function TRelativeTimer.GetTime: real;
begin
  if Paused then
    Result := RelativeTimeOffset
  else
    Result := RelativeTimeOffset + (SDL_GetTicks() - AbsoluteTime) / cSDLCorrectionRatio;
end;

procedure TRelativeTimer.SetTime(Time: real);
begin
  RelativeTimeOffset := Time;
  AbsoluteTime := SDL_GetTicks();
end;


end.
