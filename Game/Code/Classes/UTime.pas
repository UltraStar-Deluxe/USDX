unit UTime;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

type
  TTime = class
    constructor Create;
    function    GetTime: real;
  end;

procedure CountSkipTimeSet;
procedure CountSkipTime;
procedure CountMidTime;

var
  USTime      : TTime;
  
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


constructor TTime.Create;
begin
  inherited;
  CountSkipTimeSet;
end;

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

function TTime.GetTime: real;
begin
  Result      := SDL_GetTicks() / cSDLCorrectionRatio;
end;

end.
