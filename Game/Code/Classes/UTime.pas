unit UTime;

interface

{$I switches.inc}

{$UNDEF DebugDisplay}

type
  TTime = class
    constructor Create;
    function GetTime: real;
  end;

procedure CountSkipTimeSet;
procedure CountSkipTime;
procedure CountMidTime;

var
  USTime:   TTime;
  
  TimeNew:  int64;
  TimeOld:  int64;
  TimeSkip: real;
  TimeMid:  real;
  TimeMidTemp:  int64;

implementation

uses
//  sysutils,
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
  CountSkipTimeSet;
end;


procedure CountSkipTimeSet;
begin
  TimeNew  := SDL_GetTicks();

  {$IFDEF DebugDisplay}
  Writeln( 'CountSkipTimeSet : ' + inttostr(trunc(TimeNew)) );
  {$ENDIF}
end;


procedure CountSkipTime;
begin
  TimeOld  := TimeNew;
  TimeNew  := SDL_GetTicks();
  TimeSkip := (TimeNew-TimeOld) / cSDLCorrectionRatio;

  {$IFDEF DebugDisplay}
    Writeln( 'TimeNew       : ' + inttostr(trunc(TimeNew)) );
    Writeln( 'CountSkipTime : ' + inttostr(trunc(TimeSkip)) );
  {$ENDIF}
end;


procedure CountMidTime;
begin
  TimeMidTemp := SDL_GetTicks();
  TimeMid     := (TimeMidTemp - TimeNew) / cSDLCorrectionRatio;

  {$IFDEF DebugDisplay}
  Writeln( 'TimeNew      : ' + inttostr(trunc(TimeNew)) );
  Writeln( 'CountMidTime : ' + inttostr(trunc(TimeMid)) );
  {$ENDIF}
end;


function TTime.GetTime: real;
begin
  Result   := SDL_GetTicks() / cSDLCorrectionRatio;

  {$IFDEF DebugDisplay}
  Writeln( 'GetTime : ' + inttostr(trunc(Result)) );
  {$ENDIF}
end;


end.
