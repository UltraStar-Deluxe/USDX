unit UTime;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$DEFINE SDLTimer}
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
  
  TimeFreq: int64;
  TimeNew:  int64;
  TimeOld:  int64;
  TimeSkip: real;
  TimeMid:  real;
  TimeMidTemp:  int64;

implementation

uses
  {$IFDEF win32}
    windows,
  {$ELSE}
  libc,
  time,
  {$ENDIF}
  sysutils,
  {$IFDEF SDLTimer}
  sdl,
  {$ENDIF}
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
  {$IFDEF SDLTimer}
    TimeNew  := SDL_GetTicks(); // / cSDLCorrectionRatio
    TimeFreq := 0;
  {$ELSE}
    {$IFDEF win32}
    QueryPerformanceFrequency(TimeFreq);
    QueryPerformanceCounter(TimeNew);
    {$ELSE}
    TimeNew  := CurrentSec100OfDay();     // TODO - JB_Linux will prob need looking at
    TimeFreq := 0;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF DebugDisplay}
  Writeln( 'CountSkipTimeSet : ' + inttostr(trunc(TimeNew)) );
  {$ENDIF}
end;


procedure CountSkipTime;
begin
  TimeOld := TimeNew;
  
  {$IFDEF SDLTimer}
    TimeNew  := SDL_GetTicks();
    TimeSkip := (TimeNew-TimeOld) / cSDLCorrectionRatio;
  {$ELSE}
    {$IFDEF win32}
    QueryPerformanceCounter(TimeNew);
    
    if ( TimeNew-TimeOld > 0 ) AND
       ( TimeFreq        > 0 ) THEN
    begin
      TimeSkip := (TimeNew-TimeOld)/TimeFreq;
    end;
    
    {$ELSE}
    TimeNew  := CurrentSec100OfDay();    // TODO - JB_Linux will prob need looking at
    TimeSkip := (TimeNew-TimeOld);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF DebugDisplay}
    Writeln( 'TimeNew       : ' + inttostr(trunc(TimeNew)) );
    Writeln( 'CountSkipTime : ' + inttostr(trunc(TimeSkip)) );
  {$ENDIF}
end;


procedure CountMidTime;
begin
  {$IFDEF SDLTimer}
    TimeMidTemp := SDL_GetTicks();
    TimeMid     := (TimeMidTemp - TimeNew) / cSDLCorrectionRatio;
  {$ELSE}
    {$IFDEF win32}
    QueryPerformanceCounter(TimeMidTemp);
    TimeMid := (TimeMidTemp-TimeNew)/TimeFreq;
    {$ELSE}
    TimeMidTemp := CurrentSec100OfDay();
    TimeMid     := (TimeMidTemp-TimeNew);      // TODO - JB_Linux will prob need looking at
    {$ENDIF}
  {$ENDIF}

  {$IFDEF DebugDisplay}
  Writeln( 'TimeNew       : ' + inttostr(trunc(TimeNew)) );
  Writeln( 'CountMidTime : ' + inttostr(trunc(TimeMid)) );
  {$ENDIF}
end;


function TTime.GetTime: real;
var
  TimeTemp:   int64;
begin
  {$IFDEF SDLTimer}
    TimeTemp := SDL_GetTicks();
    Result   := TimeTemp / cSDLCorrectionRatio;  // TODO - JB_Linux will prob need looking at
  {$ELSE}
    {$IFDEF win32}
      QueryPerformanceCounter(TimeTemp);
      Result := TimeTemp / TimeFreq;
    {$ELSE}
      TimeTemp := CurrentSec100OfDay();
      Result   := TimeTemp;  // TODO - JB_Linux will prob need looking at
    {$ENDIF}
  {$ENDIF}
  
  {$IFDEF DebugDisplay}
  Writeln( 'GetTime : ' + inttostr(trunc(Result)) );
  {$ENDIF}
end;


end.
