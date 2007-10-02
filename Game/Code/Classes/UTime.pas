unit UTime;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

type
  TTime = class
    constructor Create;
    function GetTime: real;
  end;

procedure CountSkipTimeSet;
procedure CountSkipTime;
procedure CountMidTime;
procedure TimeSleep(ms: real);

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
  ucommon;


// -- ON Linux it MAY Be better to use ...   clock_gettime() instead of  CurrentSec100OfDay
// who knows how fast or slow that function is !
// but this gets a compile for now .. :)

constructor TTime.Create;
begin
  CountSkipTimeSet;
end;

procedure CountSkipTimeSet;
begin
  {$IFDEF win32}
  QueryPerformanceFrequency(TimeFreq);
  QueryPerformanceCounter(TimeNew);
  {$ELSE}
  TimeNew  := CurrentSec100OfDay();     // TODO - JB_Linux will prob need looking at
  TimeFreq := 0;
  {$ENDIF}
end;

procedure CountSkipTime;
begin
  TimeOld := TimeNew;
  
  {$IFDEF win32}
  QueryPerformanceCounter(TimeNew);
  {$ELSE}
  TimeNew := CurrentSec100OfDay();    // TODO - JB_Linux will prob need looking at
  {$ENDIF}
  
  if ( TimeNew-TimeOld > 0 ) AND
     ( TimeFreq        > 0 ) THEN
  begin
    TimeSkip := (TimeNew-TimeOld)/TimeFreq;
  end;
end;

procedure CountMidTime;
begin
  {$IFDEF win32}
  QueryPerformanceCounter(TimeMidTemp);
  TimeMid := (TimeMidTemp-TimeNew)/TimeFreq;
  {$ELSE}
  TimeMidTemp := CurrentSec100OfDay();
  TimeMid     := (TimeMidTemp-TimeNew);      // TODO - JB_Linux will prob need looking at
  {$ENDIF}
end;

procedure TimeSleep(ms: real);
var
  TimeStart:  int64;
  TimeHalf:   int64;
  Time:       real;
  Stop:       boolean;
begin
  {$IFDEF win32}
  QueryPerformanceCounter(TimeStart);
  {$ELSE}
  TimeStart := CurrentSec100OfDay();   // TODO - JB_Linux will prob need looking at
  {$ENDIF}


  Stop := false;
  while (not Stop) do
  begin
    {$IFDEF win32}
    QueryPerformanceCounter(TimeHalf);
    Time := 1000 * (TimeHalf-TimeStart)/TimeFreq;
    {$ELSE}
    TimeHalf := CurrentSec100OfDay();
    Time := 1000 * (TimeHalf-TimeStart); // TODO - JB_Linux will prob need looking at
    {$ENDIF}

    if Time > ms then
      Stop := true;
  end;

end;

function TTime.GetTime: real;
var
  TimeTemp:   int64;
begin
  {$IFDEF win32}
    QueryPerformanceCounter(TimeTemp);
    Result := TimeTemp / TimeFreq;
  {$ELSE}
    TimeTemp := CurrentSec100OfDay();
    Result   := TimeTemp;  // TODO - JB_Linux will prob need looking at
  {$ENDIF}
end;


end.
