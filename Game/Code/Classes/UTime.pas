unit UTime;

interface

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

uses Windows;

constructor TTime.Create;
begin
  CountSkipTimeSet;
end;

procedure CountSkipTimeSet;
begin
  QueryPerformanceFrequency(TimeFreq);
  QueryPerformanceCounter(TimeNew);
end;

procedure CountSkipTime;
begin
  TimeOld := TimeNew;
  QueryPerformanceCounter(TimeNew);
  TimeSkip := (TimeNew-TimeOld)/TimeFreq;
end;

procedure CountMidTime;
begin
  QueryPerformanceCounter(TimeMidTemp);
  TimeMid := (TimeMidTemp-TimeNew)/TimeFreq;
end;

procedure TimeSleep(ms: real);
var
  TimeStart:  int64;
  TimeHalf:   int64;
  Time:       real;
  Stop:       boolean;
begin
  QueryPerformanceCounter(TimeStart);

  Stop := false;
  while (not Stop) do begin
    QueryPerformanceCounter(TimeHalf);
    Time := 1000 * (TimeHalf-TimeStart)/TimeFreq;
    if Time > ms then Stop := true;
  end;

end;

function TTime.GetTime: real;
var
  TimeTemp:   int64;
begin
  QueryPerformanceCounter(TimeTemp);
  Result := TimeTemp/TimeFreq;
end;


end.
