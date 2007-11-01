unit ULight;

interface

{$I switches.inc}

type
  TLight = class
    private
      Enabled:      boolean;
      Light:        array[0..7] of boolean;
      LightTime:    array[0..7] of real;    // time to stop, need to call update to change state
      LastTime:     real;
    public
      constructor Create;
      procedure Enable;
      procedure SetState(State: integer);
      procedure AutoSetState;
      procedure TurnOn;
      procedure TurnOff;
      procedure LightOne(Number: integer; Time: real);
      procedure Refresh;
  end;

var
  Light:      TLight;

const
  Data    = $378; // default port address
  Status  = Data + 1;
  Control = Data + 2;

implementation

uses
  SysUtils,
  {$IFDEF UseSerialPort}
  zlportio,
  {$ENDIF}
  {$IFNDEF win32}
  libc,
  {$ENDIF}
  UTime;
  
{$IFDEF FPC}

  function GetTime: TDateTime;
  {$IFDEF win32}
  var
    SystemTime: TSystemTime;
  begin
    GetLocalTime(SystemTime);
    with SystemTime do
{$IFDEF DARWIN}
      Result := EncodeTime(Hour, Minute, Second, MilliSecond);
{$ELSE}
      Result := EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
{$ENDIF}
  end;
  {$ELSE}
  Type
    Time_t  = longint;
    TTime_T = Time_t;
  var
    T : TTime_T;
    TV: TTimeVal;
    UT: TUnixTime;
  begin
    gettimeofday(TV, nil);
    T := TV.tv_sec;
    localtime_r(@T, @UT);
    Result := EncodeTime(UT.tm_hour, UT.tm_min, UT.tm_sec, TV.tv_usec div 1000);
  end;
  {$ENDIF}
  
{$ENDIF}


constructor TLight.Create;
begin
  Enabled := false;
end;

procedure TLight.Enable;
begin
  Enabled  := true;
  LastTime := GetTime;
end;

procedure TLight.SetState(State: integer);
begin
  {$IFDEF UseSerialPort}
  if Enabled then
    PortWriteB($378, State);
  {$ENDIF}
end;

procedure TLight.AutoSetState;
var
  State:    integer;
begin
  if Enabled then begin
    State := 0;
    if Light[0] then State := State + 2;
    if Light[1] then State := State + 1;
    // etc
    SetState(State);
  end;
end;

procedure TLight.TurnOn;
begin
  if Enabled then
    SetState(3);
end;

procedure TLight.TurnOff;
begin
  if Enabled then
    SetState(0);
end;

procedure TLight.LightOne(Number: integer; Time: real);
begin
  if Enabled then begin
    if Light[Number] = false then begin
      Light[Number] := true;
      AutoSetState;
    end;

    LightTime[Number] := GetTime + Time/1000; // [s]
  end;
end;

procedure TLight.Refresh;
var
  Time:     real;
//  TimeSkip: real;
  L:        integer;
begin
  if Enabled then begin
    Time := GetTime;
//    TimeSkip := Time - LastTime;
    for L := 0 to 7 do begin
      if Light[L] = true then begin
        if LightTime[L] > Time then begin
          // jest jeszcze zapas - bez zmian
          //LightTime[L] := LightTime[L] - TimeSkip;
        end else begin
          // czas minal
          Light[L] := false;
        end;
      end;
    end;
    LastTime := Time;
    AutoSetState;
  end;
end;

end.


