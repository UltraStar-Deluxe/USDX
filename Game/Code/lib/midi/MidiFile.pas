{
  Load a midifile and get access to tracks and events
  I did build this component to convert midifiles to wave files
  or play the files on a software synthesizer which I'm currenly
  building.

  version 1.0 first release

  version 1.1
    added some function
    function KeyToStr(key : integer) : string;
    function MyTimeToStr(val : integer) : string;
    Bpm can be set to change speed

  version 1.2
    added some functions
    function  GetTrackLength:integer;
    function  Ready: boolean;

  version 1.3
    update by Chulwoong,
    He knows how to use the MM timer, the timing is much better now, thank you

  for comments/bugs
  F.Bouwmans
  fbouwmans@spiditel.nl

  if you think this component is nice and you use it, sent me a short email.
  I've seen that other of my components have been downloaded a lot, but I've
  got no clue wether they are actually used.
  Don't worry because you are free to use these components

  Timing has improved, however because the messages are handled by the normal
  windows message loop (of the main window) it is still influenced by actions
  done on the window (minimize/maximize ..).
  Use of a second thread with higher priority which only handles the
  timer message should increase performance. If somebody knows such a component
  which is freeware please let me know.

  interface description:

  procedure ReadFile:
    actually read the file which is set in Filename

  function GetTrack(index: integer) : TMidiTrack;

  property Filename
    set/read filename of midifile

  property NumberOfTracks
    read number of tracks in current file

  property TicksPerQuarter: integer
    ticks per quarter, tells how to interpret the time value in midi events

  property FileFormat: TFileFormat
    tells the format of the current midifile

  property Bpm:integer
    tells Beats per minut

  property OnMidiEvent:TOnMidiEvent
    called while playing for each midi event

  procedure StartPlaying;
    start playing the current loaded midifile from the beginning

  procedure StopPlaying;
    stop playing the current midifile

  procedure PlayToTime(time : integer);
    if playing yourself then events from last time to this time are produced


  function KeyToStr(key : integer) : string;
      give note string on key value:  e.g. C4

  function MyTimeToStr(val : integer) : string;
      give time string from msec time

  function  GetTrackLength:integer;
      gives the track lenght in msec (assuming the bpm at the start oof the file)

  function  Ready: boolean;
      now you can check wether the playback is finished

}

unit MidiFile;

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+} // use AnsiString
{$ENDIF}

uses
  Windows,
  //Forms,
  Messages,
  Classes,
  {$IFDEF FPC}
  WinAllocation,
  {$ENDIF}
  SysUtils;

type
  TChunkType = (illegal, header, track);
  TFileFormat = (single, multi_synch, multi_asynch);
  PByte = ^byte;

  TMidiEvent = record
    event: byte;
    data1: byte;
    data2: byte;
    str: string;
    dticks: integer;
    time: integer;
    mtime: integer;
    len: integer;
  end;
  PMidiEvent = ^TMidiEvent;

  TOnMidiEvent = procedure(event: PMidiEvent) of object;
  TEvent = procedure of object;

  TMidiTrack = class(TObject)
  protected
    events: TList;
      name: string;
    instrument: string;
    currentTime: integer;
    currentPos: integer;
    ready: boolean;
    trackLenght: integer;
    procedure checkReady;
  public
    OnMidiEvent: TOnMidiEvent;
    OnTrackReady: TEvent;
    constructor Create;
    destructor Destroy; override;

    procedure Rewind(pos: integer);
    procedure PlayUntil(pos: integer);
    procedure GoUntil(pos: integer);

    procedure putEvent(event: PMidiEvent);
    function getEvent(index: integer): PMidiEvent;
    function getName: string;
    function getInstrument: string;
    function getEventCount: integer;
    function getCurrentTime: integer;
    function getTrackLength: integer;
    function isReady:boolean;
  end;

  TMidiFile = class(TComponent)
  private
    { Private declarations }
    procedure MidiTimer(sender : TObject);
    procedure WndProc(var Msg : TMessage);
  protected
    { Protected declarations }
    midiFile: file of byte;
    chunkType: TChunkType;
    chunkLength: integer;
    chunkData: PByte;
    chunkIndex: PByte;
    chunkEnd: PByte;
    FPriority: DWORD;

    // midi file attributes
    FFileFormat: TFileFormat;
    numberTracks: integer;
    deltaTicks: integer;
    FBpm: integer;
    FBeatsPerMeasure: integer;
    FusPerTick: double;
    FFilename: string;

    Tracks: TList;
    currentTrack: TMidiTrack;
    FOnMidiEvent: TOnMidiEvent;
    FOnUpdateEvent: TNotifyEvent;

    // playing attributes
    playing: boolean;
    PlayStartTime: integer;
    currentTime: integer; // Current playtime in msec
    currentPos: Double; // Current Position in ticks

    procedure OnTrackReady;
    procedure setFilename(val: string);
    procedure ReadChunkHeader;
    procedure ReadChunkContent;
    procedure ReadChunk;
    procedure ProcessHeaderChunk;
    procedure ProcessTrackChunk;
    function ReadVarLength: integer;
    function ReadString(l: integer): string;
    procedure SetOnMidiEvent(handler: TOnMidiEvent);
    procedure SetBpm(val: integer);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ReadFile;
    function GetTrack(index: integer): TMidiTrack;

    procedure StartPlaying;
    procedure StopPlaying;
    procedure ContinuePlaying;

    procedure PlayToTime(time: integer);
    procedure GoToTime(time: integer);
    function GetCurrentTime: integer;
    function GetFusPerTick : Double;
    function  GetTrackLength:integer;
    function  Ready: boolean;
  published
    { Published declarations }
    property Filename: string read FFilename write setFilename;
    property NumberOfTracks: integer read numberTracks;
    property TicksPerQuarter: integer read deltaTicks;
    property FileFormat: TFileFormat read FFileFormat;
    property Bpm: integer read FBpm write SetBpm;
    property OnMidiEvent: TOnMidiEvent read FOnMidiEvent write SetOnMidiEvent;
    property OnUpdateEvent: TNotifyEvent read FOnUpdateEvent write FOnUpdateEvent;
  end;

function KeyToStr(key: integer): string;
function MyTimeToStr(val: integer): string;
procedure Register;

implementation

uses mmsystem;

type
{$IFDEF FPC}
  TTimerProc = TTIMECALLBACK;
  TTimeCaps = TIMECAPS;
{$ELSE}
  TTimerProc = TFNTimeCallBack;
{$ENDIF}

const TIMER_RESOLUTION=10;
const WM_MULTIMEDIA_TIMER=WM_USER+127;

var MIDIFileHandle : HWND;
    TimerProc      : TTimerProc;
    MIDITimerID    : Integer;
    TimerPeriod     : Integer;

procedure TimerCallBackProc(uTimerID,uMsg: Cardinal; dwUser,dwParam1,dwParam2:DWORD);stdcall;
begin
     PostMessage(HWND(dwUser),WM_MULTIMEDIA_TIMER,0,0);
end;

procedure SetMIDITimer;
  var TimeCaps    : TTimeCaps;
begin
  timeGetDevCaps(@TimeCaps,SizeOf(TimeCaps));
  if TIMER_RESOLUTION < TimeCaps.wPeriodMin then
    TimerPeriod:=TimeCaps.wPeriodMin
  else if TIMER_RESOLUTION > TimeCaps.wPeriodMax then
    TimerPeriod:=TimeCaps.wPeriodMax
  else
    TimerPeriod:=TIMER_RESOLUTION;

  timeBeginPeriod(TimerPeriod);
  MIDITimerID:=timeSetEvent(TimerPeriod,TimerPeriod,TimerProc,
                            DWORD(MIDIFileHandle),TIME_PERIODIC);
  if MIDITimerID=0 then
    timeEndPeriod(TimerPeriod);
end;

procedure KillMIDITimer;
begin
  timeKillEvent(MIDITimerID);
  timeEndPeriod(TimerPeriod);
end;

constructor TMidiTrack.Create;
begin
  inherited Create;
  events := TList.Create;
  currentTime := 0;
  currentPos := 0;
end;

destructor TMidiTrack.Destroy;
var
  i: integer;
begin
  for i := 0 to events.count - 1 do
    Dispose(PMidiEvent(events.items[i]));
  events.Free;
  inherited Destroy;
end;

procedure TMidiTRack.putEvent(event: PMidiEvent);
var
  command: integer;
  i: integer;
  pevent: PMidiEvent;
begin
  if (event.event = $FF) then
  begin
    if (event.data1 = 3) then
      name := event.str;
    if (event.data1 = 4) then
      instrument := event.str;
  end;
  currentTime := currentTime + event.dticks;
  event.time := currentTime; // for the moment just add dticks
  event.len := 0;
  events.add(TObject(event));
  command := event.event and $F0;

  if ((command = $80) // note off
    or ((command = $90) and (event.data2 = 0))) //note on with speed 0
  then
  begin
    // this is a note off, try to find the accompanion note on
    command := event.event or $90;
    i := events.count - 2;
    while i >= 0 do
    begin
      pevent := PMidiEvent(events[i]);
      if (pevent.event = command) and
        (pevent.data1 = event.data1)
        then
      begin
        pevent.len := currentTIme - pevent.time;
        i := 0;
        event.len := -1;
      end;
      dec(i);
    end;
  end;
end;

function TMidiTrack.getName: string;
begin
  result := name;
end;

function TMidiTrack.getInstrument: string;
begin
  result := instrument;
end;

function TMiditrack.getEventCount: integer;
begin
  result := events.count;
end;

function TMiditrack.getEvent(index: integer): PMidiEvent;
begin
  if ((index < events.count) and (index >= 0)) then
    result := events[index]
  else
    result := nil;
end;

function TMiditrack.getCurrentTime: integer;
begin
  result := currentTime;
end;

procedure TMiditrack.Rewind(pos: integer);
begin
  if currentPos = events.count then
    dec(currentPos);
  while ((currentPos > 0) and
    (PMidiEvent(events[currentPos]).time > pos))
    do
  begin
    dec(currentPos);
  end;
  checkReady;
end;

procedure TMiditrack.PlayUntil(pos: integer);
begin
  if assigned(OnMidiEvent) then
  begin
    while ((currentPos < events.count) and
      (PMidiEvent(events[currentPos]).time < pos)) do
    begin
      OnMidiEvent(PMidiEvent(events[currentPos]));
      inc(currentPos);
    end;
  end;
  checkReady;
end;

procedure TMidiTrack.GoUntil(pos: integer);
begin
  while ((currentPos < events.count) and
    (PMidiEvent(events[currentPos]).time < pos)) do
  begin
    inc(currentPos);
  end;
  checkReady;
end;

procedure TMidiTrack.checkReady;
begin
  if currentPos >= events.count then
  begin
    ready := true;
    if assigned(OnTrackReady) then
      OnTrackReady;
  end
  else
    ready := false;
end;

function TMidiTrack.getTrackLength: integer;
begin
  result := PMidiEvent(events[events.count-1]).time
end;

function TMidiTrack.isReady: boolean;
begin
  result := ready;
end;

constructor TMidifile.Create(AOwner: TComponent);
begin
  inherited Create(AOWner);
  MIDIFileHandle:=AllocateHWnd(WndProc);
  chunkData := nil;
  chunkType := illegal;
  Tracks := TList.Create;
  TimerProc:=@TimerCallBackProc;
  FPriority:=GetPriorityClass(MIDIFileHandle);
end;

destructor TMidifile.Destroy;
var
  i: integer;
begin
  if not (chunkData = nil) then FreeMem(chunkData);
  for i := 0 to Tracks.Count - 1 do
    TMidiTrack(Tracks.Items[i]).Free;
  Tracks.Free;
  SetPriorityClass(MIDIFileHandle,FPriority);

  if MIDITimerID<>0 then KillMIDITimer;

  DeallocateHWnd(MIDIFileHandle);

  inherited Destroy;
end;

function TMidiFile.GetTrack(index: integer): TMidiTrack;
begin
  result := Tracks.Items[index];
end;

procedure TMidifile.setFilename(val: string);
begin
  FFilename := val;
//  ReadFile;
end;

procedure TMidifile.SetOnMidiEvent(handler: TOnMidiEvent);
var
  i: integer;
begin
//  if not (FOnMidiEvent = handler) then
//  begin
  FOnMidiEvent := handler;
  for i := 0 to tracks.count - 1 do
    TMidiTrack(tracks.items[i]).OnMidiEvent := handler;
//  end;
end;

{$WARNINGS OFF}
procedure TMidifile.MidiTimer(Sender: TObject);
begin
  if playing then
  begin
    PlayToTime(GetTickCount - PlayStartTime);
    if assigned(FOnUpdateEvent) then FOnUpdateEvent(self);
  end;
end;
{$WARNINGS ON}

procedure TMidifile.StartPlaying;
var
  i: integer;
begin
  for i := 0 to tracks.count - 1 do
    TMidiTrack(tracks[i]).Rewind(0);
  playStartTime := getTickCount;
  playing := true;

  SetPriorityClass(MIDIFileHandle,REALTIME_PRIORITY_CLASS);

  SetMIDITimer;
  currentPos := 0.0;
  currentTime := 0;
end;

{$WARNINGS OFF}
procedure TMidifile.ContinuePlaying;
begin
  PlayStartTime := GetTickCount - currentTime;
  playing := true;

  SetPriorityClass(MIDIFileHandle,REALTIME_PRIORITY_CLASS);

  SetMIDITimer;
end;
{$WARNINGS ON}

procedure TMidifile.StopPlaying;
begin
  playing := false;
  KillMIDITimer;
  SetPriorityClass(MIDIFileHandle,FPriority);
end;

function TMidiFile.GetCurrentTime: integer;
begin
  Result := currentTime;
end;

procedure TMidifile.PlayToTime(time: integer);
var
  i: integer;
  track: TMidiTrack;
  pos: integer;
  deltaTime: integer;
begin
  // calculate the pos in the file.
  // pos is actually tick
  // Current FusPerTick is uses to determine the actual pos

  deltaTime := time - currentTime;
  currentPos := currentPos + (deltaTime * 1000) / FusPerTick;
  pos := round(currentPos);

  for i := 0 to tracks.count - 1 do
  begin
    TMidiTrack(tracks.items[i]).PlayUntil(pos);
  end;
  currentTime := time;
end;

procedure TMidifile.GoToTime(time: integer);
var
  i: integer;
  track: TMidiTrack;
  pos: integer;
begin
  // this function should be changed because FusPerTick might not be constant
  pos := round((time * 1000) / FusPerTick);
  for i := 0 to tracks.count - 1 do
  begin
    TMidiTrack(tracks.items[i]).Rewind(0);
    TMidiTrack(tracks.items[i]).GoUntil(pos);
  end;
end;

procedure TMidifile.SetBpm(val: integer);
var
  us_per_quarter: integer;
begin
  if not (val = FBpm) then
  begin
    us_per_quarter := 60000000 div val;

    FBpm := 60000000 div us_per_quarter;
    FusPerTick := us_per_quarter / deltaTicks;
  end;
end;

procedure TMidifile.ReadChunkHeader;
var
  theByte: array[0..7] of byte;
begin
  BlockRead(midiFile, theByte, 8);
  if (theByte[0] = $4D) and (theByte[1] = $54) then
  begin
    if (theByte[2] = $68) and (theByte[3] = $64) then
      chunkType := header
    else if (theByte[2] = $72) and (theByte[3] = $6B) then
      chunkType := track
    else
      chunkType := illegal;
  end
  else
  begin
    chunkType := illegal;
  end;
  chunkLength := theByte[7] + theByte[6] * $100 + theByte[5] * $10000 + theByte[4] * $1000000;
end;

procedure TMidifile.ReadChunkContent;
begin
  if not (chunkData = nil) then
    FreeMem(chunkData);
  GetMem(chunkData, chunkLength + 10);
  BlockRead(midiFile, chunkData^, chunkLength);
  chunkIndex := chunkData;
  chunkEnd := PByte(integer(chunkIndex) + integer(chunkLength) - 1);
end;

procedure TMidifile.ReadChunk;
begin
  ReadChunkHeader;
  ReadChunkContent;
  case chunkType of
    header:
      ProcessHeaderChunk;
    track:
      ProcessTrackCHunk;
  end;
end;

procedure TMidifile.ProcessHeaderChunk;
begin
  chunkIndex := chunkData;
  inc(chunkIndex);
  if chunkType = header then
  begin
    case chunkIndex^ of
      0: FfileFormat := single;
      1: FfileFormat := multi_synch;
      2: FfileFormat := multi_asynch;
    end;
    inc(chunkIndex);
    numberTracks := chunkIndex^ * $100;
    inc(chunkIndex);
    numberTracks := numberTracks + chunkIndex^;
    inc(chunkIndex);
    deltaTicks := chunkIndex^ * $100;
    inc(chunkIndex);
    deltaTicks := deltaTicks + chunkIndex^;
  end;
end;

procedure TMidifile.ProcessTrackChunk;
var
  dTime: integer;
  event: integer;
  len: integer;
  str: string;
  midiEvent: PMidiEvent;
  i: integer;
  us_per_quarter: integer;
begin
  chunkIndex := chunkData;
//  inc(chunkIndex);
  event := 0;
  if chunkType = track then
  begin
    currentTrack := TMidiTrack.Create;
    currentTrack.OnMidiEvent := FOnMidiEvent;
    Tracks.add(currentTrack);
    while integer(chunkIndex) < integer(chunkEnd) do
    begin
      // each event starts with var length delta time
      dTime := ReadVarLength;
      if chunkIndex^ >= $80 then
      begin
        event := chunkIndex^;
        inc(chunkIndex);
      end;
      // else it is a running status event (just the same event as before)

      if event = $FF then
      begin
{        case chunkIndex^ of
        $00: // sequence number, not implemented jet
            begin
              inc(chunkIndex); // $02
              inc(chunkIndex);
            end;
        $01 .. $0f: // text events  FF ty len text
            begin
              New(midiEvent);
              midiEvent.event := $FF;
              midiEvent.data1 := chunkIndex^;     // type is stored in data1
              midiEvent.dticks := dtime;

              inc(chunkIndex);
              len := ReadVarLength;
              midiEvent.str    := ReadString(len);

              currentTrack.putEvent(midiEvent);
            end;
        $20: // Midi channel prefix  FF 20 01 cc
             begin
               inc(chunkIndex); // $01
               inc(chunkIndex); // channel
               inc(chunkIndex);
             end;
        $2F: // End of track FF 2F 00
             begin
               inc(chunkIndex); // $00
               inc(chunkIndex);
             end;
        $51: // Set Tempo  FF 51 03 tttttt
             begin
               inc(chunkIndex); // $03
               inc(chunkIndex); // tt
               inc(chunkIndex); // tt
               inc(chunkIndex); // tt
               inc(chunkIndex);
             end;
        $54: // SMPTE offset  FF 54 05 hr mn se fr ff
             begin
               inc(chunkIndex); // $05
               inc(chunkIndex); // hr
               inc(chunkIndex); // mn
               inc(chunkIndex); // se
               inc(chunkIndex); // fr
               inc(chunkIndex); // ff
               inc(chunkIndex);
             end;
        $58: // Time signature FF 58 04 nn dd cc bb
             begin
               inc(chunkIndex); // $04
               inc(chunkIndex); // nn
               inc(chunkIndex); // dd
               inc(chunkIndex); // cc
               inc(chunkIndex); // bb
               inc(chunkIndex);
             end;
        $59: // Key signature FF 59 02 df mi
             begin
               inc(chunkIndex); // $02
               inc(chunkIndex); // df
               inc(chunkIndex); // mi
               inc(chunkIndex);
             end;
        $7F: // Sequence specific Meta-event
            begin
              inc(chunkIndex);
              len := ReadVarLength;
              str := ReadString(len);
            end;
        else // unknown meta event
        }
        begin
          New(midiEvent);
          midiEvent.event := $FF;
          midiEvent.data1 := chunkIndex^; // type is stored in data1
          midiEvent.dticks := dtime;

          inc(chunkIndex);
          len := ReadVarLength;
          midiEvent.str := ReadString(len);
          currentTrack.putEvent(midiEvent);

          case midiEvent.data1 of
            $51:
              begin
                us_per_quarter :=
                  (integer(byte(midiEvent.str[1])) shl 16 +
                  integer(byte(midiEvent.str[2])) shl 8 +
                  integer(byte(midiEvent.str[3])));
                FBpm := 60000000 div us_per_quarter;
                FusPerTick := us_per_quarter / deltaTicks;
              end;
          end;
        end;
//        end;
      end
      else
      begin
      // these are all midi events
        New(midiEvent);
        midiEvent.event := event;
        midiEvent.dticks := dtime;
//         inc(chunkIndex);
        case event of
          $80..$8F, // note off
          $90..$9F, // note on
          $A0..$AF, // key aftertouch
          $B0..$BF, // control change
          $E0..$EF: // pitch wheel change
            begin
              midiEvent.data1 := chunkIndex^; inc(chunkIndex);
              midiEvent.data2 := chunkIndex^; inc(chunkIndex);
            end;
          $C0..$CF, // program change
          $D0..$DF: // channel aftertouch
            begin
              midiEvent.data1 := chunkIndex^; inc(chunkIndex);
            end;
        else
           // error
        end;
        currentTrack.putEvent(midiEvent);
      end;
    end;
  end;
end;


function TMidifile.ReadVarLength: integer;
var
  i: integer;
  b: byte;
begin
  b := 128;
  i := 0;
  while b > 127 do
  begin
    i := i shl 7;
    b := chunkIndex^;
    i := i + b and $7F;
    inc(chunkIndex);
  end;
  result := i;
end;

function TMidifile.ReadString(l: integer): string;
var
  s: PChar;
  i: integer;
begin
  GetMem(s, l + 1); ;
  s[l] := chr(0);
  for i := 0 to l - 1 do
  begin
    s[i] := Chr(chunkIndex^);
    inc(chunkIndex);
  end;
  result := string(s);
end;

procedure TMidifile.ReadFile;
var
  i: integer;
begin
  for i := 0 to Tracks.Count - 1 do
    TMidiTrack(Tracks.Items[i]).Free;
  Tracks.Clear;
  chunkType := illegal;

  AssignFile(midiFile, FFilename);
  FileMode := 0;
  Reset(midiFile);
  while not eof(midiFile) do
    ReadChunk;
  CloseFile(midiFile);
  numberTracks := Tracks.Count;
end;

function KeyToStr(key: integer): string;
var
  n: integer;
  str: string;
begin
  n := key mod 12;
  case n of
    0: str := 'C';
    1: str := 'C#';
    2: str := 'D';
    3: str := 'D#';
    4: str := 'E';
    5: str := 'F';
    6: str := 'F#';
    7: str := 'G';
    8: str := 'G#';
    9: str := 'A';
    10: str := 'A#';
    11: str := 'B';
  end;
  Result := str + IntToStr(key div 12);
end;

function IntToLenStr(val: integer; len: integer): string;
var
  str: string;
begin
  str := IntToStr(val);
  while Length(str) < len do
    str := '0' + str;
  Result := str;
end;

function MyTimeToStr(val: integer): string;
 var
  hour: integer;
  min: integer;
  sec: integer;
  msec: integer;
begin
  msec := val mod 1000;
  sec := val div 1000;
  min := sec div 60;
  sec := sec mod 60;
  hour := min div 60;
  min := min mod 60;
  Result := IntToStr(hour) + ':' + IntToLenStr(min, 2) + ':' + IntToLenStr(sec, 2) + '.' + IntToLenStr(msec, 3);
end;

function TMidiFIle.GetFusPerTick : Double;
begin
  Result := FusPerTick;
end;

function  TMidiFIle.GetTrackLength:integer;
var i,length : integer;
    time : extended;
begin
  length := 0;
  for i := 0 to Tracks.Count - 1 do
    if TMidiTrack(Tracks.Items[i]).getTrackLength > length then
      length := TMidiTrack(Tracks.Items[i]).getTrackLength;
  time := length * FusPerTick;
  time := time / 1000.0;
  result := round(time);
end;

function  TMidiFIle.Ready: boolean;
var i : integer;
begin
  result := true;
  for i := 0 to Tracks.Count - 1 do
    if not TMidiTrack(Tracks.Items[i]).isready then
      result := false;
end;

procedure TMidiFile.OnTrackReady;
begin
  if ready then
    if assigned(FOnUpdateEvent) then FOnUpdateEvent(self);
end;

procedure TMidiFile.WndProc(var Msg : TMessage);
begin
  with MSG do
  begin
    case Msg of
      WM_MULTIMEDIA_TIMER:
      begin
        //try
          MidiTimer(self);
        //except
        // Note: HandleException() is called by default if exception is not handled
        //  Application.HandleException(Self);
        //end;
      end;
    else
      begin
        Result := DefWindowProc(MIDIFileHandle, Msg, wParam, lParam);
      end;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Synth', [TMidiFile]);
end;

end.

