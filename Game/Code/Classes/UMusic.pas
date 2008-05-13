unit UMusic;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UTime,
  Classes;

type
  TNoteType = (ntFreestyle, ntNormal, ntGolden);

  PLine = ^TLine;
  TLine = record
    Start:      integer;
    Lyric:      string;
    LyricWidth: real;
    End_:       integer;
    BaseNote:   integer;
    HighNote:   integer;
    TotalNotes: integer;
    LastLine:   boolean;
    Note:     array of record
      Color:      integer;
      Start:      integer;
      Length:     integer;
      Tone:       integer;    // full range tone
      Text:       string;
      NoteType:   TNoteType;
    end;
  end;
  ALine = array of TLine;     // (TODO: rename to TLineArray)

  TLines = record
    Current:    integer;      // for drawing of current line
    High:       integer;
    Number:     integer;
    Resolution: integer;
    NotesGAP:   integer;
    ScoreValue: integer;
    Line:       ALine;
  end;

  TLineState = class        // all that concerns the current frames
    private
      Timer:        TRelativeTimer; // keeps track of the current time
    public
      OldBeat:      integer;    // previous discovered beat
      CurrentBeat:  integer;
      MidBeat:      real;       // like CurrentBeat

      // now we use this for super synchronization!
      // only used when analyzing voice
      OldBeatD:     integer;    // previous discovered beat
      CurrentBeatD: integer;
      MidBeatD:     real;       // like CurrentBeatD
      FracBeatD:    real;       // fractional part of MidBeatD

      // we use this for audible clicks
      OldBeatC:     integer;    // previous discovered beat
      CurrentBeatC: integer;
      MidBeatC:     real;       // like CurrentBeatC
      FracBeatC:    real;       // fractional part of MidBeatC

      OldLine:      integer;    // previous displayed sentence

      TotalTime:    real;       // total song time

      constructor Create();
      procedure Pause();
      procedure Resume();
      function GetCurrentTime(): real;
      procedure SetCurrentTime(Time: real);

      // current song time used as base-timer for lyrics etc.
      property CurrentTime: real READ GetCurrentTime WRITE SetCurrentTime;
  end;


const
  FFTSize = 512; // size of FFT data (output: FFTSize/2 values)
type
  TFFTData  = array[0..(FFTSize div 2)-1] of Single;

type
  PPCMStereoSample = ^TPCMStereoSample;
  TPCMStereoSample = array[0..1] of SmallInt;
  TPCMData  = array[0..511] of TPCMStereoSample;

type
  TStreamStatus = (ssStopped, ssPlaying, ssPaused, ssBlocked, ssUnknown);
const
  StreamStatusStr:  array[TStreamStatus] of string =
    ('Stopped', 'Playing', 'Paused', 'Blocked', 'Unknown');

type
  TAudioSampleFormat = (
    asfU8, asfS8,         // unsigned/signed  8 bits
    asfU16LSB, asfS16LSB, // unsigned/signed 16 bits (endianness: LSB)
    asfU16MSB, asfS16MSB, // unsigned/signed 16 bits (endianness: MSB)
    asfU16, asfS16,       // unsigned/signed 16 bits (endianness: System)
    asfS24,               // signed 24 bits (endianness: System)
    asfS32,               // signed 32 bits (endianness: System)
    asfFloat              // float
  );

const
  // Size of one sample (one channel only) in bytes
  AudioSampleSize: array[TAudioSampleFormat] of integer = (
    1, 1,     // asfU8, asfS8
    2, 2,     // asfU16LSB, asfS16LSB
    2, 2,     // asfU16MSB, asfS16MSB
    2, 2,     // asfU16,    asfS16
    3,        // asfS24
    4,        // asfS32
    4         // asfFloat
  );

type
  TAudioFormatInfo = class
    public
      Channels    : byte;
      SampleRate  : double;
      Format      : TAudioSampleFormat;
      FrameSize   : integer; // calculated on construction

      constructor Create(Channels: byte; SampleRate: double; Format: TAudioSampleFormat);
  end;

type
  TSoundEffect = class
    public
      EngineData: Pointer; // can be used for engine-specific data
      procedure Callback(Buffer: PChar; BufSize: integer); virtual; abstract;
  end;

  TVoiceRemoval = class(TSoundEffect)
    public
      procedure Callback(Buffer: PChar; BufSize: integer); override;
  end;

type
  TAudioProcessingStream = class
    public
      procedure Close();                    virtual; abstract;
  end;

  TAudioPlaybackStream = class(TAudioProcessingStream)
    protected
      function GetPosition: real;           virtual; abstract;
      procedure SetPosition(Time: real);    virtual; abstract;
      function GetLength(): real;           virtual; abstract;
      function GetStatus(): TStreamStatus;  virtual; abstract;
      function GetVolume(): single;         virtual; abstract;
      procedure SetVolume(Volume: single);  virtual; abstract;
      function GetLoop(): boolean;          virtual; abstract;
      procedure SetLoop(Enabled: boolean);  virtual; abstract;
    public
      procedure Play();                     virtual; abstract;
      procedure Pause();                    virtual; abstract;
      procedure Stop();                     virtual; abstract;
      procedure FadeIn(Time: real; TargetVolume: single);  virtual; abstract;

      procedure GetFFTData(var data: TFFTData);          virtual; abstract;
      function GetPCMData(var data: TPCMData): Cardinal; virtual; abstract;

      procedure AddSoundEffect(effect: TSoundEffect);    virtual; abstract;
      procedure RemoveSoundEffect(effect: TSoundEffect); virtual; abstract;

      property Length: real READ GetLength;
      property Position: real READ GetPosition WRITE SetPosition;
      property Status: TStreamStatus READ GetStatus;
      property Volume: single READ GetVolume WRITE SetVolume;
      property Loop: boolean READ GetLoop WRITE SetLoop;
  end;

  TAudioDecodeStream = class(TAudioProcessingStream)
    protected
      function GetLength(): real;           virtual; abstract;
      function GetPosition(): real;         virtual; abstract;
      procedure SetPosition(Time: real);    virtual; abstract;
      function IsEOF(): boolean;            virtual; abstract;
      function IsError(): boolean;          virtual; abstract;
    public
      function ReadData(Buffer: PChar; BufSize: integer): integer; virtual; abstract;
      function GetAudioFormatInfo(): TAudioFormatInfo; virtual; abstract;

      property Length: real READ GetLength;
      property Position: real READ GetPosition WRITE SetPosition;
      property EOF: boolean READ IsEOF;
  end;

type
  TAudioVoiceStream = class(TAudioProcessingStream)
    public
      function ReadData(Buffer: PChar; BufSize: integer): integer; virtual; abstract;
      function GetAudioFormatInfo(): TAudioFormatInfo; virtual; abstract;
  end;
  // soundcard output-devices information
  TAudioOutputDevice = class
    public
      Name: string; // soundcard name
  end;
  TAudioOutputDeviceList = array of TAudioOutputDevice;

type
  IGenericPlayback = Interface
  ['{63A5EBC3-3F4D-4F23-8DFB-B5165FCE33DD}']
      function GetName: String;

      function  Open(const Filename: string): boolean; // true if succeed
      procedure Close;

      procedure Play;
      procedure Pause;
      procedure Stop;

      procedure SetPosition(Time: real);
      function  GetPosition: real;

      property  Position : real READ GetPosition WRITE SetPosition;
  end;

  IVideoPlayback = Interface( IGenericPlayback )
  ['{3574C40C-28AE-4201-B3D1-3D1F0759B131}']
    procedure init();

    procedure GetFrame(Time: Extended); // WANT TO RENAME THESE TO BE MORE GENERIC
    procedure DrawGL(Screen: integer);  // WANT TO RENAME THESE TO BE MORE GENERIC

  end;

  IVideoVisualization = Interface( IVideoPlayback )
  ['{5AC17D60-B34D-478D-B632-EB00D4078017}']
  end;

  IAudioPlayback = Interface( IGenericPlayback )
  ['{E4AE0B40-3C21-4DC5-847C-20A87E0DFB96}']
      function InitializePlayback: boolean;
      function FinalizePlayback: boolean;
      
      function GetOutputDeviceList(): TAudioOutputDeviceList;
      procedure SetAppVolume(Volume: single);
      procedure SetVolume(Volume: single);
      procedure SetLoop(Enabled: boolean);
      procedure FadeIn(Time: real; TargetVolume: single);

      procedure Rewind;
      function  Finished: boolean;
      function  Length: real;

      // Sounds
      function OpenSound(const Filename: String): TAudioPlaybackStream;
      procedure PlaySound(stream: TAudioPlaybackStream);
      procedure StopSound(stream: TAudioPlaybackStream);

      // Equalizer
      procedure GetFFTData(var data: TFFTData);

      // Interface for Visualizer
      function GetPCMData(var data: TPCMData): Cardinal;
  end;

  IGenericDecoder = Interface
  ['{557B0E9A-604D-47E4-B826-13769F3E10B7}']
      function GetName(): String;
      function InitializeDecoder(): boolean;
      function FinalizeDecoder(): boolean;
      //function IsSupported(const Filename: string): boolean;
  end;

  (*
  IVideoDecoder = Interface( IGenericDecoder )
  ['{2F184B2B-FE69-44D5-9031-0A2462391DCA}']
      function Open(const Filename: string): TVideoDecodeStream;
  end;
  *)

  IAudioDecoder = Interface( IGenericDecoder )
  ['{AB47B1B6-2AA9-4410-BF8C-EC79561B5478}']
      function Open(const Filename: string): TAudioDecodeStream;
  end;

  IAudioInput = Interface
  ['{A5C8DA92-2A0C-4AB2-849B-2F7448C6003A}']
      function GetName: String;
      function InitializeRecord: boolean;
      function FinalizeRecord(): boolean;

      procedure CaptureStart;
      procedure CaptureStop;
  end;

type
  TSoundLibrary = class
    public
      Start:   TAudioPlaybackStream;
      Back:    TAudioPlaybackStream;
      Swoosh:  TAudioPlaybackStream;
      Change:  TAudioPlaybackStream;
      Option:  TAudioPlaybackStream;
      Click:   TAudioPlaybackStream;
      Drum:    TAudioPlaybackStream;
      Hihat:   TAudioPlaybackStream;
      Clap:    TAudioPlaybackStream;
      Shuffle: TAudioPlaybackStream;

      constructor Create();
      destructor Destroy(); override;

      procedure LoadSounds();
      procedure UnloadSounds();
  end;

var // TODO : JB --- THESE SHOULD NOT BE GLOBAL
  // czesci z nutami;
  Lines:   array of TLines;

  // LineState
  LineState:     TLineState;

  SoundLib: TSoundLibrary;


procedure InitializeSound;
procedure FinalizeSound;

function  Visualization(): IVideoPlayback;
function  VideoPlayback(): IVideoPlayback;
function  AudioPlayback(): IAudioPlayback;
function  AudioInput(): IAudioInput;
function  AudioDecoder(): IAudioDecoder;

function  AudioManager: TInterfaceList;


implementation

uses
  sysutils,
  UMain,
  UCommandLine,
  URecord,
  ULog;

var
  singleton_VideoPlayback : IVideoPlayback  = nil;
  singleton_Visualization : IVideoPlayback  = nil;
  singleton_AudioPlayback : IAudioPlayback  = nil;
  singleton_AudioInput    : IAudioInput     = nil;
  singleton_AudioDecoder  : IAudioDecoder   = nil;

  singleton_AudioManager  : TInterfaceList  = nil;


constructor TAudioFormatInfo.Create(Channels: byte; SampleRate: double; Format: TAudioSampleFormat);
begin
  inherited Create();
  Self.Channels := Channels;
  Self.SampleRate := SampleRate;
  Self.Format := Format;
  Self.FrameSize := AudioSampleSize[Format] * Channels;
end;

function AudioManager: TInterfaceList;
begin
  if singleton_AudioManager = nil then
    singleton_AudioManager := TInterfaceList.Create();
  
  Result := singleton_AudioManager;
end; //CompressionPluginManager


function  VideoPlayback(): IVideoPlayback;
begin
  result := singleton_VideoPlayback;
end;

function  Visualization(): IVideoPlayback;
begin
  result := singleton_Visualization;
end;

function AudioPlayback(): IAudioPlayback;
begin
  result := singleton_AudioPlayback;
end;

function AudioInput(): IAudioInput;
begin
  result := singleton_AudioInput;
end;

function AudioDecoder(): IAudioDecoder;
begin
  result := singleton_AudioDecoder;
end;

procedure FilterInterfaceList(const IID: TGUID; InList, OutList: TInterfaceList);
var
  i: integer;
  obj: IInterface;
begin
  if (not assigned(OutList)) then
    Exit;

  OutList.Clear;
  for i := 0 to InList.Count-1 do
  begin
    if assigned(InList[i]) then
    begin
      // add object to list if it implements the interface searched for
      if (InList[i].QueryInterface(IID, obj) = 0) then
        OutList.Add(obj);
    end;
  end;
end;

procedure AssignSingletonObjects();
var
  lTmpInterface : IInterface;
  iCount        : Integer;
begin
  lTmpInterface := nil;

  for iCount := 0 to AudioManager.Count - 1 do
  begin
    if assigned( AudioManager[iCount] ) then
    begin
      // if this interface is a Playback, then set it as the default used

      if ( AudioManager[iCount].QueryInterface( IAudioPlayback, lTmpInterface ) = 0 ) AND
         ( true ) then //not assigned( singleton_AudioPlayback ) ) then
      begin
        singleton_AudioPlayback := IAudioPlayback( lTmpInterface );
      end;

      // if this interface is a Input, then set it as the default used
      if ( AudioManager[iCount].QueryInterface( IAudioInput, lTmpInterface )    = 0 ) AND
         ( true ) then //not assigned( singleton_AudioInput ) ) then
      begin
        singleton_AudioInput := IAudioInput( lTmpInterface );
      end;

      // if this interface is a Decoder, then set it as the default used
      if ( AudioManager[iCount].QueryInterface( IAudioDecoder, lTmpInterface )    = 0 ) AND
         ( true ) then //not assigned( singleton_AudioDecoder ) ) then
      begin
        singleton_AudioDecoder := IAudioDecoder( lTmpInterface );
      end;

      // if this interface is a Input, then set it as the default used
      if ( AudioManager[iCount].QueryInterface( IVideoPlayback, lTmpInterface ) = 0 ) AND
         ( true ) then //not assigned( singleton_VideoPlayback ) ) then
      begin
        singleton_VideoPlayback := IVideoPlayback( lTmpInterface );
      end;

      if ( AudioManager[iCount].QueryInterface( IVideoVisualization, lTmpInterface ) = 0 ) AND
         ( true ) then //not assigned( singleton_Visualization ) ) then
      begin
        singleton_Visualization := IVideoPlayback( lTmpInterface );
      end;

    end;
  end;
end;

procedure InitializeSound;
begin
  singleton_AudioPlayback := nil;
  singleton_AudioInput    := nil;
  singleton_AudioDecoder  := nil;
  singleton_VideoPlayback := nil;
  singleton_Visualization := nil;

  AssignSingletonObjects();

  if VideoPlayback <> nil then
  begin
  end;

  if AudioDecoder <> nil then
  begin
    while not AudioDecoder.InitializeDecoder do
    begin
      Log.LogError('Initialize failed, Removing - '+ AudioDecoder.GetName);
      AudioManager.remove( AudioDecoder ); 
      singleton_AudioDecoder  := nil;
      AssignSingletonObjects();
    end;
  end;

  if AudioPlayback <> nil then
  begin
    while not AudioPlayback.InitializePlayback do
    begin
      Log.LogError('Initialize failed, Removing - '+ AudioPlayback.GetName);
      AudioManager.remove( AudioPlayback ); 
      singleton_AudioPlayback := nil;
      AssignSingletonObjects();
    end;
  end;

  if AudioInput <> nil then
  begin
    while not AudioInput.InitializeRecord do
    begin
      Log.LogError('Initialize failed, Removing - '+ AudioInput.GetName);
      AudioManager.remove( AudioInput ); 
      singleton_AudioInput    := nil;
      AssignSingletonObjects();
    end;    
  end;

  // Update input-device list with registered devices
  AudioInputProcessor.UpdateInputDeviceConfig();
  // Load in-game sounds
  SoundLib := TSoundLibrary.Create;

  if FindCmdLineSwitch( cMediaInterfaces ) then
  begin
    writeln( '' );
    writeln( '--------------------------------------------------------------' );
    writeln( '  In-use Media Interfaces                                     ' );
    writeln( '--------------------------------------------------------------' );
    writeln( 'Registered Audio Playback Interface : ' + AudioPlayback.GetName );
    writeln( 'Registered Audio Input    Interface : ' + AudioInput.GetName    );
    writeln( 'Registered Video Playback Interface : ' + VideoPlayback.GetName );
    writeln( 'Registered Visualization  Interface : ' + Visualization.GetName );
    writeln( '--------------------------------------------------------------' );
    writeln( '' );

    halt;
  end;
end;

procedure FinalizeSound;
var
  i: integer;
  AudioIntfList: TInterfaceList;
begin
  // stop, close and free sounds
  SoundLib.Free;

  // stop and close music stream
  if (AudioPlayback <> nil) then
    AudioPlayback.Close;

  // stop any active captures
  if (AudioInput <> nil) then
    AudioInput.CaptureStop;

  singleton_AudioPlayback := nil;
  singleton_AudioDecoder := nil;
  singleton_AudioInput := nil;

  // create temporary interface list
  AudioIntfList := TInterfaceList.Create();

  // finalize audio playback interfaces (should be done before the decoders) 
  FilterInterfaceList(IAudioPlayback, AudioManager, AudioIntfList);
  for i := 0 to AudioIntfList.Count-1 do
    IAudioPlayback(AudioIntfList[i]).FinalizePlayback();

  // finalize audio input interfaces
  FilterInterfaceList(IAudioInput, AudioManager, AudioIntfList);
  for i := 0 to AudioIntfList.Count-1 do
    IAudioInput(AudioIntfList[i]).FinalizeRecord();

  // finalize audio decoder interfaces
  FilterInterfaceList(IAudioDecoder, AudioManager, AudioIntfList);
  for i := 0 to AudioIntfList.Count-1 do
    IAudioDecoder(AudioIntfList[i]).FinalizeDecoder();

  AudioIntfList.Free;

  // free audio interfaces
  while (AudioManager.Count > 0) do
    AudioManager.Delete(0);
end;

{ TSoundLibrary }

constructor TSoundLibrary.Create();
begin
  inherited;
  LoadSounds();
end;

destructor TSoundLibrary.Destroy();
begin
  UnloadSounds();
  inherited;
end;

procedure TSoundLibrary.LoadSounds();
begin
  //Log.LogStatus('Loading Sounds', 'Music Initialize');
  //Log.BenchmarkStart(4);

  UnloadSounds();

  Start   := AudioPlayback.OpenSound(SoundPath + 'Common start.mp3');
  Back    := AudioPlayback.OpenSound(SoundPath + 'Common back.mp3');
  Swoosh  := AudioPlayback.OpenSound(SoundPath + 'menu swoosh.mp3');
  Change  := AudioPlayback.OpenSound(SoundPath + 'select music change music 50.mp3');
  Option  := AudioPlayback.OpenSound(SoundPath + 'option change col.mp3');
  Click   := AudioPlayback.OpenSound(SoundPath + 'rimshot022b.mp3');

  //Drum    := AudioPlayback.OpenSound(SoundPath + 'bassdrumhard076b.mp3');
  //Hihat   := AudioPlayback.OpenSound(SoundPath + 'hihatclosed068b.mp3');
  //Clap    := AudioPlayback.OpenSound(SoundPath + 'claps050b.mp3');

  //Shuffle := AudioPlayback.OpenSound(SoundPath + 'Shuffle.mp3');

  //Log.BenchmarkEnd(4);
  //Log.LogBenchmark('--> Loading Sounds', 4);
end;

procedure TSoundLibrary.UnloadSounds();
begin
  Start.Free;
  Back.Free;
  Swoosh.Free;
  Change.Free;
  Option.Free;
  Click.Free;

  //Drum.Free;
  //Hihat.Free;
  //Clap.Free;

  //Shuffle.Free;
end;

procedure TVoiceRemoval.Callback(Buffer: PChar; BufSize: integer);
var
  FrameIndex, FrameSize: integer;
  Value: integer;
  Sample: PPCMStereoSample;
begin
  FrameSize := 2 * SizeOf(SmallInt);
  for FrameIndex := 0 to (BufSize div FrameSize)-1 do
  begin
    Sample := PPCMStereoSample(Buffer);
    // channel difference
    Value := Sample[0] - Sample[1];
    // clip
    if (Value > High(SmallInt)) then
      Value := High(SmallInt)
    else if (Value < Low(SmallInt)) then
      Value := Low(SmallInt);
    // assign result
    Sample[0] := Value;
    Sample[1] := Value;
    // increase to next frame
    Inc(Buffer, FrameSize);
  end;
end;

constructor TLineState.Create();
begin
  Timer := TRelativeTimer.Create();
end;

procedure TLineState.Pause();
begin
  Timer.Pause();
end;

procedure TLineState.Resume();
begin
  Timer.Resume();
end;

procedure TLineState.SetCurrentTime(Time: real);
begin
  Timer.SetTime(Time);
end;

function TLineState.GetCurrentTime(): real;
begin
  Result := Timer.GetTime();
end;


initialization
begin
  singleton_AudioManager := TInterfaceList.Create();

end;

finalization
  singleton_AudioManager.clear;
  FreeAndNil( singleton_AudioManager );

end.
