unit UMusic;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes;

type
  TNoteType = (ntFreestyle, ntNormal, ntGolden);

  //http://paste.ubuntu-nl.org/51892/

  TMuzyka = record           // (TODO: rename to TMusic/TMelody?)
    Path:   string;
    Start:  integer;        // start of song in ms
    IlNut:  integer;        // (TODO: Il = tone, Nut(a) = Note)
    DlugoscNut:   integer;  // (TODO: Dlugosc = length, Nut(a) = Note)
  end;

  PLine = ^TLine;
  TLine = record              // (TODO: rename to TSentence?)
    Start:      integer;
    StartNote:  integer;
    Lyric:      string;
    LyricWidth: real;
    Koniec:     integer;      // (TODO: rename to End_/Ending?) 
    BaseNote:   integer;
    HighNut:    integer;      // (TODO: rename to HighNote)
    IlNut:      integer;      // (TODO: Il = tone, Nut(a) = Note)
    TotalNotes: integer;
    Nuta:     array of record // (TODO: rename to Note)
      Color:      integer;
      Start:      integer;
      Dlugosc:    integer;    // (TODO: rename to Length)
      Ton:        integer;    // full range tone (TODO: rename to Tone)
      TonGamy:    integer;    // tone unified to one octave (TODO: rename to something meaningful, ToneGamus)
      Tekst:      string;     // (TODO: rename to Text)
      FreeStyle:  boolean;
      Wartosc:    integer;    // normal-note: 1, golden-note: 2 (TODO: wartosc=value, rename to Type_ or Kind?)
    end;
  end;
  ALine = array of TLine; // (TODO: rename to TLineArray)

  // (TCzesci = TSentences)
  TCzesci = record
    Akt:        integer;        // for drawing of current line (Akt = Current)
    High:       integer;
    Ilosc:      integer;        // (TODO: Ilosc = Number/Count)
    Resolution: integer;
    NotesGAP:   integer;
    Wartosc:    integer;        // TODO: rename (wartosc=value)
    Czesc:      ALine;          // TODO: rename to Sentence or Line
  end;

  // (TODO: rename TCzas to something like T(Line/Sentence)Time/TLinePosition/TLineState)
  // (Czas = time)
  TCzas = record              // all that concerns the current frames
    OldBeat:      integer;    // previous discovered beat 
    AktBeat:      integer;    // current beat (TODO: rename)
    MidBeat:      real;       // like AktBeat

    // now we use this for super synchronization!
    // only used when analyzing voice
    OldBeatD:     integer;    // previous discovered beat
    AktBeatD:     integer;    // current beat (TODO: rename)
    MidBeatD:     real;       // like AktBeatD
    FracBeatD:    real;       // fractional part of MidBeatD

    // we use this for audible clicks
    OldBeatC:     integer;    // previous discovered beat
    AktBeatC:     integer;    // current beat (TODO: rename)
    MidBeatC:     real;       // like AktBeatC
    FracBeatC:    real;       // fractional part of MidBeatC


    OldCzesc:     integer;    // previous displayed sentence (Czesc = part (here: sentence/line))

    Teraz:        real;       // (TODO: Teraz = current time)
    Razem:        real;       // (TODO: Razem = total time)
  end;

  
type
  TFFTData  = array[0..255] of Single;

  TPCMStereoSample = array[0..1] of Smallint;
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
      SampleRate  : integer;
      Format      : TAudioSampleFormat;
      FrameSize   : integer; // calculated on construction

      constructor Create(Channels: byte; SampleRate: integer; Format: TAudioSampleFormat);
  end;

type
  TAudioProcessingStream = class
    public
      procedure Close();                    virtual; abstract;
  end;

  TAudioPlaybackStream = class(TAudioProcessingStream)
    protected
      function GetLoop(): boolean;          virtual; abstract;
      procedure SetLoop(Enabled: boolean);  virtual; abstract;
      function GetLength(): real;           virtual; abstract;
      function GetStatus(): TStreamStatus;  virtual; abstract;
      function GetVolume(): integer;        virtual; abstract;
      procedure SetVolume(volume: integer); virtual; abstract;
    public
      procedure Play();                     virtual; abstract;
      procedure Pause();                    virtual; abstract;
      procedure Stop();                     virtual; abstract;

      property Loop: boolean READ GetLoop WRITE SetLoop;
      property Length: real READ GetLength;
      property Status: TStreamStatus READ GetStatus;
      property Volume: integer READ GetVolume WRITE SetVolume;
  end;

  (*
  TAudioMixerStream = class(TAudioProcessingStream)
    procedure AddStream(stream: TAudioProcessingStream);
    procedure RemoveStream(stream: TAudioProcessingStream);
    procedure SetMasterVolume(volume: cardinal);
    function GetMasterVolume(): cardinal;
    procedure SetStreamVolume(stream: TAudioProcessingStream; volume: cardinal);
    function GetStreamVolume(stream: TAudioProcessingStream): cardinal;
  end;
  *)

  TAudioDecodeStream = class(TAudioProcessingStream)
    protected
      function GetLength(): real;           virtual; abstract;
      function GetPosition(): real;         virtual; abstract;
      procedure SetPosition(Time: real);    virtual; abstract;
      function IsEOF(): boolean;            virtual; abstract;
    public
      function ReadData(Buffer: PChar; BufSize: integer): integer; virtual; abstract;
      function GetAudioFormatInfo(): TAudioFormatInfo; virtual; abstract;

      property Length: real READ GetLength;
      property Position: real READ GetPosition WRITE SetPosition;
      property EOF: boolean READ IsEOF;
  end;

type
  IGenericPlayback = Interface
  ['{63A5EBC3-3F4D-4F23-8DFB-B5165FCE33DD}']
      function  GetName: String;

      function  Open(Filename: string): boolean; // true if succeed
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
      procedure SetVolume(Volume: integer);
      procedure SetMusicVolume(Volume: integer);
      procedure SetLoop(Enabled: boolean);

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
      function InitializeDecoder(): boolean;
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
      function  GetName: String;
      function InitializeRecord: boolean;

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
  end;

var // TODO : JB --- THESE SHOULD NOT BE GLOBAL
  // music
  Muzyka:   TMuzyka; // TODO: rename

  // czesci z nutami;
  Czesci:   array of TCzesci;  // TODO: rename to Sentences/Lines

  // czas
  Czas:     TCzas;             // TODO: rename

  SoundLib: TSoundLibrary;


procedure InitializeSound;

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
  UCommandLine;
//  uLog;

var
  singleton_VideoPlayback : IVideoPlayback  = nil;
  singleton_Visualization : IVideoPlayback  = nil;
  singleton_AudioPlayback : IAudioPlayback  = nil;
  singleton_AudioInput    : IAudioInput     = nil;
  singleton_AudioDecoder  : IAudioDecoder   = nil;

  singleton_AudioManager  : TInterfaceList  = nil;


constructor TAudioFormatInfo.Create(Channels: byte; SampleRate: integer; Format: TAudioSampleFormat);
begin
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
      //writeln('Initialize failed, Removing - '+ AudioDecoder.GetName  );
      AudioManager.remove( AudioDecoder ); 
      singleton_AudioDecoder  := nil;
      AssignSingletonObjects();
    end;
  end;

  if AudioPlayback <> nil then
  begin
    while not AudioPlayback.InitializePlayback do
    begin
      writeln('Initialize failed, Removing - '+ AudioPlayback.GetName  );
      AudioManager.remove( AudioPlayback ); 
      singleton_AudioPlayback := nil;
      AssignSingletonObjects();
    end;
  end;

  if AudioInput <> nil then
  begin
    while not AudioInput.InitializeRecord do
    begin
      writeln('Initialize failed, Removing - '+ AudioInput.GetName  );
      AudioManager.remove( AudioInput ); 
      singleton_AudioInput    := nil;
      AssignSingletonObjects();
    end;    
  end;

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

constructor TSoundLibrary.Create();
begin
  //Log.LogStatus('Loading Sounds', 'Music Initialize');

  //Log.BenchmarkStart(4);

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

destructor TSoundLibrary.Destroy();
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


initialization
begin
  singleton_AudioManager := TInterfaceList.Create();
  
end;

finalization
  singleton_AudioManager.clear;
  FreeAndNil( singleton_AudioManager );

end.
