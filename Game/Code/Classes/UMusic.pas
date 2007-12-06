unit UMusic;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses Classes ;

type
  TMuzyka = record
    Path:   string;
    Start:  integer;        // start of song in ms
    IlNut:  integer;
    DlugoscNut:   integer;
  end;

  PLine = ^TLine;
  TLine = record
    Start:    integer;
    StartNote:  integer;
    Lyric:      string;
    LyricWidth: real;
    Koniec:   integer;
    BaseNote: integer;
    HighNut:  integer;
    IlNut:    integer;
    TotalNotes: integer;
    Nuta:     array of record
      Color:      integer;
      Start:      integer;
      Dlugosc:    integer;
      Ton:        integer;
      TonGamy:    integer;
      Tekst:      string;
      FreeStyle:  boolean;
      Wartosc:    integer;    // zwykla nuta x1, zlota nuta x2
    end;
  end;
  ALine = array of TLine;

  TCzesci = record
    Akt:      integer;      // aktualna czesc utworu do rysowania
    High:     integer;
    Ilosc:    integer;
    Resolution: integer;
    NotesGAP: integer;
    Wartosc:  integer;
    Czesc:    ALine;
  end;

  TCzas = record              // wszystko, co dotyczy aktualnej klatki
    OldBeat:      integer;    // poprzednio wykryty beat w utworze
    AktBeat:      integer;    // aktualny beat w utworze
    MidBeat:      real;       // dokladny AktBeat

    // now we use this for super synchronization!
    // only used when analyzing voice
    OldBeatD:     integer;    // poprzednio wykryty beat w utworze
    AktBeatD:     integer;    // aktualny beat w utworze
    MidBeatD:     real;       // dokladny AktBeatD
    FracBeatD:    real;       // fractional part of MidBeatD

    // we use this for audiable clicks
    OldBeatC:     integer;    // poprzednio wykryty beat w utworze
    AktBeatC:     integer;    // aktualny beat w utworze
    MidBeatC:     real;       // dokladny AktBeatC
    FracBeatC:    real;       // fractional part of MidBeatC


    OldCzesc:     integer;    // poprzednio wyswietlana czesc
                              // akt jest w czesci.akt

    Teraz:        real;       // aktualny czas w utworze
    Razem:        real;       // caly czas utworu
  end;

  TSoundCard = record
    Name:     string;
    Source:   array of string;
  end;

  TFFTData  = array[0..256] of Single;

  TPCMStereoSample = array[0..1] of Smallint;
  TPCMData  = array[0..511] of TPCMStereoSample;

  hStream = Cardinal;

  TCustomSoundEntry = record
    Filename : String;
    Handle   : hStream;
  end;

type
  IGenericPlayback = Interface
  ['{63A5EBC3-3F4D-4F23-8DFB-B5165FCE33DD}']
      function  GetName: String;

      function  Open(Name: string): boolean; // true if succeed
      procedure Close;

      procedure Play;
      procedure Pause;
      procedure Stop;

      procedure MoveTo(Time: real);
      function  getPosition: real;

      property position : real READ getPosition WRITE MoveTo;
  end;

  IVideoPlayback = Interface( IGenericPlayback )
  ['{3574C40C-28AE-4201-B3D1-3D1F0759B131}']
(*
    procedure FFmpegOpenFile(FileName: pAnsiChar);
    procedure FFmpegClose;

    procedure FFmpegGetFrame(Time: Extended);
    procedure FFmpegDrawGL(Screen: integer);
    procedure FFmpegTogglePause;
    procedure FFmpegSkip(Time: Single);
*)
    procedure init();
    
    procedure GetFrame(Time: Extended); // WANT TO RENAME THESE TO BE MORE GENERIC
    procedure DrawGL(Screen: integer);  // WANT TO RENAME THESE TO BE MORE GENERIC

  end;

  IVideoVisualization = Interface( IVideoPlayback )
  ['{5AC17D60-B34D-478D-B632-EB00D4078017}']
  end;

  IAudioPlayback = Interface( IGenericPlayback )
  ['{E4AE0B40-3C21-4DC5-847C-20A87E0DFB96}']
      procedure InitializePlayback;
      procedure SetVolume(Volume: integer);
      procedure SetMusicVolume(Volume: integer);
      procedure SetLoop(Enabled: boolean);
//      function Open(Name: string): boolean; // true if succeed
      procedure Rewind;
//      procedure MoveTo(Time: real);
//      procedure Play;
//      procedure Pause;
//      procedure Stop;
//      procedure Close;
      function Finished: boolean;
      function Length: real;
//      function getPosition: real;

      procedure PlayStart;
      procedure PlayBack;
      procedure PlaySwoosh;
      procedure PlayChange;
      procedure PlayOption;
      procedure PlayClick;
      procedure PlayDrum;
      procedure PlayHihat;
      procedure PlayClap;
      procedure PlayShuffle;
      procedure StopShuffle;

      function LoadSoundFromFile(var hStream: hStream; Name: string): boolean;

      //Custom Sounds
      function LoadCustomSound(const Filename: String): Cardinal;
      procedure PlayCustomSound(const Index: Cardinal );

      //Equalizer
      function GetFFTData: TFFTData;

      // Interface for Visualizer
      function GetPCMData(var data: TPCMData): Cardinal;
  end;

  IAudioInput = Interface
  ['{A5C8DA92-2A0C-4AB2-849B-2F7448C6003A}']
      function  GetName: String;
      procedure InitializeRecord;

      procedure CaptureStart;
      procedure CaptureStop;
  end;


var // TODO : JB --- THESE SHOULD NOT BE GLOBAL
  // muzyka
  Muzyka:   TMuzyka;

  // czesci z nutami;
  Czesci:   array of TCzesci;

  // czas
  Czas:     TCzas;
  

procedure InitializeSound;

function  Visualization(): IVideoPlayback;
function  VideoPlayback(): IVideoPlayback;
function  AudioPlayback(): IAudioPlayback;
function  AudioInput(): IAudioInput;

function  AudioManager: TInterfaceList;


implementation

uses
  sysutils;
//  uLog;

var
  singleton_VideoPlayback : IVideoPlayback  = nil;
  singleton_Visualization : IVideoPlayback  = nil;
  singleton_AudioPlayback : IAudioPlayback  = nil;
  singleton_AudioInput    : IAudioInput     = nil;

  singleton_AudioManager  : TInterfaceList  = nil;


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

procedure InitializeSound;
var
  lTmpInterface : IInterface;
  iCount        : Integer;
begin
  lTmpInterface := nil;

  singleton_AudioPlayback := nil;
  singleton_AudioInput    := nil;
  singleton_VideoPlayback := nil;
  singleton_Visualization := nil;

  writeln( 'InitializeSound , Enumerate Registered Audio Interfaces' );
  for iCount := 0 to AudioManager.Count - 1 do
  begin
    if assigned( AudioManager[iCount] ) then
    begin
      // if this interface is a Playback, then set it as the default used

      if ( AudioManager[iCount].QueryInterface( IAudioPlayback, lTmpInterface ) = 0 ) AND
         ( true ) then
//         ( not assigned( singleton_AudioPlayback )                                  ) then
      begin
        singleton_AudioPlayback := IAudioPlayback( lTmpInterface );
      end;

      // if this interface is a Input, then set it as the default used
      if ( AudioManager[iCount].QueryInterface( IAudioInput, lTmpInterface )    = 0 ) AND
         ( true ) then
//         ( not assigned( singleton_AudioInput )                                     ) then
      begin
        singleton_AudioInput := IAudioInput( lTmpInterface );
        Writeln(singleton_AudioInput.GetName);
      end;

      // if this interface is a Input, then set it as the default used
      if ( AudioManager[iCount].QueryInterface( IVideoPlayback, lTmpInterface ) = 0 ) AND
         //( AudioManager[iCount].QueryInterface( IVideoVisualization, lTmpInterface ) <> 0 ) AND
         ( true ) then
//         ( not assigned( singleton_VideoPlayback )                                  ) then
      begin
        singleton_VideoPlayback := IVideoPlayback( lTmpInterface );
      end;

      if ( AudioManager[iCount].QueryInterface( IVideoVisualization, lTmpInterface ) = 0 ) AND
         ( true ) then
      begin
        singleton_Visualization := IVideoPlayback( lTmpInterface );
      end;

    end;
  end;


  if VideoPlayback <> nil then
  begin
    writeln( 'Registered Video Playback Interface : ' + VideoPlayback.GetName );
  end;

  if AudioPlayback <> nil then
  begin
    writeln( 'Registered Audio Playback Interface : ' + AudioPlayback.GetName );
  //  Log.LogStatus('Initializing Playback ('+AudioPlayback.GetName+')', 'InitializeSound');
    AudioPlayback.InitializePlayback;
  end;

  if AudioInput <> nil then
  begin
    writeln( 'Registered Audio Input Interface    : ' + AudioInput.GetName );

//    Log.LogStatus('Initializing Record ('+AudioPlayback.GetName+')', 'InitializeSound');
    AudioInput.InitializeRecord;
  end;

  writeln( 'InitializeSound DONE' );

end;

initialization
begin
  writeln('Init AudioManager');
  singleton_AudioManager := TInterfaceList.Create();
end;

finalization
  writeln('Finalize AudioManager');
  singleton_AudioManager.clear;
  FreeAndNil( singleton_AudioManager );

end.
