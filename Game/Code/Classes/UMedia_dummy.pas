unit UMedia_dummy;
{< #############################################################################
#                    FFmpeg support for UltraStar deluxe                       #
#                                                                              #
#    Created by b1indy                                                         #
#    based on 'An ffmpeg and SDL Tutorial' (http://www.dranger.com/ffmpeg/)    #
#                                                                              #
# http://www.mail-archive.com/fpc-pascal@lists.freepascal.org/msg09949.html    #
# http://www.nabble.com/file/p11795857/mpegpas01.zip                           #
#                                                                              #
############################################################################## }

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

implementation

uses
     SysUtils,
     UMusic;


var
  singleton_dummy : IVideoPlayback;

type
    Tmedia_dummy = class( TInterfacedObject, IVideoPlayback, IVideoVisualization, IAudioPlayback, IAudioInput )
    private
    public
      constructor create();
      function  GetName: String;

      procedure init();

      function  Open( aFileName : string): boolean; // true if succeed
      procedure Close;

      procedure Play;
      procedure Pause;
      procedure Stop;

      procedure MoveTo(Time: real);
      function  getPosition: real;

      procedure GetFrame(Time: Extended);
      procedure DrawGL(Screen: integer);

      // IAudioInput
      procedure InitializeRecord;
      procedure CaptureStart;
      procedure CaptureStop;
      procedure CaptureCard(RecordI, PlayerLeft, PlayerRight: byte);
      procedure StopCard(Card: byte);
      function GetFFTData: TFFTData;
      function GetPCMData(var data: TPCMData): Cardinal;

      // IAudioPlayback
      procedure InitializePlayback;
      procedure SetVolume(Volume: integer);
      procedure SetMusicVolume(Volume: integer);
      procedure SetLoop(Enabled: boolean);
      procedure Rewind;

      function Finished: boolean;
      function Length: real;

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

      function LoadCustomSound(const Filename: String): Cardinal;
      procedure PlayCustomSound(const Index: Cardinal );

    end;



function  Tmedia_dummy.GetName: String;
begin
  result := 'dummy';
end;


procedure Tmedia_dummy.GetFrame(Time: Extended);
begin
end;

procedure Tmedia_dummy.DrawGL(Screen: integer);
begin
end;

constructor Tmedia_dummy.create();
begin
end;

procedure Tmedia_dummy.init();
begin
end;


function Tmedia_dummy.Open( aFileName : string): boolean; // true if succeed
begin
  result := false;
end;

procedure Tmedia_dummy.Close;
begin
end;

procedure Tmedia_dummy.Play;
begin
end;

procedure Tmedia_dummy.Pause;
begin
end;

procedure Tmedia_dummy.Stop;
begin
end;

procedure Tmedia_dummy.MoveTo(Time: real);
begin
end;

function  Tmedia_dummy.getPosition: real;
begin
  result := 0;
end;

// IAudioInput
procedure Tmedia_dummy.InitializeRecord;
begin
end;

procedure Tmedia_dummy.CaptureStart;
begin
end;

procedure Tmedia_dummy.CaptureStop;
begin
end;

procedure Tmedia_dummy.CaptureCard(RecordI, PlayerLeft, PlayerRight: byte);
begin
end;

procedure Tmedia_dummy.StopCard(Card: byte);
begin
end;

function  Tmedia_dummy.GetFFTData: TFFTData;
begin
end;

function  Tmedia_dummy.GetPCMData(var data: TPCMData): Cardinal;
begin
  result := 0;
end;

// IAudioPlayback
procedure Tmedia_dummy.InitializePlayback;
begin
end;

procedure Tmedia_dummy.SetVolume(Volume: integer);
begin
end;

procedure Tmedia_dummy.SetMusicVolume(Volume: integer);
begin
end;

procedure Tmedia_dummy.SetLoop(Enabled: boolean);
begin
end;

procedure Tmedia_dummy.Rewind;
begin
end;

function Tmedia_dummy.Finished: boolean;
begin
  result := false;
end;

function Tmedia_dummy.Length: real;
begin
  Result := 60;
end;

procedure Tmedia_dummy.PlayStart;
begin
end;

procedure Tmedia_dummy.PlayBack;
begin
end;

procedure Tmedia_dummy.PlaySwoosh;
begin
end;

procedure Tmedia_dummy.PlayChange;
begin
end;

procedure Tmedia_dummy.PlayOption;
begin
end;

procedure Tmedia_dummy.PlayClick;
begin
end;

procedure Tmedia_dummy.PlayDrum;
begin
end;

procedure Tmedia_dummy.PlayHihat;
begin
end;

procedure Tmedia_dummy.PlayClap;
begin
end;

procedure Tmedia_dummy.PlayShuffle;
begin
end;

procedure Tmedia_dummy.StopShuffle;
begin
end;

function Tmedia_dummy.LoadSoundFromFile(var hStream: hStream; Name: string): boolean;
begin
  result := false;
end;

function Tmedia_dummy.LoadCustomSound(const Filename: String): Cardinal;
begin
 result := 0;
end;

procedure Tmedia_dummy.PlayCustomSound(const Index: Cardinal );
begin
end;



initialization
  singleton_dummy := Tmedia_dummy.create();

  writeln( 'UMedia_dummy - Register' );
  AudioManager.add( singleton_dummy );

finalization
  writeln( 'UMedia_dummy - UnRegister' );
  AudioManager.Remove( singleton_dummy );

end.
