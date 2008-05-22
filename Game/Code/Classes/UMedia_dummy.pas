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
     math,
     UMusic;

var
  singleton_dummy : IVideoPlayback;

type
    TMedia_dummy = class( TInterfacedObject, IVideoPlayback, IVideoVisualization, IAudioPlayback, IAudioInput )
    private
      DummyOutputDeviceList: TAudioOutputDeviceList;
    public
      constructor create();
      function  GetName: string;

      procedure init();

      function  Open(const aFileName : string): boolean; // true if succeed
      procedure Close;

      procedure Play;
      procedure Pause;
      procedure Stop;

      procedure SetPosition(Time: real);
      function  GetPosition: real;

      procedure GetFrame(Time: Extended);
      procedure DrawGL(Screen: integer);

      // IAudioInput
      function InitializeRecord: boolean;
      function FinalizeRecord: boolean;
      procedure CaptureStart;
      procedure CaptureStop;
      procedure GetFFTData(var data: TFFTData);
      function GetPCMData(var data: TPCMData): Cardinal;

      // IAudioPlayback
      function InitializePlayback: boolean;
      function FinalizePlayback: boolean;

      function GetOutputDeviceList(): TAudioOutputDeviceList;
      procedure FadeIn(Time: real; TargetVolume: single);
      procedure SetAppVolume(Volume: single);
      procedure SetVolume(Volume: single);
      procedure SetLoop(Enabled: boolean);
      procedure Rewind;

      function Finished: boolean;
      function Length: real;

      function OpenSound(const Filename: string): TAudioPlaybackStream;
      procedure PlaySound(stream: TAudioPlaybackStream);
      procedure StopSound(stream: TAudioPlaybackStream);
    end;

function  Tmedia_dummy.GetName: string;
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
  inherited;
end;

procedure Tmedia_dummy.init();
begin
end;

function Tmedia_dummy.Open(const aFileName : string): boolean; // true if succeed
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

procedure Tmedia_dummy.SetPosition(Time: real);
begin
end;

function  Tmedia_dummy.GetPosition: real;
begin
  result := 0;
end;

// IAudioInput
function Tmedia_dummy.InitializeRecord: boolean;
begin
  result := true;
end;

function Tmedia_dummy.FinalizeRecord: boolean;
begin
  result := true;
end;

procedure Tmedia_dummy.CaptureStart;
begin
end;

procedure Tmedia_dummy.CaptureStop;
begin
end;

procedure Tmedia_dummy.GetFFTData(var data: TFFTData);
begin
end;

function  Tmedia_dummy.GetPCMData(var data: TPCMData): Cardinal;
begin
  result := 0;
end;

// IAudioPlayback
function Tmedia_dummy.InitializePlayback: boolean;
begin
  SetLength(DummyOutputDeviceList, 1);
  DummyOutputDeviceList[0] := TAudioOutputDevice.Create();
  DummyOutputDeviceList[0].Name := '[Dummy Device]';
  result := true;
end;

function Tmedia_dummy.FinalizePlayback: boolean;
begin
  result := true;
end;

function Tmedia_dummy.GetOutputDeviceList(): TAudioOutputDeviceList;
begin
  Result := DummyOutputDeviceList;
end;

procedure Tmedia_dummy.SetAppVolume(Volume: single);
begin
end;

procedure Tmedia_dummy.SetVolume(Volume: single);
begin
end;

procedure Tmedia_dummy.SetLoop(Enabled: boolean);
begin
end;

procedure Tmedia_dummy.FadeIn(Time: real; TargetVolume: single);
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

function Tmedia_dummy.OpenSound(const Filename: string): TAudioPlaybackStream;
begin
 result := nil;
end;

procedure Tmedia_dummy.PlaySound(stream: TAudioPlaybackStream);
begin
end;

procedure Tmedia_dummy.StopSound(stream: TAudioPlaybackStream);
begin
end;

initialization
  singleton_dummy := Tmedia_dummy.create();
  AudioManager.add( singleton_dummy );

finalization
  AudioManager.Remove( singleton_dummy );

end.
