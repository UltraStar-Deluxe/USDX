unit UAudioPlaybackBase;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMusic;

type
  TAudioPlaybackBase = class(TInterfacedObject, IAudioPlayback)
    protected
      OutputDeviceList: TAudioOutputDeviceList;
      MusicStream: TAudioPlaybackStream;
      // open sound or music stream (used by Open() and OpenSound())
      function OpenStream(const Filename: string): TAudioPlaybackStream; virtual; abstract;
      procedure ClearOutputDeviceList();
    public
      function GetName: String; virtual; abstract;

      function  Open(const Filename: string): boolean; // true if succeed
      procedure Close;

      procedure Play;
      procedure Pause;
      procedure Stop;
      procedure FadeIn(Time: real; TargetVolume: integer);

      procedure SetPosition(Time: real);
      function  GetPosition: real;

      function InitializePlayback: boolean; virtual; abstract;
      function FinalizePlayback: boolean; virtual;

      //      function SetOutputDevice(Device: TAudioOutputDevice): boolean;
      function GetOutputDeviceList(): TAudioOutputDeviceList;

      procedure SetAppVolume(Volume: integer); virtual; abstract;
      procedure SetVolume(Volume: integer);
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


implementation

uses
  SysUtils;

{ TAudioPlaybackBase }

function TAudioPlaybackBase.FinalizePlayback: boolean;
begin
  FreeAndNil(MusicStream);
  ClearOutputDeviceList();
end;

function TAudioPlaybackBase.Open(const Filename: string): boolean;
begin
  // free old MusicStream
  MusicStream.Free;

  MusicStream := OpenStream(Filename);
  if not assigned(MusicStream) then
  begin
    Result := false;
    Exit;
  end;

  //MusicStream.AddSoundEffect(TVoiceRemoval.Create());

  Result := true;
end;

procedure TAudioPlaybackBase.Close;
begin
  if assigned(MusicStream) then
    MusicStream.Close();
end;

procedure TAudioPlaybackBase.Play;
begin
  if assigned(MusicStream) then
    MusicStream.Play();
end;

procedure TAudioPlaybackBase.Pause;
begin
  if assigned(MusicStream) then
    MusicStream.Pause();
end;

procedure TAudioPlaybackBase.Stop;
begin
  if assigned(MusicStream) then
    MusicStream.Stop();
end;

function TAudioPlaybackBase.Length: real;
begin
  if assigned(MusicStream) then
    Result := MusicStream.Length
  else
    Result := 0;
end;

function TAudioPlaybackBase.GetPosition: real;
begin
  if assigned(MusicStream) then
    Result := MusicStream.Position
  else
    Result := 0;
end;

procedure TAudioPlaybackBase.SetPosition(Time: real);
begin
  if assigned(MusicStream) then
    MusicStream.Position := Time;
end;

procedure TAudioPlaybackBase.Rewind;
begin
  SetPosition(0);
end;

function TAudioPlaybackBase.Finished: boolean;
begin
  if assigned(MusicStream) then
    Result := (MusicStream.Status = ssStopped)
  else
    Result := true;
end;

procedure TAudioPlaybackBase.SetVolume(Volume: Integer);
begin
  if assigned(MusicStream) then
    MusicStream.Volume := Volume;
end;

procedure TAudioPlaybackBase.FadeIn(Time: real; TargetVolume: integer);
begin
  if assigned(MusicStream) then
    MusicStream.FadeIn(Time, TargetVolume);
end;

procedure TAudioPlaybackBase.SetLoop(Enabled: boolean);
begin
  if assigned(MusicStream) then
    MusicStream.Loop := Enabled;
end;

// Equalizer
procedure TAudioPlaybackBase.GetFFTData(var data: TFFTData);
begin
  if assigned(MusicStream) then
    MusicStream.GetFFTData(data);
end;

{*
 * Copies interleaved PCM SInt16 stereo samples into data.
 * Returns the number of frames
 *}
function TAudioPlaybackBase.GetPCMData(var data: TPCMData): Cardinal;
begin
  if assigned(MusicStream) then
    Result := MusicStream.GetPCMData(data)
  else
    Result := 0;
end;

function TAudioPlaybackBase.OpenSound(const Filename: string): TAudioPlaybackStream;
begin
  Result := OpenStream(Filename);
end;

procedure TAudioPlaybackBase.PlaySound(stream: TAudioPlaybackStream);
begin
  if assigned(stream) then
    stream.Play();
end;

procedure TAudioPlaybackBase.StopSound(stream: TAudioPlaybackStream);
begin
  if assigned(stream) then
    stream.Stop();
end;

procedure TAudioPlaybackBase.ClearOutputDeviceList();
var
  DeviceIndex: integer;
begin
  for DeviceIndex := 0 to High(OutputDeviceList) do
    OutputDeviceList[DeviceIndex].Free();
  SetLength(OutputDeviceList, 0);
end;

function TAudioPlaybackBase.GetOutputDeviceList(): TAudioOutputDeviceList;
begin
  Result := OutputDeviceList;
end;

end.
