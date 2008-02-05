unit UAudioPlayback_Bass;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}


uses
  Classes,
  SysUtils,
  UMusic;

implementation

uses
  UIni,
  UMain,
  ULog,
  UAudioCore_Bass,
  bass;

type
  TBassPlaybackStream = class(TAudioPlaybackStream)
    private
      Handle: HSTREAM;
      Loop:   boolean;
    public
      constructor Create(); overload;
      constructor Create(stream: HSTREAM); overload;

      procedure Reset();

      procedure Play();                     override;
      procedure Pause();                    override;
      procedure Stop();                     override;
      procedure Close();                    override;
      function GetLoop(): boolean;          override;
      procedure SetLoop(Enabled: boolean);  override;
      function GetLength(): real;           override;
      function GetStatus(): TStreamStatus;  override;
      function GetVolume(): integer;        override;
      procedure SetVolume(volume: integer); override;

      function GetPosition: real;
      procedure SetPosition(Time: real);

      function IsLoaded(): boolean;
  end;

type
  TAudioPlayback_Bass = class( TInterfacedObject, IAudioPlayback)
    private
      MusicStream: TBassPlaybackStream;

      function Load(Filename: string): TBassPlaybackStream;
    public
      function  GetName: String;

      {IAudioOutput interface}

      function InitializePlayback(): boolean;
      procedure SetVolume(Volume: integer);
      procedure SetMusicVolume(Volume: integer);
      procedure SetLoop(Enabled: boolean);

      function Open(Filename: string): boolean; // true if succeed

      procedure Rewind;
      procedure Play;
      procedure Pause; //Pause Mod
      procedure Stop;
      procedure Close;
      function Finished: boolean;
      function Length: real;
      function GetPosition: real;
      procedure SetPosition(Time: real);

      //Equalizer
      procedure GetFFTData(var data: TFFTData);

      // Interface for Visualizer
      function GetPCMData(var data: TPCMData): Cardinal;

      // Sounds
      function OpenSound(const Filename: String): TAudioPlaybackStream;
      procedure PlaySound(stream: TAudioPlaybackStream);
      procedure StopSound(stream: TAudioPlaybackStream);
  end;

var
  singleton_AudioPlaybackBass : IAudioPlayback;


constructor TBassPlaybackStream.Create();
begin
  inherited;
  Reset();
end;

constructor TBassPlaybackStream.Create(stream: HSTREAM);
begin
  Create();
  Handle := stream;
end;

procedure TBassPlaybackStream.Reset();
begin
  Loop := false;
  if (Handle <> 0) then
    Bass_StreamFree(Handle);
  Handle := 0;
end;

procedure TBassPlaybackStream.Play();
begin
  BASS_ChannelPlay(Handle, Loop);
end;

procedure TBassPlaybackStream.Pause();
begin
  BASS_ChannelPause(Handle);
end;

procedure TBassPlaybackStream.Stop();
begin
  BASS_ChannelStop(Handle);
end;

procedure TBassPlaybackStream.Close();
begin
  Reset();
end;

function TBassPlaybackStream.GetVolume(): integer;
begin
  Result := 0;
  BASS_ChannelSetAttributes(Handle, PInteger(nil)^, Result, PInteger(nil)^);
end;

procedure TBassPlaybackStream.SetVolume(volume: integer);
begin
  // clamp volume
  if volume < 0 then
    volume := 0;
  if volume > 100 then
    volume := 100;
  // set volume
  BASS_ChannelSetAttributes(Handle, -1, volume, -101);
end;

function TBassPlaybackStream.GetPosition: real;
var
  bytes:    integer;
begin
  bytes  := BASS_ChannelGetPosition(Handle);
  Result := BASS_ChannelBytes2Seconds(Handle, bytes);
end;

procedure TBassPlaybackStream.SetPosition(Time: real);
var
  bytes:    integer;
begin
  bytes := BASS_ChannelSeconds2Bytes(Handle, Time);
  BASS_ChannelSetPosition(Handle, bytes);
end;

function TBassPlaybackStream.GetLoop(): boolean;
begin
  result := Loop;
end;

procedure TBassPlaybackStream.SetLoop(Enabled: boolean);
begin
  Loop := Enabled;
end;

function TBassPlaybackStream.GetLength(): real;
var
  bytes:    integer;
begin
  bytes  := BASS_ChannelGetLength(Handle);
  Result := BASS_ChannelBytes2Seconds(Handle, bytes);
end;

function TBassPlaybackStream.GetStatus(): TStreamStatus;
var
  state: DWORD;
begin
  state := BASS_ChannelIsActive(Handle);
  case state of
    BASS_ACTIVE_PLAYING:
      result := ssPlaying;
    BASS_ACTIVE_PAUSED:
      result := ssPaused;
    BASS_ACTIVE_STALLED:
      result := ssBlocked;
    BASS_ACTIVE_STOPPED:
      result := ssStopped;
    else
      result := ssUnknown;
  end;
end;

function TBassPlaybackStream.IsLoaded(): boolean;
begin
  Result := (Handle <> 0);
end;


function  TAudioPlayback_Bass.GetName: String;
begin
  result := 'BASS_Playback';
end;

function TAudioPlayback_Bass.InitializePlayback(): boolean;
var
  Pet:  integer;
  S:    integer;
begin
  result := false;

  //Log.BenchmarkStart(4);
  //Log.LogStatus('Initializing Playback Subsystem', 'Music Initialize');

  if not BASS_Init(1, 44100, 0, 0, nil) then
  begin
    Log.LogError('Could not initialize BASS', 'Error');
    Exit;
  end;

  //Log.BenchmarkEnd(4); Log.LogBenchmark('--> Bass Init', 4);

  // config playing buffer
  //BASS_SetConfig(BASS_CONFIG_UPDATEPERIOD, 10);
  //BASS_SetConfig(BASS_CONFIG_BUFFER, 100);

  result := true;
end;

function TAudioPlayback_Bass.Load(Filename: string): TBassPlaybackStream;
var
  L: Integer;
  stream: HSTREAM;
begin
  Result := nil;

  //Log.LogStatus('Loading Sound: "' + Filename + '"', 'LoadSoundFromFile');
  stream := BASS_StreamCreateFile(False, pchar(Filename), 0, 0, 0);
  if (stream = 0) then
  begin
    Log.LogError('Failed to open "' + Filename + '", ' +
                 TAudioCore_Bass.ErrorGetString(BASS_ErrorGetCode()), 'TAudioPlayback_Bass.Load');
    Exit;
  end;

  Result := TBassPlaybackStream.Create(stream);
end;

procedure TAudioPlayback_Bass.SetVolume(Volume: integer);
begin
  //Old Sets Wave Volume
  //BASS_SetVolume(Volume);
  //New: Sets Volume only for this Application
  BASS_SetConfig(BASS_CONFIG_GVOL_SAMPLE, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, Volume);
end;

procedure TAudioPlayback_Bass.SetMusicVolume(Volume: Integer);
begin
  if assigned(MusicStream) then
    MusicStream.SetVolume(Volume);
end;

procedure TAudioPlayback_Bass.SetLoop(Enabled: boolean);
begin
  if assigned(MusicStream) then
    MusicStream.Loop := Enabled;
end;

function TAudioPlayback_Bass.Open(Filename: string): boolean;
var
  stream: HSTREAM;
begin
  Result := false;

  // free old MusicStream
  if assigned(MusicStream) then
    MusicStream.Free;

  MusicStream := Load(Filename);
  if not assigned(MusicStream) then
    Exit;

  //Set Max Volume
  SetMusicVolume(100);

  Result := true;
end;

procedure TAudioPlayback_Bass.Rewind;
begin
  SetPosition(0);
end;

procedure TAudioPlayback_Bass.Play;
begin
  if assigned(MusicStream) then
    MusicStream.Play();
end;

procedure TAudioPlayback_Bass.Pause;
begin
  if assigned(MusicStream) then
    MusicStream.Pause();
end;

procedure TAudioPlayback_Bass.Stop;
begin
  if assigned(MusicStream) then
    MusicStream.Stop();
end;

procedure TAudioPlayback_Bass.Close;
begin
  if assigned(MusicStream) then
    MusicStream.Close();
end;

function TAudioPlayback_Bass.Length: real;
var
  bytes:    integer;
begin
  if assigned(MusicStream) then
    Result := MusicStream.GetLength()
  else
    Result := -1;
end;

function TAudioPlayback_Bass.GetPosition: real;
begin
  if assigned(MusicStream) then
    Result := MusicStream.GetPosition()
  else
    Result := -1;
end;

procedure TAudioPlayback_Bass.SetPosition(Time: real);
begin
  if assigned(MusicStream) then
    MusicStream.SetPosition(Time);
end;

function TAudioPlayback_Bass.Finished: boolean;
begin
  if assigned(MusicStream) then
    Result := (MusicStream.GetStatus() = ssStopped)
  else
    Result := true;
end;

//Equalizer
procedure TAudioPlayback_Bass.GetFFTData(var data: TFFTData);
begin
  //Get Channel Data Mono and 256 Values
  BASS_ChannelGetData(MusicStream.Handle, @data, BASS_DATA_FFT512);
end;

{*
 * Copies interleaved PCM 16bit uint (maybe fake) stereo samples into data.
 * Returns the number of frames (= stereo/mono sample)
 *}
function TAudioPlayback_Bass.GetPCMData(var data: TPCMData): Cardinal;
var
  info: BASS_CHANNELINFO;
  nBytes: DWORD;
begin
  //Get Channel Data Mono and 256 Values
  BASS_ChannelGetInfo(MusicStream.Handle, info);
  FillChar(data, sizeof(TPCMData), 0);
  
  if (info.chans = 1) then
  begin
    // mono file -> add stereo channel
    nBytes := 0;//BASS_ChannelGetData(Bass, @data[0], samples*sizeof(Smallint));
    // interleave data
    //CopyMemory(@data[1], @data[0], samples*sizeof(Smallint));
    result := 0;
  end
  else
  begin
    // stereo file
    nBytes := BASS_ChannelGetData(MusicStream.Handle, @data, sizeof(TPCMData));
  end;
  if(nBytes <= 0) then
    result := 0
  else
    result := nBytes div sizeof(TPCMStereoSample);
end;

function TAudioPlayback_Bass.OpenSound(const Filename: string): TAudioPlaybackStream;
begin
  result := Load(Filename);
end;

procedure TAudioPlayback_Bass.PlaySound(stream: TAudioPlaybackStream);
begin
  if assigned(stream) then
    stream.Play();
end;

procedure TAudioPlayback_Bass.StopSound(stream: TAudioPlaybackStream);
begin
  if assigned(stream) then
    stream.Stop();
end;


initialization
  singleton_AudioPlaybackBass := TAudioPlayback_Bass.create();
  AudioManager.add( singleton_AudioPlaybackBass );

finalization
  AudioManager.Remove( singleton_AudioPlaybackBass );

end.
