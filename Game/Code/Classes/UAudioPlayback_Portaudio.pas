unit UAudioPlayback_Portaudio;

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
  {$IFNDEF Win32}
  libc,
  {$ENDIF}
  sdl,
  portaudio,
  ULog,
  UIni,
  UMain;

type
  TPortaudioPlaybackStream = class(TAudioPlaybackStream)
    private
      Status:   TStreamStatus;
      Loop:     boolean;

      _volume: integer;

      procedure Reset();
    public
      DecodeStream: TAudioDecodeStream;

      constructor Create();
      destructor Destroy(); override;

      function SetDecodeStream(decodeStream: TAudioDecodeStream): boolean;

      procedure Play();                     override;
      procedure Pause();                    override;
      procedure Stop();                     override;
      procedure Close();                    override;
      function GetLoop(): boolean;          override;
      procedure SetLoop(Enabled: boolean);  override;
      function GetLength(): real;           override;
      function GetStatus(): TStreamStatus;  override;

      function IsLoaded(): boolean;

      function GetVolume(): integer;        override;
      procedure SetVolume(volume: integer); override;

      // functions delegated to the decode stream
      function GetPosition: real;
      procedure SetPosition(Time: real);
      function ReadData(Buffer: PChar; BufSize: integer): integer;
  end;

type
  TAudioMixerStream = class
    private
      activeStreams: TList;
      mixerBuffer: PChar;
      internalLock: PSDL_Mutex;

      _volume: integer;

      procedure Lock(); inline;
      procedure Unlock(); inline;

      function GetVolume(): integer;
      procedure SetVolume(volume: integer);
    public
      constructor Create();
      destructor Destroy(); override;
      procedure AddStream(stream: TAudioPlaybackStream);
      procedure RemoveStream(stream: TAudioPlaybackStream);
      function ReadData(Buffer: PChar; BufSize: integer): integer;

      property Volume: integer READ GetVolume WRITE SetVolume;
  end;

type
  TAudioPlayback_Portaudio = class( TInterfacedObject, IAudioPlayback )
    private
      MusicStream:        TPortaudioPlaybackStream;

      MixerStream: TAudioMixerStream;
      paStream:    PPaStream;

      FrameSize: integer;

      function InitializePortaudio(): boolean;
      function StartPortaudioStream(): boolean;

      function InitializeSDLAudio(): boolean;
      function StartSDLAudioStream(): boolean;
      procedure StopSDLAudioStream();
    public
      function  GetName: String;

      function InitializePlayback(): boolean;
      destructor Destroy; override;

      function Load(const Filename: String): TPortaudioPlaybackStream;

      procedure SetVolume(Volume: integer);
      procedure SetMusicVolume(Volume: integer);
      procedure SetLoop(Enabled: boolean);
      function Open(Filename: string): boolean; // true if succeed
      procedure Rewind;
      procedure SetPosition(Time: real);
      procedure Play;
      procedure Pause;

      procedure Stop;
      procedure Close;
      function Finished: boolean;
      function Length: real;
      function GetPosition: real;

      // Equalizer
      procedure GetFFTData(var data: TFFTData);

      // Interface for Visualizer
      function GetPCMData(var data: TPCMData): Cardinal;

      // Sounds
      function OpenSound(const Filename: String): TAudioPlaybackStream;
      procedure PlaySound(stream: TAudioPlaybackStream);
      procedure StopSound(stream: TAudioPlaybackStream);
  end;


function AudioCallback(input: Pointer; output: Pointer; frameCount: Longword;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      userData: Pointer): Integer; cdecl; forward;

var
  singleton_AudioPlaybackPortaudio : IAudioPlayback;


{ TAudioMixerStream }

constructor TAudioMixerStream.Create();
begin
  activeStreams := TList.Create;
  internalLock := SDL_CreateMutex();
  _volume := 100;
end;

destructor TAudioMixerStream.Destroy();
begin
  if assigned(mixerBuffer) then
    Freemem(mixerBuffer);
  activeStreams.Free;
  SDL_DestroyMutex(internalLock);
end;

procedure TAudioMixerStream.Lock();
begin
  SDL_mutexP(internalLock);
end;

procedure TAudioMixerStream.Unlock();
begin
  SDL_mutexV(internalLock);
end;

function TAudioMixerStream.GetVolume(): integer;
begin
  Lock();
  result := _volume;
  Unlock();
end;

procedure TAudioMixerStream.SetVolume(volume: integer);
begin
  Lock();
  _volume := volume;
  Unlock();
end;

procedure TAudioMixerStream.AddStream(stream: TAudioPlaybackStream);
begin
  if not assigned(stream) then
    Exit;

  Lock();
  // check if stream is already in list to avoid duplicates
  if (activeStreams.IndexOf(Pointer(stream)) = -1) then
    activeStreams.Add(Pointer(stream));
  Unlock();
end;

procedure TAudioMixerStream.RemoveStream(stream: TAudioPlaybackStream);
begin
  Lock();
  activeStreams.Remove(Pointer(stream));
  Unlock();
end;

function TAudioMixerStream.ReadData(Buffer: PChar; BufSize: integer): integer;
var
  i: integer;
  size: integer;
  stream: TPortaudioPlaybackStream;
  appVolume: single;
begin
  result := BufSize;

  // zero target-buffer (silence)
  FillChar(Buffer^, BufSize, 0);

  // resize mixer-buffer if necessary
  ReallocMem(mixerBuffer, BufSize);
  if not assigned(mixerBuffer) then
    Exit;

  Lock();

  //writeln('Mix: ' + inttostr(activeStreams.Count));

  // use _volume instead of Volume to prevent recursive locking 
  appVolume := _volume / 100 * SDL_MIX_MAXVOLUME;

  for i := 0 to activeStreams.Count-1 do
  begin
    stream := TPortaudioPlaybackStream(activeStreams[i]);
    if (stream.GetStatus() = ssPlaying) then
    begin
      // fetch data from current stream
      size := stream.ReadData(mixerBuffer, BufSize);
      if (size > 0) then
      begin
        SDL_MixAudio(PUInt8(Buffer), PUInt8(mixerBuffer), size,
          Trunc(appVolume * stream.Volume / 100));
      end;
    end;
  end;

  Unlock();
end;


{ TPortaudioPlaybackStream }

constructor TPortaudioPlaybackStream.Create();
begin
  inherited Create();
  Reset();
end;

destructor TPortaudioPlaybackStream.Destroy();
begin
  Close();
  inherited Destroy();
end;

procedure TPortaudioPlaybackStream.Reset();
begin
  Status := ssStopped;
  Loop := false;
  DecodeStream := nil;
  _volume := 0;
end;

function TPortaudioPlaybackStream.SetDecodeStream(decodeStream: TAudioDecodeStream): boolean;
begin
  result := false;

  Reset();

  if not assigned(decodeStream) then
    Exit;
  Self.DecodeStream := decodeStream;

  _volume := 100;

  result := true;
end;

procedure TPortaudioPlaybackStream.Close();
begin
  Reset();
end;

procedure TPortaudioPlaybackStream.Play();
begin
  if (status <> ssPaused) then
  begin
    // rewind
    if assigned(DecodeStream) then
      DecodeStream.Position := 0;
  end;
  status := ssPlaying;
  //MixerStream.AddStream(Self);
end;

procedure TPortaudioPlaybackStream.Pause();
begin
  status := ssPaused;
end;

procedure TPortaudioPlaybackStream.Stop();
begin
  status := ssStopped;
end;

function TPortaudioPlaybackStream.IsLoaded(): boolean;
begin
  result := assigned(DecodeStream);
end;

function TPortaudioPlaybackStream.GetLoop(): boolean;
begin
  result := Loop;
end;

procedure TPortaudioPlaybackStream.SetLoop(Enabled: boolean);
begin
  Loop := Enabled;
end;

function TPortaudioPlaybackStream.GetLength(): real;
begin
  if assigned(DecodeStream) then
    result := DecodeStream.Length
  else
    result := -1;
end;

function TPortaudioPlaybackStream.GetStatus(): TStreamStatus;
begin
  result := status;
end;

function TPortaudioPlaybackStream.ReadData(Buffer: PChar; BufSize: integer): integer;
begin
  if not assigned(DecodeStream) then
  begin
    result := -1;
    Exit;
  end;
  result := DecodeStream.ReadData(Buffer, BufSize);
  // end-of-file reached -> stop playback
  if (DecodeStream.EOF) then
  begin
    status := ssStopped;
  end;
end;

function TPortaudioPlaybackStream.GetPosition: real;
begin
  if assigned(DecodeStream) then
    result := DecodeStream.Position
  else
    result := -1;
end;

procedure TPortaudioPlaybackStream.SetPosition(Time: real);
begin
  if assigned(DecodeStream) then
    DecodeStream.Position := Time;
end;

function TPortaudioPlaybackStream.GetVolume(): integer;
begin
  result := _volume;
end;

procedure TPortaudioPlaybackStream.SetVolume(volume: integer);
begin
  // clamp volume
  if (volume > 100) then
    _volume := 100
  else if (volume < 0) then
    _volume := 0
  else
    _volume := volume;
end;


{ TAudioPlayback_Portaudio }

function AudioCallback(input: Pointer; output: Pointer; frameCount: Longword;
    timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
    userData: Pointer): Integer; cdecl;
var
  playback: TAudioPlayback_Portaudio;
begin
  playback := TAudioPlayback_Portaudio(userData);
  with playback do
  begin
    MixerStream.ReadData(output, frameCount * FrameSize);
  end;
  result := paContinue;
end;

procedure SDLAudioCallback(userdata: Pointer; stream: PChar; len: integer); cdecl;
var
  playback: TAudioPlayback_Portaudio;
begin
  playback := TAudioPlayback_Portaudio(userdata);
  with playback do
  begin
    //MixerStream.ReadData(stream, len);
  end;
end;

function TAudioPlayback_Portaudio.GetName: String;
begin
  result := 'Portaudio_Playback';
end;

function TAudioPlayback_Portaudio.InitializePortaudio(): boolean;
var
  paApi           : TPaHostApiIndex;
  paApiInfo       : PPaHostApiInfo;
  paOutParams     : TPaStreamParameters;
  paOutDevice     : TPaDeviceIndex;
  paOutDeviceInfo : PPaDeviceInfo;
  err             : TPaError;
begin
  result := false;

  Pa_Initialize();

  // FIXME: determine automatically
  {$IFDEF WIN32}
  paApi := Pa_HostApiTypeIdToHostApiIndex(paDirectSound);
  {$ELSE}
  paApi := Pa_HostApiTypeIdToHostApiIndex(paALSA);
  {$ENDIF}
  if (paApi < 0) then
  begin
    Log.LogStatus('Pa_HostApiTypeIdToHostApiIndex: '+Pa_GetErrorText(paApi), 'UAudioPlayback_Portaudio');
    exit;
  end;

  paApiInfo := Pa_GetHostApiInfo(paApi);
  paOutDevice     := paApiInfo^.defaultOutputDevice;
  paOutDeviceInfo := Pa_GetDeviceInfo(paOutDevice);

  with paOutParams do begin
    device := paOutDevice;
    channelCount := 2;
    sampleFormat := paInt16;
    suggestedLatency := paOutDeviceInfo^.defaultHighOutputLatency;
    hostApiSpecificStreamInfo := nil;
  end;

  // set the size of one audio frame (2channel 16bit uint sample)
  FrameSize := 2 * sizeof(Smallint);

  err := Pa_OpenStream(paStream, nil, @paOutParams, 44100,
          paFramesPerBufferUnspecified,
          paNoFlag, @AudioCallback, Self);
  if(err <> paNoError) then begin
    Log.LogStatus('Pa_OpenStream: '+Pa_GetErrorText(err), 'UAudioPlayback_Portaudio');
    exit;
  end;

  Log.LogStatus('Opened audio device', 'UAudioPlayback_Portaudio');

  result := true;
end;

function TAudioPlayback_Portaudio.StartPortaudioStream(): boolean;
var
  err: TPaError;
begin
  result := false;

  err := Pa_StartStream(paStream);
  if(err <> paNoError) then
  begin
    Log.LogStatus('Pa_StartStream: '+Pa_GetErrorText(err), 'UAudioPlayback_Portaudio');
    exit;
  end;

  result := true;
end;

function TAudioPlayback_Portaudio.InitializeSDLAudio(): boolean;
var
  desiredAudioSpec, obtainedAudioSpec: TSDL_AudioSpec;
  err: integer;
begin
  result := false;

  SDL_InitSubSystem(SDL_INIT_AUDIO);

  FillChar(desiredAudioSpec, sizeof(desiredAudioSpec), 0);
  with desiredAudioSpec do
  begin
    freq := 44100;
    format := AUDIO_S16SYS;
    channels := 2;
    samples := 1024; // latency: 23 ms
    callback := @SDLAudioCallback;
    userdata := Self;
  end;

  // set the size of one audio frame (2channel 16bit uint sample)
  FrameSize := 2 * sizeof(Smallint);

  if(SDL_OpenAudio(@desiredAudioSpec, @obtainedAudioSpec) = -1) then
  begin
    Log.LogStatus('SDL_OpenAudio: ' + SDL_GetError(), 'UAudioPlayback_SDL');
    exit;
  end;

  Log.LogStatus('Opened audio device', 'UAudioPlayback_SDL');

  result := true;
end;

function TAudioPlayback_Portaudio.StartSDLAudioStream(): boolean;
begin
  SDL_PauseAudio(0);
  result := true;
end;

procedure TAudioPlayback_Portaudio.StopSDLAudioStream();
begin
  SDL_CloseAudio();
end;

function TAudioPlayback_Portaudio.InitializePlayback: boolean;
begin
  result := false;

  //Log.LogStatus('InitializePlayback', 'UAudioPlayback_Portaudio');

  //if(not InitializePortaudio()) then
  if(not InitializeSDLAudio()) then
    Exit;

  MixerStream := TAudioMixerStream.Create;

  //if(not StartPortaudioStream()) then;
  if(not StartSDLAudioStream()) then
    Exit;

  result := true;
end;

destructor TAudioPlayback_Portaudio.Destroy;
begin
  StopSDLAudioStream();

  MixerStream.Free();
  MusicStream.Free();

  inherited Destroy();
end;

function TAudioPlayback_Portaudio.Load(const Filename: String): TPortaudioPlaybackStream;
var
  decodeStream: TAudioDecodeStream;
  playbackStream: TPortaudioPlaybackStream;
begin
  Result := nil;

  decodeStream := AudioDecoder.Open(Filename);
  if not assigned(decodeStream) then
  begin
    Log.LogStatus('LoadSoundFromFile: Sound not found "' + Filename + '"', 'UAudioPlayback_Portaudio');
    Exit;
  end;

  playbackStream := TPortaudioPlaybackStream.Create();
  if (not playbackStream.SetDecodeStream(decodeStream)) then
    Exit;

  // FIXME: remove this line
  MixerStream.AddStream(playbackStream);

  result := playbackStream;
end;

procedure TAudioPlayback_Portaudio.SetVolume(Volume: integer);
begin
  // sets volume only for this application
  MixerStream.Volume := Volume;
end;

procedure TAudioPlayback_Portaudio.SetMusicVolume(Volume: Integer);
begin
  if assigned(MusicStream) then
    MusicStream.Volume := Volume;
end;

procedure TAudioPlayback_Portaudio.SetLoop(Enabled: boolean);
begin
  if assigned(MusicStream) then
    MusicStream.SetLoop(Enabled);
end;

function TAudioPlayback_Portaudio.Open(Filename: string): boolean;
var
  decodeStream: TAudioDecodeStream;
begin
  Result := false;

  // free old MusicStream
  MusicStream.Free();

  MusicStream := Load(Filename);
  if not assigned(MusicStream) then
    Exit;

  //Set Max Volume
  SetMusicVolume(100);

  Result := true;
end;

procedure TAudioPlayback_Portaudio.Rewind;
begin
  SetPosition(0);
end;

procedure TAudioPlayback_Portaudio.SetPosition(Time: real);
begin
  if assigned(MusicStream) then
    MusicStream.SetPosition(Time);
end;

function TAudioPlayback_Portaudio.GetPosition: real;
begin
  if assigned(MusicStream) then
    Result := MusicStream.GetPosition()
  else
    Result := -1;
end;

function TAudioPlayback_Portaudio.Length: real;
begin
  if assigned(MusicStream) then
    Result := MusicStream.GetLength()
  else
    Result := -1;
end;

procedure TAudioPlayback_Portaudio.Play;
begin
  if assigned(MusicStream) then
    MusicStream.Play();
end;

procedure TAudioPlayback_Portaudio.Pause;
begin
  if assigned(MusicStream) then
    MusicStream.Pause();
end;

procedure TAudioPlayback_Portaudio.Stop;
begin
  if assigned(MusicStream) then
    MusicStream.Stop();
end;

procedure TAudioPlayback_Portaudio.Close;
begin
  if assigned(MusicStream) then
  begin
    MixerStream.RemoveStream(MusicStream);
    MusicStream.Close();
  end;
end;

function TAudioPlayback_Portaudio.Finished: boolean;
begin
  if assigned(MusicStream) then
    Result := (MusicStream.GetStatus() = ssStopped)
  else
    Result := true;
end;

//Equalizer
procedure TAudioPlayback_Portaudio.GetFFTData(var data: TFFTData);
begin
  //Get Channel Data Mono and 256 Values
//  BASS_ChannelGetData(Bass, @Result, BASS_DATA_FFT512);
end;

// Interface for Visualizer
function TAudioPlayback_Portaudio.GetPCMData(var data: TPCMData): Cardinal;
begin
  result := 0;
end;

function TAudioPlayback_Portaudio.OpenSound(const Filename: String): TAudioPlaybackStream;
begin
  result := Load(Filename);
end;

procedure TAudioPlayback_Portaudio.PlaySound(stream: TAudioPlaybackStream);
begin
  if assigned(stream) then
    stream.Play();
end;

procedure TAudioPlayback_Portaudio.StopSound(stream: TAudioPlaybackStream);
begin
  if assigned(stream) then
    stream.Stop();
end;


initialization
  singleton_AudioPlaybackPortaudio := TAudioPlayback_Portaudio.create();
  AudioManager.add( singleton_AudioPlaybackPortaudio );

finalization
  AudioManager.Remove( singleton_AudioPlaybackPortaudio );


end.
