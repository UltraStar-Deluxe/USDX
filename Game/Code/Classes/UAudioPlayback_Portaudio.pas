unit UAudioPlayback_Portaudio;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}


uses Classes,
     SysUtils,
     UMusic;

implementation

uses
     {$IFDEF LAZARUS}
     lclintf,
       {$ifndef win32}
       libc,
       {$endif}
     {$ENDIF}
     sdl,
     portaudio,
     ULog,
     UIni,
     UMain;

type
  TPortaudioPlaybackStream = class(TAudioPlaybackStream)
    private
      status:   TStreamStatus;
      Loaded:   boolean;
      Loop:     boolean;
    public
      decodeStream: TAudioDecodeStream;

      constructor Create(decodeStream: TAudioDecodeStream);
      procedure Play();                     override;
      procedure Pause();                    override;
      procedure Stop();                     override;
      procedure Close();                    override;
      function GetLoop(): boolean;          override;
      procedure SetLoop(Enabled: boolean);  override;
      function GetLength(): real;           override;
      function GetStatus(): TStreamStatus;  override;

      function IsLoaded(): boolean;

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
    public
      constructor Create();
      destructor Destroy(); override;
      procedure AddStream(stream: TAudioPlaybackStream);
      procedure RemoveStream(stream: TAudioPlaybackStream);
      function ReadData(Buffer: PChar; BufSize: integer): integer;
  end;

type
  TAudioPlayback_Portaudio = class( TInterfacedObject, IAudioPlayback )
    private
      MusicStream:        TPortaudioPlaybackStream;

      StartSoundStream:   TPortaudioPlaybackStream;
      BackSoundStream:    TPortaudioPlaybackStream;
      SwooshSoundStream:  TPortaudioPlaybackStream;
      ChangeSoundStream:  TPortaudioPlaybackStream;
      OptionSoundStream:  TPortaudioPlaybackStream;
      ClickSoundStream:   TPortaudioPlaybackStream;
      DrumSoundStream:    TPortaudioPlaybackStream;
      HihatSoundStream:   TPortaudioPlaybackStream;
      ClapSoundStream:    TPortaudioPlaybackStream;
      ShuffleSoundStream: TPortaudioPlaybackStream;

      //Custom Sounds
      CustomSounds: array of TCustomSoundEntry;

      mixerStream: TAudioMixerStream;
      paStream:    PPaStream;
    public
      FrameSize: integer;

      function  GetName: String;

      function InitializePortaudio(): boolean;
      function StartPortaudioStream(): boolean;

      procedure InitializePlayback();
      procedure SetVolume(Volume: integer);
      procedure SetMusicVolume(Volume: integer);
      procedure SetLoop(Enabled: boolean);
      function Open(Filename: string): boolean; // true if succeed
      function Load(Filename: string): TPortaudioPlaybackStream;
      procedure Rewind;
      procedure SetPosition(Time: real);
      procedure Play;
      procedure Pause;

      procedure Stop;
      procedure Close;
      function Finished: boolean;
      function Length: real;
      function GetPosition: real;
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

      //Equalizer
      function GetFFTData: TFFTData;

      // Interface for Visualizer
      function GetPCMData(var data: TPCMData): Cardinal;

      //Custom Sounds
      function LoadCustomSound(const Filename: String): Cardinal;
      procedure PlayCustomSound(const Index: Cardinal );
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
end;

destructor TAudioMixerStream.Destroy();
begin
  if (mixerBuffer <> nil) then
    Freemem(mixerBuffer);
  activeStreams.Free;
end;

procedure TAudioMixerStream.AddStream(stream: TAudioPlaybackStream);
begin
  // check if stream is already in list to avoid duplicates
  if (activeStreams.IndexOf(Pointer(stream)) = -1) then
    activeStreams.Add(Pointer(stream));
end;

procedure TAudioMixerStream.RemoveStream(stream: TAudioPlaybackStream);
begin
  activeStreams.Remove(Pointer(stream));
end;

function TAudioMixerStream.ReadData(Buffer: PChar; BufSize: integer): integer;
var
  i: integer;
  size: integer;
  stream: TPortaudioPlaybackStream;
begin
  result := BufSize;

  // zero target-buffer (silence)
  FillChar(Buffer^, BufSize, 0);

  // resize mixer-buffer
  ReallocMem(mixerBuffer, BufSize);
  if (mixerBuffer = nil) then
    Exit;

  writeln('Mix: ' + inttostr(activeStreams.Count));

  for i := 0 to activeStreams.Count-1 do
  begin
    stream := TPortaudioPlaybackStream(activeStreams[i]);
    if (stream.GetStatus() = sPlaying) then
    begin
      // fetch data from current stream
      size := stream.ReadData(mixerBuffer, BufSize);
      if (size > 0) then
      begin
        SDL_MixAudio(PUInt8(Buffer), PUInt8(mixerBuffer), size, SDL_MIX_MAXVOLUME);
      end;
    end;
  end;
end;


{ TPortaudioPlaybackStream }

constructor TPortaudioPlaybackStream.Create(decodeStream: TAudioDecodeStream);
begin
  inherited Create();
  status := sStopped;
  if (decodeStream <> nil) then
  begin
    Self.decodeStream := decodeStream;
    Loaded := true;
  end;
end;

procedure TPortaudioPlaybackStream.Play();
begin
  if (status <> sPaused) then
  begin
    // rewind
    decodeStream.Position := 0;
  end;
  status := sPlaying;
  mixerStream.AddStream(Self);
end;

procedure TPortaudioPlaybackStream.Pause();
begin
  status := sPaused;
end;

procedure TPortaudioPlaybackStream.Stop();
begin
  status := sStopped;
end;

procedure TPortaudioPlaybackStream.Close();
begin
  status := sStopped;
  Loaded := false;
  // TODO: cleanup decode-stream
end;

function TPortaudioPlaybackStream.IsLoaded(): boolean;
begin
  result := Loaded;
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
  result := decodeStream.Length;
end;

function TPortaudioPlaybackStream.GetStatus(): TStreamStatus;
begin
  result := status;
end;

function TPortaudioPlaybackStream.ReadData(Buffer: PChar; BufSize: integer): integer;
begin
  result := decodeStream.ReadData(Buffer, BufSize);
  // end-of-file reached -> stop playback
  if (decodeStream.EOF) then
  begin
    status := sStopped;
  end;
end;

function TPortaudioPlaybackStream.GetPosition: real;
begin
  result := decodeStream.Position;
end;

procedure TPortaudioPlaybackStream.SetPosition(Time: real);
begin
  decodeStream.Position := Time;
end;


{ TAudioPlayback_Portaudio }

function AudioCallback(input: Pointer; output: Pointer; frameCount: Longword;
    timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
    userData: Pointer): Integer; cdecl;
var
  playback        : TAudioPlayback_Portaudio;
  playbackStream  : TPortaudioPlaybackStream;
  decodeStream    : TAudioDecodeStream;
begin
  playback := TAudioPlayback_Portaudio(userData);
  with playback do
  begin
    mixerStream.ReadData(output, frameCount * FrameSize);
  end;
  result := paContinue;
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
    suggestedLatency := paOutDeviceInfo^.defaultLowOutputLatency;
    hostApiSpecificStreamInfo := nil;
  end;

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

procedure TAudioPlayback_Portaudio.InitializePlayback;
begin
  Log.LogStatus('InitializePlayback', 'UAudioPlayback_Portaudio');

  InitializePortaudio();
  mixerStream := TAudioMixerStream.Create;

  StartSoundStream  := Load(SoundPath + 'Common start.mp3');
  BackSoundStream   := Load(SoundPath + 'Common back.mp3');
  SwooshSoundStream := Load(SoundPath + 'menu swoosh.mp3');
  ChangeSoundStream := Load(SoundPath + 'select music change music 50.mp3');
  OptionSoundStream := Load(SoundPath + 'option change col.mp3');
  ClickSoundStream  := Load(SoundPath + 'rimshot022b.mp3');

//  DrumSoundStream  := Load(SoundPath + 'bassdrumhard076b.mp3');
//  HihatSoundStream := Load(SoundPath + 'hihatclosed068b.mp3');
//  ClapSoundStream  := Load(SoundPath + 'claps050b.mp3');

//  ShuffleSoundStream := Load(SoundPath + 'Shuffle.mp3');

  StartPortaudioStream();
end;


procedure TAudioPlayback_Portaudio.SetVolume(Volume: integer);
begin
  //New: Sets Volume only for this Application
(*
  BASS_SetConfig(BASS_CONFIG_GVOL_SAMPLE, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, Volume);
  BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, Volume);
*)
end;

procedure TAudioPlayback_Portaudio.SetMusicVolume(Volume: Integer);
begin
  //Max Volume Prevention
  if Volume > 100 then
    Volume := 100;

  if Volume < 0 then
    Volume := 0;

  //Set Volume
//  BASS_ChannelSetAttributes (Bass, -1, Volume, -101);
end;

procedure TAudioPlayback_Portaudio.SetLoop(Enabled: boolean);
begin
  if (MusicStream <> nil) then
  if (MusicStream.IsLoaded) then
   MusicStream.SetLoop(Enabled);
end;

function TAudioPlayback_Portaudio.Open(Filename: string): boolean;
var
  decodeStream: TAudioDecodeStream;
begin
  decodeStream := AudioDecoder.Open(Filename);
  MusicStream := TPortaudioPlaybackStream.Create(decodeStream);

  if(MusicStream.IsLoaded()) then
  begin
    //Set Max Volume
    SetMusicVolume(100);
  end;

  Result := MusicStream.IsLoaded();
end;

procedure TAudioPlayback_Portaudio.Rewind;
begin
  SetPosition(0);
end;

procedure TAudioPlayback_Portaudio.SetPosition(Time: real);
begin
  if (MusicStream.IsLoaded) then
  begin
    MusicStream.SetPosition(Time);
  end;
end;

function TAudioPlayback_Portaudio.GetPosition: real;
begin
  if (MusicStream.IsLoaded) then
  Result := MusicStream.GetPosition();
end;

function TAudioPlayback_Portaudio.Length: real;
begin
  Result := 0;
  if assigned( MusicStream ) then
  if (MusicStream.IsLoaded) then
  begin
    Result := MusicStream.GetLength();
  end;
end;

procedure TAudioPlayback_Portaudio.Play;
begin
  if (MusicStream <> nil) then
  if (MusicStream.IsLoaded) then
  begin
    if (MusicStream.GetLoop()) then
    begin
    end;
    // start from beginning...
    // actually bass itself does not loop, nor does this TAudio_FFMpeg Class
    MusicStream.Play();
  end;
end;

procedure TAudioPlayback_Portaudio.Pause;
begin
  if (MusicStream <> nil) then
    MusicStream.Pause();
end;

procedure TAudioPlayback_Portaudio.Stop;
begin
  if MusicStream <> nil then
  MusicStream.Stop();
end;

procedure TAudioPlayback_Portaudio.Close;
begin
  if MusicStream <> nil then
  MusicStream.Close();
end;

function TAudioPlayback_Portaudio.Finished: boolean;
begin
  if MusicStream <> nil then
  Result := (MusicStream.GetStatus() = sStopped);
end;

procedure TAudioPlayback_Portaudio.PlayStart;
begin
  if StartSoundStream <> nil then
  StartSoundStream.Play();
end;

procedure TAudioPlayback_Portaudio.PlayBack;
begin
  if BackSoundStream <> nil then
  BackSoundStream.Play();
end;

procedure TAudioPlayback_Portaudio.PlaySwoosh;
begin
  if SwooshSoundStream <> nil then
  SwooshSoundStream.Play();
end;

procedure TAudioPlayback_Portaudio.PlayChange;
begin
  if ChangeSoundStream <> nil then
  ChangeSoundStream.Play();
end;

procedure TAudioPlayback_Portaudio.PlayOption;
begin
  if OptionSoundStream <> nil then
  OptionSoundStream.Play();
end;

procedure TAudioPlayback_Portaudio.PlayClick;
begin
  if ClickSoundStream <> nil then
  ClickSoundStream.Play();
end;

procedure TAudioPlayback_Portaudio.PlayDrum;
begin
  if DrumSoundStream <> nil then
  DrumSoundStream.Play();
end;

procedure TAudioPlayback_Portaudio.PlayHihat;
begin
  if HihatSoundStream <> nil then
  HihatSoundStream.Play();
end;

procedure TAudioPlayback_Portaudio.PlayClap;
begin
  if ClapSoundStream <> nil then
  ClapSoundStream.Play();
end;

procedure TAudioPlayback_Portaudio.PlayShuffle;
begin
  if ShuffleSoundStream <> nil then
  ShuffleSoundStream.Play();
end;

procedure TAudioPlayback_Portaudio.StopShuffle;
begin
  if ShuffleSoundStream <> nil then
  ShuffleSoundStream.Stop();
end;

//Equalizer
function TAudioPlayback_Portaudio.GetFFTData: TFFTData;
var
  data: TFFTData;
begin
  //Get Channel Data Mono and 256 Values
//  BASS_ChannelGetData(Bass, @Result, BASS_DATA_FFT512);
  result := data;
end;

// Interface for Visualizer
function TAudioPlayback_Portaudio.GetPCMData(var data: TPCMData): Cardinal;
begin
  result := 0;
end;

function TAudioPlayback_Portaudio.Load(Filename: string): TPortaudioPlaybackStream;
var
  decodeStream    : TAudioDecodeStream;
  playbackStream  : TPortaudioPlaybackStream;
  csIndex         : integer;
begin
  result := nil;

  decodeStream := AudioDecoder.Open(Filename);
  if (decodeStream = nil) then
  begin
    Log.LogStatus('LoadSoundFromFile: Sound not found "' + Filename + '"', 'UAudioPlayback_Portaudio');
    exit;
  end;

  playbackStream := TPortaudioPlaybackStream.Create(decodeStream);

  //Add CustomSound
  csIndex := High(CustomSounds) + 1;
  SetLength(CustomSounds, csIndex + 1);
  CustomSounds[csIndex].Filename := Filename;
  CustomSounds[csIndex].Stream := playbackStream;

  result := playbackStream;
end;

function TAudioPlayback_Portaudio.LoadCustomSound(const Filename: String): Cardinal;
var
  S: TAudioPlaybackStream;
  I: Integer;
  F: String;
begin
  //Search for Sound in already loaded Sounds
  F := UpperCase(SoundPath + FileName);
  For I := 0 to High(CustomSounds) do
  begin
    if (UpperCase(CustomSounds[I].Filename) = F) then
    begin
      Result := I;
      Exit;
    end;
  end;

  S := Load(SoundPath + Filename);
  if (S <> nil) then
    Result := High(CustomSounds)
  else
    Result := 0;
end;

procedure TAudioPlayback_Portaudio.PlayCustomSound(const Index: Cardinal );
begin
  if (Index <= High(CustomSounds)) then
    CustomSounds[Index].Stream.Play();
end;



initialization
  singleton_AudioPlaybackPortaudio := TAudioPlayback_Portaudio.create();
  AudioManager.add( singleton_AudioPlaybackPortaudio );

finalization
  AudioManager.Remove( singleton_AudioPlaybackPortaudio );


end.
