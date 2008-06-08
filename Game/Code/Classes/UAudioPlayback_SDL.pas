unit UAudioPlayback_SDL;

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
  sdl,
  UAudioPlayback_SoftMixer,
  ULog,
  UIni,
  UMain;

type
  TAudioPlayback_SDL = class(TAudioPlayback_SoftMixer)
    private
      function EnumDevices(): boolean;
    protected
      function InitializeAudioPlaybackEngine(): boolean; override;
      function StartAudioPlaybackEngine(): boolean;      override;
      procedure StopAudioPlaybackEngine();               override;
      function FinalizeAudioPlaybackEngine(): boolean;   override;
    public
      function GetName: String;                          override;
      procedure MixBuffers(dst, src: PChar; size: Cardinal; volume: Single); override;
  end;

var
  singleton_AudioPlaybackSDL : IAudioPlayback;

  
{ TAudioPlayback_SDL }

procedure SDLAudioCallback(userdata: Pointer; stream: PChar; len: integer); cdecl;
var
  engine: TAudioPlayback_SDL;
begin
  engine := TAudioPlayback_SDL(userdata);
  engine.AudioCallback(stream, len);
end;

function TAudioPlayback_SDL.GetName: String;
begin
  result := 'SDL_Playback';
end;

function TAudioPlayback_SDL.EnumDevices(): boolean;
begin
  // Note: SDL does not provide Device-Selection capabilities (will be introduced in 1.3)
  ClearOutputDeviceList();
  SetLength(OutputDeviceList, 1);
  OutputDeviceList[0] := TAudioOutputDevice.Create();
  OutputDeviceList[0].Name := '[SDL Default-Device]';
  Result := true;
end;

function TAudioPlayback_SDL.InitializeAudioPlaybackEngine(): boolean;
var
  desiredAudioSpec, obtainedAudioSpec: TSDL_AudioSpec;
  SampleBufferSize: integer;
begin
  result := false;

  EnumDevices();

  if (SDL_InitSubSystem(SDL_INIT_AUDIO) = -1) then
  begin
    Log.LogError('SDL_InitSubSystem failed!', 'TAudioPlayback_SDL.InitializeAudioPlaybackEngine');
    exit;
  end;

  SampleBufferSize := IAudioOutputBufferSizeVals[Ini.AudioOutputBufferSizeIndex];
  if (SampleBufferSize <= 0) then
  begin
    // Automatic setting defaults to 1024 samples
    SampleBufferSize := 1024;
  end;

  FillChar(desiredAudioSpec, sizeof(desiredAudioSpec), 0);
  with desiredAudioSpec do
  begin
    freq := 44100;
    format := AUDIO_S16SYS;
    channels := 2;
    samples := SampleBufferSize;
    callback := @SDLAudioCallback;
    userdata := Self;
  end;

  if(SDL_OpenAudio(@desiredAudioSpec, @obtainedAudioSpec) = -1) then
  begin
    Log.LogStatus('SDL_OpenAudio: ' + SDL_GetError(), 'TAudioPlayback_SDL.InitializeAudioPlaybackEngine');
    exit;
  end;

  FormatInfo := TAudioFormatInfo.Create(
    obtainedAudioSpec.channels,
    obtainedAudioSpec.freq,
    asfS16
  );

  Log.LogStatus('Opened audio device', 'TAudioPlayback_SDL.InitializeAudioPlaybackEngine');

  result := true;
end;

function TAudioPlayback_SDL.StartAudioPlaybackEngine(): boolean;
begin
  SDL_PauseAudio(0);
  result := true;
end;

procedure TAudioPlayback_SDL.StopAudioPlaybackEngine();
begin
  SDL_PauseAudio(1);
end;

function TAudioPlayback_SDL.FinalizeAudioPlaybackEngine(): boolean;
begin
  SDL_CloseAudio();
  SDL_QuitSubSystem(SDL_INIT_AUDIO);
  Result := true;
end;

procedure TAudioPlayback_SDL.MixBuffers(dst, src: PChar; size: Cardinal; volume: Single);
begin
  SDL_MixAudio(PUInt8(dst), PUInt8(src), size, Round(volume * SDL_MIX_MAXVOLUME));
end;


initialization
  singleton_AudioPlaybackSDL := TAudioPlayback_SDL.create();
  AudioManager.add( singleton_AudioPlaybackSDL );

finalization
  AudioManager.Remove( singleton_AudioPlaybackSDL );

end.
