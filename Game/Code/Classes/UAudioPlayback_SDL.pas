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
    protected
      function InitializeAudioPlaybackEngine(): boolean; override;
      function StartAudioPlaybackEngine(): boolean;      override;
      procedure StopAudioPlaybackEngine();               override;
    public
      function  GetName: String;                         override;
      procedure MixBuffers(dst, src: PChar; size: Cardinal; volume: Integer); override;
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

function TAudioPlayback_SDL.InitializeAudioPlaybackEngine(): boolean;
var
  desiredAudioSpec, obtainedAudioSpec: TSDL_AudioSpec;
// err: integer; // Auto Removed, Unused Variable
begin
  result := false;

  SDL_InitSubSystem(SDL_INIT_AUDIO);

  FillChar(desiredAudioSpec, sizeof(desiredAudioSpec), 0);
  with desiredAudioSpec do
  begin
    freq := 44100;
    format := AUDIO_S16SYS;
    channels := 2;
    samples := Ini.SDLBufferSize;
    callback := @SDLAudioCallback;
    userdata := Self;
  end;

  if(SDL_OpenAudio(@desiredAudioSpec, @obtainedAudioSpec) = -1) then
  begin
    Log.LogStatus('SDL_OpenAudio: ' + SDL_GetError(), 'UAudioPlayback_SDL');
    exit;
  end;

  FormatInfo := TAudioFormatInfo.Create(
    obtainedAudioSpec.channels,
    obtainedAudioSpec.freq,
    asfS16
  );

  Log.LogStatus('Opened audio device', 'UAudioPlayback_SDL');

  result := true;
end;

function TAudioPlayback_SDL.StartAudioPlaybackEngine(): boolean;
begin
  SDL_PauseAudio(0);
  result := true;
end;

procedure TAudioPlayback_SDL.StopAudioPlaybackEngine();
begin
  SDL_CloseAudio();
end;

procedure TAudioPlayback_SDL.MixBuffers(dst, src: PChar; size: Cardinal; volume: Integer);
begin
  // Note: (volume * SDL_MIX_MAXVOLUME) may exceed High(Integer)
  //   if SDL_MIX_MAXVOLUME (=128 at the moment) changes
  SDL_MixAudio(PUInt8(dst), PUInt8(src), size, volume * SDL_MIX_MAXVOLUME div 100);
end;


initialization
  singleton_AudioPlaybackSDL := TAudioPlayback_SDL.create();
  AudioManager.add( singleton_AudioPlaybackSDL );

finalization
  AudioManager.Remove( singleton_AudioPlaybackSDL );


end.
