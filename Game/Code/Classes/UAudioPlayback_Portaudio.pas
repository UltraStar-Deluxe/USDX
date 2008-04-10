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
  portaudio,
  UAudioCore_Portaudio,
  UAudioPlayback_SoftMixer,
  ULog,
  UIni,
  UMain;

type
  TAudioPlayback_Portaudio = class(TAudioPlayback_SoftMixer)
    private
      paStream:  PPaStream;
    protected
      function InitializeAudioPlaybackEngine(): boolean; override;
      function StartAudioPlaybackEngine(): boolean;      override;
      procedure StopAudioPlaybackEngine();               override;
    public
      function  GetName: String;                         override;
  end;

var
  singleton_AudioPlaybackPortaudio : IAudioPlayback;


{ TAudioPlayback_Portaudio }

function PortaudioAudioCallback(input: Pointer; output: Pointer; frameCount: Longword;
    timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
    userData: Pointer): Integer; cdecl;
var
  engine: TAudioPlayback_Portaudio;
begin
  engine := TAudioPlayback_Portaudio(userData);
  engine.AudioCallback(output, frameCount * engine.FormatInfo.FrameSize);
  result := paContinue;
end;

function TAudioPlayback_Portaudio.GetName: String;
begin
  result := 'Portaudio_Playback';
end;

function TAudioPlayback_Portaudio.InitializeAudioPlaybackEngine(): boolean;
var
  paApiIndex      : TPaHostApiIndex;
  paApiInfo       : PPaHostApiInfo;
  paOutParams     : TPaStreamParameters;
  paOutDevice     : TPaDeviceIndex;
  paOutDeviceInfo : PPaDeviceInfo;
  err             : TPaError;
  sampleRate      : double;
begin
  result := false;

  Pa_Initialize();

  paApiIndex := TAudioCore_Portaudio.GetPreferredApiIndex();
  if(paApiIndex = -1) then
  begin
    Log.LogError('No working Audio-API found', 'TAudioPlayback_Portaudio.InitializeAudioPlaybackEngine');
    Exit;
  end;

  paApiInfo := Pa_GetHostApiInfo(paApiIndex);
  paOutDevice     := paApiInfo^.defaultOutputDevice;
  paOutDeviceInfo := Pa_GetDeviceInfo(paOutDevice);

  sampleRate := paOutDeviceInfo^.defaultSampleRate;

  with paOutParams do begin
    device := paOutDevice;
    channelCount := 2;
    sampleFormat := paInt16;
    suggestedLatency := paOutDeviceInfo^.defaultLowOutputLatency;
    hostApiSpecificStreamInfo := nil;
  end;

  // check souncard and adjust sample-rate
  if not TAudioCore_Portaudio.TestDevice(nil, @paOutParams, sampleRate) then
  begin
    Log.LogStatus('TestDevice failed!', 'TAudioPlayback_Portaudio.OpenDevice');
    exit;
  end;

  // open output stream
  err := Pa_OpenStream(paStream, nil, @paOutParams, sampleRate,
          paFramesPerBufferUnspecified,
          paNoFlag, @PortaudioAudioCallback, Self);
  if(err <> paNoError) then
  begin
    Log.LogStatus(Pa_GetErrorText(err), 'TAudioPlayback_Portaudio.OpenDevice');
    paStream := nil;
    exit;
  end;
  
  FormatInfo := TAudioFormatInfo.Create(
    paOutParams.channelCount,
    sampleRate,
    asfS16 // FIXME: is paInt16 system-dependant or -independant?
  );

  Log.LogStatus('Opened audio device', 'UAudioPlayback_Portaudio');

  result := true;
end;

function TAudioPlayback_Portaudio.StartAudioPlaybackEngine(): boolean;
var
  err: TPaError;
begin
  result := false;

  if (paStream = nil) then
    Exit;

  err := Pa_StartStream(paStream);
  if(err <> paNoError) then
  begin
    Log.LogStatus('Pa_StartStream: '+Pa_GetErrorText(err), 'UAudioPlayback_Portaudio');
    exit;
  end;

  result := true;
end;

procedure TAudioPlayback_Portaudio.StopAudioPlaybackEngine();
begin
  if (paStream <> nil) then
    Pa_StopStream(paStream);
end;



initialization
  singleton_AudioPlaybackPortaudio := TAudioPlayback_Portaudio.create();
  AudioManager.add( singleton_AudioPlaybackPortaudio );

finalization
  AudioManager.Remove( singleton_AudioPlaybackPortaudio );


end.
