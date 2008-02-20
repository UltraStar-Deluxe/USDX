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
  paApi           : TPaHostApiIndex;
  paApiInfo       : PPaHostApiInfo;
  paOutParams     : TPaStreamParameters;
  paOutDevice     : TPaDeviceIndex;
  paOutDeviceInfo : PPaDeviceInfo;
  err             : TPaError;
const
  sampleFreq = 44100;
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

  err := Pa_OpenStream(paStream, nil, @paOutParams, sampleFreq,
          paFramesPerBufferUnspecified,
          paNoFlag, @PortaudioAudioCallback, Self);
  if(err <> paNoError) then begin
    Log.LogStatus('Pa_OpenStream: '+Pa_GetErrorText(err), 'UAudioPlayback_Portaudio');
    exit;
  end;

  FormatInfo := TAudioFormatInfo.Create(
    paOutParams.channelCount,
    sampleFreq,
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
