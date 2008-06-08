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
      AudioCore: TAudioCore_Portaudio;
      function OpenDevice(deviceIndex: TPaDeviceIndex): boolean;
      function EnumDevices(): boolean;
    protected
      function InitializeAudioPlaybackEngine(): boolean; override;
      function StartAudioPlaybackEngine(): boolean;      override;
      procedure StopAudioPlaybackEngine();               override;
      function FinalizeAudioPlaybackEngine(): boolean;   override;
    public
      function GetName: String;                          override;
  end;

  TPortaudioOutputDevice = class(TAudioOutputDevice)
    private
      PaDeviceIndex:  TPaDeviceIndex;
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

function TAudioPlayback_Portaudio.OpenDevice(deviceIndex: TPaDeviceIndex): boolean;
var
  deviceInfo : PPaDeviceInfo;
  sampleRate : double;
  outParams  : TPaStreamParameters;
  err        : TPaError;
begin
  Result := false;

  deviceInfo := Pa_GetDeviceInfo(deviceIndex);

  Log.LogInfo('Audio-Output Device: ' + deviceInfo^.name, 'TAudioPlayback_Portaudio.OpenDevice');

  sampleRate := deviceInfo^.defaultSampleRate;

  with outParams do
  begin
    device := deviceIndex;
    channelCount := 2;
    sampleFormat := paInt16;
    suggestedLatency := deviceInfo^.defaultLowOutputLatency;
    hostApiSpecificStreamInfo := nil;
  end;

  // check souncard and adjust sample-rate
  if not AudioCore.TestDevice(nil, @outParams, sampleRate) then
  begin
    Log.LogStatus('TestDevice failed!', 'TAudioPlayback_Portaudio.OpenDevice');
    exit;
  end;

  // open output stream
  err := Pa_OpenStream(paStream, nil, @outParams, sampleRate,
          paFramesPerBufferUnspecified,
          paNoFlag, @PortaudioAudioCallback, Self);
  if(err <> paNoError) then
  begin
    Log.LogStatus(Pa_GetErrorText(err), 'TAudioPlayback_Portaudio.OpenDevice');
    paStream := nil;
    exit;
  end;
  
  FormatInfo := TAudioFormatInfo.Create(
    outParams.channelCount,
    sampleRate,
    asfS16 // FIXME: is paInt16 system-dependant or -independant?
  );

  Result := true;
end;

function TAudioPlayback_Portaudio.EnumDevices(): boolean;
var
  i:           integer;
  paApiIndex:  TPaHostApiIndex;
  paApiInfo:   PPaHostApiInfo;
  deviceName:  string;
  deviceIndex: TPaDeviceIndex;
  deviceInfo:  PPaDeviceInfo;
  channelCnt:  integer;
  SC:          integer; // soundcard
  err:         TPaError;
  errMsg:      string;
  paDevice:    TPortaudioOutputDevice;
  inputParams: TPaStreamParameters;
  stream:      PPaStream;
  streamInfo:  PPaStreamInfo;
  sampleRate:  double;
  latency:     TPaTime;
  cbPolls: integer;
  cbWorks: boolean;
begin
  Result := false;
(*
  // choose the best available Audio-API
  paApiIndex := AudioCore.GetPreferredApiIndex();
  if(paApiIndex = -1) then
  begin
    Log.LogError('No working Audio-API found', 'TAudioPlayback_Portaudio.EnumDevices');
    Exit;
  end;

  paApiInfo := Pa_GetHostApiInfo(paApiIndex);

  SC := 0;

  // init array-size to max. output-devices count
  SetLength(OutputDeviceList, paApiInfo^.deviceCount);
  for i:= 0 to High(OutputDeviceList) do
  begin
    // convert API-specific device-index to global index
    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(paApiIndex, i);
    deviceInfo := Pa_GetDeviceInfo(deviceIndex);

    channelCnt := deviceInfo^.maxOutputChannels;

    // current device is no output device -> skip
    if (channelCnt <= 0) then
      continue;

    // portaudio returns a channel-count of 128 for some devices
    // (e.g. the "default"-device), so we have to detect those
    // fantasy channel counts.
    if (channelCnt > 8) then
      channelCnt := 2;

    paDevice := TPortaudioOutputDevice.Create();
    OutputDeviceList[SC] := paDevice;

    // retrieve device-name
    deviceName := deviceInfo^.name;
    paDevice.Name := deviceName;
    paDevice.PaDeviceIndex := deviceIndex;

    if (deviceInfo^.defaultSampleRate > 0) then
      sampleRate := deviceInfo^.defaultSampleRate
    else
      sampleRate := 44100;

    // on vista and xp the defaultLowInputLatency may be set to 0 but it works.
    // TODO: correct too low latencies (what is a too low latency, maybe < 10ms?)
    latency := deviceInfo^.defaultLowInputLatency;

    // setup desired input parameters
    // TODO: retry with input-latency set to 20ms (defaultLowInputLatency might
    //       not be set correctly in OSS)
    with inputParams do
    begin
      device := deviceIndex;
      channelCount := channelCnt;
      sampleFormat := paInt16;
      suggestedLatency := latency;
      hostApiSpecificStreamInfo := nil;
    end;

    // check if mic-callback works (might not be called on some devices)
    if (not TAudioCore_Portaudio.TestDevice(@inputParams, nil, sampleRate)) then
    begin
      // ignore device if callback did not work
      Log.LogError('Device "'+paDevice.Name+'" does not respond',
                   'TAudioInput_Portaudio.InitializeRecord');
      paDevice.Free();
      continue;
    end;

    // open device for further info
    err := Pa_OpenStream(stream, @inputParams, nil, sampleRate,
        paFramesPerBufferUnspecified, paNoFlag, @MicrophoneTestCallback, nil);
    if(err <> paNoError) then
    begin
      // unable to open device -> skip
      errMsg := Pa_GetErrorText(err);
      Log.LogError('Device error: "'+ deviceName +'" ('+ errMsg +')',
                   'TAudioInput_Portaudio.InitializeRecord');
      paDevice.Free();
      continue;
    end;

    // adjust sample-rate (might be changed by portaudio)
    streamInfo := Pa_GetStreamInfo(stream);
    if (streamInfo <> nil) then
    begin
      if (sampleRate <> streamInfo^.sampleRate) then
      begin
        Log.LogStatus('Portaudio changed Samplerate from ' + FloatToStr(sampleRate) +
            ' to ' + FloatToStr(streamInfo^.sampleRate),
            'TAudioInput_Portaudio.InitializeRecord');
        sampleRate := streamInfo^.sampleRate;
      end;
    end;
    
    // create audio-format info and resize capture-buffer array
    paDevice.AudioFormat := TAudioFormatInfo.Create(
        channelCnt,
        sampleRate,
        asfS16
    );
    SetLength(paDevice.CaptureChannel, paDevice.AudioFormat.Channels);

    Log.LogStatus('InputDevice "'+paDevice.Name+'"@' +
        IntToStr(paDevice.AudioFormat.Channels)+'x'+
        FloatToStr(paDevice.AudioFormat.SampleRate)+'Hz ('+
        FloatTostr(inputParams.suggestedLatency)+'sec)' ,
        'Portaudio.InitializeRecord');

    // close test-stream
    Pa_CloseStream(stream);

    Inc(SC);
  end;

  // adjust size to actual input-device count
  SetLength(OutputDeviceList, SC);

  Log.LogStatus('#Output-Devices: ' + inttostr(SC), 'Portaudio');

  Result := true;
  *)
end;

function TAudioPlayback_Portaudio.InitializeAudioPlaybackEngine(): boolean;
var
  paApiIndex      : TPaHostApiIndex;
  paApiInfo       : PPaHostApiInfo;
  paOutDevice     : TPaDeviceIndex;
  err: TPaError;
begin
  result := false;

  AudioCore := TAudioCore_Portaudio.GetInstance();

  // initialize portaudio
  err := Pa_Initialize();
  if(err <> paNoError) then
  begin
    Log.LogError(Pa_GetErrorText(err), 'TAudioInput_Portaudio.InitializeRecord');
    Exit;
  end;

  paApiIndex := AudioCore.GetPreferredApiIndex();
  if(paApiIndex = -1) then
  begin
    Log.LogError('No working Audio-API found', 'TAudioPlayback_Portaudio.InitializeAudioPlaybackEngine');
    Exit;
  end;

  EnumDevices();

  paApiInfo := Pa_GetHostApiInfo(paApiIndex);
  Log.LogInfo('Audio-Output API-Type: ' + paApiInfo^.name, 'TAudioPlayback_Portaudio.OpenDevice');

  paOutDevice := paApiInfo^.defaultOutputDevice;
  if (not OpenDevice(paOutDevice)) then
  begin
    Exit;
  end;

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

function TAudioPlayback_Portaudio.FinalizeAudioPlaybackEngine(): boolean;
begin
  Pa_Terminate();
  Result := true;
end;


initialization
  singleton_AudioPlaybackPortaudio := TAudioPlayback_Portaudio.create();
  AudioManager.add( singleton_AudioPlaybackPortaudio );

finalization
  AudioManager.Remove( singleton_AudioPlaybackPortaudio );


end.
