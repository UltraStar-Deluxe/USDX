unit UAudioInput_Portaudio;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I ../switches.inc}


uses
  Classes,
  SysUtils,
  UMusic;

implementation

uses
  {$IFDEF UsePortmixer}
  portmixer,
  {$ENDIF}
  portaudio,
  UAudioCore_Portaudio,
  URecord,
  UIni,
  ULog,
  UMain;

type
  TAudioInput_Portaudio = class(TAudioInputBase)
    public
      function GetName: String; override;
      function InitializeRecord: boolean; override;
      destructor Destroy; override;
  end;

  TPortaudioInputDevice = class(TAudioInputDevice)
    public
      RecordStream:   PPaStream;
      PaDeviceIndex:  TPaDeviceIndex;

      function Start(): boolean; override;
      procedure Stop();  override;
  end;

function MicrophoneCallback(input: Pointer; output: Pointer; frameCount: Longword;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      inputDevice: Pointer): Integer; cdecl; forward;

function MicrophoneTestCallback(input: Pointer; output: Pointer; frameCount: Longword;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      inputDevice: Pointer): Integer; cdecl; forward;

var
  singleton_AudioInputPortaudio : IAudioInput;


{ TPortaudioInputDevice }

function TPortaudioInputDevice.Start(): boolean;
var
  Error:       TPaError;
  ErrorMsg:    string;
  inputParams: TPaStreamParameters;
  deviceInfo:  PPaDeviceInfo;
begin
  Result := false;

  // get input latency info
  deviceInfo := Pa_GetDeviceInfo(PaDeviceIndex);

  // set input stream parameters
  with inputParams do
  begin
    device := PaDeviceIndex;
    channelCount := AudioFormat.Channels;
    sampleFormat := paInt16;
    suggestedLatency := deviceInfo^.defaultLowInputLatency;
    hostApiSpecificStreamInfo := nil;
  end;

  //Log.LogStatus(deviceInfo^.name, 'Portaudio');
  //Log.LogStatus(floattostr(deviceInfo^.defaultLowInputLatency), 'Portaudio');

  // open input stream
  Error := Pa_OpenStream(RecordStream, @inputParams, nil,
      AudioFormat.SampleRate,
      paFramesPerBufferUnspecified, paNoFlag,
      @MicrophoneCallback, Pointer(Self));
  if(Error <> paNoError) then
  begin
    ErrorMsg := Pa_GetErrorText(Error);
    Log.LogError('Error opening stream: ' + ErrorMsg, 'TPortaudioInputDevice.Start');
    Exit;
  end;

  // start capture
  Error := Pa_StartStream(RecordStream);
  if(Error <> paNoError) then
  begin
    Pa_CloseStream(RecordStream);
    ErrorMsg := Pa_GetErrorText(Error);
    Log.LogError('Error starting stream: ' + ErrorMsg, 'TPortaudioInputDevice.Start');
    Exit;
  end;
  
  Result := true;
end;

procedure TPortaudioInputDevice.Stop();
begin
  if assigned(RecordStream) then
  begin
    // Note: do NOT call Pa_StopStream here!
    // It gets stuck on devices with non-working callback as Pa_StopStream
    // waits until all buffers have been handled (which never occurs in that
    // case).
    // Pa_CloseStream internally calls Pa_AbortStream which works as expected.
    Pa_CloseStream(RecordStream);
  end;
end;


{ TAudioInput_Portaudio }

function TAudioInput_Portaudio.GetName: String;
begin
  result := 'Portaudio';
end;

function TAudioInput_Portaudio.InitializeRecord(): boolean;
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
  paDevice:    TPortaudioInputDevice;
  inputParams: TPaStreamParameters;
  stream:      PPaStream;
  streamInfo:  PPaStreamInfo;
  sampleRate:  integer;
  latency:     TPaTime;
  {$IFDEF UsePortmixer}
  sourceIndex: integer;
  mixer:       PPxMixer;
  sourceCnt:   integer;
  sourceName:  string;
  {$ENDIF}
  cbPolls: integer;
  cbWorks: boolean;
begin
  result := false;

  // initialize portaudio
  err := Pa_Initialize();
  if(err <> paNoError) then
  begin
    Log.LogError(Pa_GetErrorText(err), 'TAudioInput_Portaudio.InitializeRecord');
    Exit;
  end;

  // choose the best available Audio-API
  paApiIndex := TAudioCore_Portaudio.GetPreferredApiIndex();
  if(paApiIndex = -1) then
  begin
    Log.LogError('No working Audio-API found', 'TAudioInput_Portaudio.InitializeRecord');
    Exit;
  end;

  paApiInfo := Pa_GetHostApiInfo(paApiIndex);

  SC := 0;

  // init array-size to max. input-devices count
  SetLength(AudioInputProcessor.Device, paApiInfo^.deviceCount);
  for i:= 0 to High(AudioInputProcessor.Device) do
  begin
    // convert API-specific device-index to global index
    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(paApiIndex, i);
    deviceInfo := Pa_GetDeviceInfo(deviceIndex);

    channelCnt := deviceInfo^.maxInputChannels;

    // current device is no input device -> skip
    if (channelCnt <= 0) then
      continue;

    // portaudio returns a channel-count of 128 for some devices
    // (e.g. the "default"-device), so we have to detect those
    // fantasy channel counts.
    if (channelCnt > 8) then
      channelCnt := 2;

    paDevice := TPortaudioInputDevice.Create();
    AudioInputProcessor.Device[SC] := paDevice;

    // retrieve device-name
    deviceName := deviceInfo^.name;
    paDevice.Description := deviceName;
    paDevice.PaDeviceIndex := deviceIndex;

    if (deviceInfo^.defaultSampleRate > 0) then
      sampleRate := Trunc(deviceInfo^.defaultSampleRate)
    else
      sampleRate := 44100;

    // on vista and xp the defaultLowInputLatency may be set to 0 but it works.
    // TODO: correct too low latencies (what is a too low latency, maybe < 10ms?)
    latency := deviceInfo^.defaultLowInputLatency;

    // setup desired input parameters
    with inputParams do
    begin
      device := deviceIndex;
      channelCount := channelCnt;
      sampleFormat := paInt16;
      suggestedLatency := latency;
      hostApiSpecificStreamInfo := nil;
    end;

    // check if device supports our input-format
    // TODO: retry with input-latency set to 20ms (defaultLowInputLatency might
    //       not be set correctly in OSS)
    err := Pa_IsFormatSupported(@inputParams, nil, sampleRate);
    if(err <> 0) then
    begin
      // format not supported -> skip
      errMsg := Pa_GetErrorText(err);
      Log.LogError('Device error: "'+ deviceName +'" ('+ errMsg +')',
                   'TAudioInput_Portaudio.InitializeRecord');
      paDevice.Free();
      continue;
    end;

    // check if the device really works
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


    // check if mic-callback works (might not be called on some devices)

    // start the callback
    Pa_StartStream(stream);
    
    cbWorks := false;
    // check if the callback was called (poll for max. 200ms)
    for cbPolls := 1 to 20 do
    begin
      // if the test-callback was called it should be aborted now
      if (Pa_IsStreamActive(stream) = 0) then
      begin
        cbWorks := true;
        break;
      end;
      // not yet aborted, wait and try (poll) again
      Pa_Sleep(10);
    end;

    // finally abort the stream (Note: Pa_StopStream might hang here)
    Pa_AbortStream(stream);

    // ignore device if callback did not work
    if (not cbWorks) then
    begin
      Log.LogError('Device "'+paDevice.Description+'" does not respond',
                   'TAudioInput_Portaudio.InitializeRecord');
      Pa_CloseStream(stream);
      paDevice.Free();
      continue;
    end;

    // adjust sample-rate (might be changed by portaudio)
    streamInfo := Pa_GetStreamInfo(stream);
    if (streamInfo <> nil) then
    begin
      if (sampleRate <> Trunc(streamInfo^.sampleRate)) then
      begin
        Log.LogStatus('Portaudio changed Samplerate from ' + IntToStr(sampleRate) +
            ' to ' + FloatToStr(streamInfo^.sampleRate),
            'TAudioInput_Portaudio.InitializeRecord');
        sampleRate := Trunc(streamInfo^.sampleRate);
      end;
    end;
    
    // create audio-format info and resize capture-buffer array
    paDevice.AudioFormat := TAudioFormatInfo.Create(
        channelCnt,
        sampleRate,
        asfS16
    );
    SetLength(paDevice.CaptureChannel, paDevice.AudioFormat.Channels);

    Log.LogStatus('InputDevice "'+paDevice.Description+'"@' +
        IntToStr(paDevice.AudioFormat.Channels)+'x'+
        IntToStr(paDevice.AudioFormat.SampleRate)+'Hz ('+
        FloatTostr(inputParams.suggestedLatency)+'sec)' ,
        'Portaudio.InitializeRecord');

    {$IFDEF UsePortmixer}
      // use default mixer
      mixer := Px_OpenMixer(stream, 0);

      // get input count
      sourceCnt := Px_GetNumInputSources(mixer);
      SetLength(paDevice.Source, sourceCnt);

      // get input names
      for sourceIndex := 0 to sourceCnt-1 do
      begin
        sourceName := Px_GetInputSourceName(mixer, sourceIndex);
        paDevice.Source[sourceIndex].Name := sourceName;
      end;

      Px_CloseMixer(mixer);
    {$ELSE} // not UsePortmixer
      // create a standard input source
      SetLength(paDevice.Source, 1);
      paDevice.Source[0].Name := 'Standard';
    {$ENDIF}

    // close test-stream
    Pa_CloseStream(stream);

    // use default input source
    paDevice.SourceSelected := 0;

    Inc(SC);
  end;

  // adjust size to actual input-device count
  SetLength(AudioInputProcessor.Device, SC);

  Log.LogStatus('#Soundcards: ' + inttostr(SC), 'Portaudio');

  result := true;
end;

destructor TAudioInput_Portaudio.Destroy;
var
  i: integer;
  //paSoundCard: TPortaudioInputDevice;
begin
  Pa_Terminate();
  for i := 0 to High(AudioInputProcessor.Device) do
  begin
    AudioInputProcessor.Device[i].Free();
  end;
  AudioInputProcessor.Device := nil;

  inherited Destroy;
end;

{*
 * Portaudio input capture callback.
 *}
function MicrophoneCallback(input: Pointer; output: Pointer; frameCount: Longword;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      inputDevice: Pointer): Integer; cdecl;
begin
  AudioInputProcessor.HandleMicrophoneData(input, frameCount*4, inputDevice);
  result := paContinue;
end;

{*
 * Portaudio test capture callback.
 *}
function MicrophoneTestCallback(input: Pointer; output: Pointer; frameCount: Longword;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      inputDevice: Pointer): Integer; cdecl;
begin
  // this callback is called only once
  result := paAbort;
end;


initialization
  singleton_AudioInputPortaudio := TAudioInput_Portaudio.create();
  AudioManager.add( singleton_AudioInputPortaudio );

finalization
  AudioManager.Remove( singleton_AudioInputPortaudio );

end.
