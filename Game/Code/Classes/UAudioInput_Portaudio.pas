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
  URecord,
  UIni,
  ULog,
  UMain,
  {$IFDEF UsePortmixer}
  portmixer,
  {$ENDIF}
  portaudio;

type
  TAudioInput_Portaudio = class(TAudioInputBase)
    private
      function GetPreferredApiIndex(): TPaHostApiIndex;
    public
      function GetName: String; override;
      function InitializeRecord: boolean; override;
      destructor Destroy; override;
  end;

  TPortaudioInputDevice = class(TAudioInputDevice)
    public
      RecordStream:   PPaStream;
      PaDeviceIndex:  TPaDeviceIndex;

      procedure Start(); override;
      procedure Stop();  override;
  end;

function MicrophoneCallback(input: Pointer; output: Pointer; frameCount: Longword;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      inputDevice: Pointer): Integer; cdecl; forward;

var
  singleton_AudioInputPortaudio : IAudioInput;

{* the default API used by Portaudio is the least common denominator
 * and might lack efficiency. ApiPreferenceOrder defines the order of
 * preferred APIs to use. The first API-type in the list is tried first. If it's
 * not available the next is tried, ...
 * If none of the preferred APIs was found the default API is used.
 * Pascal doesn't permit zero-length static arrays, so you can use paDefaultApi
 * as an array's only member if you do not have any preferences.
 * paDefaultApi also terminates a preferences list but this is optional.
 *}
const
  paDefaultApi = -1;
const
  ApiPreferenceOrder:
{$IF Defined(WIN32)}
    // Note1: Portmixer has no mixer support for paASIO and paWASAPI at the moment
    // Note2: Windows Default-API is MME
    //array[0..0] of TPaHostApiTypeId = ( paDirectSound, paMME );
    array[0..0] of TPaHostApiTypeId = ( paDirectSound );
{$ELSEIF Defined(LINUX)}
    // Note1: Portmixer has no mixer support for paJACK at the moment
    // Note2: Not tested, but ALSA might be better than OSS.
    array[0..1] of TPaHostApiTypeId = ( paALSA, paOSS );
{$ELSEIF Defined(DARWIN)}
    // Note: Not tested.
    //array[0..0] of TPaHostApiTypeId = ( paCoreAudio );
    array[0..0] of TPaHostApiTypeId = ( paDefaultApi );
{$ELSE}
    array[0..0] of TPaHostApiTypeId = ( paDefaultApi );
{$IFEND}


{ TPortaudioInputDevice }

procedure TPortaudioInputDevice.Start();
var
  Error:       TPaError;
  ErrorMsg:    string;
  inputParams: TPaStreamParameters;
  deviceInfo:  PPaDeviceInfo;
begin
  // get input latency info
  deviceInfo := Pa_GetDeviceInfo(PaDeviceIndex);

  // set input stream parameters
  with inputParams do begin
    device := PaDeviceIndex;
    channelCount := 2;
    sampleFormat := paInt16;
    suggestedLatency := deviceInfo^.defaultLowInputLatency;
    hostApiSpecificStreamInfo := nil;
  end;

  Log.LogStatus(inttostr(PaDeviceIndex), 'Portaudio');
  Log.LogStatus(floattostr(deviceInfo^.defaultLowInputLatency), 'Portaudio');

  // open input stream
  Error := Pa_OpenStream(RecordStream, @inputParams, nil, SampleRate,
      paFramesPerBufferUnspecified, paNoFlag,
      @MicrophoneCallback, Pointer(Self));
  if(Error <> paNoError) then begin
    ErrorMsg := Pa_GetErrorText(Error);
    Log.CriticalError('TPortaudioInputDevice.Start(): Error opening stream: ' + ErrorMsg);
    //Halt;
  end;

  // start capture
  Error := Pa_StartStream(RecordStream);
  if(Error <> paNoError) then begin
    Pa_CloseStream(RecordStream);
    ErrorMsg := Pa_GetErrorText(Error);
    Log.CriticalError('TPortaudioInputDevice.Start(): Error starting stream: ' + ErrorMsg);
    //Halt;
  end;
end;

procedure TPortaudioInputDevice.Stop();
begin
  if assigned(RecordStream) then begin
    Pa_StopStream(RecordStream);
    Pa_CloseStream(RecordStream);
  end;
end;


{ TAudioInput_Portaudio }

function TAudioInput_Portaudio.GetName: String;
begin
  result := 'Portaudio';
end;

function TAudioInput_Portaudio.GetPreferredApiIndex(): TPaHostApiIndex;
var
  i: integer;
begin
  result := -1;

  // select preferred sound-API
  for i:= 0 to High(ApiPreferenceOrder) do
  begin
    if(ApiPreferenceOrder[i] <> paDefaultApi) then begin
      // check if API is available
      result := Pa_HostApiTypeIdToHostApiIndex(ApiPreferenceOrder[i]);
      if(result >= 0) then
        break;
    end;
  end;

  // None of the preferred APIs is available -> use default
  if(result < 0) then begin
    result := Pa_GetDefaultHostApi();
  end;
end;

function TAudioInput_Portaudio.InitializeRecord(): boolean;
var
  i:           integer;
  apiIndex:    TPaHostApiIndex;
  apiInfo:     PPaHostApiInfo;
  deviceName:  string;
  deviceIndex: TPaDeviceIndex;
  deviceInfo:  PPaDeviceInfo;
  sourceCnt:   integer;
  sourceName:  string;
  SC:          integer; // soundcard
  SCI:         integer; // soundcard source
  err:         TPaError;
  errMsg:      string;
  paDevice:    TPortaudioInputDevice;
  inputParams: TPaStreamParameters;
  stream:      PPaStream;
  {$IFDEF UsePortmixer}
  mixer:       PPxMixer;
  {$ENDIF}
const
  captureFreq = 44100;
begin
  result := false;

  err := Pa_Initialize();
  if(err <> paNoError) then begin
    Log.LogError('Portaudio.InitializeRecord: ' + Pa_GetErrorText(err));
    Exit;
  end;
  apiIndex := GetPreferredApiIndex();
  apiInfo := Pa_GetHostApiInfo(apiIndex);

  SC := 0;

  // init array-size to max. input-devices count
  SetLength(AudioInputProcessor.Device, apiInfo^.deviceCount);
  for i:= 0 to High(AudioInputProcessor.Device) do
  begin
    // convert API-specific device-index to global index
    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(apiIndex, i);
    deviceInfo := Pa_GetDeviceInfo(deviceIndex);

    // current device is no input device -> skip
    if(deviceInfo^.maxInputChannels <= 0) then
      continue;

    paDevice := TPortaudioInputDevice.Create();
    AudioInputProcessor.Device[SC] := paDevice;
    
    // retrieve device-name
    deviceName := deviceInfo^.name;
    paDevice.Description := deviceName;
    paDevice.PaDeviceIndex := deviceIndex;

    // setup desired input parameters
    with inputParams do begin
      device := deviceIndex;
      channelCount := 2;
      sampleFormat := paInt16;
      suggestedLatency := deviceInfo^.defaultLowInputLatency;
      hostApiSpecificStreamInfo := nil;
    end;

    paDevice.SampleRate := captureFreq;

    // check if device supports our input-format
    err := Pa_IsFormatSupported(@inputParams, nil, paDevice.SampleRate);
    if(err <> 0) then begin
      // format not supported -> skip
      errMsg := Pa_GetErrorText(err);
      Log.LogError('Portaudio.InitializeRecord, device: "'+ deviceName +'" '
                 + '('+ errMsg +')');
      paDevice.Free();
      continue;
    end;

    // TODO: retry with mono if stereo is not supported
    // TODO: retry with input-latency set to 20ms (defaultLowInputLatency might
    //       not be set correctly in OSS)
    // use TPaDeviceInfo.defaultSampleRate

    err := Pa_OpenStream(stream, @inputParams, nil, paDevice.SampleRate,
        paFramesPerBufferUnspecified, paNoFlag, @MicrophoneCallback, nil);
    if(err <> paNoError) then begin
      // unable to open device -> skip
      errMsg := Pa_GetErrorText(err);
      Log.LogError('Portaudio.InitializeRecord, device: "'+ deviceName +'" '
                 + '('+ errMsg +')');
      paDevice.Free();
      continue;
    end;


    {$IFDEF UsePortmixer}

    // use default mixer
    mixer := Px_OpenMixer(stream, 0);

    // get input count
    sourceCnt := Px_GetNumInputSources(mixer);
    SetLength(paDevice.Source, sourceCnt);

    // get input names
    for SCI := 0 to sourceCnt-1 do
    begin
      sourceName := Px_GetInputSourceName(mixer, SCI);
      paDevice.Source[SCI].Name := sourceName;
    end;

    Px_CloseMixer(mixer);

    {$ELSE} // !UsePortmixer

    //Pa_StartStream(stream);
    // TODO: check if callback was called (this problem may occur on some devices)
    //Pa_StopStream(stream);

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

  {
    SoundCard[SC].InputSelected := Mic[Device];
  }
  result := true;
end;

destructor TAudioInput_Portaudio.Destroy;
var
  i: integer;
  paSoundCard: TPortaudioInputDevice;
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


initialization
  singleton_AudioInputPortaudio := TAudioInput_Portaudio.create();
  AudioManager.add( singleton_AudioInputPortaudio );

finalization
  AudioManager.Remove( singleton_AudioInputPortaudio );

end.
