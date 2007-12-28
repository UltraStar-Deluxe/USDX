unit UAudioInput_Portaudio;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}


uses Classes,
     SysUtils,
     portaudio,
     {$IFDEF UsePortmixer}
     portmixer,
     {$ENDIF}
     ULog,
     UMusic;

implementation

uses
     {$IFDEF LAZARUS}
     lclintf,
     {$ENDIF}
     URecord,
     UIni,
     UMain,
     UCommon,
     UThemes;
{
type
  TPaHostApiIndex = PaHostApiIndex;
  TPaDeviceIndex = PaDeviceIndex;
  PPaStream = ^PaStreamPtr;
  PPaStreamCallbackTimeInfo = ^PaStreamCallbackTimeInfo;
  TPaStreamCallbackFlags = PaStreamCallbackFlags;
  TPaHostApiTypeId = PaHostApiTypeId;
  PPaHostApiInfo = ^PaHostApiInfo;
  PPaDeviceInfo = ^PaDeviceInfo;
  TPaError = PaError;
  TPaStreamParameters = PaStreamParameters;
}
type
  TAudioInput_Portaudio = class( TInterfacedObject, IAudioInput )
    private
      function GetPreferredApiIndex(): TPaHostApiIndex;
    public
      function  GetName: String;
      procedure InitializeRecord;

      procedure CaptureStart;
      procedure CaptureStop;

      procedure CaptureCard(Card: byte; CaptureSoundLeft, CaptureSoundRight: TSound);
      procedure StopCard(Card: byte);
  end;

  TPortaudioSoundCard = class(TGenericSoundCard)
    RecordStream:   PPaStream;
    DeviceIndex:    TPaDeviceIndex;
  end;

function MicrophoneCallback(input: Pointer; output: Pointer; frameCount: Longword;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      inputDevice: Pointer): Integer; cdecl; forward;

var
  singleton_AudioInputPortaudio : IAudioInput;

const
  sampleRate:  Double = 44100.;

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
var
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

// TODO: should be a function with boolean return type
procedure TAudioInput_Portaudio.InitializeRecord;
var
  i:           integer;
  apiIndex:    TPaHostApiIndex;
  apiInfo:     PPaHostApiInfo;
  deviceName:  string;
  deviceIndex: TPaDeviceIndex;
  deviceInfo:  PPaDeviceInfo;
  inputCnt:    integer;
  inputName:   string;
  SC:          integer; // soundcard
  SCI:         integer; // soundcard input
  err:         TPaError;
  errMsg:      string;
  paSoundCard: TPortaudioSoundCard;
  inputParams: TPaStreamParameters;
  stream:      PPaStream;
  {$IFDEF UsePortmixer}
  mixer:       PPxMixer;
  {$ENDIF}
begin
  // TODO: call Pa_Terminate() on termination
  err := Pa_Initialize();
  if(err <> paNoError) then begin
    Log.CriticalError('Portaudio.InitializeRecord: ' + Pa_GetErrorText(err));
    //Log.LogError('Portaudio.InitializeRecord: ' + Pa_GetErrorText(err));
    // result := false;
    Exit;
  end;

  apiIndex := GetPreferredApiIndex();
  apiInfo := Pa_GetHostApiInfo(apiIndex);

  SC := 0;

  // init array-size to max. input-devices count
  SetLength(Recording.SoundCard, apiInfo^.deviceCount); // fix deviceCountL
  for i:= 0 to High(Recording.SoundCard) do
  begin
    // convert API-specific device-index to global index
    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(apiIndex, i);
    deviceInfo := Pa_GetDeviceInfo(deviceIndex);

    // current device is no input device -> skip
    if(deviceInfo^.maxInputChannels <= 0) then
      continue;

    // TODO: free object on termination
    paSoundCard := TPortaudioSoundCard.Create();
    Recording.SoundCard[SC] := paSoundCard;
    
    // retrieve device-name
    deviceName := deviceInfo^.name;
    paSoundCard.Description := deviceName;
    paSoundCard.DeviceIndex := deviceIndex;

    // setup desired input parameters
    with inputParams do begin
      device := deviceIndex;
      channelCount := 2;
      sampleFormat := paInt16;
      suggestedLatency := deviceInfo^.defaultLowInputLatency;
      hostApiSpecificStreamInfo := nil;
    end;

    // check if device supports our input-format
    err := Pa_IsFormatSupported(@inputParams, nil, sampleRate);
    if(err <> 0) then begin
      // format not supported -> skip
      errMsg := Pa_GetErrorText(err);
      Log.LogError('Portaudio.InitializeRecord, device: "'+ deviceName +'" '
                 + '('+ errMsg +')');
      paSoundCard.Free();
      continue;
    end;

    // TODO: retry with mono if stereo is not supported
    // TODO: retry with input-latency set to 20ms (defaultLowInputLatency might
    //       not be set correctly in OSS)

    err := Pa_OpenStream(stream, @inputParams, nil, sampleRate,
        paFramesPerBufferUnspecified, paNoFlag, @MicrophoneCallback, nil);
    if(err <> paNoError) then begin
      // unable to open device -> skip
      errMsg := Pa_GetErrorText(err);
      Log.LogError('Portaudio.InitializeRecord, device: "'+ deviceName +'" '
                 + '('+ errMsg +')');
      paSoundCard.Free();
      continue;
    end;


    {$IFDEF UsePortmixer}

    // use default mixer
    mixer := Px_OpenMixer(stream, 0);

    // get input count
    inputCnt := Px_GetNumInputSources(mixer);
    SetLength(paSoundCard.Input, inputCnt);

    // get input names
    for SCI := 0 to inputCnt-1 do
    begin
      inputName := Px_GetInputSourceName(mixer, SCI);
      paSoundCard.Input[SCI].Name := inputName;
    end;

    Px_CloseMixer(mixer);

    {$ELSE} // !UsePortmixer

    //Pa_StartStream(stream);
    // TODO: check if callback was called (this problem may occur on some devices)
    //Pa_StopStream(stream);

    Pa_CloseStream(stream);

    // create a standard input source
    SetLength(paSoundCard.Input, 1);
    paSoundCard.Input[0].Name := 'Standard';

    {$ENDIF}

    // use default input source
    paSoundCard.InputSelected := 0;

    Inc(SC);
  end;

  // adjust size to actual input-device count
  SetLength(Recording.SoundCard, SC);

  Log.LogStatus('#Soundcards: ' + inttostr(SC), 'Portaudio');

  {
    SoundCard[SC].InputSelected := Mic[Device];
  }
end;

// TODO: code is used by all IAudioInput implementors
//   -> move to a common superclass (TAudioInput_Generic?)
procedure TAudioInput_Portaudio.CaptureStart;
var
  S:  integer;
  SC: integer;
  PlayerLeft, PlayerRight: integer;
  CaptureSoundLeft, CaptureSoundRight: TSound;
begin
  for S := 0 to High(Recording.Sound) do
    Recording.Sound[S].BufferLong[0].Clear;

  for SC := 0 to High(Ini.CardList) do begin
    PlayerLeft  := Ini.CardList[SC].ChannelL-1;
    PlayerRight := Ini.CardList[SC].ChannelR-1;
    if PlayerLeft  >= PlayersPlay then PlayerLeft  := -1;
    if PlayerRight >= PlayersPlay then PlayerRight := -1;
    if (PlayerLeft > -1) or (PlayerRight > -1) then begin
      if (PlayerLeft > -1) then
        CaptureSoundLeft := Recording.Sound[PlayerLeft]
      else
        CaptureSoundLeft := nil;
      if (PlayerRight > -1) then
        CaptureSoundRight := Recording.Sound[PlayerRight]
      else
        CaptureSoundRight := nil;

      CaptureCard(SC, CaptureSoundLeft, CaptureSoundRight);
    end;
  end;
end;

// TODO: code is used by all IAudioInput implementors
//   -> move to a common superclass (TAudioInput_Generic?)
procedure TAudioInput_Portaudio.CaptureStop;
var
  SC:   integer;
  PlayerLeft:  integer;
  PlayerRight: integer;
begin

  for SC := 0 to High(Ini.CardList) do begin
    PlayerLeft  := Ini.CardList[SC].ChannelL-1;
    PlayerRight := Ini.CardList[SC].ChannelR-1;
    if PlayerLeft  >= PlayersPlay then PlayerLeft  := -1;
    if PlayerRight >= PlayersPlay then PlayerRight := -1;
    if (PlayerLeft > -1) or (PlayerRight > -1) then
      StopCard(SC);
  end;

end;

{*
 * Portaudio input capture callback.
 *}
function MicrophoneCallback(input: Pointer; output: Pointer; frameCount: Longword;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      inputDevice: Pointer): Integer; cdecl;
begin
  Recording.HandleMicrophoneData(input, frameCount*4, inputDevice);
  result := paContinue;
end;

{*
 * Start input-capturing on Soundcard specified by Card.
 * Params:
 *   Card - soundcard index in Recording.SoundCard array
 *   CaptureSoundLeft  - sound(-buffer) used for left channel capture data
 *   CaptureSoundRight - sound(-buffer) used for right channel capture data
 *}
procedure TAudioInput_Portaudio.CaptureCard(Card: byte; CaptureSoundLeft, CaptureSoundRight: TSound);
var
  Error:       TPaError;
  ErrorMsg:    string;
  inputParams: TPaStreamParameters;
  deviceInfo:  PPaDeviceInfo;
  stream:      PPaStream;
  paSoundCard: TPortaudioSoundCard;
begin
  paSoundCard := TPortaudioSoundCard(Recording.SoundCard[Card]);
  paSoundCard.CaptureSoundLeft  := CaptureSoundLeft;
  paSoundCard.CaptureSoundRight := CaptureSoundRight;

  // get input latency info
  deviceInfo := Pa_GetDeviceInfo(paSoundCard.DeviceIndex);

  // set input stream parameters
  with inputParams do begin
    device := paSoundCard.DeviceIndex;
    channelCount := 2;
    sampleFormat := paInt16;
    suggestedLatency := deviceInfo^.defaultLowInputLatency;
    hostApiSpecificStreamInfo := nil;
  end;

  Log.LogStatus(inttostr(paSoundCard.DeviceIndex), 'Portaudio');
  Log.LogStatus(floattostr(deviceInfo^.defaultLowInputLatency), 'Portaudio');

  // open input stream
  Error := Pa_OpenStream(stream, @inputParams, nil, sampleRate,
      paFramesPerBufferUnspecified, paNoFlag,
      @MicrophoneCallback, Pointer(paSoundCard));
  if(Error <> paNoError) then begin
    ErrorMsg := Pa_GetErrorText(Error);
    Log.CriticalError('TAudio_Portaudio.CaptureCard('+ IntToStr(Card) +'): Error opening stream: ' + ErrorMsg);
    //Halt;
  end;
  
  paSoundCard.RecordStream := stream;

  // start capture
  Error := Pa_StartStream(stream);
  if(Error <> paNoError) then begin
    Pa_CloseStream(stream);
    ErrorMsg := Pa_GetErrorText(Error);
    Log.CriticalError('TAudio_Portaudio.CaptureCard('+ IntToStr(Card) +'): Error starting stream: ' + ErrorMsg);
    //Halt;
  end;
end;

{*
 * Stop input-capturing on Soundcard specified by Card.
 * Params:
 *   Card - soundcard index in Recording.SoundCard array
 *}
procedure TAudioInput_Portaudio.StopCard(Card: byte);
var
  stream:      PPaStream;
  paSoundCard: TPortaudioSoundCard;
begin
  paSoundCard := TPortaudioSoundCard(Recording.SoundCard[Card]);
  stream := paSoundCard.RecordStream;
  if(stream <> nil) then begin
    Pa_StopStream(stream);
    Pa_CloseStream(stream);
  end;
end;


initialization
  singleton_AudioInputPortaudio := TAudioInput_Portaudio.create();
  AudioManager.add( singleton_AudioInputPortaudio );

finalization
  AudioManager.Remove( singleton_AudioInputPortaudio );

end.
