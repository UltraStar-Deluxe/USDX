{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *}

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
    private
      AudioCore: TAudioCore_Portaudio;
      function EnumDevices(): boolean;
    public
      function GetName: String; override;
      function InitializeRecord: boolean; override;
      function FinalizeRecord: boolean; override;
  end;

  TPortaudioInputDevice = class(TAudioInputDevice)
    private
      RecordStream: PPaStream;
      {$IFDEF UsePortmixer}
      Mixer: PPxMixer;
      {$ENDIF}
      PaDeviceIndex:  TPaDeviceIndex;
    public
      function Open(): boolean;
      function Close(): boolean;
      function Start(): boolean; override;
      function Stop(): boolean;  override;

      function GetVolume(): single;        override;
      procedure SetVolume(Volume: single); override;
  end;

function MicrophoneCallback(input: Pointer; output: Pointer; frameCount: Longword;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      inputDevice: Pointer): Integer; cdecl; forward;

function MicrophoneTestCallback(input: Pointer; output: Pointer; frameCount: Longword;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      inputDevice: Pointer): Integer; cdecl; forward;


{ TPortaudioInputDevice }

function TPortaudioInputDevice.Open(): boolean;
var
  Error:       TPaError;
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
    Log.LogError('Error opening stream: ' + Pa_GetErrorText(Error), 'TPortaudioInputDevice.Open');
    Exit;
  end;

  {$IFDEF UsePortmixer}
    // open default mixer
    Mixer := Px_OpenMixer(RecordStream, 0);
    if (Mixer = nil) then
    begin
      Log.LogError('Error opening mixer: ' + Pa_GetErrorText(Error), 'TPortaudioInputDevice.Open');
    end
    else
    begin
      // save current source selection and select new source
      SourceIndex := Ini.InputDeviceConfig[CfgIndex].Input-1;
      if (SourceIndex = -1) then
      begin
        // nothing to do if default source is used
        SourceRestore := -1;
      end
      else
      begin
        // store current source-index and select new source
        SourceRestore := Px_GetCurrentInputSource(Mixer); // -1 in error case
        Px_SetCurrentInputSource(Mixer, SourceIndex);
      end;
    end;
  {$ENDIF}

  Result := true;
end;

function TPortaudioInputDevice.Start(): boolean;
var
  Error:       TPaError;
begin
  Result := false;

  // recording already started -> stop first
  if (RecordStream <> nil) then
    Stop();

  // TODO: Do not open the device here (takes too much time).
  if (not Open()) then
    Exit;

  // start capture
  Error := Pa_StartStream(RecordStream);
  if(Error <> paNoError) then
  begin
    Log.LogError('Error starting stream: ' + Pa_GetErrorText(Error), 'TPortaudioInputDevice.Start');
    Close();
    RecordStream := nil;
    Exit;
  end;

  Result := true;
end;

function TPortaudioInputDevice.Stop(): boolean;
var
  Error: TPaError;
begin
  Result := false;

  if (RecordStream = nil) then
    Exit;

  // Note: do NOT call Pa_StopStream here!
  // It gets stuck on devices with non-working callback as Pa_StopStream
  // waits until all buffers have been handled (which never occurs in that case).
  Error := Pa_AbortStream(RecordStream);
  if (Error <> paNoError) then
  begin
    Log.LogError('Pa_AbortStream: ' + Pa_GetErrorText(Error), 'TPortaudioInputDevice.Stop');
  end;

  Result := Close();
end;

function TPortaudioInputDevice.Close(): boolean;
var
  Error: TPaError;
begin
  {$IFDEF UsePortmixer}
    if (Mixer <> nil) then
    begin
      // restore source selection
      if (SourceRestore >= 0) then
      begin
        Px_SetCurrentInputSource(Mixer, SourceRestore);
      end;

      // close mixer
      Px_CloseMixer(Mixer);
      Mixer := nil;
    end;
  {$ENDIF}

  Error := Pa_CloseStream(RecordStream);
  if (Error <> paNoError) then
  begin
    Log.LogError('Pa_CloseStream: ' + Pa_GetErrorText(Error), 'TPortaudioInputDevice.Close');
    Result := false;
  end
  else
  begin
    Result := true;
  end;

  RecordStream := nil;
end;

function TPortaudioInputDevice.GetVolume(): single;
begin
  Result := 0;
  {$IFDEF UsePortmixer}
    if (Mixer <> nil) then
      Result := Px_GetInputVolume(Mixer);
  {$ENDIF}
end;

procedure TPortaudioInputDevice.SetVolume(Volume: single);
begin
  {$IFDEF UsePortmixer}
    if (Mixer <> nil) then
    begin
      // clip to valid range
      if (Volume > 1.0) then
        Volume := 1.0
      else if (Volume < 0) then
        Volume := 0;
      Px_SetInputVolume(Mixer, Volume);
    end;
  {$ENDIF}
end;


{ TAudioInput_Portaudio }

function TAudioInput_Portaudio.GetName: String;
begin
  result := 'Portaudio';
end;

function TAudioInput_Portaudio.EnumDevices(): boolean;
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
  sampleRate:  double;
  latency:     TPaTime;
  {$IFDEF UsePortmixer}
  mixer:       PPxMixer;
  sourceCnt:   integer;
  sourceIndex: integer;
  sourceName:  string;
  {$ENDIF}
begin
  Result := false;

  // choose the best available Audio-API
  paApiIndex := AudioCore.GetPreferredApiIndex();
  if(paApiIndex = -1) then
  begin
    Log.LogError('No working Audio-API found', 'TAudioInput_Portaudio.EnumDevices');
    Exit;
  end;

  paApiInfo := Pa_GetHostApiInfo(paApiIndex);

  SC := 0;

  // init array-size to max. input-devices count
  SetLength(AudioInputProcessor.DeviceList, paApiInfo^.deviceCount);
  for i:= 0 to High(AudioInputProcessor.DeviceList) do
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
    AudioInputProcessor.DeviceList[SC] := paDevice;

    // retrieve device-name
    deviceName := deviceInfo^.name;
    paDevice.Name := deviceName;
    paDevice.PaDeviceIndex := deviceIndex;

    sampleRate := deviceInfo^.defaultSampleRate;

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

    // check souncard and adjust sample-rate
    if (not AudioCore.TestDevice(@inputParams, nil, sampleRate)) then
    begin
      // ignore device if it does not work
      Log.LogError('Device "'+paDevice.Name+'" does not work',
                   'TAudioInput_Portaudio.EnumDevices');
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
                   'TAudioInput_Portaudio.EnumDevices');
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
        'Portaudio.EnumDevices');

    // portaudio does not provide a source-type check
    paDevice.MicSource := -1;
    paDevice.SourceRestore := -1;

    // add a virtual default source (will not change mixer-settings)
    SetLength(paDevice.Source, 1);
    paDevice.Source[0].Name := DEFAULT_SOURCE_NAME;

    {$IFDEF UsePortmixer}
      // use default mixer
      mixer := Px_OpenMixer(stream, 0);

      // get input count
      sourceCnt := Px_GetNumInputSources(mixer);
      SetLength(paDevice.Source, sourceCnt+1);

      // get input names
      for sourceIndex := 1 to sourceCnt do
      begin
        sourceName := Px_GetInputSourceName(mixer, sourceIndex-1);
        paDevice.Source[sourceIndex].Name := sourceName;
      end;

      Px_CloseMixer(mixer);
    {$ENDIF}

    // close test-stream
    Pa_CloseStream(stream);

    Inc(SC);
  end;

  // adjust size to actual input-device count
  SetLength(AudioInputProcessor.DeviceList, SC);

  Log.LogStatus('#Input-Devices: ' + inttostr(SC), 'Portaudio');

  Result := true;
end;

function TAudioInput_Portaudio.InitializeRecord(): boolean;
var
  err: TPaError;
begin
  AudioCore := TAudioCore_Portaudio.GetInstance();

  // initialize portaudio
  err := Pa_Initialize();
  if(err <> paNoError) then
  begin
    Log.LogError(Pa_GetErrorText(err), 'TAudioInput_Portaudio.InitializeRecord');
    Result := false;
    Exit;
  end;

  Result := EnumDevices();
end;

function TAudioInput_Portaudio.FinalizeRecord: boolean;
begin
  CaptureStop;
  Pa_Terminate();
  Result := inherited FinalizeRecord();
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
  MediaManager.add(TAudioInput_Portaudio.Create);

end.
