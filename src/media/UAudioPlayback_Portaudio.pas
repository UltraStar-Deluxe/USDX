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
      Latency: double;
      function OpenDevice(deviceIndex: TPaDeviceIndex): boolean;
      function EnumDevices(): boolean;
    protected
      function InitializeAudioPlaybackEngine(): boolean; override;
      function StartAudioPlaybackEngine(): boolean;      override;
      procedure StopAudioPlaybackEngine();               override;
      function FinalizeAudioPlaybackEngine(): boolean;   override;
      function GetLatency(): double;                     override;
    public
      function GetName: String;                          override;
  end;

  TPortaudioOutputDevice = class(TAudioOutputDevice)
    private
      PaDeviceIndex:  TPaDeviceIndex;
  end;


{ TAudioPlayback_Portaudio }

function PortaudioAudioCallback(input: Pointer; output: Pointer; frameCount: Longword;
    timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
    userData: Pointer): Integer; cdecl;
var
  Engine: TAudioPlayback_Portaudio;
begin
  Engine := TAudioPlayback_Portaudio(userData);
  // update latency
  Engine.Latency := timeInfo.outputBufferDacTime - timeInfo.currentTime;
  // call superclass callback
  Engine.AudioCallback(output, frameCount * Engine.FormatInfo.FrameSize);
  Result := paContinue;
end;

function TAudioPlayback_Portaudio.GetName: String;
begin
  Result := 'Portaudio_Playback';
end;

function TAudioPlayback_Portaudio.OpenDevice(deviceIndex: TPaDeviceIndex): boolean;
var
  DeviceInfo : PPaDeviceInfo;
  SampleRate : double;
  OutParams  : TPaStreamParameters;
  StreamInfo : PPaStreamInfo;
  err        : TPaError;
begin
  Result := false;

  DeviceInfo := Pa_GetDeviceInfo(deviceIndex);

  Log.LogInfo('Audio-Output Device: ' + DeviceInfo^.name, 'TAudioPlayback_Portaudio.OpenDevice');

  SampleRate := DeviceInfo^.defaultSampleRate;

  with OutParams do
  begin
    device := deviceIndex;
    channelCount := 2;
    sampleFormat := paInt16;
    suggestedLatency := DeviceInfo^.defaultLowOutputLatency;
    hostApiSpecificStreamInfo := nil;
  end;

  // check souncard and adjust sample-rate
  if not AudioCore.TestDevice(nil, @OutParams, SampleRate) then
  begin
    Log.LogStatus('TestDevice failed!', 'TAudioPlayback_Portaudio.OpenDevice');
    Exit;
  end;

  // open output stream
  err := Pa_OpenStream(paStream, nil, @OutParams, SampleRate,
          paFramesPerBufferUnspecified,
          paNoFlag, @PortaudioAudioCallback, Self);
  if(err <> paNoError) then
  begin
    Log.LogStatus(Pa_GetErrorText(err), 'TAudioPlayback_Portaudio.OpenDevice');
    paStream := nil;
    Exit;
  end;

  // get estimated latency (will be updated with real latency in the callback)
  StreamInfo := Pa_GetStreamInfo(paStream);
  if (StreamInfo <> nil) then
    Latency := StreamInfo^.outputLatency
  else
    Latency := 0;

  FormatInfo := TAudioFormatInfo.Create(
    OutParams.channelCount,
    SampleRate,
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
  outputParams: TPaStreamParameters;
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

    // setup desired output parameters
    // TODO: retry with input-latency set to 20ms (defaultLowOutputLatency might
    //       not be set correctly in OSS)
    with outputParams do
    begin
      device := deviceIndex;
      channelCount := channelCnt;
      sampleFormat := paInt16;
      suggestedLatency := latency;
      hostApiSpecificStreamInfo := nil;
    end;

    // check if mic-callback works (might not be called on some devices)
    if (not TAudioCore_Portaudio.TestDevice(nil, @outputParams, sampleRate)) then
    begin
      // ignore device if callback did not work
      Log.LogError('Device "'+paDevice.Name+'" does not respond',
                   'TAudioPlayback_Portaudio.InitializeRecord');
      paDevice.Free();
      continue;
    end;

    // open device for further info
    err := Pa_OpenStream(stream, nil, @outputParams, sampleRate,
        paFramesPerBufferUnspecified, paNoFlag, @MicrophoneTestCallback, nil);
    if(err <> paNoError) then
    begin
      // unable to open device -> skip
      errMsg := Pa_GetErrorText(err);
      Log.LogError('Device error: "'+ deviceName +'" ('+ errMsg +')',
                   'TAudioPlayback_Portaudio.InitializeRecord');
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

    Log.LogStatus('OutputDevice "'+paDevice.Name+'"@' +
        IntToStr(paDevice.AudioFormat.Channels)+'x'+
        FloatToStr(paDevice.AudioFormat.SampleRate)+'Hz ('+
        FloatTostr(outputParams.suggestedLatency)+'sec)' ,
        'TAudioInput_Portaudio.InitializeRecord');

    // close test-stream
    Pa_CloseStream(stream);

    Inc(SC);
  end;

  // adjust size to actual input-device count
  SetLength(OutputDeviceList, SC);

  Log.LogStatus('#Output-Devices: ' + inttostr(SC), 'Portaudio');
*)

  Result := true;
end;

function TAudioPlayback_Portaudio.InitializeAudioPlaybackEngine(): boolean;
var
  paApiIndex      : TPaHostApiIndex;
  paApiInfo       : PPaHostApiInfo;
  paOutDevice     : TPaDeviceIndex;
  err: TPaError;
begin
  Result := false;

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

  Result := true;
end;

function TAudioPlayback_Portaudio.StartAudioPlaybackEngine(): boolean;
var
  err: TPaError;
begin
  Result := false;

  if (paStream = nil) then
    Exit;

  err := Pa_StartStream(paStream);
  if(err <> paNoError) then
  begin
    Log.LogStatus('Pa_StartStream: '+Pa_GetErrorText(err), 'UAudioPlayback_Portaudio');
    Exit;
  end;

  Result := true;
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

function TAudioPlayback_Portaudio.GetLatency(): double;
begin
  Result := Latency;
end;


initialization
  MediaManager.Add(TAudioPlayback_Portaudio.Create);

end.
