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

program TestPortAudioDevice;

{* TestPortAudioDevice does some basic tests of the portaudio libs.
 * If all works, it lists all audio input and output devices and their
 * characteristics. Compile and run with simple commands.
 *}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  SysUtils,
  ctypes,
  crt,
  math,
  PortAudio in '../src/lib/portaudio/portaudio.pas';

const
  paDefaultApi = -1;

  ApiPreferenceOrder:
{$IF Defined(MSWINDOWS)}
    // Note1: Portmixer has no mixer support for paASIO and paWASAPI at the moment
    // Note2: Windows Default-API is MME, but DirectSound is faster
    array[0..0] of TPaHostApiTypeId = ( paDirectSound );
{$ELSEIF Defined(DARWIN)}
    array[0..0] of TPaHostApiTypeId = ( paDefaultApi ); // paCoreAudio
{$ELSEIF Defined(UNIX)}
    // Note: Portmixer has no mixer support for JACK at the moment
    array[0..2] of TPaHostApiTypeId = ( paALSA, paJACK, paOSS );
{$ELSE}
    array[0..0] of TPaHostApiTypeId = ( paDefaultApi );
{$IFEND}

  standardSampleRates: array[1..13] of cdouble =
    ( 8000.0,  9600.0,  11025.0,  12000.0,  16000.0,
     22050.0, 24000.0,  32000.0,  44100.0,  48000.0,
     88200.0, 96000.0, 192000.0
    );

  SampleFormat: array[1..8] of culong =
    (paFloat32,      paInt32,          paInt24, paInt16, paInt8, paUInt8,
     paCustomFormat, paNonInterleaved
    );
  SampleFormatName: array[1..8] of string =
    ('paFloat32',      'paInt32',          'paInt24', 'paInt16', 'paInt8', 'paUInt8',
     'paCustomFormat', 'paNonInterleaved'
    );

var
  i, j:        integer;
  PaError:     TPaError;
  paApiIndex:  TPaHostApiIndex;
  paApiInfo:   PPaHostApiInfo;
  deviceIndex: TPaDeviceIndex;
  deviceInfo:  PPaDeviceInfo;
  inputParameters:  PPaStreamParameters;
  outputParameters: PPaStreamParameters;
  sampleRate:       cdouble;
  stream:           PPaStream;
  framesPerBuffer:  culong;
  streamFlags:      TPaStreamFlags;
  streamCallback:   PPaStreamCallback;
  callbackStartTime: TDateTime;
  callbackWorks:    boolean;
  userData:         Pointer;


function GetPreferredApiIndex(): TPaHostApiIndex;
var
  i:        integer;
  apiIndex: TPaHostApiIndex;
  apiInfo:  PPaHostApiInfo;
begin
  result := -1;

  // select preferred sound-API
  for i:= 0 to High(ApiPreferenceOrder) do
  begin
    if (ApiPreferenceOrder[i] <> paDefaultApi) then
    begin
      // check if API is available
      apiIndex := Pa_HostApiTypeIdToHostApiIndex(ApiPreferenceOrder[i]);
      if (apiIndex >= 0) then
      begin
        // we found an API but we must check if it works
        // (on linux portaudio might detect OSS but does not provide
        // any devices if ALSA is enabled)
        apiInfo := Pa_GetHostApiInfo(apiIndex);
        if (apiInfo^.deviceCount > 0) then
        begin
          Result := apiIndex;
          break;
        end;
      end;
    end;
  end;

  // None of the preferred APIs is available -> use default
  if (result < 0) then
  begin
    result := Pa_GetDefaultHostApi();
  end;
end;

{
type
  TAudioSampleFormat = (
    asfU8, asfS8,         // unsigned/signed  8 bits
    asfU16LSB, asfS16LSB, // unsigned/signed 16 bits (endianness: LSB)
    asfU16MSB, asfS16MSB, // unsigned/signed 16 bits (endianness: MSB)
    asfU16, asfS16,       // unsigned/signed 16 bits (endianness: System)
    asfS32,               // signed 32 bits (endianness: System)
    asfFloat,             // float
    asfDouble             // double
  );
  TAudioFormatInfo = ;
  TAudioInputDevice = record
      AudioFormat:     TAudioFormatInfo; // capture format info (e.g. 44.1kHz SInt16 stereo)
      CaptureChannel:  array of TCaptureBuffer; // sound-buffer references used for mono or stereo channel's capture data
  end;

procedure HandleMicrophoneData(Buffer: PByteArray; Size: integer; InputDevice: TAudioInputDevice);
var
  MultiChannelBuffer:      PByteArray;  // buffer handled as array of bytes (offset relative to channel)
  SingleChannelBuffer:     PByteArray;  // temporary buffer for new samples per channel
  SingleChannelBufferSize: integer;
  ChannelIndex:            integer;
  CaptureChannel:          TCaptureBuffer;
  AudioFormat:             TAudioFormatInfo;
  SampleSize:              integer;
  SamplesPerChannel:       integer;
  i:                       integer;
begin
  AudioFormat := InputDevice.AudioFormat;
  SampleSize := AudioSampleSize[AudioFormat.Format];
  SamplesPerChannel := Size div AudioFormat.FrameSize;

  SingleChannelBufferSize := SamplesPerChannel * SampleSize;
  GetMem(SingleChannelBuffer, SingleChannelBufferSize);

  // process channels
  for ChannelIndex := 0 to High(InputDevice.CaptureChannel) do
  begin
    CaptureChannel := InputDevice.CaptureChannel[ChannelIndex];
    // check if a capture buffer was assigned, otherwise there is nothing to do
    if (CaptureChannel <> nil) then
    begin
      // set offset according to channel index
      MultiChannelBuffer := @Buffer[ChannelIndex * SampleSize];
      // separate channel-data from interleaved multi-channel (e.g. stereo) data
      for i := 0 to SamplesPerChannel-1 do
      begin
        Move(MultiChannelBuffer[i*AudioFormat.FrameSize],
             SingleChannelBuffer[i*SampleSize],
             SampleSize);
      end;
      CaptureChannel.ProcessNewBuffer(SingleChannelBuffer, SingleChannelBufferSize);
    end;
  end;

  FreeMem(SingleChannelBuffer);
end;
}

procedure TestInitTerminate();
begin
  writeln ('*** Test of Pa_Initialize and Pa_Terminate ***');
  PaError := Pa_Initialize;
  if PaError = paNoError then
    writeln ('Pa_Initialize: No error')
  else
    writeln ('Pa_Initialize: Error No ', PaError);

  PaError := Pa_Terminate; 
  if PaError = paNoError then
    writeln ('Pa_Terminate:  No error')
  else
    writeln ('Pa_Terminate:  Error No: ', PaError);
  writeln;
end;

procedure TestErrorText();
begin
  writeln ('*** Test of Pa_GetErrorText ***');
  PaError := Pa_Initialize;
  writeln ('paNoError (0): ', Pa_GetErrorText(PaError));
  writeln;
  writeln ('Code   Text');
  writeln ('------------------------------------');
  i := paNotInitialized;
  repeat
    writeln (i:6, ' ', Pa_GetErrorText(i));
    i := succ(i);
  until SameText(Pa_GetErrorText(i), 'Invalid error code') or (i = paNotInitialized + 100);
  writeln (i:6, ' ', Pa_GetErrorText(i));
  PaError := Pa_Terminate; 
  writeln;
end;

procedure TestVersion();
begin
  writeln ('*** Test of Pa_GetVersion and Pa_GetVersionText ***');
  PaError := Pa_Initialize;
  writeln ('Pa_GetVersion:     ', Pa_GetVersion);
  writeln ('Pa_GetVersionText: ', Pa_GetVersionText);
  PaError := Pa_Terminate; 
  writeln;
end;

procedure TestApiInfo();
begin
  writeln ('*** Test of GetPreferredApiIndex ***');
  PaError    := Pa_Initialize;
  paApiIndex := GetPreferredApiIndex();
  if (paApiIndex = -1) then
    writeln ('GetPreferredApiIndex: No working Audio-API found.')
  else
    writeln ('GetPreferredApiIndex: working Audio-API found. No: ', paApiIndex);
  PaError := Pa_Terminate; 
  writeln;

  writeln ('*** Test of Pa_GetHostApiInfo ***');
  PaError    := Pa_Initialize;
  paApiIndex := GetPreferredApiIndex();
  paApiInfo  := Pa_GetHostApiInfo(paApiIndex);
  writeln ('Pa_GetHostApiInfo:');
  writeln ('paApiInfo.structVersion:       ', paApiInfo.structVersion);
  writeln ('paApiInfo._type:               ', paApiInfo._type);
  writeln ('paApiInfo.name:                ', paApiInfo.name);
  writeln ('paApiInfo.deviceCount:         ', paApiInfo.deviceCount);
  writeln ('paApiInfo.defaultInputDevice:  ', paApiInfo.defaultInputDevice);
  writeln ('paApiInfo.defaultOutputDevice: ', paApiInfo.defaultOutputDevice);
  PaError := Pa_Terminate; 
  writeln;

  writeln ('*** Test of Pa_HostApiDeviceIndexToDeviceIndex ***');
  PaError    := Pa_Initialize;
  paApiIndex := GetPreferredApiIndex();
  paApiInfo  := Pa_GetHostApiInfo(paApiIndex);
  for i:= 0 to paApiInfo^.deviceCount-1 do
  begin
    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(paApiIndex, i);
    writeln ('deviceIndex[', i, ']: ', deviceIndex);
  end;
  PaError := Pa_Terminate; 
  writeln;
end;

procedure TestDeviceInfo();
begin
  writeln ('*** Test of Pa_GetDeviceCount ***');
  PaError := Pa_Initialize;
  writeln ('Pa_GetDeviceCount: ', Pa_GetDeviceCount);
  PaError := Pa_Terminate; 
  writeln;

  writeln ('*** Test of Pa_GetDefaultInputDevice ***');
  PaError := Pa_Initialize;
  writeln ('Pa_GetDefaultInputDevice: ', Pa_GetDefaultInputDevice);
  PaError := Pa_Terminate; 
  writeln;

  writeln ('*** Test of Pa_GetDefaultOutputDevice ***');
  PaError := Pa_Initialize;
  writeln ('Pa_GetDefaultOutputDevice: ', Pa_GetDefaultOutputDevice);
  PaError := Pa_Terminate; 
  writeln;

  writeln ('*** Test of Pa_GetDeviceInfo ***');
// Note: the fields of deviceInfo can also be used without the '^'.
// deviceInfo.name works as well as deviceInfo^.name
  PaError    := Pa_Initialize;
  paApiIndex := GetPreferredApiIndex();
  paApiInfo  := Pa_GetHostApiInfo(paApiIndex);
  for i:= 0 to paApiInfo^.deviceCount - 1 do
  begin
    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(paApiIndex, i);
    deviceInfo  := Pa_GetDeviceInfo(deviceIndex);
    writeln ('deviceInfo[', i, '].name:                     ', deviceInfo^.name);
    writeln ('deviceInfo[', i, '].structVersion:            ', deviceInfo^.structVersion, ' (should be 2)');
    writeln ('deviceInfo[', i, '].hostApi:                  ', deviceInfo^.hostApi);
    writeln ('deviceInfo[', i, '].maxInputChannels:         ', deviceInfo^.maxInputChannels);
    writeln ('deviceInfo[', i, '].maxOutputChannels:        ', deviceInfo^.maxOutputChannels);
    writeln ('deviceInfo[', i, '].defaultLowInputLatency:   ', deviceInfo^.defaultLowInputLatency:6:4);
    writeln ('deviceInfo[', i, '].defaultLowOutputLatency:  ', deviceInfo^.defaultLowOutputLatency:6:4);
    writeln ('deviceInfo[', i, '].defaultHighInputLatency:  ', deviceInfo^.defaultHighInputLatency:6:4);
    writeln ('deviceInfo[', i, '].defaultHighOutputLatency: ', deviceInfo^.defaultHighOutputLatency:6:4);
    writeln ('deviceInfo[', i, '].defaultSampleRate:        ', deviceInfo^.defaultSampleRate:5:0);
    writeln;
  end;
  PaError := Pa_Terminate; 
end;

procedure TestFormatInfo();
begin
  writeln ('*** Test of Pa_IsFormatSupported ***');
  PaError    := Pa_Initialize;
  paApiIndex := GetPreferredApiIndex();
  paApiInfo  := Pa_GetHostApiInfo(paApiIndex);
  for i:= 0 to paApiInfo^.deviceCount - 1 do
  begin
    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(paApiIndex, i);
    deviceInfo  := Pa_GetDeviceInfo(deviceIndex);
    writeln ('Device[', i, '] ', deviceInfo^.name, ':');
    New(inputParameters);
    New(outputParameters);

    if deviceInfo^.maxInputChannels > 0 then
    begin
      inputParameters^.device                    := deviceIndex;
      inputParameters^.channelCount              := deviceInfo^.maxInputChannels;
      inputParameters^.sampleFormat              := paInt16;
      inputParameters^.suggestedLatency          := 0;
      inputParameters^.hostApiSpecificStreamInfo := nil;
      outputParameters := nil;
    end
    else
    begin
      inputParameters := nil;
      outputParameters^.device                    := deviceIndex;
      outputParameters^.channelCount              := deviceInfo^.maxOutputChannels;
      outputParameters^.sampleFormat              := paInt16;
      outputParameters^.suggestedLatency          := 0;
      outputParameters^.hostApiSpecificStreamInfo := nil;
    end;

    sampleRate := deviceInfo^.defaultSampleRate;
    PaError    := Pa_IsFormatSupported(inputParameters, outputParameters, sampleRate);
    if PaError = paFormatIsSupported then
      writeln ('Sample rate: ', sampleRate:5:0, ' : supported')
    else
      writeln ('Sample rate: ', sampleRate:5:0, ' : Error: ', Pa_GetErrorText(PaError));

    for j := low(standardSampleRates) to high(standardSampleRates) do
    begin 
      sampleRate := standardSampleRates[j];
      PaError    := Pa_IsFormatSupported(inputParameters, outputParameters, sampleRate);
      if PaError = paFormatIsSupported then
	writeln ('Sample rate: ', sampleRate:5:0, ' : supported')
      else
	writeln ('Sample rate: ', sampleRate:5:0, ' : Error: ', PaError);
    end;

    writeln;
    for j := low(SampleFormat) to high(SampleFormat) do
    begin 
      if inputParameters <> nil then
        inputParameters^.sampleFormat := SampleFormat[j]
      else
	outputParameters^.sampleFormat := SampleFormat[j];
      PaError := Pa_IsFormatSupported(inputParameters, outputParameters, sampleRate);
      if PaError = paFormatIsSupported then
        writeln ('Sample Format ', SampleFormatName[j], ': supported')
      else
        writeln ('Sample Format ', SampleFormatName[j], ': ', Pa_GetErrorText(PaError));
    end;

    Dispose(inputParameters);
    Dispose(outputParameters);
    writeln;
  end;
  PaError := Pa_Terminate; 
end;

function AudioCallback(input: pointer; output: pointer; frameCount: culong;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      inputDevice: pointer): cint; cdecl;
var
  duration: real;
begin
  duration := (Now() - callbackStartTime) * 24 * 3600;
  if (duration < 2.0) then
    result := paContinue
  else
  begin
    callbackWorks := true;
    result := paComplete;
  end;
end;

procedure TestStreams();
begin
  writeln ('*** Test of Pa_OpenStream and Pa_CloseStream ***');
  PaError    := Pa_Initialize;
  paApiIndex := GetPreferredApiIndex();
  paApiInfo  := Pa_GetHostApiInfo(paApiIndex);
  for i:= 0 to paApiInfo^.deviceCount - 1 do
  begin
    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(paApiIndex, i);
    deviceInfo  := Pa_GetDeviceInfo(deviceIndex);
    writeln ('Device[', i, '] ', deviceInfo^.name, ':');
    New(inputParameters);
    New(outputParameters);
    if deviceInfo^.maxInputChannels > 0 then
    begin
      inputParameters^.device                    := deviceIndex;
      inputParameters^.channelCount              := deviceInfo^.maxInputChannels;
      inputParameters^.sampleFormat              := paInt16;
      inputParameters^.suggestedLatency          := deviceInfo.defaultHighInputLatency;
      inputParameters^.hostApiSpecificStreamInfo := nil;
      outputParameters := nil;
    end
    else
    begin
      inputParameters := nil;
      outputParameters^.device                    := deviceIndex;
      outputParameters^.channelCount              := deviceInfo^.maxOutputChannels;
      outputParameters^.sampleFormat              := paInt16;
      outputParameters^.suggestedLatency          := deviceInfo.defaultLowOutputLatency;
      outputParameters^.hostApiSpecificStreamInfo := nil;
    end;
      
    sampleRate      := deviceInfo^.defaultSampleRate;
    framesPerBuffer := paFramesPerBufferUnspecified;
    streamFlags     := paNoFlag;
    streamCallback  := @AudioCallback;
    userData        := nil;

    PaError := Pa_OpenStream(
                     stream,
                     inputParameters,
                     outputParameters,
		     sampleRate,
                     framesPerBuffer,
                     streamFlags,
                     streamCallback,
                     userData 
		     );
    if (PaError = paNoError) and (stream <> nil) then
      writeln ('Pa_OpenStream: success')
    else
      writeln ('Pa_OpenStream: ', Pa_GetErrorText(PaError));

    if (PaError = paNoError) and (stream <> nil) then
    begin
      callbackStartTime := Now();

      PaError := Pa_StartStream(stream);
      if (PaError = paNoError) then
	writeln ('Pa_StartStream: success')
      else
	writeln ('Pa_StartStream: ', Pa_GetErrorText(PaError));

      callbackWorks := false;

      // wait twice the time a successful callback would need for termination
      writeln('Wait for callback');
      delay(4000);

      if (callbackWorks and (Pa_IsStreamStopped(stream) = 0)) then
      begin
	writeln ('Success: Device works');
	PaError := Pa_StopStream(stream);
	if (PaError = paNoError) then
	  writeln ('Pa_StopStream: success')
	else
	  writeln ('Pa_StopStream: ', Pa_GetErrorText(PaError));
      end
      else
      begin
	writeln ('Error: Non working device');
        PaError := Pa_AbortStream(stream);
        if (PaError = paNoError) then
          writeln ('Pa_AbortStream: success')
        else
          writeln ('Pa_AbortStream: ', Pa_GetErrorText(PaError));

      end;
    end;

    PaError := Pa_CloseStream(stream);
    if PaError = paNoError then
      writeln ('Pa_CloseStream: success')
    else
      writeln ('Pa_CloseStream: ', Pa_GetErrorText(PaError));

    Dispose(inputParameters);
    Dispose(outputParameters);
    
    writeln;    
  end;
  PaError := Pa_Terminate; 
end;

begin
  // floating point exceptions are raised. Therefore, set the exception mask.
  SetExceptionMask([exZeroDivide, exPrecision]);

  writeln ('Start: Test of Portaudio libs');
  writeln;

  //TestInitTerminate();
  //TestErrorText(); 
  //TestVersion();
  //TestApiInfo();
  //TestDeviceInfo();
  //TestFormatInfo();
  TestStreams();
  
  writeln ('End: Test of Portaudio libs');
end.