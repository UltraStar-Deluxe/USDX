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
  PortAudio in '../src/lib/portaudio/portaudio.pas';

const
  paDefaultApi = -1;

const
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

var
  i:           integer;
  PaError:     TPaError;
  paApiIndex:  TPaHostApiIndex;
  paApiInfo:   PPaHostApiInfo;
  deviceIndex: TPaDeviceIndex;
  deviceInfo:  PPaDeviceInfo;

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

begin
  writeln ('Start: Test of Portaudio libs');
  writeln;

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
  
  writeln ('*** Test of Pa_GetVersion and Pa_GetVersionText ***');
  PaError := Pa_Initialize;
  writeln ('Pa_GetVersion:     ', Pa_GetVersion);
  writeln ('Pa_GetVersionText: ', Pa_GetVersionText);
  PaError := Pa_Terminate; 
  writeln;

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

  writeln ('*** Test of Pa_GetDeviceInfo ***');
// 
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
    writeln ('deviceInfo[', i, '].defaultLowInputLatency:   ', deviceInfo^.defaultLowInputLatency);
    writeln ('deviceInfo[', i, '].defaultLowOutputLatency:  ', deviceInfo^.defaultLowOutputLatency);
    writeln ('deviceInfo[', i, '].defaultHighInputLatency:  ', deviceInfo^.defaultHighInputLatency);
    writeln ('deviceInfo[', i, '].defaultHighOutputLatency: ', deviceInfo^.defaultHighOutputLatency);
    writeln ('deviceInfo[', i, '].defaultSampleRate:        ', deviceInfo^.defaultSampleRate:5:1);
    writeln;
  end;
  PaError := Pa_Terminate; 

  writeln ('End: Test of Portaudio libs');
end.