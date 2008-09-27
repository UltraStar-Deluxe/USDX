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

unit UAudioPlayback_SDL;

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
  sdl,
  UAudioPlayback_SoftMixer,
  ULog,
  UIni,
  UMain;

type
  TAudioPlayback_SDL = class(TAudioPlayback_SoftMixer)
    private
      Latency: double;
      function EnumDevices(): boolean;
    protected
      function InitializeAudioPlaybackEngine(): boolean; override;
      function StartAudioPlaybackEngine(): boolean;      override;
      procedure StopAudioPlaybackEngine();               override;
      function FinalizeAudioPlaybackEngine(): boolean;   override;
      function GetLatency(): double;                     override;
    public
      function GetName: String;                          override;
      procedure MixBuffers(dst, src: PChar; size: Cardinal; volume: Single); override;
  end;

  
{ TAudioPlayback_SDL }

procedure SDLAudioCallback(userdata: Pointer; stream: PChar; len: integer); cdecl;
var
  Engine: TAudioPlayback_SDL;
begin
  Engine := TAudioPlayback_SDL(userdata);
  Engine.AudioCallback(stream, len);
end;

function TAudioPlayback_SDL.GetName: String;
begin
  Result := 'SDL_Playback';
end;

function TAudioPlayback_SDL.EnumDevices(): boolean;
begin
  // Note: SDL does not provide Device-Selection capabilities (will be introduced in 1.3)
  ClearOutputDeviceList();
  SetLength(OutputDeviceList, 1);
  OutputDeviceList[0] := TAudioOutputDevice.Create();
  OutputDeviceList[0].Name := '[SDL Default-Device]';
  Result := true;
end;

function TAudioPlayback_SDL.InitializeAudioPlaybackEngine(): boolean;
var
  DesiredAudioSpec, ObtainedAudioSpec: TSDL_AudioSpec;
  SampleBufferSize: integer;
begin
  Result := false;

  EnumDevices();

  if (SDL_InitSubSystem(SDL_INIT_AUDIO) = -1) then
  begin
    Log.LogError('SDL_InitSubSystem failed!', 'TAudioPlayback_SDL.InitializeAudioPlaybackEngine');
    Exit;
  end;

  SampleBufferSize := IAudioOutputBufferSizeVals[Ini.AudioOutputBufferSizeIndex];
  if (SampleBufferSize <= 0) then
  begin
    // Automatic setting default
    // FIXME: too much glitches with 1024 samples
    SampleBufferSize := 2048; //1024;
  end;

  FillChar(DesiredAudioSpec, SizeOf(DesiredAudioSpec), 0);
  with DesiredAudioSpec do
  begin
    freq := 44100;
    format := AUDIO_S16SYS;
    channels := 2;
    samples := SampleBufferSize;
    callback := @SDLAudioCallback;
    userdata := Self;
  end;

  // Note: always use the "obtained" parameter, otherwise SDL might try to convert
  // the samples itself if the desired format is not available. This might lead
  // to problems if for example ALSA does not support 44100Hz and proposes 48000Hz.
  // Without the obtained parameter, SDL would try to convert 44.1kHz to 48kHz with
  // its crappy (non working) converter resulting in a wrong (too high) pitch.
  if(SDL_OpenAudio(@DesiredAudioSpec, @ObtainedAudioSpec) = -1) then
  begin
    Log.LogStatus('SDL_OpenAudio: ' + SDL_GetError(), 'TAudioPlayback_SDL.InitializeAudioPlaybackEngine');
    Exit;
  end;

  FormatInfo := TAudioFormatInfo.Create(
    ObtainedAudioSpec.channels,
    ObtainedAudioSpec.freq,
    asfS16
  );

  // Note: SDL does not provide info of the internal buffer state.
  // So we use the average buffer-size.
  Latency := (ObtainedAudioSpec.samples/2) / FormatInfo.SampleRate;

  Log.LogStatus('Opened audio device', 'TAudioPlayback_SDL.InitializeAudioPlaybackEngine');

  Result := true;
end;

function TAudioPlayback_SDL.StartAudioPlaybackEngine(): boolean;
begin
  SDL_PauseAudio(0);
  Result := true;
end;

procedure TAudioPlayback_SDL.StopAudioPlaybackEngine();
begin
  SDL_PauseAudio(1);
end;

function TAudioPlayback_SDL.FinalizeAudioPlaybackEngine(): boolean;
begin
  SDL_CloseAudio();
  SDL_QuitSubSystem(SDL_INIT_AUDIO);
  Result := true;
end;

function TAudioPlayback_SDL.GetLatency(): double;
begin
  Result := Latency;
end;

procedure TAudioPlayback_SDL.MixBuffers(dst, src: PChar; size: Cardinal; volume: Single);
begin
  SDL_MixAudio(PUInt8(dst), PUInt8(src), size, Round(volume * SDL_MIX_MAXVOLUME));
end;


initialization
  MediaManager.add(TAudioPlayback_SDL.Create);

end.
