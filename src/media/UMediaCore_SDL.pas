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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/media/UMediaCore_SDL.pas $
 * $Id: UMediaCore_SDL.pas 2475 2010-06-10 18:27:53Z brunzelchen $
 *}

unit UMediaCore_SDL;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMusic,
  SDL2;

function ConvertAudioFormatToSDL(Format: TAudioSampleFormat; out SDLFormat: UInt16): boolean;
function ConvertAudioFormatFromSDL(SDLFormat: UInt16; out Format: TAudioSampleFormat): boolean;

{*
 * Newer SDL2 functions below are used only if the SDL2 found at build time
 * (SDL2_VERSION_INT from configure) AND the library loaded at runtime are
 * new enough, so USDX still builds and runs with older SDL2 versions.
 *}

const
  // SDL versions in configure's MMMmmmrrr format
  SDL_VERSION_INT_AUDIODEVICESPEC  = 2000016; // SDL_GetAudioDeviceSpec
  SDL_VERSION_INT_DEFAULTAUDIOINFO = 2024000; // SDL_GetDefaultAudioInfo

{** Version of the SDL library loaded at runtime, in MMMmmmrrr format. *}
function SDLLinkedVersionInt(): integer;

{**
 * Preferred format of audio device Index as reported by the SDL backend.
 * Returns false if the SDL version is too old or the backend reports
 * nothing (e.g. ALSA, DirectSound). Requires a prior SDL_GetNumAudioDevices.
 *}
function SDLGetAudioDeviceSpec(Index: integer; IsCapture: boolean; out Spec: TSDL_AudioSpec): boolean;

{** e.g. 's16le 2ch 44100Hz', 'unknown format' if nothing is known. *}
function SDLAudioSpecToStr(const Spec: TSDL_AudioSpec): string;

{**
 * Logs the SDL versions and audio drivers, then the default device and all
 * devices of one direction with the format each backend prefers.
 * Must be called after SDL_InitSubSystem(SDL_INIT_AUDIO).
 *}
procedure LogSDLAudioInfo(IsCapture: boolean);

implementation

uses
  SysUtils,
  UConfig,
  ULog;


function ConvertAudioFormatToSDL(Format: TAudioSampleFormat; out SDLFormat: UInt16): boolean;
begin
  case Format of
    asfU8:     SDLFormat := AUDIO_U8;
    asfS8:     SDLFormat := AUDIO_S8;
    asfU16LSB: SDLFormat := AUDIO_U16LSB;
    asfS16LSB: SDLFormat := AUDIO_S16LSB;
    asfU16MSB: SDLFormat := AUDIO_U16MSB;
    asfS16MSB: SDLFormat := AUDIO_S16MSB;
    asfU16:    SDLFormat := AUDIO_U16SYS;
    asfS16:    SDLFormat := AUDIO_S16SYS;
    asfS32:    SDLFormat := AUDIO_S32SYS;
    asfFloat:  SDLFormat := AUDIO_F32SYS;
    else begin
      Result := false;
      Exit;
    end;
  end;
  Result := true;
end;

function ConvertAudioFormatFromSDL(SDLFormat: UInt16; out Format: TAudioSampleFormat): boolean;
begin
  case SDLFormat of
    AUDIO_U8:     Format := asfU8;
    AUDIO_S8:     Format := asfS8;
    AUDIO_U16SYS: Format := asfU16;
    AUDIO_S16SYS: Format := asfS16;
{$IF AUDIO_U16SYS <> AUDIO_U16LSB}
    AUDIO_U16LSB: Format := asfU16LSB;
    AUDIO_S16LSB: Format := asfS16LSB;
{$ENDIF}
{$IF AUDIO_U16SYS <> AUDIO_U16MSB}
    AUDIO_U16MSB: Format := asfU16MSB;
    AUDIO_S16MSB: Format := asfS16MSB;
{$ENDIF}
    AUDIO_S32SYS: Format := asfS32;
    AUDIO_F32SYS: Format := asfFloat;
    else begin
      Result := false;
      Exit;
    end;
  end;
  Result := true;
end;

function VersionIntToStr(Version: integer): string;
begin
  Result := Format('%d.%d.%d', [Version div 1000000, (Version div 1000) mod 1000, Version mod 1000]);
end;

function SDLLinkedVersionInt(): integer;
var
  Ver: TSDL_Version;
begin
  SDL_GetVersion(@Ver);
  Result := Ver.major * 1000000 + Ver.minor * 1000 + Ver.patch;
end;

function SDLGetAudioDeviceSpec(Index: integer; IsCapture: boolean; out Spec: TSDL_AudioSpec): boolean;
begin
  FillChar(Spec, SizeOf(Spec), 0);
  Result := false;
  {$IF SDL2_VERSION_INT >= SDL_VERSION_INT_AUDIODEVICESPEC}
  if SDLLinkedVersionInt() >= SDL_VERSION_INT_AUDIODEVICESPEC then
    Result := (SDL_GetAudioDeviceSpec(Index, Ord(IsCapture), @Spec) = 0) and
              (Spec.channels > 0);
  {$IFEND}
end;

function SDLAudioFormatToStr(Format: TSDL_AudioFormat): string;
begin
  if (Format and $0100) <> 0 then // SDL_AUDIO_MASK_DATATYPE
    Result := 'f'
  else if (Format and $8000) <> 0 then // SDL_AUDIO_MASK_SIGNED
    Result := 's'
  else
    Result := 'u';
  Result := Result + IntToStr(Format and $FF); // SDL_AUDIO_MASK_BITSIZE
  if (Format and $FF) > 8 then
  begin
    if (Format and $1000) <> 0 then // SDL_AUDIO_MASK_ENDIAN
      Result := Result + 'be'
    else
      Result := Result + 'le';
  end;
end;

function SDLAudioSpecToStr(const Spec: TSDL_AudioSpec): string;
begin
  if (Spec.channels = 0) and (Spec.freq = 0) then
  begin
    Result := 'unknown format';
    Exit;
  end;
  if Spec.format <> 0 then
    Result := SDLAudioFormatToStr(Spec.format) + ' '
  else
    Result := '';
  Result := Result + IntToStr(Spec.channels) + 'ch';
  if Spec.freq > 0 then
    Result := Result + ' ' + IntToStr(Spec.freq) + 'Hz';
end;

procedure LogSDLAudioInfo(IsCapture: boolean);
const
  LogArea = 'SDL Audio';
var
  i:          integer;
  Drivers:    string;
  Direction:  string;
  Count:      integer;
  Spec:       TSDL_AudioSpec;
  {$IF SDL2_VERSION_INT >= SDL_VERSION_INT_DEFAULTAUDIOINFO}
  DefaultName: PAnsiChar;
  {$IFEND}
begin
  if IsCapture then
    Direction := 'capture'
  else
    Direction := 'playback';

  Drivers := '';
  for i := 0 to SDL_GetNumAudioDrivers() - 1 do
  begin
    if Drivers <> '' then
      Drivers := Drivers + ', ';
    Drivers := Drivers + SDL_GetAudioDriver(i);
  end;
  Log.LogInfo(Format('SDL %s (built against %s), %s driver "%s", compiled-in drivers: %s',
      [VersionIntToStr(SDLLinkedVersionInt()), VersionIntToStr(SDL2_VERSION_INT),
       Direction, SDL_GetCurrentAudioDriver(), Drivers]), LogArea);

  {$IF SDL2_VERSION_INT >= SDL_VERSION_INT_DEFAULTAUDIOINFO}
  if SDLLinkedVersionInt() >= SDL_VERSION_INT_DEFAULTAUDIOINFO then
  begin
    DefaultName := nil;
    FillChar(Spec, SizeOf(Spec), 0);
    if SDL_GetDefaultAudioInfo(@DefaultName, @Spec, Ord(IsCapture)) = 0 then
    begin
      if DefaultName <> nil then
        Log.LogInfo(Format('Default %s device: "%s", %s',
            [Direction, string(DefaultName), SDLAudioSpecToStr(Spec)]), LogArea)
      else
        Log.LogInfo(Format('Default %s device: %s',
            [Direction, SDLAudioSpecToStr(Spec)]), LogArea);
      SDL_free(DefaultName);
    end
    else
      Log.LogInfo(Format('Default %s device: not reported by the driver', [Direction]), LogArea);
  end;
  {$IFEND}

  Count := SDL_GetNumAudioDevices(Ord(IsCapture));
  for i := 0 to Count - 1 do
  begin
    if SDLGetAudioDeviceSpec(i, IsCapture, Spec) then
      Log.LogInfo(Format('%s device %d: "%s", %s',
          [Direction, i, SDL_GetAudioDeviceName(i, Ord(IsCapture)), SDLAudioSpecToStr(Spec)]), LogArea)
    else
      Log.LogInfo(Format('%s device %d: "%s", format not reported',
          [Direction, i, SDL_GetAudioDeviceName(i, Ord(IsCapture))]), LogArea);
  end;
end;

end.
