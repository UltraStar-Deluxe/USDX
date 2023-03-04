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

implementation

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

end.
