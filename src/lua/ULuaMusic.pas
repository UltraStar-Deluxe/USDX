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

unit ULuaMusic;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses ULua, UMusic, UPathUtils, UPath, SysUtils;

function luaopen_Music (L: Plua_State): Integer; cdecl;

function ULuaMusic_OpenSound(L: Plua_State): Integer; cdecl;
function ULuaMusic_PlaySound(L: Plua_State): Integer; cdecl;

const
  ULuaMusic_Lib_f: array [0..2] of lual_reg = (
   (name:'OpenSound';func:ULuaMusic_OpenSound),
   (name:'PlaySound';func:ULuaMusic_PlaySound),
   (name:nil;func:nil)
   );

implementation

function ULuaMusic_OpenSound(L: Plua_State): Integer; cdecl;
  var
    SoundId: integer;
begin
  if (lua_IsString(L, 1)) then begin
    // add sound to library, obtaining the sound id
    SoundId := SoundLib.AddSound(lua_toString(L, 1));
    // return the sound id (index) for use with PlaySound
    lua_pushinteger(L, SoundId);
    result := 1;
  end else begin
    luaL_argerror(L, 1, PChar('filename (as string) expected'));
    result := 0;
  end;
end;

function ULuaMusic_PlaySound(L: Plua_State): Integer; cdecl;
  var
    PlaybackStream: TAudioPlaybackStream;
begin
  result := 0; // never return anything to lua
  // expect a sound id (index) obtained by OpenSound
  if lua_IsInteger(L, 1) then begin
    PlaybackStream := SoundLib.GetSound(lua_tointeger(L, 1));
    if PlaybackStream <> nil then begin
      AudioPlayback.PlaySound(PlaybackStream);
    end else begin
      luaL_argerror(L, 1, PChar('failed to get sound by id'));
    end;
  end else begin
    luaL_argerror(L, 1, PChar('sound id expected'));
  end;
end;

// TODO: add RemoveSound

function luaopen_Music (L: Plua_State): Integer; cdecl;
begin
    luaL_register(L, 'Music', @ULuaMusic_Lib_f[0]);
    result := 1;
end;

end.
