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
function ULuaMusic_UnloadSound(L: Plua_State): Integer; cdecl;

const
  ULuaMusic_Lib_f: array [0..3] of lual_reg = (
   (name:'OpenSound';func:ULuaMusic_OpenSound),
   (name:'PlaySound';func:ULuaMusic_PlaySound),
   (name:'UnloadSound';func:ULuaMusic_UnloadSound),
   (name:nil;func:nil)
   );

implementation

{
  Add a sound to the game's sound library, obtaining the sound ID to use with PlaySound.
  The sound should be a short sound-effect.
  In case the sound ID is -1, loading the sound file has failed.
}
function ULuaMusic_OpenSound(L: Plua_State): Integer; cdecl;
  var
    SoundId: integer;
begin
  // do not return anything to lua by default
  result := 0;
  if (lua_IsString(L, 1)) then begin
    // add the sound
    SoundId := SoundLib.AddSound(lua_toString(L, 1));
    if SoundId = -1 then begin
      luaL_error(L, PChar('Unable to open sound file.'));
    end;
    // return the sound id (index)
    result := 1;
    lua_pushinteger(L, SoundId);
  end else begin
    luaL_argerror(L, 1, PChar('Filename (string) expected.'));
  end;
end;

{
  Play a game sound once.
  Expects a sound ID obtained by OpenSound.
}
function ULuaMusic_PlaySound(L: Plua_State): Integer; cdecl;
  var
    PlaybackStream: TAudioPlaybackStream;
begin
  result := 0; // never return anything to lua
  if lua_IsInteger(L, 1) then begin
    PlaybackStream := SoundLib.GetSound(lua_tointeger(L, 1));
    if PlaybackStream <> nil then begin
      AudioPlayback.PlaySound(PlaybackStream);
    end else begin
      luaL_argerror(L, 1, PChar('Failed to get sound by ID.'));
    end;
  end else begin
    luaL_argerror(L, 1, PChar('Sound ID (integer) expected.'));
  end;
end;

{ 
  Unloads a sound. 
  Only use this function with an ID you previously obtained via OpenSound. 
  Only then it is "your" sound. Removing a sound that is not yours can induce unexpected behaviour.
}
function ULuaMusic_UnloadSound(L: Plua_State): Integer; cdecl;
  var
    PlaybackStream: TAudioPlaybackStream;
begin
  result := 0; // never return anything to lua
  if lua_IsInteger(L, 1) then begin
    SoundLib.RemoveSound(lua_tointeger(L, 1));
  end else begin
    luaL_argerror(L, 1, PChar('Sound ID (integer) expected.'));
  end;
end;

function luaopen_Music (L: Plua_State): Integer; cdecl;
begin
    luaL_register(L, 'Music', @ULuaMusic_Lib_f[0]);
    result := 1;
end;

end.
