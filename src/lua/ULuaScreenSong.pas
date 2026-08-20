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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/branches/experimental/Lua/src/lua/ULuaTexture.pas $
 * $Id: ULuaTexture.pas 1551 2009-01-04 14:08:33Z Hawkear $
 *}
unit ULuaScreenSong;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses ULua;

{ ScreenSong.GetSelectedSong - returns a table identifying the song that is
  currently highlighted on the song selection screen, or nil if there is none
  (another screen is showing, or the highlighted entry is a category header):
    | Path: string     - absolute path of the song's directory
    | FileName: string - absolute path of the song's .txt file
    | Artist: string   - artist as given in the song header
    | Title: string    - title as given in the song header
  the path is authoritative; do not rebuild it from artist and title. }
function ULuaScreenSong_GetSelectedSong(L: Plua_State): Integer; cdecl;

const
  ULuaScreenSong_Lib_f: array [0..0] of lual_reg = (
    (name:'GetSelectedSong';func:ULuaScreenSong_GetSelectedSong)
  );

implementation
uses
  UScreenSong,
  USongs,
  USong,
  UDisplay,
  UGraphic,
  UPath,
  ULuaUtils,
  SysUtils;

{ returns a table identifying the highlighted song, or nil if there is none.
  see the declaration in the interface section for its fields. }
function ULuaScreenSong_GetSelectedSong(L: Plua_State): Integer; cdecl;
  var
    Index:   Integer;
    PathStr: UTF8String;
    FileStr: UTF8String;
begin
  Result := 1;

  lua_ClearStack(L);

  if (Display.CurrentScreen <> @ScreenSong) or (CatSongs = nil) then
  begin
    lua_pushNil(L);
    Exit;
  end;

  Index := ScreenSong.Interaction;

  // Main marks a category header rather than an actual song, so there is
  // nothing to identify in that case
  if (Index < 0) or (Index > High(CatSongs.Song)) or
     (CatSongs.Song[Index].Main) then
  begin
    lua_pushNil(L);
    Exit;
  end;

  // resolve both before touching the stack, so a path error can not leave a
  // half built table behind
  PathStr := CatSongs.Song[Index].Path.ToUTF8(true);
  FileStr := CatSongs.Song[Index].Path.Append(
               CatSongs.Song[Index].FileName).ToUTF8(true);

  lua_CreateTable(L, 0, 4);

  lua_PushString(L, PChar(PathStr));
  lua_SetField(L, -2, PChar('Path'));

  lua_PushString(L, PChar(FileStr));
  lua_SetField(L, -2, PChar('FileName'));

  lua_PushString(L, PChar(CatSongs.Song[Index].Artist));
  lua_SetField(L, -2, PChar('Artist'));

  lua_PushString(L, PChar(CatSongs.Song[Index].Title));
  lua_SetField(L, -2, PChar('Title'));
end;

end.
