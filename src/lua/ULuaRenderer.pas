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

unit ULuaRenderer;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  ULua;

{ lua lib functions }
function ULuaRenderer_DrawQuad(L: Plua_State): Integer; cdecl;
function ULuaRenderer_DrawTriangle(L: Plua_State): Integer; cdecl;
function ULuaRenderer_DrawLine(L: Plua_State): Integer; cdecl;

const
  ULuaRenderer_Lib_f: array [0..2] of lual_reg = (
   (name:'DrawQuad';func:ULuaRenderer_DrawQuad),
   (name:'DrawTriangle';func:ULuaRenderer_DrawTriangle),
   (name:'DrawLine';func:ULuaRenderer_DrawQuad)
   );

implementation

uses
   ULog,
   URenderer;

function ULuaRenderer_DrawQuad(L: Plua_State): Integer; cdecl;
begin
  if (lua_gettop(L) = 9) then
    Renderer.DrawQuad(
      lual_checknumber(L,-9),
      lual_checknumber(L,-8),
      lual_checknumber(L,-7),
      lual_checknumber(L,-6),
      lual_checknumber(L,-5),
      lual_checknumber(L,-4),
      lual_checknumber(L,-3),
      lual_checknumber(L,-2),
      lual_checknumber(L,-1)
    )
  else
    luaL_error(L, 'incorrect arguments to function ''Renderer.DrawQuad''');
  Result := 0;
end;

function ULuaRenderer_DrawTriangle(L: Plua_State): Integer; cdecl;
begin
  if (lua_gettop(L) = 11) then
    Renderer.DrawTriangle(
      lual_checknumber(L,-11),
      lual_checknumber(L,-10),
      lual_checknumber(L,-9),
      lual_checknumber(L,-8),
      lual_checknumber(L,-7),
      lual_checknumber(L,-6),
      lual_checknumber(L,-5),
      lual_checknumber(L,-4),
      lual_checknumber(L,-3),
      lual_checknumber(L,-2),
      lual_checknumber(L,-1)
    )
  else
    luaL_error(L, 'incorrect arguments to function ''Renderer.DrawTriangle''');
  Result := 0;
end;

function ULuaRenderer_DrawLine(L: Plua_State): Integer; cdecl;
begin
  if (lua_gettop(L) = 10) then
    Renderer.DrawLine(
      lual_checknumber(L,-10),
      lual_checknumber(L,-9),
      lual_checknumber(L,-8),
      lual_checknumber(L,-7),
      lual_checknumber(L,-6),
      lual_checknumber(L,-5),
      lual_checknumber(L,-4),
      lual_checknumber(L,-3),
      lual_checknumber(L,-2),
      lual_checknumber(L,-1)
    )
  else
    luaL_error(L, 'incorrect arguments to function ''Renderer.DrawLine''');
  Result := 0;
end;

end.
