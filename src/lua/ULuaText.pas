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

unit ULuaText;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UText,
  SysUtils,
  ULua;

{ UText.Pos(X, Y: Float) : sets font position }
function ULuaText_Pos(L: Plua_State): Integer; cdecl;

{ UText.Size(Size: Float) : sets font size }
function ULuaText_Size(L: Plua_State): Integer; cdecl;

{ UText.Style(Style: int) : sets font style (from 0 to 3) }
function ULuaText_Style(L: Plua_State): Integer; cdecl;

{ UText.Italic(isItalic: boolean) : sets if font is italic }
function ULuaText_Italic(L: Plua_State): Integer; cdecl;

function ULuaText_Color(L: Plua_State): Integer; cdecl;


{ UText.Width(Text: String) : returns width of Text if printed
  w/ current settings in pixels }
function ULuaText_Width(L: Plua_State): Integer; cdecl;

{ UText.Print(Text: String) : prints text to screen w/ current
  settings}
function ULuaText_Print(L: Plua_State): Integer; cdecl;

const
  ULuaText_Lib_f: array [0..6] of lual_reg = (
    (name:'Pos'; func:ULuaText_Pos),
    (name:'Size'; func:ULuaText_Size),
    (name:'Style'; func:ULuaText_Style),
    (name:'Italic'; func:ULuaText_Italic),
    (name:'Color'; func:ULuaText_Color),
    (name:'Width'; func:ULuaText_Width),
    (name:'Print'; func:ULuaText_Print)
  );

implementation

{ UText.Pos(X, Y: Float) : sets font position }
function ULuaText_Pos(L: Plua_State): Integer; cdecl;
  var X, Y: Double;
begin
  X := luaL_checknumber(L, 1);
  Y := luaL_checknumber(L, 2);

  SetFontPos(X, Y);

  Result := 0;
end;

{ UText.Size(Size: Float) : sets font size }
function ULuaText_Size(L: Plua_State): Integer; cdecl;
  var Size: Double;
begin
  Size := luaL_checknumber(L, 1);

  SetFontSize(Size);
  
  Result := 0;
end;

{ UText.Style(Style: int) : sets font style (from 0 to 2) }
function ULuaText_Style(L: Plua_State): Integer; cdecl;
  var Style: Integer;
begin
  Style := luaL_checkinteger(L, 1);

  if (Style >= 0) and (Style < Length(Fonts)) then
    SetFontStyle(Style)
  else
    luaL_ArgError(L, 1, PChar('number from 0 to ' + IntToStr(High(Fonts)) + ' expected'));

  Result := 0;
end;

{ UText.Italic(isItalic: boolean) : sets if font is italic }
function ULuaText_Italic(L: Plua_State): Integer; cdecl;
  var isItalic: Boolean;
begin
  luaL_checkany(L, 1);
  isItalic := lua_toBoolean(L, 1);

  SetFontItalic(isItalic);
  
  Result := 0;
end;

function ULuaText_Color(L: Plua_State): Integer; cdecl;
var
  R, G, B, A: single;
begin
  R := luaL_checknumber(L, 1);
  G := luaL_checknumber(L, 2);
  B := luaL_checknumber(L, 3);
  A := luaL_checknumber(L, 4);
  SetFontColor(R, G, B, A);
end;

{ UText.Width(Text: String) : returns width of Text if printed
  w/ current settings in pixels }
function ULuaText_Width(L: Plua_State): Integer; cdecl;
  var Text: String;
begin
  Text := luaL_checkstring(L, 1);
  lua_pop(L, lua_gettop(L));

  lua_PushNumber(L, TextWidth(Text));

  Result := 1;
end;

{ UText.Print(Text: String) : prints text to screen w/ current
  settings}
function ULuaText_Print(L: Plua_State): Integer; cdecl;
  var Text: String;
begin
  Text := luaL_checkstring(L, 1);

  PrintText(Text);

  Result := 0;
end;

end.
