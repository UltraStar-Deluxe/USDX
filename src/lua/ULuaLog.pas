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

unit ULuaLog;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  ULog,
  ULua;

function luaopen_Log (L: Plua_State): Integer; cdecl;

function ULuaLog_LogError(L: Plua_State): Integer; cdecl;
function ULuaLog_LogMsg(L: Plua_State): Integer; cdecl;
function ULuaLog_BenchmarkStart(L: Plua_State): Integer; cdecl;
function ULuaLog_BenchmarkEnd(L: Plua_State): Integer; cdecl;
function ULuaLog_LogBenchmark(L: Plua_State): Integer; cdecl;
function ULuaLog_LogDebug(L: Plua_State): Integer; cdecl;
function ULuaLog_LogInfo(L: Plua_State): Integer; cdecl;
function ULuaLog_LogStatus(L: Plua_State): Integer; cdecl;
function ULuaLog_LogWarn(L: Plua_State): Integer; cdecl;
function ULuaLog_LogCritical(L: Plua_State): Integer; cdecl;
function ULuaLog_CriticalError(L: Plua_State): Integer; cdecl;
function ULuaLog_GetLogLevel(L: Plua_State): Integer; cdecl;
function ULuaLog_SetLogLevel(L: Plua_State): Integer; cdecl;


const
  ULuaLog_Lib_f: array [0..13] of lual_reg = (
   (name:'LogError';func:ULuaLog_LogError),
   (name:'LogMsg';func:ULuaLog_LogMsg),
   (name:'BenchmarkStart';func:ULuaLog_BenchmarkStart),
   (name:'BenchmarkEnd';func:ULuaLog_BenchmarkEnd),
   (name:'LogBenchmark';func:ULuaLog_LogBenchmark),
   (name:'LogDebug';func:ULuaLog_LogDebug),
   (name:'LogInfo';func:ULuaLog_LogInfo),
   (name:'LogStatus';func:ULuaLog_LogStatus),
   (name:'LogWarn';func:ULuaLog_LogWarn),
   (name:'LogCritical';func:ULuaLog_LogCritical),
   (name:'CriticalError';func:ULuaLog_CriticalError),
   (name:'SetLogLevel';func:ULuaLog_GetLogLevel),
   (name:'GetLogLevel';func:ULuaLog_SetLogLevel),
   (name:nil;func:nil)
   );

implementation

function ULuaLog_LogError(L: Plua_State): Integer; cdecl;
begin
  if (lua_gettop(L) > 1) then
    Log.LogError(luaL_checkstring(L,-2),luaL_checkstring(L,-1))
  else
    Log.LogError(luaL_checkstring(L,-1));
  result:=0; // number of results
end;

function ULuaLog_LogMsg(L: Plua_State): Integer; cdecl;
begin
  if (lua_gettop(L) > 2) then
    Log.LogMsg(luaL_checkstring(L,-3),luaL_checkstring(L,-1),luaL_checkinteger(L,-2))
  else
    Log.LogMsg(luaL_checkstring(L,-2),luaL_checkinteger(L,-1));
  result:=0; // number of results
end;

function ULuaLog_BenchmarkStart(L: Plua_State): Integer; cdecl;
begin
  Log.BenchmarkStart(luaL_checkinteger(L,-1));
  result:=0; // number of results
end;

function ULuaLog_BenchmarkEnd(L: Plua_State): Integer; cdecl;
begin
  Log.BenchmarkEnd(luaL_checkinteger(L,-1));
  result:=0; // number of results
end;

function ULuaLog_LogBenchmark(L: Plua_State): Integer; cdecl;
begin
  Log.LogBenchmark(luaL_checkstring(L,-2),luaL_checkinteger(L,-1));
  result:=0; // number of results
end;

function ULuaLog_LogDebug(L: Plua_State): Integer; cdecl;
begin
  Log.LogDebug(luaL_checkstring(L,-2),luaL_checkstring(L,-1));
  result:=0; // number of results
end;

function ULuaLog_LogInfo(L: Plua_State): Integer; cdecl;
begin
  Log.LogInfo(luaL_checkstring(L,-2),luaL_checkstring(L,-1));
  result:=0; // number of results
end;

function ULuaLog_LogStatus(L: Plua_State): Integer; cdecl;
begin
  Log.LogStatus(luaL_checkstring(L,-2),luaL_checkstring(L,-1));
  result:=0; // number of results
end;

function ULuaLog_LogWarn(L: Plua_State): Integer; cdecl;
begin
  Log.LogWarn(luaL_checkstring(L,-2),luaL_checkstring(L,-1));
  result:=0; // number of results
end;

function ULuaLog_LogCritical(L: Plua_State): Integer; cdecl;
begin
  Log.LogCritical(luaL_checkstring(L,-2),luaL_checkstring(L,-1));
  result:=0; // number of results
end;

function ULuaLog_CriticalError(L: Plua_State): Integer; cdecl;
begin
  Log.CriticalError(luaL_checkstring(L,-1));
  result:=0; // number of results
end;

function ULuaLog_GetLogLevel(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L,Log.GetLogLevel());
  result:=1; // number of results
end;

function ULuaLog_SetLogLevel(L: Plua_State): Integer; cdecl;
begin
  Log.SetLogLevel(luaL_checkinteger(L,-1));
  result:=0; // number of results
end;

function luaopen_Log (L: Plua_State): Integer; cdecl;
begin
    luaL_register(L,'Log',@ULuaLog_Lib_f[0]);
    result:=1;
end;
end.
