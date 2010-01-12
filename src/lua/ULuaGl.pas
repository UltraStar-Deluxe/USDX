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

unit ULuaGl;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  gl,
  ULua;

function luaopen_gl (L: Plua_State): Integer; cdecl;
function ULuaGl_StringToEnum(Str: String): GLenum;

{ lua lib functions }
function ULuaGl_Begin(L: Plua_State): Integer; cdecl;
function ULuaGl_BindTexture(L: Plua_State): Integer; cdecl;
function ULuaGl_BlendFunc(L: Plua_State): Integer; cdecl;
function ULuaGl_Clear(L: Plua_State): Integer; cdecl;
function ULuaGl_ClearAccum(L: Plua_State): Integer; cdecl;
function ULuaGl_ClearColor(L: Plua_State): Integer; cdecl;
function ULuaGl_Color(L: Plua_State): Integer; cdecl;
function ULuaGl_CullFace(L: Plua_State): Integer; cdecl;
function ULuaGl_DepthFunc(L: Plua_State): Integer; cdecl;
function ULuaGl_DepthRange(L: Plua_State): Integer; cdecl;
function ULuaGl_Disable(L: Plua_State): Integer; cdecl;
function ULuaGl_DisableClientState(L: Plua_State): Integer; cdecl;
function ULuaGl_DrawBuffer(L: Plua_State): Integer; cdecl;
function ULuaGl_Enable(L: Plua_State): Integer; cdecl;
function ULuaGl_EnableClientState(L: Plua_State): Integer; cdecl;
function ULuaGl_End(L: Plua_State): Integer; cdecl;
function ULuaGl_EndList(L: Plua_State): Integer; cdecl;
function ULuaGl_Finish(L: Plua_State): Integer; cdecl;
function ULuaGl_Flush(L: Plua_State): Integer; cdecl;
function ULuaGl_FrontFace(L: Plua_State): Integer; cdecl;
function ULuaGl_InitNames(L: Plua_State): Integer; cdecl;
function ULuaGl_LoadIdentity(L: Plua_State): Integer; cdecl;
function ULuaGl_LogicOp(L: Plua_State): Integer; cdecl;
function ULuaGl_MatrixMode(L: Plua_State): Integer; cdecl;
function ULuaGl_Ortho(L: Plua_State): Integer; cdecl;
function ULuaGl_PopAttrib(L: Plua_State): Integer; cdecl;
function ULuaGl_PopClientAttrib(L: Plua_State): Integer; cdecl;
function ULuaGl_PopMatrix(L: Plua_State): Integer; cdecl;
function ULuaGl_PopName(L: Plua_State): Integer; cdecl;
function ULuaGl_PushMatrix(L: Plua_State): Integer; cdecl;
function ULuaGl_RasterPos(L: Plua_State): Integer; cdecl;
function ULuaGl_ReadBuffer(L: Plua_State): Integer; cdecl;
function ULuaGl_Rect(L: Plua_State): Integer; cdecl;
function ULuaGl_Rotate(L: Plua_State): Integer; cdecl;
function ULuaGl_Scale(L: Plua_State): Integer; cdecl;
function ULuaGl_ShadeModel(L: Plua_State): Integer; cdecl;
function ULuaGl_TexCoord(L: Plua_State): Integer; cdecl;
function ULuaGl_Translate(L: Plua_State): Integer; cdecl;
function ULuaGl_Vertex(L: Plua_State): Integer; cdecl;
function ULuaGl_Viewport(L: Plua_State): Integer; cdecl;
function ULuaGl_Dummy(L: Plua_State): Integer; cdecl;

const
  ULuaGl_Lib_f: array [0..40] of lual_reg = (
   (name:'Begin';func:ULuaGl_Begin),
   (name:'BindTexture';func:ULuaGl_BindTexture),
   (name:'BlendFunc';func:ULuaGl_BlendFunc),
   (name:'Clear';func:ULuaGl_Clear),
   (name:'ClearAccum';func:ULuaGl_ClearAccum),
   (name:'ClearColor';func:ULuaGl_ClearColor),
   (name:'Color';func:ULuaGl_Color),
   (name:'CullFace';func:ULuaGl_CullFace),
   (name:'DepthFunc';func:ULuaGl_DepthFunc),
   (name:'DepthRange';func:ULuaGl_DepthRange),
   (name:'Disable';func:ULuaGl_Disable),
   (name:'DisableClientState';func:ULuaGl_DisableClientState),
   (name:'DrawBuffer';func:ULuaGl_DrawBuffer),
   (name:'Enable';func:ULuaGl_Enable),
   (name:'EnableClientState';func:ULuaGl_EnableClientState),
   (name:'End';func:ULuaGl_End),
   (name:'EndList';func:ULuaGl_EndList),
   (name:'Finish';func:ULuaGl_Finish),
   (name:'Flush';func:ULuaGl_Flush),
   (name:'FrontFace';func:ULuaGl_FrontFace),
   (name:'InitNames';func:ULuaGl_InitNames),
   (name:'LoadIdentity';func:ULuaGl_LoadIdentity),
   (name:'LogicOp';func:ULuaGl_LogicOp),
   (name:'MatrixMode';func:ULuaGl_MatrixMode),
   (name:'Ortho';func:ULuaGl_Ortho),
   (name:'PopAttrib';func:ULuaGl_PopAttrib),
   (name:'PopClientAttrib';func:ULuaGl_PopClientAttrib),
   (name:'PopMatrix';func:ULuaGl_PopMatrix),
   (name:'PopName';func:ULuaGl_PopName),
   (name:'PushMatrix';func:ULuaGl_PushMatrix),
   (name:'RasterPos';func:ULuaGl_RasterPos),
   (name:'ReadBuffer';func:ULuaGl_ReadBuffer),
   (name:'Rotate';func:ULuaGl_Rotate),
   (name:'Rect';func:ULuaGl_Rect),
   (name:'Scale';func:ULuaGl_Scale),
   (name:'ShadeModel';func:ULuaGl_ShadeModel),
   (name:'TexCoord';func:ULuaGl_TexCoord),
   (name:'Translate';func:ULuaGl_Translate),
   (name:'Vertex';func:ULuaGl_Vertex),
   (name:'Viewport';func:ULuaGl_Viewport),
   (name:nil;func:nil)
   );

implementation

uses
   ULog;

type
  TULuaGl_Enums = record
    Text:       string;
    Value:      GLenum;
  end;
const
  ULuaGl_EnumERROR = $fffffffe;

function ULuaGl_Begin(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.Begin''');

  glBegin(e);

  result:=0; // number of results
end;

function ULuaGl_BindTexture(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.BindTexture''');

  glBindTexture(e,lual_checkinteger(L,2));

  result:=0; // number of results
end;

function ULuaGl_BlendFunc(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
  f : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));
  f := ULuaGl_StringToEnum(lual_checkstring(L,2));

  if (e = ULuaGl_EnumERROR) or (f = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.BlendFunc''');

  glBlendFunc(e,f);

  result:=0; // number of results
end;

function ULuaGl_Clear(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.Clear''');

  glClear(e);

  result:=0; // number of results
end;

function ULuaGl_ClearAccum(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  if lua_istable(L, 1) then
    for i := 1 to lua_objlen(L,1) do
      lua_rawgeti(L,1,i);

  if (lua_istable(L, 1) and (lua_objlen(L,1) = 4)) or (lua_gettop(L) = 4) then
    glClearAccum(lual_checknumber(L,-4),
                 lual_checknumber(L,-3),
                 lual_checknumber(L,-2),
                 lual_checknumber(L,-1))
  else
    luaL_error(L, 'incorrect argument to function ''gl.ClearAccum''');
  result:=0; // number of results
end;

function ULuaGl_ClearColor(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  if lua_istable(L, 1) then
    for i := 1 to lua_objlen(L,1) do
      lua_rawgeti(L,1,i);

  if (lua_istable(L, 1) and (lua_objlen(L,1) = 4)) or (lua_gettop(L) = 4) then
    glClearColor(lual_checknumber(L,-4),
                 lual_checknumber(L,-3),
                 lual_checknumber(L,-2),
                 lual_checknumber(L,-1))
  else
    luaL_error(L, 'incorrect argument to function ''gl.ClearColor''');
  result:=0; // number of results
end;

function ULuaGl_Color(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  if lua_istable(L, 1) then
    for i := 1 to lua_objlen(L,1) do
      lua_rawgeti(L,1,i);

  if (lua_istable(L, 1) and (lua_objlen(L,1) = 3)) or (lua_gettop(L) = 3) then
    glColor3d(GLdouble(lual_checknumber(L,-3)),
              GLdouble(lual_checknumber(L,-2)),
              GLdouble(lual_checknumber(L,-1)))
  else if (lua_istable(L, 1) and (lua_objlen(L,1) = 4)) or (lua_gettop(L) = 4) then
    glColor4d(GLdouble(lual_checknumber(L,-4)),
              GLdouble(lual_checknumber(L,-3)),
              GLdouble(lual_checknumber(L,-2)),
              GLdouble(lual_checknumber(L,-1)))
  else
    luaL_error(L, 'incorrect argument to function ''gl.Color''');
  result:=0; // number of results
end;

function ULuaGl_CullFace(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.CullFace''');

  glCullFace(e);

  result:=0; // number of results
end;

function ULuaGl_DepthFunc(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.DepthFunc''');

  glDepthFunc(e);

  result:=0; // number of results
end;

function ULuaGl_DepthRange(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  if lua_istable(L, 1) then
    for i := 1 to lua_objlen(L,1) do
      lua_rawgeti(L,1,i);

  if  (lua_istable(L, 1) and (lua_objlen(L,1) = 2))
  or  (lua_gettop(L) = 2) then
    glDepthRange(lual_checkinteger(L,-2),
                 lual_checkinteger(L,-1))
  else
    luaL_error(L, 'incorrect argument to function ''gl.DepthRange''');
  result:=0; // number of results
end;

function ULuaGl_Disable(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.Disable''');

  glDisable(e);

  result:=0; // number of results
end;

function ULuaGl_DisableClientState(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.DisableClientState''');

  glDisableClientState(e);

  result:=0; // number of results
end;

function ULuaGl_DrawBuffer(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.DrawBuffer''');

  glDrawBuffer(e);

  result:=0; // number of results
end;

function ULuaGl_Enable(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.Enable''');

  glEnable(e);
  result:=0; // number of results
end;

function ULuaGl_EnableClientState(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.EnableClientState''');

  glEnableClientState(e);

  result:=0; // number of results
end;

function ULuaGl_End(L: Plua_State): Integer; cdecl;
begin
  glEnd();
  result:=0; // number of results
end;

function ULuaGl_EndList(L: Plua_State): Integer; cdecl;
begin
  glEndList();
  result:=0; // number of results
end;

function ULuaGl_Finish(L: Plua_State): Integer; cdecl;
begin
  glFinish();
  result:=0; // number of results
end;

function ULuaGl_Flush(L: Plua_State): Integer; cdecl;
begin
  glFlush();
  result:=0; // number of results
end;

function ULuaGl_FrontFace(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.FrontFace''');

  glFrontFace(e);

  result:=0; // number of results
end;

function ULuaGl_InitNames(L: Plua_State): Integer; cdecl;
begin
  glInitNames();
  result:=0; // number of results
end;

function ULuaGl_LoadIdentity(L: Plua_State): Integer; cdecl;
begin
  glLoadIdentity();
  result:=0; // number of results
end;

function ULuaGl_LogicOp(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.LogicOp''');

  glLogicOp(e);

  result:=0; // number of results
end;

function ULuaGl_MatrixMode(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.MatrixMode''');

  glMatrixMode(e);

  result:=0; // number of results
end;

function ULuaGl_Ortho(L: Plua_State): Integer; cdecl;
begin
  if  (lua_gettop(L) = 6) then
    glOrtho(lual_checkinteger(L,-6),
            lual_checkinteger(L,-5),
            lual_checkinteger(L,-4),
            lual_checkinteger(L,-3),
            lual_checkinteger(L,-2),
            lual_checkinteger(L,-1))
  else
    luaL_error(L, 'incorrect argument to function ''gl.Ortho''');
  result:=0; // number of results
end;

function ULuaGl_PopAttrib(L: Plua_State): Integer; cdecl;
begin
  glPopAttrib();
  result:=0; // number of results
end;

function ULuaGl_PopClientAttrib(L: Plua_State): Integer; cdecl;
begin
  glPopClientAttrib();
  result:=0; // number of results
end;

function ULuaGl_PopMatrix(L: Plua_State): Integer; cdecl;
begin
  glPopMatrix();
  result:=0; // number of results
end;

function ULuaGl_PopName(L: Plua_State): Integer; cdecl;
begin
  glPopName();
  result:=0; // number of results
end;

function ULuaGl_PushMatrix(L: Plua_State): Integer; cdecl;
begin
  glPopName();
  result:=0; // number of results
end;

function ULuaGl_RasterPos(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  if lua_istable(L, 1) then
    for i := 1 to lua_objlen(L,1) do
      lua_rawgeti(L,1,i);

  if (lua_istable(L, 1) and (lua_objlen(L,1) = 2)) or (lua_gettop(L) = 2) then
    glRasterPos2d(GLdouble(lual_checknumber(L,-2)),
                  GLdouble(lual_checknumber(L,-1)))
  else if (lua_istable(L, 1) and (lua_objlen(L,1) = 3)) or (lua_gettop(L) = 3) then
    glRasterPos3d(GLdouble(lual_checknumber(L,-3)),
                  GLdouble(lual_checknumber(L,-2)),
                  GLdouble(lual_checknumber(L,-1)))
  else if (lua_istable(L, 1) and (lua_objlen(L,1) = 4)) or (lua_gettop(L) = 4) then
    glRasterPos4d(GLdouble(lual_checknumber(L,-4)),
                  GLdouble(lual_checknumber(L,-3)),
                  GLdouble(lual_checknumber(L,-2)),
                  GLdouble(lual_checknumber(L,-1)))
  else
    luaL_error(L, 'incorrect argument to function ''gl.RasterPos''');
  result:=0; // number of results
end;

function ULuaGl_ReadBuffer(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.ReadBuffer''');

  glReadBuffer(e);

  result:=0; // number of results
end;

function ULuaGl_Rect(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  if lua_istable(L, 1) and lua_istable(L, 2) then
  begin
    for i := 1 to lua_objlen(L,1) do
      lua_rawgeti(L,1,i);
    for i := 1 to lua_objlen(L,2) do
      lua_rawgeti(L,2,i);
  end;

  if  (lua_istable(L, 1) and (lua_objlen(L,1) = 2))
  and (lua_istable(L, 2) and (lua_objlen(L,2) = 2))
  or  (lua_gettop(L) = 4) then
    glRectD(lual_checknumber(L,-4),
            lual_checknumber(L,-3),
            lual_checknumber(L,-2),
            lual_checknumber(L,-1))
  else
    luaL_error(L, 'incorrect argument to function ''gl.Rect''');
  result:=0; // number of results
end;

function ULuaGl_Rotate(L: Plua_State): Integer; cdecl;
begin
  if  (lua_gettop(L) = 3) then
    glRotated(lual_checkinteger(L,-4),
              lual_checkinteger(L,-3),
              lual_checkinteger(L,-2),
              lual_checkinteger(L,-1))
  else
    luaL_error(L, 'incorrect argument to function ''gl.Rotate''');
  result:=0; // number of results
end;

function ULuaGl_Scale(L: Plua_State): Integer; cdecl;
begin
  if  (lua_gettop(L) = 3) then
    glScaled(lual_checkinteger(L,-3),
             lual_checkinteger(L,-2),
             lual_checkinteger(L,-1))
  else
    luaL_error(L, 'incorrect argument to function ''gl.Scale''');
  result:=0; // number of results
end;

function ULuaGl_ShadeModel(L: Plua_State): Integer; cdecl;
var
  e : GLenum;
begin
  e := ULuaGl_StringToEnum(lual_checkstring(L,1));

  if (e = ULuaGl_EnumERROR) then
    luaL_error(L, 'incorrect string argument to function ''gl.ShadeModel''');

  glShadeModel(e);

  result:=0; // number of results
end;

function ULuaGl_TexCoord(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  if lua_istable(L, 1) then
    for i := 1 to lua_objlen(L,1) do
      lua_rawgeti(L,1,i);

  if (lua_istable(L, 1) and (lua_objlen(L,1) = 1)) or (lua_gettop(L) = 1) then
    glTexCoord1d(GLdouble(lual_checknumber(L,-1)))
  else if (lua_istable(L, 1) and (lua_objlen(L,1) = 2)) or (lua_gettop(L) = 2) then
    glTexCoord2d(GLdouble(lual_checknumber(L,-2)),
                 GLdouble(lual_checknumber(L,-1)))
  else if (lua_istable(L, 1) and (lua_objlen(L,1) = 3)) or (lua_gettop(L) = 3) then
    glTexCoord3d(GLdouble(lual_checknumber(L,-3)),
                 GLdouble(lual_checknumber(L,-2)),
                 GLdouble(lual_checknumber(L,-1)))
  else if (lua_istable(L, 1) and (lua_objlen(L,1) = 4)) or (lua_gettop(L) = 4) then
    glTexCoord4d(GLdouble(lual_checknumber(L,-4)),
                 GLdouble(lual_checknumber(L,-3)),
                 GLdouble(lual_checknumber(L,-2)),
                 GLdouble(lual_checknumber(L,-1)))
  else
    luaL_error(L, 'incorrect argument to function ''gl.TexCoord''');
  result:=0; // number of results
end;

function ULuaGl_Translate(L: Plua_State): Integer; cdecl;
begin
  if  (lua_gettop(L) = 3) then
    glTranslated(lual_checkinteger(L,-3),
                 lual_checkinteger(L,-2),
                 lual_checkinteger(L,-1))
  else
    luaL_error(L, 'incorrect argument to function ''gl.Translate''');
  result:=0; // number of results
end;

function ULuaGl_Vertex(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  if lua_istable(L, 1) then
    for i := 1 to lua_objlen(L,1) do
      lua_rawgeti(L,1,i);

  if (lua_istable(L, 1) and (lua_objlen(L,1) = 2)) or (lua_gettop(L) = 2) then
    glVertex2d(GLdouble(lual_checknumber(L,-2)),
               GLdouble(lual_checknumber(L,-1)))
  else if (lua_istable(L, 1) and (lua_objlen(L,1) = 3)) or (lua_gettop(L) = 3) then
    glVertex3d(GLdouble(lual_checknumber(L,-3)),
               GLdouble(lual_checknumber(L,-2)),
               GLdouble(lual_checknumber(L,-1)))
  else if (lua_istable(L, 1) and (lua_objlen(L,1) = 4)) or (lua_gettop(L) = 4) then
    glVertex4d(GLdouble(lual_checknumber(L,-4)),
               GLdouble(lual_checknumber(L,-3)),
               GLdouble(lual_checknumber(L,-2)),
               GLdouble(lual_checknumber(L,-1)))
  else
    luaL_error(L, 'incorrect argument to function ''gl.Vertex''');
  result:=0; // number of results
end;

function ULuaGl_Viewport(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  if lua_istable(L, 1) and lua_istable(L, 2) then
  begin
    for i := 1 to lua_objlen(L,1) do
      lua_rawgeti(L,1,i);
    for i := 1 to lua_objlen(L,2) do
      lua_rawgeti(L,2,i);
  end;

  if  (lua_istable(L, 1) and (lua_objlen(L,1) = 2))
  and (lua_istable(L, 2) and (lua_objlen(L,2) = 2))
  or  (lua_gettop(L) = 4) then
    glViewport(lual_checkinteger(L,-4),
               lual_checkinteger(L,-3),
               lual_checkinteger(L,-2),
               lual_checkinteger(L,-1))
  else
    luaL_error(L, 'incorrect argument to function ''gl.Viewport''');
  result:=0; // number of results
end;

function ULuaGl_Dummy(L: Plua_State): Integer; cdecl;
begin
  result:=0; // number of results
end;

function luaopen_gl (L: Plua_State): Integer; cdecl;
begin
    luaL_register(L,'gl',@ULuaGl_Lib_f[0]);
    result:=1;
end;

(*
  glAccum: procedure(op: GLenum; value: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glAlphaFunc: procedure(func: GLenum; ref: GLclampf); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glAreTexturesResident: function (n: GLsizei; const textures: PGLuint; residences: PGLboolean): GLboolean; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glArrayElement: procedure(i: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glBitmap: procedure (width, height: GLsizei; xorig, yorig: GLfloat; xmove, ymove: GLfloat; const bitmap: PGLubyte); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCallList: procedure(list: GLuint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCallLists: procedure(n: GLsizei; atype: GLenum; const lists: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glClearDepth: procedure(depth: GLclampd); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glClearIndex: procedure(c: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glClearStencil: procedure(s: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glClipPlane: procedure(plane: GLenum; const equation: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  glColorMask: procedure(red, green, blue, alpha: GLboolean); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glColorMaterial: procedure(face, mode: GLenum); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glColorPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCopyPixels: procedure(x, y: GLint; width, height: GLsizei; atype: GLenum); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCopyTexImage1D: procedure (target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width: GLsizei; border: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCopyTexImage2D: procedure(target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width, height: GLsizei; border: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCopyTexSubImage1D: procedure(target: GLenum; level, xoffset, x, y: GLint; width: GLsizei); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCopyTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset, x, y: GLint; width, height: GLsizei); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDeleteLists: procedure(list: GLuint; range: GLsizei); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDeleteTextures: procedure(n: GLsizei; const textures: PGLuint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDepthMask: procedure(flag: GLboolean); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDrawArrays: procedure(mode: GLenum; first: GLint; count: GLsizei); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDrawElements: procedure(mode: GLenum; count: GLsizei; atype: GLenum; const indices: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDrawPixels: procedure(width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEdgeFlag: procedure(flag: GLboolean); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEdgeFlagPointer: procedure(stride: GLsizei; const pointer: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEdgeFlagv: procedure(const flag: PGLboolean); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  glEvalCoord1d: procedure(u: GLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEvalCoord1dv: procedure(const u: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEvalCoord1f: procedure(u: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEvalCoord1fv: procedure(const u: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEvalCoord2d: procedure(u, v: GLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEvalCoord2dv: procedure(const u: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEvalCoord2f: procedure(u, v: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEvalCoord2fv: procedure(const u: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  glEvalMesh1: procedure(mode: GLenum; i1, i2: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEvalMesh2: procedure(mode: GLenum; i1, i2, j1, j2: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEvalPoint1: procedure(i: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEvalPoint2: procedure(i, j: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glFeedbackBuffer: procedure(size: GLsizei; atype: GLenum; buffer: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glFogf: procedure(pname: GLenum; param: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glFogfv: procedure(pname: GLenum; const params: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glFogi: procedure(pname: GLenum; param: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glFogiv: procedure(pname: GLenum; const params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glFrustum: procedure(left, right, bottom, top, zNear, zFar: GLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGenLists: function(range: GLsizei): GLuint; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGenTextures: procedure(n: GLsizei; textures: PGLuint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetBooleanv: procedure(pname: GLenum; params: PGLboolean); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetClipPlane: procedure(plane: GLenum; equation: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetDoublev: procedure(pname: GLenum; params: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glGetError: function: GLenum; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetFloatv: procedure(pname: GLenum; params: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetIntegerv: procedure(pname: GLenum; params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetLightfv: procedure(light, pname: GLenum; params: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetLightiv: procedure(light, pname: GLenum; params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetMapdv: procedure(target, query: GLenum; v: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetMapfv: procedure(target, query: GLenum; v: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetMapiv: procedure(target, query: GLenum; v: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetMaterialfv: procedure(face, pname: GLenum; params: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetMaterialiv: procedure(face, pname: GLenum; params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetPixelMapfv: procedure(map: GLenum; values: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetPixelMapuiv: procedure(map: GLenum; values: PGLuint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetPixelMapusv: procedure(map: GLenum; values: PGLushort); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetPointerv: procedure(pname: GLenum; params: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetPolygonStipple: procedure(mask: PGLubyte); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
/  glGetString: function(name: GLenum): PChar; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetTexEnvfv: procedure(target, pname: GLenum; params: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetTexEnviv: procedure(target, pname: GLenum; params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetTexGendv: procedure(coord, pname: GLenum; params: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetTexGenfv: procedure(coord, pname: GLenum; params: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetTexGeniv: procedure(coord, pname: GLenum; params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetTexImage: procedure(target: GLenum; level: GLint; format: GLenum; atype: GLenum; pixels: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetTexLevelParameterfv: procedure(target: GLenum; level: GLint; pname: GLenum; params: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetTexLevelParameteriv: procedure(target: GLenum; level: GLint; pname: GLenum; params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetTexParameterfv: procedure(target, pname: GLenum; params: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetTexParameteriv: procedure(target, pname: GLenum; params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glHint: procedure(target, mode: GLenum); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIndexMask: procedure(mask: GLuint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  glIndexPointer: procedure(atype: GLenum; stride: GLsizei; const pointer: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIndexd: procedure(c: GLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIndexdv: procedure(const c: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIndexf: procedure(c: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIndexfv: procedure(const c: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIndexi: procedure(c: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIndexiv: procedure(const c: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIndexs: procedure(c: GLshort); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIndexsv: procedure(const c: PGLshort); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIndexub: procedure(c: GLubyte); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIndexubv: procedure(const c: PGLubyte); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  glInterleavedArrays: procedure(format: GLenum; stride: GLsizei; const pointer: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
/  glIsEnabled: function(cap: GLenum): GLboolean; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIsList: function(list: GLuint): GLboolean; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIsTexture: function(texture: GLuint): GLboolean; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLightModelf: procedure(pname: GLenum; param: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLightModelfv: procedure(pname: GLenum; const params: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLightModeli: procedure(pname: GLenum; param: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLightModeliv: procedure(pname: GLenum; const params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLightf: procedure(light, pname: GLenum; param: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLightfv: procedure(light, pname: GLenum; const params: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLighti: procedure(light, pname: GLenum; param: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLightiv: procedure(light, pname: GLenum; const params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLineStipple: procedure(factor: GLint; pattern: GLushort); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLineWidth: procedure(width: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glListBase: procedure(base: GLuint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLoadMatrixd: procedure(const m: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLoadMatrixf: procedure(const m: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLoadName: procedure(name: GLuint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMap1d: procedure(target: GLenum; u1, u2: GLdouble; stride, order: GLint; const points: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMap1f: procedure(target: GLenum; u1, u2: GLfloat; stride, order: GLint; const points: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMap2d: procedure(target: GLenum; u1, u2: GLdouble; ustride, uorder: GLint; v1, v2: GLdouble; vstride, vorder: GLint; const points: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMap2f: procedure(target: GLenum; u1, u2: GLfloat; ustride, uorder: GLint; v1, v2: GLfloat; vstride, vorder: GLint; const points: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMapGrid1d: procedure(un: GLint; u1, u2: GLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMapGrid1f: procedure(un: GLint; u1, u2: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMapGrid2d: procedure(un: GLint; u1, u2: GLdouble; vn: GLint; v1, v2: GLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMapGrid2f: procedure(un: GLint; u1, u2: GLfloat; vn: GLint; v1, v2: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMaterialf: procedure(face, pname: GLenum; param: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMaterialfv: procedure(face, pname: GLenum; const params: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMateriali: procedure(face, pname: GLenum; param: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMaterialiv: procedure(face, pname: GLenum; const params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMultMatrixd: procedure(const m: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMultMatrixf: procedure(const m: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNewList: procedure(list: GLuint; mode: GLenum); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNormal3b: procedure(nx, ny, nz: GLbyte); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNormal3bv: procedure(const v: PGLbyte); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNormal3d: procedure(nx, ny, nz: GLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNormal3dv: procedure(const v: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNormal3f: procedure(nx, ny, nz: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNormal3fv: procedure(const v: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNormal3i: procedure(nx, ny, nz: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNormal3iv: procedure(const v: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNormal3s: procedure(nx, ny, nz: GLshort); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNormal3sv: procedure(const v: PGLshort); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNormalPointer: procedure(atype: GLenum; stride: GLsizei; const pointer: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPassThrough: procedure(token: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPixelMapfv: procedure(map: GLenum; mapsize: GLsizei; const values: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPixelMapuiv: procedure(map: GLenum; mapsize: GLsizei; const values: PGLuint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPixelMapusv: procedure(map: GLenum; mapsize: GLsizei; const values: PGLushort); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPixelStoref: procedure(pname: GLenum; param: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPixelStorei: procedure(pname: GLenum; param: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPixelTransferf: procedure(pname: GLenum; param: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPixelTransferi: procedure(pname: GLenum; param: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPixelZoom: procedure(xfactor, yfactor: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPointSize: procedure(size: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPolygonMode: procedure(face, mode: GLenum); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPolygonOffset: procedure(factor, units: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPolygonStipple: procedure(const mask: PGLubyte); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPrioritizeTextures: procedure(n: GLsizei; const textures: PGLuint; const priorities: PGLclampf); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPushAttrib: procedure(mask: GLbitfield); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPushClientAttrib: procedure(mask: GLbitfield); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPushName: procedure(name: GLuint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glReadPixels: procedure(x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  glRenderMode: function(mode: GLint): GLint; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glScissor: procedure(x, y: GLint; width, height: GLsizei); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glSelectBuffer: procedure(size: GLsizei; buffer: PGLuint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glStencilFunc: procedure(func: GLenum; ref: GLint; mask: GLuint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glStencilMask: procedure(mask: GLuint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glStencilOp: procedure(fail, zfail, zpass: GLenum); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

  glTexCoordPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexEnvf: procedure(target: GLenum; pname: GLenum; param: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexEnvfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexEnvi: procedure(target: GLenum; pname: GLenum; param: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexEnviv: procedure(target: GLenum; pname: GLenum; const params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexGend: procedure(coord: GLenum; pname: GLenum; param: GLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexGendv: procedure(coord: GLenum; pname: GLenum; const params: PGLdouble); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexGenf: procedure(coord: GLenum; pname: GLenum; param: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexGenfv: procedure(coord: GLenum; pname: GLenum; const params: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexGeni: procedure(coord: GLenum; pname: GLenum; param: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexGeniv: procedure(coord: GLenum; pname: GLenum; const params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexImage1D: procedure(target: GLenum; level, internalformat: GLint; width: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexImage2D: procedure(target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexParameterf: procedure(target: GLenum; pname: GLenum; param: GLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexParameterfv: procedure(target: GLenum; pname: GLenum; const params: PGLfloat); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexParameteri: procedure(target: GLenum; pname: GLenum; param: GLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexParameteriv: procedure(target: GLenum; pname: GLenum; const params: PGLint); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexSubImage1D: procedure(target: GLenum; level, xoffset: GLint; width: GLsizei; format, atype: GLenum; const pixels: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format, atype: GLenum; const pixels: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  glVertexPointer: procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  {$IFDEF WINDOWS}
  ChoosePixelFormat: function(DC: HDC; p2: PPixelFormatDescriptor): Integer; {$IFDEF WINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}
  {$ENDIF}
  *)

  const
  ULuaGl_Enum: array [0..579] of TULuaGl_Enums = (
    (Text:'GL_VERSION_1_1';Value:GL_VERSION_1_1),
    (Text:'GL_ACCUM';Value:GL_ACCUM),
    (Text:'GL_LOAD';Value:GL_LOAD),
    (Text:'GL_RETURN';Value:GL_RETURN),
    (Text:'GL_MULT';Value:GL_MULT),
    (Text:'GL_ADD';Value:GL_ADD),
    (Text:'GL_NEVER';Value:GL_NEVER),
    (Text:'GL_LESS';Value:GL_LESS),
    (Text:'GL_EQUAL';Value:GL_EQUAL),
    (Text:'GL_LEQUAL';Value:GL_LEQUAL),
    (Text:'GL_GREATER';Value:GL_GREATER),
    (Text:'GL_NOTEQUAL';Value:GL_NOTEQUAL),
    (Text:'GL_GEQUAL';Value:GL_GEQUAL),
    (Text:'GL_ALWAYS';Value:GL_ALWAYS),
    (Text:'GL_CURRENT_BIT';Value:GL_CURRENT_BIT),
    (Text:'GL_POINT_BIT';Value:GL_POINT_BIT),
    (Text:'GL_LINE_BIT';Value:GL_LINE_BIT),
    (Text:'GL_POLYGON_BIT';Value:GL_POLYGON_BIT),
    (Text:'GL_POLYGON_STIPPLE_BIT';Value:GL_POLYGON_STIPPLE_BIT),
    (Text:'GL_PIXEL_MODE_BIT';Value:GL_PIXEL_MODE_BIT),
    (Text:'GL_LIGHTING_BIT';Value:GL_LIGHTING_BIT),
    (Text:'GL_FOG_BIT';Value:GL_FOG_BIT),
    (Text:'GL_DEPTH_BUFFER_BIT';Value:GL_DEPTH_BUFFER_BIT),
    (Text:'GL_ACCUM_BUFFER_BIT';Value:GL_ACCUM_BUFFER_BIT),
    (Text:'GL_STENCIL_BUFFER_BIT';Value:GL_STENCIL_BUFFER_BIT),
    (Text:'GL_VIEWPORT_BIT';Value:GL_VIEWPORT_BIT),
    (Text:'GL_TRANSFORM_BIT';Value:GL_TRANSFORM_BIT),
    (Text:'GL_ENABLE_BIT';Value:GL_ENABLE_BIT),
    (Text:'GL_COLOR_BUFFER_BIT';Value:GL_COLOR_BUFFER_BIT),
    (Text:'GL_HINT_BIT';Value:GL_HINT_BIT),
    (Text:'GL_EVAL_BIT';Value:GL_EVAL_BIT),
    (Text:'GL_LIST_BIT';Value:GL_LIST_BIT),
    (Text:'GL_TEXTURE_BIT';Value:GL_TEXTURE_BIT),
    (Text:'GL_SCISSOR_BIT';Value:GL_SCISSOR_BIT),
    (Text:'GL_ALL_ATTRIB_BITS';Value:GL_ALL_ATTRIB_BITS),
    (Text:'GL_POINTS';Value:GL_POINTS),
    (Text:'GL_LINES';Value:GL_LINES),
    (Text:'GL_LINE_LOOP';Value:GL_LINE_LOOP),
    (Text:'GL_LINE_STRIP';Value:GL_LINE_STRIP),
    (Text:'GL_TRIANGLES';Value:GL_TRIANGLES),
    (Text:'GL_TRIANGLE_STRIP';Value:GL_TRIANGLE_STRIP),
    (Text:'GL_TRIANGLE_FAN';Value:GL_TRIANGLE_FAN),
    (Text:'GL_QUADS';Value:GL_QUADS),
    (Text:'GL_QUAD_STRIP';Value:GL_QUAD_STRIP),
    (Text:'GL_POLYGON';Value:GL_POLYGON),
    (Text:'GL_ZERO';Value:GL_ZERO),
    (Text:'GL_ONE';Value:GL_ONE),
    (Text:'GL_SRC_COLOR';Value:GL_SRC_COLOR),
    (Text:'GL_ONE_MINUS_SRC_COLOR';Value:GL_ONE_MINUS_SRC_COLOR),
    (Text:'GL_SRC_ALPHA';Value:GL_SRC_ALPHA),
    (Text:'GL_ONE_MINUS_SRC_ALPHA';Value:GL_ONE_MINUS_SRC_ALPHA),
    (Text:'GL_DST_ALPHA';Value:GL_DST_ALPHA),
    (Text:'GL_ONE_MINUS_DST_ALPHA';Value:GL_ONE_MINUS_DST_ALPHA),
    (Text:'GL_DST_COLOR';Value:GL_DST_COLOR),
    (Text:'GL_ONE_MINUS_DST_COLOR';Value:GL_ONE_MINUS_DST_COLOR),
    (Text:'GL_SRC_ALPHA_SATURATE';Value:GL_SRC_ALPHA_SATURATE),
    (Text:'GL_TRUE';Value:GL_TRUE),
    (Text:'GL_FALSE';Value:GL_FALSE),
    (Text:'GL_CLIP_PLANE0';Value:GL_CLIP_PLANE0),
    (Text:'GL_CLIP_PLANE1';Value:GL_CLIP_PLANE1),
    (Text:'GL_CLIP_PLANE2';Value:GL_CLIP_PLANE2),
    (Text:'GL_CLIP_PLANE3';Value:GL_CLIP_PLANE3),
    (Text:'GL_CLIP_PLANE4';Value:GL_CLIP_PLANE4),
    (Text:'GL_CLIP_PLANE5';Value:GL_CLIP_PLANE5),
    (Text:'GL_BYTE';Value:GL_BYTE),
    (Text:'GL_UNSIGNED_BYTE';Value:GL_UNSIGNED_BYTE),
    (Text:'GL_SHORT';Value:GL_SHORT),
    (Text:'GL_UNSIGNED_SHORT';Value:GL_UNSIGNED_SHORT),
    (Text:'GL_INT';Value:GL_INT),
    (Text:'GL_UNSIGNED_INT';Value:GL_UNSIGNED_INT),
    (Text:'GL_FLOAT';Value:GL_FLOAT),
    (Text:'GL_2_BYTES';Value:GL_2_BYTES),
    (Text:'GL_3_BYTES';Value:GL_3_BYTES),
    (Text:'GL_4_BYTES';Value:GL_4_BYTES),
    (Text:'GL_DOUBLE';Value:GL_DOUBLE),
    (Text:'GL_NONE';Value:GL_NONE),
    (Text:'GL_FRONT_LEFT';Value:GL_FRONT_LEFT),
    (Text:'GL_FRONT_RIGHT';Value:GL_FRONT_RIGHT),
    (Text:'GL_BACK_LEFT';Value:GL_BACK_LEFT),
    (Text:'GL_BACK_RIGHT';Value:GL_BACK_RIGHT),
    (Text:'GL_FRONT';Value:GL_FRONT),
    (Text:'GL_BACK';Value:GL_BACK),
    (Text:'GL_LEFT';Value:GL_LEFT),
    (Text:'GL_RIGHT';Value:GL_RIGHT),
    (Text:'GL_FRONT_AND_BACK';Value:GL_FRONT_AND_BACK),
    (Text:'GL_AUX0';Value:GL_AUX0),
    (Text:'GL_AUX1';Value:GL_AUX1),
    (Text:'GL_AUX2';Value:GL_AUX2),
    (Text:'GL_AUX3';Value:GL_AUX3),
    (Text:'GL_NO_ERROR';Value:GL_NO_ERROR),
    (Text:'GL_INVALID_ENUM';Value:GL_INVALID_ENUM),
    (Text:'GL_INVALID_VALUE';Value:GL_INVALID_VALUE),
    (Text:'GL_INVALID_OPERATION';Value:GL_INVALID_OPERATION),
    (Text:'GL_STACK_OVERFLOW';Value:GL_STACK_OVERFLOW),
    (Text:'GL_STACK_UNDERFLOW';Value:GL_STACK_UNDERFLOW),
    (Text:'GL_OUT_OF_MEMORY';Value:GL_OUT_OF_MEMORY),
    (Text:'GL_2D';Value:GL_2D),
    (Text:'GL_3D';Value:GL_3D),
    (Text:'GL_3D_COLOR';Value:GL_3D_COLOR),
    (Text:'GL_3D_COLOR_TEXTURE';Value:GL_3D_COLOR_TEXTURE),
    (Text:'GL_4D_COLOR_TEXTURE';Value:GL_4D_COLOR_TEXTURE),
    (Text:'GL_PASS_THROUGH_TOKEN';Value:GL_PASS_THROUGH_TOKEN),
    (Text:'GL_POINT_TOKEN';Value:GL_POINT_TOKEN),
    (Text:'GL_LINE_TOKEN';Value:GL_LINE_TOKEN),
    (Text:'GL_POLYGON_TOKEN';Value:GL_POLYGON_TOKEN),
    (Text:'GL_BITMAP_TOKEN';Value:GL_BITMAP_TOKEN),
    (Text:'GL_DRAW_PIXEL_TOKEN';Value:GL_DRAW_PIXEL_TOKEN),
    (Text:'GL_COPY_PIXEL_TOKEN';Value:GL_COPY_PIXEL_TOKEN),
    (Text:'GL_LINE_RESET_TOKEN';Value:GL_LINE_RESET_TOKEN),
    (Text:'GL_EXP';Value:GL_EXP),
    (Text:'GL_EXP2';Value:GL_EXP2),
    (Text:'GL_CW';Value:GL_CW),
    (Text:'GL_CCW';Value:GL_CCW),
    (Text:'GL_COEFF';Value:GL_COEFF),
    (Text:'GL_ORDER';Value:GL_ORDER),
    (Text:'GL_DOMAIN';Value:GL_DOMAIN),
    (Text:'GL_CURRENT_COLOR';Value:GL_CURRENT_COLOR),
    (Text:'GL_CURRENT_INDEX';Value:GL_CURRENT_INDEX),
    (Text:'GL_CURRENT_NORMAL';Value:GL_CURRENT_NORMAL),
    (Text:'GL_CURRENT_TEXTURE_COORDS';Value:GL_CURRENT_TEXTURE_COORDS),
    (Text:'GL_CURRENT_RASTER_COLOR';Value:GL_CURRENT_RASTER_COLOR),
    (Text:'GL_CURRENT_RASTER_INDEX';Value:GL_CURRENT_RASTER_INDEX),
    (Text:'GL_CURRENT_RASTER_TEXTURE_COORDS';Value:GL_CURRENT_RASTER_TEXTURE_COORDS),
    (Text:'GL_CURRENT_RASTER_POSITION';Value:GL_CURRENT_RASTER_POSITION),
    (Text:'GL_CURRENT_RASTER_POSITION_VALID';Value:GL_CURRENT_RASTER_POSITION_VALID),
    (Text:'GL_CURRENT_RASTER_DISTANCE';Value:GL_CURRENT_RASTER_DISTANCE),
    (Text:'GL_POINT_SMOOTH';Value:GL_POINT_SMOOTH),
    (Text:'GL_POINT_SIZE';Value:GL_POINT_SIZE),
    (Text:'GL_POINT_SIZE_RANGE';Value:GL_POINT_SIZE_RANGE),
    (Text:'GL_POINT_SIZE_GRANULARITY';Value:GL_POINT_SIZE_GRANULARITY),
    (Text:'GL_LINE_SMOOTH';Value:GL_LINE_SMOOTH),
    (Text:'GL_LINE_WIDTH';Value:GL_LINE_WIDTH),
    (Text:'GL_LINE_WIDTH_RANGE';Value:GL_LINE_WIDTH_RANGE),
    (Text:'GL_LINE_WIDTH_GRANULARITY';Value:GL_LINE_WIDTH_GRANULARITY),
    (Text:'GL_LINE_STIPPLE';Value:GL_LINE_STIPPLE),
    (Text:'GL_LINE_STIPPLE_PATTERN';Value:GL_LINE_STIPPLE_PATTERN),
    (Text:'GL_LINE_STIPPLE_REPEAT';Value:GL_LINE_STIPPLE_REPEAT),
    (Text:'GL_LIST_MODE';Value:GL_LIST_MODE),
    (Text:'GL_MAX_LIST_NESTING';Value:GL_MAX_LIST_NESTING),
    (Text:'GL_LIST_BASE';Value:GL_LIST_BASE),
    (Text:'GL_LIST_INDEX';Value:GL_LIST_INDEX),
    (Text:'GL_POLYGON_MODE';Value:GL_POLYGON_MODE),
    (Text:'GL_POLYGON_SMOOTH';Value:GL_POLYGON_SMOOTH),
    (Text:'GL_POLYGON_STIPPLE';Value:GL_POLYGON_STIPPLE),
    (Text:'GL_EDGE_FLAG';Value:GL_EDGE_FLAG),
    (Text:'GL_CULL_FACE';Value:GL_CULL_FACE),
    (Text:'GL_CULL_FACE_MODE';Value:GL_CULL_FACE_MODE),
    (Text:'GL_FRONT_FACE';Value:GL_FRONT_FACE),
    (Text:'GL_LIGHTING';Value:GL_LIGHTING),
    (Text:'GL_LIGHT_MODEL_LOCAL_VIEWER';Value:GL_LIGHT_MODEL_LOCAL_VIEWER),
    (Text:'GL_LIGHT_MODEL_TWO_SIDE';Value:GL_LIGHT_MODEL_TWO_SIDE),
    (Text:'GL_LIGHT_MODEL_AMBIENT';Value:GL_LIGHT_MODEL_AMBIENT),
    (Text:'GL_SHADE_MODEL';Value:GL_SHADE_MODEL),
    (Text:'GL_COLOR_MATERIAL_FACE';Value:GL_COLOR_MATERIAL_FACE),
    (Text:'GL_COLOR_MATERIAL_PARAMETER';Value:GL_COLOR_MATERIAL_PARAMETER),
    (Text:'GL_COLOR_MATERIAL';Value:GL_COLOR_MATERIAL),
    (Text:'GL_FOG';Value:GL_FOG),
    (Text:'GL_FOG_INDEX';Value:GL_FOG_INDEX),
    (Text:'GL_FOG_DENSITY';Value:GL_FOG_DENSITY),
    (Text:'GL_FOG_START';Value:GL_FOG_START),
    (Text:'GL_FOG_END';Value:GL_FOG_END),
    (Text:'GL_FOG_MODE';Value:GL_FOG_MODE),
    (Text:'GL_FOG_COLOR';Value:GL_FOG_COLOR),
    (Text:'GL_DEPTH_RANGE';Value:GL_DEPTH_RANGE),
    (Text:'GL_DEPTH_TEST';Value:GL_DEPTH_TEST),
    (Text:'GL_DEPTH_WRITEMASK';Value:GL_DEPTH_WRITEMASK),
    (Text:'GL_DEPTH_CLEAR_VALUE';Value:GL_DEPTH_CLEAR_VALUE),
    (Text:'GL_DEPTH_FUNC';Value:GL_DEPTH_FUNC),
    (Text:'GL_ACCUM_CLEAR_VALUE';Value:GL_ACCUM_CLEAR_VALUE),
    (Text:'GL_STENCIL_TEST';Value:GL_STENCIL_TEST),
    (Text:'GL_STENCIL_CLEAR_VALUE';Value:GL_STENCIL_CLEAR_VALUE),
    (Text:'GL_STENCIL_FUNC';Value:GL_STENCIL_FUNC),
    (Text:'GL_STENCIL_VALUE_MASK';Value:GL_STENCIL_VALUE_MASK),
    (Text:'GL_STENCIL_FAIL';Value:GL_STENCIL_FAIL),
    (Text:'GL_STENCIL_PASS_DEPTH_FAIL';Value:GL_STENCIL_PASS_DEPTH_FAIL),
    (Text:'GL_STENCIL_PASS_DEPTH_PASS';Value:GL_STENCIL_PASS_DEPTH_PASS),
    (Text:'GL_STENCIL_REF';Value:GL_STENCIL_REF),
    (Text:'GL_STENCIL_WRITEMASK';Value:GL_STENCIL_WRITEMASK),
    (Text:'GL_MATRIX_MODE';Value:GL_MATRIX_MODE),
    (Text:'GL_NORMALIZE';Value:GL_NORMALIZE),
    (Text:'GL_VIEWPORT';Value:GL_VIEWPORT),
    (Text:'GL_MODELVIEW_STACK_DEPTH';Value:GL_MODELVIEW_STACK_DEPTH),
    (Text:'GL_PROJECTION_STACK_DEPTH';Value:GL_PROJECTION_STACK_DEPTH),
    (Text:'GL_TEXTURE_STACK_DEPTH';Value:GL_TEXTURE_STACK_DEPTH),
    (Text:'GL_MODELVIEW_MATRIX';Value:GL_MODELVIEW_MATRIX),
    (Text:'GL_PROJECTION_MATRIX';Value:GL_PROJECTION_MATRIX),
    (Text:'GL_TEXTURE_MATRIX';Value:GL_TEXTURE_MATRIX),
    (Text:'GL_ATTRIB_STACK_DEPTH';Value:GL_ATTRIB_STACK_DEPTH),
    (Text:'GL_CLIENT_ATTRIB_STACK_DEPTH';Value:GL_CLIENT_ATTRIB_STACK_DEPTH),
    (Text:'GL_ALPHA_TEST';Value:GL_ALPHA_TEST),
    (Text:'GL_ALPHA_TEST_FUNC';Value:GL_ALPHA_TEST_FUNC),
    (Text:'GL_ALPHA_TEST_REF';Value:GL_ALPHA_TEST_REF),
    (Text:'GL_DITHER';Value:GL_DITHER),
    (Text:'GL_BLEND_DST';Value:GL_BLEND_DST),
    (Text:'GL_BLEND_SRC';Value:GL_BLEND_SRC),
    (Text:'GL_BLEND';Value:GL_BLEND),
    (Text:'GL_LOGIC_OP_MODE';Value:GL_LOGIC_OP_MODE),
    (Text:'GL_INDEX_LOGIC_OP';Value:GL_INDEX_LOGIC_OP),
    (Text:'GL_COLOR_LOGIC_OP';Value:GL_COLOR_LOGIC_OP),
    (Text:'GL_AUX_BUFFERS';Value:GL_AUX_BUFFERS),
    (Text:'GL_DRAW_BUFFER';Value:GL_DRAW_BUFFER),
    (Text:'GL_READ_BUFFER';Value:GL_READ_BUFFER),
    (Text:'GL_SCISSOR_BOX';Value:GL_SCISSOR_BOX),
    (Text:'GL_SCISSOR_TEST';Value:GL_SCISSOR_TEST),
    (Text:'GL_INDEX_CLEAR_VALUE';Value:GL_INDEX_CLEAR_VALUE),
    (Text:'GL_INDEX_WRITEMASK';Value:GL_INDEX_WRITEMASK),
    (Text:'GL_COLOR_CLEAR_VALUE';Value:GL_COLOR_CLEAR_VALUE),
    (Text:'GL_COLOR_WRITEMASK';Value:GL_COLOR_WRITEMASK),
    (Text:'GL_INDEX_MODE';Value:GL_INDEX_MODE),
    (Text:'GL_RGBA_MODE';Value:GL_RGBA_MODE),
    (Text:'GL_DOUBLEBUFFER';Value:GL_DOUBLEBUFFER),
    (Text:'GL_STEREO';Value:GL_STEREO),
    (Text:'GL_RENDER_MODE';Value:GL_RENDER_MODE),
    (Text:'GL_PERSPECTIVE_CORRECTION_HINT';Value:GL_PERSPECTIVE_CORRECTION_HINT),
    (Text:'GL_POINT_SMOOTH_HINT';Value:GL_POINT_SMOOTH_HINT),
    (Text:'GL_LINE_SMOOTH_HINT';Value:GL_LINE_SMOOTH_HINT),
    (Text:'GL_POLYGON_SMOOTH_HINT';Value:GL_POLYGON_SMOOTH_HINT),
    (Text:'GL_FOG_HINT';Value:GL_FOG_HINT),
    (Text:'GL_TEXTURE_GEN_S';Value:GL_TEXTURE_GEN_S),
    (Text:'GL_TEXTURE_GEN_T';Value:GL_TEXTURE_GEN_T),
    (Text:'GL_TEXTURE_GEN_R';Value:GL_TEXTURE_GEN_R),
    (Text:'GL_TEXTURE_GEN_Q';Value:GL_TEXTURE_GEN_Q),
    (Text:'GL_PIXEL_MAP_I_TO_I';Value:GL_PIXEL_MAP_I_TO_I),
    (Text:'GL_PIXEL_MAP_S_TO_S';Value:GL_PIXEL_MAP_S_TO_S),
    (Text:'GL_PIXEL_MAP_I_TO_R';Value:GL_PIXEL_MAP_I_TO_R),
    (Text:'GL_PIXEL_MAP_I_TO_G';Value:GL_PIXEL_MAP_I_TO_G),
    (Text:'GL_PIXEL_MAP_I_TO_B';Value:GL_PIXEL_MAP_I_TO_B),
    (Text:'GL_PIXEL_MAP_I_TO_A';Value:GL_PIXEL_MAP_I_TO_A),
    (Text:'GL_PIXEL_MAP_R_TO_R';Value:GL_PIXEL_MAP_R_TO_R),
    (Text:'GL_PIXEL_MAP_G_TO_G';Value:GL_PIXEL_MAP_G_TO_G),
    (Text:'GL_PIXEL_MAP_B_TO_B';Value:GL_PIXEL_MAP_B_TO_B),
    (Text:'GL_PIXEL_MAP_A_TO_A';Value:GL_PIXEL_MAP_A_TO_A),
    (Text:'GL_PIXEL_MAP_I_TO_I_SIZE';Value:GL_PIXEL_MAP_I_TO_I_SIZE),
    (Text:'GL_PIXEL_MAP_S_TO_S_SIZE';Value:GL_PIXEL_MAP_S_TO_S_SIZE),
    (Text:'GL_PIXEL_MAP_I_TO_R_SIZE';Value:GL_PIXEL_MAP_I_TO_R_SIZE),
    (Text:'GL_PIXEL_MAP_I_TO_G_SIZE';Value:GL_PIXEL_MAP_I_TO_G_SIZE),
    (Text:'GL_PIXEL_MAP_I_TO_B_SIZE';Value:GL_PIXEL_MAP_I_TO_B_SIZE),
    (Text:'GL_PIXEL_MAP_I_TO_A_SIZE';Value:GL_PIXEL_MAP_I_TO_A_SIZE),
    (Text:'GL_PIXEL_MAP_R_TO_R_SIZE';Value:GL_PIXEL_MAP_R_TO_R_SIZE),
    (Text:'GL_PIXEL_MAP_G_TO_G_SIZE';Value:GL_PIXEL_MAP_G_TO_G_SIZE),
    (Text:'GL_PIXEL_MAP_B_TO_B_SIZE';Value:GL_PIXEL_MAP_B_TO_B_SIZE),
    (Text:'GL_PIXEL_MAP_A_TO_A_SIZE';Value:GL_PIXEL_MAP_A_TO_A_SIZE),
    (Text:'GL_UNPACK_SWAP_BYTES';Value:GL_UNPACK_SWAP_BYTES),
    (Text:'GL_UNPACK_LSB_FIRST';Value:GL_UNPACK_LSB_FIRST),
    (Text:'GL_UNPACK_ROW_LENGTH';Value:GL_UNPACK_ROW_LENGTH),
    (Text:'GL_UNPACK_SKIP_ROWS';Value:GL_UNPACK_SKIP_ROWS),
    (Text:'GL_UNPACK_SKIP_PIXELS';Value:GL_UNPACK_SKIP_PIXELS),
    (Text:'GL_UNPACK_ALIGNMENT';Value:GL_UNPACK_ALIGNMENT),
    (Text:'GL_PACK_SWAP_BYTES';Value:GL_PACK_SWAP_BYTES),
    (Text:'GL_PACK_LSB_FIRST';Value:GL_PACK_LSB_FIRST),
    (Text:'GL_PACK_ROW_LENGTH';Value:GL_PACK_ROW_LENGTH),
    (Text:'GL_PACK_SKIP_ROWS';Value:GL_PACK_SKIP_ROWS),
    (Text:'GL_PACK_SKIP_PIXELS';Value:GL_PACK_SKIP_PIXELS),
    (Text:'GL_PACK_ALIGNMENT';Value:GL_PACK_ALIGNMENT),
    (Text:'GL_MAP_COLOR';Value:GL_MAP_COLOR),
    (Text:'GL_MAP_STENCIL';Value:GL_MAP_STENCIL),
    (Text:'GL_INDEX_SHIFT';Value:GL_INDEX_SHIFT),
    (Text:'GL_INDEX_OFFSET';Value:GL_INDEX_OFFSET),
    (Text:'GL_RED_SCALE';Value:GL_RED_SCALE),
    (Text:'GL_RED_BIAS';Value:GL_RED_BIAS),
    (Text:'GL_ZOOM_X';Value:GL_ZOOM_X),
    (Text:'GL_ZOOM_Y';Value:GL_ZOOM_Y),
    (Text:'GL_GREEN_SCALE';Value:GL_GREEN_SCALE),
    (Text:'GL_GREEN_BIAS';Value:GL_GREEN_BIAS),
    (Text:'GL_BLUE_SCALE';Value:GL_BLUE_SCALE),
    (Text:'GL_BLUE_BIAS';Value:GL_BLUE_BIAS),
    (Text:'GL_ALPHA_SCALE';Value:GL_ALPHA_SCALE),
    (Text:'GL_ALPHA_BIAS';Value:GL_ALPHA_BIAS),
    (Text:'GL_DEPTH_SCALE';Value:GL_DEPTH_SCALE),
    (Text:'GL_DEPTH_BIAS';Value:GL_DEPTH_BIAS),
    (Text:'GL_MAX_EVAL_ORDER';Value:GL_MAX_EVAL_ORDER),
    (Text:'GL_MAX_LIGHTS';Value:GL_MAX_LIGHTS),
    (Text:'GL_MAX_CLIP_PLANES';Value:GL_MAX_CLIP_PLANES),
    (Text:'GL_MAX_TEXTURE_SIZE';Value:GL_MAX_TEXTURE_SIZE),
    (Text:'GL_MAX_PIXEL_MAP_TABLE';Value:GL_MAX_PIXEL_MAP_TABLE),
    (Text:'GL_MAX_ATTRIB_STACK_DEPTH';Value:GL_MAX_ATTRIB_STACK_DEPTH),
    (Text:'GL_MAX_MODELVIEW_STACK_DEPTH';Value:GL_MAX_MODELVIEW_STACK_DEPTH),
    (Text:'GL_MAX_NAME_STACK_DEPTH';Value:GL_MAX_NAME_STACK_DEPTH),
    (Text:'GL_MAX_PROJECTION_STACK_DEPTH';Value:GL_MAX_PROJECTION_STACK_DEPTH),
    (Text:'GL_MAX_TEXTURE_STACK_DEPTH';Value:GL_MAX_TEXTURE_STACK_DEPTH),
    (Text:'GL_MAX_VIEWPORT_DIMS';Value:GL_MAX_VIEWPORT_DIMS),
    (Text:'GL_MAX_CLIENT_ATTRIB_STACK_DEPTH';Value:GL_MAX_CLIENT_ATTRIB_STACK_DEPTH),
    (Text:'GL_SUBPIXEL_BITS';Value:GL_SUBPIXEL_BITS),
    (Text:'GL_INDEX_BITS';Value:GL_INDEX_BITS),
    (Text:'GL_RED_BITS';Value:GL_RED_BITS),
    (Text:'GL_GREEN_BITS';Value:GL_GREEN_BITS),
    (Text:'GL_BLUE_BITS';Value:GL_BLUE_BITS),
    (Text:'GL_ALPHA_BITS';Value:GL_ALPHA_BITS),
    (Text:'GL_DEPTH_BITS';Value:GL_DEPTH_BITS),
    (Text:'GL_STENCIL_BITS';Value:GL_STENCIL_BITS),
    (Text:'GL_ACCUM_RED_BITS';Value:GL_ACCUM_RED_BITS),
    (Text:'GL_ACCUM_GREEN_BITS';Value:GL_ACCUM_GREEN_BITS),
    (Text:'GL_ACCUM_BLUE_BITS';Value:GL_ACCUM_BLUE_BITS),
    (Text:'GL_ACCUM_ALPHA_BITS';Value:GL_ACCUM_ALPHA_BITS),
    (Text:'GL_NAME_STACK_DEPTH';Value:GL_NAME_STACK_DEPTH),
    (Text:'GL_AUTO_NORMAL';Value:GL_AUTO_NORMAL),
    (Text:'GL_MAP1_COLOR_4';Value:GL_MAP1_COLOR_4),
    (Text:'GL_MAP1_INDEX';Value:GL_MAP1_INDEX),
    (Text:'GL_MAP1_NORMAL';Value:GL_MAP1_NORMAL),
    (Text:'GL_MAP1_TEXTURE_COORD_1';Value:GL_MAP1_TEXTURE_COORD_1),
    (Text:'GL_MAP1_TEXTURE_COORD_2';Value:GL_MAP1_TEXTURE_COORD_2),
    (Text:'GL_MAP1_TEXTURE_COORD_3';Value:GL_MAP1_TEXTURE_COORD_3),
    (Text:'GL_MAP1_TEXTURE_COORD_4';Value:GL_MAP1_TEXTURE_COORD_4),
    (Text:'GL_MAP1_VERTEX_3';Value:GL_MAP1_VERTEX_3),
    (Text:'GL_MAP1_VERTEX_4';Value:GL_MAP1_VERTEX_4),
    (Text:'GL_MAP2_COLOR_4';Value:GL_MAP2_COLOR_4),
    (Text:'GL_MAP2_INDEX';Value:GL_MAP2_INDEX),
    (Text:'GL_MAP2_NORMAL';Value:GL_MAP2_NORMAL),
    (Text:'GL_MAP2_TEXTURE_COORD_1';Value:GL_MAP2_TEXTURE_COORD_1),
    (Text:'GL_MAP2_TEXTURE_COORD_2';Value:GL_MAP2_TEXTURE_COORD_2),
    (Text:'GL_MAP2_TEXTURE_COORD_3';Value:GL_MAP2_TEXTURE_COORD_3),
    (Text:'GL_MAP2_TEXTURE_COORD_4';Value:GL_MAP2_TEXTURE_COORD_4),
    (Text:'GL_MAP2_VERTEX_3';Value:GL_MAP2_VERTEX_3),
    (Text:'GL_MAP2_VERTEX_4';Value:GL_MAP2_VERTEX_4),
    (Text:'GL_MAP1_GRID_DOMAIN';Value:GL_MAP1_GRID_DOMAIN),
    (Text:'GL_MAP1_GRID_SEGMENTS';Value:GL_MAP1_GRID_SEGMENTS),
    (Text:'GL_MAP2_GRID_DOMAIN';Value:GL_MAP2_GRID_DOMAIN),
    (Text:'GL_MAP2_GRID_SEGMENTS';Value:GL_MAP2_GRID_SEGMENTS),
    (Text:'GL_TEXTURE_1D';Value:GL_TEXTURE_1D),
    (Text:'GL_TEXTURE_2D';Value:GL_TEXTURE_2D),
    (Text:'GL_FEEDBACK_BUFFER_POINTER';Value:GL_FEEDBACK_BUFFER_POINTER),
    (Text:'GL_FEEDBACK_BUFFER_SIZE';Value:GL_FEEDBACK_BUFFER_SIZE),
    (Text:'GL_FEEDBACK_BUFFER_TYPE';Value:GL_FEEDBACK_BUFFER_TYPE),
    (Text:'GL_SELECTION_BUFFER_POINTER';Value:GL_SELECTION_BUFFER_POINTER),
    (Text:'GL_SELECTION_BUFFER_SIZE';Value:GL_SELECTION_BUFFER_SIZE),
    (Text:'GL_TEXTURE_WIDTH';Value:GL_TEXTURE_WIDTH),
    (Text:'GL_TEXTURE_HEIGHT';Value:GL_TEXTURE_HEIGHT),
    (Text:'GL_TEXTURE_INTERNAL_FORMAT';Value:GL_TEXTURE_INTERNAL_FORMAT),
    (Text:'GL_TEXTURE_BORDER_COLOR';Value:GL_TEXTURE_BORDER_COLOR),
    (Text:'GL_TEXTURE_BORDER';Value:GL_TEXTURE_BORDER),
    (Text:'GL_DONT_CARE';Value:GL_DONT_CARE),
    (Text:'GL_FASTEST';Value:GL_FASTEST),
    (Text:'GL_NICEST';Value:GL_NICEST),
    (Text:'GL_LIGHT0';Value:GL_LIGHT0),
    (Text:'GL_LIGHT1';Value:GL_LIGHT1),
    (Text:'GL_LIGHT2';Value:GL_LIGHT2),
    (Text:'GL_LIGHT3';Value:GL_LIGHT3),
    (Text:'GL_LIGHT4';Value:GL_LIGHT4),
    (Text:'GL_LIGHT5';Value:GL_LIGHT5),
    (Text:'GL_LIGHT6';Value:GL_LIGHT6),
    (Text:'GL_LIGHT7';Value:GL_LIGHT7),
    (Text:'GL_AMBIENT';Value:GL_AMBIENT),
    (Text:'GL_DIFFUSE';Value:GL_DIFFUSE),
    (Text:'GL_SPECULAR';Value:GL_SPECULAR),
    (Text:'GL_POSITION';Value:GL_POSITION),
    (Text:'GL_SPOT_DIRECTION';Value:GL_SPOT_DIRECTION),
    (Text:'GL_SPOT_EXPONENT';Value:GL_SPOT_EXPONENT),
    (Text:'GL_SPOT_CUTOFF';Value:GL_SPOT_CUTOFF),
    (Text:'GL_CONSTANT_ATTENUATION';Value:GL_CONSTANT_ATTENUATION),
    (Text:'GL_LINEAR_ATTENUATION';Value:GL_LINEAR_ATTENUATION),
    (Text:'GL_QUADRATIC_ATTENUATION';Value:GL_QUADRATIC_ATTENUATION),
    (Text:'GL_COMPILE';Value:GL_COMPILE),
    (Text:'GL_COMPILE_AND_EXECUTE';Value:GL_COMPILE_AND_EXECUTE),
    (Text:'GL_CLEAR';Value:GL_CLEAR),
    (Text:'GL_AND';Value:GL_AND),
    (Text:'GL_AND_REVERSE';Value:GL_AND_REVERSE),
    (Text:'GL_COPY';Value:GL_COPY),
    (Text:'GL_AND_INVERTED';Value:GL_AND_INVERTED),
    (Text:'GL_NOOP';Value:GL_NOOP),
    (Text:'GL_XOR';Value:GL_XOR),
    (Text:'GL_OR';Value:GL_OR),
    (Text:'GL_NOR';Value:GL_NOR),
    (Text:'GL_EQUIV';Value:GL_EQUIV),
    (Text:'GL_INVERT';Value:GL_INVERT),
    (Text:'GL_OR_REVERSE';Value:GL_OR_REVERSE),
    (Text:'GL_COPY_INVERTED';Value:GL_COPY_INVERTED),
    (Text:'GL_OR_INVERTED';Value:GL_OR_INVERTED),
    (Text:'GL_NAND';Value:GL_NAND),
    (Text:'GL_SET';Value:GL_SET),
    (Text:'GL_EMISSION';Value:GL_EMISSION),
    (Text:'GL_SHININESS';Value:GL_SHININESS),
    (Text:'GL_AMBIENT_AND_DIFFUSE';Value:GL_AMBIENT_AND_DIFFUSE),
    (Text:'GL_COLOR_INDEXES';Value:GL_COLOR_INDEXES),
    (Text:'GL_MODELVIEW';Value:GL_MODELVIEW),
    (Text:'GL_PROJECTION';Value:GL_PROJECTION),
    (Text:'GL_TEXTURE';Value:GL_TEXTURE),
    (Text:'GL_COLOR';Value:GL_COLOR),
    (Text:'GL_DEPTH';Value:GL_DEPTH),
    (Text:'GL_STENCIL';Value:GL_STENCIL),
    (Text:'GL_COLOR_INDEX';Value:GL_COLOR_INDEX),
    (Text:'GL_STENCIL_INDEX';Value:GL_STENCIL_INDEX),
    (Text:'GL_DEPTH_COMPONENT';Value:GL_DEPTH_COMPONENT),
    (Text:'GL_RED';Value:GL_RED),
    (Text:'GL_GREEN';Value:GL_GREEN),
    (Text:'GL_BLUE';Value:GL_BLUE),
    (Text:'GL_ALPHA';Value:GL_ALPHA),
    (Text:'GL_RGB';Value:GL_RGB),
    (Text:'GL_RGBA';Value:GL_RGBA),
    (Text:'GL_LUMINANCE';Value:GL_LUMINANCE),
    (Text:'GL_LUMINANCE_ALPHA';Value:GL_LUMINANCE_ALPHA),
    (Text:'GL_BITMAP';Value:GL_BITMAP),
    (Text:'GL_POINT';Value:GL_POINT),
    (Text:'GL_LINE';Value:GL_LINE),
    (Text:'GL_FILL';Value:GL_FILL),
    (Text:'GL_RENDER';Value:GL_RENDER),
    (Text:'GL_FEEDBACK';Value:GL_FEEDBACK),
    (Text:'GL_SELECT';Value:GL_SELECT),
    (Text:'GL_FLAT';Value:GL_FLAT),
    (Text:'GL_SMOOTH';Value:GL_SMOOTH),
    (Text:'GL_KEEP';Value:GL_KEEP),
    (Text:'GL_REPLACE';Value:GL_REPLACE),
    (Text:'GL_INCR';Value:GL_INCR),
    (Text:'GL_DECR';Value:GL_DECR),
    (Text:'GL_VENDOR';Value:GL_VENDOR),
    (Text:'GL_RENDERER';Value:GL_RENDERER),
    (Text:'GL_VERSION';Value:GL_VERSION),
    (Text:'GL_EXTENSIONS';Value:GL_EXTENSIONS),
    (Text:'GL_S';Value:GL_S),
    (Text:'GL_T';Value:GL_T),
    (Text:'GL_R';Value:GL_R),
    (Text:'GL_Q';Value:GL_Q),
    (Text:'GL_MODULATE';Value:GL_MODULATE),
    (Text:'GL_DECAL';Value:GL_DECAL),
    (Text:'GL_TEXTURE_ENV_MODE';Value:GL_TEXTURE_ENV_MODE),
    (Text:'GL_TEXTURE_ENV_COLOR';Value:GL_TEXTURE_ENV_COLOR),
    (Text:'GL_TEXTURE_ENV';Value:GL_TEXTURE_ENV),
    (Text:'GL_EYE_LINEAR';Value:GL_EYE_LINEAR),
    (Text:'GL_OBJECT_LINEAR';Value:GL_OBJECT_LINEAR),
    (Text:'GL_SPHERE_MAP';Value:GL_SPHERE_MAP),
    (Text:'GL_TEXTURE_GEN_MODE';Value:GL_TEXTURE_GEN_MODE),
    (Text:'GL_OBJECT_PLANE';Value:GL_OBJECT_PLANE),
    (Text:'GL_EYE_PLANE';Value:GL_EYE_PLANE),
    (Text:'GL_NEAREST';Value:GL_NEAREST),
    (Text:'GL_LINEAR';Value:GL_LINEAR),
    (Text:'GL_NEAREST_MIPMAP_NEAREST';Value:GL_NEAREST_MIPMAP_NEAREST),
    (Text:'GL_LINEAR_MIPMAP_NEAREST';Value:GL_LINEAR_MIPMAP_NEAREST),
    (Text:'GL_NEAREST_MIPMAP_LINEAR';Value:GL_NEAREST_MIPMAP_LINEAR),
    (Text:'GL_LINEAR_MIPMAP_LINEAR';Value:GL_LINEAR_MIPMAP_LINEAR),
    (Text:'GL_TEXTURE_MAG_FILTER';Value:GL_TEXTURE_MAG_FILTER),
    (Text:'GL_TEXTURE_MIN_FILTER';Value:GL_TEXTURE_MIN_FILTER),
    (Text:'GL_TEXTURE_WRAP_S';Value:GL_TEXTURE_WRAP_S),
    (Text:'GL_TEXTURE_WRAP_T';Value:GL_TEXTURE_WRAP_T),
    (Text:'GL_CLAMP';Value:GL_CLAMP),
    (Text:'GL_REPEAT';Value:GL_REPEAT),
    (Text:'GL_CLIENT_PIXEL_STORE_BIT';Value:GL_CLIENT_PIXEL_STORE_BIT),
    (Text:'GL_CLIENT_VERTEX_ARRAY_BIT';Value:GL_CLIENT_VERTEX_ARRAY_BIT),
    (Text:'GL_CLIENT_ALL_ATTRIB_BITS';Value:GL_CLIENT_ALL_ATTRIB_BITS),
    (Text:'GL_POLYGON_OFFSET_FACTOR';Value:GL_POLYGON_OFFSET_FACTOR),
    (Text:'GL_POLYGON_OFFSET_UNITS';Value:GL_POLYGON_OFFSET_UNITS),
    (Text:'GL_POLYGON_OFFSET_POINT';Value:GL_POLYGON_OFFSET_POINT),
    (Text:'GL_POLYGON_OFFSET_LINE';Value:GL_POLYGON_OFFSET_LINE),
    (Text:'GL_POLYGON_OFFSET_FILL';Value:GL_POLYGON_OFFSET_FILL),
    (Text:'GL_ALPHA4';Value:GL_ALPHA4),
    (Text:'GL_ALPHA8';Value:GL_ALPHA8),
    (Text:'GL_ALPHA12';Value:GL_ALPHA12),
    (Text:'GL_ALPHA16';Value:GL_ALPHA16),
    (Text:'GL_LUMINANCE4';Value:GL_LUMINANCE4),
    (Text:'GL_LUMINANCE8';Value:GL_LUMINANCE8),
    (Text:'GL_LUMINANCE12';Value:GL_LUMINANCE12),
    (Text:'GL_LUMINANCE16';Value:GL_LUMINANCE16),
    (Text:'GL_LUMINANCE4_ALPHA4';Value:GL_LUMINANCE4_ALPHA4),
    (Text:'GL_LUMINANCE6_ALPHA2';Value:GL_LUMINANCE6_ALPHA2),
    (Text:'GL_LUMINANCE8_ALPHA8';Value:GL_LUMINANCE8_ALPHA8),
    (Text:'GL_LUMINANCE12_ALPHA4';Value:GL_LUMINANCE12_ALPHA4),
    (Text:'GL_LUMINANCE12_ALPHA12';Value:GL_LUMINANCE12_ALPHA12),
    (Text:'GL_LUMINANCE16_ALPHA16';Value:GL_LUMINANCE16_ALPHA16),
    (Text:'GL_INTENSITY';Value:GL_INTENSITY),
    (Text:'GL_INTENSITY4';Value:GL_INTENSITY4),
    (Text:'GL_INTENSITY8';Value:GL_INTENSITY8),
    (Text:'GL_INTENSITY12';Value:GL_INTENSITY12),
    (Text:'GL_INTENSITY16';Value:GL_INTENSITY16),
    (Text:'GL_R3_G3_B2';Value:GL_R3_G3_B2),
    (Text:'GL_RGB4';Value:GL_RGB4),
    (Text:'GL_RGB5';Value:GL_RGB5),
    (Text:'GL_RGB8';Value:GL_RGB8),
    (Text:'GL_RGB10';Value:GL_RGB10),
    (Text:'GL_RGB12';Value:GL_RGB12),
    (Text:'GL_RGB16';Value:GL_RGB16),
    (Text:'GL_RGBA2';Value:GL_RGBA2),
    (Text:'GL_RGBA4';Value:GL_RGBA4),
    (Text:'GL_RGB5_A1';Value:GL_RGB5_A1),
    (Text:'GL_RGBA8';Value:GL_RGBA8),
    (Text:'GL_RGB10_A2';Value:GL_RGB10_A2),
    (Text:'GL_RGBA12';Value:GL_RGBA12),
    (Text:'GL_RGBA16';Value:GL_RGBA16),
    (Text:'GL_TEXTURE_RED_SIZE';Value:GL_TEXTURE_RED_SIZE),
    (Text:'GL_TEXTURE_GREEN_SIZE';Value:GL_TEXTURE_GREEN_SIZE),
    (Text:'GL_TEXTURE_BLUE_SIZE';Value:GL_TEXTURE_BLUE_SIZE),
    (Text:'GL_TEXTURE_ALPHA_SIZE';Value:GL_TEXTURE_ALPHA_SIZE),
    (Text:'GL_TEXTURE_LUMINANCE_SIZE';Value:GL_TEXTURE_LUMINANCE_SIZE),
    (Text:'GL_TEXTURE_INTENSITY_SIZE';Value:GL_TEXTURE_INTENSITY_SIZE),
    (Text:'GL_PROXY_TEXTURE_1D';Value:GL_PROXY_TEXTURE_1D),
    (Text:'GL_PROXY_TEXTURE_2D';Value:GL_PROXY_TEXTURE_2D),
    (Text:'GL_TEXTURE_PRIORITY';Value:GL_TEXTURE_PRIORITY),
    (Text:'GL_TEXTURE_RESIDENT';Value:GL_TEXTURE_RESIDENT),
    (Text:'GL_TEXTURE_BINDING_1D';Value:GL_TEXTURE_BINDING_1D),
    (Text:'GL_TEXTURE_BINDING_2D';Value:GL_TEXTURE_BINDING_2D),
    (Text:'GL_VERTEX_ARRAY';Value:GL_VERTEX_ARRAY),
    (Text:'GL_NORMAL_ARRAY';Value:GL_NORMAL_ARRAY),
    (Text:'GL_COLOR_ARRAY';Value:GL_COLOR_ARRAY),
    (Text:'GL_INDEX_ARRAY';Value:GL_INDEX_ARRAY),
    (Text:'GL_TEXTURE_COORD_ARRAY';Value:GL_TEXTURE_COORD_ARRAY),
    (Text:'GL_EDGE_FLAG_ARRAY';Value:GL_EDGE_FLAG_ARRAY),
    (Text:'GL_VERTEX_ARRAY_SIZE';Value:GL_VERTEX_ARRAY_SIZE),
    (Text:'GL_VERTEX_ARRAY_TYPE';Value:GL_VERTEX_ARRAY_TYPE),
    (Text:'GL_VERTEX_ARRAY_STRIDE';Value:GL_VERTEX_ARRAY_STRIDE),
    (Text:'GL_NORMAL_ARRAY_TYPE';Value:GL_NORMAL_ARRAY_TYPE),
    (Text:'GL_NORMAL_ARRAY_STRIDE';Value:GL_NORMAL_ARRAY_STRIDE),
    (Text:'GL_COLOR_ARRAY_SIZE';Value:GL_COLOR_ARRAY_SIZE),
    (Text:'GL_COLOR_ARRAY_TYPE';Value:GL_COLOR_ARRAY_TYPE),
    (Text:'GL_COLOR_ARRAY_STRIDE';Value:GL_COLOR_ARRAY_STRIDE),
    (Text:'GL_INDEX_ARRAY_TYPE';Value:GL_INDEX_ARRAY_TYPE),
    (Text:'GL_INDEX_ARRAY_STRIDE';Value:GL_INDEX_ARRAY_STRIDE),
    (Text:'GL_TEXTURE_COORD_ARRAY_SIZE';Value:GL_TEXTURE_COORD_ARRAY_SIZE),
    (Text:'GL_TEXTURE_COORD_ARRAY_TYPE';Value:GL_TEXTURE_COORD_ARRAY_TYPE),
    (Text:'GL_TEXTURE_COORD_ARRAY_STRIDE';Value:GL_TEXTURE_COORD_ARRAY_STRIDE),
    (Text:'GL_EDGE_FLAG_ARRAY_STRIDE';Value:GL_EDGE_FLAG_ARRAY_STRIDE),
    (Text:'GL_VERTEX_ARRAY_POINTER';Value:GL_VERTEX_ARRAY_POINTER),
    (Text:'GL_NORMAL_ARRAY_POINTER';Value:GL_NORMAL_ARRAY_POINTER),
    (Text:'GL_COLOR_ARRAY_POINTER';Value:GL_COLOR_ARRAY_POINTER),
    (Text:'GL_INDEX_ARRAY_POINTER';Value:GL_INDEX_ARRAY_POINTER),
    (Text:'GL_TEXTURE_COORD_ARRAY_POINTER';Value:GL_TEXTURE_COORD_ARRAY_POINTER),
    (Text:'GL_EDGE_FLAG_ARRAY_POINTER';Value:GL_EDGE_FLAG_ARRAY_POINTER),
    (Text:'GL_V2F';Value:GL_V2F),
    (Text:'GL_V3F';Value:GL_V3F),
    (Text:'GL_C4UB_V2F';Value:GL_C4UB_V2F),
    (Text:'GL_C4UB_V3F';Value:GL_C4UB_V3F),
    (Text:'GL_C3F_V3F';Value:GL_C3F_V3F),
    (Text:'GL_N3F_V3F';Value:GL_N3F_V3F),
    (Text:'GL_C4F_N3F_V3F';Value:GL_C4F_N3F_V3F),
    (Text:'GL_T2F_V3F';Value:GL_T2F_V3F),
    (Text:'GL_T4F_V4F';Value:GL_T4F_V4F),
    (Text:'GL_T2F_C4UB_V3F';Value:GL_T2F_C4UB_V3F),
    (Text:'GL_T2F_C3F_V3F';Value:GL_T2F_C3F_V3F),
    (Text:'GL_T2F_N3F_V3F';Value:GL_T2F_N3F_V3F),
    (Text:'GL_T2F_C4F_N3F_V3F';Value:GL_T2F_C4F_N3F_V3F),
    (Text:'GL_T4F_C4F_N3F_V4F';Value:GL_T4F_C4F_N3F_V4F),
    (Text:'GL_EXT_vertex_array';Value:GL_EXT_vertex_array),
    (Text:'GL_WIN_swap_hint';Value:GL_WIN_swap_hint),
    (Text:'GL_EXT_bgra';Value:GL_EXT_bgra),
    (Text:'GL_EXT_paletted_texture';Value:GL_EXT_paletted_texture),
    (Text:'GL_VERTEX_ARRAY_EXT';Value:GL_VERTEX_ARRAY_EXT),
    (Text:'GL_NORMAL_ARRAY_EXT';Value:GL_NORMAL_ARRAY_EXT),
    (Text:'GL_COLOR_ARRAY_EXT';Value:GL_COLOR_ARRAY_EXT),
    (Text:'GL_INDEX_ARRAY_EXT';Value:GL_INDEX_ARRAY_EXT),
    (Text:'GL_TEXTURE_COORD_ARRAY_EXT';Value:GL_TEXTURE_COORD_ARRAY_EXT),
    (Text:'GL_EDGE_FLAG_ARRAY_EXT';Value:GL_EDGE_FLAG_ARRAY_EXT),
    (Text:'GL_VERTEX_ARRAY_SIZE_EXT';Value:GL_VERTEX_ARRAY_SIZE_EXT),
    (Text:'GL_VERTEX_ARRAY_TYPE_EXT';Value:GL_VERTEX_ARRAY_TYPE_EXT),
    (Text:'GL_VERTEX_ARRAY_STRIDE_EXT';Value:GL_VERTEX_ARRAY_STRIDE_EXT),
    (Text:'GL_VERTEX_ARRAY_COUNT_EXT';Value:GL_VERTEX_ARRAY_COUNT_EXT),
    (Text:'GL_NORMAL_ARRAY_TYPE_EXT';Value:GL_NORMAL_ARRAY_TYPE_EXT),
    (Text:'GL_NORMAL_ARRAY_STRIDE_EXT';Value:GL_NORMAL_ARRAY_STRIDE_EXT),
    (Text:'GL_NORMAL_ARRAY_COUNT_EXT';Value:GL_NORMAL_ARRAY_COUNT_EXT),
    (Text:'GL_COLOR_ARRAY_SIZE_EXT';Value:GL_COLOR_ARRAY_SIZE_EXT),
    (Text:'GL_COLOR_ARRAY_TYPE_EXT';Value:GL_COLOR_ARRAY_TYPE_EXT),
    (Text:'GL_COLOR_ARRAY_STRIDE_EXT';Value:GL_COLOR_ARRAY_STRIDE_EXT),
    (Text:'GL_COLOR_ARRAY_COUNT_EXT';Value:GL_COLOR_ARRAY_COUNT_EXT),
    (Text:'GL_INDEX_ARRAY_TYPE_EXT';Value:GL_INDEX_ARRAY_TYPE_EXT),
    (Text:'GL_INDEX_ARRAY_STRIDE_EXT';Value:GL_INDEX_ARRAY_STRIDE_EXT),
    (Text:'GL_INDEX_ARRAY_COUNT_EXT';Value:GL_INDEX_ARRAY_COUNT_EXT),
    (Text:'GL_TEXTURE_COORD_ARRAY_SIZE_EXT';Value:GL_TEXTURE_COORD_ARRAY_SIZE_EXT),
    (Text:'GL_TEXTURE_COORD_ARRAY_TYPE_EXT';Value:GL_TEXTURE_COORD_ARRAY_TYPE_EXT),
    (Text:'GL_TEXTURE_COORD_ARRAY_STRIDE_EXT';Value:GL_TEXTURE_COORD_ARRAY_STRIDE_EXT),
    (Text:'GL_TEXTURE_COORD_ARRAY_COUNT_EXT';Value:GL_TEXTURE_COORD_ARRAY_COUNT_EXT),
    (Text:'GL_EDGE_FLAG_ARRAY_STRIDE_EXT';Value:GL_EDGE_FLAG_ARRAY_STRIDE_EXT),
    (Text:'GL_EDGE_FLAG_ARRAY_COUNT_EXT';Value:GL_EDGE_FLAG_ARRAY_COUNT_EXT),
    (Text:'GL_VERTEX_ARRAY_POINTER_EXT';Value:GL_VERTEX_ARRAY_POINTER_EXT),
    (Text:'GL_NORMAL_ARRAY_POINTER_EXT';Value:GL_NORMAL_ARRAY_POINTER_EXT),
    (Text:'GL_COLOR_ARRAY_POINTER_EXT';Value:GL_COLOR_ARRAY_POINTER_EXT),
    (Text:'GL_INDEX_ARRAY_POINTER_EXT';Value:GL_INDEX_ARRAY_POINTER_EXT),
    (Text:'GL_TEXTURE_COORD_ARRAY_POINTER_EXT';Value:GL_TEXTURE_COORD_ARRAY_POINTER_EXT),
    (Text:'GL_EDGE_FLAG_ARRAY_POINTER_EXT';Value:GL_EDGE_FLAG_ARRAY_POINTER_EXT),
    (Text:'GL_DOUBLE_EXT';Value:GL_DOUBLE_EXT),
    (Text:'GL_BGR_EXT';Value:GL_BGR_EXT),
    (Text:'GL_BGRA_EXT';Value:GL_BGRA_EXT),
    (Text:'GL_COLOR_TABLE_FORMAT_EXT';Value:GL_COLOR_TABLE_FORMAT_EXT),
    (Text:'GL_COLOR_TABLE_WIDTH_EXT';Value:GL_COLOR_TABLE_WIDTH_EXT),
    (Text:'GL_COLOR_TABLE_RED_SIZE_EXT';Value:GL_COLOR_TABLE_RED_SIZE_EXT),
    (Text:'GL_COLOR_TABLE_GREEN_SIZE_EXT';Value:GL_COLOR_TABLE_GREEN_SIZE_EXT),
    (Text:'GL_COLOR_TABLE_BLUE_SIZE_EXT';Value:GL_COLOR_TABLE_BLUE_SIZE_EXT),
    (Text:'GL_COLOR_TABLE_ALPHA_SIZE_EXT';Value:GL_COLOR_TABLE_ALPHA_SIZE_EXT),
    (Text:'GL_COLOR_TABLE_LUMINANCE_SIZE_EXT';Value:GL_COLOR_TABLE_LUMINANCE_SIZE_EXT),
    (Text:'GL_COLOR_TABLE_INTENSITY_SIZE_EXT';Value:GL_COLOR_TABLE_INTENSITY_SIZE_EXT),
    (Text:'GL_COLOR_INDEX1_EXT';Value:GL_COLOR_INDEX1_EXT),
    (Text:'GL_COLOR_INDEX2_EXT';Value:GL_COLOR_INDEX2_EXT),
    (Text:'GL_COLOR_INDEX4_EXT';Value:GL_COLOR_INDEX4_EXT),
    (Text:'GL_COLOR_INDEX8_EXT';Value:GL_COLOR_INDEX8_EXT),
    (Text:'GL_COLOR_INDEX12_EXT';Value:GL_COLOR_INDEX12_EXT),
    (Text:'GL_COLOR_INDEX16_EXT';Value:GL_COLOR_INDEX16_EXT)
   );

function ULuaGl_StringToEnum(Str: String): GLenum;
  function GetEnum(const Str: String): GLenum;
  var
    i : Integer;
  begin
    for i := 0 to high(ULuaGl_Enum) do
    begin
      if 0 = AnsiCompareText(Str, ULuaGl_Enum[i].Text) then
      begin
        Result := ULuaGl_Enum[i].Value;
        Exit;
      end;
    end;
    Result := ULuaGl_EnumERROR;
  end;
  var
    i : Integer;
    j : Integer;
    temp : GLenum;
begin
  Result := 0;
  j := 1;
  for i := 1 to Length(Str) do
  begin
    if Str[i] = ',' then
    begin
      temp := GetEnum(Copy(Str,j,i-j));
      if temp <> ULuaGl_EnumERROR then
        Result := Result or temp;
      j := i + 1;
    end;
  end;

  temp := GetEnum(Copy(Str,j,MaxInt));
  if (temp = ULuaGl_EnumERROR) then
  begin
    if Result = 0 then
      Result := ULuaGl_EnumERROR;
    exit;
  end;
  Result := Result or temp;
end;
end.

