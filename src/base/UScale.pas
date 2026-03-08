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
 *}

unit UScale;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  dglOpenGL;

type
  TLayoutScaleMode = (lsUniform, lsStretch);

  TScaleDebugInfo = record
    ScreenWidth: integer;
    ScreenHeight: integer;
    LayoutScaleX: single;
    LayoutScaleY: single;
    UniformFitScale: single;
    UniformFillScale: single;
    FitOffsetX: single;
    FitOffsetY: single;
    FillOffsetX: single;
    FillOffsetY: single;
  end;

  TUIScaleState = record
    LayoutScaleX: single;
    LayoutScaleY: single;
    FitScale: single;
    FillScale: single;
    FitScaleX: single;
    FitScaleY: single;
    FillScaleX: single;
    FillScaleY: single;
    FitOffsetX: single;
    FitOffsetY: single;
    FillOffsetX: single;
    FillOffsetY: single;
    FitOffsetPxX: single;
    FitOffsetPxY: single;
    FillOffsetPxX: single;
    FillOffsetPxY: single;
  end;

procedure BeginLayoutSpace;
procedure EndLayoutSpace;
procedure GetScaleDebugInfo(out Info: TScaleDebugInfo);
procedure UpdateUIScaleState(RenderW, RenderH, ScreenW, ScreenH: integer);
function GetLayoutScaleX: single;
function GetLayoutScaleY: single;
function GetUniformScale: single;
procedure ResolveLayoutRect(var X, Y, W, H: single; ScaleMode: TLayoutScaleMode); overload;
procedure ResolveLayoutRect(var X, Y, W, H: real; ScaleMode: TLayoutScaleMode); overload;

implementation

uses
  Math;

var
  UIScaleState: TUIScaleState;
  CurrentScreenW: integer = 0;
  CurrentScreenH: integer = 0;
  CurrentRenderW: integer = 0;
  CurrentRenderH: integer = 0;

procedure BeginLayoutSpace;
begin
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
end;

procedure EndLayoutSpace;
begin
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
end;

procedure GetScaleDebugInfo(out Info: TScaleDebugInfo);
begin
  Info.ScreenWidth := CurrentScreenW;
  Info.ScreenHeight := CurrentScreenH;
  Info.LayoutScaleX := UIScaleState.LayoutScaleX;
  Info.LayoutScaleY := UIScaleState.LayoutScaleY;
  Info.UniformFitScale := UIScaleState.FitScale;
  Info.UniformFillScale := UIScaleState.FillScale;
  Info.FitOffsetX := UIScaleState.FitOffsetPxX;
  Info.FitOffsetY := UIScaleState.FitOffsetPxY;
  Info.FillOffsetX := UIScaleState.FillOffsetPxX;
  Info.FillOffsetY := UIScaleState.FillOffsetPxY;
end;

procedure UpdateUIScaleState(RenderW, RenderH, ScreenW, ScreenH: integer);
var
  LayoutScaleX, LayoutScaleY: single;
  FitScale, FillScale: single;
  FitOffsetPxX, FitOffsetPxY: single;
  FillOffsetPxX, FillOffsetPxY: single;
begin
  if (RenderW = 0) or (RenderH = 0) then
    Exit;

  CurrentScreenW := ScreenW;
  CurrentScreenH := ScreenH;
  CurrentRenderW := RenderW;
  CurrentRenderH := RenderH;

  if ScreenW = 0 then
    LayoutScaleX := 1
  else
    LayoutScaleX := ScreenW / RenderW;

  if ScreenH = 0 then
    LayoutScaleY := 1
  else
    LayoutScaleY := ScreenH / RenderH;

  FitScale := Min(LayoutScaleX, LayoutScaleY);
  FillScale := Max(LayoutScaleX, LayoutScaleY);

  FitOffsetPxX := (ScreenW - FitScale * RenderW) * 0.5;
  FitOffsetPxY := (ScreenH - FitScale * RenderH) * 0.5;
  FillOffsetPxX := (ScreenW - FillScale * RenderW) * 0.5;
  FillOffsetPxY := (ScreenH - FillScale * RenderH) * 0.5;

  UIScaleState.LayoutScaleX := LayoutScaleX;
  UIScaleState.LayoutScaleY := LayoutScaleY;
  UIScaleState.FitScale := FitScale;
  UIScaleState.FillScale := FillScale;

  if LayoutScaleX = 0 then
  begin
    UIScaleState.FitScaleX := 1;
    UIScaleState.FillScaleX := 1;
    UIScaleState.FitOffsetX := 0;
    UIScaleState.FillOffsetX := 0;
  end
  else
  begin
    UIScaleState.FitScaleX := FitScale / LayoutScaleX;
    UIScaleState.FillScaleX := FillScale / LayoutScaleX;
    UIScaleState.FitOffsetX := FitOffsetPxX / LayoutScaleX;
    UIScaleState.FillOffsetX := FillOffsetPxX / LayoutScaleX;
  end;

  if LayoutScaleY = 0 then
  begin
    UIScaleState.FitScaleY := 1;
    UIScaleState.FillScaleY := 1;
    UIScaleState.FitOffsetY := 0;
    UIScaleState.FillOffsetY := 0;
  end
  else
  begin
    UIScaleState.FitScaleY := FitScale / LayoutScaleY;
    UIScaleState.FillScaleY := FillScale / LayoutScaleY;
    UIScaleState.FitOffsetY := FitOffsetPxY / LayoutScaleY;
    UIScaleState.FillOffsetY := FillOffsetPxY / LayoutScaleY;
  end;

  UIScaleState.FitOffsetPxX := FitOffsetPxX;
  UIScaleState.FitOffsetPxY := FitOffsetPxY;
  UIScaleState.FillOffsetPxX := FillOffsetPxX;
  UIScaleState.FillOffsetPxY := FillOffsetPxY;
end;

function GetLayoutScaleX: single;
begin
  Result := UIScaleState.LayoutScaleX;
end;

function GetLayoutScaleY: single;
begin
  Result := UIScaleState.LayoutScaleY;
end;

function GetUniformScale: single;
begin
  Result := UIScaleState.FitScale;
end;

procedure ResolveLayoutRect(var X, Y, W, H: single; ScaleMode: TLayoutScaleMode);
var
  CenterX, CenterY: single;
  ScaleX, ScaleY: single;
begin
  if ScaleMode <> lsUniform then
    Exit;

  if W <> 0 then
    CenterX := X + W * 0.5
  else
    CenterX := X;

  if H <> 0 then
    CenterY := Y + H * 0.5
  else
    CenterY := Y;

  if UIScaleState.LayoutScaleX = 0 then
    ScaleX := 1
  else
    ScaleX := UIScaleState.FitScale / UIScaleState.LayoutScaleX;

  if UIScaleState.LayoutScaleY = 0 then
    ScaleY := 1
  else
    ScaleY := UIScaleState.FitScale / UIScaleState.LayoutScaleY;

  W := W * ScaleX;
  H := H * ScaleY;
  X := CenterX - W * 0.5;
  Y := CenterY - H * 0.5;
end;

procedure ResolveLayoutRect(var X, Y, W, H: real; ScaleMode: TLayoutScaleMode);
var
  SingleX, SingleY, SingleW, SingleH: single;
begin
  SingleX := X;
  SingleY := Y;
  SingleW := W;
  SingleH := H;

  ResolveLayoutRect(SingleX, SingleY, SingleW, SingleH, ScaleMode);

  X := SingleX;
  Y := SingleY;
  W := SingleW;
  H := SingleH;
end;

end.
