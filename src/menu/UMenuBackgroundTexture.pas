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

unit UMenuBackgroundTexture;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UCommon,
  UThemes,
  UTexture,
  UMenuBackground,
  UPath;

//TMenuBackgroundColor - Background Color
//--------

type
  TMenuBackgroundTexture = class (TMenuBackground)
    private
      Tex: TTexture;
      Color: TRGB;
    public
      constructor Create(const ThemedSettings: TThemeBackground); override;
      procedure   Draw; override;
      destructor  Destroy; override;
  end;

const
  SUPPORTED_EXTS_BACKGROUNDTEXTURE: array[0..13] of string = ('.png', '.bmp', '.jpg', '.jpeg', '.gif', '.pnm', '.ppm', '.pgm', '.pbm', '.xpm', '.lbm', '.pcx', '.tga', '.tiff');

implementation
uses
  USkins,
  SysUtils,
  dglOpenGL,
  UGraphic,
  UScale,
  Math;

constructor TMenuBackgroundTexture.Create(const ThemedSettings: TThemeBackground);
var
  texFilename: IPath;
begin
  inherited;

  if (Length(ThemedSettings.Tex) = 0) then
    raise EMenuBackgroundError.Create('TMenuBackgroundTexture: No texture filename present');

  Color       := ThemedSettings.Color;

  texFilename := Skin.GetTextureFileName(ThemedSettings.Tex);
  Tex         := Texture.GetTexture(texFilename, TEXTURE_TYPE_PLAIN);

  if (Tex.TexNum = 0) then
  begin
    freeandnil(Tex);
    raise EMenuBackgroundError.Create('TMenuBackgroundTexture: Can''t load texture');
  end;

end;

destructor  TMenuBackgroundTexture.Destroy;
begin
  //freeandnil(Tex); <- this causes an Access Violation o0
  inherited;
end;

procedure   TMenuBackgroundTexture.Draw;
const
  PAD_OVERLAP = 1.0;
  EDGE_SAMPLE_PIXELS = 1.0;
  EDGE_BLUR_PIXELS = 16.0;
var
  RenderWidth, RenderHeight: real;
  Scale: real;
  ScaleXFactor: real;
  DrawW: real;
  DrawX: real;
  LeftPad: real;
  RightPad: real;
  ULeft, URight, VTop, VBottom: real;
  EdgeU: real;
  BlurU: real;
  LeftInnerU, LeftOuterU: real;
  RightInnerU, RightOuterU: real;

  procedure DrawEdgeStrip(const PadStart, PadEnd, StartU, EndU: real);
  begin
    if PadEnd <= PadStart then
      Exit;

    glBegin(GL_QUADS);
      glTexCoord2f(StartU, VTop);
      glVertex2f(PadStart, 0);

      glTexCoord2f(StartU, VBottom);
      glVertex2f(PadStart, RenderHeight);

      glTexCoord2f(EndU, VBottom);
      glVertex2f(PadEnd, RenderHeight);

      glTexCoord2f(EndU, VTop);
      glVertex2f(PadEnd, 0);
    glEnd;
  end;

begin
  If (ScreenAct = 1) then //Clear just once when in dual screen mode
    glClear(GL_DEPTH_BUFFER_BIT);
    
  glColorRGB(Color);

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glBindTexture(GL_TEXTURE_2D, Tex.TexNum);

  RenderWidth := RenderW;
  RenderHeight := RenderH;
  if (RenderWidth = 0) then
    RenderWidth := 800;
  if (RenderHeight = 0) then
    RenderHeight := 600;

  if (Tex.H <= 0) then
    Scale := 1
  else
    Scale := RenderHeight / Tex.H;

  if GetLayoutScaleX = 0 then
    ScaleXFactor := Scale
  else
    ScaleXFactor := Scale * (GetLayoutScaleY / GetLayoutScaleX);

  DrawW := Tex.W * ScaleXFactor;
  DrawX := (RenderWidth - DrawW) * 0.5;

  ULeft := Tex.TexX1 * Tex.TexW;
  URight := Tex.TexX2 * Tex.TexW;
  VTop := Tex.TexY1 * Tex.TexH;
  VBottom := Tex.TexY2 * Tex.TexH;

  if (Tex.W > 0) then
  begin
    EdgeU := (EDGE_SAMPLE_PIXELS / Tex.W) * Tex.TexW;
    BlurU := (EDGE_BLUR_PIXELS / Tex.W) * Tex.TexW;
  end
  else
  begin
    EdgeU := 0;
    BlurU := 0;
  end;

  if EdgeU = 0 then
  begin
    LeftInnerU := ULeft;
    RightInnerU := URight;
  end
  else
  begin
    LeftInnerU := Min(ULeft + EdgeU * 0.5, URight);
    RightInnerU := Max(URight - EdgeU * 0.5, ULeft);
  end;

  if BlurU <= EdgeU then
    BlurU := EdgeU * 2;

  if BlurU > 0 then
  begin
    LeftOuterU := Min(ULeft + BlurU, URight);
    RightOuterU := Max(URight - BlurU, ULeft);
  end
  else
  begin
    LeftOuterU := LeftInnerU;
    RightOuterU := RightInnerU;
  end;

  glBegin(GL_QUADS);
    glTexCoord2f(ULeft, VTop);
    glVertex2f(DrawX, 0);

    glTexCoord2f(ULeft, VBottom);
    glVertex2f(DrawX, RenderHeight);

    glTexCoord2f(URight, VBottom);
    glVertex2f(DrawX + DrawW, RenderHeight);

    glTexCoord2f(URight, VTop);
    glVertex2f(DrawX + DrawW, 0);
  glEnd;

  LeftPad := Max(DrawX, 0);
  RightPad := Max(RenderWidth - (DrawX + DrawW), 0);

  if (EdgeU > 0) and (LeftPad > 0) then
    DrawEdgeStrip(0, Min(LeftPad + PAD_OVERLAP, RenderWidth - RightPad), LeftOuterU, LeftInnerU);

  if (EdgeU > 0) and (RightPad > 0) then
    DrawEdgeStrip(Max(RenderWidth - RightPad - PAD_OVERLAP, LeftPad), RenderWidth, RightInnerU, RightOuterU);

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end;

end.
