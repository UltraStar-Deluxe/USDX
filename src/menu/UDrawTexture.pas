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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/menu/UDrawTexture.pas $
 * $Id: UDrawTexture.pas 1498 2008-11-03 14:53:17Z tobigun $
 *}

unit UDrawTexture;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  dglOpenGL, UTexture, UScale;

procedure DrawLine(X1, Y1, X2, Y2, ColR, ColG, ColB: real);
procedure DrawQuad(X,  Y,  W,  H,  ColR, ColG, ColB: real);
procedure DrawTexture(Texture: TTexture);
procedure ResolveTextureRect(const Texture: TTexture; out X, Y, W, H: real);

implementation

uses
  Math;

procedure DrawTextureEdgeExtend(const Texture: TTexture; BaseX, BaseY, BaseW, BaseH: real);
var
  DrawW: real;
  DrawX: real;
  LeftPad: real;
  RightPad: real;
  ULeft, URight: real;
  VTop, VBottom: real;
  SourceW, SourceH: real;
  LayoutScaleCorrection: real;

  procedure RestoreTextureColor;
  begin
    glEnable(GL_TEXTURE_2D);
    glColor4f(Texture.ColR * Texture.Int, Texture.ColG * Texture.Int, Texture.ColB * Texture.Int, Texture.Alpha);
  end;

  procedure DrawSolidPad(StartX, EndX: real);
  begin
    if EndX <= StartX then
      Exit;

    glDisable(GL_TEXTURE_2D);
    glColor4f(Texture.EdgeExtendFillR, Texture.EdgeExtendFillG, Texture.EdgeExtendFillB, Texture.Alpha);
    glBegin(GL_QUADS);
      glVertex3f(StartX, BaseY, Texture.Z);
      glVertex3f(StartX, BaseY + BaseH, Texture.Z);
      glVertex3f(EndX, BaseY + BaseH, Texture.Z);
      glVertex3f(EndX, BaseY, Texture.Z);
    glEnd;
    RestoreTextureColor;
  end;

  procedure DrawTexturedPad(StartU, EndU: real; StartX, EndX: real);
  begin
    if EndX <= StartX then
      Exit;

    glBegin(GL_QUADS);
      glTexCoord2f(StartU, VTop);
      glVertex3f(StartX, BaseY, Texture.Z);

      glTexCoord2f(StartU, VBottom);
      glVertex3f(StartX, BaseY + BaseH, Texture.Z);

      glTexCoord2f(EndU, VBottom);
      glVertex3f(EndX, BaseY + BaseH, Texture.Z);

      glTexCoord2f(EndU, VTop);
      glVertex3f(EndX, BaseY, Texture.Z);
    glEnd;
  end;

  procedure DrawPad(StartU, EndU, PadStart, PadEnd: real);
  begin
    if Texture.EdgeExtendSolidFill then
      DrawSolidPad(PadStart, PadEnd)
    else
      DrawTexturedPad(StartU, EndU, PadStart, PadEnd);
  end;

var
  LeftOuterU, LeftInnerU: real;
  RightOuterU, RightInnerU: real;
  ActualSourceW, ActualSourceH: real;
  HalfPixelU: real; // inset of half a source pixel to avoid sampling outside image bounds
  OverlapX: real;
begin
  // Some other render paths use GL_REPEAT, we need clamping here
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  SourceW := Texture.SourceW;
  SourceH := Texture.SourceH;

  // stretch of the coordinate system (e.g. 800x600 layout on a 16:9 screen)
  if (GetLayoutScaleX > 0) and (GetLayoutScaleY > 0) then
    LayoutScaleCorrection := GetLayoutScaleY / GetLayoutScaleX
  else
    LayoutScaleCorrection := 1.0;

  // Use the cropped portion of the source for aspect ratio calculation
  ActualSourceW := SourceW * Abs(Texture.TexX2 - Texture.TexX1);
  ActualSourceH := SourceH * Abs(Texture.TexY2 - Texture.TexY1);

  // assume 1:1 aspect ratio for covers/avatars if not set
  if (ActualSourceW <= 0) or (ActualSourceH <= 0) then
  begin
    ActualSourceW := 1.0;
    ActualSourceH := 1.0;
  end
  else
  begin
  end;

  DrawW := (ActualSourceW / ActualSourceH) * BaseH * LayoutScaleCorrection;

  if DrawW > BaseW then
    DrawW := BaseW;

  DrawX := BaseX + (BaseW - DrawW) * 0.5;

  ULeft := Texture.TexX1 * Texture.TexW;
  URight := Texture.TexX2 * Texture.TexW;
  VTop := Texture.TexY1 * Texture.TexH;
  VBottom := Texture.TexY2 * Texture.TexH;

  if (SourceW > 0) then
    HalfPixelU := (0.5 / SourceW) * Abs(Texture.TexX2 - Texture.TexX1) * Texture.TexW
  else
    HalfPixelU := 0;

  if GetLayoutScaleX > 0 then
    OverlapX := 1.0 / GetLayoutScaleX
  else
    OverlapX := 1.0;

  // Sample slightly inside the image to avoid blending with the texture border
  LeftInnerU := Min(ULeft + HalfPixelU, URight);
  LeftOuterU := LeftInnerU;
  RightInnerU := Max(URight - HalfPixelU, ULeft);
  RightOuterU := RightInnerU;

  glBegin(GL_QUADS);
    glTexCoord2f(ULeft, VTop);
    glVertex3f(DrawX, BaseY, Texture.Z);

    glTexCoord2f(ULeft, VBottom);
    glVertex3f(DrawX, BaseY + BaseH, Texture.Z);

    glTexCoord2f(URight, VBottom);
    glVertex3f(DrawX + DrawW, BaseY + BaseH, Texture.Z);

    glTexCoord2f(URight, VTop);
    glVertex3f(DrawX + DrawW, BaseY, Texture.Z);
  glEnd;

  LeftPad := DrawX - BaseX;
  RightPad := BaseX + BaseW - (DrawX + DrawW);

  if LeftPad > 0 then
    DrawPad(LeftOuterU, LeftInnerU, BaseX, Min(DrawX + OverlapX, DrawX + DrawW));

  if RightPad > 0 then
    DrawPad(RightInnerU, RightOuterU, Max(DrawX + DrawW - OverlapX, DrawX), BaseX + BaseW);

  // Restore default wrap so other draw code that expects repeat keeps working.
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
end;

procedure ResolveTextureRect(const Texture: TTexture; out X, Y, W, H: real);
begin
  X := Texture.X;
  Y := Texture.Y;
  W := Texture.W * Texture.ScaleW;
  H := Texture.H * Texture.ScaleH;
  ResolveLayoutRect(X, Y, W, H, Texture.ScaleMode);
end;

procedure DrawLine(X1, Y1, X2, Y2, ColR, ColG, ColB: real);
begin
  glColor3f(ColR, ColG, ColB);
  glBegin(GL_LINES);
    glVertex2f(x1, y1);
    glVertex2f(x2, y2);
  glEnd;
end;

procedure DrawQuad(X, Y, W, H, ColR, ColG, ColB: real);
begin
  glColor3f(ColR, ColG, ColB);
  glBegin(GL_QUADS);
    glVertex2f(x,   y);
    glVertex2f(x,   y+h);
    glVertex2f(x+w, y+h);
    glVertex2f(x+w, y);
  glEnd;
end;

procedure DrawTexture(Texture: TTexture);
var
  x1, x2, x3, x4:     real;
  y1, y2, y3, y4:     real;
  xt1, xt2, xt3, xt4: real;
  yt1, yt2, yt3, yt4: real;
  baseX, baseY, baseW, baseH: real;
begin
  with Texture do
  begin
    ResolveTextureRect(Texture, baseX, baseY, baseW, baseH);

    glColor4f(ColR * Int, ColG * Int, ColB * Int, Alpha);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glDepthRange(0, 10);
    glDepthFunc(GL_LEQUAL);
//    glDepthFunc(GL_GEQUAL);
    glEnable(GL_DEPTH_TEST);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
//    glBlendFunc(GL_SRC_COLOR, GL_ZERO);

    glBindTexture(GL_TEXTURE_2D, TexNum);

    if EdgeExtend then
    begin
      DrawTextureEdgeExtend(Texture, baseX, baseY, baseW, baseH);
      glDisable(GL_DEPTH_TEST);
      glDisable(GL_TEXTURE_2D);
      Exit;
    end;

    x1 := baseX;
    x2 := baseX;
    x3 := baseX + baseW;
    x4 := baseX + baseW;
    y1 := baseY;
    y2 := baseY + baseH;
    y3 := baseY + baseH;
    y4 := baseY;
    if Rot <> 0 then
    begin
      xt1 := x1 - (baseX + baseW/2);
      xt2 := x2 - (baseX + baseW/2);
      xt3 := x3 - (baseX + baseW/2);
      xt4 := x4 - (baseX + baseW/2);
      yt1 := y1 - (baseY + baseH/2);
      yt2 := y2 - (baseY + baseH/2);
      yt3 := y3 - (baseY + baseH/2);
      yt4 := y4 - (baseY + baseH/2);

      x1 := (baseX + baseW/2) + xt1 * cos(Rot) - yt1 * sin(Rot);
      x2 := (baseX + baseW/2) + xt2 * cos(Rot) - yt2 * sin(Rot);
      x3 := (baseX + baseW/2) + xt3 * cos(Rot) - yt3 * sin(Rot);
      x4 := (baseX + baseW/2) + xt4 * cos(Rot) - yt4 * sin(Rot);

      y1 := (baseY + baseH/2) + yt1 * cos(Rot) + xt1 * sin(Rot);
      y2 := (baseY + baseH/2) + yt2 * cos(Rot) + xt2 * sin(Rot);
      y3 := (baseY + baseH/2) + yt3 * cos(Rot) + xt3 * sin(Rot);
      y4 := (baseY + baseH/2) + yt4 * cos(Rot) + xt4 * sin(Rot);

    end;

{
    glBegin(GL_QUADS);
      glTexCoord2f(0,    0);    glVertex3f(x1, y1, z);
      glTexCoord2f(0,    TexH); glVertex3f(x2, y2, z);
      glTexCoord2f(TexW, TexH); glVertex3f(x3, y3, z);
      glTexCoord2f(TexW, 0);    glVertex3f(x4, y4, z);
    glEnd;
}
{
    glBegin(GL_QUADS);
      glTexCoord2f(TexX1*TexW, TexY1*TexH); glVertex3f(x1, y1, z);
      glTexCoord2f(TexX1*TexW, TexY2*TexH); glVertex3f(x2, y2, z);
      glTexCoord2f(TexX2*TexW, TexY2*TexH); glVertex3f(x3, y3, z);
      glTexCoord2f(TexX2*TexW, TexY1*TexH); glVertex3f(x4, y4, z);
    glEnd;
    }
    glBegin(GL_QUADS);
      glTexCoord2f(TexX1*TexW, TexY1*TexH);
      glVertex3f(x1, y1 + (y2 - (LeftScale * (y2))), z);
      glTexCoord2f(TexX1*TexW, TexY2*TexH);
      glVertex3f(x2, y2 - (y2 - (LeftScale * (y2))), z);
      glTexCoord2f(TexX2*TexW, TexY2*TexH);
      glVertex3f(x3, y3 - (y2 - (RightScale * (y2))), z);
      glTexCoord2f(TexX2*TexW, TexY1*TexH);
      glVertex3f(x4, y4 + (y2 - (RightScale * (y2))), z);
    glEnd;

  end;
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_TEXTURE_2D);
end;

end.
