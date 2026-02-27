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
  UTexture;

procedure DrawLine(X1, Y1, X2, Y2, ColR, ColG, ColB: real);
procedure DrawQuad(X,  Y,  W,  H,  ColR, ColG, ColB: real);
procedure DrawTexture(Texture: TTexture);

implementation

uses
  dglOpenGL;

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
begin
  with Texture do
  begin
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

    x1 := x;
    x2 := x;
    x3 := x+w*scaleW;
    x4 := x+w*scaleW;
    y1 := y;
    y2 := y+h*scaleH;
    y3 := y+h*scaleH;
    y4 := y;

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
      glVertex3f(x1, y1, z);
      glTexCoord2f(TexX1*TexW, TexY2*TexH);
      glVertex3f(x2, y2, z);
      glTexCoord2f(TexX2*TexW, TexY2*TexH);
      glVertex3f(x3, y3, z);
      glTexCoord2f(TexX2*TexW, TexY1*TexH);
      glVertex3f(x4, y4, z);
    glEnd;

  end;
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_TEXTURE_2D);
end;

end.
