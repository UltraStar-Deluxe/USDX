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
  UThemes,
  UTexture,
  UMenuBackground;

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
  UCommon,
  SysUtils,
  gl,
  glext;

constructor TMenuBackgroundTexture.Create(const ThemedSettings: TThemeBackground);
var texFilename: string;
begin
  inherited;

  if (Length(ThemedSettings.Tex) = 0) then
    raise EMenuBackgroundError.Create('TMenuBackgroundTexture: No texture filename present');

  Color       := ThemedSettings.Color;

  texFilename := Skin.GetTextureFileName(ThemedSettings.Tex);
  texFilename := AdaptFilePaths(texFilename);
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
begin
  glClear(GL_DEPTH_BUFFER_BIT);
  glColorRGB(Color);

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glBindTexture(GL_TEXTURE_2D, Tex.TexNum);

  glBegin(GL_QUADS);
    glTexCoord2f(Tex.TexX1*Tex.TexW, Tex.TexY1*Tex.TexH);
    glVertex2f(0, 0);

    glTexCoord2f(Tex.TexX1*Tex.TexW, Tex.TexY2*Tex.TexH);
    glVertex2f(0, 600);

    glTexCoord2f(Tex.TexX2*Tex.TexW, Tex.TexY2*Tex.TexH);
    glVertex2f(800, 600);

    glTexCoord2f(Tex.TexX2*Tex.TexW, Tex.TexY1*Tex.TexH);
    glVertex2f(800, 0);
  glEnd;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

end.
