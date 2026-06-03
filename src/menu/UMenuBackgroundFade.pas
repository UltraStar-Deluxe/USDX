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

unit UMenuBackgroundFade;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UCommon,
  UThemes,
  UMenuBackground,
  UPath,
  URenderer;

//TMenuBackgroundFade - Background Fade In for Overlay screens
//--------

type
  TMenuBackgroundFade = class (TMenuBackground)
    private
      Tex:        TTexture;
      Color:      TRGB;
      Alpha:      real;
      
      useTexture: boolean;

      FadeTime:   cardinal;
    public
      constructor Create(const ThemedSettings: TThemeBackground); override;
      procedure   OnShow; override;
      procedure   Draw; override;
      destructor  Destroy; override;
  end;

const
  FADEINTIME = 1500; //Time the bg fades in

implementation
uses
  sdl2,
  USkins,
  UGraphic;

constructor TMenuBackgroundFade.Create(const ThemedSettings: TThemeBackground);
var
  texFilename: IPath;
begin
  inherited;
  FadeTime := 0;

  Color := ThemedSettings.Color;
  Alpha := ThemedSettings.Alpha;
  if (Length(ThemedSettings.Tex) > 0) then
  begin
    texFilename := Skin.GetTextureFileName(ThemedSettings.Tex);
    Tex         := Renderer.GetTexture(texFilename, TEXTURE_TYPE_PLAIN);

    UseTexture  := (Tex <> nil);
  end
  else
    UseTexture := false;

  {if (not UseTexture) then
    FreeInstance(Tex); //BasisBit ToDo}
end;

destructor  TMenuBackgroundFade.Destroy;
begin
  Tex.Free;
  inherited;
end;

procedure   TMenuBackgroundFade.OnShow;
begin
  FadeTime := SDL_GetTicks;
end;

procedure   TMenuBackgroundFade.Draw;
var
  Progress: real;
begin
  if FadeTime = 0 then
    Progress := Alpha
  else
    Progress := Alpha * (SDL_GetTicks - FadeTime) / FADEINTIME;

  if Progress > Alpha then
  begin
    FadeTime := 0;
    Progress := Alpha;
  end;

  if (UseTexture) then
  begin //Draw Texture to Screen
    if (ScreenAct = 1) then //Clear just once when in dual screen mode
      Renderer.ClearFrameBuffer(CLEAR_DEPTH);

    with Tex do
    begin
      X := 0;
      Y := 0;
      W := RenderW;
      H := RenderH;
      ColR := Color.R;
      ColG := Color.G;
      ColB := Color.B;
      Alpha := Progress;
    end;
    Renderer.DrawTexture(Tex);
  end
  else
  begin //Clear Screen w/ progress Alpha + Color
    if (ScreenAct = 1) then //Clear just once when in dual screen mode
      Renderer.ClearFrameBuffer(CLEAR_DEPTH);
      
    Renderer.DrawQuad(0, 0, 0, RenderW, RenderH, Color.R, Color.G, Color.B, Progress);
  end;  
end;

end.
