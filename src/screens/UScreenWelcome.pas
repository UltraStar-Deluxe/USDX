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

unit UScreenWelcome;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  SDL,
  SysUtils,
  UThemes;

type
  TScreenWelcome = class(TMenu)
    public
      Animation:    real;
      Fadeout:      boolean;
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function Draw: boolean; override;
      procedure OnShow; override;
  end;

implementation

uses
  UGraphic,
  UTime,
  USkins,
  UTexture;

function TScreenWelcome.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Result := false;
        end;
      SDLK_RETURN:
        begin
          FadeTo(@ScreenMain);
          Fadeout := true;
        end;
    end;
  end;
end;

constructor TScreenWelcome.Create;
begin
  inherited Create;
  AddStatic(-10, -10,    0, 0, 1, 1, 1, Skin.GetTextureFileName('ButtonAlt'), TEXTURE_TYPE_TRANSPARENT);
  AddStatic(-500, 440, 200, 5, 0, 0, 0, Skin.GetTextureFileName('Rectangle'), TEXTURE_TYPE_COLORIZED);
  AddStatic(-500, 472, 200, 5, 0, 0, 0, Skin.GetTextureFileName('Rectangle'), TEXTURE_TYPE_COLORIZED);
  AddStatic(-500, 504, 200, 5, 0, 0, 0, Skin.GetTextureFileName('Rectangle'), TEXTURE_TYPE_COLORIZED);
  AddStatic(-500, 536, 200, 5, 0, 0, 0, Skin.GetTextureFileName('Rectangle'), TEXTURE_TYPE_COLORIZED);
  AddStatic(-500, 568, 200, 5, 0, 0, 0, Skin.GetTextureFileName('Rectangle'), TEXTURE_TYPE_COLORIZED);
  Animation := 0;
  Fadeout := false;
end;

procedure TScreenWelcome.OnShow;
begin
  inherited;

  CountSkipTimeSet;
end;

function TScreenWelcome.Draw: boolean;
var
  Min:    real;
  Max:    real;
  Factor:    real;
  Count:    integer;
begin
  // star animation
  Animation := Animation + TimeSkip*1000;

  // draw nothing
  Min := 0; Max := 1000;
  if (Animation >= Min) and (Animation < Max) then
  begin
  end;

  // popup
  Min := 1000; Max := 1120;
  if (Animation >= Min) and (Animation < Max) then
  begin
    Factor := (Animation - Min) / (Max - Min);
    Static[0].Texture.X := 600;
    Static[0].Texture.Y := 600 - Factor * 230;
    Static[0].Texture.W := 200;
    Static[0].Texture.H := Factor * 230;
  end;

  // bounce
  Min := 1120; Max := 1200;
  if (Animation >= Min) and (Animation < Max) then
  begin
    Factor := (Animation - Min) / (Max - Min);
    Static[0].Texture.Y := 370 + Factor * 50;
    Static[0].Texture.H := 230 - Factor * 50;
  end;

  // run
  Min := 1500; Max := 3500;
  if (Animation >= Min) and (Animation < Max) then
  begin
    Factor := (Animation - Min) / (Max - Min);

    Static[0].Texture.X := 600 - Factor * 1400;
    Static[0].Texture.H := 180;

    for Count := 1 to 5 do
    begin
      Static[Count].Texture.X := 770 - Factor * 1400;
      Static[Count].Texture.W := 150 + Factor * 200;
      Static[Count].Texture.Alpha := Factor * 0.5;
    end;
  end;

  Min := 3500;
  if (Animation >= Min) and (not Fadeout) then
  begin
    FadeTo(@ScreenMain);
    Fadeout := true;
  end;

  Result := inherited Draw;
end;

end.
