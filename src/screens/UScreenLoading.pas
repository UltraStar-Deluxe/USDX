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

unit UScreenLoading;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  SDL,
  SysUtils,
  UThemes,
  gl;

type
  TScreenLoading = class(TMenu)
    public
      Fadeout:      boolean;
      constructor Create; override;
      procedure   onShow; override;
      function    ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      function    GetBGTexNum: GLUInt;
  end;

implementation

uses UGraphic,
     UTime;

function TScreenLoading.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
end;

constructor TScreenLoading.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.Loading);

  Fadeout := false;
end;

procedure TScreenLoading.onShow;
begin
  inherited;
end;

function TScreenLoading.GetBGTexNum: GLUInt;
begin
  Result := Self.BackImg.TexNum;
end;

end.
