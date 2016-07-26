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

unit UPlayers;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  IniFiles,
  SysUtils,
  Classes,
  UCommon,
  UThemes,
  ULog,
  UIni,
  UTexture,
  UPath;

type
    TPlayer = object
      private

      public
        Oscilloscope:     TThemePosition;
        RefStatic: integer;
        RefImage: integer;
        RefText: integer;
        ShowOnScreen: byte;

        procedure Create();
        procedure Reset();

    end;

implementation
var
   Players: array [0..UIni.IMaxPlayerCount-1] of TPlayer;

procedure Create();
begin
  inherited;
end;

procedure Reset();
var i1:integer;
begin
  for(i1:=0 to UIni.IMaxPlayerCount-1)
  begin
       ;
  end;

end;

procedure Destroy();
begin
  inherited;
end;

end.

