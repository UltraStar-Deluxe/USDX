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
 *}

unit UMenuWidget;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

type
  IMenuWidget = class
    protected
      PosYInit: single;

    public
      Scrollable: boolean;

      procedure SetX(PosX: single); virtual; abstract;
      function GetX(): single; virtual; abstract;
      procedure SetY(PosY: single);  virtual; abstract;
      function GetY(): single;  virtual; abstract;
      function GetW(): single;  virtual; abstract;
      function GetH(): single;  virtual; abstract;
      procedure SetScrollOffset(ScrollOffset: single); virtual;
      function GetScrollOffset(): single;  virtual;
      procedure Draw; virtual; abstract;

      property X: single read GetX write SetX;
      property Y: single read GetY write SetY;
      property YInit: single read PosYInit write PosYInit;
      property W: single read GetW;
      property H: single read GetH;
      property ScrollOffset: single read GetScrollOffset write SetScrollOffset;
    end;

  ISimpleMenuWidget = class(IMenuWidget)
    protected
      PosX, PosY, Width, Height: single;

    public
      constructor Create(PosX, PosY, Width, Height: single);
      procedure SetX(PosX: single); override;
      function GetX(): single; override;
      procedure SetY(PosY: single); override;
      function GetY(): single; override;
      function GetW(): single; override;
      function GetH(): single; override;
      procedure SetScrollOffset(ScrollOffset: single); override;
      function GetScrollOffset(): single; override;
    end;

implementation

procedure IMenuWidget.SetScrollOffset(ScrollOffset: single);
begin
  Y := YInit + ScrollOffset;
end;

function IMenuWidget.GetScrollOffset(): single;
begin
  Result := Y - Yinit;
end;

constructor ISimpleMenuWidget.Create(PosX, PosY, Width, Height: single);
begin
  self.PosX := PosX;
  self.PosY := PosY;
  self.Width := Width;
  self.Height := Height;
  PosYInit := PosY;
end;

procedure ISimpleMenuWidget.SetX(PosX: single);
begin
  self.PosX := PosX;
end;

function ISimpleMenuWidget.GetX(): single;
begin
  Result := PosX
end;

procedure ISimpleMenuWidget.SetY(PosY: single);
begin
  self.PosY := PosY;
end;

function ISimpleMenuWidget.GetY(): single;
begin
  Result := PosY;
end;

function ISimpleMenuWidget.GetW(): single;
begin
  Result := Width;
end;

function ISimpleMenuWidget.GetH(): single;
begin
  Result := Height;
end;

procedure ISimpleMenuWidget.SetScrollOffset(ScrollOffset: single);
begin
  PosY := PosYInit + ScrollOffset;
end;

function ISimpleMenuWidget.GetScrollOffset(): single;
begin
  Result := PosY - PosYInit;
end;

end.
