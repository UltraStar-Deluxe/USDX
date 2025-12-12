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
      PosYInit: real;

    public
      Scrollable: boolean;

      procedure SetX(PosX: real); virtual; abstract;
      function GetX(): real; virtual; abstract;
      procedure SetY(PosY: real);  virtual; abstract;
      function GetY(): real;  virtual; abstract;
      function GetW(): real;  virtual; abstract;
      function GetH(): real;  virtual; abstract;
      procedure SetScrollOffset(ScrollOffset: real); virtual;
      function GetScrollOffset(): real;  virtual;
      procedure Draw; virtual; abstract;

      property X: real read GetX write SetX;
      property Y: real read GetY write SetY;
      property YInit: real read PosYInit write PosYInit;
      property W: real read GetW;
      property H: real read GetH;
      property ScrollOffset: real read GetScrollOffset write SetScrollOffset;
    end;

  ISimpleMenuWidget = class(IMenuWidget)
    protected
      PosX, PosY, Width, Height: real;

    public
      constructor Create(PosX, PosY, Width, Height: real);
      procedure SetX(PosX: real); override;
      function GetX(): real; override;
      procedure SetY(PosY: real); override;
      function GetY(): real; override;
      function GetW(): real; override;
      function GetH(): real; override;
      procedure SetScrollOffset(ScrollOffset: real); override;
      function GetScrollOffset(): real; override;
    end;

implementation

procedure IMenuWidget.SetScrollOffset(ScrollOffset: real);
begin
  Y := YInit + ScrollOffset;
end;

function IMenuWidget.GetScrollOffset(): real;
begin
  Result := Y - Yinit;
end;

constructor ISimpleMenuWidget.Create(PosX, PosY, Width, Height: real);
begin
  self.PosX := PosX;
  self.PosY := PosY;
  self.Width := Width;
  self.Height := Height;
  PosYInit := PosY;
end;

procedure ISimpleMenuWidget.SetX(PosX: real);
begin
  self.PosX := PosX;
end;

function ISimpleMenuWidget.GetX(): real;
begin
  Result := PosX
end;

procedure ISimpleMenuWidget.SetY(PosY: real);
begin
  self.PosY := PosY;
end;

function ISimpleMenuWidget.GetY(): real;
begin
  Result := PosY;
end;

function ISimpleMenuWidget.GetW(): real;
begin
  Result := Width;
end;

function ISimpleMenuWidget.GetH(): real;
begin
  Result := Height;
end;

procedure ISimpleMenuWidget.SetScrollOffset(ScrollOffset: real);
begin
  PosY := PosYInit + ScrollOffset;
end;

function ISimpleMenuWidget.GetScrollOffset(): real;
begin
  Result := PosY - PosYInit;
end;

end.
