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

unit UMenuList;

interface

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}
{$I switches.inc}

type
  TTextListLayout = record
    RowPitch: real;
    TextSize: real;
  end;

function MeasureTextMaxLineWidth(const Value: UTF8String; Font, Style: integer; Size: real): real;
function FitTextToWidth(const Value: UTF8String; Width: real; Font, Style: integer; Size: real): UTF8String;
function FontSizePerRowPitch(RowCount, MinEntries, MaxEntries: integer; RelaxedRatio, DenseRatio, Curve: real): real;
function CalculateTextListLayout(TopY, BottomY: real; RowCount: integer; MinTextSize, MaxTextSize, FontToPitchRatio, LineHeightFactor: real): TTextListLayout;

implementation

uses Math, StrUtils, SysUtils, UText, UUnicodeUtils;

function MeasureTextMaxLineWidth(const Value: UTF8String; Font, Style: integer;
    Size: real): real;
var
  BreakPos: integer;
  LineStart: integer;
  LineText: UTF8String;
begin
  Result := 0;
  if (Value = '') then
    Exit;

  SetFontFamily(Font);
  SetFontStyle(Style);
  SetFontSize(Size);
  SetFontItalic(false);

  LineStart := 1;
  repeat
    BreakPos := PosEx('\n', Value, LineStart);
    if (BreakPos = 0) then
    begin
      LineText := Copy(Value, LineStart, Length(Value) - LineStart + 1);
      LineStart := Length(Value) + 1;
    end
    else
    begin
      LineText := Copy(Value, LineStart, BreakPos - LineStart);
      LineStart := BreakPos + Length('\n');
    end;

    Result := Max(Result, TextWidth(Trim(LineText)));
  until (LineStart > Length(Value));
end;

function FitTextToWidth(const Value: UTF8String; Width: real; Font, Style: integer; Size: real): UTF8String;
var
  TextLength: integer;
begin
  Result := Value;
  if (Value = '') or (Width <= 0) then
    Exit;

  SetFontFamily(Font);
  SetFontStyle(Style);
  SetFontSize(Size);

  if (TextWidth(Result) <= Width) then
    Exit;

  TextLength := LengthUTF8(Value);
  repeat
    Dec(TextLength);
    Result := UTF8Copy(Value, 1, TextLength) + '...';
  until (TextLength <= 0) or (TextWidth(Result) <= Width);

  if (TextLength <= 0) and (TextWidth(Result) > Width) then
    Result := '';
end;

function EntryDensity(RowCount, MinEntries, MaxEntries: integer; Curve: real): real;
begin
  Result := EnsureRange((RowCount - MinEntries) /
      Max(1, MaxEntries - MinEntries), 0.0, 1.0);
  if (Curve > 0) then
    Result := Power(Result, Curve);
end;

function FontSizePerRowPitch(RowCount, MinEntries, MaxEntries: integer;
    RelaxedRatio, DenseRatio, Curve: real): real;
var
  Density: real;
begin
  Density := EntryDensity(RowCount, MinEntries, MaxEntries, Curve);
  Result := RelaxedRatio + Density * (DenseRatio - RelaxedRatio);
end;

{ BottomY is the lower visual bound for the final row, not its text origin. }
function CalculateTextListLayout(TopY, BottomY: real; RowCount: integer;
    MinTextSize, MaxTextSize, FontToPitchRatio, LineHeightFactor: real): TTextListLayout;
var
  AvailableHeight: real;
  TextHeight: real;
begin
  RowCount := Max(1, RowCount);
  AvailableHeight := Max(0, BottomY - TopY);
  MaxTextSize := Max(MinTextSize, MaxTextSize);
  FontToPitchRatio := Max(0.01, FontToPitchRatio);
  LineHeightFactor := Max(0.01, LineHeightFactor);

  if (RowCount > 1) then
  begin
    Result.TextSize := AvailableHeight / ((RowCount - 1) / FontToPitchRatio + LineHeightFactor);
    Result.TextSize := EnsureRange(Result.TextSize, MinTextSize, MaxTextSize);

    TextHeight := Result.TextSize * LineHeightFactor;
    Result.RowPitch := Max(0, (AvailableHeight - TextHeight) / (RowCount - 1));
  end
  else
  begin
    Result.RowPitch := 0;
    Result.TextSize := EnsureRange(AvailableHeight / LineHeightFactor, MinTextSize, MaxTextSize);
  end;
end;

end.
