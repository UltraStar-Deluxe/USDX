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

uses UMenuText;

type
  TListColumn = record
    Left, Width: real;
    Align: integer;
  end;

  TListColumnSpec = record
    Width, GapBefore: real;
    Align: integer;
    Flexible, CompactGap: boolean;
  end;

  TTextListLayout = record
    RowPitch: real;
    TextSize: real;
  end;

function MeasureTextMaxLineWidth(const Value: UTF8String; Font, Style: integer; Size: real): real;
function FitTextToWidth(const Value: UTF8String; Width: real; Font, Style: integer; Size: real): UTF8String;
function FontSizePerRowPitch(RowCount, MinEntries, MaxEntries: integer; RelaxedRatio, DenseRatio, Curve: real): real;
function CalculateTextListLayout(TopY, BottomY: real; RowCount: integer; MinTextSize, MaxTextSize, FontToPitchRatio, LineHeightFactor: real): TTextListLayout;

procedure CalculateListColumns(const Specs: array of TListColumnSpec;
    Left, Width, RightBound, GapScale, MinGapScale: real;
    var Columns: array of TListColumn);
function ListColumnFromAnchor(X, Width: real; Align: integer): TListColumn;
procedure PositionListColumnText(Text: TText; const Column: TListColumn);
procedure SetListCellText(Text: TText; const Column: TListColumn; const Value: UTF8String);

implementation

uses Math, StrUtils, SysUtils, UText, UUnicodeUtils;

procedure CalculateListColumns(const Specs: array of TListColumnSpec;
    Left, Width, RightBound, GapScale, MinGapScale: real;
    var Columns: array of TListColumn);
var
  I, Count, FlexibleCount: integer;
  BaseWidth, UsedWidth, LayoutWidth, ExtraWidth, Offset, Gap, ColumnWidth: real;
begin
  Count := Min(Length(Specs), Length(Columns));
  FlexibleCount := 0;
  BaseWidth := 0;
  UsedWidth := 0;
  for I := 0 to Count - 1 do
  begin
    Gap := Specs[I].GapBefore;
    BaseWidth := BaseWidth + Gap + Specs[I].Width;
    if Specs[I].CompactGap then
      Gap := Gap * EnsureRange(GapScale, MinGapScale, 1.0);
    UsedWidth := UsedWidth + Gap + Specs[I].Width;
    if Specs[I].Flexible then
      Inc(FlexibleCount);
  end;

  LayoutWidth := EnsureRange(RightBound, 0.0, 1.0);
  ExtraWidth := 0;
  if FlexibleCount > 0 then
    ExtraWidth := (LayoutWidth - UsedWidth) / FlexibleCount
  else
    LayoutWidth := Min(LayoutWidth, Min(1.0, BaseWidth));

  Offset := 0;
  for I := 0 to Count - 1 do
  begin
    Gap := Specs[I].GapBefore;
    if Specs[I].CompactGap then
      Gap := Gap * EnsureRange(GapScale, MinGapScale, 1.0);
    Offset := Offset + Gap;
    ColumnWidth := Specs[I].Width;
    if Specs[I].Flexible then
      ColumnWidth := Max(0.0, ColumnWidth + ExtraWidth);
    Columns[I].Left := Left + Width * Offset;
    Columns[I].Width := Width * Max(0.0, Min(ColumnWidth, LayoutWidth - Offset));
    Columns[I].Align := Specs[I].Align;
    Offset := Offset + ColumnWidth;
  end;
end;

function ListColumnFromAnchor(X, Width: real; Align: integer): TListColumn;
begin
  Result.Width := Width;
  Result.Align := Align;
  case Align of
    1: Result.Left := X - Width / 2;
    2: Result.Left := X - Width;
  else
    Result.Left := X;
  end;
end;

procedure PositionListColumnText(Text: TText; const Column: TListColumn);
begin
  Text.Align := Column.Align;
  case Column.Align of
    1: Text.X := Column.Left + Column.Width / 2;
    2: Text.X := Column.Left + Column.Width;
  else
    Text.X := Column.Left;
  end;
end;

procedure SetListCellText(Text: TText; const Column: TListColumn; const Value: UTF8String);
begin
  Text.Text := FitTextToWidth(Value, Column.Width, Text.Font, Text.Style, Text.Size);
end;

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
  FitWidth: real;
begin
  Result := Value;
  if (Value = '') or (Width <= 0) then
    Exit;

  SetFontFamily(Font);
  SetFontStyle(Style);
  SetFontSize(Size);
  SetFontItalic(false);

  // Column normalization can round an exact measured fit slightly down.
  FitWidth := Width + 0.000001 * Max(1.0, Abs(Width));
  if (TextWidth(Result) <= FitWidth) then
    Exit;

  TextLength := LengthUTF8(Value);
  repeat
    Dec(TextLength);
    Result := UTF8Copy(Value, 1, TextLength) + '...';
  until (TextLength <= 0) or (TextWidth(Result) <= FitWidth);

  if (TextLength <= 0) and (TextWidth(Result) > FitWidth) then
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
