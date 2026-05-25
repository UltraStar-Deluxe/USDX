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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/screens/UScreenStatDetail.pas $
 * $Id: UScreenStatDetail.pas 1972 2009-12-06 12:13:34Z s_alexander $
 *}

unit UScreenStatDetail;

interface

{$IFDEF FPC}
  {$mode objfpc}//{$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDataBase,
  UDisplay,
  UIni,
  UMenu,
  UMusic,
  UThemes,
  sdl2,
  SysUtils,
  Classes;

type
  TStatDetailCellValue = (
    scvNone,
    scvRank,
    scvScore,
    scvPlayer,
    scvDifficulty,
    scvDate,
    scvSong,
    scvAverageScore,
    scvSingCount,
    scvArtist
  );

  TScreenStatDetail = class(TMenu)
    private
      TextHeader: array[0..STAT_DETAIL_MAX_COLUMNS - 1] of integer;
      TextList: array of array of integer;
      TextListWidth: array of array of real;
      TextListValue: array[0..STAT_DETAIL_MAX_COLUMNS - 1] of TStatDetailCellValue;
      TextDescription: integer;
      TextPage: integer;
      EntriesPerPage: integer;

      procedure ChangeEntriesPerPage(Delta: integer);
      procedure ClearRows;
      procedure RecalculatePages;
      procedure SetRowText(Row, Column: integer; const Value: UTF8String);
      function StatCellText(StatList: TList; Row: integer; Value: TStatDetailCellValue): UTF8String;
      procedure UpdateListLayout(StatList: TList; VisibleEntries: integer);

    public
      Typ:  TStatType;
      Page: cardinal;
      Reversed: boolean;

      TotEntrys: cardinal;
      TotPages:  cardinal;

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure SetAnimationProgress(Progress: real); override;

      procedure SetTitle;
      procedure SetPage(NewPage: cardinal);
  end;

const
  ID='ID_051';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UUnicodeUtils,
  Math,
  TextGL,
  StrUtils;

type
  TStatDetailColumnFlag = (cfCompactGap, cfFlexibleWidth);
  TStatDetailColumnFlags = set of TStatDetailColumnFlag;
  TStatDetailColumnWidthMode = (cwmFixed, cwmMeasured);

  TStatDetailColumnSpec = record
    Header: RawByteString;
    Value: TStatDetailCellValue;
    Width: real;
    GapBefore: real;
    Align: integer;
    Flags: TStatDetailColumnFlags;
    WidthMode: TStatDetailColumnWidthMode;
  end;

  TStatDetailColumnSpecs = array[0..STAT_DETAIL_MAX_COLUMNS - 1] of TStatDetailColumnSpec;

  TStatDetailColumnDefinition = record
    Header: RawByteString;
    Value: TStatDetailCellValue;
  end;

  TStatDetailColumnDefinitions = array[0..STAT_DETAIL_MAX_COLUMNS - 1] of TStatDetailColumnDefinition;

  TTextListLayout = record
    RowPitch: real;
    TextSize: real;
  end;

const
  BEST_SCORE_COLUMNS: TStatDetailColumnDefinitions = (
    (Header: 'STAT_DETAIL_HEADER_RANK';       Value: scvRank),
    (Header: 'STAT_DETAIL_HEADER_SCORE';      Value: scvScore),
    (Header: 'STAT_DETAIL_HEADER_PLAYER';     Value: scvPlayer),
    (Header: 'STAT_DETAIL_HEADER_DIFFICULTY'; Value: scvDifficulty),
    (Header: 'STAT_DETAIL_HEADER_DATE';       Value: scvDate),
    (Header: 'STAT_DETAIL_HEADER_SONG';       Value: scvSong)
  );
  BEST_SINGER_COLUMNS: TStatDetailColumnDefinitions = (
    (Header: 'STAT_DETAIL_HEADER_RANK';      Value: scvRank),
    (Header: 'STAT_DETAIL_HEADER_PLAYER';    Value: scvPlayer),
    (Header: 'STAT_DETAIL_HEADER_AVG_SCORE'; Value: scvAverageScore),
    (Header: 'STAT_DETAIL_HEADER_SCORES';    Value: scvSingCount),
    (Header: '';                             Value: scvNone),
    (Header: '';                             Value: scvNone)
  );
  POPULAR_SONG_COLUMNS: TStatDetailColumnDefinitions = (
    (Header: 'STAT_DETAIL_HEADER_RANK';      Value: scvRank),
    (Header: 'STAT_DETAIL_HEADER_SONG';      Value: scvSong),
    (Header: 'STAT_DETAIL_HEADER_SUNG';      Value: scvSingCount),
    (Header: 'STAT_DETAIL_HEADER_AVG_SCORE'; Value: scvAverageScore),
    (Header: '';                             Value: scvNone),
    (Header: '';                             Value: scvNone)
  );
  POPULAR_BAND_COLUMNS: TStatDetailColumnDefinitions = (
    (Header: 'STAT_DETAIL_HEADER_RANK';      Value: scvRank),
    (Header: 'STAT_DETAIL_HEADER_ARTIST';    Value: scvArtist),
    (Header: 'STAT_DETAIL_HEADER_SUNG';      Value: scvSingCount),
    (Header: 'STAT_DETAIL_HEADER_AVG_SCORE'; Value: scvAverageScore),
    (Header: '';                             Value: scvNone),
    (Header: '';                             Value: scvNone)
  );

function FormatStatSong(const Artist, Title: UTF8String): UTF8String;
begin
  if (Artist = '') then
    Result := Title
  else if (Title = '') then
    Result := Artist
  else
    Result := Artist + ' - ' + Title;
end;

function FormatStatCount(Count: integer): UTF8String;
begin
  Result := IntToStr(Count);
end;

function CompactValue(Value, Factor, MinFactor: real): real;
begin
  Result := Value * EnsureRange(Factor, MinFactor, 1.0);
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

    Result := Max(Result, glTextWidth(Trim(LineText)));
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

  if (glTextWidth(Result) <= Width) then
    Exit;

  TextLength := LengthUTF8(Value);
  repeat
    Dec(TextLength);
    Result := UTF8Copy(Value, 1, TextLength) + '...';
  until (TextLength <= 0) or (glTextWidth(Result) <= Width);

  if (TextLength <= 0) and (glTextWidth(Result) > Width) then
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

function TScreenStatDetail.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
          Exit;
        end;
    end;
    
    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenStatMain);
        end;
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;
      SDLK_LEFT:
        begin
          if (Page > 0) then
            SetPage(Page - 1);
        end;
      SDLK_RIGHT:
        begin
          if (Page + 1 < TotPages) then
            SetPage(Page + 1);
        end;
      SDLK_UP:
        begin
          ChangeEntriesPerPage(-1);
        end;
      SDLK_DOWN:
        begin
          ChangeEntriesPerPage(1);
        end;
      SDLK_R:
        begin
          Reversed := not Reversed;
          SetPage(Page);
        end;
    end;
  end;
end;

constructor TScreenStatDetail.Create;
var
  Column: integer;
  Row: integer;
begin
  inherited Create;

  SetLength(TextList, Theme.StatDetail.MaxEntries, STAT_DETAIL_MAX_COLUMNS);
  SetLength(TextListWidth, Theme.StatDetail.MaxEntries, STAT_DETAIL_MAX_COLUMNS);

  for Row := 0 to High(TextList) do
    for Column := 0 to STAT_DETAIL_MAX_COLUMNS - 1 do
      TextList[Row, Column] := AddText(Theme.StatDetail.TextList[0]);

  for Column := 0 to STAT_DETAIL_MAX_COLUMNS - 1 do
    TextHeader[Column] := AddText(Theme.StatDetail.TextHeader);

  TextDescription := AddText(Theme.StatDetail.TextDescription);
  TextPage := AddText(Theme.StatDetail.TextPage);

  LoadFromTheme(Theme.StatDetail);

  EntriesPerPage := EnsureRange(Ini.StatDetailCount,
      Theme.StatDetail.MinEntries, Theme.StatDetail.MaxEntries);
  if (Ini.StatDetailCount <= 0) then
    EntriesPerPage := Theme.StatDetail.DefaultEntries;
  Typ := TStatType(0);
end;

procedure TScreenStatDetail.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenStatDetail');

  Reversed := false;

  //Set Tot Entrys and Pages
  TotEntrys := DataBase.GetTotalEntrys(Typ);
  RecalculatePages;

  //Show correct Title
  SetTitle;

  //Show First Page
  SetPage(0);
end;

procedure TScreenStatDetail.SetTitle;
begin
  if Reversed then
    Text[TextDescription].Text := Theme.StatDetail.DescriptionR[Ord(Typ)]
  else
    Text[TextDescription].Text := Theme.StatDetail.Description[Ord(Typ)];
end;

procedure TScreenStatDetail.RecalculatePages;
begin
  TotPages := Ceil(TotEntrys / EntriesPerPage);
  if TotPages = 0 then
    TotPages := 1;
end;

procedure TScreenStatDetail.ClearRows;
var
  Column: integer;
  Row: integer;
begin
  for Row := 0 to High(TextList) do
    for Column := 0 to STAT_DETAIL_MAX_COLUMNS - 1 do
      Text[TextList[Row, Column]].Text := '';
end;

procedure TScreenStatDetail.SetRowText(Row, Column: integer; const Value: UTF8String);
var
  TextID: integer;
begin
  TextID := TextList[Row, Column];
  Text[TextID].Text := FitTextToWidth(Value, TextListWidth[Row, Column],
      Text[TextID].Font, Text[TextID].Style, Text[TextID].Size);
end;

function TScreenStatDetail.StatCellText(StatList: TList; Row: integer;
    Value: TStatDetailCellValue): UTF8String;
begin
  Result := '';
  if (StatList = nil) or (Row < 0) or (Row >= StatList.Count) then
    Exit;

  case Typ of
    stBestScores:
      with TStatResultBestScores(StatList[Row]) do
      begin
        if (Score <= 0) then
          Exit;

        case Value of
          scvRank:       Result := IntToStr(Page * EntriesPerPage + Row + 1);
          scvScore:      Result := IntToStr(Score);
          scvPlayer:     Result := Singer;
          scvDifficulty: Result := Theme.ILevel[Difficulty];
          scvDate:       Result := Date;
          scvSong:       Result := FormatStatSong(SongArtist, SongTitle);
        end;
      end;

    stBestSingers:
      with TStatResultBestSingers(StatList[Row]) do
      begin
        if (AverageScore <= 0) then
          Exit;

        case Value of
          scvRank:         Result := IntToStr(Page * EntriesPerPage + Row + 1);
          scvPlayer:       Result := Player;
          scvAverageScore: Result := IntToStr(AverageScore);
          scvSingCount:    Result := FormatStatCount(Count);
        end;
      end;

    stMostSungSong:
      with TStatResultMostSungSong(StatList[Row]) do
      begin
        if (Artist = '') then
          Exit;

        case Value of
          scvRank:         Result := IntToStr(Page * EntriesPerPage + Row + 1);
          scvSong:         Result := FormatStatSong(Artist, Title);
          scvSingCount:    Result := FormatStatCount(TimesSung);
          scvAverageScore: Result := IntToStr(AverageScore);
        end;
      end;

    stMostPopBand:
      with TStatResultMostPopBand(StatList[Row]) do
      begin
        if (ArtistName = '') then
          Exit;

        case Value of
          scvRank:         Result := IntToStr(Page * EntriesPerPage + Row + 1);
          scvArtist:       Result := ArtistName;
          scvSingCount:    Result := FormatStatCount(TimesSungtot);
          scvAverageScore: Result := IntToStr(AverageScore);
        end;
      end;
  end;
end;

procedure TScreenStatDetail.UpdateListLayout(StatList: TList; VisibleEntries: integer);
var
  Column: integer;
  Row: integer;
  HeaderY: real;
  TopY: real;
  BottomY: real;
  ListLayout: TTextListLayout;
  HeaderSize: real;
  MaxTextSize: real;
  ListX: integer;
  ListY: integer;
  ListW: integer;
  ListBottomY: real;
  ColumnCompactFactor: real;
  LayoutRows: integer;
  ThemeText: TThemeText;

  procedure ApplyTextLayout(TextID: integer; ColumnLeft, ColumnWidth: real; Align: integer);
  begin
    Text[TextID].W := 0;
    Text[TextID].H := 0;
    Text[TextID].Align := Align;

    case Align of
      1:
        Text[TextID].X := ColumnLeft + ColumnWidth / 2;
      2:
        Text[TextID].X := ColumnLeft + ColumnWidth;
      else
        Text[TextID].X := ColumnLeft;
    end;
  end;

  procedure SetColumn(Column: integer; LeftFactor, WidthFactor: real; Align: integer; const HeaderText: RawByteString);
  var
    ColumnLeft: real;
    ColumnWidth: real;
    TextID: integer;
    Row: integer;
  begin
    ColumnLeft := ThemeText.X + ThemeText.W * LeftFactor;
    ColumnWidth := ThemeText.W * WidthFactor;

    TextID := TextHeader[Column];
    ApplyTextLayout(TextID, ColumnLeft, ColumnWidth, Align);
    Text[TextID].Y := HeaderY;
    Text[TextID].Visible := true;
    Text[TextID].Text := Language.Translate(HeaderText);

    for Row := 0 to High(TextList) do
    begin
      TextListWidth[Row, Column] := ColumnWidth;

      TextID := TextList[Row, Column];
      ApplyTextLayout(TextID, ColumnLeft, ColumnWidth, Align);
      Text[TextID].Visible := Row < EntriesPerPage;
    end;
  end;

  function ColumnGap(const Column: TStatDetailColumnSpec): real;
  begin
    Result := Column.GapBefore;
    if (cfCompactGap in Column.Flags) then
      Result := CompactValue(Result, ColumnCompactFactor,
          Theme.StatDetail.MinColumnGap);
  end;

  function TextLeftEdge(TextID: integer): real;
  var
    TextWidth: real;
  begin
    TextWidth := MeasureTextMaxLineWidth(Text[TextID].Text, Text[TextID].Font,
        Text[TextID].Style, Text[TextID].Size);

    case Text[TextID].Align of
      1:
        Result := Text[TextID].X - TextWidth / 2;
      2:
        Result := Text[TextID].X - TextWidth;
      else
        Result := Text[TextID].X;
    end;
  end;

  function PageTextRightBound: real;
  begin
    Result := 1.0;
    if (ThemeText.W > 0) and (Text[TextPage].Text <> '') then
      Result := EnsureRange((TextLeftEdge(TextPage) - Theme.StatDetail.PageTextMargin -
          ThemeText.X) / ThemeText.W, 0.0, 1.0);
  end;

  function TableRightBound(const Table: TThemeStatDetailTable): real;
  begin
    if Table.UsePageRightBound then
      Result := PageTextRightBound
    else
      Result := 1.0;

    if (ThemeText.W > 0) then
      Result := EnsureRange(Result - Table.RightInset /
          ThemeText.W, 0.0, 1.0);
  end;

  function TextWidthFactor(const Value: UTF8String; Font, Style: integer;
      Size, Padding: real): real;
  begin
    Result := 0;
    if (ThemeText.W > 0) and (Value <> '') then
      Result := (MeasureTextMaxLineWidth(Value, Font, Style, Size) + Padding) /
          ThemeText.W;
  end;

  function ValueColumnWidth(const HeaderText: RawByteString;
      Value: TStatDetailCellValue): real;
  var
    Row: integer;
    ValueText: UTF8String;
  begin
    Result := TextWidthFactor(Language.Translate(HeaderText), ThemeText.Font,
        Theme.StatDetail.TextHeader.Style, HeaderSize, Theme.StatDetail.ColumnPadding);

    for Row := 0 to VisibleEntries - 1 do
    begin
      ValueText := StatCellText(StatList, Row, Value);
      if (ValueText <> '') then
        Result := Max(Result, TextWidthFactor(ValueText, ThemeText.Font,
            ThemeText.Style, ListLayout.TextSize, Theme.StatDetail.ColumnPadding));
    end;
  end;

  function ThemeColumnSpec(const Header: RawByteString; Value: TStatDetailCellValue;
      const ThemeColumn: TThemeStatDetailColumn): TStatDetailColumnSpec;
  begin
    Result.Header := Header;
    Result.Value := Value;
    Result.Width := ThemeColumn.Width;
    Result.GapBefore := ThemeColumn.GapBefore;
    Result.Align := ThemeColumn.Align;
    Result.Flags := [];
    if ThemeColumn.CompactGap then
      Include(Result.Flags, cfCompactGap);
    if ThemeColumn.Flexible then
      Include(Result.Flags, cfFlexibleWidth);
    if ThemeColumn.AutoWidth then
      Result.WidthMode := cwmMeasured
    else
      Result.WidthMode := cwmFixed;
  end;

  procedure SetColumnLayout(const Columns: TStatDetailColumnSpecs; ColumnCount: integer;
      RightBoundFactor: real);
  var
    Column: integer;
    FlexibleColumns: integer;
    BaseWidth: real;
    UsedWidth: real;
    LayoutWidth: real;
    FlexWidth: real;
    LeftFactor: real;
    WidthFactors: array[0..STAT_DETAIL_MAX_COLUMNS - 1] of real;
    GapFactors: array[0..STAT_DETAIL_MAX_COLUMNS - 1] of real;
  begin
    ColumnCount := EnsureRange(ColumnCount, 0, STAT_DETAIL_MAX_COLUMNS);
    FlexibleColumns := 0;
    BaseWidth := 0;
    UsedWidth := 0;

    for Column := 0 to ColumnCount - 1 do
    begin
      GapFactors[Column] := ColumnGap(Columns[Column]);
      if (Columns[Column].WidthMode = cwmMeasured) then
        WidthFactors[Column] := ValueColumnWidth(Columns[Column].Header,
            Columns[Column].Value)
      else
        WidthFactors[Column] := Columns[Column].Width;

      BaseWidth := BaseWidth + Columns[Column].GapBefore + WidthFactors[Column];
      UsedWidth := UsedWidth + GapFactors[Column] + WidthFactors[Column];

      if (cfFlexibleWidth in Columns[Column].Flags) then
        Inc(FlexibleColumns);
    end;

    FlexWidth := 0;
    if (FlexibleColumns > 0) then
      LayoutWidth := EnsureRange(RightBoundFactor, 0.0, 1.0)
    else
      LayoutWidth := Min(EnsureRange(RightBoundFactor, 0.0, 1.0),
          Min(1.0, BaseWidth));

    if (FlexibleColumns > 0) then
      FlexWidth := (LayoutWidth - UsedWidth) / FlexibleColumns;

    LeftFactor := 0;
    for Column := 0 to ColumnCount - 1 do
    begin
      LeftFactor := LeftFactor + GapFactors[Column];

      if (cfFlexibleWidth in Columns[Column].Flags) then
        WidthFactors[Column] := Max(0.0, WidthFactors[Column] + FlexWidth);

      TextListValue[Column] := Columns[Column].Value;
      SetColumn(Column, LeftFactor,
          Max(0.0, Min(WidthFactors[Column], 1.0 - LeftFactor)),
          Columns[Column].Align, Columns[Column].Header);
      LeftFactor := LeftFactor + WidthFactors[Column];
    end;
  end;

  procedure SetColumns(const Table: TThemeStatDetailTable;
      const Definitions: TStatDetailColumnDefinitions);
  var
    Columns: TStatDetailColumnSpecs;
    Column: integer;
    ColumnCount: integer;
  begin
    for Column := Low(Columns) to High(Columns) do
      Columns[Column] := ThemeColumnSpec('', scvNone, Table.Column[Column]);

    ColumnCount := EnsureRange(Table.ColumnCount, 0, STAT_DETAIL_MAX_COLUMNS);
    while (ColumnCount > 0) and (Definitions[ColumnCount - 1].Value = scvNone) do
      Dec(ColumnCount);

    for Column := 0 to ColumnCount - 1 do
      Columns[Column] := ThemeColumnSpec(Definitions[Column].Header,
          Definitions[Column].Value, Table.Column[Column]);

    SetColumnLayout(Columns, ColumnCount, TableRightBound(Table));
  end;
begin
  ThemeText := Theme.StatDetail.TextList[0];
  ListX := Theme.StatDetail.ListX;
  ListY := Theme.StatDetail.ListY;
  ListW := Theme.StatDetail.ListW;
  ListBottomY := Theme.StatDetail.ListBottomY;
  if (ListW <= 0) then
    ListW := ThemeText.W;
  if (ListBottomY <= ListY) then
  begin
    ListY := ThemeText.Y;
    ListBottomY := ListY + Theme.StatDetail.FallbackRowPitch *
        (EntriesPerPage + Theme.StatDetail.PaddingRows - 1);
  end;

  LayoutRows := EntriesPerPage + Theme.StatDetail.PaddingRows;
  ThemeText.X := ListX;
  ThemeText.W := ListW;
  TopY := ListY;
  BottomY := ListBottomY;
  HeaderY := Theme.StatDetail.ListHeaderY;

  MaxTextSize := Max(Theme.StatDetail.MinTextSize, ThemeText.Size);
  ListLayout := CalculateTextListLayout(TopY, BottomY, LayoutRows,
      Theme.StatDetail.MinTextSize, MaxTextSize,
      FontSizePerRowPitch(EntriesPerPage, Theme.StatDetail.MinEntries,
      Theme.StatDetail.MaxEntries, Theme.StatDetail.FontSizeRelaxed,
      Theme.StatDetail.FontSizeDense, Theme.StatDetail.FontSizeCurve),
      Theme.StatDetail.LineHeight);
  HeaderSize := Theme.StatDetail.TextHeader.Size;
  ColumnCompactFactor := EnsureRange(ListLayout.TextSize / MaxTextSize,
      0.0, 1.0);

  for Column := 0 to STAT_DETAIL_MAX_COLUMNS - 1 do
  begin
    TextListValue[Column] := scvNone;
    Text[TextHeader[Column]].Text := '';
    Text[TextHeader[Column]].Visible := false;
  end;

  for Row := 0 to High(TextList) do
  begin
    for Column := 0 to STAT_DETAIL_MAX_COLUMNS - 1 do
    begin
      Text[TextList[Row, Column]].Y := TopY +
          (Row + Theme.StatDetail.FirstContentRow) * ListLayout.RowPitch;
      Text[TextList[Row, Column]].H := 0;
      Text[TextList[Row, Column]].Size := ListLayout.TextSize;
      Text[TextList[Row, Column]].Visible := false;
    end;
  end;

  case Typ of
    stBestScores:
      SetColumns(Theme.StatDetail.BestScoresTable, BEST_SCORE_COLUMNS);
    stBestSingers:
      SetColumns(Theme.StatDetail.BestSingersTable, BEST_SINGER_COLUMNS);
    stMostSungSong:
      SetColumns(Theme.StatDetail.PopularTable, POPULAR_SONG_COLUMNS);
    stMostPopBand:
      SetColumns(Theme.StatDetail.PopularTable, POPULAR_BAND_COLUMNS);
  end;
end;

procedure TScreenStatDetail.ChangeEntriesPerPage(Delta: integer);
var
  FirstEntry: cardinal;
  NewCount: integer;
begin
  NewCount := EnsureRange(EntriesPerPage + Delta, Theme.StatDetail.MinEntries,
      Theme.StatDetail.MaxEntries);
  if (NewCount = EntriesPerPage) then
    Exit;

  FirstEntry := Page * EntriesPerPage;
  EntriesPerPage := NewCount;
  Ini.StatDetailCount := EntriesPerPage;
  Ini.SaveStatDetailCount;

  RecalculatePages;
  SetPage(FirstEntry div EntriesPerPage);
end;

procedure TScreenStatDetail.SetPage(NewPage: cardinal);
var
  StatList: TList;
  I: integer;
  Column: integer;
  PerPage: integer;
  VisibleEntries: integer;
begin
  if (NewPage >= TotPages) then
    NewPage := TotPages - 1;
  Page := NewPage;

  if (TotEntrys = 0) then
    PerPage := 0
  else if (Page + 1 = TotPages) and (TotEntrys mod EntriesPerPage <> 0) then
    PerPage := (TotEntrys mod EntriesPerPage)
  else
    PerPage := EntriesPerPage;

  try
    Text[TextPage].Text := Format(Theme.StatDetail.PageStr,
        [Page + 1, TotPages, PerPage, TotEntrys]);
  except
    on E: EConvertError do
    begin
      Text[TextPage].Text := '';
      Log.LogError('Error Parsing FormatString in UScreenStatDetail: ' + E.Message);
    end;
  end;

  // fetch statistics
  StatList := Database.GetStats(Typ, EntriesPerPage, NewPage, Reversed);
  if (StatList <> nil) then
    VisibleEntries := Min(StatList.Count, EntriesPerPage)
  else
    VisibleEntries := 0;

  UpdateListLayout(StatList, VisibleEntries);

  // reset texts
  ClearRows;

  if (StatList <> nil) then
  begin
    for I := 0 to VisibleEntries - 1 do
      for Column := 0 to STAT_DETAIL_MAX_COLUMNS - 1 do
        SetRowText(I, Column, StatCellText(StatList, I, TextListValue[Column]));

  end;

  //Show correct Title
  SetTitle;

  Database.FreeStats(StatList);
end;

procedure TScreenStatDetail.SetAnimationProgress(Progress: real);
var
  I: integer;
begin
  for I := 0 to High(Button) do
    Button[I].Texture.ScaleW := Progress;
end;

end.
