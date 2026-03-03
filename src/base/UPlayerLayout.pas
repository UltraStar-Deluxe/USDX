{*
 * Shared player-count layout helpers.
 *}

unit UPlayerLayout;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Math;

type
  TPlayerGrid = record
    Cols: integer;
    Rows: integer;
  end;

  TPlayerSlotRect = record
    X: integer;
    Y: integer;
    W: integer;
    H: integer;
  end;

  TSingPlayerLayoutConfig = record
    ColumnContainerLeft: integer;
    ColumnContainerWidth: integer;
    GridExtraLeft: integer;
    NoteContentOffsetX: integer;
    ContentVerticalOffset: integer;
    TopLyricsReservedHeight: integer;
    OneTwoRowSpacing: integer;
    MultiRowSpacing: integer;
    OneRowAnchorY: integer;
    TwoRowsTopAnchorY: integer;
    TwoRowsBottomAnchorY: integer;
    MultiRowsTopAnchorY: integer;
    MultiRowsBottomAnchorY: integer;
    GuideRangeTopY: integer;
    GuideRangeBottomY: integer;
    WidgetScaleBaseWidth: integer;
  end;

  TSingLaneLayout = record
    ColumnLeft: integer;
    ColumnRight: integer;
    ColumnWidth: integer;
    GridLeft: integer;
    GridRight: integer;
    GridWidth: integer;
    RowAnchorY: integer;
    GuideTopY: integer;
    NoteLineSpacing: integer;
    WidgetScale: real;
  end;

function GetPlayerGrid(PlayerCount: integer; Flip: boolean = false): TPlayerGrid;
function GetWidePlayerGrid(PlayerCount: integer): TPlayerGrid;
function GetPlayerRowPosition(RowIndex, RowCount: integer;
  SingleRowPos, TwoRowsTopPos, TwoRowsBottomPos, MultiRowsTopPos, MultiRowsBottomPos: integer): integer;
function GetPlayerRowSpacing(RowCount, OneTwoRowSpacing, ThreeRowSpacing,
  MultiRowsTopPos, MultiRowsBottomPos: integer): integer;
function GetPlayerColumnGap(ColCount: integer): integer;
procedure GetPlayerColumnLayout(ColIndex, ColCount, ContainerLeft, ContainerWidth, ColumnGap: integer;
  out LaneLeft, LaneRight, LaneWidth: integer);
function GetPlayerWidgetScale(PlayerCount: integer): real;
function GetSingLaneLayout(PlayerCountOnScreen, PlayerIndexOnScreen: integer;
  const Config: TSingPlayerLayoutConfig; ReserveTopLyricsSpace: boolean = true): TSingLaneLayout;
function GetScreenPlayerCount(PlayerCount, ScreenCount, ScreenIndex: integer): integer;
function GetFirstPlayerIndexForScreen(PlayerCount, ScreenCount, ScreenIndex: integer): integer;
function GetPlayerScreen(PlayerIndex, PlayerCount, ScreenCount: integer): integer;
function GetPlayerIndexOnScreen(PlayerIndex, PlayerCount, ScreenCount: integer): integer;
function GetPlayerSlotRect(PlayerIndexOnScreen, PlayerCountOnScreen: integer;
  ContainerX, ContainerY, ContainerW, ContainerH: integer; Flip: boolean = false): TPlayerSlotRect;
function GetWidePlayerSlotRect(PlayerIndexOnScreen, PlayerCountOnScreen: integer;
  ContainerX, ContainerY, ContainerW, ContainerH: integer): TPlayerSlotRect;
function GetScaledGridLayoutBounds(const BaseBounds, AreaBounds: TPlayerSlotRect;
  PlayerCount: integer; Wide: boolean): TPlayerSlotRect;
function ScaleCoordToSlot(const Value, SourceStart, SourceSize, TargetStart, TargetSize: integer): integer;
function ScaleLengthToSlot(const Value, SourceSize, TargetSize: integer): integer;
function GetFittedSlotRect(const SourceBounds, SlotRect: TPlayerSlotRect; MaxScale: real): TPlayerSlotRect;

implementation

function GetPlayerGrid(PlayerCount: integer; Flip: boolean = false): TPlayerGrid;
var
  Temp: integer;
begin
  if PlayerCount <= 3 then
  begin
    Result.Cols := PlayerCount;
    Result.Rows := 1;
  end
  else
  begin
    Result.Rows := Trunc(Ceil(Sqrt(PlayerCount)));
    Result.Cols := Trunc(Ceil(PlayerCount / Result.Rows));
  end;

  if Flip then
  begin
    Temp := Result.Cols;
    Result.Cols := Result.Rows;
    Result.Rows := Temp;
  end;
end;

function GetWidePlayerGrid(PlayerCount: integer): TPlayerGrid;
begin
  if PlayerCount <= 3 then
  begin
    Result.Cols := PlayerCount;
    Result.Rows := 1;
  end
  else
  begin
    Result.Cols := Trunc(Ceil(Sqrt(PlayerCount)));
    Result.Rows := Trunc(Ceil(PlayerCount / Result.Cols));
  end;
end;

function GetPlayerRowPosition(RowIndex, RowCount: integer;
  SingleRowPos, TwoRowsTopPos, TwoRowsBottomPos, MultiRowsTopPos, MultiRowsBottomPos: integer): integer;
var
  ClampedRowIndex: integer;
begin
  if RowCount <= 1 then
    Exit(SingleRowPos);

  ClampedRowIndex := EnsureRange(RowIndex, 0, Max(RowCount - 1, 0));

  if RowCount = 2 then
    Exit(TwoRowsTopPos + Round(ClampedRowIndex * (TwoRowsBottomPos - TwoRowsTopPos)));

  Result := MultiRowsTopPos + Round(ClampedRowIndex * (MultiRowsBottomPos - MultiRowsTopPos) / Max(RowCount - 1, 1));
end;

function GetPlayerRowSpacing(RowCount, OneTwoRowSpacing, ThreeRowSpacing,
  MultiRowsTopPos, MultiRowsBottomPos: integer): integer;
var
  MultiRowStep: real;
  ThreeRowStep: real;
begin
  if RowCount <= 2 then
    Exit(OneTwoRowSpacing);

  ThreeRowStep := (MultiRowsBottomPos - MultiRowsTopPos) / 2.0;
  if ThreeRowStep <= 0 then
    Exit(ThreeRowSpacing);

  MultiRowStep := (MultiRowsBottomPos - MultiRowsTopPos) / Max(RowCount - 1, 1);
  Result := Max(6, Round(ThreeRowSpacing * MultiRowStep / ThreeRowStep));
end;

function GetPlayerColumnGap(ColCount: integer): integer;
begin
  if ColCount <= 1 then
    Exit(0);

  Result := 50;
end;

procedure GetPlayerColumnLayout(ColIndex, ColCount, ContainerLeft, ContainerWidth, ColumnGap: integer;
  out LaneLeft, LaneRight, LaneWidth: integer);
var
  ColumnWidth: integer;
begin
  LaneLeft := ContainerLeft;
  LaneWidth := ContainerWidth;
  LaneRight := ContainerLeft + ContainerWidth;

  if ColCount <= 1 then
    Exit;

  ColumnWidth := Round((ContainerWidth - (ColumnGap * (ColCount - 1))) / ColCount);
  LaneLeft := ContainerLeft + ColIndex * (ColumnWidth + ColumnGap);
  LaneWidth := ColumnWidth;
  LaneRight := LaneLeft + ColumnWidth;
end;

function GetPlayerWidgetScale(PlayerCount: integer): real;
begin
  Result := 1.0 - Max(0, PlayerCount - 1) * 0.02;
  if Result < 0.82 then
    Result := 0.82;
end;

function GetSingLaneLayout(PlayerCountOnScreen, PlayerIndexOnScreen: integer;
  const Config: TSingPlayerLayoutConfig; ReserveTopLyricsSpace: boolean = true): TSingLaneLayout;
var
  Grid: TPlayerGrid;
  ColIndex: integer;
  RowIndex: integer;
  OneRowAnchorY: integer;
  TwoRowsTopAnchorY: integer;
  TwoRowsBottomAnchorY: integer;
  MultiRowsTopAnchorY: integer;
  MultiRowsBottomAnchorY: integer;
  EffectiveContentVerticalOffset: integer;
begin
  Grid := GetPlayerGrid(PlayerCountOnScreen, true);
  ColIndex := PlayerIndexOnScreen div Grid.Rows;
  RowIndex := PlayerIndexOnScreen mod Grid.Rows;

  OneRowAnchorY := Config.OneRowAnchorY;
  TwoRowsTopAnchorY := Config.TwoRowsTopAnchorY;
  TwoRowsBottomAnchorY := Config.TwoRowsBottomAnchorY;
  MultiRowsTopAnchorY := Config.MultiRowsTopAnchorY;
  MultiRowsBottomAnchorY := Config.MultiRowsBottomAnchorY;

  if not ReserveTopLyricsSpace then
  begin
    Dec(OneRowAnchorY, Config.TopLyricsReservedHeight);
    Dec(TwoRowsTopAnchorY, Config.TopLyricsReservedHeight);
    Dec(MultiRowsTopAnchorY, Config.TopLyricsReservedHeight);
    EffectiveContentVerticalOffset := Config.ContentVerticalOffset;
  end
  else
    EffectiveContentVerticalOffset := 0;

  GetPlayerColumnLayout(ColIndex, Grid.Cols, Config.ColumnContainerLeft, Config.ColumnContainerWidth,
    GetPlayerColumnGap(Grid.Cols), Result.ColumnLeft, Result.ColumnRight, Result.ColumnWidth);
  Result.GridLeft := Result.ColumnLeft - Config.GridExtraLeft;
  Result.GridRight := Result.ColumnRight;
  Result.GridWidth := Result.GridRight - Result.GridLeft;

  Result.RowAnchorY := GetPlayerRowPosition(RowIndex, Grid.Rows,
    OneRowAnchorY, TwoRowsTopAnchorY, TwoRowsBottomAnchorY, MultiRowsTopAnchorY, MultiRowsBottomAnchorY)
    + EffectiveContentVerticalOffset;
  Result.NoteLineSpacing := GetPlayerRowSpacing(Grid.Rows, Config.OneTwoRowSpacing, Config.MultiRowSpacing,
    Config.GuideRangeTopY, Config.GuideRangeBottomY);
  Result.GuideTopY := Result.RowAnchorY - (7 * Result.NoteLineSpacing);
  Result.WidgetScale := Min(1.0, Result.ColumnWidth / Max(1.0, Config.WidgetScaleBaseWidth)) *
    GetPlayerWidgetScale(PlayerCountOnScreen);
end;

function GetScreenPlayerCount(PlayerCount, ScreenCount, ScreenIndex: integer): integer;
var
  BaseCount: integer;
  Remainder: integer;
begin
  if (PlayerCount <= 0) or (ScreenCount <= 0) or (ScreenIndex <= 0) or (ScreenIndex > ScreenCount) then
    Exit(0);

  BaseCount := PlayerCount div ScreenCount;
  Remainder := PlayerCount mod ScreenCount;

  Result := BaseCount;
  if ScreenIndex <= Remainder then
    Inc(Result);
end;

function GetFirstPlayerIndexForScreen(PlayerCount, ScreenCount, ScreenIndex: integer): integer;
var
  CurrentScreen: integer;
begin
  if (PlayerCount <= 0) or (ScreenCount <= 0) or (ScreenIndex <= 0) or (ScreenIndex > ScreenCount) then
    Exit(0);

  Result := 0;
  for CurrentScreen := 1 to ScreenIndex - 1 do
    Inc(Result, GetScreenPlayerCount(PlayerCount, ScreenCount, CurrentScreen));
end;

function GetPlayerScreen(PlayerIndex, PlayerCount, ScreenCount: integer): integer;
var
  CurrentScreen: integer;
  RemainingPlayers: integer;
  PlayersOnScreen: integer;
begin
  if (PlayerIndex < 0) or (PlayerIndex >= PlayerCount) or (ScreenCount <= 0) then
    Exit(0);

  RemainingPlayers := PlayerIndex;
  for CurrentScreen := 1 to ScreenCount do
  begin
    PlayersOnScreen := GetScreenPlayerCount(PlayerCount, ScreenCount, CurrentScreen);
    if RemainingPlayers < PlayersOnScreen then
      Exit(CurrentScreen);
    Dec(RemainingPlayers, PlayersOnScreen);
  end;

  Result := 0;
end;

function GetPlayerIndexOnScreen(PlayerIndex, PlayerCount, ScreenCount: integer): integer;
var
  CurrentScreen: integer;
  TargetScreen: integer;
begin
  Result := PlayerIndex;
  TargetScreen := GetPlayerScreen(PlayerIndex, PlayerCount, ScreenCount);

  for CurrentScreen := 1 to TargetScreen - 1 do
    Dec(Result, GetScreenPlayerCount(PlayerCount, ScreenCount, CurrentScreen));
end;

function GetPlayerSlotRect(PlayerIndexOnScreen, PlayerCountOnScreen: integer;
  ContainerX, ContainerY, ContainerW, ContainerH: integer; Flip: boolean = false): TPlayerSlotRect;
var
  Grid: TPlayerGrid;
  ColIndex: integer;
  RowIndex: integer;
  ColWidth: integer;
  RowHeight: integer;
begin
  if (PlayerIndexOnScreen < 0) or (PlayerIndexOnScreen >= PlayerCountOnScreen) or
     (ContainerW <= 0) or (ContainerH <= 0) then
  begin
    Result.X := ContainerX;
    Result.Y := ContainerY;
    Result.W := 0;
    Result.H := 0;
    Exit;
  end;

  Grid := GetPlayerGrid(PlayerCountOnScreen, Flip);
  if (Grid.Cols <= 0) or (Grid.Rows <= 0) then
  begin
    Result.X := ContainerX;
    Result.Y := ContainerY;
    Result.W := 0;
    Result.H := 0;
    Exit;
  end;

  ColIndex := PlayerIndexOnScreen div Grid.Rows;
  RowIndex := PlayerIndexOnScreen mod Grid.Rows;
  ColWidth := ContainerW div Grid.Cols;
  RowHeight := ContainerH div Grid.Rows;

  Result.X := ContainerX + (ColIndex * ColWidth);
  Result.Y := ContainerY + (RowIndex * RowHeight);

  if ColIndex = Grid.Cols - 1 then
    Result.W := ContainerX + ContainerW - Result.X
  else
    Result.W := ColWidth;

  if RowIndex = Grid.Rows - 1 then
    Result.H := ContainerY + ContainerH - Result.Y
  else
    Result.H := RowHeight;
end;

function GetWidePlayerSlotRect(PlayerIndexOnScreen, PlayerCountOnScreen: integer;
  ContainerX, ContainerY, ContainerW, ContainerH: integer): TPlayerSlotRect;
var
  Grid: TPlayerGrid;
  ColIndex: integer;
  RowIndex: integer;
  ColWidth: integer;
  RowHeight: integer;
begin
  if (PlayerIndexOnScreen < 0) or (PlayerIndexOnScreen >= PlayerCountOnScreen) or
     (ContainerW <= 0) or (ContainerH <= 0) then
  begin
    Result.X := ContainerX;
    Result.Y := ContainerY;
    Result.W := 0;
    Result.H := 0;
    Exit;
  end;

  Grid := GetWidePlayerGrid(PlayerCountOnScreen);
  if (Grid.Cols <= 0) or (Grid.Rows <= 0) then
  begin
    Result.X := ContainerX;
    Result.Y := ContainerY;
    Result.W := 0;
    Result.H := 0;
    Exit;
  end;

  ColIndex := PlayerIndexOnScreen mod Grid.Cols;
  RowIndex := PlayerIndexOnScreen div Grid.Cols;
  ColWidth := ContainerW div Grid.Cols;
  RowHeight := ContainerH div Grid.Rows;

  Result.X := ContainerX + (ColIndex * ColWidth);
  Result.Y := ContainerY + (RowIndex * RowHeight);

  if ColIndex = Grid.Cols - 1 then
    Result.W := ContainerX + ContainerW - Result.X
  else
    Result.W := ColWidth;

  if RowIndex = Grid.Rows - 1 then
    Result.H := ContainerY + ContainerH - Result.Y
  else
    Result.H := RowHeight;
end;

function GetScaledGridLayoutBounds(const BaseBounds, AreaBounds: TPlayerSlotRect;
  PlayerCount: integer; Wide: boolean): TPlayerSlotRect;
var
  Grid: TPlayerGrid;
  Scale: real;
  ScaleX: real;
  ScaleY: real;
begin
  if Wide then
    Grid := GetWidePlayerGrid(PlayerCount)
  else
    Grid := GetPlayerGrid(PlayerCount, true);

  if (Grid.Cols <= 0) or (Grid.Rows <= 0) or (BaseBounds.W <= 0) or (BaseBounds.H <= 0) then
    Exit(BaseBounds);

  ScaleX := AreaBounds.W / Max(1.0, BaseBounds.W * Grid.Cols);
  ScaleY := AreaBounds.H / Max(1.0, BaseBounds.H * Grid.Rows);
  Scale := Min(1.0, Min(ScaleX, ScaleY));

  Result.W := Round(BaseBounds.W * Grid.Cols * Scale);
  Result.H := Round(BaseBounds.H * Grid.Rows * Scale);
  Result.X := AreaBounds.X + Max(0, (AreaBounds.W - Result.W) div 2);
  Result.Y := AreaBounds.Y + Max(0, (AreaBounds.H - Result.H) div 2);
end;

function ScaleCoordToSlot(const Value, SourceStart, SourceSize, TargetStart, TargetSize: integer): integer;
begin
  if SourceSize <= 0 then
    Exit(TargetStart);

  Result := TargetStart + Round((Value - SourceStart) * TargetSize / SourceSize);
end;

function ScaleLengthToSlot(const Value, SourceSize, TargetSize: integer): integer;
begin
  if SourceSize <= 0 then
    Exit(Value);

  Result := Round(Value * TargetSize / SourceSize);
end;

function GetFittedSlotRect(const SourceBounds, SlotRect: TPlayerSlotRect; MaxScale: real): TPlayerSlotRect;
var
  Scale: real;
begin
  Result := SlotRect;

  if (SourceBounds.W <= 0) or (SourceBounds.H <= 0) or (SlotRect.W <= 0) or (SlotRect.H <= 0) then
    Exit;

  Scale := Min(MaxScale, Min(SlotRect.W / Max(1.0, SourceBounds.W), SlotRect.H / Max(1.0, SourceBounds.H)));
  Result.W := Max(1, Round(SourceBounds.W * Scale));
  Result.H := Max(1, Round(SourceBounds.H * Scale));
  Result.X := SlotRect.X + (SlotRect.W - Result.W) div 2;
  Result.Y := SlotRect.Y;
end;

end.
