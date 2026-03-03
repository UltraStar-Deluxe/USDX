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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/base/USingScores.pas $
 * $Id: USingScores.pas 2293 2010-04-23 22:39:26Z tobigun $
 *}

unit USingScores;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  dglOpenGL,
  UCommon,
  UIni,
  UPlayerLayout,
  UThemes,
  UTexture;

//////////////////////////////////////////////////////////////
//                        ATTENTION:                        //
// Enabled flag does not work atm. This should cause popups //
// not to move and scores to stay until re-enabling.        //
// To use e.g. in pause mode                                //
// also invisible flag causes attributes not to change.     //
// This should be fixed after next draw when visible = true,//
// but not tested yet                                       //
//////////////////////////////////////////////////////////////

// some constants containing options that could change by time
const
  MaxPlayers = UIni.IMaxPlayerCount;   // maximum of players that could be added
  MaxPositions = MaxPlayers; // maximum of score positions that could be added

type
  //-----------
  // TScorePlayer - record containing information about a players score
  //-----------
  TScorePlayer = record
    Position:       byte;     // index of the position where the player should be drawn
    Enabled:        boolean;  // is the score display enabled
    Visible:        boolean;  // is the score display visible
    Score:          word;     // current score of the player
    ScoreDisplayed: word;     // score cur. displayed (for counting up)
    ScoreBG:        TTexture; // texture of the players scores bg
    Color:          TRGB;     // the players color
    RBPos:          real;     // cur. percentille of the rating bar
    RBTarget:       real;     // target position of rating bar
    RBVisible:      boolean;  // is rating bar drawn
  end;
  aScorePlayer = array [0..MaxPlayers-1] of TScorePlayer;

  //-----------
  // TScorePosition - record containing information about a score position, that can be used
  //-----------
  PScorePosition = ^TScorePosition;
  TScorePosition = record
    // the position is used for which playercount
    PlayerCount: byte;
    // 1 - 1 player per screen
    // 2 - 2 players per screen
    // 4 - 3 players per screen
    // 8 - 4 players per screen
    // 16 - 6 players per screen
    // 6 would be 2 and 3 players per screen

    BGX: real;     // x position of the score bg
    BGY: real;     // y position of the score bg
    BGW: real;     // width of the score bg
    BGH: real;     // height of the score bg

    RBX: real;     // x position of the rating bar
    RBY: real;     // y position of the rating bar
    RBW: real;     // width of the rating bar
    RBH: real;     // height of the rating bar

    TextX:     real; // x position of the score text
    TextY:     real; // y position of the score text
    TextFont:  byte; // font family of the score text
    TextStyle: byte; // font style of the score text
    TextSize:  integer;    // size of the score text

    PUW:       real;     // width of the line bonus popup
    PUH:       real;     // height of the line bonus popup
    PUFont:    byte;     // font for the popups
    PUStyle:   byte;     // font style for the popups
    PUSize:    integer;  // font size for the popups
    PUStartX:  real;     // x start position of the line bonus popup
    PUStartY:  real;     // y start position of the line bonus popup
    PUTargetX: real;     // x target position of the line bonus popup
    PUTargetY: real;     // y target position of the line bonus popup
  end;
  aScorePosition = array [0..MaxPositions-1] of TScorePosition;

  //-----------
  // TScorePopUp - record containing information about a line bonus popup
  // list, next item is saved in next attribute
  //-----------
  PScorePopUp = ^TScorePopUp;
  TScorePopUp = record
    Player:     byte;        // index of the popups player
    TimeStamp:  cardinal;    // timestamp of popups spawn
    Rating:     integer;     // 0 to 8, type of rating (cool, bad, etc.)
    ScoreGiven: integer;     // score that has already been given to the player
    ScoreDiff:  integer;     // difference between cur score at spawn and old score
    Next:       PScorePopUp; // next item in list
  end;
  aScorePopUp = array of TScorePopUp;

  //-----------
  // TSingScores - class containing scores positions and drawing scores, rating bar + popups
  //-----------
  TSingScores = class
    private
      aPositions: aScorePosition;
      aPlayers:  aScorePlayer;
      oPositionCount: byte;
      oPlayerCount:   byte;

      // saves the first and last popup of the list
      FirstPopUp: PScorePopUp;
      LastPopUp:  PScorePopUp;

      // only defined during draw, time passed between
      // current and previous call of draw
      TimePassed: Cardinal;

      // draws a popup by pointer
      procedure DrawPopUp(const PopUp: PScorePopUp);

      // has to be called after DrawPopUp and before DrawScore
      procedure DoRaiseScore(const Index: integer);

      // draws a score by playerindex
      procedure DrawScore(const Index: integer);

      // draws the rating bar by playerindex
      procedure DrawRatingBar(const Index: integer);

      // removes a popup w/o destroying the list
      procedure KillPopUp(const last, cur: PScorePopUp);

      // calculate the amount of points for a player that is
      // still in popups and therfore not displayed
      function GetPopUpPoints(const Index: integer): integer;
    public
      Settings: record // Record containing some Displaying Options
        Phase1Time: real;     // time for phase 1 to complete (in msecs)
                              // the plop up of the popup
        Phase2Time: real;     // time for phase 2 to complete (in msecs)
                              // the moving (mainly upwards) of the popup
        Phase3Time: real;     // time for phase 3 to complete (in msecs)
                              // the fade out and score adding

        PopUpTex:   array [0..8] of TTexture; // textures for every popup rating

        RatingBar_BG_Tex:  TTexture; // rating bar texs
        RatingBar_FG_Tex:  TTexture;
        RatingBar_Bar_Tex: TTexture;

      end;

      Visible:   boolean;  // visibility of all scores
      Enabled:   boolean;  // scores are changed, popups are moved etc.
      RBVisible: boolean;  // visibility of all rating bars

      // properties for reading position and playercount
      property PositionCount: byte         read oPositionCount;
      property PlayerCount:   byte         read oPlayerCount;
      property Players:       aScorePlayer read aPlayers;
      property Positions: aScorePosition read aPositions;

      // constructor just sets some standard settings
      constructor Create;

      // adds a position to array and increases position count
      procedure AddPosition(const pPosition: PScorePosition);

      // adds a player to array and increases player count
      procedure AddPlayer(const ScoreBG: TTexture; const Color: TRGB; const Score: word = 0; const Enabled: boolean = true; const Visible: boolean = true);

      // change a players visibility, enable
      procedure ChangePlayerVisibility(const Index: byte; const pVisible: boolean);
      procedure ChangePlayerEnabled(const Index: byte; const pEnabled: boolean);

      // deletes all player information
      procedure ClearPlayers;

      // deletes positions and playerinformation
      procedure Clear;

      // loads some settings and the positions from theme
      procedure LoadfromTheme;

      // has to be called after positions and players have been added, before first call of draw
      // it gives every player a score position
      procedure Init;

      // sets the score of a specified player to the specified score
      procedure SetScore(Player: byte; Score: integer);

      // spawns a new line bonus popup for the player
      procedure SpawnPopUp(const PlayerIndex: byte; const Rating: integer; const Score: integer);

      // removes all popups from mem
      procedure KillAllPopUps;

      // draws scores and line bonus popups
      procedure Draw;
  end;

implementation

uses
  SysUtils,
  Math,
  sdl2,
  TextGL,
  ULog,
  UNote,
  UGraphic;

function IsPlayerVisibleOnScreen(const EncodedPosition: byte; const Screen: integer): boolean;
begin
  if EncodedPosition = High(byte) then
    Exit(false);

  if Screens <= 1 then
    Exit(true);

  Result := (((EncodedPosition and 128) = 0) = (Screen = 1));
end;

function GetBaseSingThemePlayer: TThemeSingPlayer;
begin
  Result := Theme.Sing.Solo1PP1;
end;

function GetThemePlayerBounds(const ThemePlayer: TThemeSingPlayer): TPlayerSlotRect;
var
  MinX: integer;
  MinY: integer;
  MaxX: integer;
  MaxY: integer;
begin
  MinX := Min(ThemePlayer.ScoreBackground.X, ThemePlayer.SingBar.X);
  MinY := Min(ThemePlayer.ScoreBackground.Y, ThemePlayer.Score.Y);
  MaxX := Max(ThemePlayer.ScoreBackground.X + ThemePlayer.ScoreBackground.W,
              ThemePlayer.SingBar.X + ThemePlayer.SingBar.W);
  MaxY := Max(ThemePlayer.ScoreBackground.Y + ThemePlayer.ScoreBackground.H,
              ThemePlayer.SingBar.Y + ThemePlayer.SingBar.H);

  Result.X := MinX;
  Result.Y := MinY;
  Result.W := MaxX - MinX;
  Result.H := MaxY - MinY;
end;

function GetScoreLayoutBounds(const BaseBounds: TPlayerSlotRect; const PlayerCountOnScreen: integer; Flip: boolean): TPlayerSlotRect;
var
  Grid: TPlayerGrid;
  Scale: real;
  ScaleX: real;
  ScaleY: real;
begin
  if Flip then
    Grid := GetPlayerGrid(PlayerCountOnScreen, Flip)
  else
    Grid := GetWidePlayerGrid(PlayerCountOnScreen);
  if (Grid.Cols <= 0) or (Grid.Rows <= 0) or (BaseBounds.W <= 0) or (BaseBounds.H <= 0) then
  begin
    Result.X := BaseBounds.X;
    Result.Y := BaseBounds.Y;
    Result.W := BaseBounds.W;
    Result.H := BaseBounds.H;
    Exit;
  end;

  ScaleX := (RenderW * 0.95) / (BaseBounds.W * Grid.Cols);
  ScaleY := (RenderH * 0.45) / (BaseBounds.H * Grid.Rows);
  Scale := Min(1.0, Min(ScaleX, ScaleY));

  Result.W := Round(BaseBounds.W * Grid.Cols * Scale);
  Result.H := Round(BaseBounds.H * Grid.Rows * Scale);
  Result.X := Max(0, (RenderW - Result.W) div 2);
  Result.Y := BaseBounds.Y;
end;

function ScaleToSlot(const Value, SourceStart, SourceSize, TargetStart, TargetSize: integer): integer;
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

function TransformThemePosition(const Source: TThemePosition; const SourceBounds, SlotRect: TPlayerSlotRect): TThemePosition;
begin
  Result := Source;
  Result.X := ScaleToSlot(Source.X, SourceBounds.X, SourceBounds.W, SlotRect.X, SlotRect.W);
  Result.Y := ScaleToSlot(Source.Y, SourceBounds.Y, SourceBounds.H, SlotRect.Y, SlotRect.H);
  Result.W := ScaleLengthToSlot(Source.W, SourceBounds.W, SlotRect.W);
  Result.H := ScaleLengthToSlot(Source.H, SourceBounds.H, SlotRect.H);
end;

function TransformThemeText(const Source: TThemeText; const SourceBounds, SlotRect: TPlayerSlotRect): TThemeText;
var
  Scale: real;
begin
  Result := Source;
  Result.X := ScaleToSlot(Source.X, SourceBounds.X, SourceBounds.W, SlotRect.X, SlotRect.W);
  Result.Y := ScaleToSlot(Source.Y, SourceBounds.Y, SourceBounds.H, SlotRect.Y, SlotRect.H);
  Result.W := ScaleLengthToSlot(Source.W, SourceBounds.W, SlotRect.W);
  Result.H := ScaleLengthToSlot(Source.H, SourceBounds.H, SlotRect.H);

  Scale := Min(SlotRect.W / Max(1.0, SourceBounds.W), SlotRect.H / Max(1.0, SourceBounds.H));
  Result.Size := Max(1, Round(Source.Size * Scale));
end;

function GetSingThemePlayer(const PlayerIndex: integer): TThemeSingPlayer;
var
  BaseBounds: TPlayerSlotRect;
  LayoutBounds: TPlayerSlotRect;
  SlotRect: TPlayerSlotRect;
  BaseTemplate: TThemeSingPlayer;
  LocalPlayerCount: integer;
  LocalIndex: integer;
begin
  if Screens > 1 then
  begin
    LocalPlayerCount := GetScreenPlayerCount(PlayersPlay, Screens, ScreenAct);
    LocalIndex := GetPlayerIndexOnScreen(PlayerIndex, PlayersPlay, Screens);
  end
  else
  begin
    LocalPlayerCount := PlayersPlay;
    LocalIndex := PlayerIndex;
  end;

  BaseTemplate := GetBaseSingThemePlayer;
  BaseBounds := GetThemePlayerBounds(BaseTemplate);
  LayoutBounds := GetScoreLayoutBounds(BaseBounds, LocalPlayerCount, false);
  SlotRect := GetWidePlayerSlotRect(LocalIndex, LocalPlayerCount, LayoutBounds.X, LayoutBounds.Y, LayoutBounds.W, LayoutBounds.H);

  Result := BaseTemplate;
  Result.ScoreBackground := TransformThemePosition(BaseTemplate.ScoreBackground, BaseBounds, SlotRect);
  Result.SingBar := TransformThemePosition(BaseTemplate.SingBar, BaseBounds, SlotRect);
  Result.Score := TransformThemeText(BaseTemplate.Score, BaseBounds, SlotRect);
end;

function GetScorePositionForPlayer(const PlayerIndex: integer): TScorePosition;
var
  BaseTemplate: TThemeSingPlayer;
  PlayerCountOnScreen: integer;
  LocalIndex: integer;
  LaneLeft: integer;
  LaneRight: integer;
  LaneWidth: integer;
  Scale: real;
  FrameH: integer;
  ScoreH: integer;
  HeaderOffsetTop: integer;
  ScoreScale: real;
  ScoreOffsetX: integer;
  ScoreOffsetY: integer;
  RatingOffsetX: integer;
  RatingOffsetY: integer;
  PopupYOffset: integer;
  PopupFontSize: integer;
  GroupTop: real;
  Layout: TSingLaneLayout;
begin
  if Screens > 1 then
  begin
    PlayerCountOnScreen := GetScreenPlayerCount(PlayersPlay, Screens, ScreenAct);
    LocalIndex := GetPlayerIndexOnScreen(PlayerIndex, PlayersPlay, Screens);
  end
  else
  begin
    PlayerCountOnScreen := PlayersPlay;
    LocalIndex := PlayerIndex;
  end;

  Layout := GetSingLaneLayout(PlayerCountOnScreen, LocalIndex, CurrentSong.isDuet and (PlayersPlay <> 1));
  LaneLeft := Layout.ColumnLeft;
  LaneRight := Layout.ColumnRight;
  LaneWidth := Layout.ColumnWidth;

  BaseTemplate := GetBaseSingThemePlayer;
  Scale := Layout.WidgetScale;

  ScoreScale := Min(1.0, (LaneWidth * 0.36) / Max(1.0, BaseTemplate.ScoreBackground.W * 1.0))
    * GetPlayerWidgetScale(PlayerCountOnScreen);

  FrameH := Max(26, Round(BaseTemplate.AvatarFrame.H * Scale));
  ScoreH := Max(18, Round(BaseTemplate.ScoreBackground.H * Scale));
  HeaderOffsetTop := Round(40 * Scale);

  Result.PlayerCount := 0;
  Result.BGW := Round(BaseTemplate.ScoreBackground.W * ScoreScale);
  Result.BGH := Round(BaseTemplate.ScoreBackground.H * ScoreScale);
  Result.BGX := LaneRight - Result.BGW;
  GroupTop := Max(10, Layout.RowAnchorY -
    Max(FrameH, ScoreH) - 18 - HeaderOffsetTop);
  Result.BGY := GroupTop;

  ScoreOffsetX := BaseTemplate.Score.X - BaseTemplate.ScoreBackground.X;
  ScoreOffsetY := BaseTemplate.Score.Y - BaseTemplate.ScoreBackground.Y;
  Result.TextX := Result.BGX + Round(ScoreOffsetX * ScoreScale);
  Result.TextY := Result.BGY + Round(ScoreOffsetY * ScoreScale);
  Result.TextFont := BaseTemplate.Score.Font;
  Result.TextStyle := BaseTemplate.Score.Style;
  Result.TextSize := Max(1, Round(BaseTemplate.Score.Size * ScoreScale));

  RatingOffsetX := BaseTemplate.SingBar.X - BaseTemplate.ScoreBackground.X;
  RatingOffsetY := BaseTemplate.SingBar.Y - BaseTemplate.ScoreBackground.Y;
  Result.RBX := Result.BGX + Round(RatingOffsetX * ScoreScale);
  Result.RBY := Result.BGY + Round(RatingOffsetY * ScoreScale);
  Result.RBW := Round(BaseTemplate.SingBar.W * ScoreScale);
  Result.RBH := Round(BaseTemplate.SingBar.H * ScoreScale);

  if CurrentSong.isDuet then
  begin
    PopupYOffset := 40;
    PopupFontSize := 14;
  end
  else
  begin
    PopupYOffset := 65;
    PopupFontSize := 18;
  end;

  Result.PUW := Result.BGW;
  Result.PUH := Result.BGH;
  Result.PUFont := 0;
  Result.PUStyle := ftOutline;
  Result.PUSize := PopupFontSize;
  Result.PUStartX := Result.BGX;
  Result.PUStartY := Result.TextY + PopupYOffset;
  Result.PUTargetX := Result.BGX;
  Result.PUTargetY := Result.TextY;
end;

{**
 * sets some standard settings
 *}
constructor TSingScores.Create;
begin
  inherited;

  // clear popuplist pointers
  FirstPopUp := nil;
  LastPopUp  := nil;

  // clear variables
  Visible   := true;
  Enabled   := true;
  RBVisible := true;
  
  // clear position index
  oPositionCount := 0;
  oPlayerCount   := 0;

  Settings.Phase1Time := 350;  // plop it up     . -> [   ]
  Settings.Phase2Time := 550;  // shift it up        ^[   ]^
  Settings.Phase3Time := 200;  // increase score      [s++]

  Settings.PopUpTex[0].TexNum := 0;
  Settings.PopUpTex[1].TexNum := 0;
  Settings.PopUpTex[2].TexNum := 0;
  Settings.PopUpTex[3].TexNum := 0;
  Settings.PopUpTex[4].TexNum := 0;
  Settings.PopUpTex[5].TexNum := 0;
  Settings.PopUpTex[6].TexNum := 0;
  Settings.PopUpTex[7].TexNum := 0;
  Settings.PopUpTex[8].TexNum := 0;

  Settings.RatingBar_BG_Tex.TexNum   := 0;
  Settings.RatingBar_FG_Tex.TexNum   := 0;
  Settings.RatingBar_Bar_Tex.TexNum  := 0;
end;

{**
 * adds a position to array and increases position count
 *}
procedure TSingScores.AddPosition(const pPosition: PScorePosition);
begin
  if (PositionCount < MaxPositions) then
  begin
    aPositions[PositionCount] := pPosition^;
    Inc(oPositionCount);
  end;
end;

{**
 * adds a player to array and increases player count
 *}
procedure TSingScores.AddPlayer(const ScoreBG: TTexture; const Color: TRGB; const Score: word; const Enabled: boolean; const Visible: boolean);
begin
  if (PlayerCount < MaxPlayers) then
  begin
    aPlayers[PlayerCount].Position  := High(byte);
    aPlayers[PlayerCount].Enabled   := Enabled;
    aPlayers[PlayerCount].Visible   := Visible;
    aPlayers[PlayerCount].Score     := Score;
    aPlayers[PlayerCount].ScoreDisplayed := Score;
    aPlayers[PlayerCount].ScoreBG   := ScoreBG;
    aPlayers[PlayerCount].Color     := Color;
    aPlayers[PlayerCount].RBPos     := 0.5;
    aPlayers[PlayerCount].RBTarget  := 0.5;
    aPlayers[PlayerCount].RBVisible := true;

    Inc(oPlayerCount);
  end;
end;

{**
 * change a players visibility
 *}
procedure TSingScores.ChangePlayerVisibility(const Index: byte; const pVisible: boolean);
begin
  if (Index < MaxPlayers) then
    aPlayers[Index].Visible := pVisible;
end;
{**
 * change player enabled
 *}
procedure TSingScores.ChangePlayerEnabled(const Index: byte; const pEnabled: boolean);
begin
  if (Index < MaxPlayers) then
    aPlayers[Index].Enabled := pEnabled;
end;
{**
 * procedure deletes all player information
 *}
procedure TSingScores.ClearPlayers;
begin
  KillAllPopUps;
  oPlayerCount := 0;
  TimePassed := 0;
end;

{**
 * procedure deletes positions and playerinformation
 *}
procedure TSingScores.Clear;
begin
  KillAllPopUps;
  oPlayerCount    := 0;
  oPositionCount  := 0;
  TimePassed := 0;
end;

{**
 * procedure loads some settings and the positions from theme
 *}
procedure TSingScores.LoadfromTheme;
var
  I: integer;
begin
  Clear;

  // set textures
  // popup tex
  for I := 0 to 8 do
    Settings.PopUpTex[I] := Tex_SingLineBonusBack[I];

  // rating bar tex
  Settings.RatingBar_BG_Tex   :=  Tex_SingBar_Back;
  Settings.RatingBar_FG_Tex   :=  Tex_SingBar_Front;
  Settings.RatingBar_Bar_Tex  :=  Tex_SingBar_Bar;
end;

{**
 * sets the score of a specified player to the specified score
 *}
procedure TSingScores.SetScore(Player: byte; Score: integer);
  var
    Diff: Integer;
begin
  if (Player <= PlayerCount - 1) then
  begin
    Diff := Score - Players[Player].Score;
    aPlayers[Player].Score := Score;
    Inc(aPlayers[Player].ScoreDisplayed, Diff);
  end;
end;

{**
 * spawns a new line bonus popup for the player
 *}
procedure TSingScores.SpawnPopUp(const PlayerIndex: byte; const Rating: integer; const Score: integer);
var
  Cur: PScorePopUp;
begin
  if (PlayerIndex < PlayerCount) then
  begin
    // get memory and add data
    GetMem(Cur, SizeOf(TScorePopUp));

    Cur.Player    := PlayerIndex;
    Cur.TimeStamp := SDL_GetTicks;

    // limit rating value to 0..8
    // a higher value would cause a crash when selecting the bg texture
    if (Rating > 8) then
      Cur.Rating := 8
    else if (Rating < 0) then
      Cur.Rating := 0
    else
      Cur.Rating := Rating;

    Cur.ScoreGiven:= 0;
    if (Players[PlayerIndex].Score < Score) then
    begin
      Cur.ScoreDiff := Score - Players[PlayerIndex].Score;
      aPlayers[PlayerIndex].Score := Score;
    end
    else
      Cur.ScoreDiff := 0;
    Cur.Next := nil;

    // Log.LogError('TSingScores.SpawnPopUp| Player: ' + InttoStr(PlayerIndex) + ', Score: ' + InttoStr(Score) + ', ScoreDiff: ' + InttoStr(Cur.ScoreDiff));

    // add it to the chain
    if (FirstPopUp = nil) then
      // the first popup in the list
      FirstPopUp := Cur
    else
    // second or earlier popup
      LastPopUp.Next := Cur;

    // set new popup to last popup in the list
    LastPopUp := Cur;
  end
  else
    Log.LogError('TSingScores: Try to add popup for non-existing player');
end;

{**
 * removes a popup w/o destroying the list
 *}
procedure TSingScores.KillPopUp(const last, cur: PScorePopUp);
begin
  // give player the last points that missing till now
  aPlayers[Cur.Player].ScoreDisplayed := aPlayers[Cur.Player].ScoreDisplayed + Cur.ScoreDiff - Cur.ScoreGiven;

  // change bars position
  if (Cur.ScoreDiff > 0) THEN
  begin // popup w/ scorechange -> give missing percentille
    aPlayers[Cur.Player].RBTarget := aPlayers[Cur.Player].RBTarget +
                                     (Cur.ScoreDiff - Cur.ScoreGiven) / Cur.ScoreDiff
                                     * (Cur.Rating / 20 - 0.26);
  end
  else
  begin // popup w/o scorechange -> give complete percentille
    aPlayers[Cur.Player].RBTarget := aPlayers[Cur.Player].RBTarget +
                                     (Cur.Rating / 20 - 0.26);
  end;

  if (aPlayers[Cur.Player].RBTarget > 1) then
    aPlayers[Cur.Player].RBTarget := 1
  else
  if (aPlayers[Cur.Player].RBTarget < 0) then
    aPlayers[Cur.Player].RBTarget := 0;

  // if this is the first popup => make next popup the first
  if (Cur = FirstPopUp) then
    FirstPopUp := Cur.Next
  // else => remove curent popup from chain
  else
    Last.Next := Cur.Next;

  // if this is the last popup, make popup before the last
  if (Cur = LastPopUp) then
    LastPopUp := Last;

  // free the memory
  FreeMem(Cur, SizeOf(TScorePopUp));
end;

{**
 * removes all popups from mem
 *}
procedure TSingScores.KillAllPopUps;
var
  Cur:  PScorePopUp;
  Last: PScorePopUp;
begin
  Cur := FirstPopUp;

  // remove all popups:
  while (Cur <> nil) do
  begin
    Last := Cur;
    Cur  := Cur.Next;
    FreeMem(Last, SizeOf(TScorePopUp));
  end;

  FirstPopUp := nil;
  LastPopUp := nil;
end;

{**
 * calculate the amount of points for a player that is
 * still in popups and therfore not displayed
 *}
function TSingScores.GetPopUpPoints(const Index: integer): integer;
  var
    CurPopUp: PScorePopUp;
begin
  Result := 0;
  
  CurPopUp := FirstPopUp;
  while (CurPopUp <> nil) do
  begin
    if (CurPopUp.Player = Index) then
    begin // add points left "in" popup to result
      Inc(Result, CurPopUp.ScoreDiff - CurPopUp.ScoreGiven);
    end;
    CurPopUp := CurPopUp.Next;
  end;
end;

{**
 * has to be called after positions and players have been added, before first call of draw
 * it gives each player a score position
 *}
procedure TSingScores.Init;
var
  I: integer;
begin
  for I := 0 to PlayerCount - 1 do
  begin
    if Screens > 1 then
      aPlayers[I].Position := GetPlayerIndexOnScreen(I, PlayerCount, Screens) or ((GetPlayerScreen(I, PlayerCount, Screens) - 1) shl 7)
    else
      aPlayers[I].Position := I;
  end;
end;

{**
 * draws scores and linebonus popups
 *}
procedure TSingScores.Draw;
var
  I: integer;
  CurTime: cardinal;
  CurPopUp, LastPopUp: PScorePopUp;
begin
  CurTime := SDL_GetTicks;
  if (TimePassed <> 0) then
    TimePassed := CurTime - TimePassed;

  if Visible then
  begin
    // draw popups
    LastPopUp := nil;
    CurPopUp  := FirstPopUp;

    while (CurPopUp <> nil) do
    begin
      if (CurTime - CurPopUp.TimeStamp > Settings.Phase1Time + Settings.Phase2Time + Settings.Phase3Time) then
      begin
        KillPopUp(LastPopUp, CurPopUp);
        if (LastPopUp = nil) then
          CurPopUp := FirstPopUp
        else
          CurPopUp  := LastPopUp.Next;
      end
      else
      begin
        DrawPopUp(CurPopUp);
        LastPopUp := CurPopUp;
        CurPopUp  := LastPopUp.Next;
      end;
    end;


    if (RBVisible) then
      // draw players w/ rating bar
      for I := 0 to PlayerCount-1 do
      begin
        DoRaiseScore(I);
        DrawScore(I);
        DrawRatingBar(I);
      end
    else
      // draw players w/o rating bar
      for I := 0 to PlayerCount-1 do
      begin
        DoRaiseScore(I);
        DrawScore(I);
      end;

  end; // eo visible

  TimePassed := CurTime;
end;

{**
 * has to be called after DrawPopUp and before DrawScore
 *}
procedure TSingScores.DoRaiseScore(const Index: integer);
  var
    S: integer;
    Diff: integer;
  const
    RaisePerSecond = 500;
begin
  S := (Players[Index].Score - (Players[Index].ScoreDisplayed + GetPopUpPoints(Index)));

  if (S <> 0) then
  begin
    Diff := Round(RoundTo((RaisePerSecond * TimePassed) / 1000, 1));

    { minimal raise per frame = 1 }
    if Abs(Diff) < 1 then
      Diff := Sign(S);

    if (Abs(Diff) < Abs(S)) then
      Inc(aPlayers[Index].ScoreDisplayed, Diff)
    else
      Inc(aPlayers[Index].ScoreDisplayed, S);
  end;
end;

{**
 * draws a popup by pointer
 *}
procedure TSingScores.DrawPopUp(const PopUp: PScorePopUp);
var
  Progress:          real;
  CurTime:           cardinal;
  X, Y, W, H, Alpha: real;
  FontSize:          integer;
  FontOffset:        real;
  TimeDiff:          cardinal;
  PIndex:            byte;
  TextLen:           real;
  ScoretoAdd:        word;
  PosDiff:           real;
  Position:          TScorePosition;
begin
  if (PopUp <> nil) then
  begin
    // only draw if player has a position
    PIndex := Players[PopUp.Player].Position;
    if PIndex <> High(byte) then
    begin
      // only draw if player is on cur screen
      if IsPlayerVisibleOnScreen(Players[PopUp.Player].Position, ScreenAct) then
      begin
        CurTime := SDL_GetTicks;
        if not (Enabled and Players[PopUp.Player].Enabled) then
        // increase timestamp with tiem where there is no movement ...
        begin
          // Inc(PopUp.TimeStamp, LastRender);
        end;
        TimeDiff := CurTime - PopUp.TimeStamp;

        // get position of popup
        PIndex := PIndex and 127;
        Position := GetScorePositionForPlayer(PopUp.Player);

        // check for phase ...
        if (TimeDiff <= Settings.Phase1Time) then
        begin
          // phase 1 - the ploping up
          Progress := TimeDiff / Settings.Phase1Time;


          W := Position.PUW * Sin(Progress/2*Pi);
          H := Position.PUH * Sin(Progress/2*Pi);

          X := Position.PUStartX + (Position.PUW - W)/2;
          Y := Position.PUStartY + (Position.PUH - H)/2;

          FontSize   := Round(Progress * Position.PUSize);
          FontOffset := (H - FontSize) / 2;
          Alpha := 1;
        end

        else if (TimeDiff <= Settings.Phase2Time + Settings.Phase1Time) then
        begin
          // phase 2 - the moving
          Progress := (TimeDiff - Settings.Phase1Time) / Settings.Phase2Time;

          W := Position.PUW;
          H := Position.PUH;

          PosDiff := Position.PUTargetX - Position.PUStartX;
          if PosDiff > 0 then
            PosDiff := PosDiff + W;
          X := Position.PUStartX + PosDiff * sqr(Progress);

          PosDiff := Position.PUTargetY - Position.PUStartY;
          if PosDiff < 0 then
            PosDiff := PosDiff + Position.BGH;
          Y := Position.PUStartY + PosDiff * sqr(Progress);

          FontSize   := Position.PUSize;
          FontOffset := (H - FontSize) / 2;
          Alpha := 1 - 0.3 * Progress;
        end

        else
        begin
          // phase 3 - the fading out + score adding
          Progress := (TimeDiff - Settings.Phase1Time - Settings.Phase2Time) / Settings.Phase3Time;

          if (PopUp.Rating > 0) then
          begin
            // add scores if player enabled
            if (Enabled and Players[PopUp.Player].Enabled) then
            begin
              ScoreToAdd := Round(PopUp.ScoreDiff * Progress) - PopUp.ScoreGiven;
              Inc(PopUp.ScoreGiven, ScoreToAdd);
              aPlayers[PopUp.Player].ScoreDisplayed := Players[PopUp.Player].ScoreDisplayed + ScoreToAdd;

              // change bar positions
              if PopUp.ScoreDiff = 0 then
                aPlayers[PopUp.Player].RBTarget := 0
              else
                aPlayers[PopUp.Player].RBTarget := aPlayers[PopUp.Player].RBTarget + ScoreToAdd/PopUp.ScoreDiff * (PopUp.Rating / 20 - 0.26);
              if (aPlayers[PopUp.Player].RBTarget > 1) then
                aPlayers[PopUp.Player].RBTarget := 1
              else if (aPlayers[PopUp.Player].RBTarget < 0) then
                aPlayers[PopUp.Player].RBTarget := 0;
            end;

            // set positions etc.
            Alpha := 0.7 - 0.7 * Progress;

            W := Position.PUW;
            H := Position.PUH;

            PosDiff := Position.PUTargetX - Position.PUStartX;
            if (PosDiff > 0) then
              PosDiff := W
            else
              PosDiff := 0;
            X := Position.PUTargetX + PosDiff * Progress;

            PosDiff := Position.PUTargetY - Position.PUStartY;
            if (PosDiff < 0) then
              PosDiff := -Position.BGH
            else
              PosDiff := 0;
            Y := Position.PUTargetY - PosDiff * (1 - Progress);

            FontSize   := Position.PUSize;
            FontOffset := (H - FontSize) / 2;
          end
          else
          begin
            // here the effect that should be shown if a popup without score is drawn
            // and or spawn with the graphicobjects etc.
            // some work for blindy to do :p

            // atm: just let it slide in the scores just like the normal popup
            Alpha := 0;
          end;
        end;

        // draw popup

        if (Alpha > 0) and (Players[PopUp.Player].Visible) then
        begin
          // draw bg:
          glEnable(GL_TEXTURE_2D);
          glEnable(GL_BLEND);
          glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

          glColor4f(1,1,1, Alpha);
          glBindTexture(GL_TEXTURE_2D, Settings.PopUpTex[PopUp.Rating].TexNum);

          glBegin(GL_QUADS);
            glTexCoord2f(0, 0); glVertex2f(X, Y);
            glTexCoord2f(0, Settings.PopUpTex[PopUp.Rating].TexH); glVertex2f(X, Y + H);
            glTexCoord2f(Settings.PopUpTex[PopUp.Rating].TexW, Settings.PopUpTex[PopUp.Rating].TexH); glVertex2f(X + W, Y + H);
            glTexCoord2f(Settings.PopUpTex[PopUp.Rating].TexW, 0); glVertex2f(X + W, Y);
          glEnd;

          glDisable(GL_TEXTURE_2D);
          glDisable(GL_BLEND);

          // set font style and size
          SetFontFamily(Position.PUFont);
          SetFontStyle(Position.PUStyle);
          SetFontItalic(false);
          SetFontSize(FontSize);
          SetFontReflection(false, 0);

          // draw text
          TextLen := glTextWidth(Theme.Sing.LineBonusText[PopUp.Rating]);

          // color and pos
          SetFontPos (X + (W - TextLen) / 2, Y + FontOffset);
          glColor4f(1, 1, 1, Alpha);

          // draw
          glPrint(Theme.Sing.LineBonusText[PopUp.Rating]);
        end; // eo alpha check
      end; // eo right screen
    end; // eo player has position
  end
  else
    Log.LogError('TSingScores: Try to draw a non-existing popup');
end;

{**
 * draws a score by playerindex
 *}
procedure TSingScores.DrawScore(const Index: integer);
var
  Position: TScorePosition;
  ScoreStr: String;
  Drawing: boolean;
begin
  Drawing := false;

  if Players[Index].Position <> High(byte) then
  begin
    if IsPlayerVisibleOnScreen(Players[Index].Position, ScreenAct) and Players[Index].Visible then
    begin
      Position := GetScorePositionForPlayer(Index);
      Drawing := true;
    end;
  end;

  if (Drawing) then
  begin
    // draw scorebg
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glColor4f(1,1,1, 1);
    glBindTexture(GL_TEXTURE_2D, Players[Index].ScoreBG.TexNum);

    glBegin(GL_QUADS);
      glTexCoord2f(0, 0); glVertex2f(Position.BGX, Position.BGY);
      glTexCoord2f(0, Players[Index].ScoreBG.TexH); glVertex2f(Position.BGX, Position.BGY + Position.BGH);
      glTexCoord2f(Players[Index].ScoreBG.TexW, Players[Index].ScoreBG.TexH); glVertex2f(Position.BGX + Position.BGW, Position.BGY + Position.BGH);
      glTexCoord2f(Players[Index].ScoreBG.TexW, 0); glVertex2f(Position.BGX + Position.BGW, Position.BGY);
    glEnd;

    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);

    // draw score text
    SetFontFamily(Position.TextFont);
    SetFontStyle(Position.TextStyle);
    SetFontItalic(false);
    SetFontSize(Position.TextSize);
    SetFontPos(Position.TextX, Position.TextY);
    SetFontReflection(false, 0);

    ScoreStr := InttoStr(Players[Index].ScoreDisplayed div 10) + '0';
    while (Length(ScoreStr) < 5) do
      ScoreStr := '0' + ScoreStr;

    glPrint(ScoreStr);
  end; // eo player has position
end;


procedure TSingScores.DrawRatingBar(const Index: integer);
var
  Position:   TScorePosition;
  R, G, B:    real;
  Size, Diff: real;
  Drawing: boolean;
begin
  Drawing := false;

  if Players[Index].Position <> High(byte) then
  begin
    if IsPlayerVisibleOnScreen(Players[Index].Position, ScreenAct) and
       Players[index].RBVisible and
       Players[index].Visible then
    begin
      Drawing := true;
      Position := GetScorePositionForPlayer(Index);
    end;
  end;

  if (Drawing) then
  begin
    if (Enabled and Players[Index].Enabled) then
    begin
      // move position if enabled
      Diff := Players[Index].RBTarget - Players[Index].RBPos;
      if (Abs(Diff) < 0.02) then
        aPlayers[Index].RBPos := aPlayers[Index].RBTarget
      else
        aPlayers[Index].RBPos := aPlayers[Index].RBPos + Diff*0.1;
    end;

    // get colors for rating bar
    if (Players[index].RBPos <= 0.22) then
    begin
      R := 1;
      G := 0;
      B := 0;
    end
    else if (Players[index].RBPos <= 0.42) then
    begin
      R := 1;
      G := Players[index].RBPos * 5;
      B := 0;
    end
    else if (Players[index].RBPos <= 0.57) then
    begin
      R := 1;
      G := 1;
      B := 0;
    end
    else if (Players[index].RBPos <= 0.77) then
    begin
      R := 1 - (Players[index].RBPos - 0.57) * 5;
      G := 1;
      B := 0;
    end
    else
    begin
      R := 0;
      G := 1;
      B := 0;
    end;

    // enable all glfuncs needed
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    // draw rating bar bg
    glColor4f(1, 1, 1, 0.8);
    glBindTexture(GL_TEXTURE_2D, Settings.RatingBar_BG_Tex.TexNum);

    glBegin(GL_QUADS);
      glTexCoord2f(0, 0);
      glVertex2f(Position.RBX, Position.RBY);

      glTexCoord2f(0, Settings.RatingBar_BG_Tex.TexH);
      glVertex2f(Position.RBX, Position.RBY+Position.RBH);

      glTexCoord2f(Settings.RatingBar_BG_Tex.TexW, Settings.RatingBar_BG_Tex.TexH);
      glVertex2f(Position.RBX+Position.RBW, Position.RBY+Position.RBH);

      glTexCoord2f(Settings.RatingBar_BG_Tex.TexW, 0);
      glVertex2f(Position.RBX+Position.RBW, Position.RBY);
    glEnd;

    // draw rating bar itself
    Size := Position.RBX + Position.RBW * Players[Index].RBPos;
    glColor4f(R, G, B, 1);
    glBindTexture(GL_TEXTURE_2D, Settings.RatingBar_Bar_Tex.TexNum);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 0);
      glVertex2f(Position.RBX, Position.RBY);

      glTexCoord2f(0, Settings.RatingBar_Bar_Tex.TexH);
      glVertex2f(Position.RBX, Position.RBY + Position.RBH);

      glTexCoord2f(Settings.RatingBar_Bar_Tex.TexW, Settings.RatingBar_Bar_Tex.TexH);
      glVertex2f(Size, Position.RBY + Position.RBH);

      glTexCoord2f(Settings.RatingBar_Bar_Tex.TexW, 0);
      glVertex2f(Size, Position.RBY);
    glEnd;

    // draw rating bar fg (the thing with the 3 lines to get better readability)
    glColor4f(1, 1, 1, 0.6);
    glBindTexture(GL_TEXTURE_2D, Settings.RatingBar_FG_Tex.TexNum);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 0);
      glVertex2f(Position.RBX, Position.RBY);

      glTexCoord2f(0, Settings.RatingBar_FG_Tex.TexH);
      glVertex2f(Position.RBX, Position.RBY + Position.RBH);

      glTexCoord2f(Settings.RatingBar_FG_Tex.TexW, Settings.RatingBar_FG_Tex.TexH);
      glVertex2f(Position.RBX + Position.RBW, Position.RBY + Position.RBH);

      glTexCoord2f(Settings.RatingBar_FG_Tex.TexW, 0);
      glVertex2f(Position.RBX + Position.RBW, Position.RBY);
    glEnd;

    // disable all enabled glfuncs
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
  end; // eo Player has Position
end;

end.
