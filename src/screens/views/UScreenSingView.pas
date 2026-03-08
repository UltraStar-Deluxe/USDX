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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenSing.pas $
 * $Id: UScreenSing.pas 3150 2015-10-20 00:07:57Z basisbit $
 *}

unit UScreenSingView;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  sdl2,
  dglOpenGL,
  TextGL,
  UCommon,
  UGraphicClasses,
  UHookableEvent,
  UIni,
  ULog,
  ULyrics,
  UAvatars,
  UMenu,
  UMusic,
  UPlayerLayout,
  USingScores,
  USongs,
  UTexture,
  UThemes,
  UTime,
  USkins;

type
  TScreenSingView = class
  public

    //StaticDuet: array of cardinal;
    // lyrics bar fields
    StaticLyricsBar: integer;
    StaticLyricsBarDuet: integer;

    // timebar fields
    StaticTimeBar: integer;
    StaticTimeProgress: integer;
    TextTimeLabelText: integer;
    TextTimeText: integer;

    PlayerFrameSlots:  array [1..UIni.IMaxPlayerCount, 0..UIni.IMaxPlayerCount-1] of integer;
    PlayerAvatarSlots: array [1..UIni.IMaxPlayerCount, 0..UIni.IMaxPlayerCount-1] of integer;
    PlayerTextSlots:   array [1..UIni.IMaxPlayerCount, 0..UIni.IMaxPlayerCount-1] of integer;


    StaticPausePopup: integer;

    SongNameStatic:   integer;
    SongNameText:     integer;



    constructor Create;
    destructor Destroy; override;

    procedure DrawMedleyCountdown();
    function Draw: boolean;

    procedure SwapToScreen(Screen: integer);

    procedure WriteMessage(msg: UTF8String);
    procedure FadeMessage();
    procedure CloseMessage();

    procedure MedleyTitleFadeOut();

    function GetLyricColor(Color: integer): TRGB;

    procedure DrawInfoLyricBar();
  end;
var
  lastVolume: single;

implementation

uses
  Classes,
  Math,
  UDatabase,
  UDllManager,
  UDraw,
  UGraphic,
  ULanguage,
  UNote,
  URecord,
  USong,
  UDisplay,
  UParty,
  UPathUtils,
  UUnicodeUtils,
  UScreenSingController,
  UWebcam,
  UWebSDK;

const
  MAX_MESSAGE = 3;

type
  TSlotArray = array of integer;

procedure GetSingWidgetSlots(const View: TScreenSingView; LayoutPlayerCount: integer;
  out FrameSlots, AvatarSlots: TSlotArray);
var
  SlotIndex: integer;
begin
  SetLength(FrameSlots, LayoutPlayerCount);
  SetLength(AvatarSlots, LayoutPlayerCount);
  for SlotIndex := 0 to LayoutPlayerCount - 1 do
  begin
    FrameSlots[SlotIndex] := View.PlayerFrameSlots[LayoutPlayerCount, SlotIndex];
    AvatarSlots[SlotIndex] := View.PlayerAvatarSlots[LayoutPlayerCount, SlotIndex];
  end;
end;

procedure GetSingTextSlots(const View: TScreenSingView; LayoutPlayerCount: integer;
  out TextSlots: TSlotArray);
var
  SlotIndex: integer;
begin
  SetLength(TextSlots, LayoutPlayerCount);
  for SlotIndex := 0 to LayoutPlayerCount - 1 do
    TextSlots[SlotIndex] := View.PlayerTextSlots[LayoutPlayerCount, SlotIndex];
end;

function GetSingPlayerColor(PlayerIndex: integer): TRGB;
begin
  Result := GetPlayerColor(Ini.PlayerColor[PlayerIndex]);
end;

function BuildSingPlayerTemplate(const PlayerCountOnScreen, PlayerIndexOnScreen: integer): TThemeSingPlayer;
var
  BaseTemplate: TThemeSingPlayer;
  LaneLeft: integer;
  LaneRight: integer;
  LaneTop: integer;
  LaneWidth: integer;
  Scale: real;
  FrameW: integer;
  FrameH: integer;
  AvatarInsetX: integer;
  AvatarInsetY: integer;
  ScoreW: integer;
  ScoreH: integer;
  NameX: integer;
  NameY: integer;
  NameW: integer;
  GroupTop: integer;
  HeaderOffsetLeft: integer;
  Layout: TSingLaneLayout;
begin
  BaseTemplate := Theme.Sing.PlayerTemplate;
  Layout := GetSingLaneLayout(PlayerCountOnScreen, PlayerIndexOnScreen, Theme.Sing.PlayerLayout,
    CurrentSong.isDuet and (PlayersPlay <> 1));
  LaneLeft := Layout.ColumnLeft;
  LaneRight := Layout.ColumnRight;
  LaneTop := Layout.RowAnchorY;
  LaneWidth := Layout.ColumnWidth;
  Scale := Layout.WidgetScale;

  FrameW := Max(Theme.Sing.PlayerWidgetLayout.MinFrameW, Round(BaseTemplate.AvatarFrame.W * Scale));
  FrameH := Max(Theme.Sing.PlayerWidgetLayout.MinFrameH, Round(BaseTemplate.AvatarFrame.H * Scale));
  ScoreW := Max(Theme.Sing.PlayerWidgetLayout.MinScoreW, Round(BaseTemplate.ScoreBackground.W * Scale));
  ScoreH := Max(Theme.Sing.PlayerWidgetLayout.MinScoreH, Round(BaseTemplate.ScoreBackground.H * Scale));
  HeaderOffsetLeft := Round(Theme.Sing.PlayerWidgetLayout.HeaderOffsetLeft * Scale);
  GroupTop := Max(10, LaneTop -
    GetSingHeaderTopOffset(Theme.Sing.PlayerWidgetLayout, Layout.GridRows, Scale));
  AvatarInsetX := Max(Theme.Sing.PlayerWidgetLayout.MinAvatarInsetX,
    Round((BaseTemplate.Avatar.X - BaseTemplate.AvatarFrame.X) * Scale));
  AvatarInsetY := Max(Theme.Sing.PlayerWidgetLayout.MinAvatarInsetY,
    Round((BaseTemplate.Avatar.Y - BaseTemplate.AvatarFrame.Y) * Scale));
  NameX := Max(0, LaneLeft - HeaderOffsetLeft) + FrameW +
    Max(Theme.Sing.PlayerWidgetLayout.NameGapMinX, Round(Theme.Sing.PlayerWidgetLayout.NameGapBaseX * Scale));
  NameW := Max(Theme.Sing.PlayerWidgetLayout.NameMinW,
    (LaneRight - ScoreW - Max(Theme.Sing.PlayerWidgetLayout.NameGapMinX,
    Round(Theme.Sing.PlayerWidgetLayout.NameGapBaseX * Scale))) - NameX);
  NameY := GroupTop + Max(0, (FrameH - Max(12, Round(BaseTemplate.Name.Size * Scale))) div 2);

  Result := BaseTemplate;
  Result.AvatarFrame.X := Max(0, LaneLeft - HeaderOffsetLeft);
  Result.AvatarFrame.Y := GroupTop;
  Result.AvatarFrame.W := FrameW;
  Result.AvatarFrame.H := FrameH;

  Result.Avatar.X := Result.AvatarFrame.X + AvatarInsetX;
  Result.Avatar.Y := Result.AvatarFrame.Y + AvatarInsetY;
  Result.Avatar.W := Max(1, FrameW - 2 * AvatarInsetX);
  Result.Avatar.H := Max(1, FrameH - 2 * AvatarInsetY);

  Result.Name.X := Max(0, NameX - Max(Theme.Sing.PlayerWidgetLayout.NamePaddingMinX,
    Round(Theme.Sing.PlayerWidgetLayout.NamePaddingBaseX * Scale)));
  Result.Name.Y := Max(0, NameY - Max(Theme.Sing.PlayerWidgetLayout.NamePaddingMinY,
    Round(Theme.Sing.PlayerWidgetLayout.NamePaddingBaseY * Scale)));
  Result.Name.W := NameW;
  Result.Name.H := Max(Theme.Sing.PlayerWidgetLayout.NameMinH, Round(BaseTemplate.Name.H * Scale));
  Result.Name.Size := Max(Theme.Sing.PlayerWidgetLayout.NameMinSize, Round(BaseTemplate.Name.Size * Scale));
  if LaneWidth <= 0 then
    Result.Name.W := 0;
end;

procedure ApplySingPlayerTemplate(const SingPlayer: TThemeSingPlayer; FrameSlot, AvatarSlot, TextSlot: integer);
begin
  ScreenSing.Statics[FrameSlot].Texture.X := SingPlayer.AvatarFrame.X;
  ScreenSing.Statics[FrameSlot].Texture.Y := SingPlayer.AvatarFrame.Y;
  ScreenSing.Statics[FrameSlot].Texture.W := SingPlayer.AvatarFrame.W;
  ScreenSing.Statics[FrameSlot].Texture.H := SingPlayer.AvatarFrame.H;
  ScreenSing.Statics[FrameSlot].Texture.Z := SingPlayer.AvatarFrame.Z;
  ScreenSing.Statics[FrameSlot].Texture.Alpha := SingPlayer.AvatarFrame.Alpha;

  ScreenSing.Statics[AvatarSlot].Texture.X := SingPlayer.Avatar.X;
  ScreenSing.Statics[AvatarSlot].Texture.Y := SingPlayer.Avatar.Y;
  ScreenSing.Statics[AvatarSlot].Texture.W := SingPlayer.Avatar.W;
  ScreenSing.Statics[AvatarSlot].Texture.H := SingPlayer.Avatar.H;
  ScreenSing.Statics[AvatarSlot].Texture.Z := SingPlayer.Avatar.Z;
  ScreenSing.Statics[AvatarSlot].Texture.Alpha := SingPlayer.Avatar.Alpha;

  ScreenSing.Text[TextSlot].X := SingPlayer.Name.X;
  ScreenSing.Text[TextSlot].Y := SingPlayer.Name.Y;
  ScreenSing.Text[TextSlot].W := SingPlayer.Name.W;
  ScreenSing.Text[TextSlot].H := SingPlayer.Name.H;
  ScreenSing.Text[TextSlot].Z := SingPlayer.Name.Z;
  ScreenSing.Text[TextSlot].Size := SingPlayer.Name.Size;
end;

//ToDo basisbit: check this again
// Dirty HacK
procedure TScreenSingView.SwapToScreen(Screen: integer);
var
  LocalPlayerCount: integer;
  FirstPlayerIndex: integer;
  FrameSlots: TSlotArray;
  AvatarSlots: TSlotArray;
  IterLayoutPlayerCount: integer;
  procedure setVisible(const elements: TSlotArray; visible: boolean);
  var
    J: integer;
  begin
    for J := 0 to High(elements) do
      ScreenSing.Statics[elements[J]].Visible := visible;
  end;
  procedure hide(const elements: TSlotArray);
  begin
    setVisible(elements, false);
  end;
  procedure maybeShowCount(const elements: TSlotArray; Count: integer);
  var
    J: integer;
  begin
    if not ScreenSing.Settings.AvatarsVisible then
      Exit;

    for J := 0 to Count - 1 do
      ScreenSing.Statics[elements[J]].Visible := true;
  end;
  procedure hideGroup(APlayerCount: integer);
  begin
    GetSingWidgetSlots(Self, APlayerCount, FrameSlots, AvatarSlots);
    hide(FrameSlots);
    hide(AvatarSlots);
  end;
  procedure setStaticColor(StaticIndex, PlayerIndex: integer);
  var
    PlayerColor: TRGB;
  begin
    PlayerColor := GetPlayerColor(Ini.SingColor[PlayerIndex]);
    ScreenSing.Statics[StaticIndex].Texture.ColR := PlayerColor.R;
    ScreenSing.Statics[StaticIndex].Texture.ColG := PlayerColor.G;
    ScreenSing.Statics[StaticIndex].Texture.ColB := PlayerColor.B;
  end;
  procedure setVisibleSlotColors(const Slots: TSlotArray; Count, FirstIndex: integer);
  var
    J: integer;
  begin
    for J := 0 to Count - 1 do
      setStaticColor(Slots[J], FirstIndex + J);
  end;
  procedure setVisibleSlotAvatars(const Slots: TSlotArray; Count, FirstIndex: integer);
  var
    J: integer;
    PlayerIndex: integer;
    CurrentTexture: TTexture;
  begin
    for J := 0 to Count - 1 do
    begin
      PlayerIndex := FirstIndex + J + 1;
      if (PlayerIndex >= 1) and (PlayerIndex <= UIni.IMaxPlayerCount) then
      begin
        CurrentTexture := ScreenSing.Statics[Slots[J]].Texture;
        ScreenSing.Statics[Slots[J]].Texture := AvatarPlayerTextures[PlayerIndex];
        ScreenSing.Statics[Slots[J]].Texture.X := CurrentTexture.X;
        ScreenSing.Statics[Slots[J]].Texture.Y := CurrentTexture.Y;
        ScreenSing.Statics[Slots[J]].Texture.W := CurrentTexture.W;
        ScreenSing.Statics[Slots[J]].Texture.H := CurrentTexture.H;
        ScreenSing.Statics[Slots[J]].Texture.Z := CurrentTexture.Z;
        ScreenSing.Statics[Slots[J]].Texture.Alpha := CurrentTexture.Alpha;
      end;
    end;
  end;
begin
  for IterLayoutPlayerCount := 1 to UIni.IMaxPlayerCount do
    hideGroup(IterLayoutPlayerCount);

  if Ini.Screens = 1 then
  begin
    LocalPlayerCount := GetScreenPlayerCount(PlayersPlay, 2, Screen);
    FirstPlayerIndex := GetFirstPlayerIndexForScreen(PlayersPlay, 2, Screen);
  end
  else
  begin
    LocalPlayerCount := PlayersPlay;
    FirstPlayerIndex := 0;
  end;

  GetSingWidgetSlots(Self, LocalPlayerCount, FrameSlots, AvatarSlots);
  setVisibleSlotColors(FrameSlots, LocalPlayerCount, FirstPlayerIndex);
  setVisibleSlotAvatars(AvatarSlots, LocalPlayerCount, FirstPlayerIndex);
  maybeShowCount(FrameSlots, LocalPlayerCount);
  maybeShowCount(AvatarSlots, LocalPlayerCount);

end;

constructor TScreenSingView.Create;
var
  Col: array [1..UIni.IMaxPlayerCount] of TRGB;
  I: integer;
  Color: cardinal;
  procedure setColor(var avatarFrame: TThemeStatic; color: TRGB);
  begin
    avatarFrame.ColR := color.R;
    avatarFrame.ColG := color.G;
    avatarFrame.ColB := color.B;
  end;
  // passing the integer for the static by reference is deliberate
  procedure setColorAndAssignAvatarFrameStatic(var avatarFrame: TThemeStatic; var static: integer; color: TRGB);
  begin
    setColor(avatarFrame, color);
    static := ScreenSing.AddStatic(avatarFrame);
  end;
  procedure setColorAndAssignAvatarFrameStaticForSlot(const PlayerCountOnScreen, PlayerIndexOnScreen: integer; var static: integer; color: TRGB);
  var
    SingPlayer: TThemeSingPlayer;
  begin
    SingPlayer := BuildSingPlayerTemplate(PlayerCountOnScreen, PlayerIndexOnScreen);
    setColorAndAssignAvatarFrameStatic(SingPlayer.AvatarFrame, static, color);
  end;
  // passing the integers for the statics by reference is deliberate
  procedure setColorAndAssignStatics(var singPlayer: TThemeSingPlayer; var avatarFrameStatic: integer; var nameStatic: integer; color: TRGB);
  begin
    setColorAndAssignAvatarFrameStatic(singPlayer.AvatarFrame, avatarFrameStatic, color);
    nameStatic := ScreenSing.AddText(singPlayer.Name);
  end;
  procedure setColorAndAssignStaticsForSlot(const PlayerCountOnScreen, PlayerIndexOnScreen: integer; var avatarFrameStatic: integer; var nameStatic: integer; color: TRGB);
  var
    SingPlayer: TThemeSingPlayer;
  begin
    SingPlayer := BuildSingPlayerTemplate(PlayerCountOnScreen, PlayerIndexOnScreen);
    setColorAndAssignStatics(SingPlayer, avatarFrameStatic, nameStatic, color);
  end;
  // passing the integer for the static by reference is deliberate
  procedure assignAvatarStatic(var singPlayer: TThemeSingPlayer; var avatarStatic: integer; var texture: TTexture);
  begin
    avatarStatic := ScreenSing.AddStaticAlphaRectangle(singPlayer.Avatar);
    ScreenSing.Statics[avatarStatic].Texture := texture;
    ScreenSing.Statics[avatarStatic].Texture.X := singPlayer.Avatar.X;
    ScreenSing.Statics[avatarStatic].Texture.Y := singPlayer.Avatar.Y;
    ScreenSing.Statics[avatarStatic].Texture.H := singPlayer.Avatar.H;
    ScreenSing.Statics[avatarStatic].Texture.W := singPlayer.Avatar.W;
    ScreenSing.Statics[avatarStatic].Texture.Z := singPlayer.Avatar.Z;
    ScreenSing.Statics[avatarStatic].Texture.Alpha := singPlayer.Avatar.Alpha;
  end;
  procedure assignAvatarStaticForSlot(const PlayerCountOnScreen, PlayerIndexOnScreen: integer; var avatarStatic: integer; var texture: TTexture);
  var
    SingPlayer: TThemeSingPlayer;
  begin
    SingPlayer := BuildSingPlayerTemplate(PlayerCountOnScreen, PlayerIndexOnScreen);
    assignAvatarStatic(SingPlayer, avatarStatic, texture);
  end;
  procedure InitFrameSlots(MinLayoutPlayerCount, MaxLayoutPlayerCount: integer; CreateTextSlots: boolean);
  var
    LayoutPlayerCount: integer;
    SlotIndex: integer;
    ColorIndex: integer;
  begin
    for LayoutPlayerCount := MinLayoutPlayerCount to MaxLayoutPlayerCount do
    begin
      for SlotIndex := 0 to LayoutPlayerCount - 1 do
      begin
        ColorIndex := (SlotIndex mod UIni.IMaxPlayerCount) + 1;
        if CreateTextSlots then
          setColorAndAssignStaticsForSlot(LayoutPlayerCount, SlotIndex,
            PlayerFrameSlots[LayoutPlayerCount, SlotIndex],
            PlayerTextSlots[LayoutPlayerCount, SlotIndex],
            Col[ColorIndex])
        else
          setColorAndAssignAvatarFrameStaticForSlot(LayoutPlayerCount, SlotIndex,
            PlayerFrameSlots[LayoutPlayerCount, SlotIndex],
            Col[ColorIndex]);
      end;
    end;
  end;
  procedure InitAvatarSlots(MaxLayoutPlayerCount: integer);
  var
    LayoutPlayerCount: integer;
    SlotIndex: integer;
    PlayerIndex: integer;
  begin
    for LayoutPlayerCount := 1 to MaxLayoutPlayerCount do
    begin
      for SlotIndex := 0 to LayoutPlayerCount - 1 do
      begin
        PlayerIndex := (SlotIndex mod UIni.IMaxPlayerCount) + 1;
        assignAvatarStaticForSlot(LayoutPlayerCount, SlotIndex,
          PlayerAvatarSlots[LayoutPlayerCount, SlotIndex],
          AvatarPlayerTextures[PlayerIndex]);
      end;
    end;
  end;
begin
  lastVolume:= -1;
  //too dangerous, a mouse button is quickly pressed by accident
  ScreenSing.RightMbESC := false;

  ScreenSing.fShowVisualization := false;
  ScreenSing.fShowWebcam := false;
  ScreenSing.fShowBackground := false;

  ScreenSing.fCurrentVideo := nil;

  // create score class
  ScreenSing.Scores := TSingScores.Create;
  ScreenSing.Scores.LoadfromTheme;

  ScreenSing.LoadFromTheme(Theme.Sing);

  // lyrics bar
  StaticLyricsBar := ScreenSing.AddStatic(Theme.Sing.StaticLyricsBar);
  StaticLyricsBarDuet := ScreenSing.AddStatic(Theme.Sing.StaticLyricsBarDuet);

  {SetLength(StaticDuet, Length(Theme.Sing.StaticDuet));
  for i := 0 to High(StaticDuet) do
    StaticDuet[i] := ScreenSing.AddStatic(Theme.Sing.StaticDuet[i]);}

  // timebar
  StaticTimeBar := ScreenSing.AddStatic(Theme.Sing.StaticTimeBar);
  StaticTimeProgress := ScreenSing.AddStaticColorRectangle(Theme.Sing.StaticTimeProgress);
  TextTimeLabelText := ScreenSing.AddText(Theme.Sing.TextTimeLabelText);
  TextTimeText := ScreenSing.AddText(Theme.Sing.TextTimeText);

  for I := 1 to UIni.IMaxPlayerCount do
    Col[I] := GetPlayerColor(Ini.SingColor[I - 1]);

  InitFrameSlots(1, UIni.IMaxPlayerCount, true);

  for I := 1 to PlayersPlay do
  begin
    if (Party.bPartyGame) then
    begin
      ScreenSing.PlayerNames[I] := Ini.NameTeam[I-1];
    end
    else
    begin
      ScreenSing.PlayerNames[I] := Player[I-1].Name;
    end;
    ScreenSing.PlayerDuetNames[I] := ScreenSing.PlayerNames[I];
  end;

  // Sing Bars
  // P1-6
  for I := 1 to UIni.IMaxPlayerCount do
  begin
    Color := RGBFloatToInt(Col[I].R, Col[I].G, Col[I].B);

	// Color := $002222; //light blue
  // Color := $10000 * Round(0.22*255) + $100 * Round(0.39*255) + Round(0.64*255); //dark blue

    Tex_Left[I]         := Texture.LoadTexture(Skin.GetTextureFileName('GrayLeft'),  TEXTURE_TYPE_COLORIZED, Color);
    Tex_Mid[I]          := Texture.LoadTexture(Skin.GetTextureFileName('GrayMid'),   TEXTURE_TYPE_COLORIZED, Color);
    Tex_Right[I]        := Texture.LoadTexture(Skin.GetTextureFileName('GrayRight'), TEXTURE_TYPE_COLORIZED, Color);

    Tex_plain_Left[I]   := Texture.LoadTexture(Skin.GetTextureFileName('NotePlainLeft'),  TEXTURE_TYPE_COLORIZED, Color);
    Tex_plain_Mid[I]    := Texture.LoadTexture(Skin.GetTextureFileName('NotePlainMid'),   TEXTURE_TYPE_COLORIZED, Color);
    Tex_plain_Right[I]  := Texture.LoadTexture(Skin.GetTextureFileName('NotePlainRight'), TEXTURE_TYPE_COLORIZED, Color);

    Tex_BG_Left[I]      := Texture.LoadTexture(Skin.GetTextureFileName('NoteBGLeft'),  TEXTURE_TYPE_COLORIZED, Color);
    Tex_BG_Mid[I]       := Texture.LoadTexture(Skin.GetTextureFileName('NoteBGMid'),   TEXTURE_TYPE_COLORIZED, Color);
    Tex_BG_Right[I]     := Texture.LoadTexture(Skin.GetTextureFileName('NoteBGRight'), TEXTURE_TYPE_COLORIZED, Color);

    Tex_Left_Rap[I]         := Texture.LoadTexture(Skin.GetTextureFileName('GrayLeftRap'),  TEXTURE_TYPE_COLORIZED, Color);
    Tex_Mid_Rap[I]          := Texture.LoadTexture(Skin.GetTextureFileName('GrayMidRap'),   TEXTURE_TYPE_COLORIZED, Color);
    Tex_Right_Rap[I]        := Texture.LoadTexture(Skin.GetTextureFileName('GrayRightRap'), TEXTURE_TYPE_COLORIZED, Color);

    Tex_plain_Left_Rap[I]   := Texture.LoadTexture(Skin.GetTextureFileName('NotePlainLeftRap'),  TEXTURE_TYPE_COLORIZED, Color);
    Tex_plain_Mid_Rap[I]    := Texture.LoadTexture(Skin.GetTextureFileName('NotePlainMidRap'),   TEXTURE_TYPE_COLORIZED, Color);
    Tex_plain_Right_Rap[I]  := Texture.LoadTexture(Skin.GetTextureFileName('NotePlainRightRap'), TEXTURE_TYPE_COLORIZED, Color);

    Tex_BG_Left_Rap[I]      := Texture.LoadTexture(Skin.GetTextureFileName('NoteBGLeftRap'),  TEXTURE_TYPE_COLORIZED, Color);
    Tex_BG_Mid_Rap[I]       := Texture.LoadTexture(Skin.GetTextureFileName('NoteBGMidRap'),   TEXTURE_TYPE_COLORIZED, Color);
    Tex_BG_Right_Rap[I]     := Texture.LoadTexture(Skin.GetTextureFileName('NoteBGRightRap'), TEXTURE_TYPE_COLORIZED, Color);

    //## backgrounds for the scores ##
    Tex_ScoreBG[I - 1] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreBG'), TEXTURE_TYPE_COLORIZED, Color);
  end;

  // Inversions of first player colors used to mark selected note in editor
  Color := RGBFloatToInt(1 - Col[1].R, 1 - Col[1].G, 1 - Col[1].B);
  Tex_Left_Inv := Texture.LoadTexture(Skin.GetTextureFileName('GrayLeft'), TEXTURE_TYPE_COLORIZED, Color);
  Tex_Mid_Inv := Texture.LoadTexture(Skin.GetTextureFileName('GrayMid'), TEXTURE_TYPE_COLORIZED, Color);
  Tex_Right_Inv := Texture.LoadTexture(Skin.GetTextureFileName('GrayRight'), TEXTURE_TYPE_COLORIZED, Color);
  Tex_Left_Rap_Inv := Texture.LoadTexture(Skin.GetTextureFileName('GrayLeftRap'), TEXTURE_TYPE_COLORIZED, Color);
  Tex_Mid_Rap_Inv := Texture.LoadTexture(Skin.GetTextureFileName('GrayMidRap'), TEXTURE_TYPE_COLORIZED, Color);
  Tex_Right_Rap_Inv := Texture.LoadTexture(Skin.GetTextureFileName('GrayRightRap'), TEXTURE_TYPE_COLORIZED, Color);

  StaticPausePopup := ScreenSing.AddStatic(Theme.Sing.PausePopUp);

  // <note> pausepopup is not visible at the beginning </note>
  ScreenSing.Statics[StaticPausePopup].Visible := false;

  ScreenSing.Lyrics := TLyricEngine.Create(
      Theme.LyricBar.UpperX, Theme.LyricBar.UpperY, Theme.LyricBar.UpperW, Theme.LyricBar.UpperH,
      Theme.LyricBar.LowerX, Theme.LyricBar.LowerY, Theme.LyricBar.LowerW, Theme.LyricBar.LowerH);

  ScreenSing.LyricsDuetP1 := TLyricEngine.Create(
      Theme.LyricBarDuetP1.UpperX, Theme.LyricBarDuetP1.UpperY, Theme.LyricBarDuetP1.UpperW, Theme.LyricBarDuetP1.UpperH,
      Theme.LyricBarDuetP1.LowerX, Theme.LyricBarDuetP1.LowerY, Theme.LyricBarDuetP1.LowerW, Theme.LyricBarDuetP1.LowerH);

  ScreenSing.LyricsDuetP2 := TLyricEngine.Create(
      Theme.LyricBarDuetP2.UpperX, Theme.LyricBarDuetP2.UpperY, Theme.LyricBarDuetP2.UpperW, Theme.LyricBarDuetP2.UpperH,
      Theme.LyricBarDuetP2.LowerX, Theme.LyricBarDuetP2.LowerY, Theme.LyricBarDuetP2.LowerW, Theme.LyricBarDuetP2.LowerH);

  ScreenSing.fLyricsSync := TLyricsSyncSource.Create();
  ScreenSing.fMusicSync := TMusicSyncSource.Create();

  SongNameStatic := ScreenSing.AddStatic(Theme.Sing.StaticSongName);;
  SongNameText := ScreenSing.AddText(Theme.Sing.TextSongName);

  ScreenSing.eSongLoaded := THookableEvent.Create('ScreenSing.SongLoaded');

  // Info Message
  ScreenSing.InfoMessageBG := ScreenSing.AddStatic(Theme.Sing.InfoMessageBG);
  ScreenSing.InfoMessageText := ScreenSing.AddText(Theme.Sing.InfoMessageText);

  InitAvatarSlots(UIni.IMaxPlayerCount);
end;

destructor TScreenSingView.Destroy;
var
  I: integer;
begin
  for I := 1 to UIni.IMaxPlayerCount do
  begin
    FreeTexture(Tex_Left[I]);
    FreeTexture(Tex_Mid[I]);
    FreeTexture(Tex_Right[I]);
    FreeTexture(Tex_plain_Left[I]);
    FreeTexture(Tex_plain_Mid[I]);
    FreeTexture(Tex_plain_Right[I]);
    FreeTexture(Tex_BG_Left[I]);
    FreeTexture(Tex_BG_Mid[I]);
    FreeTexture(Tex_BG_Right[I]);
    FreeTexture(Tex_Left_Rap[I]);
    FreeTexture(Tex_Mid_Rap[I]);
    FreeTexture(Tex_Right_Rap[I]);
    FreeTexture(Tex_plain_Left_Rap[I]);
    FreeTexture(Tex_plain_Mid_Rap[I]);
    FreeTexture(Tex_plain_Right_Rap[I]);
    FreeTexture(Tex_BG_Left_Rap[I]);
    FreeTexture(Tex_BG_Mid_Rap[I]);
    FreeTexture(Tex_BG_Right_Rap[I]);
    FreeTexture(Tex_ScoreBG[I - 1]);
  end;
  FreeTexture(Tex_Left_Inv);
  FreeTexture(Tex_Mid_Inv);
  FreeTexture(Tex_Right_Inv);
  FreeTexture(Tex_Left_Rap_Inv);
  FreeTexture(Tex_Mid_Rap_Inv);
  FreeTexture(Tex_Right_Rap_Inv);
  inherited;
end;

function TScreenSingView.Draw: boolean;
var
  DisplayTime:            real;
  DisplayPrefix:          string;
  DisplayMin:             integer;
  DisplaySec:             integer;
  T:                      integer;
  CurLyricsTime:          real;
  VideoFrameTime:         Extended;
  Line:                   TLyricLine;
  LastWord:               TLyricWord;
  LineDuet:               TLyricLine;
  LastWordDuet:           TLyricWord;
  medley_end:             boolean;
  medley_start_applause:  boolean;
  LastLineSungToEnd:      boolean;
  ScoreMode:              TSingInfoBarMode;
  LocalPlayerCount:       integer;
  LocalStartIndex:        integer;
  TextSlots:              TSlotArray;
  IterLayoutPlayerCount:  integer;
  SlotIndex:              integer;
  SingPlayer:             TThemeSingPlayer;
  procedure SetPlayerNameTexts(const Slots: TSlotArray; FirstPlayerIndex: integer; UseDuetNames: boolean);
  var
    J: integer;
    PlayerIndex: integer;
  begin
    for J := 0 to High(Slots) do
    begin
      PlayerIndex := FirstPlayerIndex + J;
      if (PlayerIndex >= 1) and (PlayerIndex <= PlayersPlay) then
      begin
        if UseDuetNames then
          ScreenSing.Text[Slots[J]].Text := ScreenSing.PlayerDuetNames[PlayerIndex]
        else
          ScreenSing.Text[Slots[J]].Text := ScreenSing.PlayerNames[PlayerIndex];
      end
      else
      begin
        ScreenSing.Text[Slots[J]].Text := '';
      end;
    end;
  end;
  procedure SetTextVisibility(const Slots: TSlotArray; VisibleCount: integer; Visible: boolean);
  var
    J: integer;
  begin
    for J := 0 to High(Slots) do
      ScreenSing.Text[Slots[J]].Visible := Visible and (J < VisibleCount) and ScreenSing.Settings.AvatarsVisible;
  end;
  procedure SetLyricsDuetColors(FirstPlayerIndex: integer);
  var
    PlayerColor: TRGB;
  begin
    PlayerColor := GetSingPlayerColor(FirstPlayerIndex - 1);
    ScreenSing.LyricsDuetP1.LineColor_act.R := PlayerColor.R;
    ScreenSing.LyricsDuetP1.LineColor_act.G := PlayerColor.G;
    ScreenSing.LyricsDuetP1.LineColor_act.B := PlayerColor.B;

    PlayerColor := GetSingPlayerColor(FirstPlayerIndex);
    ScreenSing.LyricsDuetP2.LineColor_act.R := PlayerColor.R;
    ScreenSing.LyricsDuetP2.LineColor_act.G := PlayerColor.G;
    ScreenSing.LyricsDuetP2.LineColor_act.B := PlayerColor.B;
  end;
begin
  ScreenSing.Background.Draw;

  // sound enabled/disabled (Party plugins)
  if not(ScreenSing.Settings.SoundEnabled) and not(lastVolume=0) then
  begin
    AudioPlayback.SetVolume(0);
    lastVolume:=0;
  end
  else if not(lastVolume=1) and not(lastVolume=0) then
  begin //changing volume is slow, so do not reset volume every frame
    AudioPlayback.SetVolume(1);
    lastVolume:=1;
  end;

  // swap static textures to current screen ones
  SwapToScreen(ScreenAct);

  // draw background picture (if any, and if no visualizations)
  // when we don't check for visualizations the visualizations would
  // be overdrawn by the picture when {UNDEFINED UseTexture} in UVisualizer
  //if (not fShowVisualization) then
  if (not ScreenSing.fShowVisualization) or (ScreenSing.fShowBackground) then
    SingDrawBackground;

  if (ScreenSing.fShowWebCam) then
    SingDrawWebCamFrame;

  LocalPlayerCount := GetScreenPlayerCount(PlayersPlay, Screens, ScreenAct);
  LocalStartIndex := GetFirstPlayerIndexForScreen(PlayersPlay, Screens, ScreenAct) + 1;

  for IterLayoutPlayerCount := 1 to UIni.IMaxPlayerCount do
  begin
    GetSingTextSlots(Self, IterLayoutPlayerCount, TextSlots);
    SetTextVisibility(TextSlots, 0, false);
  end;

  GetSingTextSlots(Self, LocalPlayerCount, TextSlots);
  for SlotIndex := 0 to LocalPlayerCount - 1 do
  begin
    SingPlayer := BuildSingPlayerTemplate(LocalPlayerCount, SlotIndex);
    ApplySingPlayerTemplate(
      SingPlayer,
      PlayerFrameSlots[LocalPlayerCount, SlotIndex],
      PlayerAvatarSlots[LocalPlayerCount, SlotIndex],
      PlayerTextSlots[LocalPlayerCount, SlotIndex]
    );
  end;
  SetPlayerNameTexts(TextSlots, LocalStartIndex, CurrentSong.isDuet and (LocalPlayerCount >= 3));
  SetTextVisibility(TextSlots, LocalPlayerCount, true);

  if CurrentSong.isDuet and (LocalPlayerCount = 2) then
    SetLyricsDuetColors(LocalStartIndex);

  // retrieve current lyrics time, we have to store the value to avoid
  // that min- and sec-values do not match
  if ScreenSong.Mode = smMedley then
  begin
    CurLyricsTime := LyricsState.GetCurrentTime() - ScreenSing.MedleyStart;
    TotalTime := ScreenSing.MedleyEnd - ScreenSing.MedleyStart;
  end
  else
  begin
    CurLyricsTime := LyricsState.GetCurrentTime();
    TotalTime :=  LyricsState.TotalTime;
  end;

  // retrieve time for timebar text
  case (ScreenSing.fTimebarMode) of
    tbmRemaining: begin
      DisplayTime := TotalTime - CurLyricsTime;
      DisplayPrefix := '-';
    end;
    tbmTotal: begin
      DisplayTime := TotalTime;
      DisplayPrefix := '#';
    end;
    else begin       // current time
      DisplayTime := CurLyricsTime;
      DisplayPrefix := '';
    end;
  end;
  DisplayMin := Round(DisplayTime) div 60;
  DisplaySec := Round(DisplayTime) mod 60;

  // update static menu with time ...
  ScreenSing.Text[TextTimeLabelText].Visible := ScreenSing.Settings.TimeBarVisible;
  ScreenSing.Text[TextTimeText].Text := Format('%s%.2d:%.2d', [DisplayPrefix, DisplayMin, DisplaySec]);
  ScreenSing.Text[TextTimeText].Visible := ScreenSing.Settings.TimeBarVisible;

  LastLineSungToEnd := false;
  //the song was sung to the end?
  if not (ScreenSing.SungToEnd) and not(ScreenSong.RapToFreestyle) then
  begin
    Line := ScreenSing.Lyrics.GetUpperLine();
    if Line.LastLine then
    begin
      LastLineSungToEnd := true;
      for T := 0 to High(Line.Words) do
      begin
        if (CurLyricsTime < GetTimeFromBeat(Line.Words[T].Start + Line.Words[T].Length)) and
           (not Line.Words[T].Freestyle) then
          LastLineSungToEnd := false;
      end;
    end;
    if LastLineSungToEnd then
      ScreenSing.SungToEnd := true;
  end;

  if not(ScreenSing.SungToEnd) and CurrentSong.isDuet and not(ScreenSong.RapToFreestyle) then
  begin
    Line := ScreenSing.LyricsDuetP1.GetUpperLine();
    if Line.LastLine then
    begin
      LastWord := Line.Words[Length(Line.Words)-1];
      if CurLyricsTime >= GetTimeFromBeat(LastWord.Start + LastWord.Length) then
        ScreenSing.SungToEnd := true;
    end;
    Line := ScreenSing.LyricsDuetP2.GetUpperLine();
    if Line.LastLine then
    begin
      LastWord := Line.Words[Length(Line.Words)-1];
      if CurLyricsTime >= GetTimeFromBeat(LastWord.Start + LastWord.Length) then
        ScreenSing.SungToEnd := true;
    end;
  end;

  // for medley-mode:
  CurLyricsTime := LyricsState.GetCurrentTime();
  if (ScreenSong.Mode = smMedley) and (CurLyricsTime > ScreenSing.MedleyEnd) then
    medley_end := true
  else
    medley_end := false;

  if (ScreenSong.Mode = smMedley) and (CurLyricsTime >
    GetTimeFromBeat(CurrentSong.Medley.EndBeat)) then
    medley_start_applause := true
  else
    medley_start_applause := false;

  // update and draw movie
  // USE FFMPEG
  if Assigned(ScreenSing.fCurrentVideo) and (not ScreenSing.fShowWebcam) then
  begin
    // Just call this once
    // when Screens = 2
    if (ScreenAct = 1) then
    begin
      if (ScreenSing.ShowFinish) then
      begin
        // everything is setup, determine the current position
        VideoFrameTime := CurrentSong.VideoGAP + LyricsState.GetCurrentTime();
      end
      else
      begin
        // Important: do not yet start the triggered timer by a call to
        // LyricsState.GetCurrentTime()
        VideoFrameTime := CurrentSong.VideoGAP;
      end;
      try
        ScreenSing.fCurrentVideo.GetFrame(VideoFrameTime);
      except
      end;
    end;

    ScreenSing.fCurrentVideo.SetScreen(ScreenAct);
    ScreenSing.fCurrentVideo.Draw;
  end;

  // draw static menu (FG)
  ScreenSing.DrawFG;

  //Medley Countdown
  if ScreenSong.Mode = smMedley then
    DrawMedleyCountdown;

  // check for music finish
  //Log.LogError('Check for music finish: ' + BoolToStr(Music.Finished) + ' ' + FloatToStr(LyricsState.CurrentTime*1000) + ' ' + IntToStr(CurrentSong.Finish));
  if ScreenSing.ShowFinish then
  begin
    if (not ScreenSing.FinishedMusic) and (not medley_end or (ScreenSong.Mode <> smMedley)) and
       ((CurrentSong.Finish = 0) or
        (LyricsState.GetCurrentTime() * 1000 <= CurrentSong.Finish)) and
       (not ScreenSing.Settings.Finish) then
    begin
      // analyze song if not paused
      if (not ScreenSing.Paused) then
      begin
        Sing(ScreenSing);

        //Update Medley Stats
        if (ScreenSong.Mode = smMedley) and not ScreenSing.FadeOut then
          ScreenSing.UpdateMedleyStats(medley_start_applause);

        Party.CallOnSing;
      end;
    end
    else
    begin
      if (not ScreenSing.FadeOut) and (Screens=1) or (ScreenAct=2) then
      begin
        ScreenSing.Finish;
      end;
    end;
  end;

  // draw info lyric bar if not medley
  if (ScreenSing.Settings.TimeBarVisible) then
    DrawInfoLyricBar;

  // draw scores
  if (ScreenSing.Settings.ScoresVisible) and ((Ini.SingScores = 1) or (Party.bPartyGame)) then
    ScreenSing.Scores.Draw;

  // always draw custom items
  ScreenSing.Statics[StaticLyricsBar].Visible := ScreenSing.Settings.LyricsVisible;
  ScreenSing.Statics[StaticLyricsBarDuet].Visible := ScreenSing.Settings.LyricsVisible and (CurrentSong.isDuet) and (PlayersPlay <> 1);
  ScreenSing.Statics[StaticTimeBar].Visible := ScreenSing.Settings.TimeBarVisible;
  SingDraw;

  // goldennotestarstwinkle
  GoldenRec.SpawnRec;

  // draw scores / info bars
  ScoreMode := TSingInfoBarMode(EnsureRange(Ini.SingScores, 0, Ord(High(TSingInfoBarMode))));
  if Party.bPartyGame and (ScoreMode = sibOff) then
    ScoreMode := sibRatingOnly;

  if (ScreenSing.Settings.ScoresVisible) and (ScoreMode <> sibOff) then
  begin
    ScreenSing.Scores.SetInfoBarMode(ScoreMode);
    ScreenSing.Scores.Draw;
  end;

  FadeMessage();

  // draw pausepopup
  // FIXME: this is a workaround that the static is drawn over the lyrics, lines, scores and effects
  // maybe someone could find a better solution
  if ScreenSing.Paused then
  begin
    ScreenSing.Statics[StaticPausePopup].Texture.Z := 1;
    ScreenSing.Statics[StaticPausePopup].Visible := true;
    ScreenSing.Statics[StaticPausePopup].Draw;
    ScreenSing.Statics[StaticPausePopup].Visible := false;
    SDL_Delay(33);//wait a bit to save electricity...
  end;

  Result := true;
end;

procedure TScreenSingView.DrawMedleyCountdown();
var
  w, h:           real;
  timeDiff:       real;
  t:              real;
  CountDownText:  UTF8String;
begin
  if AudioPlayback.Position < GetTimeFromBeat(CurrentSong.Medley.StartBeat) then
  begin
    ScreenSing.TextMedleyFadeOut := false;

    ScreenSing.Statics[SongNameStatic].Texture.Alpha := 1;
    ScreenSing.Text[SongNameText].Alpha := 1;

    ScreenSing.Statics[SongNameStatic].Visible := true;
    ScreenSing.Text[SongNameText].Visible := true;

    timeDiff := GetTimeFromBeat(CurrentSong.Medley.StartBeat) - AudioPlayback.Position + 1;
    t := frac(timeDiff);

    glColor4f(0.15, 0.30, 0.6, t);

    h := 300*t*ScreenH/RenderH;
    SetFontFamily(0);
    SetFontStyle(ftBoldHighRes);
    SetFontItalic(false);
    SetFontSize(h);
    CountDownText := IntToStr(round(timeDiff-t));
    w := glTextWidth(PChar(CountDownText));

    SetFontPos (RenderW/2-w/2, RenderH/2-h/2);
    glPrint(PChar(CountDownText));
  end else
  begin
    if (ScreenSing.TextMedleyFadeOut = false) then
    begin
      ScreenSing.TextMedleyFadeOut := true;
      ScreenSing.TextMedleyFadeTime := SDL_GetTicks();
    end;

    MedleyTitleFadeOut;
  end;
end;

procedure TScreenSingView.WriteMessage(msg: UTF8String);
begin
  ScreenSing.MessageTime := SDL_GetTicks();

  ScreenSing.Statics[ScreenSing.InfoMessageBG].Texture.Alpha := 1;
  ScreenSing.Text[ScreenSing.InfoMessageText].Alpha := 1;

  ScreenSing.Statics[ScreenSing.InfoMessageBG].Visible := true;
  ScreenSing.Text[ScreenSing.InfoMessageText].Visible := true;
  ScreenSing.Text[ScreenSing.InfoMessageText].Text := msg;
end;

procedure TScreenSingView.FadeMessage();
var
  factor: real;
begin
  if ((SDL_GetTicks - ScreenSing.MessageTime)/1000 > MAX_MESSAGE) then
  begin
    if (ScreenSing.MessageTimeFade = 0) then
      ScreenSing.MessageTimeFade := SDL_GetTicks();

    factor := (SDL_GetTicks - ScreenSing.MessageTimeFade)/1000/2;

    ScreenSing.Statics[ScreenSing.InfoMessageBG].Texture.Alpha := 1 - factor;
    ScreenSing.Text[ScreenSing.InfoMessageText].Alpha := 1 - factor;
  end
  else
    ScreenSing.MessageTimeFade := 0;

  ScreenSing.Statics[ScreenSing.InfoMessageBG].Draw;
  ScreenSing.Text[ScreenSing.InfoMessageText].Draw;
end;

procedure TScreenSingView.CloseMessage();
begin
  ScreenSing.Statics[ScreenSing.InfoMessageBG].Visible := false;
  ScreenSing.Text[ScreenSing.InfoMessageText].Visible := false;
end;

procedure TScreenSingView.MedleyTitleFadeOut();
var
  Alpha: real;
  CTime: cardinal;
begin

  CTime := SDL_GetTicks() - ScreenSing.TextMedleyFadeTime;
  Alpha := CTime/3000;

  if (Alpha >= 1) then
  begin
    ScreenSing.Statics[SongNameStatic].Visible := false;
    ScreenSing.Text[SongNameText].Visible := false;
  end
  else
  begin
    ScreenSing.Text[SongNameText].Alpha := 1 - Alpha;
    ScreenSing.Statics[SongNameStatic].Texture.Alpha := 1 - Alpha;
  end;
end;

function TScreenSingView.GetLyricColor(Color: integer): TRGB;
begin
  case (Color) of
    1://blue
    begin
      Result.R := 5/255;
      Result.G := 153/255;
      Result.B := 204/255;
    end;
    2: //red
    begin
      Result.R := 230/255;
      Result.G := 0;
      Result.B := 0;
    end;
    3: //green
    begin
      Result.R := 0;
      Result.G := 170/255;
      Result.B := 0;
    end;
    4: //yellow
    begin
      Result.R := 255/255;
      Result.G := 225/255;
      Result.B := 0;
    end;
    5: //orange
    begin
      Result.R := 227/255;
      Result.G := 127/255;
      Result.B := 0;
    end;
    6: //pink
    begin
      Result.R := 255/255;
      Result.G := 0/255;
      Result.B := 130/255;
    end;
    7: //purple
    begin
      Result.R := 180/255;
      Result.G := 0;
      Result.B := 220/255;
    end;
    8: //gold
    begin
      Result.R := 255/255;
      Result.G := 190/255;
      Result.B := 35/255;
    end;
    9: //gray
    begin
      Result.R := 80/255;
      Result.G := 80/255;
      Result.B := 80/255;
    end;
    10: //dark blue
    begin
      Result.R := 90/255;
      Result.G := 90/255;
      Result.B := 240/255;
    end;
    11: //sky
    begin
      Result.R := 0;
      Result.G := 110/255;
      Result.B := 210/255;
    end;
    12: //cyan
    begin
      Result.R := 0/255;
      Result.G := 215/255;
      Result.B := 215/255;
    end;
    13: //flame
    begin
      Result.R := 210/255;
      Result.G := 70/255;
      Result.B := 0/255;
    end;
    14: //orchid
    begin
      Result.R := 210/255;
      Result.G := 0;
      Result.B := 210/255;
    end;
    15: //harlequin
    begin
      Result.R := 110/255;
      Result.G := 210/255;
      Result.B := 0;
    end;
    16: //lime
    begin
      Result.R := 160/255;
      Result.G := 210/255;
      Result.B := 0;
    end;
    else//blue
    begin
      Result.R := 5/255;
      Result.G := 153/255;
      Result.B := 204/255;
    end;
  end;
end;

procedure TScreenSingView.DrawInfoLyricBar();
var
  SongStart:       real;
  SongEnd:         real;
  SongDuration:    real;
  gapInBeats:      real;

  pos:             real;
  br:              real;

  LineIndex:       integer;
  numLines:        integer;

  x, y, w, h:      real;
  CurrentTrack:    integer;
begin
  x := Theme.Sing.StaticTimeProgress.x;
  y := Theme.Sing.StaticTimeProgress.y;

  w := Theme.Sing.StaticTimeProgress.w;
  h := Theme.Sing.StaticTimeProgress.h;

  //calculate total singing beats of song
  if ScreenSong.Mode = smMedley then
  begin
    SongStart := ScreenSing.MedleyStart * CurrentSong.BPM[0].BPM / 60;
    SongEnd := ScreenSing.MedleyEnd * CurrentSong.BPM[0].BPM / 60;
  end
  else
  begin
    SongStart := CurrentSong.BPM[0].BPM*CurrentSong.Start/60;
    SongEnd := CurrentSong.BPM[0].BPM*TotalTime/60;
  end;
  SongDuration := SongEnd - SongStart;
  gapInBeats := CurrentSong.BPM[0].BPM*CurrentSong.GAP/1000/60;
  // draw sentence boxes
  for CurrentTrack := 0 to High(CurrentSong.Tracks) do //for P1 of duet or solo lyrics, P2 of duet,..
  begin
    numLines := Length(CurrentSong.Tracks[CurrentTrack].Lines); //Lyric lines
    //set color to player.color
    if (CurrentTrack = 0) then
      glColor4f(GetLyricColor(Ini.SingColor[0]).R, GetLyricColor(Ini.SingColor[0]).G, GetLyricColor(Ini.SingColor[0]).B, 0.6)
    else
      glColor4f(GetLyricColor(Ini.SingColor[CurrentTrack]).R, GetLyricColor(Ini.SingColor[CurrentTrack]).G, GetLyricColor(Ini.SingColor[CurrentTrack]).B, 0.6);

    glbegin(gl_quads);
    for LineIndex := 0 to numLines - 1 do
    begin
      if (CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes = nil) then Continue;
      if (ScreenSong.Mode = smMedley) and (CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[0].StartBeat < CurrentSong.Medley.StartBeat) then Continue;
      pos := (gapInBeats + CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[0].StartBeat - SongStart) / SongDuration*w;
      br := (CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote].StartBeat +
                CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote].Duration -
                CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[0].StartBeat) / SongDuration*w;  //br = last note of sentence position + its duration - first note of sentence position

      //draw a square
      glVertex2f(x+pos, y); //left top
      glVertex2f(x+pos, y+h); //left bottom
      glVertex2f(x+pos+br, y+h); //right bottom
      glVertex2f(x+pos+br, y); //right top
    end;
    glEnd;
  end;

  // draw progress indicator
  br := (gapInBeats + LyricsState.CurrentBeat - SongStart) / SongDuration*w;
  glColor4f(Theme.Sing.StaticTimeProgress.ColR,
             Theme.Sing.StaticTimeProgress.ColG,
             Theme.Sing.StaticTimeProgress.ColB, 1); //Set Color
  glBegin(GL_QUADS);
  glVertex2f(x, y); // left top
  glVertex2f(x, y+h); // left bottom
  glVertex2f(x+br, y+h); // right bottom
  glVertex2f(x+br, y); // right top
  glEnd;
end;

end.
