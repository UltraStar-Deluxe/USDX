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
    ColPlayer:  array[0..3] of TRGB;

    // lyrics bar fields
    StaticLyricsBar: integer;
    StaticLyricsBarDuet: integer;

    // timebar fields
    StaticTimeBar: integer;
    StaticTimeProgress: integer;
    TextTimeLabelText: integer;
    TextTimeText: integer;

    StaticP1: array [0..1] of integer;
    TextP1:   integer;
    StaticP1Avatar: array [0..1] of integer;

    // shown when game is in 2/4 player modus
    StaticP1TwoP: array [0..1] of integer;
    TextP1TwoP:   integer;
    StaticP1TwoPAvatar: array [0..1] of integer;

    // shown when game is in 3/6 player modus
    StaticP1ThreeP: array [0..1] of integer;
    TextP1ThreeP:   integer;
    StaticP1ThreePAvatar: array [0..1] of integer;

    StaticP2R: array [0..1] of integer;
    TextP2R:   integer;
    StaticP2RAvatar: array [0..1] of integer;

    StaticP2M: array [0..1] of integer;
    TextP2M:   integer;
    StaticP2MAvatar: array [0..1] of integer;

    StaticP3R: array [0..1] of integer;
    TextP3R:   integer;
    StaticP3RAvatar: array [0..1] of integer;

    // 4/6 players in one screen
    StaticP1FourP:  integer;
    StaticP2FourP:  integer;
    StaticP3FourP:  integer;
    StaticP4FourP:  integer;

    StaticP1FourPAvatar:  integer;
    StaticP2FourPAvatar:  integer;
    StaticP3FourPAvatar:  integer;
    StaticP4FourPAvatar:  integer;

    TextP1FourP:   integer;
    TextP2FourP:   integer;
    TextP3FourP:   integer;
    TextP4FourP:   integer;

    StaticP1SixP:  integer;
    StaticP2SixP:  integer;
    StaticP3SixP:  integer;
    StaticP4SixP:  integer;
    StaticP5SixP:  integer;
    StaticP6SixP:  integer;

    StaticP1SixPAvatar:  integer;
    StaticP2SixPAvatar:  integer;
    StaticP3SixPAvatar:  integer;
    StaticP4SixPAvatar:  integer;
    StaticP5SixPAvatar:  integer;
    StaticP6SixPAvatar:  integer;

    TextP1SixP:   integer;
    TextP2SixP:   integer;
    TextP3SixP:   integer;
    TextP4SixP:   integer;
    TextP5SixP:   integer;
    TextP6SixP:   integer;

    // 3/6 players duet
    StaticDuetP1ThreeP: array [0..1] of integer;
    TextDuetP1ThreeP:   integer;
    StaticDuetP1ThreePAvatar: array [0..1] of integer;

    StaticDuetP2M: array [0..1] of integer;
    TextDuetP2M:   integer;
    StaticDuetP2MAvatar: array [0..1] of integer;

    StaticDuetP3R: array [0..1] of integer;
    TextDuetP3R:   integer;
    StaticDuetP3RAvatar: array [0..1] of integer;

    // 4/6 players duet one screen
    StaticP1DuetFourP:  integer;
    StaticP2DuetFourP:  integer;
    StaticP3DuetFourP:  integer;
    StaticP4DuetFourP:  integer;

    StaticP1DuetFourPAvatar:  integer;
    StaticP2DuetFourPAvatar:  integer;
    StaticP3DuetFourPAvatar:  integer;
    StaticP4DuetFourPAvatar:  integer;

    TextP1DuetFourP:   integer;
    TextP2DuetFourP:   integer;
    TextP3DuetFourP:   integer;
    TextP4DuetFourP:   integer;

    StaticP1DuetSixP:  integer;
    StaticP2DuetSixP:  integer;
    StaticP3DuetSixP:  integer;
    StaticP4DuetSixP:  integer;
    StaticP5DuetSixP:  integer;
    StaticP6DuetSixP:  integer;

    StaticP1DuetSixPAvatar:  integer;
    StaticP2DuetSixPAvatar:  integer;
    StaticP3DuetSixPAvatar:  integer;
    StaticP4DuetSixPAvatar:  integer;
    StaticP5DuetSixPAvatar:  integer;
    StaticP6DuetSixPAvatar:  integer;

    TextP1DuetSixP:   integer;
    TextP2DuetSixP:   integer;
    TextP3DuetSixP:   integer;
    TextP4DuetSixP:   integer;
    TextP5DuetSixP:   integer;
    TextP6DuetSixP:   integer;


    StaticPausePopup: integer;

    SongNameStatic:   integer;
    SongNameText:     integer;



    constructor Create;

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

//ToDo basisbit: check this again
// Dirty HacK
procedure TScreenSingView.SwapToScreen(Screen: integer);
var
  P, I: integer;
  procedure setVisible(elements: array of integer; visible: boolean);
  var
    J: integer;
  begin
    for J := 0 to High(elements) do
      ScreenSing.Statics[elements[J]].Visible := visible;
  end;
  procedure hide(elements: array of integer);
  begin
    setVisible(elements, false);
  end;
  procedure maybeShow(elements: array of integer);
  begin
    setVisible(elements, ScreenSing.Settings.AvatarsVisible);
  end;
begin
  { if screens = 2 and playerplay <= 3 the 2nd screen shows the
    textures of screen 1 }
  if (PlayersPlay <= 3) and (Screen = 2) then
    Screen := 1;

  hide([
    // 1P - screen 1
    StaticP1[0], StaticP1Avatar[0],
    // 2P - screen 1
    StaticP1TwoP[0], StaticP1TwoPAvatar[0],
    StaticP2R[0], StaticP2RAvatar[0],
    // 3P - screen 1
    StaticP1ThreeP[0], StaticP1ThreePAvatar[0],
    StaticP2M[0], StaticP2MAvatar[0],
    StaticP3R[0], StaticP3RAvatar[0],
    // 1P - screen 2 (2 players across 2 screens)
    StaticP1[1], StaticP1Avatar[1],
    // 2P - screen 2 (4 players across 2 screens)
    StaticP1TwoP[1], StaticP1TwoPAvatar[1],
    StaticP2R[1], StaticP2RAvatar[1],
    // 3P - screen 2 (6 players across 2 screens)
    StaticP1ThreeP[1], StaticP1ThreePAvatar[1],
    StaticP2M[1], StaticP2MAvatar[1],
    StaticP3R[1], StaticP3RAvatar[1],
    // 3P duet - screen 1
    StaticDuetP1ThreeP[0], StaticDuetP1ThreePAvatar[0],
    StaticDuetP2M[0], StaticDuetP2MAvatar[0],
    StaticDuetP3R[0], StaticDuetP3RAvatar[0],
    // 3P duet - screen 2 (6 players across 2 screens)
    StaticDuetP1ThreeP[1], StaticDuetP1ThreePAvatar[1],
    StaticDuetP2M[1], StaticDuetP2MAvatar[1],
    StaticDuetP3R[1], StaticDuetP3RAvatar[1],
    // 4P - screen 1
    StaticP1FourP, StaticP1FourPAvatar,
    StaticP2FourP, StaticP2FourPAvatar,
    StaticP3FourP, StaticP3FourPAvatar,
    StaticP4FourP, StaticP4FourPAvatar,
    // 4P duet - screen 1
    StaticP1DuetFourP, StaticP1DuetFourPAvatar,
    StaticP2DuetFourP, StaticP2DuetFourPAvatar,
    StaticP3DuetFourP, StaticP3DuetFourPAvatar,
    StaticP4DuetFourP, StaticP4DuetFourPAvatar,
    // 6P - screen 1
    StaticP1SixP, StaticP1SixPAvatar,
    StaticP2SixP, StaticP2SixPAvatar,
    StaticP3SixP, StaticP3SixPAvatar,
    StaticP4SixP, StaticP4SixPAvatar,
    StaticP5SixP, StaticP5SixPAvatar,
    StaticP6SixP, StaticP6SixPAvatar,
    // 6P duet - screen 1
    StaticP1DuetSixP, StaticP1DuetSixPAvatar,
    StaticP2DuetSixP, StaticP2DuetSixPAvatar,
    StaticP3DuetSixP, StaticP3DuetSixPAvatar,
    StaticP4DuetSixP, StaticP4DuetSixPAvatar,
    StaticP5DuetSixP, StaticP5DuetSixPAvatar,
    StaticP6DuetSixP, StaticP6DuetSixPAvatar
  ]);

  if (PlayersPlay = 1) then
  begin
    // this is actually correct! it just puts the sole player on all screens
    maybeShow([StaticP1[0], StaticP1Avatar[0]]);
  end;

  if (PlayersPlay = 2) or ((PlayersPlay = 4) and (Ini.Screens = 1)) then
  begin
    if (Screen = 2) then
    begin
      maybeShow([
        StaticP1TwoP[1], StaticP1TwoPAvatar[1],
        StaticP2R[1], StaticP2RAvatar[1]
      ]);
    end;

    if (Screen = 1) then
    begin
      maybeShow([
        StaticP1TwoP[0], StaticP1TwoPAvatar[0],
        StaticP2R[0], StaticP2RAvatar[0]
      ]);
    end;
  end;

  if (PlayersPlay = 3) or ((PlayersPlay = 6) and (Ini.Screens = 1)) then
  begin
    if (CurrentSong.isDuet) then
    begin
      if (Screen = 2) then
      begin
        maybeShow([
          StaticDuetP1ThreeP[1], StaticDuetP1ThreePAvatar[1],
          StaticDuetP2M[1], StaticDuetP2MAvatar[1],
          StaticDuetP3R[1], StaticDuetP3RAvatar[1]
        ]);
      end;

      if (Screen = 1) then
      begin
        maybeShow([
          StaticDuetP1ThreeP[0], StaticDuetP1ThreePAvatar[0],
          StaticDuetP2M[0], StaticDuetP2MAvatar[0],
          StaticDuetP3R[0], StaticDuetP3RAvatar[0]
        ]);
      end;
    end
    else
    begin
      if (Screen = 2) then
      begin
        maybeShow([
          StaticP1ThreeP[1], StaticP1ThreePAvatar[1],
          StaticP2M[1], StaticP2MAvatar[1],
          StaticP3R[1], StaticP3RAvatar[1]
        ]);
      end;

      if (Screen = 1) then
      begin
        maybeShow([
          StaticP1ThreeP[0], StaticP1ThreePAvatar[0],
          StaticP2M[0], StaticP2MAvatar[0],
          StaticP3R[0], StaticP3RAvatar[0]
        ]);
      end;
    end;
  end;

  // 4 Players in 1 Screen
  if (PlayersPlay = 4) and (Ini.Screens = 0) then
  begin
    if (CurrentSong.isDuet) then
    begin
      maybeShow([
        StaticP1DuetFourP, StaticP1DuetFourPAvatar,
        StaticP2DuetFourP, StaticP2DuetFourPAvatar,
        StaticP3DuetFourP, StaticP3DuetFourPAvatar,
        StaticP4DuetFourP, StaticP4DuetFourPAvatar
      ]);
    end
    else
    begin
      maybeShow([
        StaticP1FourP, StaticP1FourPAvatar,
        StaticP2FourP, StaticP2FourPAvatar,
        StaticP3FourP, StaticP3FourPAvatar,
        StaticP4FourP, StaticP4FourPAvatar
      ]);
    end;
  end;

  // 6 Players in 1 Screen
  if (PlayersPlay = 6) and (Ini.Screens = 0) then
  begin
    if (CurrentSong.isDuet) then
    begin
      maybeShow([
        StaticP1DuetSixP, StaticP1DuetSixPAvatar,
        StaticP2DuetSixP, StaticP2DuetSixPAvatar,
        StaticP3DuetSixP, StaticP3DuetSixPAvatar,
        StaticP4DuetSixP, StaticP4DuetSixPAvatar,
        StaticP5DuetSixP, StaticP5DuetSixPAvatar,
        StaticP6DuetSixP, StaticP6DuetSixPAvatar
      ]);
    end
    else
    begin
      maybeShow([
        StaticP1SixP, StaticP1SixPAvatar,
        StaticP2SixP, StaticP2SixPAvatar,
        StaticP3SixP, StaticP3SixPAvatar,
        StaticP4SixP, StaticP4SixPAvatar,
        StaticP5SixP, StaticP5SixPAvatar,
        StaticP6SixP, StaticP6SixPAvatar
      ]);
    end;
  end;

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
  // passing the integers for the statics by reference is deliberate
  procedure setColorAndAssignStatics(var singPlayer: TThemeSingPlayer; var avatarFrameStatic: integer; var nameStatic: integer; color: TRGB);
  begin
    setColorAndAssignAvatarFrameStatic(singPlayer.AvatarFrame, avatarFrameStatic, color);
    nameStatic := ScreenSing.AddText(singPlayer.Name);
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

  // SCREEN 1
  // 1 player
  setColorAndAssignAvatarFrameStatic(Theme.Sing.Solo1PP1.AvatarFrame, StaticP1[0], Col[1]);

  // 2 or 4 players
  setColorAndAssignAvatarFrameStatic(Theme.Sing.Solo2PP1.AvatarFrame, StaticP1TwoP[0], Col[1]);
  setColorAndAssignAvatarFrameStatic(Theme.Sing.Solo2PP2.AvatarFrame, StaticP2R[0]   , Col[2]);

  // 3 or 6 players
  setColorAndAssignAvatarFrameStatic(Theme.Sing.Solo3PP1.AvatarFrame, StaticP1ThreeP[0], Col[1]);
  setColorAndAssignAvatarFrameStatic(Theme.Sing.Solo3PP2.AvatarFrame, StaticP2M[0]     , Col[2]);
  setColorAndAssignAvatarFrameStatic(Theme.Sing.Solo3PP3.AvatarFrame, StaticP3R[0]     , Col[3]);

  // 3 or 6 players duet
  setColorAndAssignAvatarFrameStatic(Theme.Sing.Duet3PP1.AvatarFrame, StaticDuetP1ThreeP[0], Col[1]);
  setColorAndAssignAvatarFrameStatic(Theme.Sing.Duet3PP2.AvatarFrame, StaticDuetP2M[0]     , Col[2]);
  setColorAndAssignAvatarFrameStatic(Theme.Sing.Duet3PP3.AvatarFrame, StaticDuetP3R[0]     , Col[3]);

  // SCREEN 2
  // 1 player
  setColorAndAssignStatics(Theme.Sing.Solo1PP1, StaticP1[1], TextP1, Col[1]);

  // 2 or 4 players
  setColorAndAssignStatics(Theme.Sing.Solo2PP1, StaticP1TwoP[1], TextP1TwoP, Col[3]);
  setColorAndAssignStatics(Theme.Sing.Solo2PP2, StaticP2R[1]   , TextP2R   , Col[4]);

  // 3 or 6 players
  setColorAndAssignStatics(Theme.Sing.Solo3PP1, StaticP1ThreeP[1], TextP1ThreeP, Col[4]);
  setColorAndAssignStatics(Theme.Sing.Solo3PP2, StaticP2M[1]     , TextP2M     , Col[5]);
  setColorAndAssignStatics(Theme.Sing.Solo3PP3, StaticP3R[1]     , TextP3R     , Col[6]);

  // 3 or 6 players duet
  setColorAndAssignStatics(Theme.Sing.Duet3PP1, StaticDuetP1ThreeP[1], TextDuetP1ThreeP, Col[4]);
  setColorAndAssignStatics(Theme.Sing.Duet3PP2, StaticDuetP2M[1]     , TextDuetP2M     , Col[5]);
  setColorAndAssignStatics(Theme.Sing.Duet3PP3, StaticDuetP3R[1]     , TextDuetP3R     , Col[6]);

  for I := 1 to PlayersPlay do
  begin
    if (Party.bPartyGame) then
    begin
      ScreenSing.PlayerNames[I] := Ini.NameTeam[I-1];
    end
    else
    begin
      ScreenSing.PlayerNames[I] := Ini.Name[I-1];
    end;
    ScreenSing.PlayerDuetNames[I] := ScreenSing.PlayerNames[I];
  end;

  // 4 players in 1 screen
  setColorAndAssignStatics(Theme.Sing.Solo4PP1, StaticP1FourP, TextP1FourP, Col[1]);
  setColorAndAssignStatics(Theme.Sing.Solo4PP2, StaticP2FourP, TextP2FourP, Col[2]);
  setColorAndAssignStatics(Theme.Sing.Solo4PP3, StaticP3FourP, TextP3FourP, Col[3]);
  setColorAndAssignStatics(Theme.Sing.Solo4PP4, StaticP4FourP, TextP4FourP, Col[4]);

  // 6 players in 1 screen
  setColorAndAssignStatics(Theme.Sing.Solo6PP1, StaticP1SixP, TextP1SixP, Col[1]);
  setColorAndAssignStatics(Theme.Sing.Solo6PP2, StaticP2SixP, TextP2SixP, Col[2]);
  setColorAndAssignStatics(Theme.Sing.Solo6PP3, StaticP3SixP, TextP3SixP, Col[3]);
  setColorAndAssignStatics(Theme.Sing.Solo6PP4, StaticP4SixP, TextP4SixP, Col[4]);
  setColorAndAssignStatics(Theme.Sing.Solo6PP5, StaticP5SixP, TextP5SixP, Col[5]);
  setColorAndAssignStatics(Theme.Sing.Solo6PP6, StaticP6SixP, TextP6SixP, Col[6]);


  // 4 players duet in 1 screen
  setColorAndAssignStatics(Theme.Sing.Duet4PP1, StaticP1DuetFourP, TextP1DuetFourP, Col[1]);
  setColorAndAssignStatics(Theme.Sing.Duet4PP2, StaticP2DuetFourP, TextP2DuetFourP, Col[2]);
  setColorAndAssignStatics(Theme.Sing.Duet4PP3, StaticP3DuetFourP, TextP3DuetFourP, Col[3]);
  setColorAndAssignStatics(Theme.Sing.Duet4PP4, StaticP4DuetFourP, TextP4DuetFourP, Col[4]);

  // 6 players duet in 1 screen
  setColorAndAssignStatics(Theme.Sing.Duet6PP1, StaticP1DuetSixP, TextP1DuetSixP, Col[1]);
  setColorAndAssignStatics(Theme.Sing.Duet6PP2, StaticP2DuetSixP, TextP2DuetSixP, Col[2]);
  setColorAndAssignStatics(Theme.Sing.Duet6PP3, StaticP3DuetSixP, TextP3DuetSixP, Col[3]);
  setColorAndAssignStatics(Theme.Sing.Duet6PP4, StaticP4DuetSixP, TextP4DuetSixP, Col[4]);
  setColorAndAssignStatics(Theme.Sing.Duet6PP5, StaticP5DuetSixP, TextP5DuetSixP, Col[5]);
  setColorAndAssignStatics(Theme.Sing.Duet6PP6, StaticP6DuetSixP, TextP6DuetSixP, Col[6]);

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

  // avatars
  // SCREEN 1
  // 1P
  assignAvatarStatic(Theme.Sing.Solo1PP1, StaticP1Avatar[0], AvatarPlayerTextures[1]);
  // 2P
  assignAvatarStatic(Theme.Sing.Solo2PP1, StaticP1TwoPAvatar[0], AvatarPlayerTextures[1]);
  assignAvatarStatic(Theme.Sing.Solo2PP2, StaticP2RAvatar[0]   , AvatarPlayerTextures[2]);
  // 3P
  assignAvatarStatic(Theme.Sing.Solo3PP1, StaticP1ThreePAvatar[0], AvatarPlayerTextures[1]);
  assignAvatarStatic(Theme.Sing.Solo3PP2, StaticP2MAvatar[0]     , AvatarPlayerTextures[2]);
  assignAvatarStatic(Theme.Sing.Solo3PP3, StaticP3RAvatar[0]     , AvatarPlayerTextures[3]);
  // 3P duet
  assignAvatarStatic(Theme.Sing.Duet3PP1, StaticDuetP1ThreePAvatar[0], AvatarPlayerTextures[1]);
  assignAvatarStatic(Theme.Sing.Duet3PP2, StaticDuetP2MAvatar[0]     , AvatarPlayerTextures[2]);
  assignAvatarStatic(Theme.Sing.Duet3PP3, StaticDuetP3RAvatar[0]     , AvatarPlayerTextures[3]);
  // 4P
  assignAvatarStatic(Theme.Sing.Solo4PP1, StaticP1FourPAvatar, AvatarPlayerTextures[1]);
  assignAvatarStatic(Theme.Sing.Solo4PP2, StaticP2FourPAvatar, AvatarPlayerTextures[2]);
  assignAvatarStatic(Theme.Sing.Solo4PP3, StaticP3FourPAvatar, AvatarPlayerTextures[3]);
  assignAvatarStatic(Theme.Sing.Solo4PP4, StaticP4FourPAvatar, AvatarPlayerTextures[4]);
  // 4P duet
  assignAvatarStatic(Theme.Sing.Duet4PP1, StaticP1DuetFourPAvatar, AvatarPlayerTextures[1]);
  assignAvatarStatic(Theme.Sing.Duet4PP2, StaticP2DuetFourPAvatar, AvatarPlayerTextures[2]);
  assignAvatarStatic(Theme.Sing.Duet4PP3, StaticP3DuetFourPAvatar, AvatarPlayerTextures[3]);
  assignAvatarStatic(Theme.Sing.Duet4PP4, StaticP4DuetFourPAvatar, AvatarPlayerTextures[4]);
  // 6P
  assignAvatarStatic(Theme.Sing.Solo6PP1, StaticP1SixPAvatar, AvatarPlayerTextures[1]);
  assignAvatarStatic(Theme.Sing.Solo6PP2, StaticP2SixPAvatar, AvatarPlayerTextures[2]);
  assignAvatarStatic(Theme.Sing.Solo6PP3, StaticP3SixPAvatar, AvatarPlayerTextures[3]);
  assignAvatarStatic(Theme.Sing.Solo6PP4, StaticP4SixPAvatar, AvatarPlayerTextures[4]);
  assignAvatarStatic(Theme.Sing.Solo6PP5, StaticP5SixPAvatar, AvatarPlayerTextures[5]);
  assignAvatarStatic(Theme.Sing.Solo6PP6, StaticP6SixPAvatar, AvatarPlayerTextures[6]);
  // 6P duet
  assignAvatarStatic(Theme.Sing.Duet6PP1, StaticP1DuetSixPAvatar, AvatarPlayerTextures[1]);
  assignAvatarStatic(Theme.Sing.Duet6PP2, StaticP2DuetSixPAvatar, AvatarPlayerTextures[2]);
  assignAvatarStatic(Theme.Sing.Duet6PP3, StaticP3DuetSixPAvatar, AvatarPlayerTextures[3]);
  assignAvatarStatic(Theme.Sing.Duet6PP4, StaticP4DuetSixPAvatar, AvatarPlayerTextures[4]);
  assignAvatarStatic(Theme.Sing.Duet6PP5, StaticP5DuetSixPAvatar, AvatarPlayerTextures[5]);
  assignAvatarStatic(Theme.Sing.Duet6PP6, StaticP6DuetSixPAvatar, AvatarPlayerTextures[6]);

  // SCREEN 2
  // 1P
  assignAvatarStatic(Theme.Sing.Solo1PP1, StaticP1Avatar[1], AvatarPlayerTextures[2]);
  // 2P
  assignAvatarStatic(Theme.Sing.Solo2PP1, StaticP1TwoPAvatar[1], AvatarPlayerTextures[3]);
  assignAvatarStatic(Theme.Sing.Solo2PP2, StaticP2RAvatar[1]   , AvatarPlayerTextures[4]);
  // 3P
  assignAvatarStatic(Theme.Sing.Solo3PP1, StaticP1ThreePAvatar[1], AvatarPlayerTextures[4]);
  assignAvatarStatic(Theme.Sing.Solo3PP2, StaticP2MAvatar[1]     , AvatarPlayerTextures[5]);
  assignAvatarStatic(Theme.Sing.Solo3PP3, StaticP3RAvatar[1]     , AvatarPlayerTextures[6]);
  // 3P duet
  assignAvatarStatic(Theme.Sing.Duet3PP1, StaticDuetP1ThreePAvatar[1], AvatarPlayerTextures[4]);
  assignAvatarStatic(Theme.Sing.Duet3PP2, StaticDuetP2MAvatar[1]     , AvatarPlayerTextures[5]);
  assignAvatarStatic(Theme.Sing.Duet3PP3, StaticDuetP3RAvatar[1]     , AvatarPlayerTextures[6]);
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
  medley_end:             boolean;
  medley_start_applause:  boolean;
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

  // set player names (for 2 screens and only singstar skin)
  if ScreenAct = 1 then
  begin
    ScreenSing.Text[TextP1].Text           := ScreenSing.PlayerNames[1];
    ScreenSing.Text[TextP1TwoP].Text       := ScreenSing.PlayerNames[1];
    ScreenSing.Text[TextP1ThreeP].Text     := ScreenSing.PlayerNames[1];
    ScreenSing.Text[TextP2R].Text          := ScreenSing.PlayerNames[2];
    ScreenSing.Text[TextP2M].Text          := ScreenSing.PlayerNames[2];
    ScreenSing.Text[TextP3R].Text          := ScreenSing.PlayerNames[3];
    ScreenSing.Text[TextDuetP1ThreeP].Text := ScreenSing.PlayerDuetNames[1];
    ScreenSing.Text[TextDuetP2M].Text      := ScreenSing.PlayerDuetNames[2];
    ScreenSing.Text[TextDuetP3R].Text      := ScreenSing.PlayerDuetNames[3];
    ScreenSing.Text[TextP1FourP].Text      := ScreenSing.PlayerNames[1];
    ScreenSing.Text[TextP2FourP].Text      := ScreenSing.PlayerNames[2];
    ScreenSing.Text[TextP3FourP].Text      := ScreenSing.PlayerNames[3];
    ScreenSing.Text[TextP4FourP].Text      := ScreenSing.PlayerNames[4];
    ScreenSing.Text[TextP1DuetFourP].Text  := ScreenSing.PlayerDuetNames[1];
    ScreenSing.Text[TextP2DuetFourP].Text  := ScreenSing.PlayerDuetNames[2];
    ScreenSing.Text[TextP3DuetFourP].Text  := ScreenSing.PlayerDuetNames[3];
    ScreenSing.Text[TextP4DuetFourP].Text  := ScreenSing.PlayerDuetNames[4];
    ScreenSing.Text[TextP1SixP].Text       := ScreenSing.PlayerNames[1];
    ScreenSing.Text[TextP2SixP].Text       := ScreenSing.PlayerNames[2];
    ScreenSing.Text[TextP3SixP].Text       := ScreenSing.PlayerNames[3];
    ScreenSing.Text[TextP4SixP].Text       := ScreenSing.PlayerNames[4];
    ScreenSing.Text[TextP5SixP].Text       := ScreenSing.PlayerNames[5];
    ScreenSing.Text[TextP6SixP].Text       := ScreenSing.PlayerNames[6];
    ScreenSing.Text[TextP1DuetSixP].Text   := ScreenSing.PlayerDuetNames[1];
    ScreenSing.Text[TextP2DuetSixP].Text   := ScreenSing.PlayerDuetNames[2];
    ScreenSing.Text[TextP3DuetSixP].Text   := ScreenSing.PlayerDuetNames[3];
    ScreenSing.Text[TextP4DuetSixP].Text   := ScreenSing.PlayerDuetNames[4];
    ScreenSing.Text[TextP5DuetSixP].Text   := ScreenSing.PlayerDuetNames[5];
    ScreenSing.Text[TextP6DuetSixP].Text   := ScreenSing.PlayerDuetNames[6];

    if (CurrentSong.isDuet) then
    begin
      if (PlayersPlay = 4) then
      begin
        ScreenSing.LyricsDuetP1.LineColor_act.R := ColPlayer[0].R;
        ScreenSing.LyricsDuetP1.LineColor_act.G := ColPlayer[0].G;
        ScreenSing.LyricsDuetP1.LineColor_act.B := ColPlayer[0].B;

        ScreenSing.LyricsDuetP2.LineColor_act.R := ColPlayer[1].R;
        ScreenSing.LyricsDuetP2.LineColor_act.G := ColPlayer[1].G;
        ScreenSing.LyricsDuetP2.LineColor_act.B := ColPlayer[1].B;
      end;
    end;
  end;

  if ScreenAct = 2 then
  begin
    case PlayersPlay of
      4:
      begin
        ScreenSing.Text[TextP1TwoP].Text := ScreenSing.PlayerNames[3];
        ScreenSing.Text[TextP2R].Text    := ScreenSing.PlayerNames[4];

        if (CurrentSong.isDuet) and (PlayersPlay = 4) then
        begin
          ScreenSing.LyricsDuetP1.LineColor_act.R := ColPlayer[2].R;
          ScreenSing.LyricsDuetP1.LineColor_act.G := ColPlayer[2].G;
          ScreenSing.LyricsDuetP1.LineColor_act.B := ColPlayer[2].B;

          ScreenSing.LyricsDuetP2.LineColor_act.R := ColPlayer[3].R;
          ScreenSing.LyricsDuetP2.LineColor_act.G := ColPlayer[3].G;
          ScreenSing.LyricsDuetP2.LineColor_act.B := ColPlayer[3].B;
        end;

      end;
      6:
      begin
        if (CurrentSong.isDuet) then
        begin
          ScreenSing.Text[TextDuetP1ThreeP].Text := ScreenSing.PlayerDuetNames[4];
          ScreenSing.Text[TextDuetP2M].Text      := ScreenSing.PlayerDuetNames[5];
          ScreenSing.Text[TextDuetP3R].Text      := ScreenSing.PlayerDuetNames[6];
        end
        else
        begin
          ScreenSing.Text[TextP1ThreeP].Text := ScreenSing.PlayerNames[4];
          ScreenSing.Text[TextP2M].Text      := ScreenSing.PlayerNames[5];
          ScreenSing.Text[TextP3R].Text      := ScreenSing.PlayerNames[6];
        end;
      end;
    end; // case
  end; // if

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

  //the song was sung to the end?
  if not(ScreenSing.SungToEnd) and not(ScreenSong.RapToFreestyle) then
  begin
    Line := ScreenSing.Lyrics.GetUpperLine();
    if Line.LastLine then
    begin
      LastWord := Line.Words[Length(Line.Words)-1];
      if CurLyricsTime >= GetTimeFromBeat(LastWord.Start + LastWord.Length) then
        ScreenSing.SungToEnd := true;
    end;
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

  // always draw custom items
  ScreenSing.Statics[StaticLyricsBar].Visible := ScreenSing.Settings.LyricsVisible;
  ScreenSing.Statics[StaticLyricsBarDuet].Visible := ScreenSing.Settings.LyricsVisible and (CurrentSong.isDuet) and (PlayersPlay <> 1);
  ScreenSing.Statics[StaticTimeBar].Visible := ScreenSing.Settings.TimeBarVisible;
  SingDraw;

  // goldennotestarstwinkle
  GoldenRec.SpawnRec;

  // draw scores
  if (ScreenSing.Settings.ScoresVisible) and ((Ini.SingScores = 1) or (Party.bPartyGame)) then
    ScreenSing.Scores.Draw;

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
  Position:       real;
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
  I: integer;
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
    SongStart := CurrentSong.Medley.StartBeat - CurrentSong.BPM[0].BPM*CurrentSong.Medley.FadeIn_time/60;
    if SongStart < 0 then
      SongStart := 0;
    SongEnd := CurrentSong.Medley.EndBeat + CurrentSong.BPM[0].BPM*CurrentSong.Medley.FadeOut_time/60;
    SongDuration := SongEnd - SongStart;
    gapInBeats := 0;
  end
  else
  begin
    SongStart := CurrentSong.BPM[0].BPM*CurrentSong.Start/60;
    SongEnd := CurrentSong.BPM[0].BPM*TotalTime/60;
    SongDuration := SongEnd - SongStart;
    gapInBeats := CurrentSong.BPM[0].BPM*CurrentSong.GAP/1000/60;
  end;

  // draw sentence boxes
  for CurrentTrack := 0 to High(Tracks) do //for P1 of duet or solo lyrics, P2 of duet,..
  begin
    numLines := Length(Tracks[CurrentTrack].Lines); //Lyric lines
    //set color to player.color
    if (CurrentTrack = 0) then
      glColor4f(GetLyricColor(Ini.SingColor[0]).R, GetLyricColor(Ini.SingColor[0]).G, GetLyricColor(Ini.SingColor[0]).B, 0.6)
    else
      glColor4f(GetLyricColor(Ini.SingColor[CurrentTrack]).R, GetLyricColor(Ini.SingColor[CurrentTrack]).G, GetLyricColor(Ini.SingColor[CurrentTrack]).B, 0.6);

    glbegin(gl_quads);
    for LineIndex := 0 to numLines - 1 do
    begin
      if (Tracks[CurrentTrack].Lines[LineIndex].Notes = nil) then Continue;
      if (ScreenSong.Mode = smMedley) and (Tracks[CurrentTrack].Lines[LineIndex].Notes[0].StartBeat < SongStart) then Continue;
      pos := (gapInBeats + Tracks[CurrentTrack].Lines[LineIndex].Notes[0].StartBeat - SongStart) / SongDuration*w;
      br := (Tracks[CurrentTrack].Lines[LineIndex].Notes[Tracks[CurrentTrack].Lines[LineIndex].HighNote].StartBeat +
                Tracks[CurrentTrack].Lines[LineIndex].Notes[Tracks[CurrentTrack].Lines[LineIndex].HighNote].Duration -
                Tracks[CurrentTrack].Lines[LineIndex].Notes[0].StartBeat) / SongDuration*w; //br = last note of sentence position + its duration - first note of sentence position

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

