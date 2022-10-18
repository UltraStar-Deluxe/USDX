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
begin
  { if screens = 2 and playerplay <= 3 the 2nd screen shows the
    textures of screen 1 }
  if (PlayersPlay <= 3) and (Screen = 2) then
    Screen := 1;

  ScreenSing.Statics[StaticP1[0]].Visible := false;
  ScreenSing.Statics[StaticP1TwoP[0]].Visible := false;
  ScreenSing.Statics[StaticP2R[0]].Visible := false;
  ScreenSing.Statics[StaticP1ThreeP[0]].Visible := false;
  ScreenSing.Statics[StaticP2M[0]].Visible := false;
  ScreenSing.Statics[StaticP3R[0]].Visible := false;
  ScreenSing.Statics[StaticP1[1]].Visible := false;
  ScreenSing.Statics[StaticP1TwoP[1]].Visible := false;
  ScreenSing.Statics[StaticP2R[1]].Visible := false;
  ScreenSing.Statics[StaticP1ThreeP[1]].Visible := false;
  ScreenSing.Statics[StaticP2M[1]].Visible := false;
  ScreenSing.Statics[StaticP3R[1]].Visible := false;

  ScreenSing.Statics[StaticP1Avatar[0]].Visible := false;
  ScreenSing.Statics[StaticP1TwoPAvatar[0]].Visible := false;
  ScreenSing.Statics[StaticP2RAvatar[0]].Visible := false;
  ScreenSing.Statics[StaticP1ThreePAvatar[0]].Visible := false;
  ScreenSing.Statics[StaticP2MAvatar[0]].Visible := false;
  ScreenSing.Statics[StaticP3RAvatar[0]].Visible := false;
  ScreenSing.Statics[StaticP1Avatar[1]].Visible := false;
  ScreenSing.Statics[StaticP1TwoPAvatar[1]].Visible := false;
  ScreenSing.Statics[StaticP2RAvatar[1]].Visible := false;
  ScreenSing.Statics[StaticP1ThreePAvatar[1]].Visible := false;
  ScreenSing.Statics[StaticP2MAvatar[1]].Visible := false;
  ScreenSing.Statics[StaticP3RAvatar[1]].Visible := false;

  ScreenSing.Statics[StaticDuetP1ThreeP[0]].Visible := false;
  ScreenSing.Statics[StaticDuetP1ThreeP[1]].Visible := false;
  ScreenSing.Statics[StaticDuetP2M[0]].Visible := false;
  ScreenSing.Statics[StaticDuetP2M[1]].Visible := false;
  ScreenSing.Statics[StaticDuetP3R[1]].Visible := false;
  ScreenSing.Statics[StaticDuetP3R[0]].Visible := false;

  ScreenSing.Statics[StaticDuetP1ThreePAvatar[0]].Visible := false;
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[1]].Visible := false;
  ScreenSing.Statics[StaticDuetP2MAvatar[0]].Visible := false;
  ScreenSing.Statics[StaticDuetP2MAvatar[1]].Visible := false;
  ScreenSing.Statics[StaticDuetP3RAvatar[1]].Visible := false;
  ScreenSing.Statics[StaticDuetP3RAvatar[0]].Visible := false;

  // 4/6 players in one screen
  ScreenSing.Statics[StaticP1FourP].Visible := false;
  ScreenSing.Statics[StaticP2FourP].Visible := false;
  ScreenSing.Statics[StaticP3FourP].Visible := false;
  ScreenSing.Statics[StaticP4FourP].Visible := false;

  ScreenSing.Statics[StaticP1FourPAvatar].Visible := false;
  ScreenSing.Statics[StaticP2FourPAvatar].Visible := false;
  ScreenSing.Statics[StaticP3FourPAvatar].Visible := false;
  ScreenSing.Statics[StaticP4FourPAvatar].Visible := false;

  ScreenSing.Statics[StaticP1DuetFourP].Visible := false;
  ScreenSing.Statics[StaticP2DuetFourP].Visible := false;
  ScreenSing.Statics[StaticP3DuetFourP].Visible := false;
  ScreenSing.Statics[StaticP4DuetFourP].Visible := false;

  ScreenSing.Statics[StaticP1DuetFourPAvatar].Visible := false;
  ScreenSing.Statics[StaticP2DuetFourPAvatar].Visible := false;
  ScreenSing.Statics[StaticP3DuetFourPAvatar].Visible := false;
  ScreenSing.Statics[StaticP4DuetFourPAvatar].Visible := false;

  ScreenSing.Statics[StaticP1SixP].Visible := false;
  ScreenSing.Statics[StaticP2SixP].Visible := false;
  ScreenSing.Statics[StaticP3SixP].Visible := false;
  ScreenSing.Statics[StaticP4SixP].Visible := false;
  ScreenSing.Statics[StaticP5SixP].Visible := false;
  ScreenSing.Statics[StaticP6SixP].Visible := false;

  ScreenSing.Statics[StaticP1SixPAvatar].Visible := false;
  ScreenSing.Statics[StaticP2SixPAvatar].Visible := false;
  ScreenSing.Statics[StaticP3SixPAvatar].Visible := false;
  ScreenSing.Statics[StaticP4SixPAvatar].Visible := false;
  ScreenSing.Statics[StaticP5SixPAvatar].Visible := false;
  ScreenSing.Statics[StaticP6SixPAvatar].Visible := false;

  ScreenSing.Statics[StaticP1DuetSixP].Visible := false;
  ScreenSing.Statics[StaticP2DuetSixP].Visible := false;
  ScreenSing.Statics[StaticP3DuetSixP].Visible := false;
  ScreenSing.Statics[StaticP4DuetSixP].Visible := false;
  ScreenSing.Statics[StaticP5DuetSixP].Visible := false;
  ScreenSing.Statics[StaticP6DuetSixP].Visible := false;

  ScreenSing.Statics[StaticP1DuetSixPAvatar].Visible := false;
  ScreenSing.Statics[StaticP2DuetSixPAvatar].Visible := false;
  ScreenSing.Statics[StaticP3DuetSixPAvatar].Visible := false;
  ScreenSing.Statics[StaticP4DuetSixPAvatar].Visible := false;
  ScreenSing.Statics[StaticP5DuetSixPAvatar].Visible := false;
  ScreenSing.Statics[StaticP6DuetSixPAvatar].Visible := false;

  if (PlayersPlay = 1) then
  begin
    if (Screen = 2) then
    begin
      ScreenSing.Statics[StaticP1[0]].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP1Avatar[0]].Visible := ScreenSing.Settings.AvatarsVisible;
    end;

    if (Screen = 1) then
    begin
      ScreenSing.Statics[StaticP1[0]].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP1Avatar[0]].Visible := ScreenSing.Settings.AvatarsVisible;
    end;
  end;

  if (PlayersPlay = 2) or ((PlayersPlay = 4) and (Ini.Screens = 1)) then
  begin
    if (Screen = 2) then
    begin
      ScreenSing.Statics[StaticP1TwoP[1]].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP2R[1]].Visible := ScreenSing.Settings.AvatarsVisible;

      ScreenSing.Statics[StaticP1TwoPAvatar[1]].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP2RAvatar[1]].Visible := ScreenSing.Settings.AvatarsVisible;
    end;

    if (Screen = 1) then
    begin
      ScreenSing.Statics[StaticP1TwoP[0]].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP2R[0]].Visible := ScreenSing.Settings.AvatarsVisible;

      ScreenSing.Statics[StaticP1TwoPAvatar[0]].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP2RAvatar[0]].Visible := ScreenSing.Settings.AvatarsVisible;
    end;
  end;

  if (PlayersPlay = 3) or ((PlayersPlay = 6) and (Ini.Screens = 1)) then
  begin
    if (CurrentSong.isDuet) then
    begin
      if (Screen = 2) then
      begin
        ScreenSing.Statics[StaticDuetP1ThreeP[1]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticDuetP2M[1]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticDuetP3R[1]].Visible := ScreenSing.Settings.AvatarsVisible;

        ScreenSing.Statics[StaticDuetP1ThreePAvatar[1]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticDuetP2MAvatar[1]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticDuetP3RAvatar[1]].Visible := ScreenSing.Settings.AvatarsVisible;
      end;

      if (Screen = 1) then
      begin
        ScreenSing.Statics[StaticDuetP1ThreeP[0]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticDuetP2M[0]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticDuetP3R[0]].Visible := ScreenSing.Settings.AvatarsVisible;

        ScreenSing.Statics[StaticDuetP1ThreePAvatar[0]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticDuetP2MAvatar[0]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticDuetP3RAvatar[0]].Visible := ScreenSing.Settings.AvatarsVisible;
      end;
    end
    else
    begin
      if (Screen = 2) then
      begin
        ScreenSing.Statics[StaticP1ThreeP[1]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticP2M[1]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticP3R[1]].Visible := ScreenSing.Settings.AvatarsVisible;

        ScreenSing.Statics[StaticP1ThreePAvatar[1]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticP2MAvatar[1]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticP3RAvatar[1]].Visible := ScreenSing.Settings.AvatarsVisible;
      end;

      if (Screen = 1) then
      begin
        ScreenSing.Statics[StaticP1ThreeP[0]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticP2M[0]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticP3R[0]].Visible := ScreenSing.Settings.AvatarsVisible;

        ScreenSing.Statics[StaticP1ThreePAvatar[0]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticP2MAvatar[0]].Visible := ScreenSing.Settings.AvatarsVisible;
        ScreenSing.Statics[StaticP3RAvatar[0]].Visible := ScreenSing.Settings.AvatarsVisible;
      end;
    end;
  end;

  // 4 Players in 1 Screen
  if (PlayersPlay = 4) and (Ini.Screens = 0) then
  begin
    if (CurrentSong.isDuet) then
    begin
      ScreenSing.Statics[StaticP1DuetFourP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP2DuetFourP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP3DuetFourP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP4DuetFourP].Visible := ScreenSing.Settings.AvatarsVisible;

      ScreenSing.Statics[StaticP1DuetFourPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP2DuetFourPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP3DuetFourPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP4DuetFourPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
    end
    else
    begin
      ScreenSing.Statics[StaticP1FourP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP2FourP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP3FourP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP4FourP].Visible := ScreenSing.Settings.AvatarsVisible;

      ScreenSing.Statics[StaticP1FourPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP2FourPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP3FourPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP4FourPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
    end;
  end;

  // 6 Players in 1 Screen
  if (PlayersPlay = 6) and (Ini.Screens = 0) then
  begin
    if (CurrentSong.isDuet) then
    begin
      ScreenSing.Statics[StaticP1DuetSixP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP2DuetSixP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP3DuetSixP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP4DuetSixP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP5DuetSixP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP6DuetSixP].Visible := ScreenSing.Settings.AvatarsVisible;

      ScreenSing.Statics[StaticP1DuetSixPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP2DuetSixPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP3DuetSixPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP4DuetSixPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP5DuetSixPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP6DuetSixPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
    end
    else
    begin
      ScreenSing.Statics[StaticP1SixP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP2SixP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP3SixP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP4SixP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP5SixP].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP6SixP].Visible := ScreenSing.Settings.AvatarsVisible;

      ScreenSing.Statics[StaticP1SixPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP2SixPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP3SixPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP4SixPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP5SixPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
      ScreenSing.Statics[StaticP6SixPAvatar].Visible := ScreenSing.Settings.AvatarsVisible;
    end;
  end;

end;

constructor TScreenSingView.Create;
var
  Col: array [1..UIni.IMaxPlayerCount] of TRGB;
  I: integer;
  Color: cardinal;
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
  StaticTimeProgress := ScreenSing.AddStatic(Theme.Sing.StaticTimeProgress);
  TextTimeLabelText := ScreenSing.AddText(Theme.Sing.TextTimeLabelText);
  TextTimeText := ScreenSing.AddText(Theme.Sing.TextTimeText);

  for I := 1 to 6 do
    Col[I] := GetPlayerColor(Ini.SingColor[I - 1]);

  // SCREEN 1
  // 1 player       | P1
  Theme.Sing.StaticP1.ColR := Col[1].R;
  Theme.Sing.StaticP1.ColG := Col[1].G;
  Theme.Sing.StaticP1.ColB := Col[1].B;

  // 2 or 4 players | P1
  Theme.Sing.StaticP1TwoP.ColR := Col[1].R;
  Theme.Sing.StaticP1TwoP.ColG := Col[1].G;
  Theme.Sing.StaticP1TwoP.ColB := Col[1].B;

  //                | P2
  Theme.Sing.StaticP2R.ColR := Col[2].R;
  Theme.Sing.StaticP2R.ColG := Col[2].G;
  Theme.Sing.StaticP2R.ColB := Col[2].B;

  // 3 or 6 players | P1
  Theme.Sing.StaticP1ThreeP.ColR := Col[1].R;
  Theme.Sing.StaticP1ThreeP.ColG := Col[1].G;
  Theme.Sing.StaticP1ThreeP.ColB := Col[1].B;

  //                | P2
  Theme.Sing.StaticP2M.ColR := Col[2].R;
  Theme.Sing.StaticP2M.ColG := Col[2].G;
  Theme.Sing.StaticP2M.ColB := Col[2].B;

  //                | P3

  Theme.Sing.StaticP3R.ColR := Col[3].R;
  Theme.Sing.StaticP3R.ColG := Col[3].G;
  Theme.Sing.StaticP3R.ColB := Col[3].B;

  // 3 or 6 players | P1 DUET
  Theme.Sing.StaticDuetP1ThreeP.ColR := Col[1].R;
  Theme.Sing.StaticDuetP1ThreeP.ColG := Col[1].G;
  Theme.Sing.StaticDuetP1ThreeP.ColB := Col[1].B;

  //                | P2 DUET
  Theme.Sing.StaticDuetP2M.ColR := Col[2].R;
  Theme.Sing.StaticDuetP2M.ColG := Col[2].G;
  Theme.Sing.StaticDuetP2M.ColB := Col[2].B;

  //                | P3 DUET

  Theme.Sing.StaticDuetP3R.ColR := Col[3].R;
  Theme.Sing.StaticDuetP3R.ColG := Col[3].G;
  Theme.Sing.StaticDuetP3R.ColB := Col[3].B;

  StaticP1[0]       := ScreenSing.AddStatic(Theme.Sing.StaticP1);
  StaticP1TwoP[0]   := ScreenSing.AddStatic(Theme.Sing.StaticP1TwoP);
  StaticP2R[0]      := ScreenSing.AddStatic(Theme.Sing.StaticP2R);
  StaticP1ThreeP[0] := ScreenSing.AddStatic(Theme.Sing.StaticP1ThreeP);
  StaticP2M[0]      := ScreenSing.AddStatic(Theme.Sing.StaticP2M);
  StaticP3R[0]      := ScreenSing.AddStatic(Theme.Sing.StaticP3R);
  StaticDuetP1ThreeP[0] := ScreenSing.AddStatic(Theme.Sing.StaticDuetP1ThreeP);
  StaticDuetP2M[0]      := ScreenSing.AddStatic(Theme.Sing.StaticDuetP2M);
  StaticDuetP3R[0]      := ScreenSing.AddStatic(Theme.Sing.StaticDuetP3R);

  // SCREEN 2
  // 1 player       | P1
  Theme.Sing.StaticP1.ColR := Col[1].R;
  Theme.Sing.StaticP1.ColG := Col[1].G;
  Theme.Sing.StaticP1.ColB := Col[1].B;

  // 2 or 4 players | P1
  Theme.Sing.StaticP1TwoP.ColR := Col[3].R;
  Theme.Sing.StaticP1TwoP.ColG := Col[3].G;
  Theme.Sing.StaticP1TwoP.ColB := Col[3].B;

  //                | P2
  Theme.Sing.StaticP2R.ColR := Col[4].R;
  Theme.Sing.StaticP2R.ColG := Col[4].G;
  Theme.Sing.StaticP2R.ColB := Col[4].B;

  // 3 or 6 players | P1
  Theme.Sing.StaticP1ThreeP.ColR := Col[4].R;
  Theme.Sing.StaticP1ThreeP.ColG := Col[4].G;
  Theme.Sing.StaticP1ThreeP.ColB := Col[4].B;

  //                | P2
  Theme.Sing.StaticP2M.ColR := Col[5].R;
  Theme.Sing.StaticP2M.ColG := Col[5].G;
  Theme.Sing.StaticP2M.ColB := Col[5].B;

  //                | P3
  Theme.Sing.StaticP3R.ColR := Col[6].R;
  Theme.Sing.StaticP3R.ColG := Col[6].G;
  Theme.Sing.StaticP3R.ColB := Col[6].B;

    // 3 or 6 players | P1 DUET
  Theme.Sing.StaticDuetP1ThreeP.ColR := Col[4].R;
  Theme.Sing.StaticDuetP1ThreeP.ColG := Col[4].G;
  Theme.Sing.StaticDuetP1ThreeP.ColB := Col[4].B;

  //                | P2 DUET
  Theme.Sing.StaticDuetP2M.ColR := Col[5].R;
  Theme.Sing.StaticDuetP2M.ColG := Col[5].G;
  Theme.Sing.StaticDuetP2M.ColB := Col[5].B;

  //                | P3 DUET
  Theme.Sing.StaticDuetP3R.ColR := Col[6].R;
  Theme.Sing.StaticDuetP3R.ColG := Col[6].G;
  Theme.Sing.StaticDuetP3R.ColB := Col[6].B;

  StaticP1[1]       := ScreenSing.AddStatic(Theme.Sing.StaticP1);
  StaticP1TwoP[1]   := ScreenSing.AddStatic(Theme.Sing.StaticP1TwoP);
  StaticP2R[1]      := ScreenSing.AddStatic(Theme.Sing.StaticP2R);
  StaticP1ThreeP[1] := ScreenSing.AddStatic(Theme.Sing.StaticP1ThreeP);
  StaticP2M[1]      := ScreenSing.AddStatic(Theme.Sing.StaticP2M);
  StaticP3R[1]      := ScreenSing.AddStatic(Theme.Sing.StaticP3R);
  StaticDuetP1ThreeP[1] := ScreenSing.AddStatic(Theme.Sing.StaticDuetP1ThreeP);
  StaticDuetP2M[1]      := ScreenSing.AddStatic(Theme.Sing.StaticDuetP2M);
  StaticDuetP3R[1]      := ScreenSing.AddStatic(Theme.Sing.StaticDuetP3R);

  TextP1   := ScreenSing.AddText(Theme.Sing.TextP1);
  TextP1TwoP   := ScreenSing.AddText(Theme.Sing.TextP1TwoP);
  TextP2R   := ScreenSing.AddText(Theme.Sing.TextP2R);
  TextP1ThreeP   := ScreenSing.AddText(Theme.Sing.TextP1ThreeP);
  TextP2M   := ScreenSing.AddText(Theme.Sing.TextP2M);
  TextP3R   := ScreenSing.AddText(Theme.Sing.TextP3R);
  TextDuetP1ThreeP   := ScreenSing.AddText(Theme.Sing.TextDuetP1ThreeP);
  TextDuetP2M   := ScreenSing.AddText(Theme.Sing.TextDuetP2M);
  TextDuetP3R   := ScreenSing.AddText(Theme.Sing.TextDuetP3R);

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

  // 4/6 players in 1 screen
  // P1
  Theme.Sing.StaticP1FourP.ColR := Col[1].R;
  Theme.Sing.StaticP1FourP.ColG := Col[1].G;
  Theme.Sing.StaticP1FourP.ColB := Col[1].B;

  // P2
  Theme.Sing.StaticP2FourP.ColR := Col[2].R;
  Theme.Sing.StaticP2FourP.ColG := Col[2].G;
  Theme.Sing.StaticP2FourP.ColB := Col[2].B;

  // P3
  Theme.Sing.StaticP3FourP.ColR := Col[3].R;
  Theme.Sing.StaticP3FourP.ColG := Col[3].G;
  Theme.Sing.StaticP3FourP.ColB := Col[3].B;

  // P4
  Theme.Sing.StaticP4FourP.ColR := Col[4].R;
  Theme.Sing.StaticP4FourP.ColG := Col[4].G;
  Theme.Sing.StaticP4FourP.ColB := Col[4].B;

  StaticP1FourP   := ScreenSing.AddStatic(Theme.Sing.StaticP1FourP);
  StaticP2FourP   := ScreenSing.AddStatic(Theme.Sing.StaticP2FourP);
  StaticP3FourP   := ScreenSing.AddStatic(Theme.Sing.StaticP3FourP);
  StaticP4FourP   := ScreenSing.AddStatic(Theme.Sing.StaticP4FourP);

  TextP1FourP   := ScreenSing.AddText(Theme.Sing.TextP1FourP);
  TextP2FourP   := ScreenSing.AddText(Theme.Sing.TextP2FourP);
  TextP3FourP   := ScreenSing.AddText(Theme.Sing.TextP3FourP);
  TextP4FourP   := ScreenSing.AddText(Theme.Sing.TextP4FourP);

  // P1
  Theme.Sing.StaticP1SixP.ColR := Col[1].R;
  Theme.Sing.StaticP1SixP.ColG := Col[1].G;
  Theme.Sing.StaticP1SixP.ColB := Col[1].B;

  // P2
  Theme.Sing.StaticP2SixP.ColR := Col[2].R;
  Theme.Sing.StaticP2SixP.ColG := Col[2].G;
  Theme.Sing.StaticP2SixP.ColB := Col[2].B;

  // P3
  Theme.Sing.StaticP3SixP.ColR := Col[3].R;
  Theme.Sing.StaticP3SixP.ColG := Col[3].G;
  Theme.Sing.StaticP3SixP.ColB := Col[3].B;

  // P4
  Theme.Sing.StaticP4SixP.ColR := Col[4].R;
  Theme.Sing.StaticP4SixP.ColG := Col[4].G;
  Theme.Sing.StaticP4SixP.ColB := Col[4].B;

  // P5
  Theme.Sing.StaticP5SixP.ColR := Col[5].R;
  Theme.Sing.StaticP5SixP.ColG := Col[5].G;
  Theme.Sing.StaticP5SixP.ColB := Col[5].B;

  // P6
  Theme.Sing.StaticP6SixP.ColR := Col[6].R;
  Theme.Sing.StaticP6SixP.ColG := Col[6].G;
  Theme.Sing.StaticP6SixP.ColB := Col[6].B;

  StaticP1SixP  := ScreenSing.AddStatic(Theme.Sing.StaticP1SixP);
  StaticP2SixP  := ScreenSing.AddStatic(Theme.Sing.StaticP2SixP);
  StaticP3SixP  := ScreenSing.AddStatic(Theme.Sing.StaticP3SixP);
  StaticP4SixP  := ScreenSing.AddStatic(Theme.Sing.StaticP4SixP);
  StaticP5SixP  := ScreenSing.AddStatic(Theme.Sing.StaticP5SixP);
  StaticP6SixP  := ScreenSing.AddStatic(Theme.Sing.StaticP6SixP);

  TextP1SixP   := ScreenSing.AddText(Theme.Sing.TextP1SixP);
  TextP2SixP   := ScreenSing.AddText(Theme.Sing.TextP2SixP);
  TextP3SixP   := ScreenSing.AddText(Theme.Sing.TextP3SixP);
  TextP4SixP   := ScreenSing.AddText(Theme.Sing.TextP4SixP);
  TextP5SixP   := ScreenSing.AddText(Theme.Sing.TextP5SixP);
  TextP6SixP   := ScreenSing.AddText(Theme.Sing.TextP6SixP);


  // 4/6 players duet in 1 screen
  // P1
  Theme.Sing.StaticP1DuetFourP.ColR := Col[1].R;
  Theme.Sing.StaticP1DuetFourP.ColG := Col[1].G;
  Theme.Sing.StaticP1DuetFourP.ColB := Col[1].B;

  // P2
  Theme.Sing.StaticP2DuetFourP.ColR := Col[2].R;
  Theme.Sing.StaticP2DuetFourP.ColG := Col[2].G;
  Theme.Sing.StaticP2DuetFourP.ColB := Col[2].B;

  // P3
  Theme.Sing.StaticP3DuetFourP.ColR := Col[3].R;
  Theme.Sing.StaticP3DuetFourP.ColG := Col[3].G;
  Theme.Sing.StaticP3DuetFourP.ColB := Col[3].B;

  // P4
  Theme.Sing.StaticP4DuetFourP.ColR := Col[4].R;
  Theme.Sing.StaticP4DuetFourP.ColG := Col[4].G;
  Theme.Sing.StaticP4DuetFourP.ColB := Col[4].B;

  StaticP1DuetFourP   := ScreenSing.AddStatic(Theme.Sing.StaticP1DuetFourP);
  StaticP2DuetFourP   := ScreenSing.AddStatic(Theme.Sing.StaticP2DuetFourP);
  StaticP3DuetFourP   := ScreenSing.AddStatic(Theme.Sing.StaticP3DuetFourP);
  StaticP4DuetFourP   := ScreenSing.AddStatic(Theme.Sing.StaticP4DuetFourP);

  TextP1DuetFourP   := ScreenSing.AddText(Theme.Sing.TextP1DuetFourP);
  TextP2DuetFourP   := ScreenSing.AddText(Theme.Sing.TextP2DuetFourP);
  TextP3DuetFourP   := ScreenSing.AddText(Theme.Sing.TextP3DuetFourP);
  TextP4DuetFourP   := ScreenSing.AddText(Theme.Sing.TextP4DuetFourP);

  // P1
  Theme.Sing.StaticP1DuetSixP.ColR := Col[1].R;
  Theme.Sing.StaticP1DuetSixP.ColG := Col[1].G;
  Theme.Sing.StaticP1DuetSixP.ColB := Col[1].B;

  // P2
  Theme.Sing.StaticP2DuetSixP.ColR := Col[2].R;
  Theme.Sing.StaticP2DuetSixP.ColG := Col[2].G;
  Theme.Sing.StaticP2DuetSixP.ColB := Col[2].B;

  // P3
  Theme.Sing.StaticP3DuetSixP.ColR := Col[3].R;
  Theme.Sing.StaticP3DuetSixP.ColG := Col[3].G;
  Theme.Sing.StaticP3DuetSixP.ColB := Col[3].B;

  // P4
  Theme.Sing.StaticP4DuetSixP.ColR := Col[4].R;
  Theme.Sing.StaticP4DuetSixP.ColG := Col[4].G;
  Theme.Sing.StaticP4DuetSixP.ColB := Col[4].B;

  // P5
  Theme.Sing.StaticP5DuetSixP.ColR := Col[5].R;
  Theme.Sing.StaticP5DuetSixP.ColG := Col[5].G;
  Theme.Sing.StaticP5DuetSixP.ColB := Col[5].B;

  // P6
  Theme.Sing.StaticP6DuetSixP.ColR := Col[6].R;
  Theme.Sing.StaticP6DuetSixP.ColG := Col[6].G;
  Theme.Sing.StaticP6DuetSixP.ColB := Col[6].B;

  StaticP1DuetSixP  := ScreenSing.AddStatic(Theme.Sing.StaticP1DuetSixP);
  StaticP2DuetSixP  := ScreenSing.AddStatic(Theme.Sing.StaticP2DuetSixP);
  StaticP3DuetSixP  := ScreenSing.AddStatic(Theme.Sing.StaticP3DuetSixP);
  StaticP4DuetSixP  := ScreenSing.AddStatic(Theme.Sing.StaticP4DuetSixP);
  StaticP5DuetSixP  := ScreenSing.AddStatic(Theme.Sing.StaticP5DuetSixP);
  StaticP6DuetSixP  := ScreenSing.AddStatic(Theme.Sing.StaticP6DuetSixP);

  TextP1DuetSixP   := ScreenSing.AddText(Theme.Sing.TextP1DuetSixP);
  TextP2DuetSixP   := ScreenSing.AddText(Theme.Sing.TextP2DuetSixP);
  TextP3DuetSixP   := ScreenSing.AddText(Theme.Sing.TextP3DuetSixP);
  TextP4DuetSixP   := ScreenSing.AddText(Theme.Sing.TextP4DuetSixP);
  TextP5DuetSixP   := ScreenSing.AddText(Theme.Sing.TextP5DuetSixP);
  TextP6DuetSixP   := ScreenSing.AddText(Theme.Sing.TextP6DuetSixP);

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
  StaticP1Avatar[0] := ScreenSing.AddStatic(Theme.Sing.StaticP1Avatar);
  ScreenSing.Statics[StaticP1Avatar[0]].Texture := AvatarPlayerTextures[1];
  ScreenSing.Statics[StaticP1Avatar[0]].Texture.X  := Theme.Sing.StaticP1Avatar.X;
  ScreenSing.Statics[StaticP1Avatar[0]].Texture.Y  := Theme.Sing.StaticP1Avatar.Y;
  ScreenSing.Statics[StaticP1Avatar[0]].Texture.H  := Theme.Sing.StaticP1Avatar.H;
  ScreenSing.Statics[StaticP1Avatar[0]].Texture.W  := Theme.Sing.StaticP1Avatar.W;
  ScreenSing.Statics[StaticP1Avatar[0]].Texture.Z := Theme.Sing.StaticP1Avatar.Z;
  ScreenSing.Statics[StaticP1Avatar[0]].Texture.Alpha := Theme.Sing.StaticP1Avatar.Alpha;

  StaticP1Avatar[1] := ScreenSing.AddStatic(Theme.Sing.StaticP1Avatar);
  ScreenSing.Statics[StaticP1Avatar[1]].Texture := AvatarPlayerTextures[2];
  ScreenSing.Statics[StaticP1Avatar[1]].Texture.X  := Theme.Sing.StaticP1Avatar.X;
  ScreenSing.Statics[StaticP1Avatar[1]].Texture.Y  := Theme.Sing.StaticP1Avatar.Y;
  ScreenSing.Statics[StaticP1Avatar[1]].Texture.H  := Theme.Sing.StaticP1Avatar.H;
  ScreenSing.Statics[StaticP1Avatar[1]].Texture.W  := Theme.Sing.StaticP1Avatar.W;
  ScreenSing.Statics[StaticP1Avatar[1]].Texture.Z := Theme.Sing.StaticP1Avatar.Z;
  ScreenSing.Statics[StaticP1Avatar[1]].Texture.Alpha := Theme.Sing.StaticP1Avatar.Alpha;

  StaticP1TwoPAvatar[0] := ScreenSing.AddStatic(Theme.Sing.StaticP1TwoPAvatar);
  ScreenSing.Statics[StaticP1TwoPAvatar[0]].Texture := AvatarPlayerTextures[1];
  ScreenSing.Statics[StaticP1TwoPAvatar[0]].Texture.X  := Theme.Sing.StaticP1TwoPAvatar.X;
  ScreenSing.Statics[StaticP1TwoPAvatar[0]].Texture.Y  := Theme.Sing.StaticP1TwoPAvatar.Y;
  ScreenSing.Statics[StaticP1TwoPAvatar[0]].Texture.H  := Theme.Sing.StaticP1TwoPAvatar.H;
  ScreenSing.Statics[StaticP1TwoPAvatar[0]].Texture.W  := Theme.Sing.StaticP1TwoPAvatar.W;
  ScreenSing.Statics[StaticP1TwoPAvatar[0]].Texture.Z := Theme.Sing.StaticP1TwoPAvatar.Z;
  ScreenSing.Statics[StaticP1TwoPAvatar[0]].Texture.Alpha := Theme.Sing.StaticP1TwoPAvatar.Alpha;

  StaticP1TwoPAvatar[1] := ScreenSing.AddStatic(Theme.Sing.StaticP1TwoPAvatar);
  ScreenSing.Statics[StaticP1TwoPAvatar[1]].Texture := AvatarPlayerTextures[3];
  ScreenSing.Statics[StaticP1TwoPAvatar[1]].Texture.X  := Theme.Sing.StaticP1TwoPAvatar.X;
  ScreenSing.Statics[StaticP1TwoPAvatar[1]].Texture.Y  := Theme.Sing.StaticP1TwoPAvatar.Y;
  ScreenSing.Statics[StaticP1TwoPAvatar[1]].Texture.H  := Theme.Sing.StaticP1TwoPAvatar.H;
  ScreenSing.Statics[StaticP1TwoPAvatar[1]].Texture.W  := Theme.Sing.StaticP1TwoPAvatar.W;
  ScreenSing.Statics[StaticP1TwoPAvatar[1]].Texture.Z := Theme.Sing.StaticP1TwoPAvatar.Z;
  ScreenSing.Statics[StaticP1TwoPAvatar[1]].Texture.Alpha := Theme.Sing.StaticP1TwoPAvatar.Alpha;

  StaticP2RAvatar[0] := ScreenSing.AddStatic(Theme.Sing.StaticP2RAvatar);
  ScreenSing.Statics[StaticP2RAvatar[0]].Texture := AvatarPlayerTextures[2];
  ScreenSing.Statics[StaticP2RAvatar[0]].Texture.X  := Theme.Sing.StaticP2RAvatar.X;
  ScreenSing.Statics[StaticP2RAvatar[0]].Texture.Y  := Theme.Sing.StaticP2RAvatar.Y;
  ScreenSing.Statics[StaticP2RAvatar[0]].Texture.H  := Theme.Sing.StaticP2RAvatar.H;
  ScreenSing.Statics[StaticP2RAvatar[0]].Texture.W  := Theme.Sing.StaticP2RAvatar.W;
  ScreenSing.Statics[StaticP2RAvatar[0]].Texture.Z := Theme.Sing.StaticP2RAvatar.Z;
  ScreenSing.Statics[StaticP2RAvatar[0]].Texture.Alpha := Theme.Sing.StaticP2RAvatar.Alpha;

  StaticP2RAvatar[1] := ScreenSing.AddStatic(Theme.Sing.StaticP2RAvatar);
  ScreenSing.Statics[StaticP2RAvatar[1]].Texture := AvatarPlayerTextures[4];
  ScreenSing.Statics[StaticP2RAvatar[1]].Texture.X  := Theme.Sing.StaticP2RAvatar.X;
  ScreenSing.Statics[StaticP2RAvatar[1]].Texture.Y  := Theme.Sing.StaticP2RAvatar.Y;
  ScreenSing.Statics[StaticP2RAvatar[1]].Texture.H  := Theme.Sing.StaticP2RAvatar.H;
  ScreenSing.Statics[StaticP2RAvatar[1]].Texture.W  := Theme.Sing.StaticP2RAvatar.W;
  ScreenSing.Statics[StaticP2RAvatar[1]].Texture.Z := Theme.Sing.StaticP2RAvatar.Z;
  ScreenSing.Statics[StaticP2RAvatar[1]].Texture.Alpha := Theme.Sing.StaticP2RAvatar.Alpha;

  StaticP1ThreePAvatar[0] := ScreenSing.AddStatic(Theme.Sing.StaticP1ThreePAvatar);
  ScreenSing.Statics[StaticP1ThreePAvatar[0]].Texture := AvatarPlayerTextures[1];
  ScreenSing.Statics[StaticP1ThreePAvatar[0]].Texture.X  := Theme.Sing.StaticP1ThreePAvatar.X;
  ScreenSing.Statics[StaticP1ThreePAvatar[0]].Texture.Y  := Theme.Sing.StaticP1ThreePAvatar.Y;
  ScreenSing.Statics[StaticP1ThreePAvatar[0]].Texture.H  := Theme.Sing.StaticP1ThreePAvatar.H;
  ScreenSing.Statics[StaticP1ThreePAvatar[0]].Texture.W  := Theme.Sing.StaticP1ThreePAvatar.W;
  ScreenSing.Statics[StaticP1ThreePAvatar[0]].Texture.Z := Theme.Sing.StaticP1ThreePAvatar.Z;
  ScreenSing.Statics[StaticP1ThreePAvatar[0]].Texture.Alpha := Theme.Sing.StaticP1ThreePAvatar.Alpha;

  StaticP1ThreePAvatar[1] := ScreenSing.AddStatic(Theme.Sing.StaticP1ThreePAvatar);
  ScreenSing.Statics[StaticP1ThreePAvatar[1]].Texture := AvatarPlayerTextures[4];
  ScreenSing.Statics[StaticP1ThreePAvatar[1]].Texture.X  := Theme.Sing.StaticP1ThreePAvatar.X;
  ScreenSing.Statics[StaticP1ThreePAvatar[1]].Texture.Y  := Theme.Sing.StaticP1ThreePAvatar.Y;
  ScreenSing.Statics[StaticP1ThreePAvatar[1]].Texture.H  := Theme.Sing.StaticP1ThreePAvatar.H;
  ScreenSing.Statics[StaticP1ThreePAvatar[1]].Texture.W  := Theme.Sing.StaticP1ThreePAvatar.W;
  ScreenSing.Statics[StaticP1ThreePAvatar[1]].Texture.Z := Theme.Sing.StaticP1ThreePAvatar.Z;
  ScreenSing.Statics[StaticP1ThreePAvatar[1]].Texture.Alpha := Theme.Sing.StaticP1ThreePAvatar.Alpha;

  StaticP2MAvatar[0] := ScreenSing.AddStatic(Theme.Sing.StaticP2MAvatar);
  ScreenSing.Statics[StaticP2MAvatar[0]].Texture := AvatarPlayerTextures[2];
  ScreenSing.Statics[StaticP2MAvatar[0]].Texture.X  := Theme.Sing.StaticP2MAvatar.X;
  ScreenSing.Statics[StaticP2MAvatar[0]].Texture.Y  := Theme.Sing.StaticP2MAvatar.Y;
  ScreenSing.Statics[StaticP2MAvatar[0]].Texture.H  := Theme.Sing.StaticP2MAvatar.H;
  ScreenSing.Statics[StaticP2MAvatar[0]].Texture.W  := Theme.Sing.StaticP2MAvatar.W;
  ScreenSing.Statics[StaticP2MAvatar[0]].Texture.Z := Theme.Sing.StaticP2MAvatar.Z;
  ScreenSing.Statics[StaticP2MAvatar[0]].Texture.Alpha := Theme.Sing.StaticP2MAvatar.Alpha;

  StaticP2MAvatar[1] := ScreenSing.AddStatic(Theme.Sing.StaticP2MAvatar);
  ScreenSing.Statics[StaticP2MAvatar[1]].Texture := AvatarPlayerTextures[5];
  ScreenSing.Statics[StaticP2MAvatar[1]].Texture.X  := Theme.Sing.StaticP2MAvatar.X;
  ScreenSing.Statics[StaticP2MAvatar[1]].Texture.Y  := Theme.Sing.StaticP2MAvatar.Y;
  ScreenSing.Statics[StaticP2MAvatar[1]].Texture.H  := Theme.Sing.StaticP2MAvatar.H;
  ScreenSing.Statics[StaticP2MAvatar[1]].Texture.W  := Theme.Sing.StaticP2MAvatar.W;
  ScreenSing.Statics[StaticP2MAvatar[1]].Texture.Z := Theme.Sing.StaticP2MAvatar.Z;
  ScreenSing.Statics[StaticP2MAvatar[1]].Texture.Alpha := Theme.Sing.StaticP2MAvatar.Alpha;

  StaticP3RAvatar[0] := ScreenSing.AddStatic(Theme.Sing.StaticP3RAvatar);
  ScreenSing.Statics[StaticP3RAvatar[0]].Texture := AvatarPlayerTextures[3];
  ScreenSing.Statics[StaticP3RAvatar[0]].Texture.X  := Theme.Sing.StaticP3RAvatar.X;
  ScreenSing.Statics[StaticP3RAvatar[0]].Texture.Y  := Theme.Sing.StaticP3RAvatar.Y;
  ScreenSing.Statics[StaticP3RAvatar[0]].Texture.H  := Theme.Sing.StaticP3RAvatar.H;
  ScreenSing.Statics[StaticP3RAvatar[0]].Texture.W  := Theme.Sing.StaticP3RAvatar.W;
  ScreenSing.Statics[StaticP3RAvatar[0]].Texture.Z := Theme.Sing.StaticP3RAvatar.Z;
  ScreenSing.Statics[StaticP3RAvatar[0]].Texture.Alpha := Theme.Sing.StaticP3RAvatar.Alpha;

  StaticP3RAvatar[1] := ScreenSing.AddStatic(Theme.Sing.StaticP3RAvatar);
  ScreenSing.Statics[StaticP3RAvatar[1]].Texture := AvatarPlayerTextures[6];
  ScreenSing.Statics[StaticP3RAvatar[1]].Texture.X  := Theme.Sing.StaticP3RAvatar.X;
  ScreenSing.Statics[StaticP3RAvatar[1]].Texture.Y  := Theme.Sing.StaticP3RAvatar.Y;
  ScreenSing.Statics[StaticP3RAvatar[1]].Texture.H  := Theme.Sing.StaticP3RAvatar.H;
  ScreenSing.Statics[StaticP3RAvatar[1]].Texture.W  := Theme.Sing.StaticP3RAvatar.W;
  ScreenSing.Statics[StaticP3RAvatar[1]].Texture.Z := Theme.Sing.StaticP3RAvatar.Z;
  ScreenSing.Statics[StaticP3RAvatar[1]].Texture.Alpha := Theme.Sing.StaticP3RAvatar.Alpha;

  StaticDuetP1ThreePAvatar[0] := ScreenSing.AddStatic(Theme.Sing.StaticDuetP1ThreePAvatar);
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[0]].Texture := AvatarPlayerTextures[1];
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[0]].Texture.X  := Theme.Sing.StaticDuetP1ThreePAvatar.X;
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[0]].Texture.Y  := Theme.Sing.StaticDuetP1ThreePAvatar.Y;
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[0]].Texture.H  := Theme.Sing.StaticDuetP1ThreePAvatar.H;
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[0]].Texture.W  := Theme.Sing.StaticDuetP1ThreePAvatar.W;
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[0]].Texture.Z := Theme.Sing.StaticDuetP1ThreePAvatar.Z;
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[0]].Texture.Alpha := Theme.Sing.StaticDuetP1ThreePAvatar.Alpha;

  StaticDuetP1ThreePAvatar[1] := ScreenSing.AddStatic(Theme.Sing.StaticDuetP1ThreePAvatar);
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[1]].Texture := AvatarPlayerTextures[4];
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[1]].Texture.X  := Theme.Sing.StaticDuetP1ThreePAvatar.X;
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[1]].Texture.Y  := Theme.Sing.StaticDuetP1ThreePAvatar.Y;
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[1]].Texture.H  := Theme.Sing.StaticDuetP1ThreePAvatar.H;
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[1]].Texture.W  := Theme.Sing.StaticDuetP1ThreePAvatar.W;
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[1]].Texture.Z := Theme.Sing.StaticDuetP1ThreePAvatar.Z;
  ScreenSing.Statics[StaticDuetP1ThreePAvatar[1]].Texture.Alpha := Theme.Sing.StaticDuetP1ThreePAvatar.Alpha;

  StaticDuetP2MAvatar[0] := ScreenSing.AddStatic(Theme.Sing.StaticDuetP2MAvatar);
  ScreenSing.Statics[StaticDuetP2MAvatar[0]].Texture := AvatarPlayerTextures[2];
  ScreenSing.Statics[StaticDuetP2MAvatar[0]].Texture.X  := Theme.Sing.StaticDuetP2MAvatar.X;
  ScreenSing.Statics[StaticDuetP2MAvatar[0]].Texture.Y  := Theme.Sing.StaticDuetP2MAvatar.Y;
  ScreenSing.Statics[StaticDuetP2MAvatar[0]].Texture.H  := Theme.Sing.StaticDuetP2MAvatar.H;
  ScreenSing.Statics[StaticDuetP2MAvatar[0]].Texture.W  := Theme.Sing.StaticDuetP2MAvatar.W;
  ScreenSing.Statics[StaticDuetP2MAvatar[0]].Texture.Z := Theme.Sing.StaticDuetP2MAvatar.Z;
  ScreenSing.Statics[StaticDuetP2MAvatar[0]].Texture.Alpha := Theme.Sing.StaticDuetP2MAvatar.Alpha;

  StaticDuetP2MAvatar[1] := ScreenSing.AddStatic(Theme.Sing.StaticDuetP2MAvatar);
  ScreenSing.Statics[StaticDuetP2MAvatar[1]].Texture := AvatarPlayerTextures[5];
  ScreenSing.Statics[StaticDuetP2MAvatar[1]].Texture.X  := Theme.Sing.StaticDuetP2MAvatar.X;
  ScreenSing.Statics[StaticDuetP2MAvatar[1]].Texture.Y  := Theme.Sing.StaticDuetP2MAvatar.Y;
  ScreenSing.Statics[StaticDuetP2MAvatar[1]].Texture.H  := Theme.Sing.StaticDuetP2MAvatar.H;
  ScreenSing.Statics[StaticDuetP2MAvatar[1]].Texture.W  := Theme.Sing.StaticDuetP2MAvatar.W;
  ScreenSing.Statics[StaticDuetP2MAvatar[1]].Texture.Z := Theme.Sing.StaticDuetP2MAvatar.Z;
  ScreenSing.Statics[StaticDuetP2MAvatar[1]].Texture.Alpha := Theme.Sing.StaticDuetP2MAvatar.Alpha;

  StaticDuetP3RAvatar[0] := ScreenSing.AddStatic(Theme.Sing.StaticDuetP3RAvatar);
  ScreenSing.Statics[StaticDuetP3RAvatar[0]].Texture := AvatarPlayerTextures[3];
  ScreenSing.Statics[StaticDuetP3RAvatar[0]].Texture.X  := Theme.Sing.StaticDuetP3RAvatar.X;
  ScreenSing.Statics[StaticDuetP3RAvatar[0]].Texture.Y  := Theme.Sing.StaticDuetP3RAvatar.Y;
  ScreenSing.Statics[StaticDuetP3RAvatar[0]].Texture.H  := Theme.Sing.StaticDuetP3RAvatar.H;
  ScreenSing.Statics[StaticDuetP3RAvatar[0]].Texture.W  := Theme.Sing.StaticDuetP3RAvatar.W;
  ScreenSing.Statics[StaticDuetP3RAvatar[0]].Texture.Z := Theme.Sing.StaticDuetP3RAvatar.Z;
  ScreenSing.Statics[StaticDuetP3RAvatar[0]].Texture.Alpha := Theme.Sing.StaticDuetP3RAvatar.Alpha;

  StaticDuetP3RAvatar[1] := ScreenSing.AddStatic(Theme.Sing.StaticDuetP3RAvatar);
  ScreenSing.Statics[StaticDuetP3RAvatar[1]].Texture := AvatarPlayerTextures[6];
  ScreenSing.Statics[StaticDuetP3RAvatar[1]].Texture.X  := Theme.Sing.StaticDuetP3RAvatar.X;
  ScreenSing.Statics[StaticDuetP3RAvatar[1]].Texture.Y  := Theme.Sing.StaticDuetP3RAvatar.Y;
  ScreenSing.Statics[StaticDuetP3RAvatar[1]].Texture.H  := Theme.Sing.StaticDuetP3RAvatar.H;
  ScreenSing.Statics[StaticDuetP3RAvatar[1]].Texture.W  := Theme.Sing.StaticDuetP3RAvatar.W;
  ScreenSing.Statics[StaticDuetP3RAvatar[1]].Texture.Z := Theme.Sing.StaticDuetP3RAvatar.Z;
  ScreenSing.Statics[StaticDuetP3RAvatar[1]].Texture.Alpha := Theme.Sing.StaticDuetP3RAvatar.Alpha;

  StaticP1FourPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP1FourPAvatar);
  ScreenSing.Statics[StaticP1FourPAvatar].Texture := AvatarPlayerTextures[1];
  ScreenSing.Statics[StaticP1FourPAvatar].Texture.X  := Theme.Sing.StaticP1FourPAvatar.X;
  ScreenSing.Statics[StaticP1FourPAvatar].Texture.Y  := Theme.Sing.StaticP1FourPAvatar.Y;
  ScreenSing.Statics[StaticP1FourPAvatar].Texture.H  := Theme.Sing.StaticP1FourPAvatar.H;
  ScreenSing.Statics[StaticP1FourPAvatar].Texture.W  := Theme.Sing.StaticP1FourPAvatar.W;
  ScreenSing.Statics[StaticP1FourPAvatar].Texture.Z := Theme.Sing.StaticP1FourPAvatar.Z;
  ScreenSing.Statics[StaticP1FourPAvatar].Texture.Alpha := Theme.Sing.StaticP1FourPAvatar.Alpha;

  StaticP2FourPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP2FourPAvatar);
  ScreenSing.Statics[StaticP2FourPAvatar].Texture := AvatarPlayerTextures[2];
  ScreenSing.Statics[StaticP2FourPAvatar].Texture.X  := Theme.Sing.StaticP2FourPAvatar.X;
  ScreenSing.Statics[StaticP2FourPAvatar].Texture.Y  := Theme.Sing.StaticP2FourPAvatar.Y;
  ScreenSing.Statics[StaticP2FourPAvatar].Texture.H  := Theme.Sing.StaticP2FourPAvatar.H;
  ScreenSing.Statics[StaticP2FourPAvatar].Texture.W  := Theme.Sing.StaticP2FourPAvatar.W;
  ScreenSing.Statics[StaticP2FourPAvatar].Texture.Z := Theme.Sing.StaticP2FourPAvatar.Z;
  ScreenSing.Statics[StaticP2FourPAvatar].Texture.Alpha := Theme.Sing.StaticP2FourPAvatar.Alpha;

  StaticP3FourPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP3FourPAvatar);
  ScreenSing.Statics[StaticP3FourPAvatar].Texture := AvatarPlayerTextures[3];
  ScreenSing.Statics[StaticP3FourPAvatar].Texture.X  := Theme.Sing.StaticP3FourPAvatar.X;
  ScreenSing.Statics[StaticP3FourPAvatar].Texture.Y  := Theme.Sing.StaticP3FourPAvatar.Y;
  ScreenSing.Statics[StaticP3FourPAvatar].Texture.H  := Theme.Sing.StaticP3FourPAvatar.H;
  ScreenSing.Statics[StaticP3FourPAvatar].Texture.W  := Theme.Sing.StaticP3FourPAvatar.W;
  ScreenSing.Statics[StaticP3FourPAvatar].Texture.Z := Theme.Sing.StaticP3FourPAvatar.Z;
  ScreenSing.Statics[StaticP3FourPAvatar].Texture.Alpha := Theme.Sing.StaticP3FourPAvatar.Alpha;

  StaticP4FourPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP4FourPAvatar);
  ScreenSing.Statics[StaticP4FourPAvatar].Texture := AvatarPlayerTextures[4];
  ScreenSing.Statics[StaticP4FourPAvatar].Texture.X  := Theme.Sing.StaticP4FourPAvatar.X;
  ScreenSing.Statics[StaticP4FourPAvatar].Texture.Y  := Theme.Sing.StaticP4FourPAvatar.Y;
  ScreenSing.Statics[StaticP4FourPAvatar].Texture.H  := Theme.Sing.StaticP4FourPAvatar.H;
  ScreenSing.Statics[StaticP4FourPAvatar].Texture.W  := Theme.Sing.StaticP4FourPAvatar.W;
  ScreenSing.Statics[StaticP4FourPAvatar].Texture.Z := Theme.Sing.StaticP4FourPAvatar.Z;
  ScreenSing.Statics[StaticP4FourPAvatar].Texture.Alpha := Theme.Sing.StaticP4FourPAvatar.Alpha;

  StaticP1DuetFourPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP1DuetFourPAvatar);
  ScreenSing.Statics[StaticP1DuetFourPAvatar].Texture := AvatarPlayerTextures[1];
  ScreenSing.Statics[StaticP1DuetFourPAvatar].Texture.X  := Theme.Sing.StaticP1DuetFourPAvatar.X;
  ScreenSing.Statics[StaticP1DuetFourPAvatar].Texture.Y  := Theme.Sing.StaticP1DuetFourPAvatar.Y;
  ScreenSing.Statics[StaticP1DuetFourPAvatar].Texture.H  := Theme.Sing.StaticP1DuetFourPAvatar.H;
  ScreenSing.Statics[StaticP1DuetFourPAvatar].Texture.W  := Theme.Sing.StaticP1DuetFourPAvatar.W;
  ScreenSing.Statics[StaticP1DuetFourPAvatar].Texture.Z := Theme.Sing.StaticP1DuetFourPAvatar.Z;
  ScreenSing.Statics[StaticP1DuetFourPAvatar].Texture.Alpha := Theme.Sing.StaticP1DuetFourPAvatar.Alpha;

  StaticP2DuetFourPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP2DuetFourPAvatar);
  ScreenSing.Statics[StaticP2DuetFourPAvatar].Texture := AvatarPlayerTextures[2];
  ScreenSing.Statics[StaticP2DuetFourPAvatar].Texture.X  := Theme.Sing.StaticP2DuetFourPAvatar.X;
  ScreenSing.Statics[StaticP2DuetFourPAvatar].Texture.Y  := Theme.Sing.StaticP2DuetFourPAvatar.Y;
  ScreenSing.Statics[StaticP2DuetFourPAvatar].Texture.H  := Theme.Sing.StaticP2DuetFourPAvatar.H;
  ScreenSing.Statics[StaticP2DuetFourPAvatar].Texture.W  := Theme.Sing.StaticP2DuetFourPAvatar.W;
  ScreenSing.Statics[StaticP2DuetFourPAvatar].Texture.Z := Theme.Sing.StaticP2DuetFourPAvatar.Z;
  ScreenSing.Statics[StaticP2DuetFourPAvatar].Texture.Alpha := Theme.Sing.StaticP2DuetFourPAvatar.Alpha;

  StaticP3DuetFourPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP3DuetFourPAvatar);
  ScreenSing.Statics[StaticP3DuetFourPAvatar].Texture := AvatarPlayerTextures[3];
  ScreenSing.Statics[StaticP3DuetFourPAvatar].Texture.X  := Theme.Sing.StaticP3DuetFourPAvatar.X;
  ScreenSing.Statics[StaticP3DuetFourPAvatar].Texture.Y  := Theme.Sing.StaticP3DuetFourPAvatar.Y;
  ScreenSing.Statics[StaticP3DuetFourPAvatar].Texture.H  := Theme.Sing.StaticP3DuetFourPAvatar.H;
  ScreenSing.Statics[StaticP3DuetFourPAvatar].Texture.W  := Theme.Sing.StaticP3DuetFourPAvatar.W;
  ScreenSing.Statics[StaticP3DuetFourPAvatar].Texture.Z := Theme.Sing.StaticP3DuetFourPAvatar.Z;
  ScreenSing.Statics[StaticP3DuetFourPAvatar].Texture.Alpha := Theme.Sing.StaticP3DuetFourPAvatar.Alpha;

  StaticP4DuetFourPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP4DuetFourPAvatar);
  ScreenSing.Statics[StaticP4DuetFourPAvatar].Texture := AvatarPlayerTextures[4];
  ScreenSing.Statics[StaticP4DuetFourPAvatar].Texture.X  := Theme.Sing.StaticP4DuetFourPAvatar.X;
  ScreenSing.Statics[StaticP4DuetFourPAvatar].Texture.Y  := Theme.Sing.StaticP4DuetFourPAvatar.Y;
  ScreenSing.Statics[StaticP4DuetFourPAvatar].Texture.H  := Theme.Sing.StaticP4DuetFourPAvatar.H;
  ScreenSing.Statics[StaticP4DuetFourPAvatar].Texture.W  := Theme.Sing.StaticP4DuetFourPAvatar.W;
  ScreenSing.Statics[StaticP4DuetFourPAvatar].Texture.Z := Theme.Sing.StaticP4DuetFourPAvatar.Z;
  ScreenSing.Statics[StaticP4DuetFourPAvatar].Texture.Alpha := Theme.Sing.StaticP4DuetFourPAvatar.Alpha;

  StaticP1SixPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP1SixPAvatar);
  ScreenSing.Statics[StaticP1SixPAvatar].Texture := AvatarPlayerTextures[1];
  ScreenSing.Statics[StaticP1SixPAvatar].Texture.X  := Theme.Sing.StaticP1SixPAvatar.X;
  ScreenSing.Statics[StaticP1SixPAvatar].Texture.Y  := Theme.Sing.StaticP1SixPAvatar.Y;
  ScreenSing.Statics[StaticP1SixPAvatar].Texture.H  := Theme.Sing.StaticP1SixPAvatar.H;
  ScreenSing.Statics[StaticP1SixPAvatar].Texture.W  := Theme.Sing.StaticP1SixPAvatar.W;
  ScreenSing.Statics[StaticP1SixPAvatar].Texture.Z := Theme.Sing.StaticP1SixPAvatar.Z;
  ScreenSing.Statics[StaticP1SixPAvatar].Texture.Alpha := Theme.Sing.StaticP1SixPAvatar.Alpha;

  StaticP2SixPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP2SixPAvatar);
  ScreenSing.Statics[StaticP2SixPAvatar].Texture := AvatarPlayerTextures[2];
  ScreenSing.Statics[StaticP2SixPAvatar].Texture.X  := Theme.Sing.StaticP2SixPAvatar.X;
  ScreenSing.Statics[StaticP2SixPAvatar].Texture.Y  := Theme.Sing.StaticP2SixPAvatar.Y;
  ScreenSing.Statics[StaticP2SixPAvatar].Texture.H  := Theme.Sing.StaticP2SixPAvatar.H;
  ScreenSing.Statics[StaticP2SixPAvatar].Texture.W  := Theme.Sing.StaticP2SixPAvatar.W;
  ScreenSing.Statics[StaticP2SixPAvatar].Texture.Z := Theme.Sing.StaticP2SixPAvatar.Z;
  ScreenSing.Statics[StaticP2SixPAvatar].Texture.Alpha := Theme.Sing.StaticP2SixPAvatar.Alpha;

  StaticP3SixPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP3SixPAvatar);
  ScreenSing.Statics[StaticP3SixPAvatar].Texture := AvatarPlayerTextures[3];
  ScreenSing.Statics[StaticP3SixPAvatar].Texture.X  := Theme.Sing.StaticP3SixPAvatar.X;
  ScreenSing.Statics[StaticP3SixPAvatar].Texture.Y  := Theme.Sing.StaticP3SixPAvatar.Y;
  ScreenSing.Statics[StaticP3SixPAvatar].Texture.H  := Theme.Sing.StaticP3SixPAvatar.H;
  ScreenSing.Statics[StaticP3SixPAvatar].Texture.W  := Theme.Sing.StaticP3SixPAvatar.W;
  ScreenSing.Statics[StaticP3SixPAvatar].Texture.Z := Theme.Sing.StaticP3SixPAvatar.Z;
  ScreenSing.Statics[StaticP3SixPAvatar].Texture.Alpha := Theme.Sing.StaticP3SixPAvatar.Alpha;

  StaticP4SixPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP4SixPAvatar);
  ScreenSing.Statics[StaticP4SixPAvatar].Texture := AvatarPlayerTextures[4];
  ScreenSing.Statics[StaticP4SixPAvatar].Texture.X  := Theme.Sing.StaticP4SixPAvatar.X;
  ScreenSing.Statics[StaticP4SixPAvatar].Texture.Y  := Theme.Sing.StaticP4SixPAvatar.Y;
  ScreenSing.Statics[StaticP4SixPAvatar].Texture.H  := Theme.Sing.StaticP4SixPAvatar.H;
  ScreenSing.Statics[StaticP4SixPAvatar].Texture.W  := Theme.Sing.StaticP4SixPAvatar.W;
  ScreenSing.Statics[StaticP4SixPAvatar].Texture.Z := Theme.Sing.StaticP4SixPAvatar.Z;
  ScreenSing.Statics[StaticP4SixPAvatar].Texture.Alpha := Theme.Sing.StaticP4SixPAvatar.Alpha;

  StaticP5SixPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP5SixPAvatar);
  ScreenSing.Statics[StaticP5SixPAvatar].Texture := AvatarPlayerTextures[5];
  ScreenSing.Statics[StaticP5SixPAvatar].Texture.X  := Theme.Sing.StaticP5SixPAvatar.X;
  ScreenSing.Statics[StaticP5SixPAvatar].Texture.Y  := Theme.Sing.StaticP5SixPAvatar.Y;
  ScreenSing.Statics[StaticP5SixPAvatar].Texture.H  := Theme.Sing.StaticP5SixPAvatar.H;
  ScreenSing.Statics[StaticP5SixPAvatar].Texture.W  := Theme.Sing.StaticP5SixPAvatar.W;
  ScreenSing.Statics[StaticP5SixPAvatar].Texture.Z := Theme.Sing.StaticP5SixPAvatar.Z;
  ScreenSing.Statics[StaticP5SixPAvatar].Texture.Alpha := Theme.Sing.StaticP5SixPAvatar.Alpha;

  StaticP6SixPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP6SixPAvatar);
  ScreenSing.Statics[StaticP6SixPAvatar].Texture := AvatarPlayerTextures[6];
  ScreenSing.Statics[StaticP6SixPAvatar].Texture.X  := Theme.Sing.StaticP6SixPAvatar.X;
  ScreenSing.Statics[StaticP6SixPAvatar].Texture.Y  := Theme.Sing.StaticP6SixPAvatar.Y;
  ScreenSing.Statics[StaticP6SixPAvatar].Texture.H  := Theme.Sing.StaticP6SixPAvatar.H;
  ScreenSing.Statics[StaticP6SixPAvatar].Texture.W  := Theme.Sing.StaticP6SixPAvatar.W;
  ScreenSing.Statics[StaticP6SixPAvatar].Texture.Z := Theme.Sing.StaticP6SixPAvatar.Z;
  ScreenSing.Statics[StaticP6SixPAvatar].Texture.Alpha := Theme.Sing.StaticP6SixPAvatar.Alpha;

  StaticP1DuetSixPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP1DuetSixPAvatar);
  ScreenSing.Statics[StaticP1DuetSixPAvatar].Texture := AvatarPlayerTextures[1];
  ScreenSing.Statics[StaticP1DuetSixPAvatar].Texture.X  := Theme.Sing.StaticP1DuetSixPAvatar.X;
  ScreenSing.Statics[StaticP1DuetSixPAvatar].Texture.Y  := Theme.Sing.StaticP1DuetSixPAvatar.Y;
  ScreenSing.Statics[StaticP1DuetSixPAvatar].Texture.H  := Theme.Sing.StaticP1DuetSixPAvatar.H;
  ScreenSing.Statics[StaticP1DuetSixPAvatar].Texture.W  := Theme.Sing.StaticP1DuetSixPAvatar.W;
  ScreenSing.Statics[StaticP1DuetSixPAvatar].Texture.Z := Theme.Sing.StaticP1DuetSixPAvatar.Z;
  ScreenSing.Statics[StaticP1DuetSixPAvatar].Texture.Alpha := Theme.Sing.StaticP1DuetSixPAvatar.Alpha;

  StaticP2DuetSixPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP2DuetSixPAvatar);
  ScreenSing.Statics[StaticP2DuetSixPAvatar].Texture := AvatarPlayerTextures[2];
  ScreenSing.Statics[StaticP2DuetSixPAvatar].Texture.X  := Theme.Sing.StaticP2DuetSixPAvatar.X;
  ScreenSing.Statics[StaticP2DuetSixPAvatar].Texture.Y  := Theme.Sing.StaticP2DuetSixPAvatar.Y;
  ScreenSing.Statics[StaticP2DuetSixPAvatar].Texture.H  := Theme.Sing.StaticP2DuetSixPAvatar.H;
  ScreenSing.Statics[StaticP2DuetSixPAvatar].Texture.W  := Theme.Sing.StaticP2DuetSixPAvatar.W;
  ScreenSing.Statics[StaticP2DuetSixPAvatar].Texture.Z := Theme.Sing.StaticP2DuetSixPAvatar.Z;
  ScreenSing.Statics[StaticP2DuetSixPAvatar].Texture.Alpha := Theme.Sing.StaticP2DuetSixPAvatar.Alpha;

  StaticP3DuetSixPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP3DuetSixPAvatar);
  ScreenSing.Statics[StaticP3DuetSixPAvatar].Texture := AvatarPlayerTextures[3];
  ScreenSing.Statics[StaticP3DuetSixPAvatar].Texture.X  := Theme.Sing.StaticP3DuetSixPAvatar.X;
  ScreenSing.Statics[StaticP3DuetSixPAvatar].Texture.Y  := Theme.Sing.StaticP3DuetSixPAvatar.Y;
  ScreenSing.Statics[StaticP3DuetSixPAvatar].Texture.H  := Theme.Sing.StaticP3DuetSixPAvatar.H;
  ScreenSing.Statics[StaticP3DuetSixPAvatar].Texture.W  := Theme.Sing.StaticP3DuetSixPAvatar.W;
  ScreenSing.Statics[StaticP3DuetSixPAvatar].Texture.Z := Theme.Sing.StaticP3DuetSixPAvatar.Z;
  ScreenSing.Statics[StaticP3DuetSixPAvatar].Texture.Alpha := Theme.Sing.StaticP3DuetSixPAvatar.Alpha;

  StaticP4DuetSixPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP4DuetSixPAvatar);
  ScreenSing.Statics[StaticP4DuetSixPAvatar].Texture := AvatarPlayerTextures[4];
  ScreenSing.Statics[StaticP4DuetSixPAvatar].Texture.X  := Theme.Sing.StaticP4DuetSixPAvatar.X;
  ScreenSing.Statics[StaticP4DuetSixPAvatar].Texture.Y  := Theme.Sing.StaticP4DuetSixPAvatar.Y;
  ScreenSing.Statics[StaticP4DuetSixPAvatar].Texture.H  := Theme.Sing.StaticP4DuetSixPAvatar.H;
  ScreenSing.Statics[StaticP4DuetSixPAvatar].Texture.W  := Theme.Sing.StaticP4DuetSixPAvatar.W;
  ScreenSing.Statics[StaticP4DuetSixPAvatar].Texture.Z := Theme.Sing.StaticP4DuetSixPAvatar.Z;
  ScreenSing.Statics[StaticP4DuetSixPAvatar].Texture.Alpha := Theme.Sing.StaticP4DuetSixPAvatar.Alpha;

  StaticP5DuetSixPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP5DuetSixPAvatar);
  ScreenSing.Statics[StaticP5DuetSixPAvatar].Texture := AvatarPlayerTextures[5];
  ScreenSing.Statics[StaticP5DuetSixPAvatar].Texture.X  := Theme.Sing.StaticP5DuetSixPAvatar.X;
  ScreenSing.Statics[StaticP5DuetSixPAvatar].Texture.Y  := Theme.Sing.StaticP5DuetSixPAvatar.Y;
  ScreenSing.Statics[StaticP5DuetSixPAvatar].Texture.H  := Theme.Sing.StaticP5DuetSixPAvatar.H;
  ScreenSing.Statics[StaticP5DuetSixPAvatar].Texture.W  := Theme.Sing.StaticP5DuetSixPAvatar.W;
  ScreenSing.Statics[StaticP5DuetSixPAvatar].Texture.Z := Theme.Sing.StaticP5DuetSixPAvatar.Z;
  ScreenSing.Statics[StaticP5DuetSixPAvatar].Texture.Alpha := Theme.Sing.StaticP5DuetSixPAvatar.Alpha;

  StaticP6DuetSixPAvatar := ScreenSing.AddStatic(Theme.Sing.StaticP6DuetSixPAvatar);
  ScreenSing.Statics[StaticP6DuetSixPAvatar].Texture := AvatarPlayerTextures[6];
  ScreenSing.Statics[StaticP6DuetSixPAvatar].Texture.X  := Theme.Sing.StaticP6DuetSixPAvatar.X;
  ScreenSing.Statics[StaticP6DuetSixPAvatar].Texture.Y  := Theme.Sing.StaticP6DuetSixPAvatar.Y;
  ScreenSing.Statics[StaticP6DuetSixPAvatar].Texture.H  := Theme.Sing.StaticP6DuetSixPAvatar.H;
  ScreenSing.Statics[StaticP6DuetSixPAvatar].Texture.W  := Theme.Sing.StaticP6DuetSixPAvatar.W;
  ScreenSing.Statics[StaticP6DuetSixPAvatar].Texture.Z := Theme.Sing.StaticP6DuetSixPAvatar.Z;
  ScreenSing.Statics[StaticP6DuetSixPAvatar].Texture.Alpha := Theme.Sing.StaticP6DuetSixPAvatar.Alpha;

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
  if not(CurrentSong.isDuet) then
  begin
    Line := ScreenSing.Lyrics.GetUpperLine();
    if Line.LastLine then
    begin
      LastWord := Line.Words[Length(Line.Words)-1];
      if CurLyricsTime >= GetTimeFromBeat(LastWord.Start + LastWord.Length) then
        ScreenSing.SungToEnd := true;
    end;
  end
  else
  begin
  {  Line := Lyrics.GetUpperLine();
    LineDuet := LyricsDuet.GetUpperLine();
    if Line.LastLine and (LineDuet.LastLine) then
    begin
      LastWord := Line.Words[Length(Line.Words)-1];
      LastWordDuet := LineDuet.Words[Length(Line.Words)-1];
      if (CurLyricsTime >= GetTimeFromBeat(LastWord.Start+LastWord.Length)) and (CurLyricsTime >= GetTimeFromBeat(LastWordDuet.Start+LastWordDuet.Length)) then
        // TODO SAVE DUET SCORES
        SungToEnd := false;
        //SungToEnd := true;
    end;
    }
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

  // draw notes lines
  if (ScreenSing.Settings.NotesVisible <> 0) or (ScreenSing.Settings.InputVisible) then
    SingDrawLines;

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
  if (ScreenSong.Mode <> smMedley) and (ScreenSing.Settings.TimeBarVisible) then
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
  SongStart := CurrentSong.BPM[0].BPM*CurrentSong.Start/60;
  SongEnd := CurrentSong.BPM[0].BPM*TotalTime/60;
  SongDuration := SongEnd - SongStart;
  gapInBeats := CurrentSong.BPM[0].BPM*CurrentSong.GAP/1000/60;

  // draw sentence boxes
  for CurrentTrack := 0 to High(Tracks) do //for P1 of duet or solo lyrics, P2 of duet,..
  begin
    numLines := Length(Tracks[CurrentTrack].Lines); //Lyric lines
    if (numLines < 2) then //catch cases which could cause endless loop
      Exit;
    //set color to player.color
    if (CurrentTrack = 0) then
      glColor4f(GetLyricColor(Ini.SingColor[0]).R, GetLyricColor(Ini.SingColor[0]).G, GetLyricColor(Ini.SingColor[0]).B, 0.8)
    else
      glColor4f(GetLyricColor(Ini.SingColor[CurrentTrack]).R, GetLyricColor(Ini.SingColor[CurrentTrack]).G, GetLyricColor(Ini.SingColor[CurrentTrack]).B, 0.4);

    glbegin(gl_quads);
    for LineIndex := 0 to numLines - 1 do
    begin
      if (Tracks[CurrentTrack].Lines[LineIndex].Notes = nil) then Continue;
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

