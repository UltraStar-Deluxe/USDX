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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/screens/UScreenScore.pas $
 * $Id: UScreenScore.pas 2246 2010-04-18 13:43:36Z tobigun $
 *}

unit UScreenScore;

interface
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$I switches.inc}

uses
  UCommon,
  UDataBase,
  UDisplay,
  UDLLManager,
  UIni,
  UMenu,
  UMusic,
  USongs,
  UTexture,
  UThemes,
  UScale,
  UWebSDK,
  dglOpenGL,
  math,
  sdl2,
  SysUtils;

const
  ZBars:            real = 0.8;   // Z value for the bars
  ZRatingPic:       real = 0.8;   // Z value for the rating pictures

  EaseOut_MaxSteps: real = 10;    // that's the speed of the bars (10 is fast | 100 is slower)

  BarRaiseSpeed:    cardinal = 14; // Time for raising the bar one step higher (in ms)

type
  TScoreBarType = (sbtScore, sbtLine, sbtGolden);
  TPlayerScoreScreenTexture = record            // holds all colorized textures for one player
    //Bar textures
    Score_NoteBarLevel_Dark:     TTexture;      // Note
    Score_NoteBarRound_Dark:     TTexture;      // that's the round thing on top

    Score_NoteBarLevel_Light:    TTexture;      // LineBonus | Phrasebonus
    Score_NoteBarRound_Light:    TTexture;

    Score_NoteBarLevel_Lightest: TTexture;      // GoldenNotes
    Score_NoteBarRound_Lightest: TTexture;
  end;

  TPlayerScoreScreenData = record               // holds the positions and other data
    Bar_Y:                  real;
    Bar_Actual_Height:      real;               // this one holds the actual height of the bar, while we animate it
    BarScore_ActualHeight:  real;
    BarLine_ActualHeight:   real;
    BarGolden_ActualHeight: real;
  end;

  TPlayerScoreRatingPics = record               // a fine array of the rating pictures
    RateEaseStep:  integer;
    RateEaseValue: real;
  end;

  { hold maps of players to the different positions }
  TPlayerPositionMap = record
    Position: byte; // local slot index on the assigned screen; 0 if unused
    Screen: byte;   // 0 - Screen 1; 1 - Screen 2
  end;
  APlayerPositionMap = array of TPlayerPositionMap;

  { textures for playerstatics of seconds screen players }
  TPlayerStaticTexture = record
    Tex: TTexture;
  end;

  TScreenScore = class(TMenu)
    private
      { holds position and screen of players(index)
        set by calling MapPlayerstoPosition() }
      PlayerPositionMap: APlayerPositionMap;

      BarTime:            cardinal;
      FinishScreenDraw:   boolean;

      aPlayerScoreScreenTextures: array[1..UIni.IMaxPlayerCount] of TPlayerScoreScreenTexture;
      aPlayerScoreScreenDatas:    array[1..UIni.IMaxPlayerCount] of TPlayerScoreScreenData;
      aPlayerScoreScreenRatings:  array[1..UIni.IMaxPlayerCount] of TPlayerScoreRatingPics;

      BarScore_EaseOut_Step:  real;
      BarPhrase_EaseOut_Step: real;
      BarGolden_EaseOut_Step: real;

      TextArtist:             integer;
      TextTitle:              integer;

      TextArtistTitle:        integer;

      TextName:             array[1..UIni.IMaxPlayerCount] of integer;
      TextScore:            array[1..UIni.IMaxPlayerCount] of integer;

      TextNotes:            array[1..UIni.IMaxPlayerCount] of integer;
      TextNotesScore:       array[1..UIni.IMaxPlayerCount] of integer;
      TextLineBonus:        array[1..UIni.IMaxPlayerCount] of integer;
      TextLineBonusScore:   array[1..UIni.IMaxPlayerCount] of integer;
      TextGoldenNotes:      array[1..UIni.IMaxPlayerCount] of integer;
      TextGoldenNotesScore: array[1..UIni.IMaxPlayerCount] of integer;
      TextTotal:            array[1..UIni.IMaxPlayerCount] of integer;
      TextTotalScore:       array[1..UIni.IMaxPlayerCount] of integer;

      PlayerStatic:         array[1..UIni.IMaxPlayerCount] of array of integer;
      AvatarStaticRef:      array[1..UIni.IMaxPlayerCount] of Integer;
      { texture pairs for swapping when screens = 2
        first array level: slot index on a screen
        second array level: static index within that slot
        third array level: precolored texture for the target player }
      PlayerStaticTextures: array[1..UIni.IMaxPlayerCount] of array of array [1..UIni.IMaxPlayerCount] of TPlayerStaticTexture;
      PlayerTexts:          array[1..UIni.IMaxPlayerCount] of array of integer;

      StaticBoxLightest:    array[1..UIni.IMaxPlayerCount] of integer;
      StaticBoxLight:       array[1..UIni.IMaxPlayerCount] of integer;
      StaticBoxDark:        array[1..UIni.IMaxPlayerCount] of integer;
      { texture pairs for swapping when screens = 2
        first array level: slot index on a screen
        second array level: box variant (0: lightest; 1: light; 2: dark)
        third array level: precolored texture for the target player }
      PlayerBoxTextures: array[1..UIni.IMaxPlayerCount] of array[0..2] of array [1..UIni.IMaxPlayerCount] of TPlayerStaticTexture;

      StaticBackLevel:      array[1..UIni.IMaxPlayerCount] of integer;
      StaticBackLevelRound: array[1..UIni.IMaxPlayerCount] of integer;
      StaticLevel:          array[1..UIni.IMaxPlayerCount] of integer;
      StaticLevelRound:     array[1..UIni.IMaxPlayerCount] of integer;

      Voice:                integer;

      TextScore_ActualValue:  array[1..UIni.IMaxPlayerCount] of integer;
      TextPhrase_ActualValue: array[1..UIni.IMaxPlayerCount] of integer;
      TextGolden_ActualValue: array[1..UIni.IMaxPlayerCount] of integer;

      ButtonSend: array[1..3] of integer;
      CurrentRound:          integer;
      StaticNavigate:       integer;
      TextNavigate:         integer;

      procedure RefreshTexts;
      procedure ResetScores;

      procedure StartPreview;
      procedure StartVoice;

      procedure MapPlayersToPosition;

      procedure FillPlayerItems(PlayerNumber: integer);

      procedure UpdateAnimation;
      {****
       * helpers for bar easing
       *}
      procedure EaseBarIn(PlayerNumber: integer; BarType: TScoreBarType);
      procedure EaseScoreIn(PlayerNumber: integer; ScoreType: TScoreBarType);

      procedure DrawPlayerBars;

      procedure DrawBar(BarType: TScoreBarType; PlayerNumber: integer; BarStartPosY: single; NewHeight: real);

      {****
       * helpers for rating picture
       *}
      procedure ShowRating(PlayerNumber: integer);
      function  CalculateBouncing(PlayerNumber: integer): real;
      procedure DrawRating(PlayerNumber: integer; Rating: integer);

      { for player static texture swapping }
      procedure LoadSwapTextures;
      procedure SwapToScreen(Screen: integer);
    public
      constructor Create; override;
      destructor Destroy; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean; override;
      function ParseMouse(MouseButton: Integer; BtnDown: Boolean; X, Y: integer): boolean; override;
      procedure OnShow; override;
      procedure OnShowFinish; override;
      function Draw: boolean; override;
  end;

const
  ID='ID_023';   //for help system

implementation

uses
  UAvatars,
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UMenuStatic,
  UNote,
  UPlayerLayout,
  UPathUtils,
  UScreenPopup,
  UScreenSong,
  USkins,
  USong,
  UTime,
  UUnicodeUtils;

function GetScoreSlotIndex(PlayerIndex, PlayerCount, ScreenCount: integer): integer;
begin
  Result := GetPlayerIndexOnScreen(PlayerIndex, PlayerCount, ScreenCount) + 1;
end;

function ReplaceBasePlayerColor(const Color: string; TargetPlayer: integer): string;
var
  SourcePrefix: string;
begin
  Result := Color;
  SourcePrefix := 'P1';
  if Copy(Color, 1, Length(SourcePrefix)) = SourcePrefix then
    Result := 'P' + IntToStr(TargetPlayer) + Copy(Color, Length(SourcePrefix) + 1, MaxInt);
end;

function GetScorePlayerThemeColor(TargetPlayer: integer; const ThemeColor: string; out Col: TRGB): boolean;
var
  BaseColor: integer;
begin
  Result := false;
  if (TargetPlayer < 1) or (TargetPlayer > Length(Ini.PlayerColor)) then
    Exit;

  if Pos('P1', ThemeColor) <> 1 then
    Exit;

  BaseColor := Ini.PlayerColor[TargetPlayer - 1];
  if Pos('Lightest', ThemeColor) = 3 then
    Col := ColorSqrt(GetPlayerLightColor(BaseColor))
  else if Pos('Light', ThemeColor) = 3 then
    Col := GetPlayerLightColor(BaseColor)
  else
    Col := GetPlayerColor(BaseColor);

  Result := true;
end;

function ResolvePlayerThemeColor(const ThemeColor: string; TargetPlayer: integer; out Col: TRGB): boolean;
var
  ResolvedColor: string;
  R: real;
  G: real;
  B: real;
begin
  Result := false;
  if ThemeColor = '' then
    Exit;

  if GetScorePlayerThemeColor(TargetPlayer, ThemeColor, Col) then
    Exit(true);

  ResolvedColor := ReplaceBasePlayerColor(ThemeColor, TargetPlayer);
  if ResolvedColor <> ThemeColor then
  begin
    LoadColor(R, G, B, ResolvedColor);
    Col.R := R;
    Col.G := G;
    Col.B := B;
    Result := true;
  end;
end;

procedure ApplyThemeStaticForPlayer(const ScreenScore: TScreenScore; StaticIndex: integer;
  const ThemeStatic: TThemeStatic; TargetPlayer: integer);
var
  Col: TRGB;
  CurrentTexture: TTexture;
begin
  CurrentTexture := ScreenScore.Statics[StaticIndex].Texture;
  if ResolvePlayerThemeColor(ThemeStatic.Color, TargetPlayer, Col) then
  begin
    if ThemeStatic.Typ = Texture_Type_Colorized then
    begin
      ScreenScore.Statics[StaticIndex].Texture :=
        Texture.GetTexture(Skin.GetTextureFileName(ThemeStatic.Tex), ThemeStatic.Typ,
          RGBFloatToInt(Col.R, Col.G, Col.B));
      ScreenScore.Statics[StaticIndex].Texture.X := CurrentTexture.X;
      ScreenScore.Statics[StaticIndex].Texture.Y := CurrentTexture.Y;
      ScreenScore.Statics[StaticIndex].Texture.W := CurrentTexture.W;
      ScreenScore.Statics[StaticIndex].Texture.H := CurrentTexture.H;
      ScreenScore.Statics[StaticIndex].Texture.Z := CurrentTexture.Z;
      ScreenScore.Statics[StaticIndex].Texture.Alpha := CurrentTexture.Alpha;
    end
    else
    begin
      ScreenScore.Statics[StaticIndex].Texture.ColR := Col.R;
      ScreenScore.Statics[StaticIndex].Texture.ColG := Col.G;
      ScreenScore.Statics[StaticIndex].Texture.ColB := Col.B;
    end;
  end;
end;

function GetScoreButtonLayout(PlayerCount, ScreenCount: integer): integer;
begin
  Result := EnsureRange(GetScorePlayerGrid(PlayerCount,
    Theme.Score.PlayerLayoutArea.W, Theme.Score.PlayerLayoutArea.H,
    Theme.Score.PlayerLayoutExtraColsBias).Cols, 1, 3);
end;

procedure SetScoreSlotScoreAlpha(const ScreenScore: TScreenScore; SlotIndex: integer; Alpha: real);
begin
  ScreenScore.Text[ScreenScore.TextScore[SlotIndex]].Alpha := Alpha;
  ScreenScore.Text[ScreenScore.TextNotes[SlotIndex]].Alpha := Alpha;
  ScreenScore.Text[ScreenScore.TextNotesScore[SlotIndex]].Alpha := Alpha;
  ScreenScore.Text[ScreenScore.TextLineBonus[SlotIndex]].Alpha := Alpha;
  ScreenScore.Text[ScreenScore.TextLineBonusScore[SlotIndex]].Alpha := Alpha;
  ScreenScore.Text[ScreenScore.TextGoldenNotes[SlotIndex]].Alpha := Alpha;
  ScreenScore.Text[ScreenScore.TextGoldenNotesScore[SlotIndex]].Alpha := Alpha;
  ScreenScore.Text[ScreenScore.TextTotal[SlotIndex]].Alpha := Alpha;
  ScreenScore.Text[ScreenScore.TextTotalScore[SlotIndex]].Alpha := Alpha;
  ScreenScore.Statics[ScreenScore.StaticBoxLightest[SlotIndex]].Texture.Alpha := Alpha;
  ScreenScore.Statics[ScreenScore.StaticBoxLight[SlotIndex]].Texture.Alpha := Alpha;
  ScreenScore.Statics[ScreenScore.StaticBoxDark[SlotIndex]].Texture.Alpha := Alpha;
end;

procedure SetScoreSlotVisible(const ScreenScore: TScreenScore; SlotIndex: integer; Visible: boolean);
var
  I: integer;
begin
  ScreenScore.Text[ScreenScore.TextName[SlotIndex]].Visible := Visible;
  ScreenScore.Text[ScreenScore.TextScore[SlotIndex]].Visible := Visible;
  ScreenScore.Text[ScreenScore.TextNotes[SlotIndex]].Visible := Visible;
  ScreenScore.Text[ScreenScore.TextNotesScore[SlotIndex]].Visible := Visible;
  ScreenScore.Text[ScreenScore.TextLineBonus[SlotIndex]].Visible := Visible;
  ScreenScore.Text[ScreenScore.TextLineBonusScore[SlotIndex]].Visible := Visible;
  ScreenScore.Text[ScreenScore.TextGoldenNotes[SlotIndex]].Visible := Visible;
  ScreenScore.Text[ScreenScore.TextGoldenNotesScore[SlotIndex]].Visible := Visible;
  ScreenScore.Text[ScreenScore.TextTotal[SlotIndex]].Visible := Visible;
  ScreenScore.Text[ScreenScore.TextTotalScore[SlotIndex]].Visible := Visible;

  for I := 0 to High(ScreenScore.PlayerStatic[SlotIndex]) do
    ScreenScore.Statics[ScreenScore.PlayerStatic[SlotIndex, I]].Visible := Visible;

  for I := 0 to High(ScreenScore.PlayerTexts[SlotIndex]) do
    ScreenScore.Text[ScreenScore.PlayerTexts[SlotIndex, I]].Visible := Visible;

  ScreenScore.Statics[ScreenScore.StaticBoxLightest[SlotIndex]].Visible := Visible;
  ScreenScore.Statics[ScreenScore.StaticBoxLight[SlotIndex]].Visible := Visible;
  ScreenScore.Statics[ScreenScore.StaticBoxDark[SlotIndex]].Visible := Visible;
end;

procedure ApplyScoreSlotTextColors(const ScreenScore: TScreenScore; SlotIndex, TargetPlayer: integer);
var
  I: integer;
  procedure ApplyTextColor(TextIndex: integer; const ThemeColor: string);
  var
    Col: TRGB;
  begin
    if ResolvePlayerThemeColor(ThemeColor, TargetPlayer, Col) then
    begin
      ScreenScore.Text[TextIndex].ColR := Col.R;
      ScreenScore.Text[TextIndex].ColG := Col.G;
      ScreenScore.Text[TextIndex].ColB := Col.B;
    end;
  end;
begin
  for I := 0 to High(ScreenScore.PlayerTexts[SlotIndex]) do
    ApplyTextColor(ScreenScore.PlayerTexts[SlotIndex, I], Theme.Score.PlayerTexts[SlotIndex, I].Color);

  ApplyTextColor(ScreenScore.TextName[SlotIndex], Theme.Score.TextName[SlotIndex].Color);
  ApplyTextColor(ScreenScore.TextScore[SlotIndex], Theme.Score.TextScore[SlotIndex].Color);
  ApplyTextColor(ScreenScore.TextNotes[SlotIndex], Theme.Score.TextNotes[SlotIndex].Color);
  ApplyTextColor(ScreenScore.TextNotesScore[SlotIndex], Theme.Score.TextNotesScore[SlotIndex].Color);
  ApplyTextColor(ScreenScore.TextLineBonus[SlotIndex], Theme.Score.TextLineBonus[SlotIndex].Color);
  ApplyTextColor(ScreenScore.TextLineBonusScore[SlotIndex], Theme.Score.TextLineBonusScore[SlotIndex].Color);
  ApplyTextColor(ScreenScore.TextGoldenNotes[SlotIndex], Theme.Score.TextGoldenNotes[SlotIndex].Color);
  ApplyTextColor(ScreenScore.TextGoldenNotesScore[SlotIndex], Theme.Score.TextGoldenNotesScore[SlotIndex].Color);
  ApplyTextColor(ScreenScore.TextTotal[SlotIndex], Theme.Score.TextTotal[SlotIndex].Color);
  ApplyTextColor(ScreenScore.TextTotalScore[SlotIndex], Theme.Score.TextTotalScore[SlotIndex].Color);
end;

procedure ApplyScoreSlotPlayerVisuals(const ScreenScore: TScreenScore; SlotIndex, TargetPlayer: integer);
var
  I: integer;
begin
  ApplyScoreSlotTextColors(ScreenScore, SlotIndex, TargetPlayer);

  for I := 0 to High(ScreenScore.PlayerStatic[SlotIndex]) do
    ApplyThemeStaticForPlayer(ScreenScore, ScreenScore.PlayerStatic[SlotIndex, I],
      Theme.Score.PlayerStatic[SlotIndex, I], TargetPlayer);

  ApplyThemeStaticForPlayer(ScreenScore, ScreenScore.StaticBoxLightest[SlotIndex],
    Theme.Score.StaticBoxLightest[SlotIndex], TargetPlayer);
  ApplyThemeStaticForPlayer(ScreenScore, ScreenScore.StaticBoxLight[SlotIndex],
    Theme.Score.StaticBoxLight[SlotIndex], TargetPlayer);
  ApplyThemeStaticForPlayer(ScreenScore, ScreenScore.StaticBoxDark[SlotIndex],
    Theme.Score.StaticBoxDark[SlotIndex], TargetPlayer);
  ApplyThemeStaticForPlayer(ScreenScore, ScreenScore.StaticBackLevel[SlotIndex],
    Theme.Score.StaticBackLevel[SlotIndex], TargetPlayer);
  ApplyThemeStaticForPlayer(ScreenScore, ScreenScore.StaticBackLevelRound[SlotIndex],
    Theme.Score.StaticBackLevelRound[SlotIndex], TargetPlayer);
  ApplyThemeStaticForPlayer(ScreenScore, ScreenScore.StaticLevel[SlotIndex],
    Theme.Score.StaticLevel[SlotIndex], TargetPlayer);
  ApplyThemeStaticForPlayer(ScreenScore, ScreenScore.StaticLevelRound[SlotIndex],
    Theme.Score.StaticLevelRound[SlotIndex], TargetPlayer);
  ApplyThemeStaticForPlayer(ScreenScore, ScreenScore.AvatarStaticRef[TargetPlayer],
    Theme.Score.AvatarStatic[SlotIndex], TargetPlayer);
end;

{
 *****************************
 ** 100: NOT A VALID DLL :p
 ** 0: ERROR_CONNECT -> WEBSITE_NO_CONNECTION
 ** 1: OK_LOGIN
 ** 2: ERROR_LOGIN -> WEBSITE_LOGIN_ERROR
 ** 3: OK_SCORE -> WEBSITE_OK_SEND
 ** 4: ERROR_SCORE -> WEBSITE_ERROR_SCORE
 ** 5: ERROR_SCORE_DUPLICATED
 ** 6: OK_SONG
 ** 7: ERROR_SONG
 *****************************
}

procedure SendScore(SendInfo: TSendInfo; Website: integer);
var
  LoginInfo: TLoginInfo;
  SendStatus: integer;
  EncryptPassword: widestring;
begin

  // Encrypt Password (with username and password)
  LoginInfo.Username := SendInfo.Username;
  LoginInfo.Password := SendInfo.Password;

  DllMan.LoadWebsite(Website);

  if (ScreenPopUpSendScore.SelectValueU = High(ScreenPopUpSendScore.IUsername)) then
  begin
    EncryptPassword := DllMan.WebsiteEncryptPassword(LoginInfo);
    SendInfo.Password := EncryptPassword;
  end;

  SendStatus := DllMan.WebsiteSendScore(SendInfo);
  
  case SendStatus of
    0: ScreenPopupError.ShowPopup(Language.Translate('WEBSITE_NO_CONNECTION'));
    2: ScreenPopupError.ShowPopup(Language.Translate('WEBSITE_LOGIN_ERROR'));
    3: ScreenPopupInfo.ShowPopup(Language.Translate('WEBSITE_OK_SEND'));
    4: ScreenPopupError.ShowPopup(Language.Translate('WEBSITE_ERROR_SCORE'));
    5: ScreenPopupError.ShowPopup(Language.Translate('WEBSITE_ERROR_SCORE_DUPLICATED'));
    7: ScreenPopupError.ShowPopup(Language.Translate('WEBSITE_ERROR_SONG'));
  end;

  ScreenScore.Interaction := -1;
end;

procedure SaveScore(SendInfo: TSendInfo; Website: integer);
var
  LoginInfo: TLoginInfo;
  EncryptPassword: widestring;
  ScoreFile: TextFile;
  EncryptText: string;
  WebName: UTF8String;
begin
  // Encrypt Password (with username and password)
  LoginInfo.Username := SendInfo.Username;
  LoginInfo.Password := SendInfo.Password;

  DllMan.LoadWebsite(Website);

  if (ScreenPopUpSendScore.SelectValueU = High(ScreenPopUpSendScore.IUsername)) then
  begin
    EncryptPassword := DllMan.WebsiteEncryptPassword(LoginInfo);
    SendInfo.Password := EncryptPassword;
  end;

  WebName := UDataBase.DataBase.NetworkUser[Website].Website;

  EncryptText := DllMan.WebsiteEncryptScore(SendInfo);

  AssignFile(ScoreFile, WebScoresPath.Append(WebName + ' [Save Scores].txt').ToNative);

  if FileExists(WebScoresPath.Append(WebName + ' [Save Scores].txt').ToNative) then
    Append(ScoreFile)
  else
    Rewrite(ScoreFile);

  WriteLn(ScoreFile, DatetoStr(Now) + '|' + TimetoStr(Now) + '|' + EncryptText);

  Flush(ScoreFile);
  Close(ScoreFile);

  ScreenPopupInfo.ShowPopup(Language.Translate('WEBSITE_SAVE_SCORE'));

  ScreenScore.Interaction := -1;
end;

procedure OnSendScore(Value: integer; Data: Pointer);
var
  SendInfo: TSendInfo;
  Website, index: integer;
begin
  if (Value = 1) or (Value = 2) then
  begin
    Website := ScreenPopUpSendScore.SelectValueW;

    if (ScreenPopUpSendScore.SelectValueU = High(ScreenPopUpSendScore.IUsername)) then
    begin
      SendInfo.Username := ScreenPopUpSendScore.Username;
      SendInfo.Password := ScreenPopUpSendScore.Password;
    end
    else
    begin
      SendInfo.Username := UDataBase.DataBase.NetworkUser[ScreenPopupSendScore.SelectValueW].UserList[ScreenPopupSendScore.SelectValueU].Username;
      SendInfo.Password := UDataBase.DataBase.NetworkUser[ScreenPopupSendScore.SelectValueW].UserList[ScreenPopupSendScore.SelectValueU].Password;
    end;

    index := ScreenPopupSendScore.SelectValueP;
    SendInfo.ScoreInt := player[index].ScoreInt;
    SendInfo.ScoreLineInt := player[index].ScoreLineInt;
    SendInfo.ScoreGoldenInt := player[index].ScoreGoldenInt;
    SendInfo.MD5Song := ScreenSing.Act_MD5Song;
    SendInfo.Level := ScreenSing.Act_Level;

    if (Value = 1) then
      SendScore(SendInfo, Website);

    if (Value = 2) then
      SaveScore(SendInfo, Website);
  end;
end;

function TScreenScore.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin
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
      SDLK_BACKSPACE:
        begin
          if (FinishScreenDraw = true) then
          begin
            if (CurrentSong.isDuet and (Ini.DuetScores = 0)) or (ScreenSong.RapToFreestyle) or (ScreenSong.Mode = smMedley) then
              FadeTo(@ScreenSong)
            else
              FadeTo(@ScreenTop5);
            Exit;
          end
          else
            BarTime := 0;
        end;

      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

      SDLK_RETURN:
        begin
         if (Interaction <> -1) then
            ScreenPopupSendScore.ShowPopup(Language.Translate('SCORE_SEND_DESC'), @OnSendScore, nil)
         else
         begin
           if (FinishScreenDraw = true) then
           begin

             if (CurrentSong.isDuet and (Ini.DuetScores = 0)) or (ScreenSong.RapToFreestyle) or (ScreenSong.Mode = smMedley) then
               FadeTo(@ScreenSong)
             else
               FadeTo(@ScreenTop5);

          	 Exit;
           end
           else
             BarTime := 0;
          end;
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN, SDLK_UP: Interaction := -1;

      SDLK_RIGHT:
        begin
          if (ScreenSong.Mode = smMedley) then
          begin
            if (CurrentRound < Length(PlaylistMedley.Stats)-1) then
              inc(CurrentRound)
            else
              CurrentRound := 0;

            AudioPlayback.PlaySound(SoundLib.Change);
            RefreshTexts;

            if not (Ini.SavePlayback=1) then
              StartPreview
            else
              StartVoice;

          end
          else
          begin
            if (ScreenSing.SungPaused = false) and
               (ScreenSing.SungToEnd) and (Length(DllMan.Websites) > 0) then
              InteractNext;
          end;
        end;

      SDLK_LEFT:
        begin
          if (ScreenSong.Mode = smMedley) then
          begin
            if (CurrentRound > 0) then
              dec(CurrentRound)
            else
              CurrentRound := Length(PlaylistMedley.Stats)-1;

            AudioPlayback.PlaySound(SoundLib.Change);
            RefreshTexts;

            if not (Ini.SavePlayback=1) then
              StartPreview
            else
              StartVoice;

          end
          else
          begin
            if (ScreenSing.SungPaused = false) and
               (ScreenSing.SungToEnd) and (Length(DllMan.Websites) > 0) then
              InteractPrev;
          end;

        end;

    end;
  end;
end;

function TScreenScore.ParseMouse(MouseButton: Integer; BtnDown: Boolean; X, Y: integer): boolean;
var
  button_s: integer;
begin
  Result := True;

  button_s := ButtonSend[GetScoreButtonLayout(PlayersPlay, Screens)];

  // transfer mousecords to the 800x600 raster we use to draw
  X := Round((X / (ScreenW / Screens)) * RenderW);
  if (X > RenderW) then
    X := X - RenderW;
  Y := Round((Y / ScreenH) * RenderH);

  if (Button[button_s].Visible) and (InRegion(X, Y, Button[button_s].GetMouseOverArea)) then
    SetInteraction(button_s)
  else
    SetInteraction(-1);

  if (MouseButton = SDL_BUTTON_LEFT) and BtnDown then
    ParseInput(SDLK_RETURN, 0, true);

end;

procedure TScreenScore.RefreshTexts;
var
  P:    integer;

begin
  if (CurrentRound < Length(PlaylistMedley.Stats)-1) then
  begin
    Text[TextArtist].Text      := IntToStr(CurrentRound+1) + '/' +
      IntToStr(Length(PlaylistMedley.Stats)-1) + ': ' +
      PlaylistMedley.Stats[CurrentRound].SongArtist;
    Text[TextTitle].Text       := PlaylistMedley.Stats[CurrentRound].SongTitle;
    Text[TextTitle].Visible    := true;
    Text[TextArtistTitle].Text := IntToStr(CurrentRound+1) + '/' +
      IntToStr(Length(PlaylistMedley.Stats)-1) + ': ' +
      PlaylistMedley.Stats[CurrentRound].SongArtist +
      ' - ' + PlaylistMedley.Stats[CurrentRound].SongTitle;
  end else
  begin
    if (ScreenSong.Mode = smMedley) then
    begin
      Text[TextArtist].Text      := Language.Translate('SING_TOTAL');
      Text[TextTitle].Visible    := false;
      Text[TextArtistTitle].Text := Language.Translate('SING_TOTAL');
    end else
    begin
      Text[TextArtist].Text      := PlaylistMedley.Stats[CurrentRound].SongArtist;
      Text[TextTitle].Text       := PlaylistMedley.Stats[CurrentRound].SongTitle;
      Text[TextTitle].Visible    := true;
      Text[TextArtistTitle].Text := PlaylistMedley.Stats[CurrentRound].SongArtist + ' - ' +
        PlaylistMedley.Stats[CurrentRound].SongTitle;
    end;
  end;

  if (ScreenSong.Mode = smMedley) then
  begin

    for P := 0 to PlayersPlay - 1 do
      Player[P] := PlaylistMedley.Stats[CurrentRound].Player[P];
  end;

  ResetScores;
end;

procedure TScreenScore.LoadSwapTextures;
var
  P, I, TargetPlayer: integer;
  Col: TRGB;
  StaticNum: integer;
  ThemeStatic: TThemeStatic;
begin
  if Screens = 2 then
  begin
    for P := low(PlayerStatic) to High(PlayerStatic) do
    begin
      SetLength(PlayerStaticTextures[P], Length(PlayerStatic[P]));

      for I := 0 to High(PlayerStatic[P]) do
      begin
        for TargetPlayer := 1 to PlayersPlay do
        begin
          PlayerStaticTextures[P, I, TargetPlayer].Tex := Statics[PlayerStatic[P, I]].Texture;

          if (Theme.Score.PlayerStatic[P, I].Typ = Texture_Type_Colorized) and
             ResolvePlayerThemeColor(Theme.Score.PlayerStatic[P, I].Color, TargetPlayer, Col) then
          begin
            with Theme.Score.PlayerStatic[P, I] do
              PlayerStaticTextures[P, I, TargetPlayer].Tex := Texture.GetTexture(Skin.GetTextureFileName(Tex), Typ, RGBFloatToInt(Col.R, Col.G, Col.B));

            PlayerStaticTextures[P, I, TargetPlayer].Tex.X := Statics[PlayerStatic[P, I]].Texture.X;
            PlayerStaticTextures[P, I, TargetPlayer].Tex.Y := Statics[PlayerStatic[P, I]].Texture.Y;
            PlayerStaticTextures[P, I, TargetPlayer].Tex.W := Statics[PlayerStatic[P, I]].Texture.W;
            PlayerStaticTextures[P, I, TargetPlayer].Tex.H := Statics[PlayerStatic[P, I]].Texture.H;
          end;
        end;
      end;
    end;

    for P := low(PlayerBoxTextures) to High(PlayerBoxTextures) do
    begin
      for I := 0 to High(PlayerBoxTextures[P]) do
      begin
        case I of
          0: begin
            StaticNum := StaticBoxLightest[P];
            ThemeStatic := Theme.Score.StaticBoxLightest[P];
          end;
          1: begin
            StaticNum := StaticBoxLight[P];
            ThemeStatic := Theme.Score.StaticBoxLight[P];
          end;
          2: begin
            StaticNum := StaticBoxDark[P];
            ThemeStatic := Theme.Score.StaticBoxDark[P];
          end;
        end;
        for TargetPlayer := 1 to PlayersPlay do
        begin
          PlayerBoxTextures[P, I, TargetPlayer].Tex := Statics[StaticNum].Texture;

          if (ThemeStatic.Typ = Texture_Type_Colorized) and
             ResolvePlayerThemeColor(ThemeStatic.Color, TargetPlayer, Col) then
          begin
            with ThemeStatic do
              PlayerBoxTextures[P, I, TargetPlayer].Tex := Texture.GetTexture(Skin.GetTextureFileName(Tex), Typ, RGBFloatToInt(Col.R, Col.G, Col.B));

            PlayerBoxTextures[P, I, TargetPlayer].Tex.X := Statics[StaticNum].Texture.X;
            PlayerBoxTextures[P, I, TargetPlayer].Tex.Y := Statics[StaticNum].Texture.Y;
            PlayerBoxTextures[P, I, TargetPlayer].Tex.W := Statics[StaticNum].Texture.W;
            PlayerBoxTextures[P, I, TargetPlayer].Tex.H := Statics[StaticNum].Texture.H;
          end;
        end;
      end;
    end;
  end;
end;

procedure TScreenScore.SwapToScreen(Screen: integer);
var
  P, I, J: integer;

  function FindPlayerIndexForSlot(const SlotIndex, TargetScreen: integer): integer;
  var
    PlayerIdx: integer;
  begin
    Result := -1;

    if Length(PlayerPositionMap) = 0 then
      Exit;

    for PlayerIdx := Low(PlayerPositionMap) to High(PlayerPositionMap) do
    begin
      if (PlayerPositionMap[PlayerIdx].Position = SlotIndex) and
         (TargetScreen = PlayerPositionMap[PlayerIdx].Screen) then
      begin
        Result := PlayerIdx;
        Exit;
      end;
    end;
  end;

begin
  if (Screens = 2) then
  begin
    for P := 1 to UIni.IMaxPlayerCount do
    begin
      J := FindPlayerIndexForSlot(P, Screen);
      SetScoreSlotVisible(Self, P, J <> -1);

      if J = -1 then
        Continue;

      for I := 0 to High(PlayerStatic[P]) do
      begin
        Statics[PlayerStatic[P, I]].Texture := PlayerStaticTextures[P, I, J + 1].Tex;
      end;

      Statics[StaticBoxLightest[P]].Texture := PlayerBoxTextures[P, 0, J + 1].Tex;
      Statics[StaticBoxLight[P]].Texture := PlayerBoxTextures[P, 1, J + 1].Tex;
      Statics[StaticBoxDark[P]].Texture := PlayerBoxTextures[P, 2, J + 1].Tex;
      ApplyScoreSlotPlayerVisuals(Self, P, J + 1);
    end;

    for P := 1 to PlayersPlay do
    begin
      Statics[AvatarStaticRef[P]].Visible :=
        (PlayerPositionMap[P-1].Position > 0) and
        (Screen = PlayerPositionMap[P-1].Screen);
    end;
  end;
end;

constructor TScreenScore.Create;
var
  Player:  integer;
  Counter: integer;
  I: integer;
  ColDark: TRGB;
  ColLight: TRGB;
  ColLightest: TRGB;
  SlotIndex: integer;
begin
  inherited Create;

  LoadFromTheme(Theme.Score);

  // These two texts arn't used in the deluxe skin
  TextArtist      := AddText(Theme.Score.TextArtist);
  TextTitle       := AddText(Theme.Score.TextTitle);

  TextArtistTitle := AddText(Theme.Score.TextArtistTitle);

  for Player := 1 to UIni.IMaxPlayerCount do
  begin
    SetLength(PlayerStatic[Player], Length(Theme.Score.PlayerStatic[Player]));
    SetLength(PlayerTexts[Player],  Length(Theme.Score.PlayerTexts[Player]));

    for Counter := 0 to High(Theme.Score.PlayerStatic[Player]) do
      PlayerStatic[Player, Counter]      := AddStatic(Theme.Score.PlayerStatic[Player, Counter]);

    for Counter := 0 to High(Theme.Score.PlayerTexts[Player]) do
      PlayerTexts[Player, Counter]       := AddText(Theme.Score.PlayerTexts[Player, Counter]);

    TextName[Player]             := AddText(Theme.Score.TextName[Player]);
    TextScore[Player]            := AddText(Theme.Score.TextScore[Player]);

    TextNotes[Player]            := AddText(Theme.Score.TextNotes[Player]);
    TextNotesScore[Player]       := AddText(Theme.Score.TextNotesScore[Player]);
    TextLineBonus[Player]        := AddText(Theme.Score.TextLineBonus[Player]);
    TextLineBonusScore[Player]   := AddText(Theme.Score.TextLineBonusScore[Player]);
    TextGoldenNotes[Player]      := AddText(Theme.Score.TextGoldenNotes[Player]);
    TextGoldenNotesScore[Player] := AddText(Theme.Score.TextGoldenNotesScore[Player]);
    TextTotal[Player]            := AddText(Theme.Score.TextTotal[Player]);
    TextTotalScore[Player]       := AddText(Theme.Score.TextTotalScore[Player]);

    StaticBoxLightest[Player]    := AddStatic(Theme.Score.StaticBoxLightest[Player]);
    StaticBoxLight[Player]       := AddStatic(Theme.Score.StaticBoxLight[Player]);
    StaticBoxDark[Player]        := AddStatic(Theme.Score.StaticBoxDark[Player]);

    StaticBackLevel[Player]      := AddStatic(Theme.Score.StaticBackLevel[Player]);
    StaticBackLevelRound[Player] := AddStatic(Theme.Score.StaticBackLevelRound[Player]);
    StaticLevel[Player]          := AddStatic(Theme.Score.StaticLevel[Player]);
    StaticLevelRound[Player]     := AddStatic(Theme.Score.StaticLevelRound[Player]);

    // ######################
    // Score screen textures
    // ######################

    //## the bars that visualize the score ##

    //NoteBar ScoreBar
    ColDark := GetPlayerColor(Ini.PlayerColor[Player - 1]);
    Tex_Score_NoteBarLevel_Dark[Player] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreLevel_Dark'), TEXTURE_TYPE_COLORIZED, RGBFloatToInt(ColDark.R, ColDark.G, ColDark.B));
    Tex_Score_NoteBarRound_Dark[Player] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreLevel_Dark_Round'), TEXTURE_TYPE_COLORIZED, RGBFloatToInt(ColDark.R, ColDark.G, ColDark.B));

    //LineBonus ScoreBar
    ColLight := GetPlayerLightColor(Ini.PlayerColor[Player - 1]);
    Tex_Score_NoteBarLevel_Light[Player] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreLevel_Light'), TEXTURE_TYPE_COLORIZED, RGBFloatToInt(ColLight.R, ColLight.G, ColLight.B));
    Tex_Score_NoteBarRound_Light[Player] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreLevel_Light_Round'), TEXTURE_TYPE_COLORIZED, RGBFloatToInt(ColLight.R, ColLight.G, ColLight.B));

    //GoldenNotes ScoreBar
    ColLightest := ColorSqrt(ColLight);
    Tex_Score_NoteBarLevel_Lightest[Player] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreLevel_Lightest'), TEXTURE_TYPE_COLORIZED, RGBFloatToInt(ColLightest.R, ColLightest.G, ColLightest.B));
    Tex_Score_NoteBarRound_Lightest[Player] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreLevel_Lightest_Round'), TEXTURE_TYPE_COLORIZED, RGBFloatToInt(ColLightest.R, ColLightest.G, ColLightest.B));

    //textures
    aPlayerScoreScreenTextures[Player].Score_NoteBarLevel_Dark     := Tex_Score_NoteBarLevel_Dark[Player];
    aPlayerScoreScreenTextures[Player].Score_NoteBarRound_Dark     := Tex_Score_NoteBarRound_Dark[Player];

    aPlayerScoreScreenTextures[Player].Score_NoteBarLevel_Light    := Tex_Score_NoteBarLevel_Light[Player];
    aPlayerScoreScreenTextures[Player].Score_NoteBarRound_Light    := Tex_Score_NoteBarRound_Light[Player];

    aPlayerScoreScreenTextures[Player].Score_NoteBarLevel_Lightest := Tex_Score_NoteBarLevel_Lightest[Player];
    aPlayerScoreScreenTextures[Player].Score_NoteBarRound_Lightest := Tex_Score_NoteBarRound_Lightest[Player];
  end;

  MapPlayersToPosition;

  for I := 1 to PlayersPlay do
  begin
    SlotIndex := PlayerPositionMap[I - 1].Position;
    AvatarStaticRef[I] := AddStatic(Theme.Score.AvatarStatic[SlotIndex]);
    Statics[AvatarStaticRef[I]].Texture := AvatarPlayerTextures[I];
    Statics[AvatarStaticRef[I]].Texture.X := Theme.Score.AvatarStatic[SlotIndex].X;
    Statics[AvatarStaticRef[I]].Texture.Y := Theme.Score.AvatarStatic[SlotIndex].Y;
    Statics[AvatarStaticRef[I]].Texture.H := Theme.Score.AvatarStatic[SlotIndex].H;
    Statics[AvatarStaticRef[I]].Texture.W := Theme.Score.AvatarStatic[SlotIndex].W;
    Statics[AvatarStaticRef[I]].Texture.Z := Theme.Score.AvatarStatic[SlotIndex].Z;
    Statics[AvatarStaticRef[I]].Texture.Alpha := Theme.Score.AvatarStatic[SlotIndex].Alpha;
    Statics[AvatarStaticRef[I]].Visible := true;
    ApplyScoreSlotPlayerVisuals(Self, SlotIndex, I);
  end;

  StaticNavigate := AddStatic(Theme.Score.StaticNavigate);
  TextNavigate := AddText(Theme.Score.TextNavigate);

  if Screens = 2 then
    LoadSwapTextures;

  // Send buttons are chosen from the current column layout at runtime.
  for I := 1 to High(ButtonSend) do
    ButtonSend[I] := AddButton(Theme.Score.ButtonSend[I]);

end;

destructor TScreenScore.Destroy;
var
  I: integer;
begin
  for I := 1 to UIni.IMaxPlayerCount do
  begin
    FreeTexture(Tex_Score_NoteBarLevel_Dark[I]);
    FreeTexture(Tex_Score_NoteBarRound_Dark[I]);
    FreeTexture(Tex_Score_NoteBarLevel_Light[I]);
    FreeTexture(Tex_Score_NoteBarRound_Light[I]);
    FreeTexture(Tex_Score_NoteBarLevel_Lightest[I]);
    FreeTexture(Tex_Score_NoteBarRound_Lightest[I]);
  end;
  inherited;
end;

procedure TScreenScore.MapPlayersToPosition;
var
  I: integer;
  ScreenIndex: integer;
begin
  SetLength(PlayerPositionMap, PlayersPlay);

  for I := 0 to PlayersPlay - 1 do
  begin
    ScreenIndex := GetPlayerScreen(I, PlayersPlay, Screens);
    PlayerPositionMap[I].Screen := ScreenIndex;
    PlayerPositionMap[I].Position := GetScoreSlotIndex(I, PlayersPlay, Screens);
  end;
end;

procedure TScreenScore.UpdateAnimation;
var
  CurrentTime: integer;
  I: integer;
begin
  CurrentTime := SDL_GetTicks();

  if (ScreenAct = 1) and ShowFinish then
    while (CurrentTime >= BarTime) do
    begin
      Inc(BarTime, BarRaiseSpeed);

      // We actually arise them in the right order, but we have to draw them in reverse order (golden -> phrase -> mainscore)
      if (BarScore_EaseOut_Step < EaseOut_MaxSteps * 10) then
        BarScore_EaseOut_Step:= BarScore_EaseOut_Step + 1

      // PhrasenBonus
      else if (BarPhrase_EaseOut_Step < EaseOut_MaxSteps * 10) then
        BarPhrase_EaseOut_Step := BarPhrase_EaseOut_Step + 1

      // GoldenNotebonus
      else if (BarGolden_EaseOut_Step < EaseOut_MaxSteps * 10) then
        BarGolden_EaseOut_Step := BarGolden_EaseOut_Step + 1

      // rating icon
      else
        for I := 1 to PlayersPlay do
          CalculateBouncing(I);
    end;
end;

procedure TScreenScore.DrawPlayerBars;
  var
    I: integer;
begin
  for I := 0 to PlayersPlay - 1 do
  begin
    if (PlayerPositionMap[I].Position > 0) and (ScreenAct = PlayerPositionMap[I].Screen) then
    begin
      if (BarScore_EaseOut_Step >= (EaseOut_MaxSteps * 10)) then
      begin
        if (BarPhrase_EaseOut_Step >= (EaseOut_MaxSteps * 10)) then
        begin
          // Draw golden score bar #
          EaseBarIn(I + 1, sbtGolden);
          EaseScoreIn(I + 1, sbtGolden);
        end;

        // Draw phrase score bar #
        EaseBarIn(I + 1, sbtLine);
        EaseScoreIn(I + 1, sbtLine);
      end;

      // Draw plain score bar #
      EaseBarIn(I + 1, sbtScore);
      EaseScoreIn(I + 1, sbtScore);
    end;
  end;
end;

procedure TScreenScore.OnShow;
var
  P: integer;  // player
  I: integer;
  V: array[1..UIni.IMaxPlayerCount] of boolean; // visibility array
begin
  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenScore');

  FinishScreenDraw := false;

  {**
   * Turn backgroundmusic on
   *}
  //SoundLib.StartBgMusic; -- now play current song

  inherited;

  // intially show total score
  CurrentRound := Length(PlaylistMedley.Stats)-1;
  if (ScreenSong.Mode = smMedley) then
  begin
    StartPreview;
    for P := 0 to PlayersPlay - 1 do
      Player[P] := PlaylistMedley.Stats[CurrentRound].Player[P];

    Statics[StaticNavigate].Visible := true;
    Text[TextNavigate].Visible := true;
  end else
  begin
    Statics[StaticNavigate].Visible := false;
    Text[TextNavigate].Visible := false;
  end;

  MapPlayersToPosition;

  Text[TextArtist].Text      := CurrentSong.Artist;
  Text[TextTitle].Text       := CurrentSong.Title;
  Text[TextArtistTitle].Text := CurrentSong.Artist + ' - ' + CurrentSong.Title;

  FillChar(V, SizeOf(V), 0);
  for P := 0 to PlayersPlay - 1 do
    if PlayerPositionMap[P].Position > 0 then
      V[PlayerPositionMap[P].Position] := true;

  for P := 1 to UIni.IMaxPlayerCount do
  begin
    SetScoreSlotVisible(Self, P, V[P]);

    // we draw that on our own
    Statics[StaticBackLevel[P]].Visible      := false;
    Statics[StaticBackLevelRound[P]].Visible := false;
    Statics[StaticLevel[P]].Visible          := false;
    Statics[StaticLevelRound[P]].Visible     := false;
  end;

  for P := 1 to PlayersPlay do
    ApplyScoreSlotPlayerVisuals(Self, PlayerPositionMap[P - 1].Position, P);

  for I := 0 to 2 do
  begin
    Button[I].Visible := false;
    Button[I].Selectable := false;
  end;

  // Show Send Score Buttons
  if (ScreenSing.SungPaused = false) and (ScreenSing.SungToEnd) and (Length(DllMan.Websites) > 0) then
  begin
    I := GetScoreButtonLayout(PlayersPlay, Screens) - 1;
    Button[I].Visible := true;
    Button[I].Selectable := true;
  end;

  Interaction := -1;

  RefreshTexts;

  if not (Ini.SavePlayback = 1) then
    StartPreview
  else
  begin
    Voice := -1;
    StartVoice;
  end;

end;

procedure TScreenScore.ResetScores;
var
  P:    integer;

begin
  for P := 1 to PlayersPlay do
  begin
    // data
    aPlayerScoreScreenDatas[P].Bar_Y            := Theme.Score.StaticBackLevel[PlayerPositionMap[P-1].Position].Y;

    // ratings
    aPlayerScoreScreenRatings[P].RateEaseStep   := 1;
    aPlayerScoreScreenRatings[P].RateEaseValue  := 20;

    // actual values
    TextScore_ActualValue[P]  := 0;
    TextPhrase_ActualValue[P] := 0;
    TextGolden_ActualValue[P] := 0;
  end;

  for P := 1 to UIni.IMaxPlayerCount do
    SetScoreSlotScoreAlpha(Self, P, 0);

  BarScore_EaseOut_Step  := 1;
  BarPhrase_EaseOut_Step := 1;
  BarGolden_EaseOut_Step := 1;

  BarTime := SDL_GetTicks();
end;

procedure TScreenScore.onShowFinish;
var
  index: integer;
begin
  for index := 1 to (PlayersPlay) do
  begin
    TextScore_ActualValue[index]  := 0;
    TextPhrase_ActualValue[index] := 0;
    TextGolden_ActualValue[index] := 0;
  end;

  BarTime := SDL_GetTicks();
end;

function TScreenScore.Draw: boolean;
var
  PlayerCounter: integer;
begin
{
  player[0].ScoreInt       := 7000;
  player[0].ScoreLineInt   := 2000;
  player[0].ScoreGoldenInt := 1000;
  player[0].ScoreTotalInt  := 10000;

  player[1].ScoreInt       := 8500;
  player[1].ScoreLineInt   := 1000;
  player[1].ScoreGoldenInt :=  500;
  player[1].ScoreTotalInt  := 10000;

  player[2].ScoreInt       := 8000;
  player[2].ScoreLineInt   := 1000;
  player[2].ScoreGoldenInt := 1000;
  player[2].ScoreTotalInt  := 10000;

  player[3].ScoreInt       := 7000;
  player[3].ScoreLineInt   := 2000;
  player[3].ScoreGoldenInt := 1000;
  player[3].ScoreTotalInt  := 10000;

  player[4].ScoreInt       := 8500;
  player[4].ScoreLineInt   := 1000;
  player[4].ScoreGoldenInt :=  500;
  player[4].ScoreTotalInt  := 10000;

  player[5].ScoreInt       := 8000;
  player[5].ScoreLineInt   := 1000;
  player[5].ScoreGoldenInt := 1000;
  player[5].ScoreTotalInt  := 10000;

  }

  if (ScreenSong.Mode = smMedley) then
    BarTime := 0;

  // swap static textures to current screen ones
  SwapToScreen(ScreenAct);

  //Draw the Background
  DrawBG;

  // Let's start to arise the bars
  UpdateAnimation;

  if (ShowFinish) then
    DrawPlayerBars;



(*
    //todo: i need a clever method to draw statics with their z value
    for I := 0 to High(Statics) do
      Statics[I].Draw;
    for I := 0 to High(Text) do
      Text[I].Draw;
*)

  // we have to swap the themeobjects values on every draw
  // to support dual screen
  for PlayerCounter := 1 to PlayersPlay do
  begin
    FillPlayerItems(PlayerCounter);
  end;
   //Draw Theme Objects
  DrawFG;
  Result := true;
end;

procedure TscreenScore.FillPlayerItems(PlayerNumber: integer);
var
  SlotIndex: integer;
begin
  SlotIndex := PlayerPositionMap[PlayerNumber-1].Position;
  if (SlotIndex > 0) and (ScreenAct = PlayerPositionMap[PlayerNumber-1].Screen) then
  begin
    Text[TextName[SlotIndex]].Text := Player[PlayerNumber-1].Name;
    // end todo

    //golden
    Text[TextGoldenNotesScore[SlotIndex]].Text         := IntToStr(TextGolden_ActualValue[PlayerNumber]);
    Text[TextGoldenNotesScore[SlotIndex]].Alpha        := (BarGolden_EaseOut_Step / 100);

    Statics[StaticBoxLightest[SlotIndex]].Texture.Alpha := (BarGolden_EaseOut_Step / 100);
    Text[TextGoldenNotes[SlotIndex]].Alpha             := (BarGolden_EaseOut_Step / 100);

    // line bonus
    Text[TextLineBonusScore[SlotIndex]].Text           := IntToStr(TextPhrase_ActualValue[PlayerNumber]);
    Text[TextLineBonusScore[SlotIndex]].Alpha          := (BarPhrase_EaseOut_Step / 100);

    Statics[StaticBoxLight[SlotIndex]].Texture.Alpha    := (BarPhrase_EaseOut_Step / 100);
    Text[TextLineBonus[SlotIndex]].Alpha               := (BarPhrase_EaseOut_Step / 100);

    // plain score
    Text[TextNotesScore[SlotIndex]].Text               := IntToStr(TextScore_ActualValue[PlayerNumber]);
    Text[TextNotes[SlotIndex]].Alpha                   := (BarScore_EaseOut_Step / 100);

    Statics[StaticBoxDark[SlotIndex]].Texture.Alpha     := (BarScore_EaseOut_Step / 100);
    Text[TextNotesScore[SlotIndex]].Alpha              := (BarScore_EaseOut_Step / 100);

    // total score
    Text[TextTotalScore[SlotIndex]].Text               := IntToStr(TextScore_ActualValue[PlayerNumber] + TextPhrase_ActualValue[PlayerNumber] + TextGolden_ActualValue[PlayerNumber]);
    Text[TextTotalScore[SlotIndex]].Alpha              := (BarScore_EaseOut_Step / 100);

    Text[TextTotal[SlotIndex]].Alpha                   := (BarScore_EaseOut_Step / 100);

    if(BarGolden_EaseOut_Step = 100) then
    begin
      ShowRating(PlayerNumber);
    end;
  end;
end;

procedure TScreenScore.ShowRating(PlayerNumber: integer);
var
  Rating: integer;
  SlotIndex: integer;
begin
  SlotIndex := PlayerPositionMap[PlayerNumber-1].Position;
  if (SlotIndex > 0) and (ScreenAct = PlayerPositionMap[PlayerNumber-1].Screen) then
  begin
    case (Player[PlayerNumber-1].ScoreTotalInt) of
     0..2009:
       begin
         Text[TextScore[SlotIndex]].Text := Language.Translate('SING_SCORE_TONE_DEAF');
         Rating := 0;
       end;
     2010..4009:
       begin
         Text[TextScore[SlotIndex]].Text := Language.Translate('SING_SCORE_AMATEUR');
         Rating := 1;
       end;
     4010..5009:
       begin
         Text[TextScore[SlotIndex]].Text := Language.Translate('SING_SCORE_WANNABE');
         Rating := 2;
       end;
     5010..6009:
       begin
         Text[TextScore[SlotIndex]].Text := Language.Translate('SING_SCORE_HOPEFUL');
         Rating := 3;
       end;
     6010..7509:
       begin
         Text[TextScore[SlotIndex]].Text := Language.Translate('SING_SCORE_RISING_STAR');
         Rating := 4;
       end;
     7510..8509:
       begin
         Text[TextScore[SlotIndex]].Text := Language.Translate('SING_SCORE_LEAD_SINGER');
         Rating := 5;
       end;
     8510..9009:
       begin
         Text[TextScore[SlotIndex]].Text := Language.Translate('SING_SCORE_SUPERSTAR');
         Rating := 6;
       end;
     9010..10000:
       begin
         Text[TextScore[SlotIndex]].Text := Language.Translate('SING_SCORE_ULTRASTAR');
         Rating := 7;
       end;
    else
      Rating := 0; // Cheata :P
    end;

    //todo: this could break if the width is not given, for instance when there's a skin with no picture for ratings
    if ( Theme.Score.StaticRatings[SlotIndex].W > 0 ) and  ( aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue > 0 ) then
    begin
      Text[TextScore[SlotIndex]].Alpha := aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue / Theme.Score.StaticRatings[SlotIndex].W;
    end;
    // end todo

    DrawRating(PlayerNumber, Rating);
  end;
end;

procedure TscreenScore.DrawRating(PlayerNumber: integer; Rating: integer);
var
  Posx:  real;
  Posy:  real;
  Width: real;
  DrawX: real;
  DrawY: real;
  DrawW: real;
  DrawH: real;
  ThemeIndex: integer;
  SlotIndex: integer;
begin
  SlotIndex := PlayerPositionMap[PlayerNumber-1].Position;

  if (Theme.Score.StaticRatings[SlotIndex].W <> 0) and (Theme.Score.StaticRatings[SlotIndex].H <> 0) then
  begin
    PosX := Theme.Score.StaticRatings[SlotIndex].X + (Theme.Score.StaticRatings[SlotIndex].W  * 0.5);
    PosY := Theme.Score.StaticRatings[SlotIndex].Y + (Theme.Score.StaticRatings[SlotIndex].H  * 0.5);

    Width := aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue / 2;
    DrawX := PosX - Width;
    DrawY := PosY - Width;
    DrawW := Width * 2;
    DrawH := Width * 2;
    ResolveLayoutRect(DrawX, DrawY, DrawW, DrawH, lsUniform);

    glBindTexture(GL_TEXTURE_2D, Tex_Score_Ratings[Rating].TexNum);

    glColor3f(1.0, 1.0, 1.0);
      
    glEnable(GL_TEXTURE_2D);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);

    glBegin(GL_QUADS);
      glTexCoord2f(0, 0);                                                           glVertex2f(DrawX,  DrawY);
      glTexCoord2f(Tex_Score_Ratings[Rating].TexW, 0);                              glVertex2f(DrawX + DrawW,  DrawY);
      glTexCoord2f(Tex_Score_Ratings[Rating].TexW, Tex_Score_Ratings[Rating].TexH); glVertex2f(DrawX + DrawW,  DrawY + DrawH);
      glTexCoord2f(0, Tex_Score_Ratings[Rating].TexH);                              glVertex2f(DrawX,  DrawY + DrawH);
    glEnd;

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2d);
  end;
end;

function TscreenScore.CalculateBouncing(PlayerNumber: integer): real;
var
  p, s:              real;

  RaiseStep, MaxVal: real;
  EaseOut_Step:      integer;
  SlotIndex: integer;
begin
  SlotIndex := PlayerPositionMap[PlayerNumber-1].Position;

  EaseOut_Step  := aPlayerScoreScreenRatings[PlayerNumber].RateEaseStep;
  MaxVal        := Theme.Score.StaticRatings[SlotIndex].W;

  RaiseStep     := EaseOut_Step;

  if (MaxVal > 0) and (RaiseStep > 0) then
    RaiseStep := RaiseStep / MaxVal;

  if (RaiseStep = 1) then
  begin
    Result := MaxVal;
    FinishScreenDraw := true;
  end
  else
  begin
    p := MaxVal * 0.4;

    s           := p/(2*PI) * arcsin (1);
    Result := MaxVal * power(2,-5 * RaiseStep) * sin( (RaiseStep * MaxVal - s) * (2 * PI) / p) + MaxVal;

    inc(aPlayerScoreScreenRatings[PlayerNumber].RateEaseStep);
    aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue := Result;
  end;
end;

procedure TscreenScore.EaseBarIn(PlayerNumber: integer; BarType: TScoreBarType);
const
  RaiseSmoothness: integer = 100;
var
  MaxHeight:    real;
  NewHeight:    real;

  Height2Reach: real;
  RaiseStep:    real;
  BarStartPosY: single;

  lTmp:         real;
  Score:        integer;
  SlotIndex: integer;
begin
  SlotIndex := PlayerPositionMap[PlayerNumber-1].Position;
  MaxHeight    := Theme.Score.StaticBackLevel[SlotIndex].H;

  // let's get the points according to the bar we draw
  // score array starts at 0, which means the score for player 1 is in score[0]
  // EaseOut_Step is the actual step in the raising process, like the 20iest step of EaseOut_MaxSteps
  if (BarType = sbtScore) then
  begin
    Score        := Player[PlayerNumber - 1].ScoreInt;
    RaiseStep    := BarScore_EaseOut_Step;
    BarStartPosY := Theme.Score.StaticBackLevel[SlotIndex].Y + MaxHeight;
  end
  else if (BarType = sbtLine) then
  begin
    Score        := Player[PlayerNumber - 1].ScoreLineInt;
    RaiseStep    := BarPhrase_EaseOut_Step;
    BarStartPosY := Theme.Score.StaticBackLevel[SlotIndex].Y - aPlayerScoreScreenDatas[PlayerNumber].BarScore_ActualHeight + MaxHeight;
  end
  else if (BarType = sbtGolden) then
  begin
    Score        := Player[PlayerNumber - 1].ScoreGoldenInt;
    RaiseStep    := BarGolden_EaseOut_Step;
    BarStartPosY := Theme.Score.StaticBackLevel[SlotIndex].Y - aPlayerScoreScreenDatas[PlayerNumber].BarScore_ActualHeight - aPlayerScoreScreenDatas[PlayerNumber].BarLine_ActualHeight + MaxHeight;
  end;

  // the height dependend of the score
  Height2Reach := (Score / MAX_SONG_SCORE) * MaxHeight;

  if (aPlayerScoreScreenDatas[PlayerNumber].Bar_Actual_Height < Height2Reach) then
  begin
    // Check http://proto.layer51.com/d.aspx?f=400 for more info on easing functions
    // Calculate the actual step according to the maxsteps
    RaiseStep := RaiseStep / EaseOut_MaxSteps;

    // quadratic easing out - decelerating to zero velocity
    // -end_position * current_time * ( current_time - 2 ) + start_postion
    lTmp := (-Height2Reach * RaiseStep * (RaiseStep - 20) + BarStartPosY);

    if ( RaiseSmoothness > 0 ) and ( lTmp > 0 ) then
      NewHeight := lTmp / RaiseSmoothness;

  end
  else
    NewHeight := Height2Reach;

  DrawBar(BarType, PlayerNumber, BarStartPosY, NewHeight);

  if (BarType = sbtScore) then
    aPlayerScoreScreenDatas[PlayerNumber].BarScore_ActualHeight  := NewHeight
  else if (BarType = sbtLine) then
    aPlayerScoreScreenDatas[PlayerNumber].BarLine_ActualHeight   := NewHeight
  else if (BarType = sbtGolden) then
    aPlayerScoreScreenDatas[PlayerNumber].BarGolden_ActualHeight := NewHeight;
end;

procedure TscreenScore.DrawBar(BarType: TScoreBarType; PlayerNumber: integer; BarStartPosY: single; NewHeight: real);
var
  Width:        real;
  BarStartPosX: real;
  SlotIndex: integer;
begin
  SlotIndex := PlayerPositionMap[PlayerNumber-1].Position;

  // this is solely for better readability of the drawing
  Width        := Theme.Score.StaticBackLevel[SlotIndex].W;
  BarStartPosX := Theme.Score.StaticBackLevel[SlotIndex].X;

  glColor4f(1, 1, 1, 1);

  // set the texture for the bar
  if (BarType = sbtScore) then
    glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarLevel_Dark.TexNum);
  if (BarType = sbtLine) then
    glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarLevel_Light.TexNum);
  if (BarType = sbtGolden) then
    glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarLevel_Lightest.TexNum);

  //draw it
  glEnable(GL_TEXTURE_2D);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex3f(BarStartPosX,         BarStartPosY - NewHeight, ZBars);
    glTexCoord2f(1, 0); glVertex3f(BarStartPosX + Width, BarStartPosY - NewHeight, ZBars);
    glTexCoord2f(1, 1); glVertex3f(BarStartPosX + Width, BarStartPosY,             ZBars);
    glTexCoord2f(0, 1); glVertex3f(BarStartPosX,         BarStartPosY,             ZBars);
  glEnd;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2d);

  //the round thing on top
  if (BarType = sbtScore) then
    glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarRound_Dark.TexNum);
  if (BarType = sbtLine) then
    glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarRound_Light.TexNum);
  if (BarType = sbtGolden) then
    glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarRound_Lightest.TexNum);

  glEnable(GL_TEXTURE_2D);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex3f(BarStartPosX,         (BarStartPosY - Statics[StaticLevelRound[SlotIndex]].Texture.h) - NewHeight, ZBars);
    glTexCoord2f(1, 0); glVertex3f(BarStartPosX + Width, (BarStartPosY - Statics[StaticLevelRound[SlotIndex]].Texture.h) - NewHeight, ZBars);
    glTexCoord2f(1, 1); glVertex3f(BarStartPosX + Width,  BarStartPosY - NewHeight,                                                     ZBars);
    glTexCoord2f(0, 1); glVertex3f(BarStartPosX,          BarStartPosY - NewHeight,                                                     ZBars);
  glEnd;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2d);
end;

procedure TScreenScore.EaseScoreIn(PlayerNumber: integer; ScoreType: TScoreBarType);
const
  RaiseSmoothness: integer = 100;
var
  RaiseStep:        real;
  lTmpA:            real;
  ScoreReached:     integer;
  EaseOut_Step:     real;
  ActualScoreValue: integer;
begin
  if (ScoreType = sbtScore) then
  begin
    EaseOut_Step     := BarScore_EaseOut_Step;
    ActualScoreValue := TextScore_ActualValue[PlayerNumber];
    ScoreReached     := Player[PlayerNumber-1].ScoreInt;
  end;
  if (ScoreType = sbtLine) then
  begin
    EaseOut_Step     := BarPhrase_EaseOut_Step;
    ActualScoreValue := TextPhrase_ActualValue[PlayerNumber];
    ScoreReached     := Player[PlayerNumber-1].ScoreLineInt;
  end;
  if (ScoreType = sbtGolden) then
  begin
    EaseOut_Step     := BarGolden_EaseOut_Step;
    ActualScoreValue := TextGolden_ActualValue[PlayerNumber];
    ScoreReached     := Player[PlayerNumber-1].ScoreGoldenInt;
  end;

  // EaseOut_Step is the actual step in the raising process, like the 20iest step of EaseOut_MaxSteps
  RaiseStep := EaseOut_Step;

  if (ActualScoreValue < ScoreReached) then
  begin
    // Calculate the actual step according to the maxsteps
    RaiseStep := RaiseStep / EaseOut_MaxSteps;

    // quadratic easing out - decelerating to zero velocity
    // -end_position * current_time * ( current_time - 2 ) + start_postion
    lTmpA := (-ScoreReached * RaiseStep * (RaiseStep - 20));
    if ( lTmpA           > 0 ) and
       ( RaiseSmoothness > 0 ) then
    begin
      if (ScoreType = sbtScore) then
        TextScore_ActualValue[PlayerNumber]  := floor( lTmpA / RaiseSmoothness);
      if (ScoreType = sbtLine) then
        TextPhrase_ActualValue[PlayerNumber] := floor( lTmpA / RaiseSmoothness);
      if (ScoreType = sbtGolden) then
        TextGolden_ActualValue[PlayerNumber] := floor( lTmpA / RaiseSmoothness);
    end;
  end
  else
  begin
    if (ScoreType = sbtScore) then
      TextScore_ActualValue[PlayerNumber]    := ScoreReached;
    if (ScoreType = sbtLine) then
      TextPhrase_ActualValue[PlayerNumber]   := ScoreReached;
    if (ScoreType = sbtGolden) then
      TextGolden_ActualValue[PlayerNumber]   := ScoreReached;
  end;
end;

//Procedure Change current played Preview
procedure TScreenSCore.StartPreview;
var
  select:   integer;
  changed:  boolean;
  PreviewVolume: single;
begin
  //When Music Preview is activated -> then change music
  if (Ini.PreviewVolume <> 0) then
  begin
    changed := false;
    if (ScreenSong.Mode = smMedley) then
    begin
      if (CurrentRound < Length(PlaylistMedley.Stats) - 1) and (ScreenSong.SongIndex <> PlaylistMedley.Song[CurrentRound])  then
      begin
        select := PlaylistMedley.Song[CurrentRound];
        changed := true;
        ScreenSong.SongIndex := select;
      end;
      if (CurrentRound = Length(PlaylistMedley.Stats) - 1) then // Total Score --> play last song in medley
      begin
        select := PlaylistMedley.Song[CurrentRound-1];
        changed := true;
        ScreenSong.SongIndex := select;
      end
    end else
    begin
      select := ScreenSong.Interaction;
      ScreenSong.SongIndex := select;
      changed := true;
    end;

    if changed then
    begin
      AudioPlayback.Close;

      if AudioPlayback.Open(CatSongs.Song[select].Path.Append(CatSongs.Song[select].Audio),nil) then
      begin
        if (CatSongs.Song[select].PreviewStart > 0) then
          AudioPlayback.Position := CatSongs.Song[select].PreviewStart
        else
          AudioPlayback.Position := (AudioPlayback.Length / 4);

        // set preview volume
        PreviewVolume := EnsureRange(Ini.PreviewVolume, 0, 100) / 100;
        if (Ini.PreviewFading = 0) then
        begin
          // music fade disabled: start with full volume
          AudioPlayback.SetVolume(PreviewVolume);
          AudioPlayback.Play()
        end
        else
        begin
          // music fade enabled: start muted and fade-in
          AudioPlayback.SetVolume(0);
          AudioPlayback.FadeIn(Ini.PreviewFading, PreviewVolume);
        end;
      end;
    end;
  end;
end;

procedure TScreenScore.StartVoice;
begin
  //Music.Close;
  //ScreenSong.SongIndex := -1;
  //changed := false;

  // not implement voice yet
  {
  if (ScreenSong.Mode = smMedley) then
  begin
    if (CurrentRound<Length(PlaylistMedley.Stats)-1) and (Voice <> CurrentRound)  then
    begin
      Voice := CurrentRound;
      changed := true;
      SetLength(files, PlaylistMedley.NumPlayer);
      for I := 0 to High(files) do
        files[I] := PlaylistMedley.Stats[Voice].Player[I].VoiceFile;
    end;
  end else
  begin
    Voice := 0;
    changed := true;
    SetLength(files, PlayersPlay);
    for I := 0 to High(files) do
      files[I] := Player[I].VoiceFile;
  end;

  if changed then
  begin
    Music.VoicesClose;
    if (Music.VoicesOpen(files)>0) then
      Music.VoicesPlay;
  end;
  }

end;


end.
