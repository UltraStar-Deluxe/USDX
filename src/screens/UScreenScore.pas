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
  TPlayerScoreScreenTexture = record            // holds all colorized textures for up to 6 players
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
    Position: byte; // 1..6: Position of Player; 0: no position (e.g. too little screens)
    Screen: byte;   // 0 - Screen 1; 1 - Screen 2
    BothScreens: boolean; // true if player is drawn on both screens
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
      AvatarStatic:         array[1..UIni.IMaxPlayerCount] of integer;
      AvatarStaticRef:      array[1..UIni.IMaxPlayerCount] of Integer;
      { texture pairs for swapping when screens = 2
        first array level: index of player ( actually this is a position
          1    - Player 1 if PlayersPlay = 1 <- we don't need swapping here
          2..3 - Player 1 and 2 or 3 and 4 if PlayersPlay = 2 or 4
          4..6 - Player 1 - 3 or 4 - 6 if PlayersPlay = 3 or 6 )
        second array level: different playerstatics for positions
        third array level: texture for screen 1 or 2 }
      PlayerStaticTextures: array[1..UIni.IMaxPlayerCount] of array of array [1..2] of TPlayerStaticTexture;
      PlayerTexts:          array[1..UIni.IMaxPlayerCount] of array of integer;

      StaticBoxLightest:    array[1..UIni.IMaxPlayerCount] of integer;
      StaticBoxLight:       array[1..UIni.IMaxPlayerCount] of integer;
      StaticBoxDark:        array[1..UIni.IMaxPlayerCount] of integer;
      { texture pairs for swapping when screens = 2
        for boxes
        first array level: index of player ( actually this is a position
          1    - Player 1 if PlayersPlay = 1 <- we don't need swapping here
          2..3 - Player 1 and 2 or 3 and 4 if PlayersPlay = 2 or 4
          4..6 - Player 1 - 3 or 4 - 6 if PlayersPlay = 3 or 6 )
        second array level: different boxes for positions (0: lightest; 1: light; 2: dark)
        third array level: texture for screen 1 or 2 }
      PlayerBoxTextures: array[1..UIni.IMaxPlayerCount] of array[0..2] of array [1..2] of TPlayerStaticTexture;

      StaticBackLevel:      array[1..UIni.IMaxPlayerCount] of integer;
      StaticBackLevelRound: array[1..UIni.IMaxPlayerCount] of integer;
      StaticLevel:          array[1..UIni.IMaxPlayerCount] of integer;
      StaticLevelRound:     array[1..UIni.IMaxPlayerCount] of integer;

      Animation:            real;
      Voice:                integer;

      TextScore_ActualValue:  array[1..UIni.IMaxPlayerCount] of integer;
      TextPhrase_ActualValue: array[1..UIni.IMaxPlayerCount] of integer;
      TextGolden_ActualValue: array[1..UIni.IMaxPlayerCount] of integer;

      ButtonSend: array[1..UIni.IMaxPlayerCount] of integer;
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
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
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
  UPathUtils,
  UScreenPopup,
  UScreenSong,
  USkins,
  USong,
  UTime,
  UUnicodeUtils;

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

function TScreenScore.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
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
  min_y, max_y, min_x, max_x: real;
  button_s: integer;
begin
  Result := True;

  //TODO: adapt for players 7 to 12
  case PlayersPlay of
    1 : button_s := ButtonSend[1];
    2, 4: button_s := ButtonSend[2];
    3, 6: button_s := ButtonSend[3];
  else
    button_s := ButtonSend[3];
  end;

  min_x := Button[button_s].X;
  min_y := Button[button_s].Y;
  max_x := Button[button_s].X + Button[button_s].W;
  max_y := Button[button_s].Y + Button[button_s].H;

  // transfer mousecords to the 800x600 raster we use to draw
  X := Round((X / (Screen^.w / Screens)) * RenderW);
  if (X > RenderW) then
    X := X - RenderW;
  Y := Round((Y / Screen^.h) * RenderH);

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

//TODO: adapt for players 7 to 12
procedure TScreenScore.LoadSwapTextures;
  var
    P, I: integer;
    PlayerNum, PlayerNum2: integer;
    Color: string;
    R, G, B: real;
    StaticNum: integer;
    ThemeStatic: TThemeStatic;
begin
  { we only need to load swapping textures if in dualscreen mode }
  if Screens = 2 then
  begin
    { load swapping textures for custom statics }
    for P := low(PlayerStatic) to High(PlayerStatic) do
    begin
      SetLength(PlayerStaticTextures[P], Length(PlayerStatic[P]));

      { get the players that actually are on this position }
      case P of
        1: begin
          PlayerNum := 1;
          PlayerNum2 := 1;
        end;

        2, 3: begin
          PlayerNum := P - 1;
          PlayerNum2 := PlayerNum + 2;
        end;

        4..6: begin
          PlayerNum := P - 3;
          PlayerNum2 := PlayerNum + 3;
        end;
      end;

      for I := 0 to High(PlayerStatic[P]) do
      begin
        // copy current statics texture to texture for screen 1
        PlayerStaticTextures[P, I, 1].Tex := Statics[PlayerStatic[P, I]].Texture;

        // fallback to first screen texture for 2nd screen
        PlayerStaticTextures[P, I, 2].Tex := PlayerStaticTextures[P, I, 1].Tex;

        { texture for second screen }
        { we only change color for statics with playercolor
          and with Texture type colorized
          also we don't need to swap for one player position }
        if (P <> 1) and
           (Theme.Score.PlayerStatic[P, I].Typ = Texture_Type_Colorized) and
           (Length(Theme.Score.PlayerStatic[P, I].Color) >= 2) and
           (copy(Theme.Score.PlayerStatic[P, I].Color, 1, 2) = 'P' + IntToStr(PlayerNum)) then
        begin
          // get the color
          Color := Theme.Score.PlayerStatic[P, I].Color;
          Color[2] := IntToStr(PlayerNum2)[1];
          LoadColor(R, G, B, Color);

          with Theme.Score.PlayerStatic[P, I] do
            PlayerStaticTextures[P, I, 2].Tex := Texture.GetTexture(Skin.GetTextureFileName(Tex), Typ, RGBFloatToInt(R, G, B));

          PlayerStaticTextures[P, I, 2].Tex.X := Statics[PlayerStatic[P, I]].Texture.X;
          PlayerStaticTextures[P, I, 2].Tex.Y := Statics[PlayerStatic[P, I]].Texture.Y;
          PlayerStaticTextures[P, I, 2].Tex.W := Statics[PlayerStatic[P, I]].Texture.W;
          PlayerStaticTextures[P, I, 2].Tex.H := Statics[PlayerStatic[P, I]].Texture.H;
        end;
      end;
    end;

    { load swap textures for boxes }
    for P := low(PlayerBoxTextures) to High(PlayerBoxTextures) do
    begin
      { get the players that actually are on this position }
      case P of
        1: begin
          PlayerNum := 1;
          PlayerNum2 := 1;
        end;

        2, 3: begin
          PlayerNum := P - 1;
          PlayerNum2 := PlayerNum + 2;
        end;

        4..6: begin
          PlayerNum := P - 3;
          PlayerNum2 := PlayerNum + 3;
        end;
      end;

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
        // copy current statics texture to texture for screen 1
        PlayerBoxTextures[P, I, 1].Tex := Statics[StaticNum].Texture;

        // fallback to first screen texture for 2nd screen
        PlayerBoxTextures[P, I, 2].Tex := PlayerBoxTextures[P, I, 1].Tex;

        { texture for second screen }
        { we only change color for statics with playercolor
          and with Texture type colorized
          also we don't need to swap for one player position }
        if (P <> 1) and
           (ThemeStatic.Typ = Texture_Type_Colorized) and
           (Length(ThemeStatic.Color) >= 2) and
           (copy(ThemeStatic.Color, 1, 2) = 'P' + IntToStr(PlayerNum)) then
        begin
          // get the color
          Color := ThemeStatic.Color;
          Color[2] := IntToStr(PlayerNum2)[1];
          LoadColor(R, G, B, Color);

          with ThemeStatic do
            PlayerBoxTextures[P, I, 2].Tex := Texture.GetTexture(Skin.GetTextureFileName(Tex), Typ, RGBFloatToInt(R, G, B));

            PlayerBoxTextures[P, I, 2].Tex.X := Statics[StaticNum].Texture.X;
            PlayerBoxTextures[P, I, 2].Tex.Y := Statics[StaticNum].Texture.Y;
            PlayerBoxTextures[P, I, 2].Tex.W := Statics[StaticNum].Texture.W;
            PlayerBoxTextures[P, I, 2].Tex.H := Statics[StaticNum].Texture.H;
        end;
      end;
    end;
  end;
end;

//TODO: adapt for players 7 to 12
procedure TScreenScore.SwapToScreen(Screen: integer);
var
  P, I, J, Max: integer;
  Col: TRGB;
begin

  case PlayersPlay of
    1:    Max := 1;
    2, 4: Max := 2;
    3, 6: Max := 3;
    8:    Max := 4;
    12:   Max := 6;
  else
    Max := 0; //this should never happen
  end;

  { if screens = 2 and playerplay <= 3 the 2nd screen shows the
    textures of screen 1 }
  if (PlayersPlay <= 3) and (Screen = 2) then
    Screen := 1;

  { set correct box textures }
  if (Screens = 2) then
  begin

    for I:= 0 to Max - 1 do
    begin

      if (Screen = 2) then
        Col := GetPlayerColor(Ini.PlayerColor[I + Max])
      else
        Col := GetPlayerColor(Ini.PlayerColor[I]);

      if (copy(Theme.Score.TextName[I + 1 + Max].Color, 1, 2) = 'P' + IntToStr(I + 1)) then
      begin
        Text[TextName[I + 1 + Max]].ColR := Col.R;
        Text[TextName[I + 1 + Max]].ColG := Col.G;
        Text[TextName[I + 1 + Max]].ColB := Col.B;
      end;

      if (copy(Theme.Score.TextScore[I + 1 + Max].Color, 1, 2) = 'P' + IntToStr(I + 1)) then
      begin
        Text[TextScore[I + 1 + Max]].ColR := Col.R;
        Text[TextScore[I + 1 + Max]].ColG := Col.G;
        Text[TextScore[I + 1 + Max]].ColB := Col.B;
      end;

      if (copy(Theme.Score.TextNotes[I + 1 + Max].Color, 1, 2) = 'P' + IntToStr(I + 1)) then
      begin
        Text[TextNotes[I + 1 + Max]].ColR := Col.R;
        Text[TextNotes[I + 1 + Max]].ColG := Col.G;
        Text[TextNotes[I + 1 + Max]].ColB := Col.B;
      end;

      if (copy(Theme.Score.TextNotesScore[I + 1 + Max].Color, 1, 2) = 'P' + IntToStr(I + 1)) then
      begin
        Text[TextNotesScore[I + 1 + Max]].ColR := Col.R;
        Text[TextNotesScore[I + 1 + Max]].ColG := Col.G;
        Text[TextNotesScore[I + 1 + Max]].ColB := Col.B;
      end;

      if (copy(Theme.Score.TextLineBonus[I + 1 + Max].Color, 1, 2) = 'P' + IntToStr(I + 1)) then
      begin
        Text[TextLineBonus[I + 1 + Max]].ColR := Col.R;
        Text[TextLineBonus[I + 1 + Max]].ColG := Col.G;
        Text[TextLineBonus[I + 1 + Max]].ColB := Col.B;
      end;

      if (copy(Theme.Score.TextLineBonusScore[I + 1 + Max].Color, 1, 2) = 'P' + IntToStr(I + 1)) then
      begin
        Text[TextLineBonusScore[I + 1 + Max]].ColR := Col.R;
        Text[TextLineBonusScore[I + 1 + Max]].ColG := Col.G;
        Text[TextLineBonusScore[I + 1 + Max]].ColB := Col.B;
      end;

      if (copy(Theme.Score.TextGoldenNotes[I + 1 + Max].Color, 1, 2) = 'P' + IntToStr(I + 1)) then
      begin
        Text[TextGoldenNotes[I + 1 + Max]].ColR := Col.R;
        Text[TextGoldenNotes[I + 1 + Max]].ColG := Col.G;
        Text[TextGoldenNotes[I + 1 + Max]].ColB := Col.B;
      end;

      if (copy(Theme.Score.TextGoldenNotesScore[I + 1 + Max].Color, 1, 2) = 'P' + IntToStr(I + 1)) then
      begin
        Text[TextGoldenNotesScore[I + 1 + Max]].ColR := Col.R;
        Text[TextGoldenNotesScore[I + 1 + Max]].ColG := Col.G;
        Text[TextGoldenNotesScore[I + 1 + Max]].ColB := Col.B;
      end;

      if (copy(Theme.Score.TextTotal[I + 1 + Max].Color, 1, 2) = 'P' + IntToStr(I + 1)) then
      begin
        Text[TextTotal[I + 1 + Max]].ColR := Col.R;
        Text[TextTotal[I + 1 + Max]].ColG := Col.G;
        Text[TextTotal[I + 1 + Max]].ColB := Col.B;
      end;

      if (copy(Theme.Score.TextTotalScore[I + 1 + Max].Color, 1, 2) = 'P' + IntToStr(I + 1)) then
      begin
        Text[TextTotalScore[I + 1 + Max]].ColR := Col.R;
        Text[TextTotalScore[I + 1 + Max]].ColG := Col.G;
        Text[TextTotalScore[I + 1 + Max]].ColB := Col.B;
      end;
      if((PlayersPlay > Max) and (Screen = 2)) then
      begin
        Statics[AvatarStaticRef[PlayersPlay-Max+I+1]].Visible:=true;
      end
      else if((PlayersPlay > Max) and (Screen = 1)) then
      begin
        Statics[AvatarStaticRef[PlayersPlay-Max+I+1]].Visible:=false;
      end;
    end;

    { to keep it simple we just swap all statics, not just the shown ones }
    for P := Low(PlayerStatic) to High(PlayerStatic) do
      for I := 0 to High(PlayerStatic[P]) do
      begin
        Statics[PlayerStatic[P, I]].Texture := PlayerStaticTextures[P, I, Screen].Tex;
      end;

    { box statics }
    for P := Low(PlayerStatic) to High(PlayerStatic) do
    begin
      Statics[StaticBoxLightest[P]].Texture := PlayerBoxTextures[P, 0, Screen].Tex;
      Statics[StaticBoxLight[P]].Texture := PlayerBoxTextures[P, 1, Screen].Tex;
      Statics[StaticBoxDark[P]].Texture := PlayerBoxTextures[P, 2, Screen].Tex;
    end;
  end;
end;

constructor TScreenScore.Create;
var
  Player:  integer;
  Counter: integer;
  I: integer;
  Col: TRGB;
  R, G, B: real;
  Col2: integer;
  ArrayStartModifier: integer;
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
    LoadColor(R, G, B, 'P' + IntToStr(Player) + 'Dark');
    Col2 := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    Tex_Score_NoteBarLevel_Dark[Player] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreLevel_Dark'), TEXTURE_TYPE_COLORIZED, Col2);
    Tex_Score_NoteBarRound_Dark[Player] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreLevel_Dark_Round'), TEXTURE_TYPE_COLORIZED, Col2);

    //LineBonus ScoreBar
    LoadColor(R, G, B, 'P' + IntToStr(Player) + 'Light');
    Col2 := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    Tex_Score_NoteBarLevel_Light[Player] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreLevel_Light'), TEXTURE_TYPE_COLORIZED, Col2);
    Tex_Score_NoteBarRound_Light[Player] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreLevel_Light_Round'), TEXTURE_TYPE_COLORIZED, Col2);

    //GoldenNotes ScoreBar
    LoadColor(R, G, B, 'P' + IntToStr(Player) + 'Lightest');
    Col2 := $10000 * Round(R*255) + $100 * Round(G*255) + Round(B*255);
    Tex_Score_NoteBarLevel_Lightest[Player] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreLevel_Lightest'), TEXTURE_TYPE_COLORIZED, Col2);
    Tex_Score_NoteBarRound_Lightest[Player] := Texture.LoadTexture(Skin.GetTextureFileName('ScoreLevel_Lightest_Round'), TEXTURE_TYPE_COLORIZED, Col2);

    //textures
    aPlayerScoreScreenTextures[Player].Score_NoteBarLevel_Dark     := Tex_Score_NoteBarLevel_Dark[Player];
    aPlayerScoreScreenTextures[Player].Score_NoteBarRound_Dark     := Tex_Score_NoteBarRound_Dark[Player];

    aPlayerScoreScreenTextures[Player].Score_NoteBarLevel_Light    := Tex_Score_NoteBarLevel_Light[Player];
    aPlayerScoreScreenTextures[Player].Score_NoteBarRound_Light    := Tex_Score_NoteBarRound_Light[Player];

    aPlayerScoreScreenTextures[Player].Score_NoteBarLevel_Lightest := Tex_Score_NoteBarLevel_Lightest[Player];
    aPlayerScoreScreenTextures[Player].Score_NoteBarRound_Lightest := Tex_Score_NoteBarRound_Lightest[Player];
  end;

  //TODO: adapt for players 7 to 12
  // avatars
  case PlayersPlay of
    1: ArrayStartModifier := 0;
    2: ArrayStartModifier := 1;
    3: ArrayStartModifier := 3;
    4: begin
          if (Screens = 1) then
            ArrayStartModifier := 0
          else
            ArrayStartModifier := 1;
       end;
    6: begin
          if (Screens = 1) then
            ArrayStartModifier := 0
          else
            ArrayStartModifier := 3;
       end;
    else
      ArrayStartModifier := 0; //this should never happen
  end;

  for I := 1 to PlayersPlay do
  begin
    if((Screens = 2) and (PlayersPlay > 3) and (I > Trunc(PlayersPlay/2))) then
    begin
      AvatarStatic[I + ArrayStartModifier] := AddStatic(Theme.Score.AvatarStatic[I-Trunc(PlayersPlay/2) + ArrayStartModifier]);
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture := AvatarPlayerTextures[I];
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture.X := Theme.Score.AvatarStatic[I-Trunc(PlayersPlay/2) + ArrayStartModifier].X;
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture.Y := Theme.Score.AvatarStatic[I-Trunc(PlayersPlay/2) + ArrayStartModifier].Y;
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture.H := Theme.Score.AvatarStatic[I-Trunc(PlayersPlay/2) + ArrayStartModifier].H;
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture.W := Theme.Score.AvatarStatic[I-Trunc(PlayersPlay/2) + ArrayStartModifier].W;
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture.Z := Theme.Score.AvatarStatic[I-Trunc(PlayersPlay/2) + ArrayStartModifier].Z;
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture.Alpha := Theme.Score.AvatarStatic[I-Trunc(PlayersPlay/2) + ArrayStartModifier].Alpha;
    end
    else
    begin
      AvatarStatic[I + ArrayStartModifier] := AddStatic(Theme.Score.AvatarStatic[I + ArrayStartModifier]);
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture := AvatarPlayerTextures[I];
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture.X := Theme.Score.AvatarStatic[I + ArrayStartModifier].X;
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture.Y := Theme.Score.AvatarStatic[I + ArrayStartModifier].Y;
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture.H := Theme.Score.AvatarStatic[I + ArrayStartModifier].H;
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture.W := Theme.Score.AvatarStatic[I + ArrayStartModifier].W;
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture.Z := Theme.Score.AvatarStatic[I + ArrayStartModifier].Z;
      Statics[AvatarStatic[I + ArrayStartModifier]].Texture.Alpha := Theme.Score.AvatarStatic[I + ArrayStartModifier].Alpha;
    end;
    Statics[AvatarStatic[I + ArrayStartModifier]].Visible := true;
    AvatarStaticRef[I]:=AvatarStatic[I + ArrayStartModifier];
  end;

  StaticNavigate := AddStatic(Theme.Score.StaticNavigate);
  TextNavigate := AddText(Theme.Score.TextNavigate);

  if (PlayersPlay <= 3) or (Screens = 2) then
    LoadSwapTextures;

  //TODO: adapt for players 4 to 12
  //Send Buttons
  for I := 1 to 3 do
    ButtonSend[I] := AddButton(Theme.Score.ButtonSend[I]);

end;

//TODO: adapt for players 7 to 12
procedure TScreenScore.MapPlayersToPosition;
  var
    ArrayStartModifier: integer;
    PlayersPerScreen: integer;
    I: integer;
begin
  // all statics / texts are loaded at start - so that we have them all even if we change the amount of players
  // To show the corrects statics / text from the them, we simply modify the start of the according arrays
  // 1 Player -> Player[0].Score         (The score for one player starts at 0)
  //          -> Statics[1]              (The statics for the one player screen start at 1)
  // 2 Player -> Player[0..1].Score
  //          -> Statics[2..3]
  // 3 Player -> Player[0..5].Score
  //          -> Statics[4..6]
  case PlayersPlay of
    1:    ArrayStartModifier := 1;
    2, 4: ArrayStartModifier := 2;
    3, 6: ArrayStartModifier := 4;
  else
    ArrayStartModifier := 0; //this should never happen
  end;

  if (PlayersPlay <= 3) or ((PlayersPlay > 3) and (Screens = 1)) then
    PlayersPerScreen := PlayersPlay
  else
    PlayersPerScreen := PlayersPlay div 2;

  SetLength(PlayerPositionMap, PlayersPlay);

  // actually map players to positions
  if (PlayersPlay <= 3) or (Screens = 2) then
  begin
    for I := 0 to PlayersPlay - 1 do
    begin
      PlayerPositionMap[I].Screen := (I div PlayersPerScreen) + 1;
      if (PlayerPositionMap[I].Screen > Screens) then
        PlayerPositionMap[I].Position := 0
      else
        PlayerPositionMap[I].Position := ArrayStartModifier + (I mod PlayersPerScreen);
      PlayerPositionMap[I].BothScreens := (PlayersPlay <= 3) and (Screens > 1);
    end;
  end
  else
  begin
    for I := 0 to PlayersPlay - 1 do
    begin
      PlayerPositionMap[I].Screen := 0;
      PlayerPositionMap[I].Position := I + 1;
      PlayerPositionMap[I].BothScreens := true;
    end;
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
    if (PlayerPositionMap[I].Position > 0) and ((ScreenAct = PlayerPositionMap[I].Screen) or (PlayerPositionMap[I].BothScreens)) then
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
  ArrayStartModifier: integer;
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

  //TODO: adapt for players 7 to 12
  // set visibility
  case PlayersPlay of
    1:  begin
          V[1] := true;
          V[2] := false;
          V[3] := false;
          V[4] := false;
          V[5] := false;
          V[6] := false;
        end;
    2, 4:  begin
          if (PlayersPlay = 2) or ((PlayersPlay = 4) and (Screens = 2)) then
          begin
            V[1] := false;
            V[2] := true;
            V[3] := true;
            V[4] := false;
            V[5] := false;
            V[6] := false;
          end
          else
          begin
            V[1] := true;
            V[2] := true;
            V[3] := true;
            V[4] := true;
            V[5] := false;
            V[6] := false;
          end;
        end;
    3, 6:  begin
          if (PlayersPlay = 3) or ((PlayersPlay = 6) and (Screens = 2)) then
          begin
            V[1] := false;
            V[2] := false;
            V[3] := false;
            V[4] := true;
            V[5] := true;
            V[6] := true;
          end
          else
          begin
            V[1] := true;
            V[2] := true;
            V[3] := true;
            V[4] := true;
            V[5] := true;
            V[6] := true;
          end;
        end;
  end;

  for P := 1 to UIni.IMaxPlayerCount do
  begin
    Text[TextName[P]].Visible               := V[P];
    Text[TextScore[P]].Visible              := V[P];

    Text[TextNotes[P]].Visible              := V[P];
    Text[TextNotesScore[P]].Visible         := V[P];
    Text[TextLineBonus[P]].Visible          := V[P];
    Text[TextLineBonusScore[P]].Visible     := V[P];
    Text[TextGoldenNotes[P]].Visible        := V[P];
    Text[TextGoldenNotesScore[P]].Visible   := V[P];
    Text[TextTotal[P]].Visible              := V[P];
    Text[TextTotalScore[P]].Visible         := V[P];

    for I := 0 to high(PlayerStatic[P]) do
      Statics[PlayerStatic[P, I]].Visible    := V[P];

    for I := 0 to high(PlayerTexts[P]) do
      Text[PlayerTexts[P, I]].Visible       := V[P];

    Statics[StaticBoxLightest[P]].Visible    := V[P];
    Statics[StaticBoxLight[P]].Visible       := V[P];
    Statics[StaticBoxDark[P]].Visible        := V[P];

    // we draw that on our own
    Statics[StaticBackLevel[P]].Visible      := false;
    Statics[StaticBackLevelRound[P]].Visible := false;
    Statics[StaticLevel[P]].Visible          := false;
    Statics[StaticLevelRound[P]].Visible     := false;
  end;

  for I := 0 to 2 do
  begin
    Button[I].Visible := false;
    Button[I].Selectable := false;
  end;

  // Show Send Score Buttons
  if (ScreenSing.SungPaused = false) and (ScreenSing.SungToEnd) and (Length(DllMan.Websites) > 0) then
  begin
    case PlayersPlay of
      1: begin
           Button[0].Visible := true;
           Button[0].Selectable := true;
         end;
      2,4: begin
             Button[1].Visible := true;
             Button[1].Selectable := true;
           end;
      3,6: begin
             Button[2].Visible := true;
             Button[2].Selectable := true;
           end;
    end;
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
  begin
    // We set alpha to 0 , so we can nicely blend them in when we need them
    Text[TextScore[P]].Alpha                   := 0;
    Text[TextNotesScore[P]].Alpha              := 0;
    Text[TextNotes[P]].Alpha                   := 0;
    Text[TextLineBonus[P]].Alpha               := 0;
    Text[TextLineBonusScore[P]].Alpha          := 0;
    Text[TextGoldenNotes[P]].Alpha             := 0;
    Text[TextGoldenNotesScore[P]].Alpha        := 0;
    Text[TextTotal[P]].Alpha                   := 0;
    Text[TextTotalScore[P]].Alpha              := 0;
    Statics[StaticBoxLightest[P]].Texture.Alpha := 0;
    Statics[StaticBoxLight[P]].Texture.Alpha    := 0;
    Statics[StaticBoxDark[P]].Texture.Alpha     := 0;
  end;

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
  for PlayerCounter := 1 to PlayersPlay do       //TODO: adapt for players 7 to 12
  begin
    FillPlayerItems(PlayerCounter);
  end;
   //Draw Theme Objects
  DrawFG;
  Result := true;
end;

procedure TscreenScore.FillPlayerItems(PlayerNumber: integer);
var
  ThemeIndex: integer;
begin
  ThemeIndex := PlayerPositionMap[PlayerNumber-1].Position;
  if (ThemeIndex > 0) and ((ScreenAct = PlayerPositionMap[PlayerNumber-1].Screen) or (PlayerPositionMap[PlayerNumber-1].BothScreens)) then
  begin
    // todo: take the name from player[PlayerNumber].Name instead of the ini when this is done (mog)
    Text[TextName[ThemeIndex]].Text := Ini.Name[PlayerNumber-1];
    // end todo

    //golden
    Text[TextGoldenNotesScore[ThemeIndex]].Text         := IntToStr(TextGolden_ActualValue[PlayerNumber]);
    Text[TextGoldenNotesScore[ThemeIndex]].Alpha        := (BarGolden_EaseOut_Step / 100);

    Statics[StaticBoxLightest[ThemeIndex]].Texture.Alpha := (BarGolden_EaseOut_Step / 100);
    Text[TextGoldenNotes[ThemeIndex]].Alpha             := (BarGolden_EaseOut_Step / 100);

    // line bonus
    Text[TextLineBonusScore[ThemeIndex]].Text           := IntToStr(TextPhrase_ActualValue[PlayerNumber]);
    Text[TextLineBonusScore[ThemeIndex]].Alpha          := (BarPhrase_EaseOut_Step / 100);

    Statics[StaticBoxLight[ThemeIndex]].Texture.Alpha    := (BarPhrase_EaseOut_Step / 100);
    Text[TextLineBonus[ThemeIndex]].Alpha               := (BarPhrase_EaseOut_Step / 100);

    // plain score
    Text[TextNotesScore[ThemeIndex]].Text               := IntToStr(TextScore_ActualValue[PlayerNumber]);
    Text[TextNotes[ThemeIndex]].Alpha                   := (BarScore_EaseOut_Step / 100);

    Statics[StaticBoxDark[ThemeIndex]].Texture.Alpha     := (BarScore_EaseOut_Step / 100);
    Text[TextNotesScore[ThemeIndex]].Alpha              := (BarScore_EaseOut_Step / 100);

    // total score
    Text[TextTotalScore[ThemeIndex]].Text               := IntToStr(TextScore_ActualValue[PlayerNumber] + TextPhrase_ActualValue[PlayerNumber] + TextGolden_ActualValue[PlayerNumber]);
    Text[TextTotalScore[ThemeIndex]].Alpha              := (BarScore_EaseOut_Step / 100);

    Text[TextTotal[ThemeIndex]].Alpha                   := (BarScore_EaseOut_Step / 100);

    if(BarGolden_EaseOut_Step = 100) then
    begin
      ShowRating(PlayerNumber);
    end;
  end;
end;

procedure TScreenScore.ShowRating(PlayerNumber: integer);
var
  Rating: integer;
  ThemeIndex: integer;
begin
  ThemeIndex := PlayerPositionMap[PlayerNumber-1].Position;
  if (ThemeIndex > 0) and ((ScreenAct = PlayerPositionMap[PlayerNumber-1].Screen) or (PlayerPositionMap[PlayerNumber-1].BothScreens)) then
  begin
    case (Player[PlayerNumber-1].ScoreTotalInt) of
     0..2009:
       begin
         Text[TextScore[ThemeIndex]].Text := Language.Translate('SING_SCORE_TONE_DEAF');
         Rating := 0;
       end;
     2010..4009:
       begin
         Text[TextScore[ThemeIndex]].Text := Language.Translate('SING_SCORE_AMATEUR');
         Rating := 1;
       end;
     4010..5009:
       begin
         Text[TextScore[ThemeIndex]].Text := Language.Translate('SING_SCORE_WANNABE');
         Rating := 2;
       end;
     5010..6009:
       begin
         Text[TextScore[ThemeIndex]].Text := Language.Translate('SING_SCORE_HOPEFUL');
         Rating := 3;
       end;
     6010..7509:
       begin
         Text[TextScore[ThemeIndex]].Text := Language.Translate('SING_SCORE_RISING_STAR');
         Rating := 4;
       end;
     7510..8509:
       begin
         Text[TextScore[ThemeIndex]].Text := Language.Translate('SING_SCORE_LEAD_SINGER');
         Rating := 5;
       end;
     8510..9009:
       begin
         Text[TextScore[ThemeIndex]].Text := Language.Translate('SING_SCORE_SUPERSTAR');
         Rating := 6;
       end;
     9010..10000:
       begin
         Text[TextScore[ThemeIndex]].Text := Language.Translate('SING_SCORE_ULTRASTAR');
         Rating := 7;
       end;
    else
      Rating := 0; // Cheata :P
    end;

    //todo: this could break if the width is not given, for instance when there's a skin with no picture for ratings
    if ( Theme.Score.StaticRatings[ThemeIndex].W > 0 ) and  ( aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue > 0 ) then
    begin
      Text[TextScore[ThemeIndex]].Alpha := aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue / Theme.Score.StaticRatings[ThemeIndex].W;
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
  ThemeIndex: integer;
begin
  ThemeIndex := PlayerPositionMap[PlayerNumber-1].Position;

  if (Theme.Score.StaticRatings[ThemeIndex].W <> 0) and (Theme.Score.StaticRatings[ThemeIndex].H <> 0) then
  begin
    PosX := Theme.Score.StaticRatings[ThemeIndex].X + (Theme.Score.StaticRatings[ThemeIndex].W  * 0.5);
    PosY := Theme.Score.StaticRatings[ThemeIndex].Y + (Theme.Score.StaticRatings[ThemeIndex].H  * 0.5); ;

    Width := aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue/2;

    glBindTexture(GL_TEXTURE_2D, Tex_Score_Ratings[Rating].TexNum);

    glColor3f(1.0, 1.0, 1.0);
      
    glEnable(GL_TEXTURE_2D);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);

    glBegin(GL_QUADS);
      glTexCoord2f(0, 0);                                                           glVertex2f(PosX - Width,  PosY - Width);
      glTexCoord2f(Tex_Score_Ratings[Rating].TexW, 0);                              glVertex2f(PosX + Width,  PosY - Width);
      glTexCoord2f(Tex_Score_Ratings[Rating].TexW, Tex_Score_Ratings[Rating].TexH); glVertex2f(PosX + Width,  PosY + Width);
      glTexCoord2f(0, Tex_Score_Ratings[Rating].TexH);                              glVertex2f(PosX - Width,  PosY + Width);
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
  ThemeIndex: integer;
begin
  ThemeIndex := PlayerPositionMap[PlayerNumber-1].Position;

  EaseOut_Step  := aPlayerScoreScreenRatings[PlayerNumber].RateEaseStep;
  MaxVal        := Theme.Score.StaticRatings[ThemeIndex].W;

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
  ThemeIndex: integer;
begin
  ThemeIndex := PlayerPositionMap[PlayerNumber-1].Position;
  MaxHeight    := Theme.Score.StaticBackLevel[ThemeIndex].H;

  // let's get the points according to the bar we draw
  // score array starts at 0, which means the score for player 1 is in score[0]
  // EaseOut_Step is the actual step in the raising process, like the 20iest step of EaseOut_MaxSteps
  if (BarType = sbtScore) then
  begin
    Score        := Player[PlayerNumber - 1].ScoreInt;
    RaiseStep    := BarScore_EaseOut_Step;
    BarStartPosY := Theme.Score.StaticBackLevel[ThemeIndex].Y + MaxHeight;
  end
  else if (BarType = sbtLine) then
  begin
    Score        := Player[PlayerNumber - 1].ScoreLineInt;
    RaiseStep    := BarPhrase_EaseOut_Step;
    BarStartPosY := Theme.Score.StaticBackLevel[ThemeIndex].Y - aPlayerScoreScreenDatas[PlayerNumber].BarScore_ActualHeight + MaxHeight;
  end
  else if (BarType = sbtGolden) then
  begin
    Score        := Player[PlayerNumber - 1].ScoreGoldenInt;
    RaiseStep    := BarGolden_EaseOut_Step;
    BarStartPosY := Theme.Score.StaticBackLevel[ThemeIndex].Y - aPlayerScoreScreenDatas[PlayerNumber].BarScore_ActualHeight - aPlayerScoreScreenDatas[PlayerNumber].BarLine_ActualHeight + MaxHeight;
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
  ThemeIndex: integer;
begin
  ThemeIndex := PlayerPositionMap[PlayerNumber-1].Position;

  // this is solely for better readability of the drawing
  Width        := Theme.Score.StaticBackLevel[ThemeIndex].W;
  BarStartPosX := Theme.Score.StaticBackLevel[ThemeIndex].X;

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
    glTexCoord2f(0, 0); glVertex3f(BarStartPosX,         (BarStartPosY - Statics[StaticLevelRound[ThemeIndex]].Texture.h) - NewHeight, ZBars);
    glTexCoord2f(1, 0); glVertex3f(BarStartPosX + Width, (BarStartPosY - Statics[StaticLevelRound[ThemeIndex]].Texture.h) - NewHeight, ZBars);
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
        if (Ini.PreviewFading = 0) then
        begin
          // music fade disabled: start with full volume
          AudioPlayback.SetVolume(IPreviewVolumeVals[Ini.PreviewVolume]);
          AudioPlayback.Play()
        end
        else
        begin
          // music fade enabled: start muted and fade-in
          AudioPlayback.SetVolume(0);
          AudioPlayback.FadeIn(Ini.PreviewFading, IPreviewVolumeVals[Ini.PreviewVolume]);
        end;
      end;
    end;
  end;
end;

procedure TScreenScore.StartVoice;
var
  changed:  boolean;
  files:    array of string;
  I:        integer;

begin
  //Music.Close;
  //ScreenSong.SongIndex := -1;
  changed := false;

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
