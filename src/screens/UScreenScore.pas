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
 * $URL$
 * $Id$
 *}

unit UScreenScore;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  SDL,
  SysUtils,
  UDisplay,
  UMusic,
  USongs,
  UThemes,
  gl,
  math,
  UTexture;

const
  ZBars            : real = 0.8; // Z value for the bars
  ZRatingPic       : real = 0.8; // Z value for the rating pictures

  EaseOut_MaxSteps : real = 10;  // that's the speed of the bars (10 is fast | 100 is slower)

  BarRaiseSpeed    : cardinal = 0; // Time for raising the bar one step higher (in ms)

type
  TPlayerScoreScreenTexture = record            // holds all colorized textures for up to 6 players
    //Bar textures
    Score_NoteBarLevel_Dark     : TTexture;     // Note
    Score_NoteBarRound_Dark     : TTexture;     // that's the round thing on top

    Score_NoteBarLevel_Light    : TTexture;     // LineBonus | Phrasebonus
    Score_NoteBarRound_Light    : TTexture;

    Score_NoteBarLevel_Lightest : TTexture;     // GoldenNotes
    Score_NoteBarRound_Lightest : TTexture;
  end;

  TPlayerScoreScreenData = record               // holds the positions and other data
    Bar_Y      :Real;
    Bar_Actual_Height      : Real;              // this one holds the actual height of the bar, while we animate it
    BarScore_ActualHeight  : Real;
    BarLine_ActualHeight   : Real;
    BarGolden_ActualHeight : Real;
  end;

  TPlayerScoreRatingPics = record               // a fine array of the rating pictures
    RateEaseStep : Integer;
    RateEaseValue: Real;
  end;

  TScreenScore = class(TMenu)
    private
      BarTime : Cardinal;
      ArrayStartModifier : integer;
    public
      aPlayerScoreScreenTextures   : array[1..6] of TPlayerScoreScreenTexture;
      aPlayerScoreScreenDatas      : array[1..6] of TPlayerScoreScreenData;
      aPlayerScoreScreenRatings    : array[1..6] of TPlayerScoreRatingPics;

      BarScore_EaseOut_Step  : real;
      BarPhrase_EaseOut_Step : real;
      BarGolden_EaseOut_Step : real;

      TextArtist:   integer;
      TextTitle:    integer;

      TextArtistTitle : integer;

      TextName:             array[1..6] of integer;
      TextScore:            array[1..6] of integer;

      TextNotes:            array[1..6] of integer;
      TextNotesScore:       array[1..6] of integer;
      TextLineBonus:        array[1..6] of integer;
      TextLineBonusScore:   array[1..6] of integer;
      TextGoldenNotes:      array[1..6] of integer;
      TextGoldenNotesScore: array[1..6] of integer;
      TextTotal:            array[1..6] of integer;
      TextTotalScore:       array[1..6] of integer;

      PlayerStatic:         array[1..6] of array of integer;
      PlayerTexts :         array[1..6] of array of integer;


      StaticBoxLightest:    array[1..6] of integer;
      StaticBoxLight:       array[1..6] of integer;
      StaticBoxDark:        array[1..6] of integer;

      StaticBackLevel:        array[1..6] of integer;
      StaticBackLevelRound:   array[1..6] of integer;
      StaticLevel:            array[1..6] of integer;
      StaticLevelRound:       array[1..6] of integer;

      Animation:    real;

      TextScore_ActualValue  : array[1..6] of integer;
      TextPhrase_ActualValue : array[1..6] of integer;
      TextGolden_ActualValue : array[1..6] of integer;



      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure onShowFinish; override;
      function Draw: boolean; override;
      procedure FillPlayer(Item, P: integer);

      procedure EaseBarIn(PlayerNumber : Integer; BarType: String);
      procedure EaseScoreIn(PlayerNumber : Integer; ScoreType: String);

      procedure FillPlayerItems(PlayerNumber : Integer; ScoreType: String);


      procedure DrawBar(BarType:string; PlayerNumber: integer; BarStartPosY: single; NewHeight: real);

      //Rating Picture
      procedure ShowRating(PlayerNumber: integer);
        function  CalculateBouncing(PlayerNumber : Integer): real;
        procedure DrawRating(PlayerNumber:integer;Rating:integer);
  end;

implementation


uses UGraphic,
     UScreenSong,
     UMenuStatic,
     UTime,
     UMain,
     UIni,
     ULog,
     ULanguage;

function TScreenScore.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then begin
    // check normal keys
    case WideCharUpperCase(CharCode)[1] of
      'Q':
        begin
          Result := false;
          Exit;
        end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE,
      SDLK_RETURN:
        begin
          FadeTo(@ScreenTop5);
          Exit;
        end;

      SDLK_SYSREQ:
        begin
          Display.SaveScreenShot;
        end;
    end;
  end;
end;

constructor TScreenScore.Create;
var
  Player:  integer;
  Counter: integer;
begin
  inherited Create;

  LoadFromTheme(Theme.Score);

  // These two texts arn't used in the deluxe skin
  TextArtist      := AddText(Theme.Score.TextArtist);
  TextTitle       := AddText(Theme.Score.TextTitle);

  TextArtistTitle := AddText(Theme.Score.TextArtistTitle);

  for Player := 1 to 6 do
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

    //textures
    aPlayerScoreScreenTextures[Player].Score_NoteBarLevel_Dark     := Tex_Score_NoteBarLevel_Dark[Player];
    aPlayerScoreScreenTextures[Player].Score_NoteBarRound_Dark     := Tex_Score_NoteBarRound_Dark[Player];

    aPlayerScoreScreenTextures[Player].Score_NoteBarLevel_Light    := Tex_Score_NoteBarLevel_Light[Player];
    aPlayerScoreScreenTextures[Player].Score_NoteBarRound_Light    := Tex_Score_NoteBarRound_Light[Player];

    aPlayerScoreScreenTextures[Player].Score_NoteBarLevel_Lightest := Tex_Score_NoteBarLevel_Lightest[Player];
    aPlayerScoreScreenTextures[Player].Score_NoteBarRound_Lightest := Tex_Score_NoteBarRound_Lightest[Player];
  end;

end;

procedure TScreenScore.onShow;
var
  P:    integer;  // player
  I:    integer;
  V:    array[1..6] of boolean; // visibility array

begin

{**
 * Turn backgroundmusic on
 *}
 SoundLib.StartBgMusic;

  inherited;

  // all statics / texts are loaded at start - so that we have them all even if we change the amount of players
  // To show the corrects statics / text from the them, we simply modify the start of the according arrays
  // 1 Player -> Player[0].Score         (The score for one player starts at 0)
  //          -> Statics[1]              (The statics for the one player screen start at 1)
  // 2 Player -> Player[0..1].Score
  //          -> Statics[2..3]
  // 3 Player -> Player[0..5].Score
  //          -> Statics[4..6]
  case PlayersPlay of
    1:    ArrayStartModifier := 0;
    2, 4: ArrayStartModifier := 1;
    3, 6: ArrayStartModifier := 3;
  else
    ArrayStartModifier := 0; //this should never happen
  end;

  for P := 1 to PlayersPlay do
  begin
    // data
    aPlayerScoreScreenDatas[P].Bar_Y                  := Theme.Score.StaticBackLevel[P + ArrayStartModifier].Y;

    // ratings
    aPlayerScoreScreenRatings[P].RateEaseStep         := 1;
    aPlayerScoreScreenRatings[P].RateEaseValue        := 20;
  end;


  Text[TextArtist].Text      := CurrentSong.Artist;
  Text[TextTitle].Text       := CurrentSong.Title;
  Text[TextArtistTitle].Text := CurrentSong.Artist + ' - ' + CurrentSong.Title;

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
          V[1] := false;
          V[2] := true;
          V[3] := true;
          V[4] := false;
          V[5] := false;
          V[6] := false;
        end;
    3, 6:  begin
          V[1] := false;
          V[2] := false;
          V[3] := false;
          V[4] := true;
          V[5] := true;
          V[6] := true;
        end;
  end;

  for P := 1 to 6 do
  begin
   Text[TextName[P]].Visible               := V[P];
   Text[TextScore[P]].Visible              := V[P];

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
    Static[StaticBoxLightest[P]].Texture.Alpha := 0;
    Static[StaticBoxLight[P]].Texture.Alpha    := 0;
    Static[StaticBoxDark[P]].Texture.Alpha     := 0;


    Text[TextNotes[P]].Visible              := V[P];
    Text[TextNotesScore[P]].Visible         := V[P];
    Text[TextLineBonus[P]].Visible          := V[P];
    Text[TextLineBonusScore[P]].Visible     := V[P];
    Text[TextGoldenNotes[P]].Visible        := V[P];
    Text[TextGoldenNotesScore[P]].Visible   := V[P];
    Text[TextTotal[P]].Visible              := V[P];
    Text[TextTotalScore[P]].Visible         := V[P];

    for I := 0 to high(PlayerStatic[P]) do
      Static[PlayerStatic[P, I]].Visible    := V[P];

    for I := 0 to high(PlayerTexts[P]) do
      Text[PlayerTexts[P, I]].Visible       := V[P];

    Static[StaticBoxLightest[P]].Visible    := V[P];
    Static[StaticBoxLight[P]].Visible       := V[P];
    Static[StaticBoxDark[P]].Visible        := V[P];

    // we draw that on our own
    Static[StaticBackLevel[P]].Visible      := false;
    Static[StaticBackLevelRound[P]].Visible := false;
    Static[StaticLevel[P]].Visible          := false;
    Static[StaticLevelRound[P]].Visible     := false;
  end;
end;

procedure TScreenScore.onShowFinish;
var
  index : integer;
begin
  for index := 1 to (PlayersPlay) do
    begin
      TextScore_ActualValue[index]  := 0;
      TextPhrase_ActualValue[index] := 0;
      TextGolden_ActualValue[index] := 0;
    end;

  BarScore_EaseOut_Step  := 1;
  BarPhrase_EaseOut_Step := 1;
  BarGolden_EaseOut_Step := 1;
end;

function TScreenScore.Draw: boolean;
var
  CurrentTime : Cardinal;
  PlayerCounter : integer;
begin

  inherited Draw;
{*
  player[0].ScoreInt       := 7000;
  player[0].ScoreLineInt   := 2000;
  player[0].ScoreGoldenInt := 1000;
  player[0].ScoreTotalInt  := 10000;

  player[1].ScoreInt       := 2500;
  player[1].ScoreLineInt   := 1100;
  player[1].ScoreGoldenInt :=  900;
  player[1].ScoreTotalInt  := 4500;
*}
  // Let's start to arise the bars
  CurrentTime := SDL_GetTicks();
  if((CurrentTime >= BarTime) AND ShowFinish) then
  begin
    BarTime := CurrentTime + BarRaiseSpeed;

    for PlayerCounter := 1 to PlayersPlay do
    begin
      // We actually arise them in the right order, but we have to draw them in reverse order (golden -> phrase -> mainscore)
      if (BarScore_EaseOut_Step < EaseOut_MaxSteps * 10) then
        BarScore_EaseOut_Step:= BarScore_EaseOut_Step + 1;

      // PhrasenBonus
      if (BarScore_EaseOut_Step >= (EaseOut_MaxSteps * 10)) then
      begin
        if (BarPhrase_EaseOut_Step < EaseOut_MaxSteps * 10) then
          BarPhrase_EaseOut_Step := BarPhrase_EaseOut_Step + 1;


        // GoldenNotebonus
        if (BarPhrase_EaseOut_Step >= (EaseOut_MaxSteps * 10)) then
        begin
          if (BarGolden_EaseOut_Step < EaseOut_MaxSteps * 10) then
            BarGolden_EaseOut_Step := BarGolden_EaseOut_Step + 1;

          // Draw golden score bar #
          EaseBarIn(PlayerCounter,  'Golden');
          EaseScoreIn(PlayerCounter,'Golden');
        end;

        // Draw phrase score bar #
        EaseBarIn(PlayerCounter,  'Line');
        EaseScoreIn(PlayerCounter,'Line');
      end;

      // Draw plain score bar #
      EaseBarIn(PlayerCounter,  'Note');
      EaseScoreIn(PlayerCounter,'Note');


      FillPlayerItems(PlayerCounter,'Funky');

    end;
  end;


(*
    //todo: i need a clever method to draw statics with their z value
    for I := 0 to Length(Static) - 1 do
      Static[I].Draw;
    for I := 0 to Length(Text) - 1 do
      Text[I].Draw;
*)

  Result := true;
end;

procedure TscreenScore.FillPlayerItems(PlayerNumber : Integer; ScoreType: String);
var
  ThemeIndex: integer;
begin
  // todo: take the name from player[PlayerNumber].Name instead of the ini when this is done (mog)
  Text[TextName[PlayerNumber + ArrayStartModifier]].Text := Ini.Name[PlayerNumber - 1];
  // end todo

  ThemeIndex := PlayerNumber + ArrayStartModifier;

  //golden
  Text[TextGoldenNotesScore[ThemeIndex]].Text         := IntToStr(TextGolden_ActualValue[PlayerNumber]);
  Text[TextGoldenNotesScore[ThemeIndex]].Alpha        := (BarGolden_EaseOut_Step / 100);

  Static[StaticBoxLightest[ThemeIndex]].Texture.Alpha := (BarGolden_EaseOut_Step / 100);
  Text[TextGoldenNotes[ThemeIndex]].Alpha             := (BarGolden_EaseOut_Step / 100);

  // line bonus
  Text[TextLineBonusScore[ThemeIndex]].Text           := IntToStr(TextPhrase_ActualValue[PlayerNumber]);
  Text[TextLineBonusScore[ThemeIndex]].Alpha          := (BarPhrase_EaseOut_Step / 100);

  Static[StaticBoxLight[ThemeIndex]].Texture.Alpha    := (BarPhrase_EaseOut_Step / 100);
  Text[TextLineBonus[ThemeIndex]].Alpha               := (BarPhrase_EaseOut_Step / 100);

  // plain score
  Text[TextNotesScore[ThemeIndex]].Text               := IntToStr(TextScore_ActualValue[PlayerNumber]);
  Text[TextNotes[ThemeIndex]].Alpha                   := (BarScore_EaseOut_Step / 100);

  Static[StaticBoxDark[ThemeIndex]].Texture.Alpha     := (BarScore_EaseOut_Step / 100);
  Text[TextNotesScore[ThemeIndex]].Alpha              := (BarScore_EaseOut_Step / 100);

  // total score
  Text[TextTotalScore[ThemeIndex]].Text               := IntToStr(TextScore_ActualValue[PlayerNumber] + TextPhrase_ActualValue[PlayerNumber] + TextGolden_ActualValue[PlayerNumber]);
  Text[TextTotalScore[ThemeIndex]].Alpha              := (BarScore_EaseOut_Step / 100);

  Text[TextTotal[ThemeIndex]].Alpha                   := (BarScore_EaseOut_Step / 100);

  Text[TextTotal[ThemeIndex]].Alpha                   := (BarScore_EaseOut_Step / 100);

  if(BarGolden_EaseOut_Step = 100) then
  begin
    ShowRating(PlayerNumber);
  end;
end;


procedure TScreenScore.ShowRating(PlayerNumber: integer);
var
  Rating : integer;
  ThemeIndex : integer;
begin

  ThemeIndex := PlayerNumber + ArrayStartModifier;

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
  if ( Theme.Score.StaticRatings[ThemeIndex].W > 0 ) AND  ( aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue > 0 ) then
  begin
    Text[TextScore[ThemeIndex]].Alpha := aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue / Theme.Score.StaticRatings[ThemeIndex].W;
  end;
  // end todo

  DrawRating(PlayerNumber, Rating);
end;

procedure TscreenScore.DrawRating(PlayerNumber:integer;Rating:integer);
var
  Posx : real;
  Posy : real;
  Width :real;
begin

  CalculateBouncing(PlayerNumber);

  PosX := Theme.Score.StaticRatings[PlayerNumber + ArrayStartModifier].X + (Theme.Score.StaticRatings[PlayerNumber + ArrayStartModifier].W  * 0.5);
  PosY := Theme.Score.StaticRatings[PlayerNumber + ArrayStartModifier].Y + (Theme.Score.StaticRatings[PlayerNumber + ArrayStartModifier].H  * 0.5); ;

  Width := aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue/2;

  glBindTexture(GL_TEXTURE_2D, Tex_Score_Ratings[Rating].TexNum);

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



function TscreenScore.CalculateBouncing(PlayerNumber : Integer): real;
var
  ReturnValue : real;
  p, s        : real;

  RaiseStep, MaxVal  : real;
  EaseOut_Step : integer;
begin
  EaseOut_Step  := aPlayerScoreScreenRatings[PlayerNumber].RateEaseStep;
  MaxVal        := Theme.Score.StaticRatings[PlayerNumber + ArrayStartModifier].W;

  RaiseStep     := EaseOut_Step;

  if (MaxVal > 0) AND (RaiseStep > 0) then
    RaiseStep := RaiseStep / MaxVal;

    if (RaiseStep = 1) then
      begin
        ReturnValue := MaxVal;
      end
    else
      begin
        p := MaxVal * 0.4;

        s           := p/(2*PI) * arcsin (1);
        ReturnValue := MaxVal * power(2,-5 * RaiseStep) * sin( (RaiseStep * MaxVal - s) * (2 * PI) / p) + MaxVal;

        inc(aPlayerScoreScreenRatings[PlayerNumber].RateEaseStep);
        aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue := ReturnValue;
      end;

  Result := ReturnValue;
end;


procedure TscreenScore.EaseBarIn(PlayerNumber : Integer; BarType: String);
const
  RaiseSmoothness : integer = 100;
var
  MaxHeight       : real;
  NewHeight       : real;

  Height2Reach    : real;
  RaiseStep       : real;
  BarStartPosY    : single;

  lTmp            : real;
  Score           : integer;
begin
  MaxHeight    := Theme.Score.StaticBackLevel[PlayerNumber + ArrayStartModifier].H;

  // let's get the points according to the bar we draw
  // score array starts at 0, which means the score for player 1 is in score[0]
  // EaseOut_Step is the actual step in the raising process, like the 20iest step of EaseOut_MaxSteps
  if (BarType = 'Note') then
  begin
    Score        := Player[PlayerNumber - 1].ScoreInt;
    RaiseStep    := BarScore_EaseOut_Step;
    BarStartPosY := Theme.Score.StaticBackLevel[PlayerNumber + ArrayStartModifier].Y + MaxHeight;
  end
  else if (BarType = 'Line') then
  begin
    Score        := Player[PlayerNumber - 1].ScoreLineInt;
    RaiseStep    := BarPhrase_EaseOut_Step;
    BarStartPosY := Theme.Score.StaticBackLevel[PlayerNumber + ArrayStartModifier].Y - aPlayerScoreScreenDatas[PlayerNumber].BarScore_ActualHeight + MaxHeight;
  end
  else if (BarType = 'Golden') then
  begin
    Score        := Player[PlayerNumber - 1].ScoreGoldenInt;
    RaiseStep    := BarGolden_EaseOut_Step;
    BarStartPosY := Theme.Score.StaticBackLevel[PlayerNumber + ArrayStartModifier].Y - aPlayerScoreScreenDatas[PlayerNumber].BarScore_ActualHeight - aPlayerScoreScreenDatas[PlayerNumber].BarLine_ActualHeight + MaxHeight;
  end
  else
  begin
    Log.LogCritical('Unknown bar-type: ' + BarType, 'TScreenScore.EaseBarIn');
    Exit; // suppress warnings
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

  if (BarType = 'Note') then
    aPlayerScoreScreenDatas[PlayerNumber].BarScore_ActualHeight  := NewHeight
  else if (BarType = 'Line') then
    aPlayerScoreScreenDatas[PlayerNumber].BarLine_ActualHeight   := NewHeight
  else if (BarType = 'Golden') then
    aPlayerScoreScreenDatas[PlayerNumber].BarGolden_ActualHeight := NewHeight;
end;

procedure TscreenScore.DrawBar(BarType:string; PlayerNumber: integer; BarStartPosY: single; NewHeight: real);
var
  Width:real;
  BarStartPosX:real;
begin
  // this is solely for better readability of the drawing
  Width        := Theme.Score.StaticBackLevel[PlayerNumber + ArrayStartModifier].W;
  BarStartPosX := Theme.Score.StaticBackLevel[PlayerNumber + ArrayStartModifier].X;

  glColor4f(1, 1, 1, 1);

  // set the texture for the bar
  if (BarType = 'Note') then
    glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarLevel_Dark.TexNum);
  if (BarType = 'Line') then
    glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarLevel_Light.TexNum);
  if (BarType = 'Golden') then
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
  if (BarType = 'Note') then
    glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarRound_Dark.TexNum);
  if (BarType = 'Line') then
    glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarRound_Light.TexNum);
  if (BarType = 'Golden') then
    glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarRound_Lightest.TexNum);

  glEnable(GL_TEXTURE_2D);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex3f(BarStartPosX,         (BarStartPosY - Static[StaticLevelRound[PlayerNumber + ArrayStartModifier]].Texture.h) - NewHeight, ZBars);
    glTexCoord2f(1, 0); glVertex3f(BarStartPosX + Width, (BarStartPosY - Static[StaticLevelRound[PlayerNumber + ArrayStartModifier]].Texture.h) - NewHeight, ZBars);
    glTexCoord2f(1, 1); glVertex3f(BarStartPosX + Width,  BarStartPosY - NewHeight,                                                     ZBars);
    glTexCoord2f(0, 1); glVertex3f(BarStartPosX,          BarStartPosY - NewHeight,                                                     ZBars);
  glEnd;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2d);
end;

procedure TScreenScore.EaseScoreIn(PlayerNumber: integer; ScoreType : String);
const
  RaiseSmoothness : integer = 100;
var
  RaiseStep    : Real;
  lTmpA        : Real;
  ScoreReached :Integer;
  EaseOut_Step :Real;
  ActualScoreValue:integer;
begin
  if (ScoreType = 'Note') then
  begin
    EaseOut_Step     := BarScore_EaseOut_Step;
    ActualScoreValue := TextScore_ActualValue[PlayerNumber];
    ScoreReached     := Player[PlayerNumber-1].ScoreInt;
  end;
  if (ScoreType = 'Line') then
  begin
    EaseOut_Step     := BarPhrase_EaseOut_Step;
    ActualScoreValue := TextPhrase_ActualValue[PlayerNumber];
    ScoreReached     := Player[PlayerNumber-1].ScoreLineInt;
  end;
  if (ScoreType = 'Golden') then
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
    if ( lTmpA           > 0 ) AND
       ( RaiseSmoothness > 0 ) then
    begin
      if (ScoreType = 'Note') then
        TextScore_ActualValue[PlayerNumber]  := floor( lTmpA / RaiseSmoothness);
      if (ScoreType = 'Line') then
        TextPhrase_ActualValue[PlayerNumber] := floor( lTmpA / RaiseSmoothness);
      if (ScoreType = 'Golden') then
        TextGolden_ActualValue[PlayerNumber] := floor( lTmpA / RaiseSmoothness);
    end;
  end
  else
  begin
    if (ScoreType = 'Note') then
      TextScore_ActualValue[PlayerNumber]    := ScoreReached;
    if (ScoreType = 'Line') then
      TextPhrase_ActualValue[PlayerNumber]   := ScoreReached;
    if (ScoreType = 'Golden') then
      TextGolden_ActualValue[PlayerNumber]   := ScoreReached;
  end;
end;

procedure TScreenScore.FillPlayer(Item, P: integer);
var
  S:    string;
begin
  Text[TextName[Item]].Text := Ini.Name[P];

  S := IntToStr((Round(Player[P].Score) div 10) * 10);
  while (Length(S)<4) do
    S := '0' + S;
  Text[TextNotesScore[Item]].Text := S;

  //  while (Length(S)<5) do S := '0' + S;
  //  Text[TextTotalScore[Item]].Text := S;

  //fixed: line bonus and golden notes don't show up,
  //       another bug: total score was shown without added golden-, linebonus
  S := IntToStr(Player[P].ScoreTotalInt);
  while (Length(S)<5) do
    S := '0' + S;
  Text[TextTotalScore[Item]].Text := S;

  S := IntToStr(Player[P].ScoreLineInt);
  while (Length(S)<4) do
    S := '0' + S;
  Text[TextLineBonusScore[Item]].Text := S;

  S := IntToStr(Player[P].ScoreGoldenInt);
  while (Length(S)<4) do
    S := '0' + S;
  Text[TextGoldenNotesScore[Item]].Text := S;
  //end of fix


end;

end.
