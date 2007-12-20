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
  OpenGL12,
  {$ifdef win32}
  Windows,
  dialogs,
  {$endif}
  math,
  ULCD,
  UTexture;
//  OpenGL;

const
  ZBars            : real = 0.8; // Z value for the bars
  ZRatingPic       : real = 0.8; // Z value for the rating pictures

  EaseOut_MaxSteps : real = 40;  // that's the speed of the bars (10 is fast | 100 is slower)

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
    Bar_X      :Real;
    Bar_Y      :Real;
    Bar_Height :Real;                           // this is the max height of the bar, who knows if there ever will be a theme with different heights :D
    Bar_Width  :Real;                           // another rather senseless setting, but you never know what our cool users do with the them :)

    Bar_Actual_Height      : Real;              // this one holds the actual height of the bar, while we animate it
    BarScore_ActualHeight  : Real;
    BarLine_ActualHeight   : Real;
    BarGolden_ActualHeight : Real;
  end;

  TPlayerScoreRatingPics = record                    // a fine array of the rating pictures
    RatePic_X      :Real;
    RatePic_Y      :Real;
    RatePic_Height :Real;
    RatePic_Width  :Real;

    RateEaseStep : Integer;
    RateEaseValue: Real;
  end;

  TScreenScore = class(TMenu)
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
      Fadeout:      boolean;

      TextScore_ActualValue  : array[1..6] of integer;
      TextPhrase_ActualValue : array[1..6] of integer;
      TextGolden_ActualValue : array[1..6] of integer;



      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure onShowFinish; override;
      function Draw: boolean; override;
      procedure FillPlayer(Item, P: integer);

      function EaseBarIn(PlayerNumber : Integer; BarType: String) : real;
      function EaseScoreIn(PlayerNumber : Integer; ScoreType: String) : real;

      procedure FillPlayerItems(PlayerNumber : Integer; ScoreType: String);
      procedure ShowRating(PlayerNumber: integer);

      function elastique(PlayerNumber : Integer): real;
  end;

implementation


uses UGraphic,
     UScreenSong,
     UMenuStatic,
     UTime,
     UMain,
     UIni,
     {$IFNDEF win32}
     lclintf,
     {$ENDIF}
     ULanguage;

function TScreenScore.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then begin
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE,
      SDLK_BACKSPACE :

        begin
          if (not Fadeout) then begin
//            Music.StopShuffle;
            FadeTo(@ScreenTop5);
            Fadeout := true;
          end;
        end;
      SDLK_RETURN:
        begin
          if (not Fadeout) then begin
//            Music.StopShuffle;
            FadeTo(@ScreenTop5);
            Fadeout := true;
          end;
        end;
{      SDLK_SYSREQ:
        begin
          beep;
        end;}
      SDLK_SYSREQ:
        begin
          Display.PrintScreen;
        end;
    end;
  end;
end;

constructor TScreenScore.Create;
var
  P                  : integer;
  I, C               : integer;
  ArrayStartModifier : Byte;

begin
  inherited Create;

  LoadFromTheme(Theme.Score);

  TextArtist      := AddText(Theme.Score.TextArtist);
  TextTitle       := AddText(Theme.Score.TextTitle);
  TextArtistTitle := AddText(Theme.Score.TextArtistTitle);

  for P := 1 to 6 do
    begin
    //textures
      aPlayerScoreScreenTextures[P].Score_NoteBarLevel_Dark     := Tex_Score_NoteBarLevel_Dark[P];
      aPlayerScoreScreenTextures[P].Score_NoteBarRound_Dark     := Tex_Score_NoteBarRound_Dark[P];

      aPlayerScoreScreenTextures[P].Score_NoteBarLevel_Light    := Tex_Score_NoteBarLevel_Light[P];
      aPlayerScoreScreenTextures[P].Score_NoteBarRound_Light    := Tex_Score_NoteBarRound_Light[P];

      aPlayerScoreScreenTextures[P].Score_NoteBarLevel_Lightest := Tex_Score_NoteBarLevel_Lightest[P];
      aPlayerScoreScreenTextures[P].Score_NoteBarRound_Lightest := Tex_Score_NoteBarRound_Lightest[P];
    end;


//old stuff

  for P := 1 to 6 do
    begin
      SetLength(PlayerStatic[P], Length(Theme.Score.PlayerStatic[P]));
      SetLength(PlayerTexts[P],  Length(Theme.Score.PlayerTexts[P]));

    for I := 0 to High(Theme.Score.PlayerStatic[P]) do
      PlayerStatic[P, I]      := AddStatic(Theme.Score.PlayerStatic[P, I]);

    for C := 0 to High(Theme.Score.PlayerTexts[P]) do
      PlayerTexts[P, C]       := AddText(Theme.Score.PlayerTexts[P, C]);

      TextName[P]             := AddText(Theme.Score.TextName[P]);
      TextScore[P]            := AddText(Theme.Score.TextScore[P]);

      TextNotes[P]            := AddText(Theme.Score.TextNotes[P]);
      TextNotesScore[P]       := AddText(Theme.Score.TextNotesScore[P]);
      TextLineBonus[P]        := AddText(Theme.Score.TextLineBonus[P]);
      TextLineBonusScore[P]   := AddText(Theme.Score.TextLineBonusScore[P]);
      TextGoldenNotes[P]      := AddText(Theme.Score.TextGoldenNotes[P]);
      TextGoldenNotesScore[P] := AddText(Theme.Score.TextGoldenNotesScore[P]);
      TextTotal[P]            := AddText(Theme.Score.TextTotal[P]);
      TextTotalScore[P]       := AddText(Theme.Score.TextTotalScore[P]);

      StaticBoxLightest[P]    := AddStatic(Theme.Score.StaticBoxLightest[P]);
      StaticBoxLight[P]       := AddStatic(Theme.Score.StaticBoxLight[P]);
      StaticBoxDark[P]        := AddStatic(Theme.Score.StaticBoxDark[P]);

      StaticBackLevel[P]      := AddStatic(Theme.Score.StaticBackLevel[P]);
      StaticBackLevelRound[P] := AddStatic(Theme.Score.StaticBackLevelRound[P]);
      StaticLevel[P]          := AddStatic(Theme.Score.StaticLevel[P]);
      StaticLevelRound[P]     := AddStatic(Theme.Score.StaticLevelRound[P]);
  end;

end;

procedure TScreenScore.onShow;
var
  P:    integer;  // player
  PP:   integer;  // another player variable
  S:    string;
  I:    integer;
  Lev:  real;
  Skip: integer;
  V:    array[1..6] of boolean; // visibility array
  MaxH: real; // maximum height of score bar
  Wsp:  real;
  ArrayStartModifier :integer;
begin

  case PlayersPlay of
    1:    begin
            ArrayStartModifier := 0;
          end;
    2, 4: begin
            ArrayStartModifier := 1;
          end;
    3, 6: begin
            ArrayStartModifier := 3;
          end;
    end;

  for P := 1 to PlayersPlay do
    begin
    // data
      aPlayerScoreScreenDatas[P].Bar_X                  := Theme.Score.StaticBackLevel[P + ArrayStartModifier].X;
      aPlayerScoreScreenDatas[P].Bar_Y                  := Theme.Score.StaticBackLevel[P + ArrayStartModifier].Y;
      aPlayerScoreScreenDatas[P].Bar_Height             := Theme.Score.StaticBackLevel[P + ArrayStartModifier].H;
      aPlayerScoreScreenDatas[P].Bar_Width              := Theme.Score.StaticBackLevel[P + ArrayStartModifier].W;

      aPlayerScoreScreenDatas[P].Bar_Actual_Height      := 0;
      aPlayerScoreScreenDatas[P].BarScore_ActualHeight  := 0;
      aPlayerScoreScreenDatas[P].BarLine_ActualHeight   := 0;
      aPlayerScoreScreenDatas[P].BarGolden_ActualHeight := 0;

    // ratings
      aPlayerScoreScreenRatings[P].RatePic_X            := Theme.Score.StaticRatings[P + ArrayStartModifier].X;
      aPlayerScoreScreenRatings[P].RatePic_Y            := Theme.Score.StaticRatings[P + ArrayStartModifier].Y;
      aPlayerScoreScreenRatings[P].RatePic_Height       := Theme.Score.StaticRatings[P + ArrayStartModifier].H;
      aPlayerScoreScreenRatings[P].RatePic_Width        := Theme.Score.StaticRatings[P + ArrayStartModifier].W;
      aPlayerScoreScreenRatings[P].RateEaseStep         := 1;
      aPlayerScoreScreenRatings[P].RateEaseValue        := 20;
    end;


  // Singstar
  Fadeout := false;

  Text[TextArtist].Text      := AktSong.Artist;
  Text[TextTitle].Text       := AktSong.Title;
  Text[TextArtistTitle].Text := AktSong.Artist + ' - ' + AktSong.Title;

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
{  Min:    real;
  Max:    real;
  Wsp:    real;
  Wsp2:   real;
  Pet:    integer;}

  ActualTime, OldTime : Integer;

  Item:   integer;
  P:      integer;
  C, I:      integer;

  CurTime: Cardinal;

  PlayerCounter : integer;

begin
{

  // 0.5.0: try also use 4 players screen with nicks
  if PlayersPlay = 4 then begin
    for Item := 2 to 3 do begin
      if ScreenAct = 1 then P := Item-2;
      if ScreenAct = 2 then P := Item;

      FillPlayer(Item, P);
    end;
  end;


  // Singstar - let it be...... with 6 statics
  if PlayersPlay = 6 then begin
    for Item := 4 to 6 do begin
      if ScreenAct = 1 then P := Item-4;
      if ScreenAct = 2 then P := Item-1;

      FillPlayer(Item, P);

      if ScreenAct = 1 then begin
        LoadColor(
          Static[StaticBoxLightest[Item]].Texture.ColR,
          Static[StaticBoxLightest[Item]].Texture.ColG,
          Static[StaticBoxLightest[Item]].Texture.ColB,
          'P1Dark');
      end;

      if ScreenAct = 2 then begin
        LoadColor(
          Static[StaticBoxLightest[Item]].Texture.ColR,
          Static[StaticBoxLightest[Item]].Texture.ColG,
          Static[StaticBoxLightest[Item]].Texture.ColB,
          'P4Dark');
      end;

    end;


  end;
}
inherited Draw;
  {*
  player[0].ScoreI       := 7000;
  player[0].ScoreLineI   := 2000;
  player[0].ScoreGoldenI := 1000;
  player[0].ScoreTotalI  := 10000;

  player[1].ScoreI       := 2500;
  player[1].ScoreLineI   := 1100;
  player[1].ScoreGoldenI :=  900;
  player[1].ScoreTotalI  := 4500;
 *}

// Let's arise the bars

  ActualTime := GetTickCount div 33;
    if ((ActualTime <> OldTime) and ShowFinish )then
      begin
        OldTime               := ActualTime;

      For PlayerCounter := 1 to PlayersPlay do
        begin

          // We actually araise them in the right order, but we have to draw them in reverse order (golden -> phrase -> mainscore)
          if (BarScore_EaseOut_Step < EaseOut_MaxSteps * 10)
            then BarScore_EaseOut_Step := BarScore_EaseOut_Step + 1;

          // PhrasenBonus
           if (BarScore_EaseOut_Step >= (EaseOut_MaxSteps * 10)) then
            begin
              Case BarPhrase_EaseOut_Step < EaseOut_MaxSteps * 10 of
                true : BarPhrase_EaseOut_Step            := BarPhrase_EaseOut_Step + 1;
              end;

          // GoldenNotebonus
               if (BarPhrase_EaseOut_Step >= (EaseOut_MaxSteps * 10)) then
                begin
                  Case BarGolden_EaseOut_Step < EaseOut_MaxSteps * 10 of
                    true : BarGolden_EaseOut_Step        := BarGolden_EaseOut_Step + 1;
                  end;

        //########################
        // Draw golden score bar #
        //########################

        EaseBarIn(PlayerCounter,  'Golden');  // ease bar for golden notes in
        EaseScoreIn(PlayerCounter,'Golden');

                end;
        //########################
        // Draw phrase score bar #
        //########################

        EaseBarIn(PlayerCounter,  'Line');    // ease bar for line bonus / phrasenbonus notes in
        EaseScoreIn(PlayerCounter,'Line');


            end;
        //#######################
        // Draw plain score bar #
        //#######################


        EaseBarIn(PlayerCounter,  'Note');    // ease bar for all other notes in
        EaseScoreIn(PlayerCounter,'Note');


        FillPlayerItems(PlayerCounter,'Funky');

      end;
    end;
//todo: i need a clever method to draw statics with their z value 
  for I := 0 to Length(Static) - 1 do
    Static[I].Draw;
  for I := 0 to Length(Text) - 1 do
    Text[I].Draw;
end;

procedure TscreenScore.FillPlayerItems(PlayerNumber : Integer; ScoreType: String);
var
  ArrayStartModifier : integer;
begin
// okay i hate that as much as you might do too, but there's no way around that yet (imho)
// all statics / texts are loaded at start - so that we have them all even if we change the amount of players
// array overview:

// 1 Player -> Player[0].Score         (The score for one player starts at 0)
//          -> Statics[1]              (The statics for the one player screen start at 0)
// 2 Player -> Player[0..1].Score
//          -> Statics[2..3]
// 3 Player -> Player[0..5].Score
//          -> Statics[4..6]

  case PlayersPlay of
    1:  begin
          ArrayStartModifier := 0;
        end;
    2, 4:  begin
          ArrayStartModifier := 1;
        end;
    3, 6:  begin
          ArrayStartModifier := 3;
        end;
  end;

// todo: take the name from player[PlayerNumber].Name instead of the ini when this is done (mog)
   Text[TextName[PlayerNumber + ArrayStartModifier]].Text := Ini.Name[PlayerNumber - 1];
// end todo


//golden
    Text[TextGoldenNotesScore[PlayerNumber + ArrayStartModifier]].Text         := IntToStr(TextGolden_ActualValue[PlayerNumber]);
    Text[TextGoldenNotesScore[PlayerNumber + ArrayStartModifier]].Alpha        := (BarGolden_EaseOut_Step / 100);

    Static[StaticBoxLightest[PlayerNumber + ArrayStartModifier]].Texture.Alpha := (BarGolden_EaseOut_Step / 100);
    Text[TextGoldenNotes[PlayerNumber + ArrayStartModifier]].Alpha             := (BarGolden_EaseOut_Step / 100);

 // line bonus
    Text[TextLineBonusScore[PlayerNumber + ArrayStartModifier]].Text           := IntToStr(TextPhrase_ActualValue[PlayerNumber]);
    Text[TextLineBonusScore[PlayerNumber + ArrayStartModifier]].Alpha          := (BarPhrase_EaseOut_Step / 100);

    Static[StaticBoxLight[PlayerNumber + ArrayStartModifier]].Texture.Alpha    := (BarPhrase_EaseOut_Step / 100);
    Text[TextLineBonus[PlayerNumber + ArrayStartModifier]].Alpha               := (BarPhrase_EaseOut_Step / 100);

// plain score
    Text[TextNotesScore[PlayerNumber + ArrayStartModifier]].Text               := IntToStr(TextScore_ActualValue[PlayerNumber]);
    Text[TextNotes[PlayerNumber + ArrayStartModifier]].Alpha                   := (BarScore_EaseOut_Step / 100);

    Static[StaticBoxDark[PlayerNumber + ArrayStartModifier]].Texture.Alpha     := (BarScore_EaseOut_Step / 100);
    Text[TextNotesScore[PlayerNumber + ArrayStartModifier]].Alpha              := (BarScore_EaseOut_Step / 100);

// total score
    Text[TextTotalScore[PlayerNumber + ArrayStartModifier]].Text               := IntToStr(TextScore_ActualValue[PlayerNumber] + TextPhrase_ActualValue[PlayerNumber] + TextGolden_ActualValue[PlayerNumber]);
    Text[TextTotalScore[PlayerNumber + ArrayStartModifier]].Alpha              := (BarScore_EaseOut_Step / 100);

    Text[TextTotal[PlayerNumber + ArrayStartModifier]].Alpha                   := (BarScore_EaseOut_Step / 100);

    Text[TextTotal[PlayerNumber + ArrayStartModifier]].Alpha                   := (BarScore_EaseOut_Step / 100);

  if(BarGolden_EaseOut_Step > 100) then
    begin
      ShowRating(PlayerNumber);
    end;
 end;


procedure TScreenScore.ShowRating(PlayerNumber: integer);
var
  ArrayStartModifier : integer;
  Rating : integer;
  fu : integer;

  Posx : real;
  Posy : real;
  width : array[1..3] of real;
begin
  case PlayersPlay of
    1:  begin
          ArrayStartModifier := 0;
        end;
    2, 4:  begin
          ArrayStartModifier := 1;
        end;
    3, 6:  begin
          ArrayStartModifier := 3;
        end;
  end;

    fu := PlayerNumber + ArrayStartModifier;

//todo: this could break if the width is not given, for instance when there's a skin with no picture for ratings
    if ( aPlayerScoreScreenRatings[PlayerNumber].RatePic_Width > 0 ) AND   // JB :)
       ( aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue > 0 ) then
    begin
      Text[TextScore[PlayerNumber + ArrayStartModifier]].Alpha := aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue / aPlayerScoreScreenRatings[PlayerNumber].RatePic_Width;
    end;
    
// end todo
       {{$IFDEF TRANSLATE}
       case (Player[PlayerNumber-1].ScoreTotalI) of
         0..2000:
           begin
             Text[TextScore[fu]].Text := Language.Translate('SING_SCORE_TONE_DEAF');
             Rating := 0;
           end;
         2010..4000:
           begin
             Text[TextScore[fu]].Text := Language.Translate('SING_SCORE_AMATEUR');
             Rating := 1;
           end;
         4010..6000:
           begin
             Text[TextScore[fu]].Text := Language.Translate('SING_SCORE_RISING_STAR');
             Rating := 2;
           end;
         6010..8000:
           begin
             Text[TextScore[fu]].Text := Language.Translate('SING_SCORE_LEAD_SINGER');
             Rating := 3;
           end;
         8010..9000:
           begin
             Text[TextScore[fu]].Text := Language.Translate('SING_SCORE_HIT_ARTIST');
             Rating := 4;
           end;
         9010..9800:
           begin
             Text[TextScore[fu]].Text := Language.Translate('SING_SCORE_SUPERSTAR');
             Rating := 5;
           end;
         9810..10000:
           begin
             Text[TextScore[fu]].Text := Language.Translate('SING_SCORE_ULTRASTAR');
             Rating := 6;
           end;
       end;
       {{$ELSE}{
       case (Player[P].ScoreTotalI-1) of
         0..2000:        Text[TextScore[fu]].Text := 'Tone Deaf';
         2010..4000:     Text[TextScore[fu]].Text := 'Amateur';
         4010..6000:     Text[TextScore[fu]].Text := 'Rising Star';
         6010..8000:     Text[TextScore[fu]].Text := 'Lead Singer';
         8010..9000:     Text[TextScore[fu]].Text := 'Hit Artist';
         9010..9800:     Text[TextScore[fu]].Text := 'Superstar';
         9810..10000:    Text[TextScore[fu]].Text := 'Ultrastar';
       end;
       {$ENDIF}

// Bounce the rating picture in
    PosX := aPlayerScoreScreenRatings[PlayerNumber].RatePic_X + (aPlayerScoreScreenRatings[PlayerNumber].RatePic_Width  / 2);
    PosY := aPlayerScoreScreenRatings[PlayerNumber].RatePic_Y + (aPlayerScoreScreenRatings[PlayerNumber].RatePic_Height / 2); ;

    elastique(PlayerNumber);

    width[PlayerNumber] := aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue/2;

    glBindTexture(GL_TEXTURE_2D, Tex_Score_Ratings[Rating].TexNum);

    glEnable(GL_TEXTURE_2D);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);

    glBegin(GL_QUADS);
      glTexCoord2f(0, 0);                                                           glVertex2f(PosX - width[PlayerNumber],  PosY - width[PlayerNumber]);
      glTexCoord2f(Tex_Score_Ratings[Rating].TexW, 0);                              glVertex2f(PosX + width[PlayerNumber],  PosY - width[PlayerNumber]);
      glTexCoord2f(Tex_Score_Ratings[Rating].TexW, Tex_Score_Ratings[Rating].TexH); glVertex2f(PosX + width[PlayerNumber],  PosY + width[PlayerNumber]);
      glTexCoord2f(0, Tex_Score_Ratings[Rating].TexH);                              glVertex2f(PosX - width[PlayerNumber],  PosY + width[PlayerNumber]);
    glEnd;

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2d);
end;


function TscreenScore.elastique(PlayerNumber : Integer): real;
var
  ReturnValue : real;
  p, s        : real;

  RaiseStep, Actual_Value, MaxVal  : real;
  EaseOut_Step : integer;
begin

  EaseOut_Step  := aPlayerScoreScreenRatings[PlayerNumber].RateEaseStep;
  Actual_Value  := aPlayerScoreScreenRatings[PlayerNumber].RateEaseValue;
  MaxVal        := aPlayerScoreScreenRatings[PlayerNumber].RatePic_Width;

  RaiseStep     := EaseOut_Step;

	if (RaiseStep = 0)
   then ReturnValue := MaxVal;

  if ( MaxVal    > 0 ) AND  // JB :)
     ( RaiseStep > 0 ) then
  begin
    RaiseStep := RaiseStep / MaxVal;
  end;

  if (RaiseStep = 1)
    then
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


function TscreenScore.EaseBarIn(PlayerNumber : Integer; BarType: String) : real;
const
  RaiseSmoothness : integer = 100;
var
  MaxHeight       : real;
  NewHeight       : real;
  Width           : real;

  Height2Reach    : real;

  RaiseStep       : real;

  BarStartPosX    : Single;
  BarStartPosY    : Single;

  lTmp            : real;
  Score           : integer;

  //textures
  TextureBar   : integer;
  TextureRound : integer;
begin

  MaxHeight    := aPlayerScoreScreenDatas[PlayerNumber].Bar_Height;
  Width        := aPlayerScoreScreenDatas[PlayerNumber].Bar_Width;

  BarStartPosX := aPlayerScoreScreenDatas[PlayerNumber].Bar_X;

   // The texture starts in the upper left corner, so let's subtract the height - so we can arise it

  // let's get the points according to the bar we draw
  // score array starts at 0, which means the score for player 1 is in score[0]
  // EaseOut_Step is the actual step in the raising process, like the 20iest step of EaseOut_MaxSteps
  if (BarType = 'Note')
    then
      begin
        Score        := Player[PlayerNumber - 1].ScoreI;
        RaiseStep    := BarScore_EaseOut_Step;
        BarStartPosY := aPlayerScoreScreenDatas[PlayerNumber].Bar_Y + MaxHeight;
      end;
  if (BarType = 'Line')
    then
      begin
        Score        := Player[PlayerNumber - 1].ScoreLineI;
        RaiseStep    := BarPhrase_EaseOut_Step;
        BarStartPosY := aPlayerScoreScreenDatas[PlayerNumber].Bar_Y - aPlayerScoreScreenDatas[PlayerNumber].BarScore_ActualHeight + MaxHeight;
      end;
  if (BarType = 'Golden')
    then
      begin
        Score        := Player[PlayerNumber - 1].ScoreGoldenI;
        RaiseStep    := BarGolden_EaseOut_Step;
        BarStartPosY := aPlayerScoreScreenDatas[PlayerNumber].Bar_Y - aPlayerScoreScreenDatas[PlayerNumber].BarScore_ActualHeight - aPlayerScoreScreenDatas[PlayerNumber].BarLine_ActualHeight + MaxHeight;
      end;

  // the height dependend of the score
  Height2Reach := (Score / 10000) * MaxHeight;

  if (aPlayerScoreScreenDatas[PlayerNumber].Bar_Actual_Height < Height2Reach) then
    begin
      // Check http://proto.layer51.com/d.aspx?f=400 for more info on easing functions
      // Calculate the actual step according to the maxsteps
      RaiseStep := RaiseStep / EaseOut_MaxSteps;

      // quadratic easing out - decelerating to zero velocity
      // -end_position * current_time * ( current_time - 2 ) + start_postion
      lTmp := (-Height2Reach * RaiseStep * (RaiseStep - 20) + BarStartPosY);

      if ( RaiseSmoothness > 0 ) AND ( lTmp > 0 )
        then
          begin
            NewHeight := lTmp / RaiseSmoothness;
          end;
    end
  else
    begin
      NewHeight := Height2Reach;
    end;

    glColor4f(1, 1, 1, 1);

// set the texture for the bar
  if (BarType = 'Note')
    then
      begin
        glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarLevel_Dark.TexNum);
      end;
  if (BarType = 'Line')
    then
      begin
        glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarLevel_Light.TexNum);
      end;
  if (BarType = 'Golden')
    then
      begin
        glBindTexture(GL_TEXTURE_2D, aPlayerScoreScreenTextures[PlayerNumber].Score_NoteBarLevel_Lightest.TexNum);
      end;

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
      glTexCoord2f(0, 0); glVertex3f(BarStartPosX,         (BarStartPosY - Static[StaticLevelRound[PlayerNumber]].Texture.h) - NewHeight, ZBars);
      glTexCoord2f(1, 0); glVertex3f(BarStartPosX + Width, (BarStartPosY - Static[StaticLevelRound[PlayerNumber]].Texture.h) - NewHeight, ZBars);
      glTexCoord2f(1, 1); glVertex3f(BarStartPosX + Width,  BarStartPosY - NewHeight,                                                     ZBars);
      glTexCoord2f(0, 1); glVertex3f(BarStartPosX,          BarStartPosY - NewHeight,                                                     ZBars);
    glEnd;

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2d);

  if (BarType = 'Note')
    then
      aPlayerScoreScreenDatas[PlayerNumber].BarScore_ActualHeight  := NewHeight;
  if (BarType = 'Line')
    then
      aPlayerScoreScreenDatas[PlayerNumber].BarLine_ActualHeight   := NewHeight;
  if (BarType = 'Golden')
    then
      aPlayerScoreScreenDatas[PlayerNumber].BarGolden_ActualHeight := NewHeight;
end;


function TScreenScore.EaseScoreIn(PlayerNumber: integer; ScoreType : String) : real;
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
      ScoreReached     := Player[PlayerNumber-1].ScoreI;
    end;
  if (ScoreType = 'Line') then
    begin
      EaseOut_Step     := BarPhrase_EaseOut_Step;
      ActualScoreValue := TextPhrase_ActualValue[PlayerNumber];
      ScoreReached     := Player[PlayerNumber-1].ScoreLineI;
    end;
  if (ScoreType = 'Golden') then
    begin
      EaseOut_Step     := BarGolden_EaseOut_Step;
      ActualScoreValue := TextGolden_ActualValue[PlayerNumber];
      ScoreReached     := Player[PlayerNumber-1].ScoreGoldenI;
    end;

  // EaseOut_Step is the actual step in the raising process, like the 20iest step of EaseOut_MaxSteps
  RaiseStep     := EaseOut_Step;

  if (ActualScoreValue < ScoreReached) then
    begin
      // Calculate the actual step according to the maxsteps
      RaiseStep := RaiseStep / EaseOut_MaxSteps;

      // quadratic easing out - decelerating to zero velocity
      // -end_position * current_time * ( current_time - 2 ) + start_postion
      lTmpA := (-ScoreReached * RaiseStep * (RaiseStep - 20));
      if ( lTmpA           > 0 ) AND
         ( RaiseSmoothness > 0 ) THEN
      begin

        if (ScoreType = 'Note') then
          begin
            TextScore_ActualValue[PlayerNumber]  := floor( lTmpA / RaiseSmoothness);
          end;
        if (ScoreType = 'Line') then
          begin
            TextPhrase_ActualValue[PlayerNumber] := floor( lTmpA / RaiseSmoothness);
          end;
        if (ScoreType = 'Golden') then
          begin
            TextGolden_ActualValue[PlayerNumber] := floor( lTmpA / RaiseSmoothness);
          end;

      end;
    end
  else
    begin

      if (ScoreType = 'Note') then
        begin
          TextScore_ActualValue[PlayerNumber]    := ScoreReached;
        end;
      if (ScoreType = 'Line') then
        begin
          TextPhrase_ActualValue[PlayerNumber]   := ScoreReached;
        end;
      if (ScoreType = 'Golden') then
        begin
          TextGolden_ActualValue[PlayerNumber]   := ScoreReached;
        end;

    end;

end;

procedure TScreenScore.FillPlayer(Item, P: integer);
var
  S:    string;
begin
  Text[TextName[Item]].Text := Ini.Name[P];

  S := IntToStr((Round(Player[P].Score) div 10) * 10);
  while (Length(S)<4) do S := '0' + S;
  Text[TextNotesScore[Item]].Text := S;

//  while (Length(S)<5) do S := '0' + S;
//  Text[TextTotalScore[Item]].Text := S;

//fixed: line bonus and golden notes don't show up,
//       another bug: total score was shown without added golden-, linebonus
    S := IntToStr(Player[P].ScoreTotalI);
    while (Length(S)<5) do S := '0' + S;
    Text[TextTotalScore[Item]].Text := S;

    S := IntToStr(Player[P].ScoreLineI);
    while (Length(S)<4) do S := '0' + S;
    Text[TextLineBonusScore[Item]].Text := S;

    S := IntToStr(Player[P].ScoreGoldenI);
    while (Length(S)<4) do S := '0' + S;
    Text[TextGoldenNotesScore[Item]].Text := S;
//end of fix

  LoadColor(
    Text[TextName[Item]].ColR,
    Text[TextName[Item]].ColG,
    Text[TextName[Item]].ColB,
    'P' + IntToStr(P+1) + 'Dark');
 {
  LoadColor(
    Static[StaticBoxLightest[Item]].Texture.ColR,
    Static[StaticBoxLightest[Item]].Texture.ColG,
    Static[StaticBoxLightest[Item]].Texture.ColB,
    'P' + IntToStr(P+1) + 'Lightest');

  LoadColor(
    Static[StaticBoxLight[Item]].Texture.ColR,
    Static[StaticBoxLight[Item]].Texture.ColG,
    Static[StaticBoxLight[Item]].Texture.ColB,
    'P' + IntToStr(P+1) + 'Light');

  LoadColor(
    Static[StaticBoxDark[Item]].Texture.ColR,
    Static[StaticBoxDark[Item]].Texture.ColG,
    Static[StaticBoxDark[Item]].Texture.ColB,
    'P' + IntToStr(P+1) + 'Dark');
  }
end;

end.
