unit UScreenScore;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

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
  {$endif}
  math,
  ULCD;

//  OpenGL;

type
  TScreenScore = class(TMenu)
    public
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

      BarScore_ActualHeight  : array[1..6] of real;
      BarPhrase_ActualHeight : array[1..6] of real;
      BarGolden_ActualHeight : array[1..6] of real;

      BarScore_EaseOut_Step  : real;
      BarPhrase_EaseOut_Step : real;
      BarGolden_EaseOut_Step : real;

      TextScore_ActualValue  : array[1..6] of integer;
      TextPhrase_ActualValue : array[1..6] of integer;
      TextGolden_ActualValue : array[1..6] of integer;

      EaseOut_MaxSteps       : real;

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure onShowFinish; override;
      function Draw: boolean; override;
      procedure FillPlayer(Item, P: integer);

      function RaiseBar(PlayerNumber: integer; BarStartPosY: Single; ActualHeight: real; Score: integer; ColorBrightness : String; EaseOut_Step: Real) : real;
      function IncrementScore(PlayerNumber: integer; ActualScoreValue: Integer; ScoreReached: integer; EaseOut_Step: Real) : integer;
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
  P:    integer;
  I, C:    integer;
begin
  inherited Create;

  LoadFromTheme(Theme.Score);

  TextArtist := AddText(Theme.Score.TextArtist);
  TextTitle := AddText(Theme.Score.TextTitle);

  TextArtistTitle := AddText(Theme.Score.TextArtistTitle);

  for P := 1 to 6 do begin
    TextName[P] := AddText(Theme.Score.TextName[P]);
    TextScore[P] := AddText(Theme.Score.TextScore[P]);

    TextNotes[P] := AddText(Theme.Score.TextNotes[P]);
    TextNotesScore[P] := AddText(Theme.Score.TextNotesScore[P]);
    TextLineBonus[P] := AddText(Theme.Score.TextLineBonus[P]);
    TextLineBonusScore[P] := AddText(Theme.Score.TextLineBonusScore[P]);
    TextGoldenNotes[P] := AddText(Theme.Score.TextGoldenNotes[P]);
    TextGoldenNotesScore[P] := AddText(Theme.Score.TextGoldenNotesScore[P]);
    TextTotal[P] := AddText(Theme.Score.TextTotal[P]);
    TextTotalScore[P] := AddText(Theme.Score.TextTotalScore[P]);

    SetLength(PlayerStatic[P], Length(Theme.Score.PlayerStatic[P]));

    SetLength(PlayerTexts[P], Length(Theme.Score.PlayerTexts[P]));

    for I := 0 to High(Theme.Score.PlayerStatic[P]) do
      PlayerStatic[P, I] := AddStatic(Theme.Score.PlayerStatic[P, I]);


    //added by mog
    for C := 0 to High(Theme.Score.PlayerTexts[P]) do
      PlayerTexts[P, C] := AddText(Theme.Score.PlayerTexts[P, C]);
    // more skinable now

    StaticBoxLightest[P] := AddStatic(Theme.Score.StaticBoxLightest[P]);
    StaticBoxLight[P] := AddStatic(Theme.Score.StaticBoxLight[P]);
    StaticBoxDark[P] := AddStatic(Theme.Score.StaticBoxDark[P]);

    StaticBackLevel[P] := AddStatic(Theme.Score.StaticBackLevel[P]);
    StaticBackLevelRound[P] := AddStatic(Theme.Score.StaticBackLevelRound[P]);
    StaticLevel[P] := AddStatic(Theme.Score.StaticLevel[P]);
    StaticLevelRound[P] := AddStatic(Theme.Score.StaticLevelRound[P]);
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
begin
{*
  CountSkipTimeSet;

  Animation := 0;
  Fadeout := false;

  Text[1].Text := AktSong.Artist + ' - ' + AktSong.Title;
  Text[2].Text := '  ' + IntToStr((Round(Gracz[0].Punkty) div 10) * 10) + ' points';

  Static[0].Texture.X := -2000;
  Static[1].Texture.X := -2000;
  Static[2].Texture.X := -2000;
  Static[3].Texture.X := -2000;
  Static[4].Texture.X := -2000;
  Static[5].Texture.X := -2000;
  Static[6].Texture.X := -2000;
  Static[7].Texture.X := -2000;

  Text[0].X := -2000;
  Text[1].X := -2000;
  Text[2].X := -2000;
  Text[3].X := -2000;


  case (Round(Gracz[0].Punkty) div 10) * 10 of
    0..1000:        Text[3].Text := '  Tone Deaf';
    2010..4000:     Text[3].Text := '  Amateur';
    4010..6000:     Text[3].Text := '  Rising Star';
    6010..8000:     Text[3].Text := '  Lead Singer';
    8010..9000:     Text[3].Text := '  Hit Artist';
    9010..10000:    Text[3].Text := '  Superstar';
  end;

  Music.PlayShuffle;
*}

  // Singstar
  Fadeout := false;

  Text[TextArtist].Text := AktSong.Artist;
  Text[TextTitle].Text := AktSong.Title;
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

  for P := 1 to 6 do begin
    Text[TextName[P]].Visible               := V[P];
    Text[TextScore[P]].Visible              := V[P];

    // We set alpha to 0 , so we can nicely blend them in when we need them
    Text[TextScore[P]].Alpha              := 0;
    Text[TextNotesScore[P]].Alpha         := 0;
    Text[TextNotes[P]].Alpha              := 0;
    Text[TextLineBonus[P]].Alpha          := 0;
    Text[TextLineBonusScore[P]].Alpha     := 0;
    Text[TextGoldenNotes[P]].Alpha        := 0;
    Text[TextGoldenNotesScore[P]].Alpha   := 0;
    Text[TextTotal[P]].Alpha              := 0;
    Text[TextTotalScore[P]].Alpha         := 0;

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

    Static[StaticBackLevel[P]].Visible      := false; //V[P];
    Static[StaticBackLevelRound[P]].Visible := false; //V[P];
    Static[StaticLevel[P]].Visible          := false; //V[P];
    Static[StaticLevelRound[P]].Visible     := false; //V[P];
  end;

end;

procedure TScreenScore.onShowFinish;
var
  index : integer;
begin
for index := 1 to 2 do
  begin
    BarScore_ActualHeight[index]  := 0;
    BarPhrase_ActualHeight[index] := 0;
    BarGolden_ActualHeight[index] := 0;

    TextScore_ActualValue[index]  := 0;
    TextPhrase_ActualValue[index] := 0;
    TextGolden_ActualValue[index] := 0;
  end;


  BarScore_EaseOut_Step  := 1;
  BarPhrase_EaseOut_Step := 1;
  BarGolden_EaseOut_Step := 1;

  EaseOut_MaxSteps := 100;
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
  C:      integer;

  katze : integer;
begin

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

{      if ScreenAct = 1 then begin
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
      end;}

    end;
  end;

  inherited Draw;
{
  player[1].ScoreI       := 7000;
  player[1].ScoreLineI   := 2000;
  player[1].ScoreGoldenI := 1000;

  player[2].ScoreI       := 2500;
  player[2].ScoreLineI   := 1100;
  player[2].ScoreGoldenI :=  900;
 }
// Let's arise the bars
  ActualTime := GetTickCount div 33;
    if ((ActualTime <> OldTime) and ShowFinish )then
      begin
        OldTime               := ActualTime;

      For katze:= 0 to 5 do
        begin

          // We actually araise them in the right order, but we have to draw them in reverse order (golden -> phrase -> mainscore)
          Case BarScore_EaseOut_Step < EaseOut_MaxSteps * 10 of
            true : BarScore_EaseOut_Step := BarScore_EaseOut_Step + 1;
          end;

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

                  BarGolden_ActualHeight[katze] := RaiseBar(katze,
                                                            Static[StaticBackLevel[katze+1]].Texture.y - BarScore_ActualHeight[katze] - BarPhrase_ActualHeight[katze],
                                                            BarGolden_ActualHeight[katze],
                                                            player[katze+1].ScoreGoldenI,
                                                            'Lightest',
                                                            BarGolden_EaseOut_Step);

                  // Increment and show total score and plain score
                  TextGolden_ActualValue[katze] := IncrementScore(katze,
                                                            TextGolden_ActualValue[katze],
                                                            Player[katze+1].ScoreGoldenI,
                                                            BarGolden_EaseOut_Step);
                  Text[TextGoldenNotesScore[katze+1]].Text  := IntToStr(TextGolden_ActualValue[katze]);

                  // Blend in
                  Text[TextGoldenNotesScore[katze+1]].Alpha := (BarGolden_EaseOut_Step / 100);
                  Text[TextGoldenNotes[katze+1]].Alpha      := (BarGolden_EaseOut_Step / 100);

                end;
        //########################
        // Draw phrase score bar #
        //########################
              BarPhrase_ActualHeight[katze]     := RaiseBar(katze,
                                                            Static[StaticBackLevel[katze+1]].Texture.y - BarScore_ActualHeight[katze],
                                                            BarPhrase_ActualHeight[katze],
                                                            Player[katze+1].ScoreLineI,
                                                            'Light',
                                                            BarPhrase_EaseOut_Step);

              // Increment and show total score and plain score
              TextPhrase_ActualValue[katze]     := IncrementScore(katze,
                                                            TextPhrase_ActualValue[katze],
                                                            Player[katze+1].ScoreLineI,
                                                            BarPhrase_EaseOut_Step);
              Text[TextLineBonusScore[katze+1]].Text   := IntToStr(TextPhrase_ActualValue[katze]);

              //Blend in
              Text[TextLineBonusScore[katze+1]].Alpha  := (BarPhrase_EaseOut_Step / 100);
              Text[TextLineBonus[katze+1]].Alpha       := (BarPhrase_EaseOut_Step / 100);


            end;
        //#######################
        // Draw plain score bar #
        //#######################
        BarScore_ActualHeight[katze]            := RaiseBar(katze,
                                                            Static[StaticBackLevel[katze+1]].Texture.y,
                                                            BarScore_ActualHeight[katze],
                                                            Player[katze+1].ScoreI,
                                                            'Dark',
                                                            BarScore_EaseOut_Step);
        // Increment and show total score and plain score
        TextScore_ActualValue[katze]            := IncrementScore(katze,
                                                            TextScore_ActualValue[katze],
                                                            Player[katze+1].ScoreI,
                                                            BarScore_EaseOut_Step);
        Text[TextNotesScore[katze+1]].Text      := IntToStr(TextScore_ActualValue[katze]);

        Text[TextTotalScore[katze+1]].Text      := IntToStr(TextScore_ActualValue[katze] + TextPhrase_ActualValue[katze] + TextGolden_ActualValue[katze]);

      //Blend em in
        Text[TextTotalScore[katze+1]].Alpha     := (BarScore_EaseOut_Step / 100);
        Text[TextTotal[katze+1]].Alpha          := (BarScore_EaseOut_Step / 100);
        Text[TextNotesScore[katze+1]].Alpha     := (BarScore_EaseOut_Step / 100);
        Text[TextNotes[katze+1]].Alpha          := (BarScore_EaseOut_Step / 100);
        Text[TextScore[katze+1]].Alpha          := (BarScore_EaseOut_Step / 100);

      end; // me loop
    end;
end;


function TscreenScore.RaiseBar(PlayerNumber: integer; BarStartPosY: Single; ActualHeight: real; Score: integer; ColorBrightness : String; EaseOut_Step: Real) : real;
const
  RaiseSmoothness : integer = 100;
var
  MaxHeight       : real;
  NewHeight       : real;
  Width           : real;

  Height2Reach    : real;

  RaiseStep       : real;

  BarStartPosX    : Single;

  R,G,B           : real;
begin

  MaxHeight    := Static[StaticBackLevel[PlayerNumber + 1]].Texture.H;
  Width        := Static[StaticBackLevel[PlayerNumber + 1]].Texture.W;

  BarStartPosX := Static[StaticBackLevel[PlayerNumber + 1]].Texture.X;

  BarStartPosY := BarStartPosY + MaxHeight; // The texture starts in the upper left corner, so let's subtract the height - so we can arise it

  // the height dependend of the score
  Height2Reach := (Score / 10000) * MaxHeight;

  // EaseOut_Step is the actual step in the raising process, like the 20iest step of EaseOut_MaxSteps
  RaiseStep := EaseOut_Step;

  if (ActualHeight < Height2Reach) then
    begin
      // Check http://proto.layer51.com/d.aspx?f=400 for more info on easing functions
      // Calculate the actual step according to the maxsteps
      RaiseStep := RaiseStep / EaseOut_MaxSteps;

      // quadratic easing out - decelerating to zero velocity
      // -end_position * current_time * ( current_time - 2 ) + start_postion
      NewHeight := (-Height2Reach * RaiseStep * (RaiseStep - 20) + BarStartPosY) / RaiseSmoothness;
    end
  else
    begin
      NewHeight := Height2Reach;
    end;
                                                //+1
    LoadColor(R, G, B, 'P' + inttostr(PlayerNumber+1) + ColorBrightness); //dark, light, lightest
    glColor4f(R, G, B, 1);

    //the actual bar
    glBindTexture(GL_TEXTURE_2D, Static[StaticLevel[PlayerNumber + 1]].Texture.TexNum);

    glEnable(GL_TEXTURE_2D);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);

    glBegin(GL_QUADS);
      glTexCoord2f(0, 0); glVertex2f(BarStartPosX,         BarStartPosY - NewHeight);
      glTexCoord2f(1, 0); glVertex2f(BarStartPosX + Width, BarStartPosY - NewHeight);
      glTexCoord2f(1, 1); glVertex2f(BarStartPosX + Width, BarStartPosY);
      glTexCoord2f(0, 1); glVertex2f(BarStartPosX,         BarStartPosY);
    glEnd;

    //the round thing on top
    glBindTexture(GL_TEXTURE_2D, Static[StaticLevelRound[PlayerNumber + 1]].Texture.TexNum);

    glEnable(GL_TEXTURE_2D);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);

    glBegin(GL_QUADS);
      glTexCoord2f(0, 0); glVertex2f(BarStartPosX,         (BarStartPosY - Static[StaticLevelRound[PlayerNumber + 1]].Texture.h) - NewHeight);
      glTexCoord2f(1, 0); glVertex2f(BarStartPosX + Width, (BarStartPosY - Static[StaticLevelRound[PlayerNumber + 1]].Texture.h) - NewHeight);
      glTexCoord2f(1, 1); glVertex2f(BarStartPosX + Width,  BarStartPosY - NewHeight);
      glTexCoord2f(0, 1); glVertex2f(BarStartPosX,          BarStartPosY - NewHeight);
    glEnd;

  Result := NewHeight;
end;

function TScreenScore.IncrementScore(PlayerNumber: integer; ActualScoreValue: Integer; ScoreReached: integer; EaseOut_Step: Real) : integer;
const
  RaiseSmoothness : integer = 100;
var
  RaiseStep    : Real;
  lTmpA        : Real;
begin
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
        Result    := floor( lTmpA / RaiseSmoothness);
      end;
    end
  else
    begin
      Result    := ScoreReached;
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
end;

end.
