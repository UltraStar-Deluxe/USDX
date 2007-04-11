unit UScreenScore;

interface

uses
  UMenu, SDL, SysUtils, UDisplay, UMusic, USongs, UThemes, ULCD, OpenGL;

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
      constructor Create(Back: String); override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      function Draw: boolean; override;
      procedure FillPlayer(Item, P: integer);
  end;

implementation

{{$IFDEF TRANSLATE}
uses UGraphic, UScreenSong, UMenuStatic, UTime, UMain, UIni, ULanguage;
{{$ELSE}{
uses UGraphic, UScreenSong, UMenuStatic, UTime, UMain, UIni;
{{$ENDIF}
function TScreenScore.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then begin
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE :
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

constructor TScreenScore.Create(Back: String);
var
  P:    integer;
  I, C:    integer;
begin
  inherited Create(Back);

  // background arrows sorted from farthest to nearest 
{  AddStatic(-2000 + 400,  100, 360,  60, 1, 1, 1, Skin.Arrow2, 'JPG', 'Arrow');
  AddStatic(-2000 + -50,  200, 420,  70, 1, 1, 1, Skin.Arrow, 'JPG', 'Arrow');
  AddStatic(-2000 + 90,    30, 500,  90, 1, 1, 1, Skin.Arrow, 'JPG', 'Arrow');
  AddStatic(-2000 + -250, 100, 800, 150, 1, 1, 1, Skin.Arrow, 'JPG', 'Arrow');

  Static[0].Texture.Rot := 100 * pi/180;
  Static[1].Texture.Rot := 7   * pi/180;
  Static[2].Texture.Rot := 35  * pi/180;


  // main arrow with text
  AddStatic(0, 340, 1000, 180, 1, 1, 1, Skin.Arrow2, 'JPG', 'Arrow');
//  AddText(450, 409, 4, 15, 1, 1, 1, 'Smile');
  AddText(450, 409, 4, 15, 1, 1, 1, 'Let''s see the results');
  Text[0].Y := 401;

  Static[4].Texture.Rot := -3  * pi/180;


  // two mid arrows
  AddStatic(-2000 + -250, 100, 800, 150, 1, 1, 1, Skin.Arrow, 'JPG', 'Arrow');
  AddStatic(-2000 + -250, 100, 800, 150, 1, 1, 1, Skin.Arrow, 'JPG', 'Arrow');


  // last arrow
  AddStatic(-2000, 340, 1100, 180, 1, 1, 1, Skin.Arrow2, 'JPG', 'Arrow');
//  AddText(-2000, 407, 4, 17, 1, 1, 1, 'SHUFFLE !');
  AddText(-2000, 407, 4, 15, 1, 1, 1, 'SHUFFLE !');

  Static[7].Texture.Rot := 184  * pi/180;

  // score text
  AddText(-2000, 407, 4, 17, 1, 1, 1, '10010 points');
  AddText(-2000, 407, 4, 17, 1, 1, 1, 'Cheater');

  Fadeout := false;}


  // Singstar Theme
  AddBackground(Theme.Score.Background.Tex);

  for I := 0 to High(Theme.Score.Static) do
    AddStatic(Theme.Score.Static[I]);

  for I := 0 to High(Theme.Score.Text) do
    AddText(Theme.Score.Text[I]);

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
{  CountSkipTimeSet;

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

  Music.PlayShuffle;}

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
    Text[TextName[P]].Visible := V[P];
    Text[TextScore[P]].Visible := V[P];

    Text[TextNotes[P]].Visible := V[P];
    Text[TextNotesScore[P]].Visible := V[P];
    Text[TextLineBonus[P]].Visible := V[P];
    Text[TextLineBonusScore[P]].Visible := V[P];
    Text[TextGoldenNotes[P]].Visible := V[P];
    Text[TextGoldenNotesScore[P]].Visible := V[P];
    Text[TextTotal[P]].Visible := V[P];
    Text[TextTotalScore[P]].Visible := V[P];

    for I := 0 to high(PlayerStatic[P]) do
      Static[PlayerStatic[P, I]].Visible := V[P];

    for I := 0 to high(PlayerTexts[P]) do
      Text[PlayerTexts[P, I]].Visible := V[P];

    Static[StaticBoxLightest[P]].Visible := V[P];
    Static[StaticBoxLight[P]].Visible := V[P];
    Static[StaticBoxDark[P]].Visible := V[P];

    Static[StaticBackLevel[P]].Visible := V[P];
    Static[StaticBackLevelRound[P]].Visible := V[P];
    Static[StaticLevel[P]].Visible := V[P];
    Static[StaticLevelRound[P]].Visible := V[P];
  end;

  if PlayersPlay <= 3 then begin // only for 1 screen mode
  for P := 0 to PlayersPlay-1 do begin
    case PlayersPlay of
      1:  PP := 1;
      2:  PP := P + 2;
      3:  PP := P + 4;
    end;
    //PP := 1;

    Text[TextName[PP]].Text := Ini.Name[P];

    {{$IFDEF TRANSLATE}
    case (Player[P].ScoreTotalI) of
      0..2000:        Text[TextScore[PP]].Text := Language.Translate('SING_SCORE_TONE_DEAF');
      2010..4000:     Text[TextScore[PP]].Text := Language.Translate('SING_SCORE_AMATEUR');
      4010..6000:     Text[TextScore[PP]].Text := Language.Translate('SING_SCORE_RISING_STAR');
      6010..8000:     Text[TextScore[PP]].Text := Language.Translate('SING_SCORE_LEAD_SINGER');
      8010..9000:     Text[TextScore[PP]].Text := Language.Translate('SING_SCORE_HIT_ARTIST');
      9010..9800:     Text[TextScore[PP]].Text := Language.Translate('SING_SCORE_SUPERSTAR');
      9810..10000:    Text[TextScore[PP]].Text := Language.Translate('SING_SCORE_ULTRASTAR');
    end;
    {{$ELSE}{
    case (Player[P].ScoreTotalI) of
      0..2000:        Text[TextScore[PP]].Text := 'Tone Deaf';
      2010..4000:     Text[TextScore[PP]].Text := 'Amateur';
      4010..6000:     Text[TextScore[PP]].Text := 'Rising Star';
      6010..8000:     Text[TextScore[PP]].Text := 'Lead Singer';
      8010..9000:     Text[TextScore[PP]].Text := 'Hit Artist';
      9010..9800:     Text[TextScore[PP]].Text := 'Superstar';
      9810..10000:    Text[TextScore[PP]].Text := 'Ultrastar';
    end;
    {$ENDIF}

    S := IntToStr(Player[P].ScoreI);
    while (Length(S)<4) do S := '0' + S;
    Text[TextNotesScore[PP]].Text := S;

    S := IntToStr(Player[P].ScoreLineI);
    while (Length(S)<4) do S := '0' + S;
    Text[TextLineBonusScore[PP]].Text := S;

    S := IntToStr(Player[P].ScoreGoldenI);
    while (Length(S)<4) do S := '0' + S;
    Text[TextGoldenNotesScore[PP]].Text := S;

    S := IntToStr(Player[P].ScoreTotalI);
    while (Length(S)<5) do S := '0' + S;
    Text[TextTotalScore[PP]].Text := S;

    // Level bar length
{    Lev := ((Round(Player[P].Punkty) div 10) * 10) / 10000;
    Static[StaticLevel[PP]].Texture.H := Round(Static[StaticBackLevel[PP]].Texture.H * Lev);
    Static[StaticLevel[PP]].Texture.Y := Static[StaticBackLevel[PP]].Texture.Y + Static[StaticBackLevel[PP]].Texture.H  - Static[StaticLevel[PP]].Texture.H;
    Static[StaticLevelRound[PP]].Texture.Y := Static[StaticLevel[PP]].Texture.Y - Static[StaticLevelRound[PP]].Texture.H;}
    // doesn't align too much... (to fix)
    // hint: play with wrapping textures
    // resolution: setting TexY1 and TexY2 to 0.1 and 0.9

    Lev := Player[P].ScoreTotalI / 10000;
    MaxH := Static[StaticBackLevel[PP]].Texture.H + Static[StaticBackLevelRound[PP]].Texture.H / 2;

    // developer note (Polish):
    // w sumie np. 120 pix
    // ten static moze miec 100 pix
    // wlacza sie od 20 pix i rosnie do 120 pix
    // wiec wysokosc = wyznaczona ilosc - 20
    // nie moze byc mniejsze od 0
    // Lev * MaxH = total number of pixels to draw
    Static[StaticLevel[PP]].Visible := true;
    Static[StaticLevel[PP]].Texture.H := Lev * MaxH - Static[StaticBackLevelRound[PP]].Texture.H / 2;
    if Static[StaticLevel[PP]].Texture.H < 0 then Static[StaticLevel[PP]].Visible := false;

    // Y doesn't change and depend on the back texture coordinate
    Static[StaticLevel[PP]].Texture.Y := Static[StaticBackLevel[PP]].Texture.Y + Static[StaticBackLevel[PP]].Texture.H  - Static[StaticLevel[PP]].Texture.H;

    // we modify LevelRound texture by changing it's Y. TexY1 and TexY2 change when the height to draw is lower than 20
    if Lev * MaxH < Static[StaticBackLevelRound[PP]].Texture.H / 2 then begin
      // when it's lower than 20 => we move TexY1 and TexY2 higher to show only part of this texture
      Static[StaticLevelRound[PP]].Texture.Y := Static[StaticBackLevel[PP]].Texture.Y + Static[StaticBackLevel[PP]].Texture.H - Static[StaticBackLevelRound[PP]].Texture.H;
      // - 0.25 when points = 0
      // - 0 wnen there are more points
      // if Lev * MaxH = Static[StaticBackLevelRound[PP]].Texture.H / 2) then we do not change it
      // if Lev * MaxH = 0 then we substract 0.25
      // we substract (0.25 - 0.25 * (Lev * MaxH)/Static[StaticBackLevelRound[PP]].Texture.H / 2)
      Wsp := Lev * MaxH / (Static[StaticBackLevelRound[PP]].Texture.H / 2);
      Static[StaticLevelRound[PP]].Texture.TexY1 := Static[StaticBackLevelRound[PP]].Texture.TexY1 - 0.25 + 0.25 * Wsp;
      Static[StaticLevelRound[PP]].Texture.TexY2 := Static[StaticBackLevelRound[PP]].Texture.TexY2 - 0.25 + 0.25 * Wsp;
    end else begin
      // when it's higher or equal 20 => full texture is being shown
      Static[StaticLevelRound[PP]].Texture.TexY1 := Static[StaticBackLevelRound[PP]].Texture.TexY1;
      Static[StaticLevelRound[PP]].Texture.TexY2 := Static[StaticBackLevelRound[PP]].Texture.TexY2;
      Static[StaticLevelRound[PP]].Texture.Y := Static[StaticLevel[PP]].Texture.Y - Static[StaticBackLevelRound[PP]].Texture.H;
    end;

  end; // for
  end; // if

  LCD.HideCursor;
  LCD.Clear;
  LCD.WriteText(1, Ini.Name[0]);
  LCD.WriteText(2, 'Score: ' + Text[TextTotalScore[1]].Text);

end;

function TScreenScore.Draw: boolean;
var
{  Min:    real;
  Max:    real;
  Wsp:    real;
  Wsp2:   real;
  Pet:    integer;}

  Item:   integer;
  P:      integer;
  C:      integer;
begin
  // star animation
{  Animation := Animation + TimeSkip*1000;

  // move right
  Min := 0; Max := 500;
  if (Animation >= Min) and (Animation < Max) then begin
    Wsp := (Animation - Min) / (Max - Min);
    Wsp2 := 1 - Wsp;

    Static[0].Texture.X := 400 + Wsp2 * 50; // prawa mala
    Static[0].Texture.Y := 150 - Wsp2 * 500;
    Static[1].Texture.X := -50 - Wsp2 * 500; // lewa mala
    Static[1].Texture.Y := 200 + Wsp2 * 50;
    Static[2].Texture.X := 100 - Wsp2 * 200;  // gorna w prawo
    Static[2].Texture.Y := 80 - Wsp2 * 200;
    Static[3].Texture.X := -280 - Wsp2 * 1000; // lewa wieksza gorna
    Static[3].Texture.Y := 90;


    Static[4].Texture.X := -1200 + Wsp * 1000;
    Text[0].X := Static[4].Texture.X + 430;
  end;

  // slowly move right
  Min := 500; Max := 4100;
  if (Animation >= Min) and (Animation < Max) then begin
    Wsp := (Animation - Min) / (Max - Min);

    Static[0].Texture.X := 400 - Wsp * 10; // prawa mala
    Static[0].Texture.Y := 150 + Wsp * 50;
    Static[1].Texture.X := -50 + Wsp * 50; // lewa mala
    Static[1].Texture.Y := 200;
    Static[2].Texture.X := 100 + Wsp * 50;  // gorna w prawo
    Static[2].Texture.Y := 80 + Wsp * 30;
    Static[3].Texture.X := -280 + Wsp * 200; // lewa wieksza gorna
    Static[3].Texture.Y := 90;

    Static[4].Texture.X := -200 + Wsp * 150; // duza glowna
    Text[0].X := Static[4].Texture.X + 430;
  end;

  // fast move right
  Min := 4100; Max := 4400;
  if (Animation >= Min) and (Animation < Max) then begin
    Wsp := (Animation - Min) / (Max - Min);
    Wsp2 := 1 - Wsp;

    Static[0].Texture.X := 390 - Wsp * 200; // prawa mala
    Static[0].Texture.Y := 200 + Wsp * 1000;
    Static[1].Texture.X := 0 + Wsp * 1000; // lewa mala
    Static[1].Texture.Y := 200;
    Static[2].Texture.X := 150 + Wsp * 1000;  // gorna w prawo
    Static[2].Texture.Y := 110 + Wsp * 600;
    Static[3].Texture.X := -80 + Wsp * 2000; // lewa wieksza gorna
    Static[3].Texture.Y := 90;

    Static[4].Texture.X := -50 + Wsp * 2000;
    Text[0].X := Static[4].Texture.X + 430;

    Static[7].Texture.X := 100 + Wsp2 * 3000;
    Text[1].X := Static[7].Texture.X + 230; // 300
    Text[1].Y := Static[7].Texture.Y + 140; // 120 Sh

    Text[2].X := Static[7].Texture.X + 250;
    Text[2].Y := Static[7].Texture.Y - 250;
    Text[3].X := Static[7].Texture.X + 250;
    Text[3].Y := Static[7].Texture.Y - 200;
  end;

  // last arrow
  Min := 4400; Max := 8000;
  if (Animation >= Min) and (Animation < Max) then begin
    Wsp := (Animation - Min) / (Max - Min);

    Static[7].Texture.X := 100 - Wsp * 100;
    Text[1].X := Static[7].Texture.X + 230; // 300
    Text[1].Y := Static[7].Texture.Y + 140; // 120

    Text[2].X := Static[7].Texture.X + 250;
    Text[2].Y := Static[7].Texture.Y - 250;
    Text[3].X := Static[7].Texture.X + 250;
    Text[3].Y := Static[7].Texture.Y - 200;
  end;

  // fade last arrow to left
  Min := 8000; Max := 8300;
  if (Animation >= Min) and (Animation < Max) then begin
    Wsp := (Animation - Min) / (Max - Min);

    Static[7].Texture.X := 0 - Wsp * 3000;
    Static[7].Texture.Y := 340 - Wsp * 50;
    Text[1].X := Static[7].Texture.X + 230; // 300 Sh
    Text[1].Y := Static[7].Texture.Y + 140; // 120 Sh

    Text[2].X := Static[7].Texture.X + 250;
    Text[2].Y := Static[7].Texture.Y - 250;
    Text[3].X := Static[7].Texture.X + 250;
    Text[3].Y := Static[7].Texture.Y - 200;
  end;

  Min := 8300;
  if (Animation >= Min) and (not Fadeout) then begin
    Music.StopShuffle;
    FadeTo(@ScreenSong);
    Fadeout := true;
  end;}


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
end;

procedure TScreenScore.FillPlayer(Item, P: integer);
var
  S:    string;
begin
  Text[TextName[Item]].Text := Ini.Name[P];

  S := IntToStr((Round(Player[P].Score) div 10) * 10);
  while (Length(S)<4) do S := '0' + S;
  Text[TextNotesScore[Item]].Text := S;

  while (Length(S)<5) do S := '0' + S;
  Text[TextTotalScore[Item]].Text := S;

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
