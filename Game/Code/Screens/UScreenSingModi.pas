unit UScreenSingModi;

interface

uses UMenu, UMusic, SDL, SysUtils, UPliki, UTime, USongs, UIni, ULog, USmpeg, UTexture, ULyrics,
  TextGL, OpenGL12, BASS, UThemes, ULCD, UScreenSing, ModiSDK;

type
  TScreenSingModi = class(TScreenSing)
    protected
    //paused: boolean; //Pause Mod
    //PauseTime: Real;
    //NumEmptySentences: integer;
    public
      //TextTime:           integer;

      //StaticP1:           integer;
      //StaticP1ScoreBG:    integer;
      //TextP1:             integer;
      //TextP1Score:        integer;

      //StaticP2R:          integer;
      //StaticP2RScoreBG:   integer;
      //TextP2R:            integer;
      //TextP2RScore:       integer;

      //StaticP2M:          integer;
      //StaticP2MScoreBG:   integer;
      //TextP2M:            integer;
      //TextP2MScore:       integer;

      //StaticP3R:          integer;
      //StaticP3RScoreBG:   integer;
      //TextP3R:            integer;
      //TextP3RScore:       integer;

      //Tex_Background:     TTexture;
      //FadeOut:            boolean;
      //LyricMain:          TLyric;
      //LyricSub:           TLyric;
      Winner: Byte; //Who Wins
      PlayerInfo: TPlayerInfo;
      TeamInfo:   TTeamInfo;

      constructor Create; override;
      procedure onShow; override;
      //procedure onShowFinish; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function Draw: boolean; override;
      procedure Finish; override;
      //procedure UpdateLCD;
      //procedure Pause; //Pause Mod(Toggles Pause)
  end;

//Procedured for Plugin
function LoadTex   (const Name, Typ: PChar): TsmallTexture; stdcall;
//function Translate (const Name: PChar): PChar; stdcall;
procedure Print (const Style, Size: Byte; const X, Y: Real; const Text: PChar); stdcall;       //Procedure to Print Text
function LoadSound  (const Name: PChar): Cardinal; stdcall;       //Procedure that loads a Custom Sound
procedure PlaySound (const Index: Cardinal); stdcall;       //Plays a Custom Sound

//Utilys
function ToSentences(Const Czeski: TCzesci): TSentences;

implementation
uses UGraphic, UDraw, UMain, Classes, URecord, ULanguage, math, UDLLManager, USkins, UGraphicClasses;

// Method for input parsing. If False is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenSingModi.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_ESCAPE :
        begin
          Finish;
          Music.PlayBack;
          FadeTo(@ScreenPartyScore);
        end;

      else
        Result := inherited ParseInput(PressedKey, ScanCode, PressedDown);
    end;
  end;
end;

constructor TScreenSingModi.Create;
begin
  inherited Create;

end;

function ToSentences(Const Czeski: TCzesci): TSentences;
var
  I, J: Integer;
begin
  Result.Akt := Czeski.Akt;
  Result.High := Czeski.High;
  Result.Ilosc := Czeski.Ilosc;
  Result.Resolution := Czeski.Resolution;
  Result.NotesGAP := Czeski.NotesGAP;
  Result.TotalLength := Czeski.Wartosc;

  SetLength(Result.Sentence, Length(Czeski.Czesc));
  for I := low(Result.Sentence) to high(Result.Sentence) do
  begin
    Result.Sentence[I].Start := Czeski.Czesc[I].Start;
    Result.Sentence[I].StartNote := Czeski.Czesc[I].StartNote;
    Result.Sentence[I].Lyric := Czeski.Czesc[I].Lyric;
    Result.Sentence[I].LyricWidth := Czeski.Czesc[I].LyricWidth;
    Result.Sentence[I].Koniec := Czeski.Czesc[I].Koniec;
    Result.Sentence[I].BaseNote := Czeski.Czesc[I].BaseNote;
    Result.Sentence[I].HighNote := Czeski.Czesc[I].HighNut;
    Result.Sentence[I].IlNut := Czeski.Czesc[I].IlNut;
    Result.Sentence[I].TotalNotes := Czeski.Czesc[I].TotalNotes;

    SetLength(Result.Sentence[I].Note, Length(Czeski.Czesc[I].Nuta));
    for J := low(Result.Sentence[I].Note) to high(Result.Sentence[I].Note) do
    begin
      Result.Sentence[I].Note[J].Color := Czeski.Czesc[I].Nuta[J].Color;
      Result.Sentence[I].Note[J].Start := Czeski.Czesc[I].Nuta[J].Start;
      Result.Sentence[I].Note[J].Length := Czeski.Czesc[I].Nuta[J].Dlugosc;
      Result.Sentence[I].Note[J].Ton := Czeski.Czesc[I].Nuta[J].Ton;
      Result.Sentence[I].Note[J].TonGamy := Czeski.Czesc[I].Nuta[J].TonGamy;
      //Result.Sentence[I].Note[J].Text := Czeski.Czesc[I].Nuta[J].Tekst;
      Result.Sentence[I].Note[J].FreeStyle := Czeski.Czesc[I].Nuta[J].FreeStyle;
      Result.Sentence[I].Note[J].Typ := Czeski.Czesc[I].Nuta[J].Wartosc;
    end;
  end;
end;

procedure TScreenSingModi.onShow;
{var
  P:      integer;
  V1:     boolean;
  V2R:    boolean;
  V2M:    boolean;
  V3R:    boolean;
  NR:       TRecR; //Line Bonus Mod    }
var
  I: Integer;
begin

 { Log.LogStatus('Begin', 'onShow');
  FadeOut := false; // 0.5.0: early 0.5.0 problems were by this line commented

  // prepare players
  SetLength(Player, PlayersPlay);
//  Player[0].ScoreTotalI := 0;


  case PlayersPlay of
    1:  begin
          V1 := true;
          V2R := false;
          V2M := false;
          V3R := false;
        end;
    2:  begin
          V1 := true;
          V2R := true;
          V2M := false;
          V3R := false;
        end;
    3:  begin
          V1 := true;
          V2R := false;
          V2M := true;
          V3R := true;
        end;
    4:  begin // double screen
          V1 := true;
          V2R := true;
          V2M := false;
          V3R := false;
        end;
    6:  begin // double screen
          V1 := true;
          V2R := false;
          V2M := true;
          V3R := true;
        end;

  end;



  Static[StaticP2R].Visible := V2R;
  Static[StaticP2RScoreBG].Visible := V2R;
  Text[TextP2R].Visible := V2R;
  Text[TextP2RScore].Visible := V2R;

  Static[StaticP2M].Visible := V2M;
  Static[StaticP2MScoreBG].Visible := V2M;
  Text[TextP2M].Visible := V2M;
  Text[TextP2MScore].Visible := V2M;

  Static[StaticP3R].Visible := V3R;
  Static[StaticP3RScoreBG].Visible := V3R;
  Text[TextP3R].Visible := V3R;
  Text[TextP3RScore].Visible := V3R;



  // load notes
  CzyscNuty;
//  Log.LogWarning(CatSongs.Song[CatSongs.Selected].Path + CatSongs.Song[CatSongs.Selected].FileName, '!!!');
  AktSong := CatSongs.Song[CatSongs.Selected];

  WczytajCzesci(CatSongs.Song[CatSongs.Selected].Path + CatSongs.Song[CatSongs.Selected].FileName);
  AktSong.Path := CatSongs.Song[CatSongs.Selected].Path;
//  AktSong.GAP := AktSong.GAP + 40 {4096 = 100ms for buffer}{ + 20 {microphone}{ + 60000 / AktSong.BPM[0].BPM / 2; // temporary until UMain will be fixed

  // set movie
  if (AktSong.Video <> '') and FileExists(AktSong.Path + AktSong.Video) then begin
    OpenSmpeg(AktSong.Path + AktSong.Video);
    SkipSmpeg(AktSong.VideoGAP + AktSong.Start);
    AktSong.VideoLoaded := true;
  end;

  // set background
  if (AktSong.Background <> '')  and (AktSong.VideoLoaded = false) then
    Tex_Background := Texture.LoadTexture(AktSong.Path + AktSong.Background)
  else
    Tex_Background.TexNum := -1;

  // play music (I)
  //Music.CaptureStart;
  Music.MoveTo(AktSong.Start);
//  Music.Play;

  // prepare timer (I)
//  CountSkipTimeSet;
  Czas.Teraz := AktSong.Start;
  Czas.Razem := Music.Length;
  if (AktSong.Finish > 0) then Czas.Razem := AktSong.Finish / 1000;
  Czas.OldBeat := -1;
  for P := 0 to High(Player) do
    ClearScores(P);

  // main text
  LyricMain.Clear;
  LyricMain.X := 400;
  LyricMain.Y := Skin_LyricsT;
  LyricMain.Scale := 1.4; //1.4
  LyricMain.Align := 1;

  // sub text
  LyricSub.Clear;
  LyricSub.X := 400;
  LyricSub.Y := Skin_LyricsT + 42; //40
  LyricSub.Align := 1;

  // set custom options
  case Ini.LyricsFont of
    0:
      begin
        LyricMain.FontStyle := 0;
        LyricSub.FontStyle := 0;
        LyricMain.Size := 14; // 13
        LyricSub.Size := 14; // 13
        LyricMain.ColR := Skin_FontR;
        LyricMain.ColG := Skin_FontG;
        LyricMain.ColB := Skin_FontB; //Change für Crazy Joker
        {LyricMain.ColSR := Skin_FontHighlightR;
        LyricMain.ColSG := Skin_FontHighlightG;
        LyricMain.ColSB := Skin_FontHighlightB;1aa5dc}    {
        LyricMain.ColSR := 26/255;
        LyricMain.ColSG := 165/255;
        LyricMain.ColSB := 220/255;

        LyricSub.ColR := 0.6;
        LyricSub.ColG := 0.6;
        LyricSub.ColB := 0.6;
      end;
    1:
      begin
        LyricMain.FontStyle := 2;
        LyricSub.FontStyle := 2;
        LyricMain.Size := 14;
        LyricSub.Size := 14;
        LyricMain.ColR := 0.75;
        LyricMain.ColG := 0.75;
        LyricMain.ColB := 1;
        LyricMain.ColSR := 0.5;
        LyricMain.ColSG := 0.5;
        LyricMain.ColSB := 1;
        LyricSub.ColR := 0.8;
        LyricSub.ColG := 0.8;
        LyricSub.ColB := 0.8;
      end;
    2:
      begin
        LyricMain.FontStyle := 3;
        LyricSub.FontStyle := 3;
        LyricMain.Size := 12;
        LyricSub.Size := 12;
        LyricMain.ColR := 0.75;
        LyricMain.ColG := 0.75;
        LyricMain.ColB := 1;
        LyricMain.ColSR := 0.5;
        LyricMain.ColSG := 0.5;
        LyricMain.ColSB := 1;
        LyricSub.ColR := 0.8;
        LyricSub.ColG := 0.8;
        LyricSub.ColB := 0.8;
      end;
  end; // case

  case Ini.LyricsEffect of
    0:  LyricMain.Style := 1; // 0 - one selected, 1 - selected all to the current
    1:  LyricMain.Style := 2;
    2:  LyricMain.Style := 3;
    3:  LyricMain.Style := 4;
  end; // case

  // fill texts
  LyricMain.AddCzesc(0);
  LyricMain.Selected := -1;
  LyricSub.AddCzesc(1);
  LyricSub.Selected := -1;

  UpdateLCD;

  //Deactivate Pause
  Paused := False;

  {Static[StaticP2R].Visible := V2R;
  Static[StaticP2RScoreBG].Visible := V2R;
  Text[TextP2R].Visible := V2R;
  Text[TextP2RScore].Visible := V2R;

  Static[StaticP2M].Visible := V2M;
  Static[StaticP2MScoreBG].Visible := V2M;
  Text[TextP2M].Visible := V2M;
  Text[TextP2MScore].Visible := V2M;

  Static[StaticP3R].Visible := V3R;
  Static[StaticP3RScoreBG].Visible := V3R;
  Text[TextP3R].Visible := V3R;
  Text[TextP3RScore].Visible := V3R;}     {

  //Set Position of Line Bonus - PhrasenBonus
  if (Ini.LineBonus = 1) then //Show Line Bonus at Scores
  begin
  Case PlayersPlay of
    1: begin
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1ScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1Score.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1ScoreBG.x;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1Score.Y + 65;
    end;

    2: begin
      //P1
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1ScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1Score.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1ScoreBG.X;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1Score.Y + 65;

      //P2
      Player[1].LineBonus_TargetX := Theme.Sing.StaticP2RScoreBG.X;
      Player[1].LineBonus_TargetY := Theme.Sing.TextP2RScore.Y;
      Player[1].LineBonus_StartX  := Theme.Sing.StaticP2RScoreBG.X;
      Player[1].LineBonus_StartY  := Theme.Sing.TextP2RScore.Y + 65;
    end;

    3: begin
      //P1
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1ScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1Score.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1ScoreBG.x;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1Score.Y + 65;

      //P2
      Player[1].LineBonus_TargetX := Theme.Sing.StaticP2MScoreBG.x;
      Player[1].LineBonus_TargetY := Theme.Sing.TextP2MScore.Y;
      Player[1].LineBonus_StartX  := Theme.Sing.StaticP2MScoreBG.x;
      Player[1].LineBonus_StartY  := Theme.Sing.TextP2MScore.Y + 65;

      //P3
      Player[2].LineBonus_TargetX := Theme.Sing.StaticP3RScoreBG.x;
      Player[2].LineBonus_TargetY := Theme.Sing.TextP3RScore.Y;
      Player[2].LineBonus_StartX  := Theme.Sing.StaticP3RScoreBG.x;
      Player[2].LineBonus_StartY  := Theme.Sing.TextP3RScore.Y + 65;
    end;

    4: begin
      //P1
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1ScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1Score.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1ScoreBG.x;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1Score.Y + 65;

      //P2
      Player[1].LineBonus_TargetX := Theme.Sing.StaticP2RScoreBG.x;
      Player[1].LineBonus_TargetY := Theme.Sing.TextP2RScore.Y;
      Player[1].LineBonus_StartX  := Theme.Sing.StaticP2RScoreBG.x;
      Player[1].LineBonus_StartY  := Theme.Sing.TextP2RScore.Y + 65;

      //P3
      Player[2].LineBonus_TargetX := Theme.Sing.StaticP1ScoreBG.x;
      Player[2].LineBonus_TargetY := Theme.Sing.TextP1Score.Y;
      Player[2].LineBonus_StartX  := Theme.Sing.StaticP1ScoreBG.x;
      Player[2].LineBonus_StartY  := Theme.Sing.TextP1Score.Y + 65;

      //P4
      Player[3].LineBonus_TargetX := Theme.Sing.StaticP2RScoreBG.x;
      Player[3].LineBonus_TargetY := Theme.Sing.TextP2RScore.Y;
      Player[3].LineBonus_StartX  := Theme.Sing.StaticP2RScoreBG.x;
      Player[3].LineBonus_StartY  := Theme.Sing.TextP2RScore.Y + 65;
    end;

    6: begin
      //P1
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1ScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1Score.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1ScoreBG.x;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1Score.Y + 65;

      //P2
      Player[1].LineBonus_TargetX := Theme.Sing.StaticP2MScoreBG.x;
      Player[1].LineBonus_TargetY := Theme.Sing.TextP2MScore.Y;
      Player[1].LineBonus_StartX  := Theme.Sing.StaticP2MScoreBG.x;
      Player[1].LineBonus_StartY  := Theme.Sing.TextP2MScore.Y + 65;

      //P3
      Player[2].LineBonus_TargetX := Theme.Sing.StaticP3RScoreBG.x;
      Player[2].LineBonus_TargetY := Theme.Sing.TextP3RScore.Y;
      Player[2].LineBonus_StartX  := Theme.Sing.StaticP3RScoreBG.x;
      Player[2].LineBonus_StartY  := Theme.Sing.TextP3RScore.Y + 65;

      //P4
      Player[3].LineBonus_TargetX := Theme.Sing.StaticP1ScoreBG.x;
      Player[3].LineBonus_TargetY := Theme.Sing.TextP1Score.Y;
      Player[3].LineBonus_StartX  := Theme.Sing.StaticP1ScoreBG.x;
      Player[3].LineBonus_StartY  := Theme.Sing.TextP1Score.Y + 65;

      //P5
      Player[4].LineBonus_TargetX := Theme.Sing.StaticP2MScoreBG.x;
      Player[4].LineBonus_TargetY := Theme.Sing.TextP2MScore.Y;
      Player[4].LineBonus_StartX  := Theme.Sing.StaticP2MScoreBG.x;
      Player[4].LineBonus_StartY  := Theme.Sing.TextP2MScore.Y + 65;

      //P6
      Player[5].LineBonus_TargetX := Theme.Sing.StaticP3RScoreBG.x;
      Player[5].LineBonus_TargetY := Theme.Sing.TextP3RScore.Y;
      Player[5].LineBonus_StartX  := Theme.Sing.StaticP3RScoreBG.x;
      Player[5].LineBonus_StartY  := Theme.Sing.TextP3RScore.Y + 65;
    end;
  end;
  end
  else if (Ini.LineBonus = 2) then //Show Line Bonus at Notes
  begin
  //SingDrawNoteLines(Nr.Left + 10*ScreenX, 120, Nr.Right + 10*ScreenX, 12);
  //SingDrawNoteLines(Nr.Left + 10*ScreenX, 245, Nr.Right + 10*ScreenX, 12);
  //SingDrawNoteLines(Nr.Left + 10*ScreenX, 370, Nr.Right + 10*ScreenX, 12);

  // positions
  if Ini.SingWindow = 0 then begin
    NR.Left := 120;
  end else begin
    NR.Left := 20;
  end;
  NR.Right := 780;

  NR.Width := NR.Right - NR.Left;
  NR.WMid := NR.Width / 2;
  NR.Mid := NR.Left + NR.WMid;

  Case PlayersPlay of
    1: begin
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[0].LineBonus_TargetY := Skin_P2_NotesB - 105 - 65;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[0].LineBonus_StartY  := Skin_P2_NotesB - 105;
    end;

    2: begin
      //P1
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[0].LineBonus_TargetY := Skin_P1_NotesB - 105 - 65 + 28;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[0].LineBonus_StartY  := Skin_P1_NotesB - 105 + 28;

      //P2
      Player[1].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[1].LineBonus_TargetY := Skin_P2_NotesB - 105 - 65 + 28;
      Player[1].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[1].LineBonus_StartY  := Skin_P2_NotesB - 105 + 28;
    end;

    3: begin
      //P1
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[0].LineBonus_TargetY := 120 - 65 + 28;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[0].LineBonus_StartY  := 120 + 28;

      //P2
      Player[1].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[1].LineBonus_TargetY := 245 - 65 + 28;
      Player[1].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[1].LineBonus_StartY  := 245 + 28;

      //P3
      Player[2].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[2].LineBonus_TargetY := 370 - 65 + 28;
      Player[2].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[2].LineBonus_StartY  := 370 + 28;
    end;

    4: begin
      //P1
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[0].LineBonus_TargetY := Skin_P1_NotesB - 105 - 65 + 28;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[0].LineBonus_StartY  := Skin_P1_NotesB - 105 + 28;

      //P2
      Player[1].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[1].LineBonus_TargetY := Skin_P2_NotesB - 105 - 65 + 28;
      Player[1].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[1].LineBonus_StartY  := Skin_P2_NotesB - 105 + 28;

      //P3
      Player[2].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[2].LineBonus_TargetY := Skin_P1_NotesB - 105 - 65 + 28;
      Player[2].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[2].LineBonus_StartY  := Skin_P1_NotesB - 105 + 28;

      //P4
      Player[3].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[3].LineBonus_TargetY := Skin_P2_NotesB - 105 - 65 + 28;
      Player[3].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[3].LineBonus_StartY  := Skin_P2_NotesB - 105 + 28;
    end;

    6: begin
      //P1
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[0].LineBonus_TargetY := 120 - 65 + 28;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[0].LineBonus_StartY  := 120 + 28;

      //P2
      Player[1].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[1].LineBonus_TargetY := 245 - 65 + 28;
      Player[1].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[1].LineBonus_StartY  := 245 + 28;

      //P3
      Player[2].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[2].LineBonus_TargetY := 370 - 65 + 28;
      Player[2].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[2].LineBonus_StartY  := 370 + 28;

      //P4
      Player[3].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[3].LineBonus_TargetY := 120 - 65 + 28;
      Player[3].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[3].LineBonus_StartY  := 120 + 28;

      //P5
      Player[4].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[4].LineBonus_TargetY := 245 - 65 + 28;
      Player[4].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[4].LineBonus_StartY  := 245 + 28;

      //P6
      Player[5].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 87);
      Player[5].LineBonus_TargetY := 370 - 65 + 28;
      Player[5].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 87);
      Player[5].LineBonus_StartY  := 370 + 28;
    end;
  end;
  end;
  //Set Position of Line Bonus - PhrasenBonus End
  //Set Num of Empty Sentences for Phrasen Bonus
  NumEmptySentences := 0;
  for P := low(Czesci[0].Czesc) to high(Czesci[0].Czesc) do
    if Czesci[0].Czesc[P].TotalNotes = 0 then Inc(NumEmptySentences);

  Log.LogStatus('End', 'onShow');  }

  PlayersPlay := TeamInfo.NumTeams;

  if DLLMan.Selected.LoadSong then //Start with Song
  begin
    inherited;
  end
  else //Start Without Song
  begin
    Music.CaptureStart;
  end;

//Set Playerinfo
  PlayerInfo.NumPlayers := PlayersPlay;
  for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    PlayerInfo.Playerinfo[I].Name := PChar(Ini.Name[I]);
    PlayerInfo.Playerinfo[I].Score:=  0;
    PlayerInfo.Playerinfo[I].Bar :=  50;
    PlayerInfo.Playerinfo[I].Enabled := True;
  end;

  for I := PlayerInfo.NumPlayers to high(PlayerInfo.Playerinfo) do
  begin
    PlayerInfo.Playerinfo[I].Score:=  0;
    PlayerInfo.Playerinfo[I].Bar :=  0;
    PlayerInfo.Playerinfo[I].Enabled := False;
  end;

  Case PlayersPlay of
    1: begin
      PlayerInfo.Playerinfo[0].PosX := Static[StaticP1ScoreBG].Texture.X;
      PlayerInfo.Playerinfo[0].PosY := Static[StaticP1ScoreBG].Texture.Y + Static[StaticP1ScoreBG].Texture.H;
    end;
    2,4: begin
      PlayerInfo.Playerinfo[0].PosX := Static[StaticP1TwoPScoreBG].Texture.X;
      PlayerInfo.Playerinfo[0].PosY := Static[StaticP1TwoPScoreBG].Texture.Y + Static[StaticP1TwoPScoreBG].Texture.H;
      PlayerInfo.Playerinfo[2].PosX := Static[StaticP1TwoPScoreBG].Texture.X;
      PlayerInfo.Playerinfo[2].PosY := Static[StaticP1TwoPScoreBG].Texture.Y + Static[StaticP1TwoPScoreBG].Texture.H;
      PlayerInfo.Playerinfo[1].PosX := Static[StaticP2RScoreBG].Texture.X;
      PlayerInfo.Playerinfo[1].PosY := Static[StaticP2RScoreBG].Texture.Y + Static[StaticP2RScoreBG].Texture.H;
      PlayerInfo.Playerinfo[3].PosX := Static[StaticP2RScoreBG].Texture.X;
      PlayerInfo.Playerinfo[3].PosY := Static[StaticP2RScoreBG].Texture.Y + Static[StaticP2RScoreBG].Texture.H;
    end;
    3,6: begin
      PlayerInfo.Playerinfo[0].PosX := Static[StaticP1ThreePScoreBG].Texture.X;
      PlayerInfo.Playerinfo[0].PosY := Static[StaticP1ThreePScoreBG].Texture.Y + Static[StaticP1ThreePScoreBG].Texture.H;
      PlayerInfo.Playerinfo[3].PosX := Static[StaticP1ThreePScoreBG].Texture.X;
      PlayerInfo.Playerinfo[3].PosY := Static[StaticP1ThreePScoreBG].Texture.Y + Static[StaticP1ThreePScoreBG].Texture.H;
      PlayerInfo.Playerinfo[1].PosX := Static[StaticP2MScoreBG].Texture.X;
      PlayerInfo.Playerinfo[1].PosY := Static[StaticP2MScoreBG].Texture.Y + Static[StaticP2MScoreBG].Texture.H;
      PlayerInfo.Playerinfo[4].PosX := Static[StaticP2MScoreBG].Texture.X;
      PlayerInfo.Playerinfo[4].PosY := Static[StaticP2MScoreBG].Texture.Y + Static[StaticP2MScoreBG].Texture.H;
      PlayerInfo.Playerinfo[2].PosX := Static[StaticP3RScoreBG].Texture.X;
      PlayerInfo.Playerinfo[2].PosY := Static[StaticP3RScoreBG].Texture.Y + Static[StaticP3RScoreBG].Texture.H;
      PlayerInfo.Playerinfo[5].PosX := Static[StaticP3RScoreBG].Texture.X;
      PlayerInfo.Playerinfo[5].PosY := Static[StaticP3RScoreBG].Texture.Y + Static[StaticP3RScoreBG].Texture.H;
    end;
  end;

  // play music (I)
  //Music.CaptureStart;
  //Music.MoveTo(AktSong.Start);

  //Init Plugin
  if not DLLMan.PluginInit(TeamInfo, PlayerInfo, ToSentences(Czesci[0]), LoadTex, Print, LoadSound, PlaySound) then
  begin
    //Fehler
    Log.LogError('Could not Init Plugin');
    Halt;
  end;

  // Set Background (Little Workaround, maybe change sometime)
  if (DLLMan.Selected.LoadBack) AND (DLLMan.Selected.LoadSong) then
    ScreenSing.Tex_Background := Tex_Background;

  Winner := 0;

  //Set Score Visibility
  if PlayersPlay = 1 then begin
    Text[TextP1Score].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP1ScoreBG].Visible := DLLMan.Selected.ShowScore;
  end;

  if (PlayersPlay = 2) OR (PlayersPlay = 4) then begin
    Text[TextP1TwoPScore].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP1TwoPScoreBG].Visible := DLLMan.Selected.ShowScore;

    Text[TextP2RScore].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP2RScoreBG].Visible := DLLMan.Selected.ShowScore;
  end;

  if (PlayersPlay = 3) OR (PlayersPlay = 6) then begin
    Text[TextP1ThreePScore].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP1ThreePScoreBG].Visible := DLLMan.Selected.ShowScore;

    Text[TextP2MScore].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP2MScoreBG].Visible := DLLMan.Selected.ShowScore;

    Text[TextP3RScore].Visible := DLLMan.Selected.ShowScore;
    Static[StaticP3RScoreBG].Visible := DLLMan.Selected.ShowScore;
  end;
end;

function TScreenSingModi.Draw: boolean;
var
  Min:    integer;
  Sec:    integer;
  Tekst:  string;
  S, I:      integer;
  T:      integer;
begin
//Set Playerinfo
  PlayerInfo.NumPlayers := PlayersPlay;
  for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    PlayerInfo.Playerinfo[I].Name := PChar(Player[I].Name);
    if PlayerInfo.Playerinfo[I].Enabled then
    begin
      if (Player[I].ScoreTotalI<=10000) then
        PlayerInfo.Playerinfo[I].Score:=  Player[I].ScoreTotalI;
      PlayerInfo.Playerinfo[I].Bar :=  Player[I].ScorePercent;
    end;
  end;

//Show Score
if DLLMan.Selected.ShowScore then
begin
  //ScoreBG Mod
  // set player colors
  if PlayersPlay = 4 then begin
    if ScreenAct = 1 then begin
      LoadColor(Static[StaticP1TwoP].Texture.ColR, Static[StaticP1TwoP].Texture.ColG,
      Static[StaticP1TwoP].Texture.ColB, 'P1Dark');
      LoadColor(Static[StaticP2R].Texture.ColR, Static[StaticP2R].Texture.ColG,
      Static[StaticP2R].Texture.ColB, 'P2Dark');



      LoadColor(Static[StaticP1TwoPScoreBG].Texture.ColR, Static[StaticP1TwoPScoreBG].Texture.ColG,
      Static[StaticP1TwoPScoreBG].Texture.ColB, 'P1Dark');
      LoadColor(Static[StaticP2RScoreBG].Texture.ColR, Static[StaticP2RScoreBG].Texture.ColG,
      Static[StaticP2RScoreBG].Texture.ColB, 'P2Dark');



    end;
    if ScreenAct = 2 then begin
      LoadColor(Static[StaticP1TwoP].Texture.ColR, Static[StaticP1TwoP].Texture.ColG,
        Static[StaticP1TwoP].Texture.ColB, 'P3Dark');
      LoadColor(Static[StaticP2R].Texture.ColR, Static[StaticP2R].Texture.ColG,
        Static[StaticP2R].Texture.ColB, 'P4Dark');



      LoadColor(Static[StaticP1TwoPScoreBG].Texture.ColR, Static[StaticP1TwoPScoreBG].Texture.ColG,
        Static[StaticP1TwoPScoreBG].Texture.ColB, 'P3Dark');
      LoadColor(Static[StaticP2RScoreBG].Texture.ColR, Static[StaticP2RScoreBG].Texture.ColG,
        Static[StaticP2RScoreBG].Texture.ColB, 'P4Dark');



     end;
  end;

  if PlayersPlay = 6 then begin
    if ScreenAct = 1 then begin
      LoadColor(Static[StaticP1ThreeP].Texture.ColR, Static[StaticP1ThreeP].Texture.ColG,
        Static[StaticP1ThreeP].Texture.ColB, 'P1Dark');
      LoadColor(Static[StaticP2M].Texture.ColR, Static[StaticP2M].Texture.ColG,
        Static[StaticP2R].Texture.ColB, 'P2Dark');
      LoadColor(Static[StaticP3R].Texture.ColR, Static[StaticP3R].Texture.ColG,
        Static[StaticP3R].Texture.ColB, 'P3Dark');



      LoadColor(Static[StaticP1ThreePScoreBG].Texture.ColR, Static[StaticP1ThreePScoreBG].Texture.ColG,
        Static[StaticP1ThreePScoreBG].Texture.ColB, 'P1Dark');
      LoadColor(Static[StaticP2MScoreBG].Texture.ColR, Static[StaticP2MScoreBG].Texture.ColG,
        Static[StaticP2RScoreBG].Texture.ColB, 'P2Dark');
      LoadColor(Static[StaticP3RScoreBG].Texture.ColR, Static[StaticP3RScoreBG].Texture.ColG,
        Static[StaticP3RScoreBG].Texture.ColB, 'P3Dark');



    end;
    if ScreenAct = 2 then begin
      LoadColor(Static[StaticP1ThreeP].Texture.ColR, Static[StaticP1ThreeP].Texture.ColG,
        Static[StaticP1ThreeP].Texture.ColB, 'P4Dark');
      LoadColor(Static[StaticP2M].Texture.ColR, Static[StaticP2M].Texture.ColG,
        Static[StaticP2R].Texture.ColB, 'P5Dark');
      LoadColor(Static[StaticP3R].Texture.ColR, Static[StaticP3R].Texture.ColG,
        Static[StaticP3R].Texture.ColB, 'P6Dark');




      LoadColor(Static[StaticP1ThreePScoreBG].Texture.ColR, Static[StaticP1ThreePScoreBG].Texture.ColG,
        Static[StaticP1ThreePScoreBG].Texture.ColB, 'P4Dark');
      LoadColor(Static[StaticP2MScoreBG].Texture.ColR, Static[StaticP2MScoreBG].Texture.ColG,
        Static[StaticP2RScoreBG].Texture.ColB, 'P5Dark');
      LoadColor(Static[StaticP3RScoreBG].Texture.ColR, Static[StaticP3RScoreBG].Texture.ColG,
        Static[StaticP3RScoreBG].Texture.ColB, 'P6Dark');




    end;
  end;
  //end ScoreBG Mod

// set player names (for 2 screens and only Singstar skin)
  if ScreenAct = 1 then begin
    Text[TextP1].Text       := 'P1';
    Text[TextP1TwoP].Text   := 'P1'; // added for ps3 skin
    Text[TextP1ThreeP].Text := 'P1'; // added for ps3 skin
    Text[TextP2R].Text      := 'P2';
    Text[TextP2M].Text      := 'P2';
    Text[TextP3R].Text      := 'P3';
  end;

  if ScreenAct = 2 then begin
    case PlayersPlay of
      4:  begin
            Text[TextP1TwoP].Text := 'P3';
            Text[TextP2R].Text := 'P4';
          end;
      6:  begin
            Text[TextP1ThreeP].Text := 'P4';
            Text[TextP2M].Text := 'P5';
            Text[TextP3R].Text := 'P6';
          end;
    end; // case
  end; // if


  // stereo   <- and where iss P2M? or P3?
  Static[StaticP1].Texture.X := Static[StaticP1].Texture.X + 10*ScreenX;
  Static[StaticP1ScoreBG].Texture.X := Static[StaticP1ScoreBG].Texture.X + 10*ScreenX;

  Text[TextP1].X := Text[TextP1].X + 10*ScreenX;
  Text[TextP1Score].X := Text[TextP1Score].X + 10*ScreenX;

  Static[StaticP2R].Texture.X := Static[StaticP2R].Texture.X + 10*ScreenX;
  Static[StaticP2RScoreBG].Texture.X := Static[StaticP2RScoreBG].Texture.X + 10*ScreenX;

  Text[TextP2R].X := Text[TextP2R].X + 10*ScreenX;
  Text[TextP2RScore].X := Text[TextP2RScore].X + 10*ScreenX;

  // .. and scores
  if PlayersPlay = 1 then begin
    Tekst := IntToStr(Player[0].ScoreTotalI);
    while Length(Tekst) < 5 do Tekst := '0' + Tekst;
    Text[TextP1Score].Text := Tekst;
  end;

  if PlayersPlay = 2 then begin
    Tekst := IntToStr(Player[0].ScoreTotalI);
    while Length(Tekst) < 5 do Tekst := '0' + Tekst;
    Text[TextP1TwoPScore].Text := Tekst;

    Tekst := IntToStr(Player[1].ScoreTotalI);
    while Length(Tekst) < 5 do Tekst := '0' + Tekst;
    Text[TextP2RScore].Text := Tekst;
  end;

  if PlayersPlay = 3 then begin
    Tekst := IntToStr(Player[0].ScoreTotalI);
    while Length(Tekst) < 5 do Tekst := '0' + Tekst;
    Text[TextP1ThreePScore].Text := Tekst;

    Tekst := IntToStr(Player[1].ScoreTotalI);
    while Length(Tekst) < 5 do Tekst := '0' + Tekst;
    Text[TextP2MScore].Text := Tekst;

    Tekst := IntToStr(Player[2].ScoreTotalI);
    while Length(Tekst) < 5 do Tekst := '0' + Tekst;
    Text[TextP3RScore].Text := Tekst;
  end;

  if PlayersPlay = 4 then begin
    if ScreenAct = 1 then begin
      Tekst := IntToStr(Player[0].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP1TwoPScore].Text := Tekst;

      Tekst := IntToStr(Player[1].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP2RScore].Text := Tekst;
    end;
    if ScreenAct = 2 then begin
      Tekst := IntToStr(Player[2].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP1TwoPScore].Text := Tekst;

      Tekst := IntToStr(Player[3].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP2RScore].Text := Tekst;
    end;
  end;

  if PlayersPlay = 6 then begin
    if ScreenAct = 1 then begin
      Tekst := IntToStr(Player[0].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP1ThreePScore].Text := Tekst;

      Tekst := IntToStr(Player[1].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP2MScore].Text := Tekst;

      Tekst := IntToStr(Player[2].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP3RScore].Text := Tekst;
    end;
    if ScreenAct = 2 then begin
      Tekst := IntToStr(Player[3].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP1ThreePScore].Text := Tekst;

      Tekst := IntToStr(Player[4].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP2MScore].Text := Tekst;

      Tekst := IntToStr(Player[5].ScoreTotalI);
      while Length(Tekst) < 5 do Tekst := '0' + Tekst;
      Text[TextP3RScore].Text := Tekst;
    end;
  end;

end; //ShowScore

  for S := 1 to 1 do
    Static[S].Texture.X := Static[S].Texture.X + 10*ScreenX;

  for T := 0 to 1 do
    Text[T].X := Text[T].X + 10*ScreenX;

if DLLMan.Selected.LoadSong then
begin
  // update static menu with time ...
  Min := Round(Czas.Teraz) div 60;
  Sec := Round(Czas.Teraz) mod 60;
  Text[TextTime].Text := '';
  if Min < 10 then Text[TextTime].Text := '0';
  Text[TextTime].Text := Text[TextTime].Text + IntToStr(Min) + ':';
  if Sec < 10 then Text[TextTime].Text := Text[TextTime].Text + '0';
  Text[TextTime].Text := Text[TextTime].Text + IntToStr(Sec);
end;

  // draw static menu (BG)
  DrawBG;

  //Draw Background
  if (DllMan.Selected.LoadSong) AND (DllMan.Selected.LoadBack) then
    SingDrawBackground;

  // update and draw movie
  if ShowFinish and AktSong.VideoLoaded AND DllMan.Selected.LoadVideo then begin
    UpdateSmpeg; // this only draws
  end;

  // draw static menu (FG)
  DrawFG;

  if ShowFinish then begin
    if DllMan.Selected.LoadSong then
    begin
      if (not Music.Finished) and ((AktSong.Finish = 0) or (Czas.Teraz*1000 <= AktSong.Finish)) then begin
        //Pause Mod:
        if not Paused then
          Sing(Self);       // analyze song
      end else begin
        if not FadeOut then begin
          Finish;
          FadeOut := true;
          FadeTo(@ScreenPartyScore);
        end;
      end;
    end;
  end;

  // draw custom items
  SingModiDraw(PlayerInfo);  // always draw

  //GoldenNoteStarsTwinkle Mod
    GoldenRec.SpawnRec;
  //GoldenNoteStarsTwinkle Mod

  //Update PlayerInfo
  for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    if PlayerInfo.Playerinfo[I].Enabled then
    begin
      PlayerInfo.Playerinfo[I].Bar :=  Player[I].ScorePercent;
      PlayerInfo.Playerinfo[I].Score := Player[I].ScoreTotalI;
    end;
  end;

  if ((ShowFinish) AND (NOT Paused)) then
  begin
    if not DLLMan.PluginDraw(Playerinfo, Czesci[0].Akt) then
    begin
      if not FadeOut then begin
          Finish;
          FadeOut := true;
          FadeTo(@ScreenPartyScore);
      end;
    end;   
  end;

  //Change PlayerInfo/Changeables
  for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    if (Player[I].ScoreTotalI <> PlayerInfo.Playerinfo[I].Score) then
    begin
      //Player[I].ScoreTotal   := Player[I].ScoreTotal + (PlayerInfo.Playerinfo[I].Score - Player[I].ScoreTotalI);
      Player[I].ScoreTotalI := PlayerInfo.Playerinfo[I].Score;
    end;
    if (PlayerInfo.Playerinfo[I].Bar <> Player[I].ScorePercent) then
      Player[I].ScorePercentTarget := PlayerInfo.Playerinfo[I].Bar;
  end;

  // back stereo
  Static[StaticP1].Texture.X := Static[StaticP1].Texture.X - 10*ScreenX;
  Static[StaticP1ScoreBG].Texture.X := Static[StaticP1ScoreBG].Texture.X - 10*ScreenX;

  Text[TextP1].X := Text[TextP1].X - 10*ScreenX;
  Text[TextP1Score].X := Text[TextP1Score].X - 10*ScreenX;


  Static[StaticP2R].Texture.X := Static[StaticP2R].Texture.X - 10*ScreenX;
  Static[StaticP2RScoreBG].Texture.X := Static[StaticP2RScoreBG].Texture.X - 10*ScreenX;

  Text[TextP2R].X := Text[TextP2R].X - 10*ScreenX;
  Text[TextP2RScore].X := Text[TextP2RScore].X - 10*ScreenX;


  for S := 1 to 1 do
    Static[S].Texture.X := Static[S].Texture.X - 10*ScreenX;

  for T := 0 to 1 do
    Text[T].X := Text[T].X - 10*ScreenX;


end;

procedure TScreenSingModi.Finish;
begin
inherited Finish;

Winner := DllMan.PluginFinish(PlayerInfo);

//Log.LogError('Winner: ' + InttoStr(Winner));

//DLLMan.UnLoadPlugin;
end;

function LoadTex (const Name, Typ: PChar): TsmallTexture; stdcall;
var
  Texname, EXT: String;
  Tex: TTexture;
begin
  //Get texture Name
  TexName := Skin.GetTextureFileName(String(Name));
  //Get File Typ
  Ext := ExtractFileExt(TexName);
  if (uppercase(Ext) = '.JPG') then
    Ext := 'JPG'
  else
    Ext := 'BMP';

  Tex := Texture.LoadTexture(PChar(TexName),  PChar(Ext), Typ, 0);

  Result.TexNum := Tex.TexNum;
  Result.W := Tex.W;
  Result.H := Tex.H;
  Result.ScaleW := Tex.ScaleW;
  Result.ScaleH := Tex.ScaleH;
end;
{
function Translate (const Name: PChar): PChar; stdcall;
begin
  Result := PChar(Language.Translate(String(Name)));
end; }

procedure Print (const Style, Size: Byte; const X, Y: Real; const Text: PChar); stdcall;       //Procedure to Print Text
begin
  SetFontItalic ((Style and 128) = 128);
  SetFontStyle(Style and 7);
  SetFontSize(Size);
  SetFontPos (X, Y);
  glPrint (PChar(Language.Translate(String(Text))));
end;

function LoadSound  (const Name: PChar): Cardinal; stdcall;       //Procedure that loads a Custom Sound
begin
 Result := Music.LoadCustomSound(String(Name));
end;

procedure PlaySound (const Index: Cardinal); stdcall;       //Plays a Custom Sound
begin
  Music.PlayCustomSound(Index);
end;

end.
