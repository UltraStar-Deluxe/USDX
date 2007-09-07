unit UScreenSing;

interface

uses UMenu, UMusic, SDL, SysUtils, UFiles, UTime, USongs, UIni, ULog, USmpeg, UTexture, ULyrics,
  TextGL, OpenGL12, BASS, UThemes, ULCD, UGraphicClasses, UVideo;

type
  TScreenSing = class(TMenu)
    protected
      paused: boolean; //Pause Mod
      PauseTime: Real;
      NumEmptySentences: integer;
    public
      //TextTime:           integer;

      //TimeBar mod
       StaticTimeProgress:  integer;
       TextTimeText:        integer;
      //eoa TimeBar mod

      StaticP1:           integer;
      StaticP1ScoreBG:    integer;
      TextP1:             integer;
      TextP1Score:        integer;

      //moveable singbar mod
      StaticP1SingBar:         integer;
      StaticP1ThreePSingBar:   integer;
      StaticP1TwoPSingBar:     integer;
      StaticP2RSingBar:        integer;
      StaticP2MSingBar:        integer;
      StaticP3SingBar:         integer;
      //eoa moveable singbar

      //Added for ps3 skin
      //shown when game is in 2/4 player modus
      StaticP1TwoP:           integer;
      StaticP1TwoPScoreBG:    integer;
      TextP1TwoP:             integer;
      TextP1TwoPScore:        integer;
      //shown when game is in 3/6 player modus
      StaticP1ThreeP:           integer;
      StaticP1ThreePScoreBG:    integer;
      TextP1ThreeP:             integer;
      TextP1ThreePScore:        integer;
      //eoa

      StaticP2R:          integer;
      StaticP2RScoreBG:   integer;
      TextP2R:            integer;
      TextP2RScore:       integer;

      StaticP2M:          integer;
      StaticP2MScoreBG:   integer;
      TextP2M:            integer;
      TextP2MScore:       integer;

      StaticP3R:          integer;
      StaticP3RScoreBG:   integer;
      TextP3R:            integer;
      TextP3RScore:       integer;

      Tex_Background:     TTexture;
      FadeOut:            boolean;
      LyricMain:          TLyric;
      LyricSub:           TLyric;

      constructor Create; override;
      procedure onShow; override;
      procedure onShowFinish; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function Draw: boolean; override;
      procedure Finish; virtual;
      procedure UpdateLCD;
      procedure Pause; //Pause Mod(Toggles Pause)

      //OnSentenceEnd for LineBonus + Singbar
      procedure onSentenceEnd(S: Cardinal);
      //OnSentenceChange (for Golden Notes)
      procedure onSentenceChange(S: Cardinal);
  end;

implementation
uses UGraphic, UDraw, UMain, Classes, URecord, ULanguage, math;

// Method for input parsing. If False is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenSing.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_Q:
        begin
          //When not ask before Exit then Finish now
          if (Ini.AskbeforeDel <> 1) then
            Finish
          //else just Pause and let the Popup make the Work  
          else if not paused then
            Pause;
          
          Result := false;
        end;

      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          //Record Sound Hack:
          //Sound[0].BufferLong

          Finish;
          Music.PlayBack;
          FadeTo(@ScreenScore);
        end;

      SDLK_P://Pause Mod
        begin
          Pause;
        end;
        
      SDLK_RETURN:
        begin
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN :
        begin
        end;
      SDLK_UP :
        begin
        end;
    end;
  end;
end;

//Pause Mod
procedure TScreenSing.Pause;
begin
  if not paused then  //Pause einschalten
  begin
    PauseTime := Czas.Teraz;
    Paused := true;
    //stop Music
    Music.Pause;
    if (AktSong.Video <> '') and FileExists(AktSong.Path + AktSong.Video) then //Video
      FFmpegTogglePause;//PauseSmpeg; //Video
  end
  else            //Pause ausschalten
  begin
    Czas.Teraz := PauseTime; //Position of Notes
    Music.MoveTo (PauseTime);//Position of Music
    Music.Play; //Play Music
    if (AktSong.Video <> '') and FileExists(AktSong.Path + AktSong.Video) then //Video
      FFmpegTogglePause;//PlaySmpeg;
      //SkipSmpeg(PauseTime);
    Paused := false;
  end;
end;
//Pause Mod End

constructor TScreenSing.Create;
var
  I:    integer;
  P:    integer;
begin
  inherited Create;

  LoadFromTheme(Theme.Sing);

  // time
  //TextTime := AddText(75, 14, 1, 8, 0.25, 0.25, 0.25, '00:00');

  //TimeBar mod
    StaticTimeProgress :=  AddStatic(Theme.Sing.StaticTimeProgress);
    TextTimeText       :=  AddText(Theme.Sing.TextTimeText);
  //eoa TimeBar mod

  StaticP1 := AddStatic(Theme.Sing.StaticP1);
  StaticP1ScoreBG := AddStatic(Theme.Sing.StaticP1ScoreBG);
  TextP1 := AddText(Theme.Sing.TextP1);
  TextP1Score := AddText(Theme.Sing.TextP1Score);

  //moveable singbar mod
  StaticP1SingBar         := AddStatic(Theme.Sing.StaticP1SingBar);
  StaticP1ThreePSingBar   := AddStatic(Theme.Sing.StaticP1ThreePSingBar);
  StaticP1TwoPSingBar     := AddStatic(Theme.Sing.StaticP2RSingBar);
  StaticP2RSingBar        := AddStatic(Theme.Sing.StaticP2RSingBar);
  StaticP2MSingBar        := AddStatic(Theme.Sing.StaticP2MSingBar);
  StaticP3SingBar         := AddStatic(Theme.Sing.StaticP3SingBar);
  //eoa moveable singbar

  //Added for ps3 skin
  //This one is shown in 2/4P mode
  StaticP1TwoP := AddStatic(Theme.Sing.StaticP1TwoP);
  StaticP1TwoPScoreBG := AddStatic(Theme.Sing.StaticP1TwoPScoreBG);
  TextP1TwoP := AddText(Theme.Sing.TextP1TwoP);
  TextP1TwoPScore := AddText(Theme.Sing.TextP1TwoPScore);

  //This one is shown in 3/6P mode
  StaticP1ThreeP := AddStatic(Theme.Sing.StaticP1ThreeP);
  StaticP1ThreePScoreBG := AddStatic(Theme.Sing.StaticP1ThreePScoreBG);
  TextP1ThreeP := AddText(Theme.Sing.TextP1ThreeP);
  TextP1ThreePScore := AddText(Theme.Sing.TextP1ThreePScore);
  //eoa

  StaticP2R := AddStatic(Theme.Sing.StaticP2R);
  StaticP2RScoreBG := AddStatic(Theme.Sing.StaticP2RScoreBG);
  TextP2R := AddText(Theme.Sing.TextP2R);
  TextP2RScore := AddText(Theme.Sing.TextP2RScore);

  StaticP2M := AddStatic(Theme.Sing.StaticP2M);
  StaticP2MScoreBG := AddStatic(Theme.Sing.StaticP2MScoreBG);
  TextP2M := AddText(Theme.Sing.TextP2M);
  TextP2MScore := AddText(Theme.Sing.TextP2MScore);

  StaticP3R := AddStatic(Theme.Sing.StaticP3R);
  StaticP3RScoreBG := AddStatic(Theme.Sing.StaticP3RScoreBG);
  TextP3R := AddText(Theme.Sing.TextP3R);
  TextP3RScore := AddText(Theme.Sing.TextP3RScore);

  LyricMain := TLyric.Create;
  LyricSub :=  TLyric.Create;
  UVideo.Init;
end;

procedure TScreenSing.onShow;
var
  P:        integer;
  V1:       boolean;
  V1TwoP:   boolean; //added for ps3 skin
  V1ThreeP: boolean; //added for ps3 skin
  V2R:      boolean;
  V2M:      boolean;
  V3R:      boolean;
  NR:       TRecR; //Line Bonus Mod
begin
  Log.LogStatus('Begin', 'onShow');
  FadeOut := false; // 0.5.0: early 0.5.0 problems were by this line commented

  // prepare players
  SetLength(Player, PlayersPlay);
//  Player[0].ScoreTotalI := 0;


  case PlayersPlay of
    1:  begin
          V1       := true;
          V1TwoP   := false;  //added for ps3 skin
          V1ThreeP := false;  //added for ps3 skin
          V2R      := false;
          V2M      := false;
          V3R      := false;
        end;
    2:  begin
          V1       := false;
          V1TwoP   := true;  //added for ps3 skin
          V1ThreeP := false; //added for ps3 skin
          V2R      := true;
          V2M      := false;
          V3R      := false;
        end;
    3:  begin
          V1       := false;
          V1TwoP   := false; //added for ps3 skin
          V1ThreeP := true;  //added for ps3 skin
          V2R      := false;
          V2M      := true;
          V3R      := true;
        end;
    4:  begin // double screen
          V1       := false;
          V1TwoP   := true;  //added for ps3 skin
          V1ThreeP := false; //added for ps3 skin
          V2R      := true;
          V2M      := false;
          V3R      := false;
        end;
    6:  begin // double screen
          V1       := false;
          V1TwoP   := false; //added for ps3 skin
          V1ThreeP := true;  //added for ps3 skin
          V2R      := false;
          V2M      := true;
          V3R      := true;
        end;

  end;

  //Added for ps3 skin
  //This one is shown in 1P mode
  Static[StaticP1].Visible := V1;
  Static[StaticP1ScoreBG].Visible := V1;
  Text[TextP1].Visible := V1;
  Text[TextP1Score].Visible := V1;
  //This one is shown in 2/4P mode
  Static[StaticP1TwoP].Visible := V1TwoP;
  Static[StaticP1TwoPScoreBG].Visible := V1TwoP;
  Text[TextP1TwoP].Visible := V1TwoP;
  Text[TextP1TwoPScore].Visible := V1TwoP;
  //This one is shown in 3/6P mode
  Static[StaticP1ThreeP].Visible := V1ThreeP;
  Static[StaticP1ThreePScoreBG].Visible := V1ThreeP;
  Text[TextP1ThreeP].Visible := V1ThreeP;
  Text[TextP1ThreePScore].Visible := V1ThreeP;
  //eoa

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
  ResetSingTemp;
//  Log.LogWarning(CatSongs.Song[CatSongs.Selected].Path + CatSongs.Song[CatSongs.Selected].FileName, '!!!');
  AktSong := CatSongs.Song[CatSongs.Selected];
  try
    if not LoadSong(CatSongs.Song[CatSongs.Selected].Path + CatSongs.Song[CatSongs.Selected].FileName) then
    begin
      //Error Loading Song -> Go back to Song Screen and Show some Error Message
      FadeTo(@ScreenSong);
      //Select New Song in Party Mode
      if ScreenSong.Mode = 1 then
        ScreenSong.SelectRandomSong;
      ScreenPopupError.ShowPopup (Language.Translate('ERROR_CORRUPT_SONG'));
      Exit;
    end;
  except
    //Error Loading Song -> Go back to Song Screen and Show some Error Message
    FadeTo(@ScreenSong);
    //Select New Song in Party Mode
      if ScreenSong.Mode = 1 then
        ScreenSong.SelectRandomSong;
    ScreenPopupError.ShowPopup (Language.Translate('ERROR_CORRUPT_SONG'));
    Exit;
  end;
  AktSong.Path := CatSongs.Song[CatSongs.Selected].Path;
//  AktSong.GAP := AktSong.GAP + 40 {4096 = 100ms for buffer} + 20 {microphone} + 60000 / AktSong.BPM[0].BPM / 2; // temporary until UMain will be fixed

  // set movie
  if (AktSong.Video <> '') and FileExists(AktSong.Path + AktSong.Video) then begin
{    OpenSmpeg(AktSong.Path + AktSong.Video);
    SkipSmpeg(AktSong.VideoGAP + AktSong.Start);}
    // todo: VideoGap and Start time verwursten
    FFmpegOpenFile(pAnsiChar(AktSong.Path + AktSong.Video));
    FFmpegSkip(AktSong.VideoGAP + AktSong.Start);
    AktSong.VideoLoaded := true;
  end;

  // set background
  if (AktSong.Background <> '')  and (AktSong.VideoLoaded = false) then
    try
      Tex_Background := Texture.LoadTexture(AktSong.Path + AktSong.Background);
    except
      log.LogError('Background could not be loaded: ' + AktSong.Path + AktSong.Background);
      Tex_Background.TexNum := -1;
    end
  else
    Tex_Background.TexNum := -1;



  // play music (I)
  Music.CaptureStart;
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
  LyricSub.Y := Skin_LyricsT + 35; //42 //40
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
        LyricMain.ColSB := Skin_FontHighlightB;1aa5dc}
        LyricMain.ColSR := 5/255; //26
        LyricMain.ColSG := 163/255; //165
        LyricMain.ColSB := 210/255;  //220

        LyricSub.ColR := 0.4; //0.6
        LyricSub.ColG := 0.4; //0.6
        LyricSub.ColB := 0.4; //0.6
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

  //Kill all Stars not Killed yet
  //GoldenStarsTwinkle Mod
    GoldenRec.SentenceChange;
  //GoldenStarsTwinkle Mod End

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
  Text[TextP3RScore].Visible := V3R;}

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
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1TwoPScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1TwoPScore.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1TwoPScoreBG.X;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1TwoPScore.Y + 65;

      //P2
      Player[1].LineBonus_TargetX := Theme.Sing.StaticP2RScoreBG.X;
      Player[1].LineBonus_TargetY := Theme.Sing.TextP2RScore.Y;
      Player[1].LineBonus_StartX  := Theme.Sing.StaticP2RScoreBG.X;
      Player[1].LineBonus_StartY  := Theme.Sing.TextP2RScore.Y + 65;
    end;

    3: begin
      //P1
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1ThreePScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1ThreePScore.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1ThreePScoreBG.x;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1ThreePScore.Y + 65;

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
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1TwoPScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1TwoPScore.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1TwoPScoreBG.x;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1TwoPScore.Y + 65;

      //P2
      Player[1].LineBonus_TargetX := Theme.Sing.StaticP2RScoreBG.x;
      Player[1].LineBonus_TargetY := Theme.Sing.TextP2RScore.Y;
      Player[1].LineBonus_StartX  := Theme.Sing.StaticP2RScoreBG.x;
      Player[1].LineBonus_StartY  := Theme.Sing.TextP2RScore.Y + 65;

      //P3
      Player[2].LineBonus_TargetX := Theme.Sing.StaticP1TwoPScoreBG.x;
      Player[2].LineBonus_TargetY := Theme.Sing.TextP1TwoPScore.Y;
      Player[2].LineBonus_StartX  := Theme.Sing.StaticP1TwoPScoreBG.x;
      Player[2].LineBonus_StartY  := Theme.Sing.TextP1TwoPScore.Y + 65;

      //P4
      Player[3].LineBonus_TargetX := Theme.Sing.StaticP2RScoreBG.x;
      Player[3].LineBonus_TargetY := Theme.Sing.TextP2RScore.Y;
      Player[3].LineBonus_StartX  := Theme.Sing.StaticP2RScoreBG.x;
      Player[3].LineBonus_StartY  := Theme.Sing.TextP2RScore.Y + 65;
    end;

    6: begin
      //P1
      Player[0].LineBonus_TargetX := Theme.Sing.StaticP1ThreePScoreBG.x;
      Player[0].LineBonus_TargetY := Theme.Sing.TextP1ThreePScore.Y;
      Player[0].LineBonus_StartX  := Theme.Sing.StaticP1ThreePScoreBG.x;
      Player[0].LineBonus_StartY  := Theme.Sing.TextP1ThreePScore.Y + 65;

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
      Player[3].LineBonus_TargetX := Theme.Sing.StaticP1ThreePScoreBG.x;
      Player[3].LineBonus_TargetY := Theme.Sing.TextP1ThreePScore.Y;
      Player[3].LineBonus_StartX  := Theme.Sing.StaticP1ThreePScoreBG.x;
      Player[3].LineBonus_StartY  := Theme.Sing.TextP1ThreePScore.Y + 65;

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
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_TargetY := Skin_P2_NotesB - 105 - 65;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_StartY  := Skin_P2_NotesB - 105;
    end;

    2: begin
      //P1
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_TargetY := Skin_P1_NotesB - 105 - 65 + 28;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_StartY  := Skin_P1_NotesB - 105 + 28;

      //P2
      Player[1].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_TargetY := Skin_P2_NotesB - 105 - 65 + 28;
      Player[1].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_StartY  := Skin_P2_NotesB - 105 + 28;
    end;

    3: begin
      //P1
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_TargetY := 120 - 65 + 28;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_StartY  := 120 + 28;

      //P2
      Player[1].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_TargetY := 245 - 65 + 28;
      Player[1].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_StartY  := 245 + 28;

      //P3
      Player[2].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[2].LineBonus_TargetY := 370 - 65 + 28;
      Player[2].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[2].LineBonus_StartY  := 370 + 28;
    end;

    4: begin
      //P1
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_TargetY := Skin_P1_NotesB - 105 - 65 + 28;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_StartY  := Skin_P1_NotesB - 105 + 28;

      //P2
      Player[1].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_TargetY := Skin_P2_NotesB - 105 - 65 + 28;
      Player[1].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_StartY  := Skin_P2_NotesB - 105 + 28;

      //P3
      Player[2].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[2].LineBonus_TargetY := Skin_P1_NotesB - 105 - 65 + 28;
      Player[2].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[2].LineBonus_StartY  := Skin_P1_NotesB - 105 + 28;

      //P4
      Player[3].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[3].LineBonus_TargetY := Skin_P2_NotesB - 105 - 65 + 28;
      Player[3].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[3].LineBonus_StartY  := Skin_P2_NotesB - 105 + 28;
    end;

    6: begin
      //P1
      Player[0].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_TargetY := 120 - 65 + 28;
      Player[0].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[0].LineBonus_StartY  := 120 + 28;

      //P2
      Player[1].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_TargetY := 245 - 65 + 28;
      Player[1].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[1].LineBonus_StartY  := 245 + 28;

      //P3
      Player[2].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[2].LineBonus_TargetY := 370 - 65 + 28;
      Player[2].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[2].LineBonus_StartY  := 370 + 28;

      //P4
      Player[3].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[3].LineBonus_TargetY := 120 - 65 + 28;
      Player[3].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[3].LineBonus_StartY  := 120 + 28;

      //P5
      Player[4].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[4].LineBonus_TargetY := 245 - 65 + 28;
      Player[4].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[4].LineBonus_StartY  := 245 + 28;

      //P6
      Player[5].LineBonus_TargetX := Round(Nr.Right + 10*ScreenX - 100);
      Player[5].LineBonus_TargetY := 370 - 65 + 28;
      Player[5].LineBonus_StartX  := Round(Nr.Right + 10*ScreenX - 100);
      Player[5].LineBonus_StartY  := 370 + 28;
    end;
  end;
  end;
  //Set Position of Line Bonus - PhrasenBonus End
  //Set Num of Empty Sentences for Phrasen Bonus
  NumEmptySentences := 0;
  for P := low(Czesci[0].Czesc) to high(Czesci[0].Czesc) do
    if Czesci[0].Czesc[P].TotalNotes = 0 then Inc(NumEmptySentences);

  Log.LogStatus('End', 'onShow');
end;

procedure TScreenSing.onShowFinish;
begin
  // play movie (II)

  if AktSong.VideoLoaded then
  begin
    try
      FFmpegGetFrame(Czas.Teraz);
      FFmpegDrawGL(ScreenAct);
//      PlaySmpeg;
    except
      //If an Error occurs Reading Video: prevent Video from being Drawn again and Close Video
      AktSong.VideoLoaded := False;
      Log.LogError('Error drawing Video, Video has been disabled for this Song/Session.');
      Log.LogError('Corrupted File: ' + AktSong.Video);
      try
//        CloseSmpeg;
        FFmpegClose;
      except

      end;
    end;
  end;

  // play music (II)
  Music.Play;

  // prepare timer (II)
  CountSkipTimeSet;
end;

function TScreenSing.Draw: boolean;
var
  Min:    integer;
  Sec:    integer;
  Tekst:  string;
  Flash:  real;
  S:      integer;
  T:      integer;
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

//Original 0.5.0
{  // set player colors
  if PlayersPlay = 4 then begin
    if ScreenAct = 1 then begin
      //LoadColor(Static[StaticP1].Texture.ColR, Static[StaticP1].Texture.ColG,
        //Static[StaticP1].Texture.ColB, 'P1Dark');
//      LoadColor(Static[StaticP2R].Texture.ColR, Static[StaticP2R].Texture.ColG,
  //      Static[StaticP2R].Texture.ColB, 'P2Dark');
    end;
    if ScreenAct = 2 then begin
      LoadColor(Static[StaticP1].Texture.ColR, Static[StaticP1].Texture.ColG,
        Static[StaticP1].Texture.ColB, 'P3Dark');
      LoadColor(Static[StaticP2R].Texture.ColR, Static[StaticP2R].Texture.ColG,
        Static[StaticP2R].Texture.ColB, 'P4Dark');
    end;
  end;

  if PlayersPlay = 6 then begin
    if ScreenAct = 1 then begin
      LoadColor(Static[StaticP1].Texture.ColR, Static[StaticP1].Texture.ColG,
        Static[StaticP1].Texture.ColB, 'P1Dark');
      LoadColor(Static[StaticP2M].Texture.ColR, Static[StaticP2M].Texture.ColG,
        Static[StaticP2R].Texture.ColB, 'P2Dark');
      LoadColor(Static[StaticP3R].Texture.ColR, Static[StaticP3R].Texture.ColG,
        Static[StaticP3R].Texture.ColB, 'P3Dark');
    end;
    if ScreenAct = 2 then begin
      LoadColor(Static[StaticP1].Texture.ColR, Static[StaticP1].Texture.ColG,
        Static[StaticP1].Texture.ColB, 'P4Dark');
      LoadColor(Static[StaticP2M].Texture.ColR, Static[StaticP2M].Texture.ColG,
        Static[StaticP2R].Texture.ColB, 'P5Dark');
      LoadColor(Static[StaticP3R].Texture.ColR, Static[StaticP3R].Texture.ColG,
        Static[StaticP3R].Texture.ColB, 'P6Dark');
    end;
  end;  }
  //end ScoreBG Mod



  // set player names (for 2 screens and only Singstar skin)
  if ScreenAct = 1 then begin
    Text[TextP1].Text := 'P1';
    Text[TextP1TwoP].Text := 'P1'; //added for ps3 skin
    Text[TextP1ThreeP].Text := 'P1'; //added for ps3 skin
    Text[TextP2R].Text := 'P2';
    Text[TextP2M].Text := 'P2';
    Text[TextP3R].Text := 'P3';
  end;

  if ScreenAct = 2 then begin
    case PlayersPlay of
{        1:  begin
              Text[TextP1].Text := 'P2';
            end;
        2:  begin
              Text[TextP1].Text := 'P3';
              Text[TextP2R].Text := 'P4';
            end;
        3:  begin
              Text[TextP1].Text := 'P4';
              Text[TextP2M].Text := 'P5';
              Text[TextP3R].Text := 'P6';
            end;}

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

  // stereo

// weird stuff, maybe this is for "dual screen?", but where is player three then?
  Static[StaticP1].Texture.X := Static[StaticP1].Texture.X + 10*ScreenX;
  Static[StaticP1ScoreBG].Texture.X := Static[StaticP1ScoreBG].Texture.X + 10*ScreenX;

  Text[TextP1].X := Text[TextP1].X + 10*ScreenX;
  Text[TextP1Score].X := Text[TextP1Score].X + 10*ScreenX;


  Static[StaticP2R].Texture.X := Static[StaticP2R].Texture.X + 10*ScreenX;
  Static[StaticP2RScoreBG].Texture.X := Static[StaticP2RScoreBG].Texture.X + 10*ScreenX;

  Text[TextP2R].X := Text[TextP2R].X + 10*ScreenX;
  Text[TextP2RScore].X := Text[TextP2RScore].X + 10*ScreenX;
// end of weird stuff

  for S := 1 to 1 do
    Static[S].Texture.X := Static[S].Texture.X + 10*ScreenX;

  for T := 0 to 1 do
    Text[T].X := Text[T].X + 10*ScreenX;

  // update static menu with time ...
  Min := Round(Czas.Teraz) div 60;
  Sec := Round(Czas.Teraz) mod 60;
  Text[TextTimeText].Text := '';
  if Min < 10 then Text[TextTimeText].Text := '0';
  Text[TextTimeText].Text := Text[TextTimeText].Text + IntToStr(Min) + ':';
  if Sec < 10 then Text[TextTimeText].Text := Text[TextTimeText].Text + '0';
  Text[TextTimeText].Text := Text[TextTimeText].Text + IntToStr(Sec);

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


  // beat flash
{  Flash := 1 - (Czas.MidBeat - Czas.AktBeat);
  if (Czas.AktBeat + AktSong.NotesGAP) mod AktSong.Resolution = 0 then Flash := 1
  else Flash := 0;
  if Czas.AktBeat < 0 then Flash := 0;
  glClearColor(Flash, Flash, Flash, 1);}

  // beat sound
//  if (Ini.BeatClick = 1) and (Flash = 1) and (Czas.AktBeat <> Czas.OldBeat) then Music.PlayClick;

  // draw static menu (BG)
  DrawBG;
  //Draw Background
  SingDrawBackground;
  // update and draw movie
  if ShowFinish and AktSong.VideoLoaded then begin
    try
//      UpdateSmpeg; // this only draws
      // todo: find a way to determine, when a new frame is needed
      // toto: same for the need to skip frames
      FFmpegGetFrame(Czas.Teraz);
      FFmpegDrawGL(ScreenAct);
    except
      //If an Error occurs drawing: prevent Video from being Drawn again and Close Video
      AktSong.VideoLoaded := False;
      log.LogError('Error drawing Video, Video has been disabled for this Song/Session.');
      Log.LogError('Corrupted File: ' + AktSong.Video);
      try
//        CloseSmpeg;
        FFmpegClose;
      except

      end;
    end;
  end;

  // draw static menu (FG)
  DrawFG;

  // check for music finish
//  Log.LogError('Check for music finish: ' + BoolToStr(Music.Finished) + ' ' + FloatToStr(Czas.Teraz*1000) + ' ' + IntToStr(AktSong.Finish));
  if ShowFinish then begin
  if (not Music.Finished) and ((AktSong.Finish = 0) or (Czas.Teraz*1000 <= AktSong.Finish)) then begin
  //Pause Mod:
    if not Paused then
    Sing(Self);       // analyze song
  end else begin
//    Log.LogError('End');
    if not FadeOut then begin
//      Log.LogError('End2');
      Finish;
      FadeOut := true;
      FadeTo(@ScreenScore);
    end;
  end;
  end;

  // draw custom items
  SingDraw;  // always draw

//GoldenNoteStarsTwinkle Mod
  GoldenRec.SpawnRec;
//GoldenNoteStarsTwinkle Mod

  // back stereo

// weird stuff, maybe this is for "dual screen?", but where is player three then?
  Static[StaticP1].Texture.X := Static[StaticP1].Texture.X - 10*ScreenX;
  Static[StaticP1ScoreBG].Texture.X := Static[StaticP1ScoreBG].Texture.X - 10*ScreenX;

  Text[TextP1].X := Text[TextP1].X - 10*ScreenX;
  Text[TextP1Score].X := Text[TextP1Score].X - 10*ScreenX;


  Static[StaticP2R].Texture.X := Static[StaticP2R].Texture.X - 10*ScreenX;
  Static[StaticP2RScoreBG].Texture.X := Static[StaticP2RScoreBG].Texture.X - 10*ScreenX;

  Text[TextP2R].X := Text[TextP2R].X - 10*ScreenX;
  Text[TextP2RScore].X := Text[TextP2RScore].X - 10*ScreenX;
//weird end

  for S := 1 to 1 do
    Static[S].Texture.X := Static[S].Texture.X - 10*ScreenX;

  for T := 0 to 1 do
    Text[T].X := Text[T].X - 10*ScreenX;

end;

procedure TScreenSing.Finish;
begin
  Music.CaptureStop;
  Music.Stop;

  if Ini.SavePlayback = 1 then begin
    Log.BenchmarkStart(0);
    Log.LogVoice(0);
    Log.LogVoice(1);
    Log.LogVoice(2);
    Log.BenchmarkEnd(0);
    Log.LogBenchmark('Creating files', 0);
  end;

  if AktSong.VideoLoaded then begin
//    CloseSmpeg;
    FFmpegClose;
    AktSong.VideoLoaded := false; // to prevent drawing closed video
  end;

  SetFontItalic (False);
end;

procedure TScreenSing.UpdateLCD;
var
  T:    string;
begin
  LCD.HideCursor;
  LCD.Clear;

  T := LyricMain.Text;
  if Copy(T, Length(T), 1) <> ' ' then T := T + ' ';
  LCD.AddTextBR(T);

  T := LyricSub.Text;
  if Copy(T, Length(T), 1) <> ' ' then T := T + ' ';
  LCD.AddTextBR(T);
end;

procedure TScreenSing.onSentenceEnd(S: Cardinal);
var
I: Integer;
A: Real;
B: integer; //Max Points for Notes
begin

  //Check for Empty Sentence
  if (Czesci[0].Czesc[S].TotalNotes<=0) then
    exit;

  //Set Max Note Points
  if (Ini.LineBonus > 0) then
    B :=  9000
  else
    B := 10000;

  for I := 0 to High(Player) do begin
    A := Player[I].Score + Player[I].ScoreGolden - Player[I].ScoreLast + 2;

    //SingBar Mod
    If ({(Ini.Oscilloscope = 2) and }(Czesci[0].Czesc[S].TotalNotes>0)) then
    begin
      Player[I].ScorePercentTarget := Player[I].ScorePercentTarget + floor(A / (B * Czesci[0].Czesc[S].TotalNotes / Czesci[0].Wartosc) * 40 - 26);
      if Player[I].ScorePercentTarget < 0 then Player[I].ScorePercentTarget := 0;
      if Player[I].ScorePercentTarget > 99 then Player[I].ScorePercentTarget := 99;

    //end Singbar Mod
    end;

    //PhrasenBonus - Line Bonus Mod

    //Generate Steps 0 to 8
    A := Floor(A / (B * Czesci[0].Czesc[S].TotalNotes / Czesci[0].Wartosc) * 8);

    If (Ini.LineBonus > 0) then
    begin

      //Generate Text
      if A >= 8 then
        Player[I].LineBonus_Text := Theme.Sing.LineBonusText[8]
      else
        Player[I].LineBonus_Text := Theme.Sing.LineBonusText[Floor(A)];

      //PhrasenBonus give Points
      Player[I].ScoreLine := Player[I].ScoreLine + (1000 / (Length(Czesci[0].Czesc) - NumEmptySentences) * A / 8);
      Player[I].ScoreLineI := Round(Player[I].ScoreLine / 10) * 10;
      //Update Total Score
      Player[I].ScoreTotalI := Player[I].ScoreI + Player[I].ScoreGoldenI + Player[I].ScoreLineI;

      //Color
      Case Floor(A) of
        0: begin
          Player[I].LineBonus_Color.R := 1;
          Player[I].LineBonus_Color.G := 0;
          Player[I].LineBonus_Color.B := 0;
        end;
        1..3: begin
          Player[I].LineBonus_Color.R := 1;
          Player[I].LineBonus_Color.G := (A * 0.25);
          Player[I].LineBonus_Color.B := 0;
        end;
        4: begin
          Player[I].LineBonus_Color.R := 1;
          Player[I].LineBonus_Color.G := 1;
          Player[I].LineBonus_Color.B := 0;
        end;
        5..7: begin
          Player[I].LineBonus_Color.R := 1-((a-4)*0.25);
          Player[I].LineBonus_Color.G := 1;
          Player[I].LineBonus_Color.B := 0;
        end;
        8: begin
          Player[I].LineBonus_Color.R := 0;
          Player[I].LineBonus_Color.G := 1;
          Player[I].LineBonus_Color.B := 0;
        end;
      End; //Case
      //Player[I].LineBonus_Color.B := 0;
      //Player[I].LineBonus_Color.R := (8-A)/8;
      //Player[I].LineBonus_Color.G := A/10;

      Player[I].LineBonus_PosX  := Player[I].LineBonus_StartX;
      Player[I].LineBonus_PosY  := Player[I].LineBonus_StartY;
      Player[I].LineBonus_Alpha := 0.92;
      Player[I].LineBonus_Visible := True;
      Player[I].LineBonus_Age := 1;
    end;
    //PhrasenBonus - Line Bonus Mod End// }

    //PerfectLineTwinkle Mod (effect) Pt.1
    If (Ini.EffectSing=1) then
    begin
      if A >= 8 then Player[I].LastSentencePerfect := True
      else Player[I].LastSentencePerfect := False;
    end;
    //PerfectLineTwinkle Mod end

  //Refresh LastScore
  Player[I].ScoreLast := Player[I].Score + Player[I].ScoreGolden;

  end;

  //PerfectLineTwinkle Mod (effect) Pt.2
  if Ini.EffectSing=1 then
    GoldenRec.SpawnPerfectLineTwinkle;
  //PerfectLineTwinkle Mod end
end;

//Called on Sentence Change S= New Current Sentence
procedure TScreenSing.onSentenceChange(S: Cardinal);
begin
  //GoldenStarsTwinkle Mod
  GoldenRec.SentenceChange;
  //GoldenStarsTwinkle Mod End
end;

end.
