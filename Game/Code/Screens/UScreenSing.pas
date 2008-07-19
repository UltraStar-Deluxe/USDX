unit UScreenSing;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}


uses UMenu,
     UMusic,
     SDL,
     SysUtils,
     UFiles,
     UTime,
     USongs,
     UIni,
     ULog,
     UTexture,
     ULyrics,
     TextGL,
     gl,
     UThemes,
     UGraphicClasses,
     USingScores;

type
  TLyricsSyncSource = class(TSyncSource)
    function GetClock(): real; override;
  end;

type
  TScreenSing = class(TMenu)
    protected
      Paused: boolean; //Pause Mod
      LyricsSync: TLyricsSyncSource;
      NumEmptySentences: integer;
    public
      //TextTime:           integer;

      // TimeBar fields
      StaticTimeProgress:  integer;
      TextTimeText:        integer;

      StaticP1:           integer;
      TextP1:             integer;
      {StaticP1ScoreBG:    integer;
      TextP1Score:        integer;}

      {//moveable singbar mod
      StaticP1SingBar:         integer;
      StaticP1ThreePSingBar:   integer;
      StaticP1TwoPSingBar:     integer;
      StaticP2RSingBar:        integer;
      StaticP2MSingBar:        integer;
      StaticP3SingBar:         integer;
      //eoa moveable singbar }

      //Added for ps3 skin
      //shown when game is in 2/4 player modus
      StaticP1TwoP:           integer;
      TextP1TwoP:             integer;

      {StaticP1TwoPScoreBG:    integer;
      TextP1TwoPScore:        integer;}
      //shown when game is in 3/6 player modus
      StaticP1ThreeP:           integer;
      TextP1ThreeP:             integer;

      {TextP1ThreePScore:        integer;
      StaticP1ThreePScoreBG:    integer;  }
      //eoa

      StaticP2R:          integer;
      TextP2R:            integer;

      {StaticP2RScoreBG:   integer;
      TextP2RScore:       integer;}

      StaticP2M:          integer;
      TextP2M:            integer;

      {StaticP2MScoreBG:   integer;
      TextP2MScore:       integer; }

      StaticP3R:          integer;
      TextP3R:            integer;

      {StaticP3RScoreBG:   integer;
      TextP3RScore:       integer;}
      StaticPausePopup:   integer;

      Tex_Background:     TTexture;
      FadeOut:            boolean;
      //LyricMain:          TLyric;
      //LyricSub:           TLyric;
      Lyrics:             TLyricEngine;

      //Score Manager:
      Scores: TSingScores;

      fShowVisualization          : boolean;
      fCurrentVideoPlaybackEngine : IVideoPlayback;

      constructor Create; override;
      procedure   onShow; override;
      procedure   onShowFinish; override;

      function    ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      function    Draw: boolean; override;

      procedure   Finish; virtual;
      procedure   Pause; // Toggle Pause

      procedure   OnSentenceEnd(SentenceIndex: Cardinal);     // for LineBonus + Singbar
      procedure   OnSentenceChange(SentenceIndex: Cardinal);  // for Golden Notes
  end;

implementation

uses UGraphic,
     UDraw,
     UMain,
     USong,
     Classes,
     URecord,
     ULanguage,
     math;

// Method for input parsing. If False is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenSing.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    // check normal keys
    case WideCharUpperCase(CharCode)[1] of
      'Q':
        begin
          //When not ask before Exit then Finish now
          if (Ini.AskbeforeDel <> 1) then
            Finish
          //else just Pause and let the Popup make the Work
          else if not Paused then
            Pause;

          Result := false;
          Exit;
        end;
      'V': //Show Visualization
        begin
          fShowVisualization := not fShowVisualization;

          if fShowVisualization then
            fCurrentVideoPlaybackEngine := Visualization
          else
            fCurrentVideoPlaybackEngine := VideoPlayback;

          if fShowVisualization then
            fCurrentVideoPlaybackEngine.play;

          Exit;  
        end;
      'P':
        begin
          Pause;
          Exit;
        end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          //Record Sound Hack:
          //Sound[0].BufferLong

          Finish;
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenScore);
        end;

      SDLK_SPACE:
        begin
          Pause;
        end;

      SDLK_TAB: //Change Visualization Preset
        begin
          if fShowVisualization then
            fCurrentVideoPlaybackEngine.Position := now; // move to a random position
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
  if (not Paused) then  //enable Pause
    begin
      // pause Time
      Paused := true;

      LyricsState.Pause();

      // pause Music
      AudioPlayback.Pause;

      // pause Video
      if (CurrentSong.Video <> '') and FileExists(CurrentSong.Path + CurrentSong.Video) then
        fCurrentVideoPlaybackEngine.Pause;

    end
  else              //disable Pause
    begin
      LyricsState.Resume();

      // Play Music
      AudioPlayback.Play;

      // Video
      if (CurrentSong.Video <> '') and FileExists(CurrentSong.Path + CurrentSong.Video) then
        fCurrentVideoPlaybackEngine.Pause;

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

  fShowVisualization := false;
  fCurrentVideoPlaybackEngine := VideoPlayback;


  //Create Score Class
  Scores := TSingScores.Create;
  Scores.LoadfromTheme;

  LoadFromTheme(Theme.Sing);

  //TimeBar
  StaticTimeProgress    := AddStatic(Theme.Sing.StaticTimeProgress);
  TextTimeText          := AddText(Theme.Sing.TextTimeText);

// 1 player       | P1
  StaticP1              := AddStatic(Theme.Sing.StaticP1);
  TextP1                := AddText(Theme.Sing.TextP1);

  {StaticP1ScoreBG       := AddStatic(Theme.Sing.StaticP1ScoreBG);
  TextP1Score           := AddText(Theme.Sing.TextP1Score);
  StaticP1SingBar       := AddStatic(Theme.Sing.StaticP1SingBar);}

// 2 or 4 players | P1
  StaticP1TwoP          := AddStatic(Theme.Sing.StaticP1TwoP);
  TextP1TwoP            := AddText(Theme.Sing.TextP1TwoP);

  {StaticP1TwoPScoreBG   := AddStatic(Theme.Sing.StaticP1TwoPScoreBG);
  TextP1TwoPScore       := AddText(Theme.Sing.TextP1TwoPScore);
  StaticP1TwoPSingBar   := AddStatic(Theme.Sing.StaticP2RSingBar);}

  //              | P2
  StaticP2R             := AddStatic(Theme.Sing.StaticP2R);
  TextP2R               := AddText(Theme.Sing.TextP2R);

  {StaticP2RScoreBG      := AddStatic(Theme.Sing.StaticP2RScoreBG);
  TextP2RScore          := AddText(Theme.Sing.TextP2RScore);
  StaticP2RSingBar      := AddStatic(Theme.Sing.StaticP2RSingBar); }

// 3 or 6 players | P1
  StaticP1ThreeP        := AddStatic(Theme.Sing.StaticP1ThreeP);
  TextP1ThreeP          := AddText(Theme.Sing.TextP1ThreeP);

  {StaticP1ThreePScoreBG := AddStatic(Theme.Sing.StaticP1ThreePScoreBG);
  TextP1ThreePScore     := AddText(Theme.Sing.TextP1ThreePScore);
  StaticP1ThreePSingBar := AddStatic(Theme.Sing.StaticP1ThreePSingBar);}

  //              | P2
  StaticP2M             := AddStatic(Theme.Sing.StaticP2M);
  TextP2M               := AddText(Theme.Sing.TextP2M);

  {StaticP2MScoreBG      := AddStatic(Theme.Sing.StaticP2MScoreBG);
  TextP2MScore          := AddText(Theme.Sing.TextP2MScore);
  StaticP2MSingBar      := AddStatic(Theme.Sing.StaticP2MSingBar);}

  //              | P3
  StaticP3R             := AddStatic(Theme.Sing.StaticP3R);
  TextP3R               := AddText(Theme.Sing.TextP3R);

  {StaticP3RScoreBG      := AddStatic(Theme.Sing.StaticP3RScoreBG);
  TextP3RScore          := AddText(Theme.Sing.TextP3RScore);
  StaticP3SingBar       := AddStatic(Theme.Sing.StaticP3SingBar);}

  StaticPausePopup      := AddStatic(Theme.Sing.PausePopUp);
  Static[StaticPausePopup].Visible := false; //Pausepopup is not visibile at the beginning

  if ScreenAct = 2 then begin
      // katze und affe

    end;

  Lyrics := TLyricEngine.Create(80,Skin_LyricsT,640,12,80,Skin_LyricsT+36,640,12);

  LyricsSync := TLyricsSyncSource.Create();
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

  Color: TRGB;

  success:  boolean;
begin
  inherited;

  Log.LogStatus('Begin', 'onShow');
  FadeOut := false;

  // reset video playback engine, to play Video Clip...
  fCurrentVideoPlaybackEngine := VideoPlayback;

  // setup score manager
  Scores.ClearPlayers; // clear old player values
  Color.R := 0; Color.G := 0; Color.B := 0; // dummy atm

  // add new players
  for P := 0 to PlayersPlay-1 do
  begin
    Scores.AddPlayer(Tex_ScoreBG[P], Color);
  end;

  Scores.Init; //Get Positions for Players



  // prepare players
  SetLength(Player, PlayersPlay);
  //Player[0].ScoreTotalInt := 0;

  case PlayersPlay of
    1:  begin
          V1       := true;
          V1TwoP   := false;
          V1ThreeP := false;
          V2R      := false;
          V2M      := false;
          V3R      := false;
        end;
    2:  begin
          V1       := false;
          V1TwoP   := true;
          V1ThreeP := false;
          V2R      := true;
          V2M      := false;
          V3R      := false;
        end;
    3:  begin
          V1       := false;
          V1TwoP   := false;
          V1ThreeP := true;
          V2R      := false;
          V2M      := true;
          V3R      := true;
        end;
    4:  begin // double screen
          V1       := false;
          V1TwoP   := true;
          V1ThreeP := false;
          V2R      := true;
          V2M      := false;
          V3R      := false;
        end;
    6:  begin // double screen
          V1       := false;
          V1TwoP   := false;
          V1ThreeP := true; 
          V2R      := false;
          V2M      := true;
          V3R      := true;
        end;

  end;

  //This one is shown in 1P mode
  Static[StaticP1].Visible              := V1;
  Text[TextP1].Visible                  := V1;

  {Static[StaticP1ScoreBG].Visible       := V1;
  Text[TextP1Score].Visible             := V1;}


  //This one is shown in 2/4P mode
  Static[StaticP1TwoP].Visible          := V1TwoP;
  Text[TextP1TwoP].Visible              := V1TwoP;

  {Static[StaticP1TwoPScoreBG].Visible   := V1TwoP;
  Text[TextP1TwoPScore].Visible         := V1TwoP;}

  Static[StaticP2R].Visible             := V2R;
  Text[TextP2R].Visible                 := V2R;

  {Static[StaticP2RScoreBG].Visible      := V2R;
  Text[TextP2RScore].Visible            := V2R;  }


  //This one is shown in 3/6P mode
  Static[StaticP1ThreeP].Visible        := V1ThreeP;
  Text[TextP1ThreeP].Visible            := V1ThreeP;

  {Static[StaticP1ThreePScoreBG].Visible := V1ThreeP;
  Text[TextP1ThreePScore].Visible       := V1ThreeP; }

  Static[StaticP2M].Visible             := V2M;
  Text[TextP2M].Visible                 := V2M;

  {Static[StaticP2MScoreBG].Visible      := V2M;
  Text[TextP2MScore].Visible            := V2M; }

  Static[StaticP3R].Visible             := V3R;
  Text[TextP3R].Visible                 := V3R;

  {Static[StaticP3RScoreBG].Visible      := V3R;
  Text[TextP3RScore].Visible            := V3R; }

  // FIXME: sets Path and Filename to ''
  ResetSingTemp;

  CurrentSong := CatSongs.Song[CatSongs.Selected];

  // FIXME: bad style, put the try-except into LoadSong() and not here
  try
    // Check if file is XML
    if copy(CurrentSong.FileName,length(CurrentSong.FileName)-3,4) = '.xml'
     then success := CurrentSong.LoadXMLSong()
     else success := CurrentSong.LoadSong();
  except
    success := false;
  end;

  if (not success) then
  begin
    // error loading song -> go back to song screen and show some error message
    FadeTo(@ScreenSong);
    // select new song in party mode
    if ScreenSong.Mode = smPartyMode then
      ScreenSong.SelectRandomSong();
    ScreenPopupError.ShowPopup (Language.Translate('ERROR_CORRUPT_SONG'));
    // FIXME: do we need this?
    CurrentSong.Path := CatSongs.Song[CatSongs.Selected].Path;
    Exit;
  end;



  // reset video playback engine, to play video clip...
  fCurrentVideoPlaybackEngine.Close;
  fCurrentVideoPlaybackEngine := VideoPlayback;

  // set movie
  CurrentSong.VideoLoaded := false;
  fShowVisualization      := false;
  if (CurrentSong.Video <> '') and FileExists(CurrentSong.Path + CurrentSong.Video) then
  begin
    fCurrentVideoPlaybackEngine.Open( CurrentSong.Path + CurrentSong.Video );
    fCurrentVideoPlaybackEngine.Position := CurrentSong.VideoGAP + CurrentSong.Start;
    CurrentSong.VideoLoaded := true;
  end;

  // set background
  if (CurrentSong.Background <> '')  and (CurrentSong.VideoLoaded = false) then
    try
      Tex_Background := Texture.LoadTexture(CurrentSong.Path + CurrentSong.Background);
    except
      Log.LogError('Background could not be loaded: ' + CurrentSong.Path + CurrentSong.Background);
      Tex_Background.TexNum := 0;
    end
  else
    Tex_Background.TexNum := 0;

  // prepare lyrics timer
  LyricsState.Reset();
  LyricsState.SetCurrentTime(CurrentSong.Start);
  LyricsState.StartTime := CurrentSong.Gap;
  if (CurrentSong.Finish > 0) then
    LyricsState.TotalTime := CurrentSong.Finish / 1000
  else
    LyricsState.TotalTime := AudioPlayback.Length;
  LyricsState.UpdateBeats();

  // prepare music
  AudioPlayback.Stop();
  AudioPlayback.Position := CurrentSong.Start;
  // synchronize music to the lyrics
  AudioPlayback.SetSyncSource(LyricsSync);

  // prepare and start voice-capture
  AudioInput.CaptureStart;

  for P := 0 to High(Player) do
    ClearScores(P);

  // main text
  Lyrics.Clear (CurrentSong.BPM[0].BPM, CurrentSong.Resolution);

  // set custom options
  case Ini.LyricsFont of
    0:
      begin
        Lyrics.UpperLineSize := 14;
        Lyrics.LowerLineSize := 14;
        Lyrics.FontStyle := 0;

        Lyrics.LineColor_en.R := Skin_FontR;
        Lyrics.LineColor_en.G := Skin_FontG;
        Lyrics.LineColor_en.B := Skin_FontB;
        Lyrics.LineColor_en.A := 1;

        Lyrics.LineColor_dis.R := 0.4;
        Lyrics.LineColor_dis.G := 0.4;
        Lyrics.LineColor_dis.B := 0.4;
        Lyrics.LineColor_dis.A := 1;

        Lyrics.LineColor_act.R := 5/256;
        Lyrics.LineColor_act.G := 163/256;
        Lyrics.LineColor_act.B := 210/256;
        Lyrics.LineColor_act.A := 1;

        {
        LyricSub.FontStyle := 0;
        LyricMain.Size := 14; // 13
        LyricSub.Size := 14; // 13
        LyricMain.ColR := Skin_FontR;
        LyricMain.ColG := Skin_FontG;
        LyricMain.ColB := Skin_FontB; //Change für Crazy Joker
        }
        {
        LyricMain.ColSR := Skin_FontHighlightR;
        LyricMain.ColSG := Skin_FontHighlightG;
        LyricMain.ColSB := Skin_FontHighlightB;
        }{
        LyricMain.ColSR := 5/255; //26
        LyricMain.ColSG := 163/255; //165
        LyricMain.ColSB := 210/255;  //220

        LyricSub.ColR := 0.4; //0.6
        LyricSub.ColG := 0.4; //0.6
        LyricSub.ColB := 0.4; //0.6
        }
      end;
    1:
      begin
        {
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
        }

        Lyrics.UpperLineSize := 14;
        Lyrics.LowerLineSize := 14;
        Lyrics.FontStyle := 2;

        Lyrics.LineColor_en.R := 0.75;
        Lyrics.LineColor_en.G := 0.75;
        Lyrics.LineColor_en.B := 1;
        Lyrics.LineColor_en.A := 1;

        Lyrics.LineColor_dis.R := 0.8;
        Lyrics.LineColor_dis.G := 0.8;
        Lyrics.LineColor_dis.B := 0.8;
        Lyrics.LineColor_dis.A := 1;

        Lyrics.LineColor_act.R := 0.5;
        Lyrics.LineColor_act.G := 0.5;
        Lyrics.LineColor_act.B := 1;
        Lyrics.LineColor_act.A := 1;
      end;
    2:
      begin
        Lyrics.UpperLineSize := 12;
        Lyrics.LowerLineSize := 12;
        Lyrics.FontStyle := 3;

        Lyrics.LineColor_en.R := 0.75;
        Lyrics.LineColor_en.G := 0.75;
        Lyrics.LineColor_en.B := 1;
        Lyrics.LineColor_en.A := 1;

        Lyrics.LineColor_dis.R := 0.8;
        Lyrics.LineColor_dis.G := 0.8;
        Lyrics.LineColor_dis.B := 0.8;
        Lyrics.LineColor_dis.A := 1;

        Lyrics.LineColor_act.R := 0.5;
        Lyrics.LineColor_act.G := 0.5;
        Lyrics.LineColor_act.B := 1;
        Lyrics.LineColor_act.A := 1;
        {
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
        }
      end;
  end; // case

  // Initialize lyrics by filling its queue
  while (not Lyrics.IsQueueFull) and
        (Lyrics.LineCounter <= High(Lines[0].Line)) do
  begin
    Lyrics.AddLine(@Lines[0].Line[Lyrics.LineCounter]);
  end;

  // Deactivate pause
  Paused := False;

  // Kill all stars not killed yet (GoldenStarsTwinkle Mod)
  GoldenRec.SentenceChange;

  {//Set Position of Line Bonus - Line Bonus start
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
  end; }
  
  // set Position of Line Bonus - Line Bonus end
  // set number of empty sentences for Line Bonus
  NumEmptySentences := 0;
  for P := Low(Lines[0].Line) to High(Lines[0].Line) do
    if Lines[0].Line[P].TotalNotes = 0 then Inc(NumEmptySentences);

  Log.LogStatus('End', 'onShow');
end;

procedure TScreenSing.onShowFinish;
begin
  // start lyrics
  LyricsState.Resume();

  // start music
  AudioPlayback.Play();

  // start timer
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
  CurLyricsTime: real;
begin
  // ScoreBG Mod
  // TODO: remove this commented out section as we do not need it anymore.
  //  We use colorized png's now. Set player colors does nothing than changing
  //  the colors of the statics which will lead to ugly effects on colorized pngs
  {
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
 }

  // set player names (for 2 screens and only Singstar skin)
  if ScreenAct = 1 then begin
    Text[TextP1].Text       := 'P1';
    Text[TextP1TwoP].Text   := 'P1';
    Text[TextP1ThreeP].Text := 'P1';
    Text[TextP2R].Text      := 'P2';
    Text[TextP2M].Text      := 'P2';
    Text[TextP3R].Text      := 'P3';
  end;

  if ScreenAct = 2 then begin
    case PlayersPlay of
      {
        1:  begin
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
            end;
      }
      4:  begin
            Text[TextP1TwoP].Text   := 'P3';
            Text[TextP2R].Text      := 'P4';
          end;
      6:  begin
            Text[TextP1ThreeP].Text := 'P4';
            Text[TextP2M].Text      := 'P5';
            Text[TextP3R].Text      := 'P6';
          end;
    end; // case
  end; // if


  ////
  // dual screen, part 1
  ////////////////////////

  // Note: ScreenX is the offset of the current screen in dual-screen mode so we
  // will move the statics and texts to the correct screen here.
  // FIXME: clean up this weird stuff. Commenting this stuff out, nothing
  //   was missing on screen w/ 6 players - so do we even need this stuff?
  Static[StaticP1].Texture.X         := Static[StaticP1].Texture.X + 10*ScreenX;

  Text[TextP1].X                     := Text[TextP1].X + 10*ScreenX;

  {Static[StaticP1ScoreBG].Texture.X  := Static[StaticP1ScoreBG].Texture.X + 10*ScreenX;
  Text[TextP1Score].X                := Text[TextP1Score].X + 10*ScreenX;}


  Static[StaticP2R].Texture.X        := Static[StaticP2R].Texture.X + 10*ScreenX;

  Text[TextP2R].X                    := Text[TextP2R].X + 10*ScreenX;

  {Static[StaticP2RScoreBG].Texture.X := Static[StaticP2RScoreBG].Texture.X + 10*ScreenX;
  Text[TextP2RScore].X               := Text[TextP2RScore].X + 10*ScreenX;}

  // end of weird stuff

  Static[1].Texture.X := Static[1].Texture.X + 10*ScreenX;

  for T := 0 to 1 do
    Text[T].X := Text[T].X + 10*ScreenX;



  // retrieve current lyrics time, we have to store the value to avoid
  // that min- and sec-values do not match 
  CurLyricsTime := LyricsState.GetCurrentTime();
  Min := Round(CurLyricsTime) div 60;
  Sec := Round(CurLyricsTime) mod 60;

  // update static menu with time ...
  Text[TextTimeText].Text := '';
  if Min < 10 then Text[TextTimeText].Text := '0';
  Text[TextTimeText].Text := Text[TextTimeText].Text + IntToStr(Min) + ':';
  if Sec < 10 then Text[TextTimeText].Text := Text[TextTimeText].Text + '0';
  Text[TextTimeText].Text := Text[TextTimeText].Text + IntToStr(Sec);

  {// .. and scores
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
  end;  }

  // draw static menu (BG)
  // Note: there is no menu and the animated background brakes the video playback
  //DrawBG;

  // Draw Background
  SingDrawBackground;

  // update and draw movie
  if (ShowFinish and
      (CurrentSong.VideoLoaded or fShowVisualization)) then
  begin
    if assigned( fCurrentVideoPlaybackEngine ) then
    begin
      fCurrentVideoPlaybackEngine.GetFrame(LineState.GetCurrentTime());
      fCurrentVideoPlaybackEngine.DrawGL(ScreenAct);
    end;
  end;

  // draw static menu (FG)
  DrawFG;

  // check for music finish
  //Log.LogError('Check for music finish: ' + BoolToStr(Music.Finished) + ' ' + FloatToStr(LineState.CurrentTime*1000) + ' ' + IntToStr(CurrentSong.Finish));
  if ShowFinish then
  begin
    if (not AudioPlayback.Finished) and
       ((CurrentSong.Finish = 0) or (LineState.GetCurrentTime()*1000 <= CurrentSong.Finish)) then
    begin
      // analyze song if not paused
      if (not Paused) then
        Sing(Self);
    end
    else
    begin
      if (not FadeOut) then
      begin
        Finish;
        FadeOut := true;
        FadeTo(@ScreenScore);
      end;
    end;
  end;

  // always draw custom items
  SingDraw;

  //GoldenNoteStarsTwinkle
  GoldenRec.SpawnRec;

  //Draw Scores
  Scores.Draw;

  ////
  // dual screen, part 2
  ////////////////////////

  // Note: ScreenX is the offset of the current screen in dual-screen mode so we
  // will move the statics and texts to the correct screen here.
  // FIXME: clean up this weird stuff

  Static[StaticP1].Texture.X         := Static[StaticP1].Texture.X - 10*ScreenX;
  Text[TextP1].X                     := Text[TextP1].X - 10*ScreenX;

  {Static[StaticP1ScoreBG].Texture.X  := Static[StaticP1ScoreBG].Texture.X - 10*ScreenX;
  Text[TextP1Score].X                := Text[TextP1Score].X - 10*ScreenX;}


  Static[StaticP2R].Texture.X        := Static[StaticP2R].Texture.X - 10*ScreenX;
  Text[TextP2R].X                    := Text[TextP2R].X - 10*ScreenX;

  {Static[StaticP2RScoreBG].Texture.X := Static[StaticP2RScoreBG].Texture.X - 10*ScreenX;
  Text[TextP2RScore].X               := Text[TextP2RScore].X - 10*ScreenX;}

  //end of weird

  Static[1].Texture.X := Static[1].Texture.X - 10*ScreenX;

  for T := 0 to 1 do
    Text[T].X := Text[T].X - 10*ScreenX;

  // Draw Pausepopup
  // FIXME: this is a workaround that the Static is drawn over the Lyrics, Lines, Scores and Effects
  // maybe someone could find a better solution
  if Paused then
  begin
    Static[StaticPausePopup].Visible := true;
    Static[StaticPausePopup].Draw;
    Static[StaticPausePopup].Visible := false;
  end;

end;

procedure TScreenSing.Finish;
begin
  AudioInput.CaptureStop;
  AudioPlayback.Stop;
  AudioPlayback.SetSyncSource(nil);

  if (Ini.SavePlayback = 1) then begin
    Log.BenchmarkStart(0);
    Log.LogVoice(0);
    Log.LogVoice(1);
    Log.LogVoice(2);
    Log.BenchmarkEnd(0);
    Log.LogBenchmark('Creating files', 0);
  end;

  if CurrentSong.VideoLoaded then
  begin
    fCurrentVideoPlaybackEngine.Close;
    CurrentSong.VideoLoaded := false; // to prevent drawing closed video
  end;

  SetFontItalic (False);
end;

procedure TScreenSing.OnSentenceEnd(SentenceIndex: Cardinal);
var
  PlayerIndex: Integer;
  CurrentPlayer: PPLayer;
  CurrentScore: Real;
  Line: PLine;
  LinePerfection: Real;  // perfection of singing performance on the current line
  Rating: integer;
  LineScore: Real;
  LineBonus: Real;
  MaxSongScore: integer; // max. points for the song (without line bonus)
  MaxLineScore: Real;    // max. points for the current line
const
  // TODO: move this to a better place
  MAX_LINE_RATING = 8;        // max. rating for singing performance
begin
  Line := @Lines[0].Line[SentenceIndex];

  // check for empty sentence
  if (Line.TotalNotes <= 0) then
    Exit;

  // set max song score
  if (Ini.LineBonus = 0) then
    MaxSongScore := MAX_SONG_SCORE
  else
    MaxSongScore := MAX_SONG_SCORE - MAX_SONG_LINE_BONUS;

  // Note: ScoreValue is the sum of all note values of the song
  MaxLineScore := MaxSongScore * (Line.TotalNotes / Lines[0].ScoreValue);

  for PlayerIndex := 0 to High(Player) do
  begin
    CurrentPlayer := @Player[PlayerIndex];
    CurrentScore := CurrentPlayer.Score + CurrentPlayer.ScoreGolden;

    // Line Bonus

    // points for this line
    LineScore := CurrentScore - CurrentPlayer.ScoreLast;

    // determine LinePerfection
    // Note: the "+2" extra points are a little bonus so the player does not
    //   have to be that perfect to reach the bonus steps.
    LinePerfection := (LineScore + 2) / MaxLineScore;

    // clamp LinePerfection to range [0..1]
    if (LinePerfection < 0) then
      LinePerfection := 0
    else if (LinePerfection > 1) then
      LinePerfection := 1;

    // add line-bonus if enabled
    if (Ini.LineBonus > 0) then
    begin
      // line-bonus points (same for each line, no matter how long the line is)
      LineBonus := MAX_SONG_LINE_BONUS /
                   (Length(Lines[0].Line) - NumEmptySentences);
      // apply line-bonus
      CurrentPlayer.ScoreLine := CurrentPlayer.ScoreLine +
                                 LineBonus * LinePerfection;
      CurrentPlayer.ScoreLineInt := Round(CurrentPlayer.ScoreLine / 10) * 10;
      // update total score
      CurrentPlayer.ScoreTotalInt := CurrentPlayer.ScoreInt +
                                     CurrentPlayer.ScoreGoldenInt +
                                     CurrentPlayer.ScoreLineInt;

      // spawn rating pop-up
      Rating := Round(LinePerfection * MAX_LINE_RATING);
      Scores.SpawnPopUp(PlayerIndex, Rating, CurrentPlayer.ScoreTotalInt);
    end;

    // PerfectLineTwinkle (effect), Part 1
    If (Ini.EffectSing = 1) then
      CurrentPlayer.LastSentencePerfect := (LinePerfection >= 1);

    // refresh last score
    CurrentPlayer.ScoreLast := CurrentScore;
  end;

  // PerfectLineTwinkle (effect), Part 2
  if (Ini.EffectSing = 1) then
    GoldenRec.SpawnPerfectLineTwinkle;
end;

// Called on sentence change
// SentenceIndex: index of the new active sentence
procedure TScreenSing.OnSentenceChange(SentenceIndex: Cardinal);
var
  LyricEngine: TLyricEngine;
begin
  //GoldenStarsTwinkle
  GoldenRec.SentenceChange;

  // Fill lyrics queue and set upper line to the current sentence
  while (Lyrics.GetUpperLineIndex() < SentenceIndex) or
        (not Lyrics.IsQueueFull) do
  begin
    // Add the next line to the queue or a dummy if no more lines are available
    if (Lyrics.LineCounter <= High(Lines[0].Line)) then
      Lyrics.AddLine(@Lines[0].Line[Lyrics.LineCounter])
    else
      Lyrics.AddLine(nil);
  end;

  // AddLine draws the passed line to the back-buffer of the render context
  // and copies it into a texture afterwards (offscreen rendering).
  // This leaves an in invalidated screen. Calling Draw() makes sure,
  // that the back-buffer stores the sing-screen, when the next
  // swap between the back- and front-buffer is done (eliminates flickering)
  // 
  // Note: calling AddLine() right before the regular screen update (Display.Draw)
  // would be a better solution.
  Draw;
end;

function TLyricsSyncSource.GetClock(): real;
begin
  Result := LineState.GetCurrentTime();
end;

end.
