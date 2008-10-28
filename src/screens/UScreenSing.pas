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
  private
    VideoLoaded: boolean;
  protected
    Paused:     boolean; //Pause Mod
    LyricsSync: TLyricsSyncSource;
    NumEmptySentences: integer;
  public
    // TimeBar fields
    StaticTimeProgress: integer;
    TextTimeText: integer;

    StaticP1: integer;
    TextP1:   integer;

    //shown when game is in 2/4 player modus
    StaticP1TwoP: integer;
    TextP1TwoP:   integer;

    //shown when game is in 3/6 player modus
    StaticP1ThreeP: integer;
    TextP1ThreeP:   integer;

    StaticP2R: integer;
    TextP2R:   integer;

    StaticP2M: integer;
    TextP2M:   integer;

    StaticP3R: integer;
    TextP3R:   integer;

    StaticPausePopup: integer;

    Tex_Background: TTexture;
    FadeOut: boolean;
    Lyrics:  TLyricEngine;

    //Score Manager:
    Scores: TSingScores;

    fShowVisualization: boolean;
    fCurrentVideoPlaybackEngine: IVideoPlayback;

    constructor Create; override;
    procedure onShow; override;
    procedure onShowFinish; override;
    procedure onHide; override;
    
    function ParseInput(PressedKey: cardinal; CharCode: widechar;
      PressedDown: boolean): boolean; override;
    function Draw: boolean; override;

    procedure Finish; virtual;
    procedure Pause; // Toggle Pause

    procedure OnSentenceEnd(SentenceIndex: cardinal);     // for LineBonus + Singbar
    procedure OnSentenceChange(SentenceIndex: cardinal);  // for Golden Notes
  end;

implementation

uses UGraphic,
  UDraw,
  UMain,
  USong,
  Classes,
  URecord,
  ULanguage,
  Math;

 // Method for input parsing. If False is returned, GetNextWindow
 // should be checked to know the next window to load;
function TScreenSing.ParseInput(PressedKey: cardinal; CharCode: widechar;
  PressedDown: boolean): boolean;
begin
  Result := True;
  if (PressedDown) then
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

        Result := False;
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
      SDLK_BACKSPACE:
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
      SDLK_DOWN:
      begin
      end;
      SDLK_UP:
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
    Paused := True;

    LyricsState.Pause();

    // pause Music
    AudioPlayback.Pause;

    // pause Video
    if (CurrentSong.Video <> '') and FileExists(CurrentSong.Path +
      CurrentSong.Video) then
      fCurrentVideoPlaybackEngine.Pause;

  end
  else              //disable Pause
  begin
    LyricsState.Resume();

    // Play Music
    AudioPlayback.Play;

    // Video
    if (CurrentSong.Video <> '') and FileExists(CurrentSong.Path +
      CurrentSong.Video) then
      fCurrentVideoPlaybackEngine.Pause;

    Paused := False;
  end;
end;
//Pause Mod End

constructor TScreenSing.Create;
begin
  inherited Create;

  fShowVisualization := False;

  fCurrentVideoPlaybackEngine := VideoPlayback;

  //Create Score Class
  Scores := TSingScores.Create;
  Scores.LoadfromTheme;

  LoadFromTheme(Theme.Sing);

  //TimeBar
  StaticTimeProgress := AddStatic(Theme.Sing.StaticTimeProgress);
  TextTimeText := AddText(Theme.Sing.TextTimeText);

  // 1 player       | P1
  StaticP1 := AddStatic(Theme.Sing.StaticP1);
  TextP1   := AddText(Theme.Sing.TextP1);

  // 2 or 4 players | P1
  StaticP1TwoP := AddStatic(Theme.Sing.StaticP1TwoP);
  TextP1TwoP   := AddText(Theme.Sing.TextP1TwoP);

  //              | P2
  StaticP2R := AddStatic(Theme.Sing.StaticP2R);
  TextP2R   := AddText(Theme.Sing.TextP2R);

  // 3 or 6 players | P1
  StaticP1ThreeP := AddStatic(Theme.Sing.StaticP1ThreeP);
  TextP1ThreeP   := AddText(Theme.Sing.TextP1ThreeP);

  //              | P2
  StaticP2M := AddStatic(Theme.Sing.StaticP2M);
  TextP2M   := AddText(Theme.Sing.TextP2M);

  //              | P3
  StaticP3R := AddStatic(Theme.Sing.StaticP3R);
  TextP3R   := AddText(Theme.Sing.TextP3R);

  StaticPausePopup := AddStatic(Theme.Sing.PausePopUp);

  //<note>Pausepopup is not visibile at the beginning</note>
  Static[StaticPausePopup].Visible := False;

  Lyrics := TLyricEngine.Create(
      Skin_LyricsUpperX, Skin_LyricsUpperY, Skin_LyricsUpperW, Skin_LyricsUpperH,
      Skin_LyricsLowerX, Skin_LyricsLowerY, Skin_LyricsLowerW, Skin_LyricsLowerH);

  LyricsSync := TLyricsSyncSource.Create();
end;

procedure TScreenSing.onShow;
var
  P:      integer;
  V1:     boolean;
  V1TwoP: boolean;   //Position of ScoreBox in two-player mode
  V1ThreeP: boolean; //Position of ScoreBox in three-player mode
  V2R:    boolean;
  V2M:    boolean;
  V3R:    boolean;
  Color: TRGB;

  success: boolean;
begin
  inherited;

  Log.LogStatus('Begin', 'onShow');
  FadeOut := False;

  // reset video playback engine, to play Video Clip...
  fCurrentVideoPlaybackEngine := VideoPlayback;

  // setup score manager
  Scores.ClearPlayers; // clear old player values
  Color.R := 0;
  Color.G := 0;
  Color.B := 0; // dummy atm  <- \(O.o)/? B like bummy?

  // add new players
  for P := 0 to PlayersPlay - 1 do
  begin
    Scores.AddPlayer(Tex_ScoreBG[P], Color);
  end;

  Scores.Init; //Get Positions for Players

  // prepare players
  SetLength(Player, PlayersPlay);

  case PlayersPlay of
    1:
    begin
      V1     := True;
      V1TwoP := False;
      V1ThreeP := False;
      V2R    := False;
      V2M    := False;
      V3R    := False;
    end;
    2:
    begin
      V1     := False;
      V1TwoP := True;
      V1ThreeP := False;
      V2R    := True;
      V2M    := False;
      V3R    := False;
    end;
    3:
    begin
      V1     := False;
      V1TwoP := False;
      V1ThreeP := True;
      V2R    := False;
      V2M    := True;
      V3R    := True;
    end;
    4:
    begin // double screen
      V1     := False;
      V1TwoP := True;
      V1ThreeP := False;
      V2R    := True;
      V2M    := False;
      V3R    := False;
    end;
    6:
    begin // double screen
      V1     := False;
      V1TwoP := False;
      V1ThreeP := True;
      V2R    := False;
      V2M    := True;
      V3R    := True;
    end;

  end;

  //This one is shown in 1P mode
  Static[StaticP1].Visible := V1;
  Text[TextP1].Visible     := V1;


  //This one is shown in 2/4P mode
  Static[StaticP1TwoP].Visible := V1TwoP;
  Text[TextP1TwoP].Visible     := V1TwoP;

  Static[StaticP2R].Visible := V2R;
  Text[TextP2R].Visible     := V2R;


  //This one is shown in 3/6P mode
  Static[StaticP1ThreeP].Visible := V1ThreeP;
  Text[TextP1ThreeP].Visible     := V1ThreeP;


  Static[StaticP2M].Visible := V2M;
  Text[TextP2M].Visible     := V2M;


  Static[StaticP3R].Visible := V3R;
  Text[TextP3R].Visible     := V3R;


  // FIXME: sets Path and Filename to ''
  ResetSingTemp;

  CurrentSong := CatSongs.Song[CatSongs.Selected];

  // FIXME: bad style, put the try-except into LoadSong() and not here
  try
    // Check if file is XML
    if copy(CurrentSong.FileName, length(CurrentSong.FileName) - 3, 4) = '.xml' then
      success := CurrentSong.LoadXMLSong()
    else
      success := CurrentSong.LoadSong();
  except
    success := False;
  end;

  if (not success) then
  begin
    // error loading song -> go back to song screen and show some error message
    FadeTo(@ScreenSong);
    // select new song in party mode
    if ScreenSong.Mode = smPartyMode then
      ScreenSong.SelectRandomSong();
    if (Length(CurrentSong.LastError) > 0) then
      ScreenPopupError.ShowPopup(Format(Language.Translate(CurrentSong.LastError), [CurrentSong.ErrorLineNo]))
    else
      ScreenPopupError.ShowPopup(Language.Translate('ERROR_CORRUPT_SONG'));
    // FIXME: do we need this?
    CurrentSong.Path := CatSongs.Song[CatSongs.Selected].Path;
    Exit;
  end;

  // reset video playback engine, to play video clip...
  fCurrentVideoPlaybackEngine.Close;
  fCurrentVideoPlaybackEngine := VideoPlayback;

  {*
   * == Background ==
   * We have four types of backgrounds:
   *   + Blank        : Nothing has been set, this is our fallback
   *   + Picture      : Picture has been set, and exists - otherwise we fallback
   *   + Video        : Video has been set, and exists - otherwise we fallback
   *   + Visualization: + Off        : No Visialization
   *                    + WhenNoVideo: Overwrites Blank and Picture
   *                    + On         : Overwrites Blank, Picture and Video
   *}

  {*
   * set background to: video
   *}
  VideoLoaded := False;
  fShowVisualization := False;
  if (CurrentSong.Video <> '') and FileExists(CurrentSong.Path + CurrentSong.Video) then
  begin
    if (fCurrentVideoPlaybackEngine.Open(CurrentSong.Path + CurrentSong.Video)) then
    begin
      fShowVisualization := False;
      fCurrentVideoPlaybackEngine := VideoPlayback;
      fCurrentVideoPlaybackEngine.Position := CurrentSong.VideoGAP + CurrentSong.Start;
      fCurrentVideoPlaybackEngine.Play;
      VideoLoaded := True;
    end;
  end;

  {*
   * set background to: picture
   *}
  if (CurrentSong.Background <> '') and (VideoLoaded = False)
    and (TVisualizerOption(Ini.VisualizerOption) = voOff)  then
    try
      Tex_Background := Texture.LoadTexture(CurrentSong.Path + CurrentSong.Background);
    except
      Log.LogError('Background could not be loaded: ' + CurrentSong.Path +
        CurrentSong.Background);
      Tex_Background.TexNum := 0;
    end
  else
  begin
    Tex_Background.TexNum := 0;
  end;

  {*
   * set background to: visualization (Overwrites all)
   *}
  if (TVisualizerOption(Ini.VisualizerOption) in [voOn]) then
  begin
    fShowVisualization := True;
    fCurrentVideoPlaybackEngine := Visualization;
    if (fCurrentVideoPlaybackEngine <> nil) then
      fCurrentVideoPlaybackEngine.Play;
  end;

  {*
   * set background to: visualization (Videos are still shown)
   *}
  if ((TVisualizerOption(Ini.VisualizerOption) in [voWhenNoVideo]) and
     (VideoLoaded = False)) then
  begin
    fShowVisualization := True;
    fCurrentVideoPlaybackEngine := Visualization;
    if (fCurrentVideoPlaybackEngine <> nil) then
      fCurrentVideoPlaybackEngine.Play;
  end;

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
  Lyrics.Clear(CurrentSong.BPM[0].BPM, CurrentSong.Resolution);

  // set custom options
  case Ini.LyricsFont of
    0: // normal fonts
    begin
      Lyrics.FontStyle := 0;

      Lyrics.LineColor_en.R := Skin_FontR;
      Lyrics.LineColor_en.G := Skin_FontG;
      Lyrics.LineColor_en.B := Skin_FontB;
      Lyrics.LineColor_en.A := 1;

      Lyrics.LineColor_dis.R := 0.4;
      Lyrics.LineColor_dis.G := 0.4;
      Lyrics.LineColor_dis.B := 0.4;
      Lyrics.LineColor_dis.A := 1;

      Lyrics.LineColor_act.R := 0.02;
      Lyrics.LineColor_act.G := 0.6;
      Lyrics.LineColor_act.B := 0.8;
      Lyrics.LineColor_act.A := 1;
    end;
    1, 2: // outline fonts (is TScalableOutlineFont)
    begin
      Lyrics.FontStyle := Ini.LyricsFont + 1;

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

  // set Position of Line Bonus - Line Bonus end
  // set number of empty sentences for Line Bonus
  NumEmptySentences := 0;
  for P := Low(Lines[0].Line) to High(Lines[0].Line) do
    if Lines[0].Line[P].TotalNotes = 0 then
      Inc(NumEmptySentences);

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

procedure TScreenSing.onHide;
begin
  // Unload background texture
  if (Tex_Background.TexNum > 0) then
  begin
    glDeleteTextures(1, PGLuint(@Tex_Background.TexNum));
    Tex_Background.TexNum := 0;
  end;

  Background.OnFinish;
end;

function TScreenSing.Draw: boolean;
var
  Min:   integer;
  Sec:   integer;
  T:     integer;
  CurLyricsTime: real;
begin

  Background.Draw;

  // set player names (for 2 screens and only Singstar skin)
  if ScreenAct = 1 then
  begin
    Text[TextP1].Text     := 'P1';
    Text[TextP1TwoP].Text := 'P1';
    Text[TextP1ThreeP].Text := 'P1';
    Text[TextP2R].Text    := 'P2';
    Text[TextP2M].Text    := 'P2';
    Text[TextP3R].Text    := 'P3';
  end;

  if ScreenAct = 2 then
  begin
    case PlayersPlay of
      4:
      begin
        Text[TextP1TwoP].Text := 'P3';
        Text[TextP2R].Text    := 'P4';
      end;
      6:
      begin
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
  Static[StaticP1].Texture.X := Static[StaticP1].Texture.X + 10 * ScreenX;

  Text[TextP1].X := Text[TextP1].X + 10 * ScreenX;

  {Static[StaticP1ScoreBG].Texture.X  := Static[StaticP1ScoreBG].Texture.X + 10*ScreenX;
  Text[TextP1Score].X                := Text[TextP1Score].X + 10*ScreenX;}


  Static[StaticP2R].Texture.X := Static[StaticP2R].Texture.X + 10 * ScreenX;

  Text[TextP2R].X := Text[TextP2R].X + 10 * ScreenX;

  {Static[StaticP2RScoreBG].Texture.X := Static[StaticP2RScoreBG].Texture.X + 10*ScreenX;
  Text[TextP2RScore].X               := Text[TextP2RScore].X + 10*ScreenX;}

  // end of weird stuff

  Static[1].Texture.X := Static[1].Texture.X + 10 * ScreenX;

  for T := 0 to 1 do
    Text[T].X := Text[T].X + 10 * ScreenX;



  // retrieve current lyrics time, we have to store the value to avoid
  // that min- and sec-values do not match
  CurLyricsTime := LyricsState.GetCurrentTime();
  Min := Round(CurLyricsTime) div 60;
  Sec := Round(CurLyricsTime) mod 60;

  // update static menu with time ...
  Text[TextTimeText].Text := '';
  if Min < 10 then
    Text[TextTimeText].Text := '0';
  Text[TextTimeText].Text := Text[TextTimeText].Text + IntToStr(Min) + ':';
  if Sec < 10 then
    Text[TextTimeText].Text := Text[TextTimeText].Text + '0';
  Text[TextTimeText].Text := Text[TextTimeText].Text + IntToStr(Sec);

  // draw static menu (BG)
  // Note: there is no menu and the animated background brakes the video playback
  //DrawBG;

  // Draw Background
  SingDrawBackground;

  // update and draw movie
  if (ShowFinish and (VideoLoaded or fShowVisualization)) then
  begin
    if assigned(fCurrentVideoPlaybackEngine) then
    begin
      fCurrentVideoPlaybackEngine.GetFrame(LyricsState.GetCurrentTime());
      fCurrentVideoPlaybackEngine.DrawGL(ScreenAct);
    end;
  end;

  // draw static menu (FG)
  DrawFG;

  // check for music finish
  //Log.LogError('Check for music finish: ' + BoolToStr(Music.Finished) + ' ' + FloatToStr(LyricsState.CurrentTime*1000) + ' ' + IntToStr(CurrentSong.Finish));
  if ShowFinish then
  begin
    if (not AudioPlayback.Finished) and ((CurrentSong.Finish = 0) or
      (LyricsState.GetCurrentTime() * 1000 <= CurrentSong.Finish)) then
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
        FadeOut := True;
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

  Static[StaticP1].Texture.X := Static[StaticP1].Texture.X - 10 * ScreenX;
  Text[TextP1].X := Text[TextP1].X - 10 * ScreenX;

  Static[StaticP2R].Texture.X := Static[StaticP2R].Texture.X - 10 * ScreenX;
  Text[TextP2R].X := Text[TextP2R].X - 10 * ScreenX;

  //end of weird

  Static[1].Texture.X := Static[1].Texture.X - 10 * ScreenX;

  for T := 0 to 1 do
    Text[T].X := Text[T].X - 10 * ScreenX;

  // Draw Pausepopup
  // FIXME: this is a workaround that the Static is drawn over the Lyrics, Lines, Scores and Effects
  // maybe someone could find a better solution
  if Paused then
  begin
    Static[StaticPausePopup].Visible := True;
    Static[StaticPausePopup].Draw;
    Static[StaticPausePopup].Visible := False;
  end;

  Result := True;
end;

procedure TScreenSing.Finish;
begin
  AudioInput.CaptureStop;
  AudioPlayback.Stop;
  AudioPlayback.SetSyncSource(nil);

  if (VideoPlayback <> nil) then
    VideoPlayback.Close;

  if (Visualization <> nil) then
    Visualization.Close;

  // to prevent drawing closed video
  VideoLoaded := False;

  //Kill all Stars and Effects
  GoldenRec.KillAll;
  
  if (Ini.SavePlayback = 1) then
  begin
    Log.BenchmarkStart(0);
    Log.LogVoice(0);
    Log.LogVoice(1);
    Log.LogVoice(2);
    Log.BenchmarkEnd(0);
    Log.LogBenchmark('Creating files', 0);
  end;

  SetFontItalic(False);
end;

procedure TScreenSing.OnSentenceEnd(SentenceIndex: cardinal);
var
  PlayerIndex: integer;
  CurrentPlayer: PPLayer;
  CurrentScore: real;
  Line:      PLine;
  LinePerfection: real;  // perfection of singing performance on the current line
  Rating:    integer;
  LineScore: real;
  LineBonus: real;
  MaxSongScore: integer; // max. points for the song (without line bonus)
  MaxLineScore: real;    // max. points for the current line
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
    CurrentScore  := CurrentPlayer.Score + CurrentPlayer.ScoreGolden;

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
      LineBonus := MAX_SONG_LINE_BONUS / (Length(Lines[0].Line) -
        NumEmptySentences);
      // apply line-bonus
      CurrentPlayer.ScoreLine :=
        CurrentPlayer.ScoreLine + LineBonus * LinePerfection;
      CurrentPlayer.ScoreLineInt := Round(CurrentPlayer.ScoreLine / 10) * 10;
      // update total score
      CurrentPlayer.ScoreTotalInt :=
        CurrentPlayer.ScoreInt +
        CurrentPlayer.ScoreGoldenInt
        + CurrentPlayer.ScoreLineInt;

      // spawn rating pop-up
      Rating := Round(LinePerfection * MAX_LINE_RATING);
      Scores.SpawnPopUp(PlayerIndex, Rating, CurrentPlayer.ScoreTotalInt);
    end;

    // PerfectLineTwinkle (effect), Part 1
    if (Ini.EffectSing = 1) then
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
procedure TScreenSing.OnSentenceChange(SentenceIndex: cardinal);
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
end;

function TLyricsSyncSource.GetClock(): real;
begin
  Result := LyricsState.GetCurrentTime();
end;

end.

