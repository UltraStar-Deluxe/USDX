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

uses
  SysUtils,
  gl,
  SDL,
  TextGL,
  UFiles,
  UGraphicClasses,
  UIni,
  ULog,
  ULyrics,
  UMenu,
  UMusic,
  USingScores,
  USongs,
  UTexture,
  UThemes,
  UTime;

type
  TLyricsSyncSource = class(TSyncSource)
    function GetClock(): real; override;
  end;

type
  TScreenSing = class(TMenu)
  protected
    VideoLoaded: boolean;
    Paused:     boolean; // pause mod
    LyricsSync: TLyricsSyncSource;
    NumEmptySentences: integer;
  public
    // timebar fields
    StaticTimeProgress: integer;
    TextTimeText: integer;

    StaticP1: integer;
    TextP1:   integer;

    // shown when game is in 2/4 player modus
    StaticP1TwoP: integer;
    TextP1TwoP:   integer;

    // shown when game is in 3/6 player modus
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

    // score manager:
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
    procedure Pause; // toggle pause

    procedure OnSentenceEnd(SentenceIndex: cardinal);     // for linebonus + singbar
    procedure OnSentenceChange(SentenceIndex: cardinal);  // for golden notes
  end;

implementation

uses
  Classes,
  Math,
  UDraw,
  UGraphic,
  ULanguage,
  UNote,
  URecord,
  USong,
  UDisplay;

// method for input parsing. if false is returned, getnextwindow
// should be checked to know the next window to load;

function TScreenSing.ParseInput(PressedKey: cardinal; CharCode: widechar;
  PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // key down
    // check normal keys
    case WideCharUpperCase(CharCode)[1] of
      'Q':
      begin
        // when not ask before exit then finish now
        if (Ini.AskbeforeDel <> 1) then
          Finish
        // else just pause and let the popup make the work
        else if not Paused then
          Pause;

        Result := false;
        Exit;
      end;
      'V': // show visualization
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
        // record sound hack:
        //Sound[0].BufferLong

        Finish;
        AudioPlayback.PlaySound(SoundLib.Back);
        FadeTo(@ScreenScore);
      end;

      SDLK_SPACE:
      begin
        Pause;
      end;

      SDLK_TAB: // change visualization preset
      begin
        if fShowVisualization then
          fCurrentVideoPlaybackEngine.Position := now; // move to a random position
      end;

      SDLK_RETURN:
      begin
      end;

      // up and down could be done at the same time,
      // but i don't want to declare variables inside
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

// pause mod
procedure TScreenSing.Pause;
begin
  if (not Paused) then  // enable pause
  begin
    // pause time
    Paused := true;

    LyricsState.Pause();

    // pause music
    AudioPlayback.Pause;

    // pause video
    if (CurrentSong.Video <> '') and FileExists(CurrentSong.Path +
      CurrentSong.Video) then
      fCurrentVideoPlaybackEngine.Pause;

  end
  else              // disable pause
  begin
    LyricsState.Resume();

    // play music
    AudioPlayback.Play;

    // video
    if (CurrentSong.Video <> '') and FileExists(CurrentSong.Path +
      CurrentSong.Video) then
      fCurrentVideoPlaybackEngine.Pause;

    Paused := false;
  end;
end;
// pause mod end

constructor TScreenSing.Create;
begin
  inherited Create;

  //too dangerous, a mouse button is quickly pressed by accident
  RightMbESC := false;

  fShowVisualization := false;

  fCurrentVideoPlaybackEngine := VideoPlayback;

  // create score class
  Scores := TSingScores.Create;
  Scores.LoadfromTheme;

  LoadFromTheme(Theme.Sing);

  // timebar
  StaticTimeProgress := AddStatic(Theme.Sing.StaticTimeProgress);
  TextTimeText := AddText(Theme.Sing.TextTimeText);

  // 1 player       | P1
  StaticP1 := AddStatic(Theme.Sing.StaticP1);
  TextP1   := AddText(Theme.Sing.TextP1);

  // 2 or 4 players | P1
  StaticP1TwoP := AddStatic(Theme.Sing.StaticP1TwoP);
  TextP1TwoP   := AddText(Theme.Sing.TextP1TwoP);

  //                | P2
  StaticP2R := AddStatic(Theme.Sing.StaticP2R);
  TextP2R   := AddText(Theme.Sing.TextP2R);

  // 3 or 6 players | P1
  StaticP1ThreeP := AddStatic(Theme.Sing.StaticP1ThreeP);
  TextP1ThreeP   := AddText(Theme.Sing.TextP1ThreeP);

  //                | P2
  StaticP2M := AddStatic(Theme.Sing.StaticP2M);
  TextP2M   := AddText(Theme.Sing.TextP2M);

  //                | P3
  StaticP3R := AddStatic(Theme.Sing.StaticP3R);
  TextP3R   := AddText(Theme.Sing.TextP3R);

  StaticPausePopup := AddStatic(Theme.Sing.PausePopUp);

  // <note> pausepopup is not visibile at the beginning </note>
  Static[StaticPausePopup].Visible := false;

  Lyrics := TLyricEngine.Create(
      Theme.LyricBar.UpperX, Theme.LyricBar.UpperY, Theme.LyricBar.UpperW, Theme.LyricBar.UpperH,
      Theme.LyricBar.LowerX, Theme.LyricBar.LowerY, Theme.LyricBar.LowerW, Theme.LyricBar.LowerH);

  LyricsSync := TLyricsSyncSource.Create();
end;

procedure TScreenSing.onShow;
var
  Index:  integer;
  V1:     boolean;
  V1TwoP: boolean;   // position of score box in two player mode
  V1ThreeP: boolean; // position of score box in three player mode
  V2R:    boolean;
  V2M:    boolean;
  V3R:    boolean;
  Color: TRGB;

  success: boolean;
begin
  inherited;

  Log.LogStatus('Begin', 'onShow');
  FadeOut := false;

  // reset video playback engine, to play video clip ...
  fCurrentVideoPlaybackEngine := VideoPlayback;

  // setup score manager
  Scores.ClearPlayers; // clear old player values
  Color.R := 0;
  Color.G := 0;
  Color.B := 0; // dummy atm  <- \(O.o)/? B like bummy?

  // add new players
  for Index := 0 to PlayersPlay - 1 do
  begin
    Scores.AddPlayer(Tex_ScoreBG[Index], Color);
  end;

  Scores.Init; // get positions for players

  // prepare players
  SetLength(Player, PlayersPlay);

  case PlayersPlay of
    1:
    begin
      V1     := true;
      V1TwoP := false;
      V1ThreeP := false;
      V2R    := false;
      V2M    := false;
      V3R    := false;
    end;
    2:
    begin
      V1     := false;
      V1TwoP := true;
      V1ThreeP := false;
      V2R    := true;
      V2M    := false;
      V3R    := false;
    end;
    3:
    begin
      V1     := false;
      V1TwoP := false;
      V1ThreeP := true;
      V2R    := false;
      V2M    := true;
      V3R    := true;
    end;
    4:
    begin // double screen
      V1     := false;
      V1TwoP := true;
      V1ThreeP := false;
      V2R    := true;
      V2M    := false;
      V3R    := false;
    end;
    6:
    begin // double screen
      V1     := false;
      V1TwoP := false;
      V1ThreeP := true;
      V2R    := false;
      V2M    := true;
      V3R    := true;
    end;

  end;

  // this one is shown in 1P mode
  Static[StaticP1].Visible := V1;
  Text[TextP1].Visible     := V1;

  // this one is shown in 2/4P mode
  Static[StaticP1TwoP].Visible := V1TwoP;
  Text[TextP1TwoP].Visible     := V1TwoP;

  Static[StaticP2R].Visible := V2R;
  Text[TextP2R].Visible     := V2R;

  // this one is shown in 3/6P mode
  Static[StaticP1ThreeP].Visible := V1ThreeP;
  Text[TextP1ThreeP].Visible     := V1ThreeP;

  Static[StaticP2M].Visible := V2M;
  Text[TextP2M].Visible     := V2M;

  Static[StaticP3R].Visible := V3R;
  Text[TextP3R].Visible     := V3R;

  // FIXME: sets path and filename to ''
  ResetSingTemp;

  CurrentSong := CatSongs.Song[CatSongs.Selected];

  // FIXME: bad style, put the try-except into loadsong() and not here
  try
    // check if file is xml
    if copy(CurrentSong.FileName, length(CurrentSong.FileName) - 3, 4) = '.xml' then
      success := CurrentSong.LoadXMLSong()
    else
      success := CurrentSong.LoadSong();
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
    if (Length(CurrentSong.LastError) > 0) then
      ScreenPopupError.ShowPopup(Format(Language.Translate(CurrentSong.LastError), [CurrentSong.ErrorLineNo]))
    else
      ScreenPopupError.ShowPopup(Language.Translate('ERROR_CORRUPT_SONG'));
    // FIXME: do we need this?
    CurrentSong.Path := CatSongs.Song[CatSongs.Selected].Path;
    Exit;
  end;

  // reset video playback engine, to play video clip ...
  fCurrentVideoPlaybackEngine.Close;
  fCurrentVideoPlaybackEngine := VideoPlayback;

  {*
   * == Background ==
   * We have four types of backgrounds:
   *   + Blank        : Nothing has been set, this is our fallback
   *   + Picture      : Picture has been set, and exists - otherwise we fallback
   *   + Video        : Video has been set, and exists - otherwise we fallback
   *   + Visualization: + Off        : No visualization
   *                    + WhenNoVideo: Overwrites blank and picture
   *                    + On         : Overwrites blank, picture and video
   *}

  {*
   * set background to: video
   *}
  VideoLoaded := false;
  fShowVisualization := false;
  if (CurrentSong.Video <> '') and FileExists(CurrentSong.Path + CurrentSong.Video) then
  begin
    if (fCurrentVideoPlaybackEngine.Open(CurrentSong.Path + CurrentSong.Video)) then
    begin
      fShowVisualization := false;
      fCurrentVideoPlaybackEngine := VideoPlayback;
      fCurrentVideoPlaybackEngine.Position := CurrentSong.VideoGAP + CurrentSong.Start;
      fCurrentVideoPlaybackEngine.Play;
      VideoLoaded := true;
    end;
  end;

  {*
   * set background to: picture
   *}
  if (CurrentSong.Background <> '') and (VideoLoaded = false)
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
    fShowVisualization := true;
    fCurrentVideoPlaybackEngine := Visualization;
    if (fCurrentVideoPlaybackEngine <> nil) then
      fCurrentVideoPlaybackEngine.Play;
  end;

  {*
   * set background to: visualization (Videos are still shown)
   *}
  if ((TVisualizerOption(Ini.VisualizerOption) in [voWhenNoVideo]) and
     (VideoLoaded = false)) then
  begin
    fShowVisualization := true;
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

  // clear the scores of all players

  for Index := 0 to High(Player) do
    with Player[Index] do
    begin
      Score          := 0;
      ScoreLine      := 0;
      ScoreGolden    := 0;

      ScoreInt       := 0;
      ScoreLineInt   := 0;
      ScoreGoldenInt := 0;
      ScoreTotalInt  := 0;

      ScoreLast      := 0;

      LastSentencePerfect := false;
    end;

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

  // initialize lyrics by filling its queue
  while (not Lyrics.IsQueueFull) and
        (Lyrics.LineCounter <= High(Lines[0].Line)) do
  begin
    Lyrics.AddLine(@Lines[0].Line[Lyrics.LineCounter]);
  end;

  // deactivate pause
  Paused := false;

  // kill all stars not killed yet (goldenstarstwinkle mod)
  GoldenRec.SentenceChange;

  // set position of line bonus - line bonus end
  // set number of empty sentences for line bonus
  NumEmptySentences := 0;
  for Index := Low(Lines[0].Line) to High(Lines[0].Line) do
    if Lines[0].Line[Index].TotalNotes = 0 then
      Inc(NumEmptySentences);

  Log.LogStatus('End', 'onShow');
end;

procedure TScreenSing.onShowFinish;
begin
  // hide cursor on singscreen show    
  Display.SetCursor;
  
  // start lyrics
  LyricsState.Resume();

  // start music
  AudioPlayback.Play();

  // start timer
  CountSkipTimeSet;
end;

procedure TScreenSing.onHide;
begin
  // background texture
  if (Tex_Background.TexNum > 0) then
  begin
    glDeleteTextures(1, PGLuint(@Tex_Background.TexNum));
    Tex_Background.TexNum := 0;
  end;

  Background.OnFinish;
  Display.SetCursor;
end;

function TScreenSing.Draw: boolean;
var
  Min:   integer;
  Sec:   integer;
  T:     integer;
  CurLyricsTime: real;
begin
  Background.Draw;

  // draw background picture (if any, and if no visualizations)
  // when we don't check for visualizations the visualizations would
  // be overdrawn by the picture when {UNDEFINED UseTexture} in UVisualizer
  if (not fShowVisualization) then
    SingDrawBackground;

  // set player names (for 2 screens and only singstar skin)
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
  {Static[StaticP1].Texture.X := Static[StaticP1].Texture.X + 10 * ScreenX;

  Text[TextP1].X := Text[TextP1].X + 10 * ScreenX;  }

  {Static[StaticP1ScoreBG].Texture.X  := Static[StaticP1ScoreBG].Texture.X + 10*ScreenX;
  Text[TextP1Score].X                := Text[TextP1Score].X + 10*ScreenX;}

  {Static[StaticP2R].Texture.X := Static[StaticP2R].Texture.X + 10 * ScreenX;

  Text[TextP2R].X := Text[TextP2R].X + 10 * ScreenX; }

  {Static[StaticP2RScoreBG].Texture.X := Static[StaticP2RScoreBG].Texture.X + 10*ScreenX;
  Text[TextP2RScore].X               := Text[TextP2RScore].X + 10*ScreenX;}

  // end of weird stuff
  {
  Static[1].Texture.X := Static[1].Texture.X + 10 * ScreenX;     }

 { for T := 0 to 1 do
    Text[T].X := Text[T].X + 10 * ScreenX;    }

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

  // update and draw movie
  if (ShowFinish and (VideoLoaded or fShowVisualization)) then
  begin
    if assigned(fCurrentVideoPlaybackEngine) then
    begin
      // Just call this once
      // when Screens = 2
      if (ScreenAct = 1) then
        fCurrentVideoPlaybackEngine.GetFrame(CurrentSong.VideoGAP + LyricsState.GetCurrentTime());

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
        FadeOut := true;
        FadeTo(@ScreenScore);
      end;
    end;
  end;

  // always draw custom items
  SingDraw;

  // goldennotestarstwinkle
  GoldenRec.SpawnRec;

  // draw scores
  Scores.Draw;

  ////
  // dual screen, part 2
  ////////////////////////

  // Note: ScreenX is the offset of the current screen in dual-screen mode so we
  // will move the statics and texts to the correct screen here.
  // FIXME: clean up this weird stuff

  {Static[StaticP1].Texture.X := Static[StaticP1].Texture.X - 10 * ScreenX;
  Text[TextP1].X := Text[TextP1].X - 10 * ScreenX;

  Static[StaticP2R].Texture.X := Static[StaticP2R].Texture.X - 10 * ScreenX;
  Text[TextP2R].X := Text[TextP2R].X - 10 * ScreenX;

  // end of weird

  Static[1].Texture.X := Static[1].Texture.X - 10 * ScreenX;

  for T := 0 to 1 do
    Text[T].X := Text[T].X - 10 * ScreenX;        }

  // draw pausepopup
  // FIXME: this is a workaround that the static is drawn over the lyrics, lines, scores and effects
  // maybe someone could find a better solution
  if Paused then
  begin
    Static[StaticPausePopup].Visible := true;
    Static[StaticPausePopup].Draw;
    Static[StaticPausePopup].Visible := false;
  end;

  Result := true;
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
  VideoLoaded := false;

  // kill all stars and effects
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

  SetFontItalic(false);
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

    // line bonus

    // points for this line
    LineScore := CurrentScore - CurrentPlayer.ScoreLast;

    // determine LinePerfection
    // Note: the "+2" extra points are a little bonus so the player does not
    // have to be that perfect to reach the bonus steps.
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
      CurrentPlayer.ScoreLineInt := Floor(CurrentPlayer.ScoreLine / 10) * 10;
      // update total score
      CurrentPlayer.ScoreTotalInt :=
        CurrentPlayer.ScoreInt +
        CurrentPlayer.ScoreGoldenInt
        + CurrentPlayer.ScoreLineInt;

      // spawn rating pop-up
      Rating := Round(LinePerfection * MAX_LINE_RATING);
      Scores.SpawnPopUp(PlayerIndex, Rating, CurrentPlayer.ScoreTotalInt);
    end;

    // PerfectLineTwinkle (effect), part 1
    if (Ini.EffectSing = 1) then
      CurrentPlayer.LastSentencePerfect := (LinePerfection >= 1);

    // refresh last score
    CurrentPlayer.ScoreLast := CurrentScore;
  end;

  // PerfectLineTwinkle (effect), part 2
  if (Ini.EffectSing = 1) then
    GoldenRec.SpawnPerfectLineTwinkle;
end;

 // Called on sentence change
 // SentenceIndex: index of the new active sentence
procedure TScreenSing.OnSentenceChange(SentenceIndex: cardinal);
begin
  // goldenstarstwinkle
  GoldenRec.SentenceChange;

  // fill lyrics queue and set upper line to the current sentence
  while (Lyrics.GetUpperLineIndex() < SentenceIndex) or
    (not Lyrics.IsQueueFull) do
  begin
    // add the next line to the queue or a dummy if no more lines are available
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

